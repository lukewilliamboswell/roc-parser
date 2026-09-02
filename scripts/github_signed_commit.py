#!/usr/bin/env python3
from __future__ import annotations

import argparse
import base64
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[1]
REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
BRANCH_RE = re.compile(r"^(?!/)(?!.*(?:\.\.|//))[A-Za-z0-9._/-]+(?<!/)$")


def run(command: Sequence[str], *, input_text: str | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=ROOT,
        input=input_text,
        text=True,
        check=True,
        capture_output=True,
    )


def append_output(path: Path, name: str, value: str) -> None:
    if any(character in name + value for character in "\r\n"):
        raise ValueError("GitHub outputs must be single-line")
    with path.open("a", encoding="utf-8") as output:
        output.write(f"{name}={value}\n")


def validate_path(path: str) -> str:
    candidate = Path(path)
    if not path or candidate.is_absolute() or ".." in candidate.parts:
        raise ValueError(f"unsafe repository path: {path!r}")
    return candidate.as_posix()


def staged_file_changes(paths: Sequence[str]) -> dict[str, list[dict[str, str]]]:
    safe_paths = [validate_path(path) for path in paths]
    run(["git", "add", "-A", "--", *safe_paths])
    status = subprocess.run(
        ["git", "diff", "--cached", "--name-status", "--no-renames", "-z", "--", *safe_paths],
        cwd=ROOT,
        check=True,
        capture_output=True,
    ).stdout.split(b"\0")

    additions: list[dict[str, str]] = []
    deletions: list[dict[str, str]] = []
    entries = status[:-1] if status and status[-1] == b"" else status
    if len(entries) % 2 != 0:
        raise ValueError("unexpected git status output")
    for index in range(0, len(entries), 2):
        change = entries[index].decode("ascii")
        path = validate_path(entries[index + 1].decode("utf-8"))
        if change == "D":
            deletions.append({"path": path})
        elif change in {"A", "M"}:
            contents = subprocess.run(
                ["git", "show", f":{path}"],
                cwd=ROOT,
                check=True,
                capture_output=True,
            ).stdout
            additions.append(
                {"path": path, "contents": base64.b64encode(contents).decode("ascii")}
            )
        else:
            raise ValueError(f"unsupported git change {change!r} for {path!r}")
    return {"additions": additions, "deletions": deletions}


def prepare_branch(repository: str, branch: str, base_oid: str) -> None:
    reference = f"repos/{repository}/git/refs/heads/{branch}"
    found = subprocess.run(
        ["gh", "api", reference],
        cwd=ROOT,
        text=True,
        capture_output=True,
    )
    if found.returncode == 0:
        run(["gh", "api", "--method", "PATCH", reference, "-f", f"sha={base_oid}", "-F", "force=true"])
    else:
        run(
            [
                "gh",
                "api",
                "--method",
                "POST",
                f"repos/{repository}/git/refs",
                "-f",
                f"ref=refs/heads/{branch}",
                "-f",
                f"sha={base_oid}",
            ]
        )


def create_signed_commit(repository: str, branch: str, message: str, paths: Sequence[str]) -> str:
    if not REPOSITORY_RE.fullmatch(repository):
        raise ValueError(f"invalid GitHub repository: {repository!r}")
    if not BRANCH_RE.fullmatch(branch):
        raise ValueError(f"invalid branch: {branch!r}")
    if not message or any(character in message for character in "\r\n"):
        raise ValueError("commit message must be a non-empty single line")
    if not os.environ.get("GH_TOKEN"):
        raise ValueError("GH_TOKEN is required")

    changes = staged_file_changes(paths)
    if not changes["additions"] and not changes["deletions"]:
        return ""

    base_oid = run(["git", "rev-parse", "HEAD"]).stdout.strip()
    prepare_branch(repository, branch, base_oid)
    request = {
        "query": """
mutation($input: CreateCommitOnBranchInput!) {
  createCommitOnBranch(input: $input) { commit { oid } }
}
""",
        "variables": {
            "input": {
                "branch": {"repositoryNameWithOwner": repository, "branchName": branch},
                "message": {"headline": message},
                "fileChanges": changes,
                "expectedHeadOid": base_oid,
            }
        },
    }
    response = json.loads(
        run(["gh", "api", "graphql", "--input", "-"], input_text=json.dumps(request)).stdout
    )
    return response["data"]["createCommitOnBranch"]["commit"]["oid"]


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Create a GitHub-signed automation commit.")
    parser.add_argument("--repository", required=True)
    parser.add_argument("--branch", required=True)
    parser.add_argument("--message", required=True)
    parser.add_argument("--path", action="append", dest="paths", required=True)
    parser.add_argument("--github-output", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        oid = create_signed_commit(args.repository, args.branch, args.message, args.paths)
        append_output(args.github_output, "changed", "true" if oid else "false")
        append_output(args.github_output, "commit-sha", oid)
    except (KeyError, OSError, subprocess.CalledProcessError, ValueError, json.JSONDecodeError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
