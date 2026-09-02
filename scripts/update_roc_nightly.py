#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path
from typing import Sequence

try:
    from .github_signed_commit import ROOT, append_output, create_signed_commit, run
except ImportError:
    from github_signed_commit import ROOT, append_output, create_signed_commit, run


NIGHTLY_RE = re.compile(r"^nightly-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9a-f]+$")


def validate_nightly_tag(tag: str) -> str:
    if not NIGHTLY_RE.fullmatch(tag):
        raise ValueError(f"invalid Roc nightly tag: {tag!r}")
    return tag


def update_version_file(path: Path, tag: str) -> bool:
    validated = validate_nightly_tag(tag)
    current = path.read_text(encoding="utf-8").strip()
    if current == validated:
        return False
    path.write_text(f"{validated}\n", encoding="utf-8")
    return True


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Update .roc-version to the latest nightly.")
    parser.add_argument("--repository", required=True)
    parser.add_argument("--branch", required=True)
    parser.add_argument("--github-output", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        latest = validate_nightly_tag(
            run(
                [
                    "gh",
                    "release",
                    "view",
                    "--repo",
                    "roc-lang/nightlies",
                    "--json",
                    "tagName",
                    "--jq",
                    ".tagName",
                ]
            ).stdout.strip()
        )
        changed = update_version_file(ROOT / ".roc-version", latest)
        oid = ""
        if changed:
            oid = create_signed_commit(
                args.repository,
                args.branch,
                f"Update Roc nightly pin to {latest}",
                [".roc-version"],
            )
        append_output(args.github_output, "nightly-tag", latest)
        append_output(args.github_output, "changed", "true" if oid else "false")
        append_output(args.github_output, "commit-sha", oid)
        append_output(args.github_output, "branch", args.branch)
    except (OSError, subprocess.CalledProcessError, ValueError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
