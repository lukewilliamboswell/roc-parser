#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Sequence

try:
    from .github_signed_commit import BRANCH_RE, REPOSITORY_RE, run
except ImportError:
    from github_signed_commit import BRANCH_RE, REPOSITORY_RE, run


def open_or_update(repository: str, head: str, base: str, title: str, body: str) -> None:
    if not REPOSITORY_RE.fullmatch(repository):
        raise ValueError(f"invalid GitHub repository: {repository!r}")
    if not BRANCH_RE.fullmatch(head) or not BRANCH_RE.fullmatch(base):
        raise ValueError("invalid pull request branch")
    existing = run(
        [
            "gh",
            "pr",
            "list",
            "--repo",
            repository,
            "--head",
            head,
            "--base",
            base,
            "--state",
            "open",
            "--json",
            "number",
            "--jq",
            ".[0].number // empty",
        ]
    ).stdout.strip()
    if existing:
        run(["gh", "pr", "edit", existing, "--repo", repository, "--title", title, "--body", body])
    else:
        run(
            [
                "gh",
                "pr",
                "create",
                "--repo",
                repository,
                "--head",
                head,
                "--base",
                base,
                "--title",
                title,
                "--body",
                body,
            ]
        )


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Open or update an automation pull request.")
    parser.add_argument("--repository", required=True)
    parser.add_argument("--head", required=True)
    parser.add_argument("--base", required=True)
    parser.add_argument("--title", required=True)
    parser.add_argument("--body", required=True)
    parser.add_argument("--test-result", default="")
    parser.add_argument("--run-url", default="")
    args = parser.parse_args(argv)
    body = args.body
    if args.test_result:
        outcome = "passed" if args.test_result == "success" else f"did not pass ({args.test_result})"
        body += f"\n\nRepository tests {outcome}. See the [workflow run]({args.run_url}) for details."
    try:
        open_or_update(args.repository, args.head, args.base, args.title, body)
    except (OSError, subprocess.CalledProcessError, ValueError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
