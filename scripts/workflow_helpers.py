#!/usr/bin/env python3
"""Small checks that roc-parser's workflows share.

    workflow_helpers.py require-success success success ...
"""
from __future__ import annotations

import argparse
import sys
from typing import Sequence


def require_success(results: Sequence[str]) -> None:
    unsuccessful = [result for result in results if result != "success"]
    if unsuccessful:
        raise ValueError(f"required jobs did not succeed: {', '.join(unsuccessful)}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subparsers = parser.add_subparsers(dest="command", required=True)
    success = subparsers.add_parser("require-success")
    success.add_argument("results", nargs="+")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        if args.command == "require-success":
            require_success(args.results)
        else:
            raise AssertionError(f"unhandled command: {args.command}")
    except ValueError as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
