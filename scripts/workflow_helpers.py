#!/usr/bin/env python3
"""Small checks that roc-parser's workflows and release scripts share.

    workflow_helpers.py validate-examples [--archive roc-parser-examples-<version>.zip]
    workflow_helpers.py require-success success success ...
"""
from __future__ import annotations

import argparse
import re
import sys
import zipfile
from pathlib import Path, PurePosixPath
from typing import Sequence

try:
    from ._common import ROOT
except ImportError:
    from _common import ROOT


def append_github_output(path: Path, name: str, value: str) -> None:
    if "\n" in name or "\n" in value or "\r" in name or "\r" in value:
        raise ValueError("GitHub output names and values must be single-line")
    with path.open("a", encoding="utf-8") as output:
        output.write(f"{name}={value}\n")


EXAMPLE_DEPENDENCY_RE = re.compile(r'(?m)^\s*parser:\s*"([^"]+)"')
LOCAL_PACKAGE_PATH = "../package/main.roc"


def example_dependencies(sources: dict[str, str]) -> dict[str, list[str]]:
    if not sources:
        raise ValueError("no Roc examples found")
    return {name: EXAMPLE_DEPENDENCY_RE.findall(text) for name, text in sorted(sources.items())}


def validate_examples(examples_dir: Path = ROOT / "examples", archive: Path | None = None) -> None:
    """The repository's examples use the package source; the release archive's
    examples use one https bundle URL."""
    if archive is None:
        sources = {path.name: path.read_text(encoding="utf-8") for path in examples_dir.glob("*.roc")}
        for name, deps in example_dependencies(sources).items():
            if deps != [LOCAL_PACKAGE_PATH]:
                raise ValueError(f"{name} must depend on parser: \"{LOCAL_PACKAGE_PATH}\", found {deps}")
        print(f"{len(sources)} examples use {LOCAL_PACKAGE_PATH}")
        return

    with zipfile.ZipFile(archive) as bundle:
        sources = {
            name: bundle.read(name).decode("utf-8")
            for name in bundle.namelist()
            if PurePosixPath(name).suffix == ".roc"
            and len(PurePosixPath(name).parts) == 3
            and PurePosixPath(name).parts[1] == "examples"
        }
    urls = set()
    for name, deps in example_dependencies(sources).items():
        if len(deps) != 1 or not deps[0].startswith(("https://", "http://")):
            raise ValueError(f"{name} must depend on parser through a bundle URL, found {deps}")
        urls.add(deps[0])
    if len(urls) != 1:
        raise ValueError(f"the archive's examples must share one bundle URL, found {sorted(urls)}")
    print(f"{len(sources)} examples in {archive} use {urls.pop()}")


def require_success(results: Sequence[str]) -> None:
    unsuccessful = [result for result in results if result != "success"]
    if unsuccessful:
        raise ValueError(f"required jobs did not succeed: {', '.join(unsuccessful)}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subparsers = parser.add_subparsers(dest="command", required=True)

    validate = subparsers.add_parser("validate-examples")
    validate.add_argument("--examples-dir", type=Path, default=ROOT / "examples")
    validate.add_argument("--archive", type=Path, help="check a packaged examples zip instead")

    success = subparsers.add_parser("require-success")
    success.add_argument("results", nargs="+")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        if args.command == "validate-examples":
            validate_examples(args.examples_dir, args.archive)
        elif args.command == "require-success":
            require_success(args.results)
        else:
            raise AssertionError(f"unhandled command: {args.command}")
    except (OSError, ValueError, zipfile.BadZipFile) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
