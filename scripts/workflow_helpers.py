#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import zipfile
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT
except ImportError:
    from _common import ROOT


REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")


def append_github_output(path: Path, name: str, value: str) -> None:
    if "\n" in name or "\n" in value or "\r" in name or "\r" in value:
        raise ValueError("GitHub output names and values must be single-line")
    with path.open("a", encoding="utf-8") as output:
        output.write(f"{name}={value}\n")


def read_roc_version(path: Path = ROOT / ".roc-version") -> str:
    version = path.read_text(encoding="utf-8").strip()
    if not version or "\n" in version or "\r" in version:
        raise ValueError(f"{path} must contain exactly one non-empty Roc version")
    return version


def validate_release_ref(ref_type: str, ref_name: str, default_branch: str) -> None:
    if ref_type != "branch":
        raise ValueError("Release workflow must be run from a branch")
    if ref_name != default_branch:
        raise ValueError(
            "Release workflow must be run from the default branch "
            f"{default_branch!r}, got {ref_name!r}"
        )


def resolve_bundle_url(metadata: Path, repository: str, version: str) -> str:
    if not REPOSITORY_RE.fullmatch(repository):
        raise ValueError(f"invalid GitHub repository: {repository!r}")
    if not version or any(character in version for character in "\r\n/"):
        raise ValueError(f"invalid release version: {version!r}")

    try:
        bundles = json.loads(metadata.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        raise ValueError(f"invalid release metadata JSON: {error}") from error
    if not isinstance(bundles, list) or len(bundles) != 1:
        count = len(bundles) if isinstance(bundles, list) else "non-list"
        raise ValueError(f"expected exactly one release bundle, found {count}")

    bundle = bundles[0]
    if not isinstance(bundle, dict):
        raise ValueError("release bundle entry must be an object")
    artifact_file = bundle.get("artifact_file")
    if (
        not isinstance(artifact_file, str)
        or not artifact_file
        or Path(artifact_file).name != artifact_file
        or any(character in artifact_file for character in "\r\n#?")
    ):
        raise ValueError("release bundle has an invalid artifact_file")

    return f"https://github.com/{repository}/releases/download/{version}/{artifact_file}"


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
            if name.endswith(".roc") and "/examples/" in name
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
    parser = argparse.ArgumentParser(description="Helpers used by roc-parser GitHub workflows.")
    subparsers = parser.add_subparsers(dest="command", required=True)

    roc_version = subparsers.add_parser("roc-version")
    roc_version.add_argument("--github-output", type=Path, required=True)

    release_ref = subparsers.add_parser("validate-release-ref")
    release_ref.add_argument("--ref-type", required=True)
    release_ref.add_argument("--ref-name", required=True)
    release_ref.add_argument("--default-branch", required=True)

    bundle_url = subparsers.add_parser("bundle-url")
    bundle_url.add_argument("--metadata", type=Path, required=True)
    bundle_url.add_argument("--repository", required=True)
    bundle_url.add_argument("--version", required=True)
    bundle_url.add_argument("--github-output", type=Path, required=True)

    validate = subparsers.add_parser("validate-examples")
    validate.add_argument("--examples-dir", type=Path, default=ROOT / "examples")
    validate.add_argument("--archive", type=Path, help="check a packaged examples zip instead")

    success = subparsers.add_parser("require-success")
    success.add_argument("results", nargs="+")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        if args.command == "roc-version":
            append_github_output(
                args.github_output,
                "nightly-tag",
                read_roc_version(),
            )
        elif args.command == "validate-release-ref":
            validate_release_ref(args.ref_type, args.ref_name, args.default_branch)
        elif args.command == "bundle-url":
            url = resolve_bundle_url(args.metadata, args.repository, args.version)
            append_github_output(args.github_output, "bundle-url", url)
        elif args.command == "validate-examples":
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
