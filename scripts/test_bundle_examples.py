#!/usr/bin/env python3
from __future__ import annotations

import argparse
import functools
import http.server
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import zipfile
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT, display_command, roc_command
    from . import release_helpers, workflow_helpers
except ImportError:
    from _common import ROOT, display_command, roc_command
    import release_helpers
    import workflow_helpers


# The archive tested locally is labelled with this placeholder version.
LOCAL_RELEASE_TAG = "local"
SKIPPED_EXAMPLES: dict[str, str] = {}


def run(command: Sequence[str], *, cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    normalized = list(command)
    print("+", display_command(normalized), flush=True)
    completed = subprocess.run(
        normalized,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )

    if completed.returncode != 0:
        if completed.stdout:
            print(completed.stdout)
        if completed.stderr:
            print(completed.stderr, file=sys.stderr)
        raise SystemExit(
            f"command failed with exit code {completed.returncode}: {display_command(normalized)}"
        )

    return completed


def bundle_package(bundle_dir: Path) -> Path:
    completed = run([sys.executable, "scripts/bundle.py", "--output-dir", str(bundle_dir)])
    match = re.search(r"^Created:\s+(.+\.tar\.zst)\s*$", completed.stdout, re.MULTILINE)

    if match is None:
        raise SystemExit("Could not find bundle path in roc bundle output")

    bundle_path = Path(match.group(1))
    if not bundle_path.is_file():
        raise SystemExit(f"Bundle was not created: {bundle_path}")

    return bundle_path


def start_server(directory: Path) -> tuple[http.server.ThreadingHTTPServer, str]:
    handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=str(directory))
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    port = int(server.server_address[1])
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    return server, f"http://127.0.0.1:{port}"


def package_and_extract(target: Path, bundle_url: str, *, source_dir: Path = ROOT / "examples") -> list[Path]:
    """Package the examples exactly as a release does, pinned to `bundle_url`,
    unzip the archive under `target`, and return its example apps."""
    try:
        archive = release_helpers.package_examples(
            source_dir, bundle_url, LOCAL_RELEASE_TAG, target / f"roc-parser-examples-{LOCAL_RELEASE_TAG}.zip"
        )
    except RuntimeError as error:
        raise SystemExit(str(error)) from error
    with zipfile.ZipFile(archive) as bundle:
        bundle.extractall(target)
    examples_dir = target / archive.stem / "examples"
    return select_examples(sorted(examples_dir.glob("*.roc")))


def select_examples(paths: Sequence[Path]) -> list[Path]:
    examples = []
    for example in paths:
        if example.name in SKIPPED_EXAMPLES:
            print(f"Skipping {example.name}: {SKIPPED_EXAMPLES[example.name]}.")
            continue
        examples.append(example)
    if not examples:
        raise SystemExit("No examples found")
    return examples


def run_example_checks(examples: Sequence[Path], roc: str) -> None:
    for example in examples:
        run([roc, "check", example.name, "--no-cache"], cwd=example.parent)


def run_example_apps(examples: Sequence[Path], roc: str) -> None:
    for example in examples:
        run([roc, example.name, "--no-cache"], cwd=example.parent)


def build_and_run_examples(examples: Sequence[Path], build_dir: Path, roc: str) -> None:
    build_dir.mkdir(parents=True, exist_ok=True)
    exe_suffix = ".exe" if os.name == "nt" else ""

    for example in examples:
        output = build_dir / f"{example.stem}{exe_suffix}"
        run([roc, "build", example.name, f"--output={output}", "--no-cache"], cwd=example.parent)
        run([str(output)])


def checkout_examples(source_dir: Path = ROOT / "examples") -> list[Path]:
    """The examples in the checkout, which must use the checkout's package."""
    try:
        workflow_helpers.validate_examples(source_dir)
    except ValueError as error:
        raise SystemExit(str(error)) from error
    return select_examples(sorted(source_dir.glob("*.roc")))


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "By default, package the examples as a release does, pinned to a locally "
            "served bundle, and check and run the archive's apps. With --checkout, "
            "check and run the examples in place against the checkout's package."
        )
    )
    parser.add_argument(
        "--bundle-path",
        type=Path,
        help="Use an existing bundle instead of creating one",
    )
    parser.add_argument(
        "--skip-build-run",
        action="store_true",
        help="Skip compiled example execution",
    )
    parser.add_argument(
        "--checkout",
        action="store_true",
        help="Test the examples in the checkout against the checkout's package (../package/main.roc)",
    )
    args = parser.parse_args(argv)

    if args.checkout and args.bundle_path is not None:
        parser.error("--checkout cannot be combined with --bundle-path")

    default_tmp = ROOT / ".roc-parser-tmp"
    tmp_parent = Path(os.environ.get("ROC_PARSER_TMPDIR", default_tmp)).resolve()
    tmp_parent.mkdir(parents=True, exist_ok=True)
    roc = roc_command()

    with tempfile.TemporaryDirectory(prefix="roc-parser-bundle-", dir=tmp_parent) as tmp:
        tmp_dir = Path(tmp)
        build_dir = tmp_dir / "build"

        if args.checkout:
            examples = checkout_examples()
            print("Testing the checkout's examples against the checkout's package")
            run_example_checks(examples, roc)
            run_example_apps(examples, roc)
            if not args.skip_build_run:
                build_and_run_examples(examples, build_dir, roc)
            return 0

        bundle_dir = tmp_dir / "bundle"
        examples_dir = tmp_dir / "archive"

        bundle_dir.mkdir()
        examples_dir.mkdir()

        if args.bundle_path is None:
            bundle_path = bundle_package(bundle_dir)
        else:
            source_bundle = args.bundle_path.resolve()
            if not source_bundle.is_file():
                raise SystemExit(f"Bundle does not exist: {source_bundle}")

            bundle_path = bundle_dir / source_bundle.name
            shutil.copy2(source_bundle, bundle_path)

        server, base_url = start_server(bundle_dir)
        try:
            bundle_url = f"{base_url}/{bundle_path.name}"
            examples = package_and_extract(examples_dir, bundle_url)

            print(f"Testing the packaged examples archive against {bundle_url}")
            run_example_checks(examples, roc)
            run_example_apps(examples, roc)

            if not args.skip_build_run:
                build_and_run_examples(examples, build_dir, roc)
        finally:
            server.shutdown()
            server.server_close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
