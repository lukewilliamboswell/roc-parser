#!/usr/bin/env python3
from __future__ import annotations

import os
import platform
import re
import shutil
import sys
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT, report_failure, resolve_command, run_command
except ImportError:
    from _common import ROOT, report_failure, resolve_command, run_command


TEST_MODULES = (
    "Parser",
    "CSV",
    "Markdown",
    "String",
    "Xml",
    "Yaml",
)
BUNDLE_PATH_RE = re.compile(r"^Created:\s+(.+\.tar\.zst)\s*$", re.MULTILINE)


def run_checked(command: Sequence[str], *, env: dict[str, str]) -> int:
    completed = run_command(command, env=env)
    if completed.returncode != 0:
        return report_failure(completed, command)
    return 0


def extract_bundle_path(output: str) -> Path:
    match = BUNDLE_PATH_RE.search(output)
    if match is None:
        raise ValueError("could not extract bundle path from roc bundle output")
    return Path(match.group(1))


def main() -> int:
    roc = resolve_command(os.environ.get("ROC", "roc"))
    tmp_base = Path(os.environ.get("ROC_PARSER_TMPDIR", ROOT / ".roc-parser-tmp")).resolve()
    tmp_dir = tmp_base / "roc-parser-ci"
    docs_dir = tmp_dir / "docs"
    bundle_dir = tmp_dir / "bundle"

    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    docs_dir.mkdir(parents=True)
    bundle_dir.mkdir(parents=True)

    env = os.environ.copy()
    env["ROC"] = roc
    env["ROC_PARSER_TMPDIR"] = str(tmp_base)

    version_command = [roc, "version"]
    version = run_command(version_command, env=env, capture_output=True)
    if version.returncode != 0:
        return report_failure(version, version_command)
    print((version.stdout or "").rstrip(), flush=True)

    print("\nChecking package...", flush=True)
    status = run_checked([roc, "check", "package/main.roc"], env=env)
    if status:
        return status

    print("\nRunning package tests...", flush=True)
    for module in TEST_MODULES:
        if module == "Markdown":
            print(
                "Skipping package/HTTP.roc tests: latest nightly segfaults in the compiler "
                "while running this module's tests.",
                flush=True,
            )
        status = run_checked([roc, "test", f"package/{module}.roc"], env=env)
        if status:
            return status

    print("\nGenerating package docs...", flush=True)
    status = run_checked(
        [roc, "docs", "package/main.roc", f"--output={docs_dir}"],
        env=env,
    )
    if status:
        return status

    if platform.system().upper().startswith(("WINDOWS", "MINGW", "MSYS", "CYGWIN")):
        print("\nSkipping package bundling on Windows.", flush=True)
        return 0

    print("\nBundling package...", flush=True)
    bundle_command = [
        sys.executable,
        "scripts/bundle.py",
        "--output-dir",
        str(bundle_dir),
    ]
    bundled = run_command(bundle_command, env=env, capture_output=True)
    if bundled.stdout:
        print(
            bundled.stdout,
            end="" if bundled.stdout.endswith("\n") else "\n",
            flush=True,
        )
    if bundled.returncode != 0:
        return report_failure(bundled, bundle_command)

    try:
        bundle_path = extract_bundle_path(bundled.stdout or "")
    except ValueError as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1

    print("\nTesting examples against localhost bundle...", flush=True)
    return run_checked(
        [
            sys.executable,
            "scripts/test_bundle_examples.py",
            "--bundle-path",
            str(bundle_path),
        ],
        env=env,
    )


if __name__ == "__main__":
    raise SystemExit(main())
