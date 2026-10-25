#!/usr/bin/env python3
from __future__ import annotations

import os
import shutil
import sys
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT, report_failure, resolve_command, run_command
except ImportError:
    from _common import ROOT, report_failure, resolve_command, run_command


def run_checked(command: Sequence[str], *, env: dict[str, str]) -> int:
    completed = run_command(command, env=env)
    if completed.returncode != 0:
        return report_failure(completed, command)
    return 0


def main() -> int:
    roc = resolve_command(os.environ.get("ROC", "roc"))
    tmp_base = Path(os.environ.get("ROC_PARSER_TMPDIR", ROOT / ".roc-parser-tmp")).resolve()
    tmp_dir = tmp_base / "roc-parser-ci"
    docs_dir = tmp_dir / "docs"

    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    docs_dir.mkdir(parents=True)

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

    print("\nChecking package formatting...", flush=True)
    status = run_checked([roc, "fmt", "--check", "package"], env=env)
    if status:
        return status

    print("\nRunning package tests...", flush=True)
    # Testing through main.roc runs every module's expects with the package's
    # dependencies resolved; a module tested alone cannot import them.
    status = run_checked([roc, "test", "package/main.roc"], env=env)
    if status:
        return status

    print("\nChecking compile-time literals and imported files...", flush=True)
    status = run_checked([sys.executable, "scripts/check_literals.py"], env=env)
    if status:
        return status

    print("\nGenerating package docs...", flush=True)
    status = run_checked(
        [roc, "docs", "package/main.roc", f"--output={docs_dir}"],
        env=env,
    )
    if status:
        return status

    return 0

if __name__ == "__main__":
    raise SystemExit(main())
