#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT, resolve_command, run_command
except ImportError:
    from _common import ROOT, resolve_command, run_command


def parse_args(argv: Sequence[str] | None = None) -> tuple[Path, list[str]]:
    parser = argparse.ArgumentParser(
        description="Bundle the roc-parser package.",
        allow_abbrev=False,
    )
    parser.add_argument("--output-dir", type=Path, default=ROOT / "dist")
    args, roc_args = parser.parse_known_args(argv)
    return args.output_dir, roc_args


def bundle_command(
    *,
    root: Path,
    output_dir: Path,
    roc: str,
    roc_args: Sequence[str],
) -> list[str]:
    package_dir = root / "package"
    roc_files = [package_dir / "main.roc"]
    roc_files.extend(path for path in sorted(package_dir.glob("*.roc")) if path.name != "main.roc")
    return [
        roc,
        "bundle",
        *(path.name for path in roc_files),
        "--output-dir",
        str(output_dir),
        *roc_args,
    ]


def main(argv: Sequence[str] | None = None) -> int:
    output_dir, roc_args = parse_args(argv)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_dir = output_dir.resolve()
    roc = resolve_command(os.environ.get("ROC", "roc"))
    command = bundle_command(
        root=ROOT,
        output_dir=output_dir,
        roc=roc,
        roc_args=roc_args,
    )
    return run_command(command, cwd=ROOT / "package").returncode


if __name__ == "__main__":
    raise SystemExit(main())
