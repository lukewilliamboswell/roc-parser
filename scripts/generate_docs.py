#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import re
import shutil
import sys
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT, report_failure, resolve_command, run_command
except ImportError:
    from _common import ROOT, report_failure, resolve_command, run_command


VERSION_RE = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")


def normalize_version(version: str) -> str:
    normalized = version.removeprefix("v")
    if not VERSION_RE.fullmatch(normalized):
        raise ValueError("Version number must be in format x.y.z (e.g., 0.12.0)")
    return normalized


def redirect_page(version: str) -> str:
    return f"""<!doctype html>
<html lang="en">
    <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Redirecting...</title>
        <script>
            window.location.href = "/roc-parser/{version}/";
        </script>
    </head>
    <body>
        <noscript>
            <p>
                If you are not automatically redirected, please
                <a href="/roc-parser/{version}/">click here</a>.
            </p>
        </noscript>
    </body>
</html>
"""


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate versioned roc-parser documentation.")
    parser.add_argument("version", nargs="?", default=os.environ.get("DOCS_VERSION"))
    parser.add_argument(
        "--docs-root",
        type=Path,
        default=Path(os.environ.get("DOCS_ROOT", "www")),
    )
    parser.add_argument(
        "--skip-index",
        action="store_true",
        help="Do not replace the docs root index with a version redirect.",
    )
    args = parser.parse_args(argv)
    if args.version is None:
        parser.error("VERSION is required (or set DOCS_VERSION)")
    return args


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        version = normalize_version(args.version)
    except ValueError as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1

    docs_root = args.docs_root
    if not docs_root.is_absolute():
        docs_root = ROOT / docs_root
    docs_root = docs_root.resolve()
    output_dir = docs_root / version

    if output_dir.exists():
        if output_dir.is_dir():
            shutil.rmtree(output_dir)
        else:
            output_dir.unlink()
    docs_root.mkdir(parents=True, exist_ok=True)

    roc = resolve_command(os.environ.get("ROC", "roc"))
    command = [roc, "docs", "package/main.roc", f"--output={output_dir}"]
    completed = run_command(command)
    if completed.returncode != 0:
        return report_failure(completed, command)

    if not args.skip_index:
        (docs_root / "index.html").write_text(redirect_page(version), encoding="utf-8")

    print(f"Generated docs for {version} in {output_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
