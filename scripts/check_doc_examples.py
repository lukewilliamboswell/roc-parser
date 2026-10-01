#!/usr/bin/env python3
"""Format-check, check, test and run every Roc example the manual includes.

The manual includes these files verbatim (docs/examples/*.roc, through
`include::examples/<file>.roc[tag=<name>,indent=0]`), so a snippet that stops
compiling fails here rather than in a reader's terminal, and one `roc fmt`
would change fails too, since readers copy what the manual shows.

Each example names the working-tree package by the relative path
`../../package/main.roc`, so it always documents the code beside it. When
`<name>.expected` sits next to `<name>.roc`, the app's standard output must
match it exactly.

    scripts/check_doc_examples.py            # every example
    scripts/check_doc_examples.py csv        # only files whose name contains "csv"

`ROC` selects the compiler (default: `roc` on PATH).
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
EXAMPLES = ROOT / "docs" / "examples"
EXPECT_RE = re.compile(r"^\s*expect\b", re.M)


def run(command: list[str], cwd: Path) -> subprocess.CompletedProcess[str] | None:
    """Run `command`; print why and return None when it fails."""
    result = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
    if result.returncode != 0:
        print("FAILED")
        print(f"    $ {' '.join(command)}")
        for line in (result.stdout + result.stderr).strip().splitlines():
            print(f"    {line}")
        return None
    return result


def check(app: Path, roc: str) -> bool:
    cwd = app.parent
    name = app.name
    for command in ([roc, "fmt", "--check", name], [roc, "check", name]):
        if run(command, cwd) is None:
            return False
    if EXPECT_RE.search(app.read_text(encoding="utf-8")) and run([roc, "test", name], cwd) is None:
        return False
    result = run([roc, name], cwd)
    if result is None:
        return False
    expected_file = app.with_suffix(".expected")
    if expected_file.is_file():
        expected = expected_file.read_text(encoding="utf-8")
        if result.stdout != expected:
            print("FAILED")
            print(f"    stdout of {name} does not match {expected_file.name}")
            print(f"    expected: {expected!r}")
            print(f"    actual:   {result.stdout!r}")
            return False
    return True


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("filter", nargs="?", default="", help="only check files whose name contains this")
    args = parser.parse_args(argv)

    apps = [app for app in sorted(EXAMPLES.glob("*.roc")) if args.filter in app.name]
    if not apps:
        print(f"no documentation examples match {args.filter!r}", file=sys.stderr)
        return 1

    roc = os.environ.get("ROC", "roc")
    failed = []
    for app in apps:
        print(f"{app.relative_to(ROOT)} ...", end=" ", flush=True)
        if check(app, roc):
            print("ok")
        else:
            failed.append(app.name)

    if failed:
        print(f"\n{len(failed)} documentation example(s) failed: {', '.join(failed)}", file=sys.stderr)
        return 1
    print(f"\nAll {len(apps)} documentation examples format-check, check, test and run.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
