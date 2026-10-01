#!/usr/bin/env python3
"""Check, test, format-check and run every docs example, comparing stdout with its .expected file.

Usage: python3 docs/examples/verify.py [--write] [NAME.roc ...]
Set ROC to the compiler path (defaults to `roc`).
"""
import os
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROC = os.environ.get("ROC", "roc")


def main() -> int:
    args = sys.argv[1:]
    write = "--write" in args
    names = [a for a in args if a != "--write"] or [p.name for p in sorted(HERE.glob("*.roc"))]
    failed = False
    for name in names:
        for cmd in (["fmt", "--check", name], ["check", name], ["test", name]):
            r = subprocess.run([ROC, *cmd], cwd=HERE, capture_output=True, text=True)
            if r.returncode != 0:
                failed = True
                print(f"FAIL {' '.join(cmd)}\n{(r.stdout + r.stderr)[-4000:]}")
        expected = HERE / (Path(name).stem + ".expected")
        r = subprocess.run([ROC, name], cwd=HERE, capture_output=True, text=True)
        if write:
            expected.write_text(r.stdout)
        if r.returncode != 0 or not expected.exists() or expected.read_text() != r.stdout:
            failed = True
            print(f"FAIL run {name} (exit {r.returncode})\n--- stdout\n{r.stdout}\n--- stderr\n{r.stderr[-4000:]}")
        else:
            print(f"ok {name}")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
