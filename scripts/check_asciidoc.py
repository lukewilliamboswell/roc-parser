#!/usr/bin/env python3
"""Compare the AsciiDoc module with the pinned Asciidoctor, and smoke-test its
performance harness.

    check_asciidoc.py                                    # the pull-request sizes
    check_asciidoc.py --generated 2000 --mutations 2000 --seed 13

It installs the hash-pinned Asciidoctor gem (scripts/asciidoc/setup_oracle.py),
builds the public API probe with `roc` ($ROC or `roc` on PATH), checks the
fixtures, generated documents, mutations and the manual's own sources, then
runs the local performance harness in smoke mode. Evidence and reproducers go
under .roc-parser-tmp/asciidoc/. See scripts/asciidoc/README.adoc.
"""

from __future__ import annotations

import argparse
import shlex
import subprocess
import sys
from typing import Sequence

try:
    from ._common import ROOT, roc_command
except ImportError:
    from _common import ROOT, roc_command


WORK = ROOT / ".roc-parser-tmp" / "asciidoc"


def commands(roc: str, generated: int, mutations: int, seed: int, performance: bool) -> list[list[str]]:
    probe = WORK / "probe"
    steps = [
        [sys.executable, "scripts/asciidoc/setup_oracle.py"],
        [roc, "build", "--no-cache", "--opt=speed", "scripts/asciidoc/probe.roc", f"--output={probe}"],
        [sys.executable, "scripts/review_asciidoc.py", "check", "--require-complete", "--compare-html",
         "--probe", str(probe), "--generated", str(generated), "--mutations", str(mutations),
         "--seed", str(seed), "--corpus", "docs"],
    ]
    if performance:
        steps.append([sys.executable, "scripts/check_asciidoc_performance.py", "--smoke", "--roc", roc,
                      "--output", str(WORK / "performance-smoke")])
    return steps


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--generated", type=int, default=200, help="generated documents (default: 200)")
    parser.add_argument("--mutations", type=int, default=200, help="mutated documents (default: 200)")
    parser.add_argument("--seed", type=int, default=1, help="seed for generation and mutation (default: 1)")
    parser.add_argument("--skip-performance", action="store_true", help="omit the performance harness smoke run")
    args = parser.parse_args(argv)
    WORK.mkdir(parents=True, exist_ok=True)
    for command in commands(roc_command(), args.generated, args.mutations, args.seed, not args.skip_performance):
        print(f"+ {shlex.join(command)}", flush=True)
        status = subprocess.run(command, cwd=ROOT, check=False).returncode
        if status:
            print(f"AsciiDoc check failed; evidence is under {WORK}", file=sys.stderr)
            return status
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
