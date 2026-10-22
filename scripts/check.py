#!/usr/bin/env python3
"""Run everything a pull request must pass: `blueprint run check`.

    check.py                  # every section, stopping at the first failure
    check.py fuzz asciidoc    # chosen sections
    check.py --keep-going     # every section, then report each failure
    check.py --list

The package is built and tested with `roc` ($ROC or `roc` on PATH), which in
CI is the latest Roc nightly. Fuzz targets, probes and oracles are smoke-sized
here; the scheduled fuzz.yml and bench.yml workflows run the long campaigns.
Scratch files and evidence go under .roc-parser-tmp/.
"""

from __future__ import annotations

import argparse
import os
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import Sequence

try:
    from ._common import ROOT, roc_command
except ImportError:
    from _common import ROOT, roc_command


PY = sys.executable


@dataclass(frozen=True)
class Section:
    summary: str
    commands: tuple[tuple[str, ...], ...]


SECTIONS = {
    "scripts": Section(
        "Python automation tests and the repository policy",
        ((PY, "-m", "unittest", "discover", "-s", "scripts/tests", "-p", "test_*.py"),),
    ),
    "package": Section(
        "check, format and test the package, compile-time literals, roc docs, and a release dry run: "
        "bundle, examples archive and its spec run against a loopback bundle",
        ((PY, "scripts/all_tests.py"),),
    ),
    "examples": Section(
        "the repository's examples against the working tree, compared with tests/examples.json",
        ((PY, "scripts/test_bundle_examples.py", "--checkout"),),
    ),
    "docs": Section(
        "the manual's Roc examples and the API reference audit",
        ((PY, "scripts/check_doc_examples.py"), (PY, "scripts/build_docs.py", "--check")),
    ),
    "fuzz": Section(
        "build every registered fuzz target and run a short smoke campaign from its seeds",
        ((PY, "scripts/run_fuzz.py", "smoke", "all"),),
    ),
    "conformance": Section(
        "the fast conformance checks against each format's pinned reference implementation",
        ((PY, "scripts/check_conformance.py"),),
    ),
    "asciidoc": Section(
        "AsciiDoc against the pinned Asciidoctor, and the performance harness smoke run",
        ((PY, "scripts/check_asciidoc.py"),),
    ),
    "roc-syntax": Section(
        "RocSyntax against the compiler's own parser, recovery, mutations and the performance protocol",
        ((PY, "scripts/check_roc_syntax.py", "--skip-fuzz", "--reference-performance", "smoke"),),
    ),
    "bench": Section(
        "benchmark smoke run of the roc-parser drivers; timings here are not measurements",
        ((PY, "scripts/bench.py", "--quick", "--langs", "roc", "--timeout", "60", "--fail-on-error"),),
    ),
}


def select(requested: Sequence[str]) -> list[str]:
    unknown = sorted(set(requested) - set(SECTIONS))
    if unknown:
        raise ValueError(f"unknown section(s): {', '.join(unknown)}; choose from {', '.join(SECTIONS)}")
    return list(dict.fromkeys(requested)) if requested else list(SECTIONS)


def run_section(name: str, section: Section, env: dict[str, str]) -> int:
    for command in section.commands:
        print(f"+ {shlex.join(command)}", flush=True)
        status = subprocess.run(command, cwd=ROOT, env=env, check=False).returncode
        if status:
            return status
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("sections", nargs="*", metavar="SECTION", help="sections to run (default: all)")
    parser.add_argument("--keep-going", action="store_true", help="run every section even after a failure")
    parser.add_argument("--list", action="store_true", help="list the sections and stop")
    args = parser.parse_args(argv)
    if args.list:
        for name, section in SECTIONS.items():
            print(f"{name:12} {section.summary}")
        return 0
    try:
        names = select(args.sections)
    except ValueError as error:
        parser.error(str(error))

    env = os.environ.copy()
    env["ROC"] = roc_command()
    version = subprocess.run([env["ROC"], "version"], capture_output=True, text=True, check=False)
    if version.returncode:
        print(f"error: `{env['ROC']} version` failed; put the latest Roc nightly on PATH or set ROC", file=sys.stderr)
        return 1
    print(version.stdout.strip(), flush=True)

    grouped = os.environ.get("GITHUB_ACTIONS") == "true"
    results: list[tuple[str, int, float]] = []
    for index, name in enumerate(names, start=1):
        section = SECTIONS[name]
        title = f"[{index}/{len(names)}] {name}: {section.summary}"
        print(f"::group::{title}" if grouped else f"\n==> {title}", flush=True)
        started = time.monotonic()
        status = run_section(name, section, env)
        elapsed = time.monotonic() - started
        if grouped:
            print("::endgroup::", flush=True)
        results.append((name, status, elapsed))
        print(f"<== {name}: {'passed' if status == 0 else f'FAILED ({status})'} in {elapsed:.0f}s", flush=True)
        if status and not args.keep_going:
            break

    print("\nSection        Result   Time", flush=True)
    for name, status, elapsed in results:
        print(f"{name:14} {'passed' if status == 0 else 'FAILED':8} {elapsed:5.0f}s")
    skipped = [name for name in names if name not in {result[0] for result in results}]
    if skipped:
        print(f"Not run after the failure: {', '.join(skipped)}")
    print(f"Total {sum(result[2] for result in results):.0f}s")
    failed = [name for name, status, _ in results if status]
    if failed:
        print(f"check failed: {', '.join(failed)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
