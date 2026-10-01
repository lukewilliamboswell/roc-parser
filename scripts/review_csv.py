#!/usr/bin/env python3
"""Differential review of package/CSV.roc against Python's csv module.

The oracle is the standard library `csv.reader` configured for RFC 4180
(delimiter ',', quotechar '"', doublequote=True, strict=True, no
skipinitialspace). Each case is fed to scripts/csv/probe.roc on stdin; the probe
prints the records it decoded (via the public CSV.parse_str API) as JSON.

Documented dialect decisions where the oracle is normalized:

* A blank line is one record holding one empty field ([""]), as RFC 4180's
  grammar says. Python returns [] for it; we map [] to [""] before comparing.
* Everything else (LF/CRLF/CR line breaks, optional final line break, bare
  quotes inside unquoted fields being literal, no BOM stripping, no whitespace
  trimming) is compared verbatim.

Known failures live in scripts/csv/known-failures.json with explicit reasons;
the run fails on any unexpected divergence or on a known failure that now
passes. Only the standard library is needed; set ROC to choose the compiler.

Usage: python3 scripts/review_csv.py [--random N] [--seed S] [--corpus DIR]
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import os
from pathlib import Path
import random
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "csv"
WORK = ROOT / ".roc-parser-tmp" / "csv-review"
ROC = Path(os.environ.get("ROC", "roc"))


def oracle(text: str) -> dict:
    try:
        rows = list(csv.reader(io.StringIO(text, newline=""), delimiter=",", quotechar='"', doublequote=True, strict=True, skipinitialspace=False))
    except csv.Error as error:
        return {"status": "error", "message": str(error)}
    return {"status": "ok", "records": [row if row else [""] for row in rows]}


def probe(binary: Path, text: str) -> dict:
    result = subprocess.run([str(binary)], input=text.encode("utf-8"), capture_output=True, timeout=20)
    if result.returncode != 0:
        return {"status": "crash", "stderr": result.stderr.decode("utf-8", "replace")[-400:]}
    try:
        return json.loads(result.stdout.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError):
        return {"status": "protocol_error", "stdout": result.stdout[-400:].decode("utf-8", "replace")}


def same(expected: dict, actual: dict) -> bool:
    if expected["status"] != actual["status"]:
        return False
    return expected["status"] != "ok" or expected["records"] == actual["records"]


ALPHABET = ["a", "b", "1", " ", ",", ",", '"', '"', '""', "\n", "\r\n", "\r", "\t", "é", "😀", "\x00", "\x7f", "﻿", "'", "\\"]


def random_case(rng: random.Random) -> str:
    return "".join(rng.choice(ALPHABET) for _ in range(rng.randrange(0, 24)))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--random", type=int, default=2000)
    parser.add_argument("--seed", type=int, default=4180)
    parser.add_argument("--corpus", type=Path, help="also check every UTF-8 file in this directory")
    parser.add_argument("--binary", type=Path, default=WORK / "probe")
    parser.add_argument("--no-build", action="store_true")
    args = parser.parse_args()

    if not args.no_build:
        args.binary.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([str(ROC), "build", str(DATA / "probe.roc"), f"--output={args.binary}"], check=True, cwd=ROOT)

    cases: list[tuple[str, str]] = [(c["name"], c["input"]) for c in json.loads((DATA / "cases.json").read_text(encoding="utf-8"))]
    rng = random.Random(args.seed)
    cases += [(f"random-{i}", random_case(rng)) for i in range(args.random)]
    if args.corpus:
        for path in sorted(args.corpus.iterdir()):
            try:
                cases.append((f"corpus-{path.name}", path.read_bytes().decode("utf-8")))
            except UnicodeDecodeError:
                pass

    known = {entry["input"]: entry["reason"] for entry in json.loads((DATA / "known-failures.json").read_text(encoding="utf-8"))}
    unexpected, still_known, seen_known = [], 0, set()
    for name, text in cases:
        expected, actual = oracle(text), probe(args.binary, text)
        if same(expected, actual):
            continue
        if text in known:
            still_known += 1
            seen_known.add(text)
            continue
        unexpected.append({"name": name, "input": text, "expected": expected, "actual": actual})

    stale = [text for text in known if text not in seen_known and not same(oracle(text), probe(args.binary, text))]
    fixed = [text for text in known if same(oracle(text), probe(args.binary, text))]
    for failure in unexpected[:20]:
        print(json.dumps(failure, ensure_ascii=False))
    print(f"cases={len(cases)} unexpected={len(unexpected)} known={still_known} fixed_known={len(fixed)}")
    for text in fixed:
        print("known failure now passes (remove it):", json.dumps(text, ensure_ascii=False))
    return 1 if unexpected or fixed else 0


if __name__ == "__main__":
    sys.exit(main())
