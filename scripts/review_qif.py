#!/usr/bin/env python3
"""Pinned Quiffen differential checks, cross-writer checks and replayable campaigns.

QIF has no universal date convention. Both probes use explicit day-first dates.
Normalization compares exact Decimal values and category hierarchy; it does not
hide malformed framing, alter identifiers, or discard unknown mismatches.
"""
from __future__ import annotations

import argparse
from decimal import Decimal, InvalidOperation
from datetime import datetime
import re
import json
import os
from pathlib import Path
import random
import subprocess
import sys
import time

try:
    from _oracle_cache import OracleCache, add_arguments as add_cache_arguments, from_arguments as cache_from_arguments
except ModuleNotFoundError:
    from scripts._oracle_cache import OracleCache, add_arguments as add_cache_arguments, from_arguments as cache_from_arguments

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts/qif"
WORK = ROOT / ".roc-parser-tmp/qif-review"

ORACLE_CACHE = OracleCache(ROOT / ".roc-parser-tmp/oracle-cache")


def decimal(text):
    value = format(Decimal(str(text)), "f")
    if "." in value:
        value = value.rstrip("0").rstrip(".")
    return "0" if value == "-0" else value


def normalize(rows):
    for row in rows:
        row["amount"] = decimal(row["amount"])
        for split in row["splits"]:
            split["amount"] = decimal(split["amount"])
    return rows


def _oracle_uncached(text):
    from quiffen import Qif
    from quiffen.core.qif import ParserException
    try:
        qif = Qif.parse_string(text.replace("\r\n", "\n").replace("\r", "\n"), day_first=True)
        rows = []
        for name, account in qif.accounts.items():
            for transactions in account.transactions.values():
                for row in transactions:
                    category = row.category.hierarchy if row.category else ""
                    rows.append({"account": "" if name == "Quiffen Default Account" else name,
                                 "date": [row.date.year, row.date.month, row.date.day],
                                 "amount": str(row.amount), "payee": row.payee or "",
                                 "memo": row.memo or "", "category": category,
                                 "number": str(row.check_number) if row.check_number is not None else "",
                                 "cleared": row.cleared or "",
                                 "splits": [{"category": s.category.hierarchy if s.category else "",
                                             "memo": s.memo or "", "amount": str(s.amount)} for s in row.splits]})
        written = qif.to_qif(date_format="%d/%m/%Y")
        # Quiffen invents a named default account for headerless source. Remove
        # only that generated block when writing back a source with no account.
        if "!Account" not in text:
            written = written.replace("!Account\nNQuiffen Default Account\nDThe default account created by Quiffen when no other accounts were present\n^\n", "")
        return {"status": "ok", "rows": normalize(rows), "written": written}
    except (ValueError, ParserException, InvalidOperation) as error:
        return {"status": "error", "message": str(error)}


def oracle_date_defaults(text):
    """Dateutil consults today's local date for partial/two-digit dates."""
    dates = [line[1:].strip() for line in text.splitlines() if line.startswith("D")]
    complete = re.compile(r"(?:[0-9]{1,2}[/\-][0-9]{1,2}[/\-][0-9]{4}|[0-9]{4}[/\-][0-9]{1,2}[/\-][0-9]{1,2})$")
    if all(complete.fullmatch(value) for value in dates):
        return {"date_defaults": "unused-complete-four-digit-dates"}
    now = datetime.now().astimezone()
    return {"local_date": now.date().isoformat(), "local_timezone": now.tzname(), "utc_offset_seconds": now.utcoffset().total_seconds()}


def oracle(text):
    return ORACLE_CACHE.answer("quiffen.read-write", text.encode("utf-8"),
                              {"day_first": True, "output_date_format": "%d/%m/%Y", "newline_normalization": "CRLF/CR to LF", **oracle_date_defaults(text)},
                              lambda: _oracle_uncached(text), adapters=(_oracle_uncached, oracle_date_defaults),
                              requirements=DATA / "requirements.txt", primary="quiffen")


def probe(binary, text):
    result = subprocess.run([str(binary)], input=text.encode(), capture_output=True, timeout=20)
    if result.returncode:
        return {"status": "crash", "stderr": result.stderr.decode(errors="replace")[-1000:]}
    try:
        value = json.loads(result.stdout)
        if value["status"] == "ok":
            normalize(value["rows"])
        return value
    except Exception as error:
        return {"status": "protocol_error", "message": str(error)}


def compare(binary, case):
    text = case["input"]
    actual = probe(binary, text)
    if "policy" in case:
        if actual["status"] != case["expected"]:
            return {"actual": actual, "expected": case["expected"]}
        if "expected_number" in case and actual["rows"][0]["number"] != case["expected_number"]:
            return {"actual": actual, "expected_number": case["expected_number"]}
        return None
    expected = oracle(text)
    if actual["status"] != expected["status"]:
        return {"actual": actual, "expected": expected}
    if actual["status"] != "ok":
        return None
    if actual["rows"] != expected["rows"]:
        return {"actual": actual, "expected": expected}
    # Both independent cross-writer directions, compared on supported fields.
    external_read = oracle(actual["written"])
    our_read = probe(binary, expected["written"])
    if external_read.get("rows") != actual["rows"] or our_read.get("rows") != expected["rows"]:
        return {"cross_external": external_read, "cross_ours": our_read, "expected": expected}
    return None


def generated(rng, n):
    rows = []
    for i in range(n):
        rows += ["D27/08/2024", f"T-{rng.randrange(10000)}.{rng.randrange(100):02d}",
                 "P" + rng.choice(["Ada", "Café", "€ shop", "日本", "a,b", '"quoted"']),
                 "M" + str(i), "LFood", "^"]
    newline = rng.choice(["\n", "\r\n", "\r"])
    return newline.join(["!Type:Bank", *rows]) + newline


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["check", "campaign", "replay", "corpus"], nargs="?", default="check")
    parser.add_argument("paths", nargs="*", type=Path)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--binary", type=Path, default=WORK / "probe")
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--smoke", action="store_true")
    parser.add_argument("--seconds", type=float, default=60)
    parser.add_argument("--seed", type=int, default=20241117)
    parser.add_argument("--random", type=int, default=30)
    add_cache_arguments(parser)
    args = parser.parse_args()
    global ORACLE_CACHE
    ORACLE_CACHE = cache_from_arguments(args, ROOT)
    WORK.mkdir(parents=True, exist_ok=True)
    if not args.no_build:
        subprocess.run([args.roc, "build", "--opt=dev", str(DATA / "probe.roc"), f"--output={args.binary}"], cwd=ROOT, check=True)
    cases = json.loads((DATA / "cases.json").read_text())
    if args.command in ("replay", "corpus"):
        cases = []
        for path in args.paths:
            for source in sorted(path.iterdir()) if path.is_dir() else [path]:
                try:
                    entry = json.loads(source.read_text())
                    cases.append(entry.get("case", entry))
                except (ValueError, UnicodeError):
                    cases.append({"name": source.name, "input": source.read_text()})
    rng = random.Random(args.seed)
    cases += [{"name": f"generated-{i}", "input": generated(rng, rng.randrange(1, 12))} for i in range(args.random)]
    if args.command == "campaign":
        seconds = 1 if args.smoke else max(1, args.seconds / 10)
        for target in ("qif-raw", "qif-roundtrip", "qif-malformed", "qif-stream", "qif-ceilings"):
            command = [sys.executable, "scripts/run_fuzz.py", "smoke" if args.smoke else "campaign", target]
            command += ["--runs", "100"] if args.smoke else ["--seconds", str(int(seconds))]
            subprocess.run(command, cwd=ROOT, check=True)
    known = {entry["input"]: entry["reason"] for entry in json.loads((DATA / "known-failures.json").read_text())}
    failures = []
    start = time.monotonic()
    index = 0
    while cases or (args.command == "campaign" and not args.smoke and time.monotonic() - start < args.seconds / 2):
        case = cases.pop(0) if cases else {"name": f"campaign-{index}", "input": generated(rng, rng.randrange(1, 30))}
        failure = compare(args.binary, case)
        if failure and case["input"] not in known:
            report = {"case": case, "seed": args.seed, "failure": failure}
            (WORK / f"failure-{args.seed}-{index}.json").write_text(json.dumps(report, ensure_ascii=False, indent=2))
            failures.append(report)
        index += 1
    for text, reason in known.items():
        if compare(args.binary, {"name": "known", "input": text}) is None:
            failures.append({"stale_known_failure": text, "reason": reason})
    print(json.dumps({"cases": index, "failures": len(failures), "known": len(known), "seed": args.seed}))
    for failure in failures[:5]:
        print(json.dumps(failure, ensure_ascii=False))
    return bool(failures)


if __name__ == "__main__":
    raise SystemExit(main())
