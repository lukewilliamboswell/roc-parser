#!/usr/bin/env python3
"""Extract a reproducible exploratory corpus from pinned Zig compiler snapshots.

Read Git objects, never the source checkout's working files. This imports source
and provenance, not compiler snapshot output as expected parser results. Run the
corpus through the persistent oracle to establish current parser coverage.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import subprocess

REFERENCE_COMMIT = "7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5"
ROOT = Path(__file__).resolve().parents[1]
# These snapshot types all invoke the file parser in the pinned snapshot tool.
FILE_TYPES = {"file", "snippet", "mono", "package", "platform", "app", "reporting"}


def section(text: str, heading: str, language: str) -> str:
    pattern = rf"^# {re.escape(heading)}\n~~~{re.escape(language)}\n(.*?)^~~~(?:\n|$)"
    match = re.search(pattern, text, re.MULTILINE | re.DOTALL)
    if match is None:
        raise ValueError(f"missing fenced {heading} section")
    return match.group(1)


def snapshot_case(path: str, blob: str, contents: bytes) -> tuple[dict | None, str | None]:
    text = contents.decode("utf-8")
    # Do not use universal-newline translation: source bytes determine all spans.
    metadata = {}
    for line in section(text, "META", "ini").splitlines():
        key, separator, value = line.partition("=")
        if separator:
            metadata[key.strip()] = value.strip()
    kind = metadata.get("type")
    if kind == "expr":
        mode = "expr"
    elif kind in FILE_TYPES:
        mode = "file"
    else:
        return None, f"unsupported_snapshot_mode:{kind}"
    source = section(text, "SOURCE", "roc")
    if metadata.get("source_escapes") == "true":
        # Exact pinned snapshot tool transformation; do not decode other escapes.
        source = source.replace("\\r", "\r")
    case = {
        "id": "upstream/" + path.removeprefix("test/snapshots/").removesuffix(".md"),
        "mode": mode,
        "source": source,
        "rule": f"upstream/{mode}",
        "coverage": "exploratory",
        "source_sha256": hashlib.sha256(source.encode("utf-8")).hexdigest(),
        "upstream": {"path": path, "blob": blob, "type": kind,
                     "snapshot_skip": metadata.get("skip") == "true"},
    }
    return case, None


def git(repository: Path, *args: str, input_bytes: bytes | None = None) -> bytes:
    return subprocess.run(["git", "-C", str(repository), *args], input=input_bytes,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True).stdout


def import_snapshots(repository: Path, *, limit: int | None = None,
                     name_filter: str = "") -> dict:
    resolved = git(repository, "rev-parse", REFERENCE_COMMIT + "^{commit}").decode().strip()
    if resolved != REFERENCE_COMMIT:
        raise ValueError("reference commit identity differs")
    listing = git(repository, "ls-tree", "-rz", REFERENCE_COMMIT, "test/snapshots/")
    entries = []
    for item in listing.split(b"\0"):
        if not item:
            continue
        info, raw_path = item.split(b"\t", 1)
        _, object_type, blob = info.split()
        path = raw_path.decode("utf-8")
        if object_type == b"blob" and path.endswith(".md") and name_filter in path:
            entries.append((path, blob.decode("ascii")))
    entries.sort()
    # One Git process reads all immutable blobs; avoid a subprocess per case.
    payload = git(repository, "cat-file", "--batch",
                  input_bytes="".join(blob + "\n" for _, blob in entries).encode("ascii"))
    cursor = 0
    cases, skipped = [], []
    for path, blob in entries:
        line_end = payload.index(b"\n", cursor)
        actual_blob, object_type, raw_size = payload[cursor:line_end].split()
        if actual_blob.decode() != blob or object_type != b"blob":
            raise ValueError(f"unexpected Git object response for {path}")
        start = line_end + 1
        end = start + int(raw_size)
        contents = payload[start:end]
        if payload[end:end + 1] != b"\n":
            raise ValueError(f"truncated Git object response for {path}")
        cursor = end + 1
        if path.endswith("/README.md"):
            skipped.append({"path": path, "reason": "documentation"})
            continue
        try:
            case, reason = snapshot_case(path, blob, contents)
        except ValueError as error:
            # Imported coverage must never silently drop a broken source section.
            raise ValueError(f"{path}: {error}") from error
        if case is None:
            skipped.append({"path": path, "reason": reason})
        elif limit is None or len(cases) < limit:
            cases.append(case)
        else:
            skipped.append({"path": path, "reason": "requested_case_limit"})
    if cursor != len(payload):
        raise ValueError("unexpected trailing Git object data")
    return {"version": 1, "syntax": "current-zig", "reference_commit": REFERENCE_COMMIT,
            "coverage": sorted({case["rule"] for case in cases}), "cases": cases,
            "provenance": {"source": "roc-lang/roc test/snapshots", "filter": name_filter,
                           "case_limit": limit, "skipped": skipped}}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc-source", required=True, type=Path,
                        help="compiler Git repository containing the pinned commit")
    parser.add_argument("--output", type=Path,
                        default=ROOT / ".roc-parser-tmp/roc-syntax-review/upstream.json")
    parser.add_argument("--filter", default="", help="substring of upstream snapshot path")
    parser.add_argument("--max-cases", type=int)
    args = parser.parse_args(argv)
    if args.max_cases is not None and args.max_cases < 1:
        parser.error("--max-cases must be positive")
    result = import_snapshots(args.roc_source, limit=args.max_cases, name_filter=args.filter)
    if not result["cases"]:
        parser.error("no supported snapshot cases match the requested filter")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")
    skipped = Counter(item["reason"] for item in result["provenance"]["skipped"])
    print(json.dumps({"cases": len(result["cases"]), "skipped": dict(skipped),
                      "reference_commit": REFERENCE_COMMIT, "output": str(args.output)}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
