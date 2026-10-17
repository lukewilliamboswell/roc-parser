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

REFERENCE_COMMIT = "0d4b0ab02f9239448d034db17b4e63051a7c57ae"
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
    elif kind in FILE_TYPES or (kind is not None and kind.startswith("file:")):
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


def snapshot_cases(path: str, blob: str, contents: bytes) -> tuple[list[dict], str | None]:
    """Extract parse-only inputs using the pinned snapshot runner's transforms.

    main.zig:1008 maps header/statement goals; :3639 parses multi-file SOURCE;
    :4974 splits REPL input on literal » then ASCII-trims each nonempty part.
    REPL mode dispatch is the worker's job, not a text heuristic here.
    """
    text = contents.decode('utf-8')
    metadata = dict(line.partition('=')[::2] for line in section(text, 'META', 'ini').splitlines() if '=' in line)
    metadata = {key.strip(): value.strip() for key, value in metadata.items()}
    kind = metadata.get('type')
    if kind not in {'repl', 'header', 'statement', 'dev_object', 'docs'}:
        case, reason = snapshot_case(path, blob, contents)
        return [case] if case else [], reason
    case_base = {'mode': kind, 'coverage': 'exploratory',
                 'upstream': {'path': path, 'blob': blob, 'type': kind,
                              'snapshot_skip': metadata.get('skip') == 'true'}}
    base_id = 'upstream/' + path.removeprefix('test/snapshots/').removesuffix('.md')
    cases = []
    def add(source, mode, suffix, start, end, extraction):
        original = source
        if metadata.get('source_escapes') == 'true':
            source = source.replace('\\r', '\r')
        cases.append({**case_base, 'id': base_id + suffix, 'mode': mode, 'rule': 'upstream/' + mode,
                      'source': source, 'source_sha256': hashlib.sha256(source.encode()).hexdigest(),
                      'upstream': {**case_base['upstream'], 'extraction': {
                          'kind': extraction, 'snapshot_byte_start': len(text[:start].encode()),
                          'snapshot_byte_end': len(text[:end].encode()),
                          'extracted_sha256': hashlib.sha256(original.encode()).hexdigest(),
                          'source_escapes': metadata.get('source_escapes') == 'true'}}})
    if kind in {'header', 'statement', 'repl'}:
        match = re.search(r'^# SOURCE\n~~~roc\n(.*?)^~~~(?:\n|$)', text, re.M | re.S)
        if match is None:
            raise ValueError('missing fenced SOURCE section')
        raw = match.group(1)
        if kind != 'repl':
            add(raw, kind, '', match.start(1), match.end(1), kind)
        else:
            # Content applies source_escapes before the REPL runner trims inputs.
            # Keep an index map so audit spans still refer to original blob bytes.
            decoded, boundaries = [], [0]
            offset = 0
            while offset < len(raw):
                if metadata.get('source_escapes') == 'true' and raw[offset:offset + 2] == '\\r':
                    decoded.append('\r')
                    offset += 2
                else:
                    decoded.append(raw[offset])
                    offset += 1
                boundaries.append(offset)
            decoded = ''.join(decoded)
            # splitSequence is intentionally not restricted to line starts.
            cursor = 0
            for index, part in enumerate(decoded.split('»')):
                start = cursor
                cursor += len(part) + 1
                if index == 0:
                    continue
                source = part.strip(' \t\r\n')
                if not source:
                    continue
                trim = len(part) - len(part.lstrip(' \t\r\n'))
                first = boundaries[start + trim]
                last = boundaries[start + trim + len(source)]
                add(raw[first:last], 'repl', f'/input-{index}', match.start(1) + first,
                    match.start(1) + last, 'repl_ascii_trim')
    else:
        marker = '# SOURCE\n'
        if marker not in text:
            raise ValueError('missing multi-file SOURCE section')
        start = text.index(marker) + len(marker)
        end = start
        fenced = False
        external_headings = 0
        for line in text[start:].splitlines(keepends=True):
            if not fenced and line.startswith('# '):
                break
            if not fenced and line.startswith('## '):
                external_headings += 1
            if line.startswith('~~~'):
                fenced = not fenced
            end += len(line)
        body = text[start:end]
        matches = list(re.finditer(r'^## ([^\n]+)\n~~~roc\n(.*?)~~~', body, re.M | re.S))
        if len(matches) != external_headings:
            raise ValueError('unhandled multi-file SOURCE heading or fence')
        for index, match in enumerate(matches):
            filename = match.group(1).strip(' \t\r')
            if not filename.endswith('.roc'):
                raise ValueError('non-Roc source file in parse-only inventory: ' + filename)
            add(match.group(2), 'file', f'/file-{index}-{filename}', start + match.start(2),
                start + match.end(2), 'multi_file')
            cases[-1]['upstream']['filename'] = filename
    return cases, None if cases else 'empty_parse_input_inventory:' + str(kind)

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
            extracted, reason = snapshot_cases(path, blob, contents)
        except ValueError as error:
            # Imported coverage must never silently drop a broken source section.
            raise ValueError(f"{path}: {error}") from error
        if not extracted:
            skipped.append({"path": path, "reason": reason})
        for case in extracted:
            if limit is None or len(cases) < limit:
                cases.append(case)
            else:
                skipped.append({"path": path, "reason": "requested_case_limit", "id": case['id']})
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
