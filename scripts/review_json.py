#!/usr/bin/env python3
"""Reproducible RFC 8259 conformance checks and review campaigns for package/JsonValue.roc.

Oracle: Python's stdlib ``json`` module, configured so that it models
JsonValue.Options exactly:

- numbers stay as their exact text: ``parse_int`` and ``parse_float`` both
  return a tagged ``("number", text)`` tuple, never an int or float;
- ``parse_constant`` accepts NaN and Infinity only with ``non_finite``;
- ``strict=not control_characters`` decides raw U+0000 to U+001F in strings;
- ``object_pairs_hook`` keeps member order and applies the duplicate mode to
  keys compared after unescaping;
- lone ``\\u`` surrogates, which ``json`` decodes silently, are found
  afterwards and rejected or replaced with U+FFFD;
- the byte order mark, comments, trailing commas, ``top_level``, the size
  limits, the nesting limit and I-JSON numbers are handled here, in Python
  (I-JSON with ``Decimal`` and ``float``); ``RecursionError`` is a rejection.

The writer oracle is written by hand, mirroring ``json.dumps(value,
ensure_ascii=False, sort_keys=..., indent=n or None)`` with separators
``(",", ":")`` when compact and ``(",", ": ")`` when indented, and it escapes
strings with ``json.dumps(text, ensure_ascii=False)`` itself.

Inputs come from scripts/json/cases.json, the vendored JSONTestSuite
(scripts/json/suite.json), and real documents fetched by URL and pinned by
SHA-256. Known failures are explicit and carry a reason; crashes, timeouts,
protocol errors and diagnostics outside the input are never baseline-able.

Usage:
  python3 scripts/review_json.py check [--baseline scripts/json/known-failures.json]
  python3 scripts/review_json.py check --baseline scripts/json/known-failures.json --record-known-failures
  python3 scripts/review_json.py corpus TARGET CORPUS_DIR
  python3 scripts/review_json.py campaign [--seconds N] [--smoke] [--seed N]
  python3 scripts/review_json.py replay REPRODUCER.json
  python3 scripts/review_json.py vendor-suite JSONTESTSUITE_CLONE

`corpus` renders each file with the fuzz target's `show` command, which must
print `JSON-HEX: <hex>` and may print `JSON-OPTIONS: <probe flags>`.
"""

from __future__ import annotations

import argparse
from collections import Counter
from decimal import Decimal
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import time
import urllib.request
from typing import Any, Iterator

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "json"
DEFAULT_WORK = ROOT / ".roc-parser-tmp" / "json-review"
FATAL = {"crash", "timeout", "protocol_error", "invalid_diagnostic"}
FUZZ_TARGETS = ("json-raw", "json-options", "json-roundtrip")
WRITERS = ("compact", "pretty", "sorted")
WRITE_DEPTH = 128
MAX_GENERATED_BYTES = 4096
PROBE_TIMEOUT = 5.0
DOCUMENT_TIMEOUT = 60.0

DEFAULT_OPTIONS: dict[str, Any] = {
    "max_depth": 128,
    "max_items": None,
    "max_string_bytes": None,
    "max_number_digits": None,
    "comments": False,
    "trailing_commas": False,
    "bom": False,
    "non_finite": False,
    "control_characters": False,
    "lone_surrogates": "Reject",
    "duplicates": "Reject",
    "top_level": "Any",
    "interoperable_numbers": False,
    # WriteOptions for the probe's `pretty` output.
    "sort_keys": False,
    "indent": 2,
}
LIMITS = ("max_depth", "max_items", "max_string_bytes", "max_number_digits", "indent")
ENUMS = {
    "lone_surrogates": {"Reject": "reject", "Replace": "replace"},
    "duplicates": {"Reject": "reject", "CollapseIdentical": "collapse-identical", "KeepFirst": "keep-first", "KeepLast": "keep-last"},
    "top_level": {"Any": "any", "ObjectOrArray": "object-or-array", "Object": "object"},
}

# JSONTestSuite's y_object_duplicated_key* are valid RFC 8259 text, but strict
# JsonValue rejects duplicate keys by policy, so they are checked under KeepLast.
POLICY_DUPLICATES = re.compile(r"suite/y_object_duplicated_key")
POLICY_NOTE = "policy deviation: strict parsing rejects duplicate keys, so this case is checked with duplicates: KeepLast"

# Real documents, fetched at review time and pinned by SHA-256. They must be
# accepted with these options and agree with the oracle.
DOCUMENTS = [
    {
        "name": "spdx-3.0.1-json-schema",
        "url": "https://spdx.org/schema/3.0.1/spdx-json-schema.json",
        "sha256": "582c64e809d5b3ef9bd0c4de13a32391b47b0284a3e8d199569fb96f649234b1",
        # The published schema repeats "extension" and "prop_Element_extension" with equal values.
        "options": {"duplicates": "CollapseIdentical"},
    },
    {
        "name": "spdx-2.3-json-schema",
        # Tag v2.3.
        "url": "https://raw.githubusercontent.com/spdx/spdx-spec/aadf3b0b8dbbabdb4d880b0fc714255fea436ff7/schemas/spdx-schema.json",
        "sha256": "239208b7ac287b3cf5d9a9af23f9d69863971102a5e1587a27a398b43490b89b",
        "options": {},
    },
    {
        "name": "cyclonedx-1.7-json-schema",
        # Tag 1.7.
        "url": "https://raw.githubusercontent.com/CycloneDX/specification/4b3f59453366e27c8073fd24e98bf21ef8892c8e/schema/bom-1.7.schema.json",
        "sha256": "df472ef4aaf593904c479293723a1a5c191d6672715c93b3c0b5c318f3914221",
        "options": {},
    },
    {
        "name": "syft-1.54.1-sbom",
        "url": "https://github.com/anchore/syft/releases/download/v1.54.1/syft_1.54.1_linux_amd64.sbom",
        "sha256": "7c66b144b8941c69ac39d986fb7d8c98ee4294d4c8bf9d015ed649b50225a8fb",
        "options": {},
    },
    {
        "name": "trivy-0.76.0-cyclonedx-sbom",
        "url": "https://github.com/aquasecurity/trivy/releases/download/v0.76.0/bom.json",
        "sha256": "00564eea98e6e75f3dd6456b6ddcd51846a6793eef28291e35443787b2a4f9c9",
        "options": {},
    },
]

# A string (possibly unterminated), a line comment, a block comment, an
# unterminated block comment, a run of anything else, or a lone slash.
COMMENT_TOKENS = re.compile(r'"(?:[^"\\]|\\.)*"?|//[^\r\n]*|/\*.*?\*/|/\*|[^"/]+|/', re.S)
# A string, a whitespace run, a run of plain characters, or one structural character.
STRUCTURE_TOKENS = re.compile(r'"(?:[^"\\]|\\.)*"?|[ \t\r\n]+|[^"\s,:\[\]{}]+|.', re.S)
NUMBER = re.compile(r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\Z")
SURROGATES = re.compile("[\ud800-\udfff]")
NON_FINITE = ("NaN", "Infinity", "-Infinity")
I_JSON_INTEGER = Decimal(2**53 - 1)


class Rejected(ValueError):
    """The oracle refuses the input for a reason JsonValue.Options models."""


class Object:
    """An object built by the pairs hook, already converted to the tree form."""

    __slots__ = ("tree",)

    def __init__(self, tree: list) -> None:
        self.tree = tree


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def digest(value: Any) -> str:
    return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True).encode()).hexdigest()


# Options --------------------------------------------------------------------


def normalize_options(options: dict | None) -> dict:
    """Fill in defaults and check every field, so a typo cannot pass silently."""
    options = dict(options or {})
    unknown = sorted(set(options) - set(DEFAULT_OPTIONS))
    if unknown:
        raise ValueError("unknown option(s): " + ", ".join(unknown))
    result = {**DEFAULT_OPTIONS, **options}
    for key in LIMITS:
        value = result[key]
        if value is not None and (type(value) is not int or value < 0):
            raise ValueError(f"{key} must be a non-negative integer or null")
    if result["max_depth"] is None or result["indent"] is None:
        raise ValueError("max_depth and indent need a value")
    for key, default in DEFAULT_OPTIONS.items():
        if type(default) is bool and type(result[key]) is not bool:
            raise ValueError(f"{key} must be true or false")
    for key, names in ENUMS.items():
        if result[key] not in names:
            raise ValueError(f"{key} must be one of " + ", ".join(names))
    return result


def option_flags(options: dict | None) -> list[str]:
    """The probe arguments for options; defaults are left out."""
    options = normalize_options(options)
    flags = []
    for key, default in DEFAULT_OPTIONS.items():
        value = options[key]
        if value == default and type(value) is type(default):
            continue
        flag = "--" + key.replace("_", "-")
        if type(value) is bool:
            flags.append(flag)
        elif key in ENUMS:
            flags += [flag, ENUMS[key][value]]
        elif value is not None:
            flags += [flag, str(value)]
    return flags


def options_from_flags(flags: list[str] | str) -> dict:
    """Read probe arguments back into options, as fuzz targets print them."""
    if isinstance(flags, str):
        flags = flags.split()
    options: dict[str, Any] = {}
    names = {"--" + key.replace("_", "-"): key for key in DEFAULT_OPTIONS}
    index = 0
    while index < len(flags):
        key = names.get(flags[index])
        if key is None:
            raise ValueError("unknown probe flag " + flags[index])
        if type(DEFAULT_OPTIONS[key]) is bool:
            options[key] = True
            index += 1
            continue
        if index + 1 >= len(flags):
            raise ValueError(flags[index] + " needs a value")
        value = flags[index + 1]
        if key in ENUMS:
            reverse = {flag: tag for tag, flag in ENUMS[key].items()}
            if value not in reverse:
                raise ValueError(f"{flags[index]} does not take {value}")
            options[key] = reverse[value]
        elif re.fullmatch(r"[0-9]+", value):
            options[key] = int(value)
        else:
            raise ValueError(f"{flags[index]} takes a non-negative integer, not {value}")
        index += 2
    return normalize_options(options)


# Parsing oracle -------------------------------------------------------------


def strip_comments(text: str) -> str:
    """Turn `//` and `/* */` comments outside strings into a space."""
    pieces = []
    for match in COMMENT_TOKENS.finditer(text):
        token = match.group()
        if token == "/*":
            raise Rejected("unterminated block comment")
        pieces.append(" " if token.startswith(("//", "/*")) else token)
    return "".join(pieces)


def strip_trailing_commas(text: str) -> str:
    """Drop a comma that follows a value and comes right before `]` or `}`.

    A comma after `[`, `{`, `,` or `:` stays, so `[,]` and `[1,,]` still fail.
    """
    tokens = STRUCTURE_TOKENS.findall(text)
    significant = [index for index, token in enumerate(tokens) if token.strip(" \t\r\n")]
    dropped = set()
    for position, index in enumerate(significant):
        if tokens[index] != ",":
            continue
        following = tokens[significant[position + 1]] if position + 1 < len(significant) else None
        preceding = tokens[significant[position - 1]] if position else None
        if following in ("]", "}") and preceding not in (None, "[", "{", ",", ":"):
            dropped.add(index)
    return "".join(token for index, token in enumerate(tokens) if index not in dropped)


def nesting_depth(text: str) -> int:
    """The deepest bracket nesting outside strings.

    In valid JSON this is the container depth, and any text whose bracket
    depth exceeds a limit is rejected by a correct parser either way, so it is
    a sound early check that keeps Python's recursion shallow.
    """
    depth = deepest = 0
    for match in STRUCTURE_TOKENS.finditer(text):
        token = match.group()
        if token in ("[", "{"):
            depth += 1
            deepest = max(deepest, depth)
        elif token in ("]", "}"):
            depth -= 1
    return deepest


def interoperable(lexeme: str) -> bool:
    """I-JSON (RFC 7493 section 2.2) as JsonValue.Options specifies it."""
    if lexeme in NON_FINITE:
        return False
    mantissa, marker, _ = lexeme.lower().partition("e")
    if not marker and "." not in mantissa:
        return abs(Decimal(lexeme)) <= I_JSON_INTEGER
    digits = mantissa.lstrip("-").replace(".", "").strip("0")
    if len(digits) > 17:
        return False
    value = float(lexeme)
    return math.isfinite(value) and not (value == 0 and digits)


def parse_tree(text: str, options: dict) -> Any:
    """Parse with stdlib json and return the probe's tree form, or raise."""
    if text.startswith("\ufeff"):
        if not options["bom"]:
            raise Rejected("byte order mark")
        text = text[1:]
    if options["comments"]:
        text = strip_comments(text)
    if options["trailing_commas"]:
        text = strip_trailing_commas(text)
    if nesting_depth(text) > options["max_depth"]:
        raise Rejected("nesting deeper than max_depth")

    def limited(count: int) -> None:
        if options["max_items"] is not None and count > options["max_items"]:
            raise Rejected("more items than max_items")

    def string(value: str) -> str:
        if SURROGATES.search(value):
            if options["lone_surrogates"] == "Reject":
                raise Rejected("lone surrogate")
            value = SURROGATES.sub("\ufffd", value)
        if options["max_string_bytes"] is not None and len(value.encode("utf-8")) > options["max_string_bytes"]:
            raise Rejected("string longer than max_string_bytes")
        return value

    def number(lexeme: str) -> tuple:
        return ("number", lexeme)

    def constant(lexeme: str) -> tuple:
        if not options["non_finite"]:
            raise Rejected("non-finite number " + lexeme)
        return ("number", lexeme)

    def convert(raw: Any) -> Any:
        if raw is None or type(raw) is bool:
            return raw
        if type(raw) is str:
            return string(raw)
        if type(raw) is tuple:
            lexeme = raw[1]
            if options["max_number_digits"] is not None and len(lexeme) > options["max_number_digits"]:
                raise Rejected("number longer than max_number_digits")
            if options["interoperable_numbers"] and not interoperable(lexeme):
                raise Rejected("number outside I-JSON " + lexeme)
            return ["number", lexeme]
        if type(raw) is list:
            limited(len(raw))
            return ["array", [convert(item) for item in raw]]
        if type(raw) is Object:
            return raw.tree
        raise TypeError("unexpected decoded value " + repr(raw))

    def pairs_hook(pairs: list) -> Object:
        limited(len(pairs))
        members: list[list] = []
        positions: dict[str, int] = {}
        for raw_key, raw_value in pairs:
            key, value = string(raw_key), convert(raw_value)
            if key not in positions:
                positions[key] = len(members)
                members.append([key, value])
            elif options["duplicates"] == "Reject":
                raise Rejected("duplicate key " + repr(key))
            elif options["duplicates"] == "CollapseIdentical":
                if members[positions[key]][1] != value:
                    raise Rejected("duplicate key with a different value " + repr(key))
            elif options["duplicates"] == "KeepLast":
                members[positions[key]][1] = value
        return Object(["object", members])

    tree = convert(json.loads(text, parse_int=number, parse_float=number, parse_constant=constant, object_pairs_hook=pairs_hook, strict=not options["control_characters"]))
    shape = tree[0] if type(tree) is list else None
    if options["top_level"] == "Object" and shape != "object":
        raise Rejected("top-level value is not an object")
    if options["top_level"] == "ObjectOrArray" and shape not in ("object", "array"):
        raise Rejected("top-level value is not an object or array")
    return tree


def oracle(data: bytes, options: dict | None = None) -> dict:
    options = normalize_options(options)
    try:
        text = data.decode("utf-8")
    except UnicodeDecodeError as error:
        return {"status": "error", "message": f"invalid UTF-8 at byte {error.start}"}
    try:
        tree = parse_tree(text, options)
    except RecursionError:
        return {"status": "error", "message": "RecursionError (oracle nesting limit)"}
    except ValueError as error:  # json.JSONDecodeError and Rejected are ValueErrors
        return {"status": "error", "message": str(error)}
    return {
        "status": "ok",
        "value": tree,
        "compact": write_expectation(tree, False, 0),
        "pretty": write_expectation(tree, options["sort_keys"], options["indent"]),
        "sorted": write_expectation(tree, True, 0),
    }


# Writer oracle --------------------------------------------------------------


def write_errors(tree: Any) -> list[dict]:
    """Every error JsonValue.to_str may report for a tree; it may report any one."""
    errors: list[dict] = []
    stack = [(tree, 0)]
    while stack:
        value, depth = stack.pop()
        if type(value) is not list:
            continue
        kind, body = value
        if kind == "number":
            if not NUMBER.match(body):
                errors.append({"error": "InvalidNumber", "value": body})
            continue
        if depth + 1 > WRITE_DEPTH and {"error": "TooDeep", "value": None} not in errors:
            errors.append({"error": "TooDeep", "value": None})
        if kind == "array":
            stack.extend((item, depth + 1) for item in body)
        else:
            seen = set()
            for key, member in body:
                if key in seen:
                    errors.append({"error": "DuplicateKey", "value": key})
                seen.add(key)
                stack.append((member, depth + 1))
    return errors


def render(tree: Any, sort_keys: bool, indent: int, level: int = 0) -> str:
    """Lay out a valid tree exactly as json.dumps does."""
    if tree is None:
        return "null"
    if tree is True:
        return "true"
    if tree is False:
        return "false"
    if type(tree) is str:
        return json.dumps(tree, ensure_ascii=False)
    kind, body = tree
    if kind == "number":
        return body
    if not body:
        return "[]" if kind == "array" else "{}"
    if kind == "array":
        opening, closing = "[", "]"
        items = [render(item, sort_keys, indent, level + 1) for item in body]
    else:
        opening, closing = "{", "}"
        members = sorted(body, key=lambda member: member[0]) if sort_keys else body
        separator = ": " if indent else ":"
        items = [json.dumps(key, ensure_ascii=False) + separator + render(value, sort_keys, indent, level + 1) for key, value in members]
    if not indent:
        return opening + ",".join(items) + closing
    inner = "\n" + " " * (indent * (level + 1))
    return opening + inner + ("," + inner).join(items) + "\n" + " " * (indent * level) + closing


def write_expectation(tree: Any, sort_keys: bool, indent: int) -> Any:
    errors = write_errors(tree)
    return {"error_any_of": errors} if errors else render(tree, sort_keys, indent)


def writer_matches(actual: Any, expected: Any) -> bool:
    if isinstance(expected, str):
        return actual == expected
    return isinstance(actual, dict) and {"error": actual.get("error"), "value": actual.get("value")} in expected["error_any_of"]


# Probe ----------------------------------------------------------------------


def validate_tree(tree: Any) -> None:
    stack = [tree]
    while stack:
        value = stack.pop()
        if value is None or type(value) in (bool, str):
            continue
        if type(value) is not list or len(value) != 2:
            raise ValueError("malformed tree node " + repr(value)[:200])
        kind, body = value
        if kind == "number" and type(body) is str:
            continue
        if kind == "array" and type(body) is list:
            stack.extend(body)
        elif kind == "object" and type(body) is list:
            for member in body:
                if type(member) is not list or len(member) != 2 or type(member[0]) is not str:
                    raise ValueError("malformed object member " + repr(member)[:200])
                stack.append(member[1])
        else:
            raise ValueError("malformed tree node " + repr(value)[:200])


def validate_written(value: Any) -> None:
    if type(value) is str:
        return
    if type(value) is not dict or value.get("error") not in ("InvalidNumber", "DuplicateKey", "TooDeep"):
        raise ValueError("malformed writer result " + repr(value)[:200])
    if (value["error"] == "TooDeep") != (value.get("value") is None) or (value.get("value") is not None and type(value["value"]) is not str):
        raise ValueError("malformed writer error value " + repr(value)[:200])


def location_inside(data: bytes, line: Any, column: Any) -> bool:
    if type(line) is not int or type(column) is not int:
        return False
    lines = re.split(rb"\r\n|\r|\n", data)
    return 1 <= line <= len(lines) and 1 <= column <= len(lines[line - 1]) + 1


def interpret(data: bytes, stdout: bytes) -> dict:
    """Check one probe output line against the protocol in probe.roc."""
    try:
        result = json.loads(stdout)
        status = result["status"]
        if status == "ok":
            validate_tree(result["value"])
            for name in WRITERS:
                validate_written(result[name])
            if type(result["parser_agrees"]) is not bool:
                raise ValueError("parser_agrees must be a boolean")
        elif status == "error":
            if type(result["message"]) is not str or type(result["parser_agrees"]) is not bool:
                raise ValueError("malformed error")
            offset = result.get("offset")
            if not result["message"] or not location_inside(data, result["line"], result["column"]) or (offset is not None and (type(offset) is not int or not 0 <= offset <= len(data))):
                return {"status": "invalid_diagnostic", "result": result}
        else:
            return {"status": "protocol_error", "result": result}
        return result
    except (ValueError, KeyError, TypeError, IndexError, RecursionError) as error:
        return {"status": "protocol_error", "message": str(error)[:500], "stdout": stdout.decode(errors="replace")[:4000]}


def probe(executable: Path, data: bytes, flags: list[str], timeout: float = PROBE_TIMEOUT) -> dict:
    try:
        completed = subprocess.run([str(executable), *flags], input=data, capture_output=True, timeout=timeout, check=False)
    except subprocess.TimeoutExpired:
        return {"status": "timeout", "seconds": timeout}
    if completed.returncode:
        return {"status": "crash", "return_code": completed.returncode, "stderr": completed.stderr.decode(errors="replace")[:4000]}
    return interpret(data, completed.stdout)


def build(roc: str, work: Path) -> Path:
    executable = work / "bin" / "probe"
    executable.parent.mkdir(parents=True, exist_ok=True)
    command = [roc, "build", str(DATA / "probe.roc"), "--output=" + str(executable)]
    completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    log = work / "probe-build.log"
    log.write_text(completed.stdout + completed.stderr)
    if completed.returncode:
        raise RuntimeError("probe build failed; see " + str(log))
    return executable


# Comparison -----------------------------------------------------------------


def compare(case: dict, actual: dict, expected: dict) -> dict:
    """Classify one probe result against the oracle (and a suite verdict, if any)."""
    expect = case.get("expect")
    verdict = {"y": True, "n": False}.get(expect)
    disagreement = None
    if verdict is not None and (expected["status"] == "ok") != verdict:
        disagreement = "suite verdict disagrees with the oracle"
    if verdict is None:
        verdict = expected["status"] == "ok"
    if actual["status"] in FATAL:
        kind = actual["status"]
    elif not verdict:
        kind = "pass" if actual["status"] == "error" else "invalid_acceptance"
    elif actual["status"] == "error":
        kind = "valid_rejection"
    elif expected["status"] != "ok":
        kind = "pass"  # the suite says valid and the oracle disagrees; there is no tree to compare
    elif actual["value"] != expected["value"]:
        kind = "wrong_value"
    elif not all(writer_matches(actual[name], expected[name]) for name in WRITERS):
        kind = "writer_mismatch"
    else:
        kind = "pass"
    if kind == "pass" and actual.get("parser_agrees") is False:
        kind = "parser_disagreement"
    data = case["data"]
    row = {
        "id": case["id"],
        "group": case.get("group", "case"),
        "options": normalize_options(case.get("options")),
        "input_sha256": hashlib.sha256(data).hexdigest(),
        "kind": kind,
        "expected": expected,
        "actual": actual,
        "oracle_disagreement": disagreement,
    }
    if len(data) <= MAX_GENERATED_BYTES:
        row["input_hex"] = data.hex()
    if case.get("note"):
        row["note"] = case["note"]
    if expect == "i" and kind not in FATAL:
        # Implementation-defined suite cases may go either way; record, never fail.
        row["implementation_defined"] = {"probe": actual["status"], "oracle": expected["status"], "comparison": kind}
        row["kind"] = "pass"
    return row


def signature(row: dict) -> str:
    detail = None
    if row["kind"] == "wrong_value":
        detail = digest(row["actual"]["value"])
    elif row["kind"] == "writer_mismatch":
        detail = digest({name: row["actual"][name] for name in WRITERS})
    stable = {
        "input_sha256": row["input_sha256"],
        "options": row["options"],
        "kind": row["kind"],
        "expected_status": row["expected"]["status"],
        "actual_status": row["actual"]["status"],
        "detail": detail,
    }
    return digest(stable)


def failure_reason(row: dict) -> str:
    message = row["actual"].get("message", "")
    reasons = {
        "valid_rejection": "Rejects input the oracle accepts" + (": " + message if message else "."),
        "invalid_acceptance": "Accepts input the oracle rejects (" + row["expected"].get("message", "") + ").",
        "wrong_value": "Accepted value differs from the oracle's.",
        "writer_mismatch": "Writer output differs from json.dumps.",
        "parser_disagreement": "JsonValue.parser_with disagrees with JsonValue.parse_with.",
    }
    return reasons.get(row["kind"], "Known divergence from the oracle.")


def slim(row: dict) -> dict:
    """Drop the trees of passing rows so reports stay readable."""
    if row["kind"] != "pass":
        return row
    result = dict(row)
    result["expected"] = {"status": row["expected"]["status"]}
    result["actual"] = {"status": row["actual"]["status"]}
    return result


def review(executable: Path, case: dict) -> dict:
    flags = option_flags(case.get("options"))
    actual = probe(executable, case["data"], flags, case.get("timeout", PROBE_TIMEOUT))
    return compare(case, actual, oracle(case["data"], case.get("options")))


# Inputs ---------------------------------------------------------------------


def hand_cases() -> list[dict]:
    cases = []
    for entry in json.loads((DATA / "cases.json").read_text(encoding="utf-8")):
        if ("input" in entry) == ("input_hex" in entry):
            raise ValueError("case " + entry.get("name", "?") + " needs exactly one of input and input_hex")
        data = entry["input"].encode("utf-8") if "input" in entry else bytes.fromhex(entry["input_hex"])
        cases.append({"id": "case/" + entry["name"], "group": "case", "data": data, "options": normalize_options(entry.get("options"))})
    ids = [case["id"] for case in cases]
    if len(ids) != len(set(ids)):
        raise ValueError("duplicate case names: " + ", ".join(sorted({i for i in ids if ids.count(i) > 1})))
    return cases


def suite_source() -> dict:
    return json.loads((DATA / "suite.json").read_text(encoding="utf-8"))["source"]


def suite_cases() -> list[dict]:
    cases = []
    for entry in json.loads((DATA / "suite.json").read_text(encoding="utf-8"))["cases"]:
        case = {"id": entry["id"], "group": "suite", "data": bytes.fromhex(entry["input_hex"]), "options": {}, "expect": entry["expect"]}
        if POLICY_DUPLICATES.match(entry["id"]):
            case["options"] = {"duplicates": "KeepLast"}
            case["note"] = POLICY_NOTE
        cases.append(case)
    return cases


def fetch_document(document: dict, directory: Path) -> bytes:
    path = directory / (document["name"] + ".json")
    if not path.exists():
        directory.mkdir(parents=True, exist_ok=True)
        request = urllib.request.Request(document["url"], headers={"User-Agent": "roc-parser-review-json"})
        with urllib.request.urlopen(request, timeout=60) as response:
            data = response.read()
        actual = hashlib.sha256(data).hexdigest()
        if actual != document["sha256"]:
            raise ValueError(f"{document['url']} has sha256 {actual}, not the pinned {document['sha256']}")
        path.write_bytes(data)
    data = path.read_bytes()
    actual = hashlib.sha256(data).hexdigest()
    if actual != document["sha256"]:
        raise ValueError(f"{path} has sha256 {actual}, not the pinned {document['sha256']}; delete it to fetch again")
    return data


def document_cases(work: Path) -> list[dict]:
    directory = work / "documents"
    return [
        {"id": "document/" + document["name"], "group": "document", "data": fetch_document(document, directory), "options": normalize_options(document["options"]), "expect": "y", "timeout": DOCUMENT_TIMEOUT}
        for document in DOCUMENTS
    ]


def load_cases(args) -> list[dict]:
    cases = hand_cases()
    if not args.no_suite:
        cases += suite_cases()
    if not args.no_documents:
        cases += document_cases(args.work_dir)
    if args.case:
        selected = [case for case in cases if case["id"] in args.case]
        missing = set(args.case) - {case["id"] for case in selected}
        if missing:
            raise ValueError("unknown case(s): " + ", ".join(sorted(missing)))
        return selected
    return cases


# Reports --------------------------------------------------------------------


def command_output(command: list[str]) -> str:
    try:
        completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    except OSError as error:
        return "unavailable: " + str(error)
    return (completed.stdout or completed.stderr).strip()


def metadata(roc: str, executable: Path | None = None) -> dict:
    result = {
        "roc": command_output([roc, "version"]),
        "python": sys.version,
        "oracle": "json " + json.__version__ + " (Python standard library)",
        "json_value_sha256": hashlib.sha256((ROOT / "package" / "JsonValue.roc").read_bytes()).hexdigest(),
        "source_revision": command_output(["git", "rev-parse", "HEAD"]),
        "suite": suite_source(),
        "documents": [{key: document[key] for key in ("name", "url", "sha256")} for document in DOCUMENTS],
    }
    if executable is not None and executable.exists():
        result["probe_sha256"] = hashlib.sha256(executable.read_bytes()).hexdigest()
    return result


def summarize(rows: list[dict]) -> dict:
    groups: dict[str, Counter] = {}
    for row in rows:
        groups.setdefault(row["group"], Counter())[row["kind"]] += 1
    implementation_defined = Counter(row["implementation_defined"]["probe"] for row in rows if "implementation_defined" in row)
    return {
        "total": len(rows),
        "counts": dict(Counter(row["kind"] for row in rows)),
        "by_group": {group: dict(counts) for group, counts in groups.items()},
        "implementation_defined": dict(implementation_defined),
        "policy_deviations": [row["id"] for row in rows if "note" in row],
        "oracle_disagreements": [row["id"] for row in rows if row["oracle_disagreement"]],
    }


def check(args) -> int:
    executable = build(args.roc, args.work_dir)
    cases = load_cases(args)
    rows = [review(executable, case) for case in cases]
    failures = [row for row in rows if row["kind"] != "pass"]
    summary = summarize(rows)
    report = {"metadata": metadata(args.roc, executable), **summary, "cases": [slim(row) for row in rows]}
    write_json(args.work_dir / "check.json", report)
    print(json.dumps({**summary, "report": str(args.work_dir / "check.json")}, ensure_ascii=False, indent=2))
    if args.record_known_failures:
        fatal = sorted(row["id"] for row in rows if row["kind"] in FATAL)
        if fatal:
            raise ValueError("crashes, timeouts, protocol errors and invalid diagnostics cannot be baselined: " + ", ".join(fatal[:20]))
        known = {row["id"]: {"signature": signature(row), "kind": row["kind"], "reason": failure_reason(row)} for row in failures}
        write_json(args.record_known_failures, {"metadata": report["metadata"], "failures": known})
        return 0
    if args.baseline:
        known = json.loads(args.baseline.read_text(encoding="utf-8"))["failures"]
        new = [row["id"] for row in failures if row["kind"] in FATAL or known.get(row["id"], {}).get("signature") != signature(row)]
        selected = {case["id"] for case in cases}
        stale = sorted((set(known) & selected) - {row["id"] for row in failures})
        if not args.case:
            stale += sorted(set(known) - selected)
        if new or stale:
            print(json.dumps({"new_or_changed_failures": new, "stale_known_failures": stale}, indent=2))
            return 1
        return 0
    return int(bool(failures))


# Fuzz corpus ----------------------------------------------------------------


def parse_show(text: str) -> dict | None:
    """Find the `JSON-HEX:` document and optional `JSON-OPTIONS:` flags in `show` output."""
    document = re.search(r"JSON-HEX:[ \t]*([0-9a-fA-F]*)", text)
    if not document or len(document[1]) % 2:
        return None
    flags = re.search(r"JSON-OPTIONS:[ \t]*([^\r\n]*)", text)
    return {"data": bytes.fromhex(document[1]), "options": options_from_flags(flags[1].strip() if flags else [])}


def load_run_fuzz():
    """scripts/run_fuzz.py, whose TARGETS and build conventions the fuzz runs share."""
    if "run_fuzz" not in sys.modules:
        spec = importlib.util.spec_from_file_location("run_fuzz", ROOT / "scripts" / "run_fuzz.py")
        module = importlib.util.module_from_spec(spec)
        sys.modules["run_fuzz"] = module  # dataclasses look their module up while it loads
        spec.loader.exec_module(module)
    return sys.modules["run_fuzz"]


def fuzz_binary(roc: str, target: str, no_build: bool = False) -> Path:
    """Build a fuzz target the way scripts/run_fuzz.py does, into .roc-parser-tmp/fuzz/bin."""
    run_fuzz = load_run_fuzz()
    if target not in run_fuzz.TARGETS:
        raise RuntimeError(f"fuzz target {target} is not registered in scripts/run_fuzz.py")
    status, binary = run_fuzz.build_target(target, roc, no_build)
    if status:
        raise RuntimeError(f"building fuzz target {target} failed with status {status}")
    return binary


def corpus(args) -> int:
    """Render each corpus input with the fuzz target's `show`, then compare probe and oracle."""
    executable = build(args.roc, args.work_dir)
    target = fuzz_binary(args.roc, args.target, args.no_build)
    counts: Counter = Counter()
    mismatches = []
    for path in sorted(item for item in args.corpus.iterdir() if item.is_file()):
        shown = subprocess.run([str(target), "show", str(path)], capture_output=True, text=True, errors="replace", check=False)
        try:
            document = parse_show(shown.stdout + "\n" + shown.stderr)
        except ValueError as error:
            counts["bad_options"] += 1
            mismatches.append({"id": path.name, "kind": "bad_options", "message": str(error)})
            continue
        if document is None:
            counts["no_document"] += 1
            continue
        row = review(executable, {"id": path.name, "group": "corpus", **document})
        counts[row["kind"]] += 1
        if row["kind"] != "pass":
            mismatches.append(row)
    output = args.work_dir / "corpus" / (args.target + ".json")
    write_json(output, {"target": args.target, "corpus": str(args.corpus), "counts": dict(counts), "mismatches": mismatches})
    print(json.dumps({"counts": dict(counts), "report": str(output)}, indent=2))
    return int(bool(mismatches))


# Campaign -------------------------------------------------------------------

NUMBER_LEXEMES = [
    "0", "-0", "1", "-1", "7", "10", "123", "-42", "1.5", "-0.0", "0.1", "1.0", "1.10", "1e3", "1E+3", "1e-3", "2.5E-10",
    "9007199254740991", "-9007199254740991", "9007199254740992", "12345678901234567", "123456789012345678",
    "0.12345678901234567", "0.123456789012345678", "1e308", "1e309", "-1e309", "1e-400", "5e-324", "2e-324",
    "1.7976931348623157e308", "1.7976931348623159e308", "123456789012345678901234567890", "0e-999",
]
ODD_NUMBERS = ["01", "-", "+1", ".5", "1.", "1e", "1e+", "0x10", "1_0", "--1", "NaN", "Infinity", "-Infinity", "-NaN", "nan", "inf"]
STRING_PIECES = [
    "", "a", "b", "key", "é", "€", "😀", "\u2028", "\x7f", "\\n", "\\t", "\\\"", "\\\\", "\\/", "\\b", "\\f", "\\r", "\\u0041", "\\u00e9",
    "\\u00E9", "\\ud83d\\ude00", "\\uD834\\uDD1E", "\\u0000", "\\u001f", " ", "//", "/*", "*/", ",", "]", "}", "[", "{", ":",
]
ODD_STRING_PIECES = ["\\ud800", "\\udc00", "\\ud800\\ud800\\udc00", "\x00", "\x01", "\x1f", "\t", "\n", "\\x", "\\u12", "\\U0041"]
KEYS = ['"a"', '"b"', '"\\u0061"', '"A"', '""', '"é"', '"\\u00e9"', '"key"', '"k\\u0000"']
ODD_KEYS = ['"\\ud800"', '"\\udc00"', '"\ufffd"', "a", "'a'", '"a\x01"']
WHITESPACE = ["", "", "", " ", "\n", "\t", "\r\n", "\r", "  "]
ODD_WHITESPACE = ["/* c */", "// c\n", "/**/", "/* [ */", "\f", "\u00a0", "\ufeff"]
MUTATIONS = [
    b"", b",", b"[", b"]", b"{", b"}", b":", b'"', b"\\", b"/", b"*", b"\n", b"\r", b" ", b"0", b"1", b"-", b".", b"e", b"E", b"+",
    b"N", b"I", b"n", b"t", b"\x00", b"\x1f", b"\x7f", b"\xef\xbb\xbf", b"\xff", b"\xc3", b"\xed\xa0\x80", b"\\u", b"\\ud800",
    b"//", b"/*", b"*/", b"NaN", b"Infinity", b",]", b",}",
]


def random_options(rng: random.Random) -> dict:
    options: dict[str, Any] = {}
    for key in ("comments", "trailing_commas", "bom", "non_finite", "control_characters", "interoperable_numbers", "sort_keys"):
        if rng.random() < 0.3:
            options[key] = True
    choices = {"max_depth": [0, 1, 2, 3, 5, 127, 128, 129, 200], "max_items": [0, 1, 2, 3, 10], "max_string_bytes": [0, 1, 2, 3, 4, 8], "max_number_digits": [1, 2, 3, 5, 9, 17, 20], "indent": [0, 1, 2, 4]}
    for key, values in choices.items():
        if rng.random() < 0.12:
            options[key] = rng.choice(values)
    for key, names in ENUMS.items():
        if rng.random() < 0.3:
            options[key] = rng.choice(list(names))
    return normalize_options(options)


def random_value(rng: random.Random, depth: int = 0) -> str:
    """A document that is mostly valid, with occasional relaxed or broken syntax."""

    def space() -> str:
        return rng.choice(ODD_WHITESPACE if rng.random() < 0.01 else WHITESPACE)

    def piece() -> str:
        return rng.choice(ODD_STRING_PIECES if rng.random() < 0.02 else STRING_PIECES)

    roll = rng.random()
    if depth > 6 or roll < 0.45:
        scalar = rng.random()
        if scalar < 0.15:
            return rng.choice(["null", "true", "false"])
        if scalar < 0.45:
            return rng.choice(ODD_NUMBERS if rng.random() < 0.05 else NUMBER_LEXEMES)
        return '"' + "".join(piece() for _ in range(rng.randrange(4))) + '"'
    if roll < 0.5:
        # Nesting near the default limit only at the top, so it stays meaningful.
        size = rng.choice([127, 128, 129, 130] if depth == 0 else [1, 2, 3])
        opening = rng.choice(["[", '{"a":'])
        closing = "]" if opening == "[" else "}"
        return opening * size + random_value(rng, 7) + closing * size
    items = rng.randrange(5)
    if roll < 0.75:
        values = [space() + random_value(rng, depth + 1) + space() for _ in range(items)]
        trailing = "," if values and rng.random() < 0.05 else ""
        return "[" + ",".join(values) + trailing + space() + "]"
    members = [space() + rng.choice(ODD_KEYS if rng.random() < 0.05 else KEYS) + space() + ":" + space() + random_value(rng, depth + 1) + space() for _ in range(items)]
    trailing = "," if members and rng.random() < 0.05 else ""
    return "{" + ",".join(members) + trailing + space() + "}"


def mutate(data: bytes, rng: random.Random) -> bytes:
    for _ in range(rng.randint(1, 3)):
        position = rng.randrange(len(data) + 1)
        operation = rng.random()
        if operation < 0.4:
            data = data[:position] + rng.choice(MUTATIONS) + data[position:]
        elif operation < 0.7:
            data = data[:position] + data[position + rng.randint(1, 4):]
        elif operation < 0.9:
            data = data[:position] + rng.choice(MUTATIONS) + data[position + 1:]
        else:
            end = min(len(data), position + rng.randint(1, 16))
            data = data[:end] + data[position:end] + data[end:]
    return data


def generated_inputs(seed: int) -> Iterator[tuple[bytes, dict]]:
    """An endless, reproducible stream of (document, options) pairs."""
    rng = random.Random(seed)
    pool = [case["data"] for case in hand_cases() + suite_cases() if len(case["data"]) <= MAX_GENERATED_BYTES]
    while True:
        roll = rng.random()
        if roll < 0.45:
            data = random_value(rng).encode("utf-8")
        elif roll < 0.65:
            data = rng.choice(pool)  # a reviewed input under a random options set
        elif roll < 0.9:
            data = mutate(rng.choice(pool), rng)
        else:
            data = mutate(random_value(rng).encode("utf-8"), rng)
        if rng.random() < 0.05:
            data = b"\xef\xbb\xbf" + data
        if len(data) <= MAX_GENERATED_BYTES:
            yield data, random_options(rng)


def differential(executable: Path, seconds: float, seed: int, output: Path, limit: int | None = None) -> dict:
    start = time.monotonic()
    counts: Counter = Counter()
    examples: dict[str, dict] = {}
    tested = 0
    for data, options in generated_inputs(seed):
        if time.monotonic() - start >= seconds or (limit is not None and tested >= limit):
            break
        row = review(executable, {"id": f"generated/{seed}/{tested}", "group": "generated", "data": data, "options": options})
        counts[row["kind"]] += 1
        tested += 1
        if row["kind"] != "pass":
            key = row["kind"] + ":" + row["expected"]["status"] + ":" + row["actual"].get("message", row["actual"]["status"])
            if (key not in examples and len(examples) < 64) or (key in examples and len(data) < len(bytes.fromhex(examples[key]["input_hex"]))):
                examples[key] = row
        if row["kind"] in FATAL:
            break
    reproducers = output / "reproducers"
    paths = []
    for row in examples.values():
        path = reproducers / f"{row['kind']}-{row['input_sha256'][:16]}.json"
        write_json(path, {"id": row["id"], "kind": row["kind"], "input_hex": row["input_hex"], "options": row["options"], "flags": option_flags(row["options"]), "expected": row["expected"], "actual": row["actual"]})
        paths.append(str(path))
    result = {"seed": seed, "seconds": time.monotonic() - start, "executions": tested, "counts": dict(counts), "reproducers": paths, "examples": list(examples.values())}
    write_json(output / "differential.json", result)
    return result


def seed_corpus(run_fuzz, target: str, corpus_dir: Path) -> None:
    """The reviewed run_fuzz.py seeds plus every short case and suite input."""
    run_fuzz.prepare_corpus(target, corpus_dir)
    inputs = [case["data"] for case in hand_cases() + suite_cases() if len(case["data"]) < MAX_GENERATED_BYTES]
    if target == "json-raw":
        seeds = inputs
    elif target == "json-options":
        # The first byte selects the options set.
        seeds = [bytes([index % 256]) + data for index, data in enumerate(inputs)]
    else:
        seeds = [bytes([value, 0, 1, 2, 3]) for value in range(16)]
    for raw in seeds:
        (corpus_dir / ("review-" + hashlib.sha256(raw).hexdigest())).write_bytes(raw)


def fuzz_run(roc: str, target: str, seconds: float, seed: int, output: Path, smoke: bool) -> dict:
    """One libFuzzer run of a registered target, with run_fuzz.py's limits and dictionary."""
    work = output / target
    try:
        binary = fuzz_binary(roc, target)
    except RuntimeError as error:
        return {"target": target, "return_code": 2, "error": str(error)}
    run_fuzz = load_run_fuzz()
    config = run_fuzz.TARGETS[target]
    corpus_dir = work / "corpus"
    corpus_dir.mkdir(parents=True, exist_ok=True)
    seed_corpus(run_fuzz, target, corpus_dir)
    limit = "--runs=2000" if smoke else "--time=" + str(max(1, int(seconds)))
    command = [str(binary), "ci", str(work / "evidence"), str(corpus_dir), limit, "--seed=" + str(seed), f"--max-input-size={config.max_input_size}", f"--timeout={config.timeout}", "--memory-limit=2048", "--print-final-stats"]
    if config.dictionary is not None:
        command.append("--dictionary=" + str(config.dictionary))
    start = time.monotonic()
    try:
        with (work / "runner.log").open("w") as log:
            completed = subprocess.run(command, cwd=work, stdout=log, stderr=subprocess.STDOUT, timeout=90 if smoke else seconds + 30, check=False)
        code = completed.returncode
    except subprocess.TimeoutExpired:
        code = 124
    return {"target": target, "command": command, "return_code": code, "seconds": time.monotonic() - start, "artifacts": str(work)}


def campaign(args) -> int:
    output = args.work_dir / "campaign"
    executable = build(args.roc, args.work_dir)
    if args.smoke:
        fuzz = [fuzz_run(args.roc, target, 0, args.seed, output, True) for target in FUZZ_TARGETS]
        diff = differential(executable, 30, args.seed, output, limit=1500)
    else:
        share = args.seconds * 0.6 / len(FUZZ_TARGETS)
        fuzz = [fuzz_run(args.roc, target, share, args.seed, output, False) for target in FUZZ_TARGETS]
        diff = differential(executable, args.seconds * 0.4, args.seed, output)
    report = {"metadata": metadata(args.roc, executable), "budget_seconds": None if args.smoke else args.seconds, "smoke": args.smoke, "fuzz": fuzz, "differential": diff}
    write_json(output / "campaign.json", report)
    print(json.dumps({"fuzz": fuzz, "differential": {key: value for key, value in diff.items() if key != "examples"}, "report": str(output / "campaign.json")}, indent=2))
    return int(any(run["return_code"] for run in fuzz) or any(kind != "pass" for kind in diff["counts"]))


def replay(args) -> int:
    reproducer = json.loads(args.reproducer.read_text(encoding="utf-8"))
    executable = build(args.roc, args.work_dir)
    row = review(executable, {"id": reproducer.get("id", args.reproducer.name), "group": "replay", "data": bytes.fromhex(reproducer["input_hex"]), "options": reproducer.get("options", {})})
    print(json.dumps(row, ensure_ascii=False, indent=2))
    return int(row["kind"] != "pass")


def vendor_suite(args) -> int:
    """Regenerate scripts/json/suite.json from a JSONTestSuite checkout."""
    revision = subprocess.run(["git", "-C", str(args.clone), "rev-parse", "HEAD"], capture_output=True, text=True, check=True).stdout.strip()
    cases = []
    for path in sorted((args.clone / "test_parsing").glob("*.json")):
        expect = path.name[0]
        if expect not in "yni" or path.name[1] != "_":
            raise ValueError("unexpected suite file " + path.name)
        cases.append({"id": "suite/" + path.stem, "input_hex": path.read_bytes().hex(), "expect": expect})
    source = {"repository": "https://github.com/nst/JSONTestSuite", "revision": revision, "branch": "master", "path": "test_parsing", "license": "MIT"}
    write_json(DATA / "suite.json", {"source": source, "cases": cases})
    print(json.dumps({"revision": revision, "counts": dict(Counter(case["expect"] for case in cases))}))
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    common.add_argument("--work-dir", type=Path, default=DEFAULT_WORK)
    commands = parser.add_subparsers(dest="command", required=True)
    check_parser = commands.add_parser("check", parents=[common])
    check_parser.add_argument("--case", action="append", help="review only this case id (repeatable)")
    check_parser.add_argument("--no-suite", action="store_true")
    check_parser.add_argument("--no-documents", action="store_true", help="skip the fetched real-world documents")
    check_parser.add_argument("--baseline", type=Path, help="fail on failures missing from, or changed since, this baseline")
    check_parser.add_argument("--record-known-failures", type=Path, nargs="?", const=True, metavar="FILE",
                              help="write the current non-fatal failures as the baseline (to FILE, or to --baseline)")
    corpus_parser = commands.add_parser("corpus", parents=[common])
    corpus_parser.add_argument("target", choices=FUZZ_TARGETS)
    corpus_parser.add_argument("corpus", type=Path)
    corpus_parser.add_argument("--no-build", action="store_true", help="reuse the fuzz target binary")
    campaign_parser = commands.add_parser("campaign", parents=[common])
    campaign_parser.add_argument("--seconds", type=int, default=300)
    campaign_parser.add_argument("--seed", type=int, default=94)
    campaign_parser.add_argument("--smoke", action="store_true")
    replay_parser = commands.add_parser("replay", parents=[common])
    replay_parser.add_argument("reproducer", type=Path)
    vendor_parser = commands.add_parser("vendor-suite")
    vendor_parser.add_argument("clone", type=Path)
    args = parser.parse_args(argv)
    # Accepted documents nest up to the limit under test, and the tree
    # conversion and comparison recurse once per level.
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 20000))
    if hasattr(args, "roc"):
        args.roc = str(Path(args.roc).expanduser().resolve()) if "/" in args.roc else args.roc
        args.work_dir = args.work_dir.expanduser().resolve()
    try:
        if args.command == "check" and args.record_known_failures is True:
            if args.baseline is None:
                raise ValueError("--record-known-failures needs a FILE or --baseline")
            args.record_known_failures = args.baseline
        if args.command == "campaign" and args.seconds < 1:
            raise ValueError("--seconds must be positive")
        handlers = {"check": check, "corpus": corpus, "campaign": campaign, "replay": replay, "vendor-suite": vendor_suite}
        return handlers[args.command](args)
    except (ValueError, RuntimeError, OSError, subprocess.CalledProcessError) as error:
        parser.exit(2, "error: " + str(error) + "\n")


if __name__ == "__main__":
    raise SystemExit(main())
