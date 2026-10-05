#!/usr/bin/env python3
"""Reproducible YAML 1.2 Core conformance checks and bounded review campaigns.

Install scripts/yaml/requirements.txt in an external virtual environment first.
The Python oracle parses events/nodes without constructing application objects.
Fixtures and known failures are explicit; discovery never updates the baseline.
"""

from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import importlib.metadata
import json
import math
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import time
import warnings
from typing import Any, Callable

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "yaml"
DEFAULT_WORK = ROOT / ".roc-parser-tmp" / "yaml-review"
FUZZ_URL = "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst"
FUZZ_SHA256 = "d9bae1e93e6e94f3fafb4040861402dbab90af247023461d8e5451db4a3ae0f2"
FATAL = {"crash", "timeout", "protocol_error", "invalid_diagnostic"}
INT_RE = re.compile(r"[-+]?[0-9]+\Z")
OCT_RE = re.compile(r"0o[0-7]+\Z")
HEX_RE = re.compile(r"0x[0-9a-fA-F]+\Z")
FLOAT_RE = re.compile(r"[-+]?(?:\.[0-9]+|[0-9]+(?:\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?\Z")
TAG_PREFIX = "tag:yaml.org,2002:"


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def digest(value: Any) -> str:
    return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True).encode()).hexdigest()


def core_scalar(text: str, tag: str | None = None, style: str | None = None) -> list:
    """Apply exactly the 1.2.2 Core regexes, without timestamp/merge extensions."""
    if tag in ("!", TAG_PREFIX + "str") or (tag is None and style is not None):
        return ["string", text]
    if tag is None:
        if text in ("", "~", "null", "Null", "NULL"):
            return ["null"]
        if text in ("true", "True", "TRUE", "false", "False", "FALSE"):
            return ["bool", text.lower() == "true"]
        if INT_RE.fullmatch(text):
            return ["int", str(int(text, 10))]
        if OCT_RE.fullmatch(text):
            return ["int", str(int(text[2:], 8))]
        if HEX_RE.fullmatch(text):
            return ["int", str(int(text[2:], 16))]
        if FLOAT_RE.fullmatch(text):
            return ["float", float_text(float(text))]
        if re.fullmatch(r"[-+]?\.(?:inf|Inf|INF)", text):
            return ["float", "-inf" if text.startswith("-") else "inf"]
        if text in (".nan", ".NaN", ".NAN"):
            return ["float", "nan"]
        return ["string", text]
    kind = tag.removeprefix(TAG_PREFIX)
    if kind == "int":
        resolved = core_scalar(text)
        if resolved[0] != "int":
            raise ValueError("invalid explicitly tagged integer")
        return resolved
    if kind == "float":
        resolved = core_scalar(text)
        if resolved[0] == "int":
            return ["float", float_text(float(resolved[1]))]
        if resolved[0] != "float":
            raise ValueError("invalid explicitly tagged float")
        return resolved
    if kind in ("null", "bool"):
        resolved = core_scalar(text)
        if resolved[0] != kind:
            raise ValueError("invalid explicitly tagged " + kind)
        return resolved
    return ["tagged", tag, ["string", text]]


def float_text(value: float) -> str:
    if math.isnan(value):
        return "nan"
    if math.isinf(value):
        return "-inf" if value < 0 else "inf"
    return value.hex()


def canonical(value: list) -> list:
    if not isinstance(value, list) or not value:
        raise ValueError("invalid tagged value")
    kind = value[0]
    lengths = {"null": 1, "bool": 2, "int": 2, "float": 2, "string": 2, "sequence": 2, "stream": 2, "mapping": 2, "tagged": 3, "alias": 2}
    if kind not in lengths or len(value) != lengths[kind]:
        raise ValueError("invalid tagged value shape")
    if kind == "bool" and type(value[1]) is not bool:
        raise ValueError("invalid boolean value")
    if kind in ("int", "float", "string", "alias") and not isinstance(value[1], str):
        raise ValueError("scalar value must be text")
    if kind in ("sequence", "stream", "mapping") and not isinstance(value[1], list):
        raise ValueError("collection value must be a list")
    if kind == "float":
        text = value[1].lower().replace(".inf", "inf").replace(".nan", "nan")
        number = float.fromhex(text) if "0x" in text else float(text)
        return [kind, float_text(number)]
    if kind == "int":
        return [kind, str(int(value[1]))]
    if kind in ("sequence", "stream"):
        return [kind, [canonical(item) for item in value[1]]]
    if kind == "mapping":
        entries = [[canonical(k), canonical(v)] for k, v in value[1]]
        return [kind, sorted(entries, key=lambda pair: json.dumps(pair, ensure_ascii=False))]
    if kind == "tagged":
        return [kind, value[1], canonical(value[2])]
    return value


def unescape_event(text: str) -> str:
    # YAML test suite event values escape these characters, not JSON strings.
    replacements = {"\\": "\\", "n": "\n", "r": "\r", "t": "\t", "0": "\0", "b": "\b"}
    return re.sub(r"\\([\\nrt0b])", lambda match: replacements[match[1]], text)


def suite_scalars(events: str) -> list[str]:
    values = []
    for line in events.splitlines():
        match = re.fullmatch(r"=VAL(?: (?:&\S+|<[^>]*>))* ([:|'\">])(.*)", line)
        if match:
            values.append(unescape_event(match[2]))
        elif line.startswith("=VAL"):
            raise ValueError("unrecognized suite scalar event: " + line)
    return values


def suite_expectation(case: dict) -> dict:
    """Read the upstream event DSL, not YAML, to obtain independent goldens.

    The suite's valid bit describes syntax. Core construction can still fail
    (for example, duplicate keys); that is a separate, legitimate load error.
    """
    if not case["valid"]:
        return {"status": "error", "phase": "syntax", "message": "invalid syntax (yaml-test-suite)"}
    documents, stack, anchors = [], [], {}

    def append(node):
        (stack[-1]["children"] if stack else documents).append(node)

    def properties(line):
        anchor = re.search(r"(?:^| )&([^ ]+)", line)
        tag = re.search(r"(?:^| )<([^>]*)>", line)
        return anchor[1] if anchor else None, tag[1] if tag else None

    for line in case["events"].splitlines():
        if line.startswith("+DOC"):
            anchors = {}
        elif line.startswith(("+MAP", "+SEQ")):
            anchor, tag = properties(line)
            node = {"kind": "mapping" if line.startswith("+MAP") else "sequence", "tag": tag, "children": []}
            append(node)
            if anchor:
                anchors[anchor] = node
            stack.append(node)
        elif line.startswith(("-MAP", "-SEQ")):
            stack.pop()
        elif line.startswith("=VAL"):
            match = re.fullmatch(r"=VAL((?: (?:&\S+|<[^>]*>))*) ([:|'\">])(.*)", line)
            if not match:
                raise ValueError("unrecognized suite scalar event: " + line)
            anchor, tag = properties(match[1])
            node = {"kind": "scalar", "tag": tag, "style": None if match[2] == ":" else match[2], "text": unescape_event(match[3])}
            append(node)
            if anchor:
                anchors[anchor] = node
        elif line.startswith("=ALI *"):
            append(anchors[line[6:]])
        elif not line.startswith(("+STR", "-STR", "-DOC", "+DOC")):
            raise ValueError("unrecognized suite event: " + line)
    if stack:
        raise ValueError("unterminated suite collection")

    def convert(node, active):
        if id(node) in active:
            return ["alias", "cycle"]
        active = active | {id(node)}
        kind, tag = node["kind"], node["tag"]
        if kind == "scalar":
            return core_scalar(node["text"], tag, node["style"])
        children = [convert(child, active) for child in node["children"]]
        if kind == "mapping":
            if len(children) % 2:
                raise ValueError("unpaired suite mapping entry")
            children = [children[index:index + 2] for index in range(0, len(children), 2)]
            keys = [json.dumps(canonical(key), ensure_ascii=False) for key, _ in children]
            if len(keys) != len(set(keys)):
                raise ValueError("duplicate mapping key after Core resolution")
        result = [kind, children]
        return result if tag in (None, TAG_PREFIX + ("map" if kind == "mapping" else "seq")) else ["tagged", tag, result]

    try:
        values = [convert(node, set()) for node in documents]
        value = ["null"] if not values else values[0] if len(values) == 1 else ["stream", values]
        return {"status": "ok", "value": canonical(value)}
    except ValueError as error:
        return {"status": "error", "phase": "resolution", "message": str(error)}


def oracle(text: str) -> dict:
    from ruamel.yaml import YAML
    from ruamel.yaml.events import ScalarEvent
    from ruamel.yaml.nodes import ScalarNode, SequenceNode, MappingNode
    from ruamel.yaml.error import YAMLError, ReusedAnchorWarning

    def parser():
        yaml = YAML(typ="base", pure=True)
        yaml.version = (1, 2)
        return yaml

    previous_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(max(previous_limit, 10000))
    phase = "syntax"
    try:
        events = list(parser().parse(text))
        scalars = [event.value for event in events if isinstance(event, ScalarEvent)]
        properties = {event.start_mark.index: (event.tag, event.style) for event in events if isinstance(event, ScalarEvent)}
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ReusedAnchorWarning)
            docs = list(parser().compose_all(text))

        def convert(node, active: set[int]) -> list:
            if id(node) in active:
                return ["alias", "cycle"]
            active = active | {id(node)}
            if isinstance(node, ScalarNode):
                tag, style = properties[node.start_mark.index]
                return core_scalar(node.value, tag, style)
            if isinstance(node, SequenceNode):
                result = ["sequence", [convert(child, active) for child in node.value]]
                return result if node.tag == TAG_PREFIX + "seq" else ["tagged", node.tag, result]
            if isinstance(node, MappingNode):
                pairs = [[convert(key, active), convert(value, active)] for key, value in node.value]
                keys = [json.dumps(canonical(key), ensure_ascii=False) for key, _ in pairs]
                if len(keys) != len(set(keys)):
                    raise ValueError("duplicate mapping key after Core resolution")
                result = ["mapping", pairs]
                return result if node.tag == TAG_PREFIX + "map" else ["tagged", node.tag, result]
            raise ValueError("unknown composed node")

        phase = "resolution"
        values = [convert(doc, set()) if doc is not None else ["null"] for doc in docs]
        value = values[0] if len(values) == 1 else ["stream", values]
        if not values:
            value = ["null"]  # parse_str's documented empty-input convenience.
        return {"status": "ok", "value": canonical(value), "scalars": scalars}
    except (YAMLError, ValueError) as error:
        return {"status": "error", "phase": phase, "message": str(error), "oracle_error_type": type(error).__name__}
    except (AssertionError, NotImplementedError, RecursionError) as error:
        return {"status": "oracle_failure", "message": str(error), "oracle_error_type": type(error).__name__}
    finally:
        sys.setrecursionlimit(previous_limit)


def failure_reason(row: dict) -> str:
    """Explain a baselined failure for reviewers of known-failures.json."""
    if row["kind"] == "valid_rejection":
        message = (row.get("actual") or {}).get("message", "")
        return "Valid YAML outside the supported subset: " + message if message else "Valid YAML outside the supported subset."
    if row["id"].startswith("core/") and "key" in row["id"]:
        return "By design: mapping keys are their source text (Str), not core-schema typed values."
    return "Known divergence from the YAML 1.2 reference behaviour."


def build(roc: str, parser_root: Path, output_dir: Path, fuzz: str | None = None) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    source = ROOT / "fuzz" / (fuzz + ".roc") if fuzz else DATA / "probe.roc"
    executable = output_dir / "bin" / (fuzz or "probe")
    executable.parent.mkdir(parents=True, exist_ok=True)
    command = [roc, "build", str(source), "--output=" + str(executable)]
    if parser_root != ROOT:
        command.extend(["--replace-dep", str(ROOT / "package" / "main.roc"), str(parser_root / "package" / "main.roc")])
    if fuzz:
        command.insert(2, "--fuzz")
    completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    (output_dir / ((fuzz or "probe") + "-build.log")).write_text(completed.stdout + completed.stderr)
    if completed.returncode:
        raise RuntimeError("build failed; see " + str(output_dir / ((fuzz or "probe") + "-build.log")))
    return executable


def probe(executable: Path, text: str, timeout: float = 2) -> dict:
    try:
        completed = subprocess.run([str(executable)], input=text.encode(), capture_output=True, timeout=timeout, check=False)
    except subprocess.TimeoutExpired:
        return {"status": "timeout"}
    if completed.returncode:
        return {"status": "crash", "return_code": completed.returncode, "stderr": completed.stderr.decode(errors="replace")[:4000]}
    try:
        result = json.loads(completed.stdout)
        if result["status"] == "ok":
            result["value"] = canonical(result["value"])
        elif result["status"] == "error":
            lines = re.split(r"\r\n|\r|\n", text)
            if not (1 <= result["line"] <= len(lines)) or not (1 <= result["column"] <= len(lines[result["line"] - 1].encode()) + 1):
                return {"status": "invalid_diagnostic", "result": result}
        else:
            return {"status": "protocol_error", "result": result}
        return result
    except (ValueError, KeyError, TypeError, IndexError, OverflowError) as error:
        return {"status": "protocol_error", "message": str(error), "stdout": completed.stdout.decode(errors="replace")[:4000]}


def compare(case: dict, actual: dict, expected: dict) -> dict:
    disagreement = None
    if "events" in case:
        golden = suite_expectation(case)
        if golden["status"] != expected["status"]:
            disagreement = "suite expectation disagrees with oracle acceptance"
        elif golden["status"] == "ok" and (golden["value"] != expected["value"] or suite_scalars(case["events"]) != expected["scalars"]):
            disagreement = "suite expectation disagrees with oracle value"
        expected = golden
    if expected["status"] == "oracle_failure":
        disagreement = "oracle failed before producing an expectation"
    elif "events" not in case and "valid" in case and (expected["status"] == "ok") != case["valid"]:
        disagreement = "suite acceptance disagrees with oracle"
    if actual["status"] in FATAL:
        kind = actual["status"]
    elif "events" not in case and disagreement and (case.get("valid") or expected["status"] == "oracle_failure"):
        kind = "oracle_disagreement"
    elif (case.get("valid") is False or expected["status"] == "error"):
        kind = "pass" if actual["status"] == "error" else "invalid_acceptance"
    elif actual["status"] == "error":
        kind = "valid_rejection"
    else:
        kind = "pass" if actual["value"] == expected["value"] else "wrong_value"
    expected = {key: value for key, value in expected.items() if key != "scalars"}
    return {"id": case["id"], "group": case.get("group", "suite"), "input": case["input"], "input_sha256": hashlib.sha256(case["input"].encode()).hexdigest(), "kind": kind, "expected": expected, "actual": actual, "oracle_disagreement": disagreement}


def signature(row: dict) -> str:
    return digest({key: row[key] for key in ("input_sha256", "kind", "expected", "actual", "oracle_disagreement")})


def load_cases() -> list[dict]:
    return json.loads((DATA / "cases.json").read_text()) + json.loads((DATA / "suite.json").read_text())["cases"]


def metadata(roc: str, parser_root: Path, executable: Path) -> dict:
    revision = parser_root / ".yaml-review-source.json"
    result = {"roc": subprocess.check_output([roc, "version"], text=True).strip(), "python": sys.version, "oracle": importlib.metadata.version("ruamel.yaml"), "parser_root": str(parser_root), "yaml_sha256": hashlib.sha256((parser_root / "package" / "Yaml.roc").read_bytes()).hexdigest(), "binary_sha256": hashlib.sha256(executable.read_bytes()).hexdigest(), "suite": json.loads((DATA / "suite.json").read_text())["source"], "fuzz": {"url": FUZZ_URL, "sha256": FUZZ_SHA256}}
    if revision.exists():
        result["source_revision"] = json.loads(revision.read_text())
    else:
        git_root = subprocess.run(["git", "-C", str(parser_root), "rev-parse", "--show-toplevel"], capture_output=True, text=True)
        if git_root.returncode == 0 and Path(git_root.stdout.strip()).resolve() == parser_root:
            result["source_revision"] = subprocess.check_output(["git", "-C", str(parser_root), "rev-parse", "HEAD"], text=True).strip()
    return result


def check(args, executable: Path) -> int:
    cases = load_cases()
    if args.case:
        cases = [case for case in cases if case["id"] in args.case]
        missing = set(args.case) - {case["id"] for case in cases}
        if missing:
            raise ValueError("unknown case(s): " + ", ".join(sorted(missing)))
    rows = [compare(case, probe(executable, case["input"]), oracle(case["input"])) for case in cases]
    failures = [row for row in rows if row["kind"] != "pass"]
    report = {"metadata": metadata(args.roc, args.parser_root, executable), "counts": dict(Counter(row["kind"] for row in rows)), "total": len(rows), "oracle_disagreements": sum(bool(row["oracle_disagreement"]) for row in rows), "cases": rows}
    write_json(args.output_dir / "check.json", report)
    print(json.dumps({"total": len(rows), "counts": report["counts"], "oracle_disagreements": report["oracle_disagreements"], "report": str(args.output_dir / "check.json")}, indent=2))
    if args.record_known_failures:
        if any(row["kind"] in FATAL for row in rows):
            raise ValueError("crashes, hangs, protocol errors, and invalid diagnostics cannot be baselined")
        write_json(args.record_known_failures, {"metadata": report["metadata"], "failures": {row["id"]: {"signature": signature(row), "kind": row["kind"], "reason": failure_reason(row)} for row in failures}})
        return 0
    if args.baseline:
        known = json.loads(args.baseline.read_text())["failures"]
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


def generated_inputs(seed: int):
    rng = random.Random(seed)
    reviewed = [case["input"] for case in load_cases()]
    while True:
        style = rng.choice(["|", ">"])
        chomp = rng.choice(["", "+", "-"])
        offset = rng.randint(1, 9)
        header = style + rng.choice([chomp, str(offset) + chomp, chomp + str(offset)])
        body = []
        for _ in range(rng.randrange(0, 12)):
            content = rng.choice(["", "a", "b # literal", "  code", "\ttab", "λ", "---", "...", " "])
            body.append(" " * offset + content)
        text = "value: " + header + "\n" + "\n".join(body)
        text += rng.choice(["", "\n", "\n\n", "\nnext: done\n"])
        if rng.choice([False, True]):
            text = rng.choice(reviewed)
            if text:
                position = rng.randrange(len(text))
                text = text[:position] + rng.choice(["", "#", "\t", "\n", ":", "'", "0", "-"]) + text[position + rng.randrange(2):]
        if rng.randrange(5) == 0:
            text = text.replace("\n", rng.choice(["\r\n", "\r"]))
        if len(text.encode()) <= 4096:
            yield text


def minimize_text(text: str, predicate: Callable[[str], bool], max_checks: int = 200) -> tuple[str, int]:
    """Bounded delta debugging; predicate must retain the original failure class."""
    checks, chunks = 0, 2
    while len(text) > 1 and checks < max_checks:
        width = math.ceil(len(text) / chunks)
        reduced = False
        for start in range(0, len(text), width):
            candidate = text[:start] + text[start + width:]
            checks += 1
            if predicate(candidate):
                text, chunks, reduced = candidate, max(2, chunks - 1), True
                break
            if checks >= max_checks:
                break
        if not reduced:
            if chunks >= len(text):
                break
            chunks = min(len(text), chunks * 2)
    return text, checks


def replay_row(executable: Path, text: str) -> dict:
    return compare({"id": "replay", "input": text, "group": "discovery"}, probe(executable, text), oracle(text))


def differential(executable: Path, seconds: float, seed: int, output_dir: Path) -> dict:
    start = time.monotonic()
    counts: Counter = Counter()
    examples: dict[str, dict] = {}
    tested = 0
    for text in generated_inputs(seed):
        if time.monotonic() - start >= seconds:
            break
        row = replay_row(executable, text)
        counts[row["kind"]] += 1
        tested += 1
        # A review campaign collects wrong answers rather than stopping at its
        # first known bug. Keep bounded examples per result/error class.
        if row["kind"] != "pass":
            actual = row["actual"]
            key = row["kind"] + ":" + (actual.get("message", "") if actual["status"] == "error" else row["expected"]["status"])
            if (key not in examples and len(examples) < 64) or (key in examples and len(text) < len(examples[key]["input"])):
                examples[key] = row
        if row["kind"] in FATAL:
            break
    result = {"seed": seed, "seconds": time.monotonic() - start, "executions": tested, "counts": dict(counts), "examples": list(examples.values())}
    write_json(output_dir / "differential.json", result)
    return result


def fuzz_run(binary: Path, seconds: float, seed: int, output_dir: Path, target: str, smoke: bool = False) -> dict:
    work = output_dir / target
    corpus = work / "corpus"
    corpus.mkdir(parents=True, exist_ok=True)
    if target == "yaml-raw":
        for case in load_cases():
            raw = case["input"].encode()
            if len(raw) <= 4096:
                (corpus / hashlib.sha256(raw).hexdigest()).write_bytes(raw)
    else:
        for value in range(16):
            raw = bytes([value, 0, 1, 2, 3])
            (corpus / hashlib.sha256(raw).hexdigest()).write_bytes(raw)
    limit = "--runs=2000" if smoke else "--time=" + str(max(1, int(seconds)))
    command = [str(binary), "ci", str(work / "evidence"), str(corpus), limit, "--seed=" + str(seed), "--max-input-size=4096", "--timeout=2", "--memory-limit=512", "--print-final-stats"]
    if target == "yaml-raw":
        command.append("--dictionary=" + str(ROOT / "fuzz" / "dictionaries" / "yaml.dict"))
    start = time.monotonic()
    try:
        with (work / "runner.log").open("w") as log:
            completed = subprocess.run(command, cwd=work, stdout=log, stderr=subprocess.STDOUT, timeout=60 if smoke else seconds + 15)
        code = completed.returncode
    except subprocess.TimeoutExpired:
        code = 124
    return {"target": target, "command": command, "return_code": code, "seconds": time.monotonic() - start, "artifacts": str(work)}


def campaign(args, executable: Path) -> int:
    raw = build(args.roc, args.parser_root, args.output_dir, "yaml-raw")
    if args.smoke:
        results = [fuzz_run(raw, 0, args.seed, args.output_dir, "yaml-raw", True)]
        write_json(args.output_dir / "campaign.json", {"metadata": metadata(args.roc, args.parser_root, executable), "fuzz": results})
        print(json.dumps(results, indent=2))
        return int(any(result["return_code"] for result in results))
    block = build(args.roc, args.parser_root, args.output_dir, "yaml-block")
    results = [fuzz_run(raw, args.seconds * .4, args.seed, args.output_dir, "yaml-raw"), fuzz_run(block, args.seconds * .3, args.seed, args.output_dir, "yaml-block")]
    diff = differential(executable, args.seconds * .3, args.seed, args.output_dir)
    report = {"metadata": metadata(args.roc, args.parser_root, executable), "budget_seconds": args.seconds, "fuzz": results, "differential": diff}
    write_json(args.output_dir / "campaign.json", report)
    print(json.dumps({"fuzz": results, "differential": {k: v for k, v in diff.items() if k != "examples"}}, indent=2))
    return int(any(result["return_code"] for result in results) or any(kind != "pass" for kind in diff["counts"]))


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("check", "campaign", "replay", "minimize"):
        operation = sub.add_parser(name)
        operation.add_argument("--roc", default=os.environ.get("ROC", "roc"))
        operation.add_argument("--parser-root", type=Path, default=ROOT)
        operation.add_argument("--output-dir", type=Path, default=DEFAULT_WORK / name)
        if name == "check":
            operation.add_argument("--case", action="append")
            baseline = operation.add_mutually_exclusive_group()
            baseline.add_argument("--baseline", type=Path)
            baseline.add_argument("--record-known-failures", type=Path)
        if name == "campaign":
            operation.add_argument("--seconds", type=int, default=300)
            operation.add_argument("--seed", type=int, default=80)
            operation.add_argument("--smoke", action="store_true")
        if name in ("replay", "minimize"):
            operation.add_argument("input", type=Path)
        if name == "minimize":
            operation.add_argument("output", type=Path)
            operation.add_argument("--max-checks", type=int, default=200)
    args = parser.parse_args(argv)
    # The nesting-limit cases are hundreds of levels deep, and canonicalising
    # them recurses once per level; older Pythons default to 1000 frames.
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 20000))
    args.parser_root = args.parser_root.expanduser().resolve()
    args.output_dir = args.output_dir.expanduser().resolve()
    args.roc = str(Path(args.roc).expanduser().resolve()) if "/" in args.roc else args.roc
    try:
        if importlib.metadata.version("ruamel.yaml") != "0.18.16":
            raise ValueError("install the pinned oracle from scripts/yaml/requirements.txt")
        if args.command == "campaign" and args.seconds < 1:
            raise ValueError("--seconds must be positive")
        executable = build(args.roc, args.parser_root, args.output_dir)
        if args.command == "check":
            return check(args, executable)
        if args.command == "campaign":
            return campaign(args, executable)
        text = args.input.read_bytes().decode("utf-8")
        row = replay_row(executable, text)
        if args.command == "minimize":
            if row["kind"] in ("pass", "oracle_disagreement"):
                raise ValueError("input does not reproduce a parser failure")
            if args.output.exists():
                raise ValueError("refusing to overwrite minimized output")
            # Retain the mismatch class and oracle acceptance. Turning a
            # valid-value bug into malformed input is not a valid reduction.
            def retains(candidate):
                other = replay_row(executable, candidate)
                return other["kind"] == row["kind"] and other["expected"]["status"] == row["expected"]["status"]
            text, checks = minimize_text(text, retains, args.max_checks)
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_bytes(text.encode())
            row = replay_row(executable, text)
            row["reduction_checks"] = checks
        write_json(args.output_dir / "replay.json", {"metadata": metadata(args.roc, args.parser_root, executable), "case": row})
        print(json.dumps(row, ensure_ascii=False, indent=2))
        return int(row["kind"] != "pass")
    except (ValueError, RuntimeError, OSError, importlib.metadata.PackageNotFoundError) as error:
        parser.exit(2, "error: " + str(error) + "\n")


if __name__ == "__main__":
    raise SystemExit(main())
