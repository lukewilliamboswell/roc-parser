#!/usr/bin/env python3
"""Reproducible XML 1.0 well-formedness checks for package/Xml.roc.

Oracles:
- expat (Python's stdlib ``pyexpat``), the reference non-validating parser,
  for well-formedness verdicts and the element/attribute/text tree;
- the W3C XML Conformance Test Suite (xmlconf 2013-09-23), fetched on demand
  into the work directory and pinned by sha256, for curated valid/not-wf cases.

Only cases inside the parser's claimed subset are selected: UTF-8 text, no
document type declaration, XML 1.0. Known failures are explicit and carry a
reason; discovery never updates the baseline.

Known oracle disagreements (reported, not failures): expat uses the pre-Fifth
Edition name character tables, so names such as <IllegalExtender〆/> that are
well-formed under XML 1.0 (5th ed.) [4]/[4a] are rejected by expat; the suite
verdict wins there. Expat also does not check VersionNum [26].

Usage:
  python3 scripts/review_xml.py check [--baseline scripts/xml/known-failures.json]
  python3 scripts/review_xml.py corpus TARGET CORPUS_DIR   # cross-check a fuzz corpus against expat
"""

from __future__ import annotations

import os
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import tarfile
import urllib.request
import xml.dom.minidom
from xml.parsers import expat

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "xml"
DEFAULT_WORK = ROOT / ".roc-parser-tmp" / "xml-review"
DEFAULT_ROC = Path(os.environ.get("ROC", "roc"))
SUITE_URL = "https://www.w3.org/XML/Test/xmlts20130923.tar.gz"
SUITE_SHA256 = "9b61db9f5dbffa545f4b8d78422167083a8568c59bd1129f94138f936cf6fc1f"
FIFTH_EDITION_ONLY = "\U00010000"  # a name character expat (pre-5th edition tables) rejects
FATAL = {"crash", "timeout", "protocol_error", "invalid_diagnostic"}


def write_json(path: Path, value) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def oracle(text: str) -> dict:
    """Parse with expat and build the tree Xml.roc is specified to produce."""
    parser = expat.ParserCreate()
    parser.ordered_attributes = True
    parser.buffer_text = True
    stack: list[list] = [["document", None, [], []]]
    state = {"doctype": False, "declaration": None}

    def start(name, attributes):
        node = ["element", name, [list(pair) for pair in zip(attributes[::2], attributes[1::2])], []]
        stack[-1][3].append(node)
        stack.append(node)

    def end(_name):
        stack.pop()

    def character_data(data):
        children = stack[-1][3]
        if children and children[-1][0] == "text":
            children[-1][1] += data
        else:
            children.append(["text", data])

    def declaration(_version, encoding, _standalone):
        state["declaration"] = {"encoding": None if encoding is None else ("utf-8" if encoding.lower() == "utf-8" else encoding)}

    def doctype(*_):
        state["doctype"] = True

    parser.StartElementHandler = start
    parser.EndElementHandler = end
    parser.CharacterDataHandler = character_data
    parser.XmlDeclHandler = declaration
    parser.StartDoctypeDeclHandler = doctype
    try:
        parser.Parse(text, True)
    except expat.ExpatError as error:
        if state["doctype"]:
            return {"status": "unsupported", "message": "document type declaration"}
        return {"status": "error", "message": str(error), "line": error.lineno, "column": error.offset + 1}
    if state["doctype"]:
        return {"status": "unsupported", "message": "document type declaration"}
    roots = [node for node in stack[0][3] if node[0] == "element"]
    return {"status": "ok", "declaration": state["declaration"], "root": roots[0]}


def build(roc: str, output_dir: Path, fuzz: str | None = None) -> Path:
    source = ROOT / "fuzz" / (fuzz + ".roc") if fuzz else DATA / "probe.roc"
    executable = output_dir / "bin" / (fuzz or "probe")
    executable.parent.mkdir(parents=True, exist_ok=True)
    command = [roc, "build", str(source), "--output=" + str(executable)]
    if fuzz:
        command.insert(2, "--fuzz")
    completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    log = output_dir / ((fuzz or "probe") + "-build.log")
    log.write_text(completed.stdout + completed.stderr)
    if completed.returncode:
        raise RuntimeError("build failed; see " + str(log))
    return executable


def probe(executable: Path, text: str, timeout: float = 5) -> dict:
    try:
        completed = subprocess.run([str(executable)], input=text.encode(), capture_output=True, timeout=timeout, check=False)
    except subprocess.TimeoutExpired:
        return {"status": "timeout"}
    if completed.returncode:
        return {"status": "crash", "return_code": completed.returncode, "stderr": completed.stderr.decode(errors="replace")[:4000]}
    try:
        result = json.loads(completed.stdout)
        if result["status"] == "error":
            lines = re.split(r"\r\n|\r|\n", text)
            if not result["message"] or not (1 <= result["line"] <= len(lines)) or not (1 <= result["column"] <= len(lines[result["line"] - 1].encode()) + 1):
                return {"status": "invalid_diagnostic", "result": result}
        elif result["status"] != "ok":
            return {"status": "protocol_error", "result": result}
        return result
    except (ValueError, KeyError, TypeError, IndexError) as error:
        return {"status": "protocol_error", "message": str(error), "stdout": completed.stdout.decode(errors="replace")[:4000]}


def compare(case: dict, actual: dict, expected: dict) -> dict:
    disagreement = None
    verdict = case.get("valid")
    if verdict is not None and expected["status"] in ("ok", "error") and (expected["status"] == "ok") != verdict:
        disagreement = "suite verdict disagrees with expat"
    if verdict is None:
        verdict = expected["status"] == "ok"
    if actual["status"] in FATAL:
        kind = actual["status"]
    elif expected["status"] == "unsupported":
        kind = "pass" if actual["status"] == "error" else "unsupported_acceptance"
    elif not verdict:
        kind = "pass" if actual["status"] == "error" else "invalid_acceptance"
    elif actual["status"] == "error":
        kind = "valid_rejection"
    elif expected["status"] != "ok":
        kind = "pass"  # the suite says well-formed and expat disagrees; no tree to compare
    else:
        same = actual["root"] == expected["root"] and actual["declaration"] == expected["declaration"]
        kind = "pass" if same else "wrong_value"
    return {
        "id": case["id"],
        "input": case["input"],
        "input_sha256": hashlib.sha256(case["input"].encode()).hexdigest(),
        "kind": kind,
        "expected": expected,
        "actual": actual,
        "oracle_disagreement": disagreement,
    }


def signature(row: dict) -> str:
    stable = {"input_sha256": row["input_sha256"], "kind": row["kind"], "actual_status": row["actual"]["status"]}
    return hashlib.sha256(json.dumps(stable, sort_keys=True).encode()).hexdigest()


def fetch_suite(work: Path) -> Path:
    archive = work / "xmlts20130923.tar.gz"
    if not archive.exists():
        work.mkdir(parents=True, exist_ok=True)
        urllib.request.urlretrieve(SUITE_URL, archive)
    actual = hashlib.sha256(archive.read_bytes()).hexdigest()
    if actual != SUITE_SHA256:
        raise ValueError(f"xmlconf archive sha256 {actual} != pinned {SUITE_SHA256}")
    root = work / "xmlconf"
    if not root.exists():
        with tarfile.open(archive) as tar:
            tar.extractall(work, filter="data")
    return root


def suite_cases(work: Path) -> list[dict]:
    """Select xmlconf XML 1.0 cases inside the claimed subset."""
    root = fetch_suite(work)
    index = (root / "xmlconf.xml").read_text(encoding="utf-8")
    cases = []
    for file in re.findall(r'<!ENTITY\s+\S+\s+SYSTEM\s+"([^"]+)"\s*>', index):
        cases += suite_file_cases(root / file)
    return cases


def suite_file_cases(file: Path) -> list[dict]:
    cases = []
    # Some suite files are fragments of TEST elements, so wrap them in a root.
    body = file.read_text(encoding="utf-8")
    body = re.sub(r"\A\s*<\?xml[^>]*\?>", "", body)
    body = re.sub(r"<!DOCTYPE[^\[>]*(\[.*?\]\s*)?>", "", body, count=1, flags=re.S)
    document = xml.dom.minidom.parseString("<WRAPPER>" + body + "</WRAPPER>")
    for test in document.getElementsByTagName("TEST"):
        kind = test.getAttribute("TYPE")
        if kind not in ("valid", "invalid", "not-wf"):
            continue
        if test.getAttribute("RECOMMENDATION") not in ("", "XML1.0", "XML1.0-errata2e", "XML1.0-errata3e", "XML1.0-errata4e"):
            continue
        editions = test.getAttribute("EDITION").split()
        if editions and "5" not in editions:
            continue
        if test.getAttribute("ENTITIES") not in ("", "none"):
            continue
        prefix, node = "", test.parentNode
        while node is not None and node.nodeType == node.ELEMENT_NODE:
            prefix = node.getAttribute("xml:base") + prefix
            node = node.parentNode
        path = file.parent / prefix / test.getAttribute("URI")
        try:
            raw = path.read_bytes()
            text = raw.decode("utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        declared = re.match(r"<\?xml[^>]*encoding\s*=\s*[\"']([^\"']*)", text)
        if declared and declared[1].lower() not in ("utf-8", "us-ascii", "ascii"):
            continue
        if "<!DOCTYPE" in text:
            continue  # DTDs are outside the claimed subset (see Xml.roc module docs)
        cases.append({"id": "xmlconf/" + test.getAttribute("ID"), "input": text, "valid": kind != "not-wf"})
    return cases


def load_cases(args) -> list[dict]:
    cases = json.loads((DATA / "cases.json").read_text())
    if not args.no_suite:
        cases += suite_cases(args.work_dir)
    return cases


def check(args) -> int:
    executable = build(args.roc, args.work_dir)
    rows = [compare(case, probe(executable, case["input"]), oracle(case["input"])) for case in load_cases(args)]
    failures = [row for row in rows if row["kind"] != "pass"]
    report = {
        "suite": {"url": SUITE_URL, "sha256": SUITE_SHA256},
        "expat": expat.EXPAT_VERSION,
        "counts": dict(Counter(row["kind"] for row in rows)),
        "total": len(rows),
        "oracle_disagreements": [row["id"] for row in rows if row["oracle_disagreement"]],
        "cases": rows,
    }
    write_json(args.work_dir / "check.json", report)
    print(json.dumps({key: report[key] for key in ("total", "counts", "oracle_disagreements")}, indent=2))
    if args.baseline:
        known = json.loads(args.baseline.read_text())["failures"]
        new = [row["id"] for row in failures if row["kind"] in FATAL or known.get(row["id"], {}).get("signature") != signature(row)]
        stale = sorted(set(known) - {row["id"] for row in failures})
        if new or stale:
            print(json.dumps({"new_or_changed_failures": new, "stale_known_failures": stale}, indent=2))
            return 1
        return 0
    if args.print_signatures:
        for row in failures:
            print(row["id"], signature(row), row["kind"])
    return int(bool(failures))


def corpus(args) -> int:
    """Render each corpus input with the fuzz target's `show` and compare probe and expat."""
    executable = build(args.roc, args.work_dir)
    target = build(args.roc, args.work_dir, args.target)
    counts, mismatches = Counter(), []
    for path in sorted(args.corpus.iterdir()):
        shown = subprocess.run([str(target), "show", str(path)], capture_output=True, text=True, check=False).stdout
        match = re.search(r"XML-HEX: ([0-9a-f]*)", shown)
        if not match:
            counts["no_document"] += 1
            continue
        text = bytes.fromhex(match[1]).decode("utf-8")
        actual, expected = probe(executable, text), oracle(text)
        if expected["status"] == "error" and FIFTH_EDITION_ONLY in text:
            # expat's pre-Fifth Edition name tables reject U+10000 in names;
            # compare with an ASCII stand-in and count the disagreement.
            counts["expat_fifth_edition_names"] += 1
            expected = oracle(text.replace(FIFTH_EDITION_ONLY, "U"))
            actual = json.loads(json.dumps(actual).replace(json.dumps(FIFTH_EDITION_ONLY)[1:-1], "U"))
        row = compare({"id": path.name, "input": text}, actual, expected)
        counts[row["kind"]] += 1
        if row["kind"] != "pass":
            mismatches.append(row)
    write_json(args.work_dir / "corpus.json", {"counts": dict(counts), "mismatches": mismatches})
    print(json.dumps({"counts": dict(counts), "report": str(args.work_dir / "corpus.json")}, indent=2))
    return int(bool(mismatches))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--roc", default=str(DEFAULT_ROC))
    parser.add_argument("--work-dir", type=Path, default=DEFAULT_WORK)
    commands = parser.add_subparsers(dest="command", required=True)
    check_parser = commands.add_parser("check")
    check_parser.add_argument("--baseline", type=Path)
    check_parser.add_argument("--no-suite", action="store_true")
    check_parser.add_argument("--print-signatures", action="store_true")
    corpus_parser = commands.add_parser("corpus")
    corpus_parser.add_argument("target")
    corpus_parser.add_argument("corpus", type=Path)
    args = parser.parse_args()
    sys.setrecursionlimit(100000)
    return check(args) if args.command == "check" else corpus(args)


if __name__ == "__main__":
    sys.exit(main())
