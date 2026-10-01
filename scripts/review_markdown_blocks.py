#!/usr/bin/env python3
"""Reproducible CommonMark/GFM block-structure checks for package/Markdown.roc.

Install scripts/markdown/blocks/requirements.txt in an external virtual environment
first (for example under .roc-parser-tmp/). The oracle is cmark-gfm, the
CommonMark reference implementation with GitHub's table, task list, and
strikethrough extensions, reached through the pinned ``cmarkgfm`` wheel. Its
XML renderer gives the syntax tree directly, so no HTML scraping is involved.

Every comparison is reported at three levels, most severe first:

* ``wrong_block``  block skeleton differs (node kinds, heading levels, list
  kind/start/tightness/tasks, code info and text, raw HTML, table shape);
* ``wrong_text``   skeleton matches, but the plain text of some inline content
  differs (formatting ignored);
* ``wrong_inline`` only inline structure differs (emphasis, links, ...).

``wrong_block`` is owned by the block parser. ``wrong_inline`` is owned by the
inline parser. ``wrong_text`` needs triage, since either layer can cause it.
The Markdown module never rejects input, so ``error`` is always a failure.

Subcommands:
  check       spec.json (CommonMark 0.31.2) + cases.json against the oracle
  replay      one file
  crosscheck  ``<fuzz-binary> show`` every corpus file of a generator target
              and compare the generator's expectation with the oracle
  differential  probe and oracle on every file of a raw-text corpus
"""

from __future__ import annotations

import argparse
from collections import Counter
import ctypes
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "markdown" / "blocks"
DEFAULT_WORK = ROOT / ".roc-parser-tmp" / "markdown-blocks-review"
ORACLE_VERSION = "2025.10.22"
FATAL = {"crash", "timeout", "protocol_error", "error"}
SEVERITY = ["pass", "wrong_inline", "wrong_text", "wrong_block"]
NS = "{http://commonmark.org/xml/1.0}"


def write_json(path: Path, value) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def digest(value) -> str:
    return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True).encode()).hexdigest()


# --------------------------------------------------------------------------
# Oracle: cmark-gfm through ctypes on the cmarkgfm wheel's shared library.

_LIB = None


def _lib():
    global _LIB
    if _LIB is None:
        import cmarkgfm

        lib = ctypes.CDLL(os.path.join(os.path.dirname(cmarkgfm.__file__), "_cmark.abi3.so"))
        for name in ("cmark_parser_new", "cmark_parser_finish", "cmark_find_syntax_extension", "cmark_render_xml"):
            getattr(lib, name).restype = ctypes.c_void_p
        lib.cmark_version_string.restype = ctypes.c_char_p
        lib.cmark_parser_new.argtypes = [ctypes.c_int]
        lib.cmark_parser_feed.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
        lib.cmark_parser_finish.argtypes = [ctypes.c_void_p]
        lib.cmark_parser_free.argtypes = [ctypes.c_void_p]
        lib.cmark_node_free.argtypes = [ctypes.c_void_p]
        lib.cmark_parser_attach_syntax_extension.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
        lib.cmark_render_xml.argtypes = [ctypes.c_void_p, ctypes.c_int]
        lib.free.argtypes = [ctypes.c_void_p]
        lib.cmark_gfm_core_extensions_ensure_registered()
        _LIB = lib
    return _LIB


def oracle_version() -> str:
    return _lib().cmark_version_string().decode()


def oracle_xml(text: str) -> str:
    lib = _lib()
    parser = lib.cmark_parser_new(0)
    try:
        # The extensions the module documents: pipe tables, task list items,
        # and ~~strikethrough~~. Autolink literals are inline-level; tagfilter
        # is an HTML rendering policy, not syntax.
        for name in (b"table", b"tasklist", b"strikethrough"):
            lib.cmark_parser_attach_syntax_extension(parser, lib.cmark_find_syntax_extension(name))
        data = text.encode("utf-8")
        lib.cmark_parser_feed(parser, data, len(data))
        root = lib.cmark_parser_finish(parser)
        raw = lib.cmark_render_xml(root, 0)
        xml = ctypes.string_at(raw).decode("utf-8")
        lib.free(raw)
        lib.cmark_node_free(root)
        return xml
    finally:
        lib.cmark_parser_free(parser)


def _tag(element) -> str:
    return element.tag.removeprefix(NS)


# cmark's XML renderer writes control characters unescaped, which XML 1.0
# forbids, and an XML parser would normalise tabs and line endings in
# attributes. Every C0 control character is moved to a private-use code point
# before parsing and moved back in each extracted string.
def _hide_controls(xml: str) -> str:
    return re.sub(r"[\x00-\x1f]", lambda match: chr(0xF0000 + ord(match[0])), xml)


def _t(value: str | None) -> str:
    return re.sub("[\U000F0000-\U000F001F]", lambda match: chr(ord(match[0]) - 0xF0000), value or "")


def _inlines(element) -> list:
    out = []
    for child in element:
        kind = _tag(child)
        if kind == "text":
            out.append(["text", _t(child.text)])
        elif kind == "softbreak":
            # The Roc AST has no soft-break node; a soft break stays a line
            # feed inside the surrounding text.
            out.append(["text", "\n"])
        elif kind == "linebreak":
            out.append(["br"])
        elif kind == "code":
            out.append(["code", _t(child.text)])
        elif kind == "html_inline":
            out.append(["html", _t(child.text)])
        elif kind == "emph":
            out.append(["emph", _inlines(child)])
        elif kind == "strong":
            out.append(["strong", _inlines(child)])
        elif kind == "strikethrough":
            out.append(["del", _inlines(child)])
        elif kind in ("link", "image"):
            out.append([kind, _t(child.get("destination")), _t(child.get("title")) or None, _inlines(child)])
        else:
            raise ValueError("unexpected inline node " + kind)
    return out


def _blocks(element) -> list:
    out = []
    for child in element:
        kind = _tag(child)
        if kind == "paragraph":
            out.append(["paragraph", _inlines(child)])
        elif kind == "heading":
            out.append(["heading", int(child.get("level")), _inlines(child)])
        elif kind == "block_quote":
            out.append(["blockquote", _blocks(child)])
        elif kind == "list":
            start = "bullet" if child.get("type") == "bullet" else int(child.get("start", "1"))
            items = []
            for item in child:
                if _tag(item) == "tasklist":
                    task = "checked" if item.get("completed") == "true" else "unchecked"
                else:
                    task = "none"
                items.append([task, _blocks(item)])
            out.append(["list", start, child.get("tight") == "true", items])
        elif kind == "code_block":
            out.append(["code", _t(child.get("info")), _t(child.text)])
        elif kind == "html_block":
            out.append(["html", _t(child.text)])
        elif kind == "thematic_break":
            out.append(["hr"])
        elif kind == "table":
            rows = list(child)
            header = rows[0]
            align = [cell.get("align") or "default" for cell in header]
            out.append(["table", align, [_inlines(cell) for cell in header], [[_inlines(cell) for cell in row] for row in rows[1:]]])
        else:
            raise ValueError("unexpected block node " + kind)
    return out


def split_frontmatter(text: str):
    """Mirror of the module's documented extension: a first line that is
    exactly ``---`` and a later line that is exactly ``---`` delimit raw
    frontmatter, which is not Markdown. Lines end at LF, CRLF or CR, and NUL
    becomes U+FFFD, as everywhere else in the document."""
    lines = re.findall(r"[^\r\n]*(?:\r\n|\r|\n)|[^\r\n]+$", text)
    bare = [re.sub(r"[\r\n]+$", "", line) for line in lines]
    if not bare or bare[0] != "---":
        return None, text
    for index in range(1, len(bare)):
        if bare[index] == "---":
            raw = "".join(line + "\n" for line in bare[1:index]).replace("\0", "\ufffd")
            return raw, "".join(lines[index + 1:])
    return None, text


def oracle(text: str) -> dict:
    try:
        front, body = split_frontmatter(text)
        blocks = _blocks(ET.fromstring(_hide_controls(oracle_xml(body).split("\n", 2)[2].strip())))
        if front is not None:
            blocks.insert(0, ["frontmatter", front])
        return {"status": "ok", "value": normalize(blocks)}
    except (ValueError, ET.ParseError) as error:
        return {"status": "oracle_failure", "message": str(error)}


# --------------------------------------------------------------------------
# Normalization and comparison.


def normalize_inlines(inlines: list) -> list:
    out = []
    for node in inlines:
        kind = node[0]
        if kind in ("strong", "emph", "del"):
            node = [kind, normalize_inlines(node[1])]
        elif kind in ("link", "image"):
            node = [kind, node[1], node[2] or None, normalize_inlines(node[3])]
        if kind == "text":
            if not node[1]:
                continue
            if out and out[-1][0] == "text":
                out[-1] = ["text", out[-1][1] + node[1]]
                continue
        out.append(node)
    return out


def normalize(blocks: list) -> list:
    out = []
    for block in blocks:
        kind = block[0]
        if kind == "paragraph":
            block = ["paragraph", normalize_inlines(block[1])]
        elif kind == "heading":
            block = ["heading", block[1], normalize_inlines(block[2])]
        elif kind == "blockquote":
            block = ["blockquote", normalize(block[1])]
        elif kind == "list":
            block = ["list", block[1], block[2], [[task, normalize(children)] for task, children in block[3]]]
        elif kind == "table":
            block = ["table", block[1], [normalize_inlines(c) for c in block[2]], [[normalize_inlines(c) for c in row] for row in block[3]]]
        out.append(block)
    return out


def plain(inlines: list) -> str:
    parts = []
    for node in inlines:
        kind = node[0]
        if kind in ("text", "code", "html"):
            parts.append(node[1])
        elif kind == "br":
            parts.append("\n")
        elif kind in ("strong", "emph", "del"):
            parts.append(plain(node[1]))
        elif kind in ("link", "image"):
            parts.append(plain(node[3]))
    return "".join(parts)


def project(blocks: list, inline) -> list:
    """Replace every inline list by ``inline(list)``."""
    out = []
    for block in blocks:
        kind = block[0]
        if kind == "paragraph":
            out.append(["paragraph", inline(block[1])])
        elif kind == "heading":
            out.append(["heading", block[1], inline(block[2])])
        elif kind == "blockquote":
            out.append(["blockquote", project(block[1], inline)])
        elif kind == "list":
            out.append(["list", block[1], block[2], [[task, project(children, inline)] for task, children in block[3]]])
        elif kind == "table":
            out.append(["table", block[1], [inline(c) for c in block[2]], [[inline(c) for c in row] for row in block[3]]])
        else:
            out.append(block)
    return out


def classify(actual: list, expected: list) -> str:
    if actual == expected:
        return "pass"
    if project(actual, lambda _: None) != project(expected, lambda _: None):
        return "wrong_block"
    if project(actual, plain) != project(expected, plain):
        return "wrong_text"
    return "wrong_inline"


def build(roc: str, output_dir: Path, fuzz: str | None = None) -> Path:
    source = ROOT / "fuzz" / (fuzz + ".roc") if fuzz else DATA / "probe.roc"
    executable = output_dir / "bin" / (fuzz or "probe")
    executable.parent.mkdir(parents=True, exist_ok=True)
    command = [roc, "build", str(source), "--output=" + str(executable)]
    if fuzz:
        command.insert(2, "--fuzz")
    completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    (output_dir / ((fuzz or "probe") + "-build.log")).write_text(completed.stdout + completed.stderr)
    if completed.returncode:
        raise RuntimeError("build failed; see " + str(output_dir / ((fuzz or "probe") + "-build.log")))
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
        if result["status"] == "ok":
            result["value"] = normalize(result["value"])
        elif result["status"] != "error":
            return {"status": "protocol_error", "result": result}
        return result
    except (ValueError, KeyError, TypeError, IndexError) as error:
        return {"status": "protocol_error", "message": str(error), "stdout": completed.stdout.decode(errors="replace")[:4000]}


def compare(case: dict, actual: dict, expected: dict) -> dict:
    disagreement = None
    if "html" in case:
        # The oracle predates spec 0.31.2 (cmark-gfm 0.29.0.gfm.13). Detect the
        # examples where it no longer matches the spec's own HTML.
        import cmarkgfm
        from cmarkgfm.cmark import Options

        if cmarkgfm.markdown_to_html(case["input"], options=Options.CMARK_OPT_UNSAFE) != case["html"]:
            disagreement = "cmark-gfm HTML differs from spec 0.31.2 example HTML"
    if expected["status"] != "ok":
        kind = "oracle_failure"
    elif actual["status"] in FATAL:
        kind = actual["status"]
    else:
        kind = classify(actual["value"], expected["value"])
    return {"id": case["id"], "section": case.get("section", case.get("group", "")), "input": case["input"], "input_sha256": hashlib.sha256(case["input"].encode()).hexdigest(), "kind": kind, "expected": expected, "actual": actual, "oracle_disagreement": disagreement}


def signature(row: dict) -> str:
    return digest({key: row[key] for key in ("input_sha256", "kind", "expected", "actual")})


def load_cases() -> list[dict]:
    spec = json.loads((DATA / "spec.json").read_text())
    cases = [{"id": "spec/%03d" % example["example"], "section": example["section"], "input": example["markdown"], "html": example["html"]} for example in spec]
    return cases + json.loads((DATA / "cases.json").read_text())


def metadata(roc: str, executable: Path) -> dict:
    result = {"roc": subprocess.check_output([roc, "version"], text=True).strip(), "python": sys.version.split()[0], "oracle": {"package": "cmarkgfm " + importlib.metadata.version("cmarkgfm"), "cmark_gfm": oracle_version(), "extensions": ["table", "tasklist", "strikethrough"]}, "spec": {"version": "0.31.2", "sha256": hashlib.sha256((DATA / "spec.json").read_bytes()).hexdigest()}, "markdown_sha256": hashlib.sha256((ROOT / "package" / "Markdown.roc").read_bytes()).hexdigest()}
    revision = subprocess.run(["git", "-C", str(ROOT), "rev-parse", "HEAD"], capture_output=True, text=True)
    if revision.returncode == 0:
        result["source_revision"] = revision.stdout.strip()
    return result


def check(args, executable: Path) -> int:
    cases = load_cases()
    if args.case:
        cases = [case for case in cases if case["id"] in args.case]
    if args.section:
        cases = [case for case in cases if case["section"] in args.section]
    rows = [compare(case, probe(executable, case["input"]), oracle(case["input"])) for case in cases]
    failures = [row for row in rows if row["kind"] != "pass"]
    by_section: dict = {}
    for row in rows:
        by_section.setdefault(row["section"], Counter())[row["kind"]] += 1
    report = {"metadata": metadata(args.roc, executable), "counts": dict(Counter(row["kind"] for row in rows)), "total": len(rows), "oracle_disagreements": [row["id"] for row in rows if row["oracle_disagreement"]], "sections": {k: dict(v) for k, v in sorted(by_section.items())}, "cases": rows}
    write_json(args.output_dir / "check.json", report)
    summary = {"total": len(rows), "counts": report["counts"], "oracle_disagreements": report["oracle_disagreements"], "report": str(args.output_dir / "check.json")}
    if args.verbose:
        summary["sections"] = report["sections"]
    print(json.dumps(summary, indent=2))
    if args.record_known_failures:
        if any(row["kind"] in FATAL for row in rows):
            raise ValueError("crashes, hangs, errors, and protocol errors cannot be baselined")
        reasons = {}
        if args.record_known_failures.exists():
            reasons = {key: value["reason"] for key, value in json.loads(args.record_known_failures.read_text())["failures"].items()}
        write_json(args.record_known_failures, {"metadata": report["metadata"], "failures": {row["id"]: {"signature": signature(row), "kind": row["kind"], "reason": reasons.get(row["id"], "TODO: explain")} for row in failures}})
        return 0
    if args.baseline:
        known = json.loads(args.baseline.read_text())["failures"]
        new = [row["id"] for row in failures if row["kind"] in FATAL or known.get(row["id"], {}).get("signature") != signature(row)]
        selected = {case["id"] for case in cases}
        stale = sorted((set(known) & selected) - {row["id"] for row in failures})
        if new or stale:
            print(json.dumps({"new_or_changed_failures": new, "stale_known_failures": stale}, indent=2))
            return 1
        return 0
    return int(bool(failures))


TASK_MARKERS = {"[ ]": "unchecked", "[x]": "checked", "[X]": "checked"}


def gfm_tasks(blocks: list) -> list:
    """cmark-gfm 0.29.0.gfm.13 recognises a task list marker only when the
    item's list marker is the first thing on the line (its scanner starts at
    column 0), so `- - [x] a` and `> - [x] a` get no task there. GFM's rule is
    that the marker begins the item's first paragraph, which the module and
    the generators follow. This rewrites the oracle's tree to that rule: an
    item without a task whose first paragraph starts with a marker and a space
    or tab becomes a task item without the marker."""
    out = []
    for block in blocks:
        if block[0] == "list":
            items = []
            for task, children in block[3]:
                children = gfm_tasks(children)
                if task == "none" and children and children[0][0] == "paragraph" and children[0][1] and children[0][1][0][0] == "text":
                    text = children[0][1][0][1]
                    if text[:3] in TASK_MARKERS and text[3:4] in (" ", "\t"):
                        task = TASK_MARKERS[text[:3]]
                        rest = normalize_inlines([["text", text[3:].lstrip(" \t")]] + children[0][1][1:])
                        children = ([["paragraph", rest]] if rest else []) + children[1:]
                items.append([task, children])
            block = ["list", block[1], block[2], items]
        elif block[0] == "blockquote":
            block = ["blockquote", gfm_tasks(block[1])]
        out.append(block)
    return out


def erase_tightness(blocks: list) -> list:
    """cmark-gfm 0.29.0.gfm.13 does not count a blank line after a thematic
    break inside a list item: `- ***\n\n- b` comes out tight, although the
    items are separated by a blank line (CommonMark 5.3; commonmark.js and
    markdown-it agree it is loose). This projection drops tightness so a
    crosscheck can attribute such disagreements to that quirk."""
    out = []
    for block in blocks:
        if block[0] == "list":
            block = ["list", block[1], None, [[task, erase_tightness(children)] for task, children in block[3]]]
        elif block[0] == "blockquote":
            block = ["blockquote", erase_tightness(block[1])]
        out.append(block)
    return out


def has_break_in_list(blocks: list, in_list: bool = False) -> bool:
    for block in blocks:
        if block[0] == "hr" and in_list:
            return True
        if block[0] == "list" and any(has_break_in_list(children, True) for _, children in block[3]):
            return True
        if block[0] == "blockquote" and has_break_in_list(block[1], in_list):
            return True
    return False


def oracle_quirk(expected: list, actual: list) -> str | None:
    """Name the documented cmark-gfm quirk that explains a disagreement."""
    tasks = gfm_tasks(actual)
    if classify(expected, tasks) == "pass":
        return "oracle_task_quirk"
    if has_break_in_list(actual) and classify(erase_tightness(expected), erase_tightness(tasks)) == "pass":
        return "oracle_break_tightness_quirk"
    return None


def first_difference(left, right, path="$"):
    if type(left) is not type(right) or not isinstance(left, list) or len(left) != len(right):
        return path, left, right
    for index, (a, b) in enumerate(zip(left, right)):
        if a != b:
            return first_difference(a, b, path + "[" + str(index) + "]")
    return None


WILDCARD = ["paragraph", [["text", "?"]]]


def apply_wildcards(expected, actual):
    """A generator may write a paragraph of just "?" where any paragraph is
    acceptable; copy that wildcard over the matching oracle paragraph."""
    if expected == WILDCARD and isinstance(actual, list) and actual[:1] == ["paragraph"]:
        return WILDCARD
    if isinstance(expected, list) and isinstance(actual, list) and len(expected) == len(actual):
        return [apply_wildcards(e, a) for e, a in zip(expected, actual)]
    return actual


def crosscheck(args) -> int:
    """Each ``show`` output is one JSON object {"markdown", "expected"} where
    expected is the generator's own block tree in the probe's JSON shape."""
    counts: Counter = Counter()
    examples = []
    for path in sorted(Path(args.corpus).iterdir()):
        if not path.is_file():
            continue
        shown = subprocess.run([str(args.binary), "show", str(path)], capture_output=True, text=True)
        try:
            case = json.loads(shown.stdout.strip().splitlines()[-1])
        except (ValueError, IndexError):
            counts["unparseable_show"] += 1
            continue
        if case.get("rejected"):
            counts["rejected"] += 1
            continue
        expected = normalize(case["expected"])
        result = oracle(case["markdown"])
        if result["status"] == "ok":
            result["value"] = apply_wildcards(expected, result["value"])
        if result["status"] != "ok":
            kind = "oracle_failure"
        else:
            kind = classify(expected, result["value"])
            if kind != "pass":
                kind = oracle_quirk(expected, result["value"]) or kind
        counts[kind] += 1
        if not kind.startswith(("pass", "oracle_")) and len(examples) < args.examples:
            where = first_difference(expected, result.get("value"))
            examples.append({"file": path.name, "kind": kind, "markdown": case["markdown"], "difference": {"path": where[0], "generator": where[1], "oracle": where[2]} if where else None})
    print(json.dumps({"counts": dict(counts), "examples": examples}, ensure_ascii=False, indent=1))
    return int(any(not kind.startswith(("pass", "rejected", "oracle_")) for kind in counts))


def differential(args, executable: Path) -> int:
    """Probe and oracle on every valid UTF-8 file of a raw corpus (for example
    the markdown-document target's), reporting block-level disagreements
    first. Inline-only differences are counted, not listed, unless asked."""
    counts: Counter = Counter()
    examples: dict[str, list] = {}
    for path in sorted(Path(args.corpus).iterdir()):
        if not path.is_file():
            continue
        try:
            text = path.read_bytes().decode("utf-8")
        except UnicodeDecodeError:
            counts["invalid_utf8"] += 1
            continue
        actual = probe(executable, text)
        expected = oracle(text)
        if expected["status"] != "ok":
            kind = "oracle_failure"
        elif actual["status"] in FATAL:
            kind = actual["status"]
        else:
            kind = classify(actual["value"], expected["value"])
            if kind != "pass":
                kind = oracle_quirk(actual["value"], expected["value"]) or kind
        counts[kind] += 1
        if kind in args.show and len(examples.setdefault(kind, [])) < args.examples:
            where = first_difference(actual.get("value"), expected.get("value"))
            examples[kind].append({"file": path.name, "input": text, "difference": {"path": where[0], "roc": where[1], "oracle": where[2]} if where else actual})
    print(json.dumps({"counts": dict(counts), "examples": examples}, ensure_ascii=False, indent=1))
    return int(any(kind in FATAL or kind == "wrong_block" for kind in counts))


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("check", "replay", "crosscheck", "differential"):
        operation = sub.add_parser(name)
        operation.add_argument("--roc", default=os.environ.get("ROC", "roc"))
        operation.add_argument("--output-dir", type=Path, default=DEFAULT_WORK / name)
        operation.add_argument("--no-build", action="store_true", help="reuse <output-dir>/bin/probe")
        if name == "check":
            operation.add_argument("--case", action="append")
            operation.add_argument("--section", action="append")
            operation.add_argument("--verbose", action="store_true")
            baseline = operation.add_mutually_exclusive_group()
            baseline.add_argument("--baseline", type=Path)
            baseline.add_argument("--record-known-failures", type=Path)
        if name == "replay":
            operation.add_argument("input", type=Path)
        if name == "crosscheck":
            operation.add_argument("binary", type=Path)
            operation.add_argument("corpus", type=Path)
            operation.add_argument("--examples", type=int, default=10)
        if name == "differential":
            operation.add_argument("corpus", type=Path)
            operation.add_argument("--examples", type=int, default=10)
            operation.add_argument("--show", action="append", default=["wrong_block", "crash", "timeout", "error", "protocol_error"])
    args = parser.parse_args(argv)
    args.output_dir = args.output_dir.expanduser().resolve()
    args.roc = str(Path(args.roc).expanduser().resolve()) if "/" in args.roc else args.roc
    try:
        if importlib.metadata.version("cmarkgfm") != ORACLE_VERSION:
            raise ValueError("install the pinned oracle from scripts/markdown/blocks/requirements.txt")
        if args.command == "crosscheck":
            return crosscheck(args)
        executable = args.output_dir / "bin" / "probe" if args.no_build else build(args.roc, args.output_dir)
        if args.command == "check":
            return check(args, executable)
        if args.command == "differential":
            return differential(args, executable)
        text = args.input.read_bytes().decode("utf-8")
        row = compare({"id": "replay", "input": text}, probe(executable, text), oracle(text))
        print(json.dumps(row, ensure_ascii=False, indent=2))
        return int(row["kind"] != "pass")
    except (ValueError, RuntimeError, OSError, importlib.metadata.PackageNotFoundError) as error:
        parser.exit(2, "error: " + str(error) + "\n")


if __name__ == "__main__":
    raise SystemExit(main())
