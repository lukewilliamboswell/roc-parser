#!/usr/bin/env python3
"""Markdown inline conformance review against CommonMark 0.31.2 and cmark-gfm.

Install scripts/markdown/requirements.txt into an external virtual environment
first (never system-wide), then run e.g.

    python scripts/review_markdown.py check                # spec + GFM + cases
    python scripts/review_markdown.py corpus DIR [DIR...]  # differential on fuzz corpora

Ground truth:
  * spec examples (scripts/markdown/spec-inline.json, CommonMark 0.31.2, CC-BY-SA
    4.0) are compared as HTML against the spec's expected output;
  * GFM extension examples (scripts/markdown/gfm-extensions.json) against theirs;
  * hand-written cases (scripts/markdown/cases.json) against cmark-gfm's AST.
The differential `corpus` mode compares the library's AST with cmark-gfm's AST
(rendered via cmark_render_xml) and asks markdown-it-py (CommonMark 0.31.2) to
adjudicate disagreements on inputs without GFM extension syntax.

Known failures are listed with reasons in scripts/markdown/known-failures.json;
the check fails on new failures and on stale entries.
"""

from __future__ import annotations

import argparse
import ctypes
import html
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import xml.etree.ElementTree as ET
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "markdown"
DEFAULT_WORK = ROOT / ".roc-parser-tmp" / "markdown-review"
ROC = os.environ.get("ROC", "roc")
EXTENSIONS = ["strikethrough", "autolink"]

# ---------------------------------------------------------------------------
# Oracles
# ---------------------------------------------------------------------------


def load_cmark():
    import cmarkgfm  # noqa: F401  (registers extensions)
    from cmarkgfm import cmark
    from cmarkgfm import _cmark

    so = ctypes.CDLL(_cmark.__file__)
    so.cmark_render_xml.restype = ctypes.c_void_p
    so.cmark_render_xml.argtypes = [ctypes.c_void_p, ctypes.c_int]
    libc = ctypes.CDLL(None)
    libc.free.argtypes = [ctypes.c_void_p]
    return cmark, _cmark, so, libc


_CMARK = None


def cmark_xml(text: str) -> str:
    global _CMARK
    if _CMARK is None:
        _CMARK = load_cmark()
    cmark, _cmark, so, libc = _CMARK
    opts = cmark.Options.CMARK_OPT_UNSAFE
    cmark.core_extensions_ensure_registered()
    parser = cmark.parser_new(opts)
    try:
        for name in EXTENSIONS:
            cmark.parser_attach_syntax_extension(parser, cmark.find_syntax_extension(name))
        cmark.parser_feed(parser, text)
        root = cmark.parser_finish(parser)
        address = int(_cmark.ffi.cast("uintptr_t", root))
        raw = so.cmark_render_xml(address, opts)
        try:
            out = ctypes.string_at(raw).decode("utf-8")
        finally:
            libc.free(raw)
        _cmark.lib.cmark_node_free(root)
    finally:
        cmark.parser_free(parser)
    return out


def cmark_html(text: str) -> str:
    import cmarkgfm
    from cmarkgfm.cmark import Options

    return cmarkgfm.markdown_to_html_with_extensions(text, options=Options.CMARK_OPT_UNSAFE, extensions=EXTENSIONS)


def markdown_it_html(text: str) -> str:
    from markdown_it import MarkdownIt

    return MarkdownIt("commonmark").render(text)


NS = "{http://commonmark.org/xml/1.0}"


def xml_inlines(element) -> list:
    out = []
    for child in element:
        tag = child.tag.replace(NS, "")
        if tag == "text":
            out.append(["text", child.text or ""])
        elif tag == "softbreak":
            out.append(["text", "\n"])
        elif tag == "linebreak":
            out.append(["br"])
        elif tag == "code":
            out.append(["code", child.text or ""])
        elif tag == "html_inline":
            out.append(["html", child.text or ""])
        elif tag == "emph":
            out.append(["emph", xml_inlines(child)])
        elif tag == "strong":
            out.append(["strong", xml_inlines(child)])
        elif tag == "strikethrough":
            out.append(["del", xml_inlines(child)])
        elif tag in ("link", "image"):
            title = child.attrib.get("title", "")
            out.append([tag, child.attrib.get("destination", ""), title, xml_inlines(child)])
        else:
            out.append(["unknown", tag])
    return out


def cmark_blocks(text: str) -> list:
    doc = ET.fromstring(cmark_xml(text))
    blocks = []
    for child in doc:
        tag = child.tag.replace(NS, "")
        if tag == "paragraph":
            blocks.append(["p", normalize(xml_inlines(child))])
        else:
            blocks.append(["other", tag])
    return blocks


# ---------------------------------------------------------------------------
# AST normalization and HTML rendering (cmark's HTML conventions)
# ---------------------------------------------------------------------------


def normalize(nodes: list) -> list:
    out: list = []
    for node in nodes:
        kind = node[0]
        if kind == "text":
            if node[1] == "":
                continue
            if out and out[-1][0] == "text":
                out[-1] = ["text", out[-1][1] + node[1]]
            else:
                out.append(["text", node[1]])
        elif kind in ("emph", "strong", "del"):
            out.append([kind, normalize(node[1])])
        elif kind in ("link", "image"):
            out.append([kind, node[1], node[2] or "", normalize(node[3])])
        else:
            out.append(list(node))
    return out


def _href_safe() -> set[int]:
    safe = set()
    # houdini_href_e.c HREF_SAFE: ASCII alphanumerics plus these.
    for c in b"!#$%()*+,-./:;=?@_~":
        safe.add(c)
    for c in range(256):
        if chr(c).isalnum() and c < 128:
            safe.add(c)
    return safe


HREF_SAFE = _href_safe()


def escape_href(url: str) -> str:
    out = []
    for b in url.encode("utf-8"):
        if b in HREF_SAFE:
            out.append(chr(b))
        elif b == ord("&"):
            out.append("&amp;")
        elif b == ord("'"):
            out.append("&#x27;")
        else:
            out.append("%%%02X" % b)
    return "".join(out)


def escape_html(text: str) -> str:
    return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


def plain(nodes: list) -> str:
    out = []
    for node in nodes:
        kind = node[0]
        if kind == "text":
            out.append(escape_html(node[1].replace("\n", " ")))
        elif kind in ("code", "html"):
            out.append(escape_html(node[1]))
        elif kind == "br":
            out.append(" ")
        elif kind in ("emph", "strong", "del"):
            out.append(plain(node[1]))
        elif kind in ("link", "image"):
            out.append(plain(node[3]))
    return "".join(out)


def render(nodes: list, parent: str = "") -> str:
    out = []
    for node in nodes:
        kind = node[0]
        if kind == "text":
            out.append(escape_html(node[1]))
        elif kind == "br":
            out.append("<br />\n")
        elif kind == "code":
            out.append("<code>" + escape_html(node[1]) + "</code>")
        elif kind == "html":
            out.append(node[1])
        elif kind == "emph":
            out.append("<em>" + render(node[1], kind) + "</em>")
        elif kind == "strong":
            out.append("<strong>" + render(node[1], kind) + "</strong>")
        elif kind == "del":
            out.append("<del>" + render(node[1], kind) + "</del>")
        elif kind == "link":
            title = f' title="{escape_html(node[2])}"' if node[2] else ""
            out.append(f'<a href="{escape_href(node[1])}"{title}>' + render(node[3], kind) + "</a>")
        elif kind == "image":
            title = f' title="{escape_html(node[2])}"' if node[2] else ""
            out.append(f'<img src="{escape_href(node[1])}" alt="{plain(node[3])}"{title} />')
        else:
            out.append(f"<!-- {kind} -->")
    return "".join(out)


def render_blocks(blocks: list) -> str:
    out = []
    for block in blocks:
        if block[0] == "p":
            out.append("<p>" + render(block[1]) + "</p>\n")
        elif block[0] == "h":
            out.append(f"<h{block[1]}>" + render(block[2]) + f"</h{block[1]}>\n")
        else:
            out.append(f"<!-- block {block[1]} -->\n")
    return "".join(out)


# ---------------------------------------------------------------------------
# Probe
# ---------------------------------------------------------------------------


def build_probe(work: Path) -> Path:
    out = work / "bin" / "markdown-probe"
    out.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run([ROC, "build", str(DATA / "probe.roc"), f"--output={out}"], check=True, cwd=ROOT)
    return out


def run_probe(probe: Path, cases: list[tuple[str, str]], timeout: float = 600) -> list[dict]:
    frames = bytearray()
    for mode, text in cases:
        body = text.encode("utf-8")
        frames += f"{mode} {len(body)}\n".encode() + body
    result = subprocess.run([str(probe)], input=bytes(frames), capture_output=True, timeout=timeout)
    lines = result.stdout.decode("utf-8", "replace").splitlines()
    out = []
    for index in range(len(cases)):
        if index < len(lines):
            try:
                out.append(json.loads(lines[index]))
                continue
            except json.JSONDecodeError:
                pass
        out.append({"status": "crash", "stderr": result.stderr.decode("utf-8", "replace")[-2000:]})
    return out


def probe_blocks(result: dict, mode: str) -> list:
    if result.get("status") != "ok":
        return [["error", result.get("status")]]
    if mode == "inline":
        return [["p", normalize(result["inlines"])]]
    blocks = []
    for block in result["blocks"]:
        if block[0] == "p":
            blocks.append(["p", normalize(block[1])])
        elif block[0] == "h":
            blocks.append(["h", block[1], normalize(block[2])])
        else:
            blocks.append(["other", block[1]])
    return blocks


# ---------------------------------------------------------------------------
# Case selection
# ---------------------------------------------------------------------------

REF_DEF_START = re.compile(r"^ {0,3}\[", re.M)


def choose_mode(text: str) -> str | None:
    """`inline` when cmark-gfm sees exactly one paragraph and no line could be a
    link reference definition; `doc` when the document is only paragraphs."""
    blocks = cmark_blocks(text)
    if not blocks or any(b[0] != "p" for b in blocks):
        return None
    if len(blocks) == 1 and not REF_DEF_START.search(text) and "\n\n" not in text:
        return "inline"
    return "doc"


GFM_TRIGGERS = re.compile(r"~|www\.|://|@")


def classify(text: str, roc_blocks: list, oracle_blocks: list) -> str:
    if roc_blocks == oracle_blocks:
        return "pass"
    if GFM_TRIGGERS.search(text):
        return "fail"
    try:
        adjudicated = markdown_it_html(text)
    except Exception:  # pragma: no cover - adjudicator failure
        return "fail"
    if render_blocks(roc_blocks) == adjudicated:
        return "oracle-quirk"
    return "fail"


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------


def load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def check(args) -> int:
    work = Path(args.work)
    probe = Path(args.probe) if args.probe else build_probe(work)
    known = load_json(DATA / "known-failures.json")

    entries = []
    for example in load_json(DATA / "spec-inline.json"):
        entries.append({"id": f"spec-{example['example']}", "markdown": example["markdown"], "html": example["html"], "section": example["section"]})
    for example in load_json(DATA / "gfm-extensions.json"):
        entries.append({"id": f"gfm-{example['example']}", "markdown": example["markdown"], "html": example["html"], "section": example["section"]})
    for case in load_json(DATA / "cases.json"):
        entries.append({"id": f"case-{case['id']}", "markdown": case["markdown"], "html": None, "section": case.get("note", "")})

    selected = []
    for entry in entries:
        if entry["html"] is None:
            mode = choose_mode(entry["markdown"])
        else:
            mode = paragraph_only(entry["html"], entry["markdown"])
        if mode is None:
            continue
        selected.append((entry, mode))

    results = run_probe(probe, [(mode, entry["markdown"]) for entry, mode in selected])
    failures = {}
    for (entry, mode), result in zip(selected, results):
        roc = probe_blocks(result, mode)
        if entry["html"] is not None:
            ok = render_blocks(roc) == entry["html"]
            got = render_blocks(roc)
            want = entry["html"]
        else:
            want_blocks = cmark_blocks(entry["markdown"])
            ok = roc == want_blocks
            got = json.dumps(roc, ensure_ascii=False)
            want = json.dumps(want_blocks, ensure_ascii=False)
        if not ok:
            failures[entry["id"]] = {"markdown": entry["markdown"], "want": want, "got": got, "section": entry["section"]}

    new = sorted(set(failures) - set(known), key=sort_key)
    stale = sorted(set(known) - set(failures), key=sort_key)
    print(f"cases: {len(selected)} selected of {len(entries)}; failures: {len(failures)}; known: {len(known)}")
    for case_id in new:
        f = failures[case_id]
        print(f"NEW FAILURE {case_id} [{f['section']}]\n  markdown: {f['markdown']!r}\n  want: {f['want']!r}\n  got:  {f['got']!r}")
    for case_id in stale:
        print(f"STALE known failure (now passes): {case_id}")
    if args.write_failures:
        write_json(work / "failures.json", failures)
    return 1 if new or stale else 0


def paragraph_only(expected_html: str, markdown: str) -> str | None:
    """Spec examples: choose the probe mode from the expected HTML."""
    parts = re.findall(r"<p>.*?</p>\n", expected_html, re.S)
    if "".join(parts) != expected_html or not parts:
        return None
    if len(parts) == 1 and not REF_DEF_START.search(markdown) and "\n\n" not in markdown.strip("\n"):
        return "inline"
    return "doc"


def sort_key(case_id: str):
    prefix, _, number = case_id.partition("-")
    return (prefix, int(number) if number.isdigit() else 0, number)


def corpus(args) -> int:
    work = Path(args.work)
    probe = Path(args.probe) if args.probe else build_probe(work)
    texts = []
    for directory in args.dirs:
        for path in sorted(Path(directory).iterdir()):
            if not path.is_file():
                continue
            raw = path.read_bytes()
            text = raw.decode("utf-8", "replace").replace("\x00", "�")
            if len(text) > args.max_len:
                continue
            texts.append((path.name, text))
    selected = []
    for name, text in texts:
        mode = choose_mode(text)
        if mode is not None:
            selected.append((name, text, mode))
    results = run_probe(probe, [(mode, text) for _, text, mode in selected])
    counts = {"pass": 0, "fail": 0, "oracle-quirk": 0}
    shown = 0
    report = []
    for (name, text, mode), result in zip(selected, results):
        roc = probe_blocks(result, mode)
        oracle = cmark_blocks(text)
        verdict = classify(text, roc, oracle)
        counts[verdict] += 1
        if verdict != "pass":
            report.append({"file": name, "verdict": verdict, "markdown": text, "roc": roc, "cmark": oracle})
            if verdict == "fail" and shown < args.show:
                shown += 1
                print(f"FAIL {name} ({mode})\n  markdown: {text!r}\n  roc:   {json.dumps(roc, ensure_ascii=False)}\n  cmark: {json.dumps(oracle, ensure_ascii=False)}")
    write_json(work / "corpus-report.json", report)
    print(f"corpus: {len(texts)} inputs, {len(selected)} comparable; {counts}")
    return 1 if counts["fail"] else 0


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--work", default=str(DEFAULT_WORK))
    parser.add_argument("--probe", help="prebuilt probe binary (default: build scripts/markdown/probe.roc)")
    sub = parser.add_subparsers(dest="command", required=True)
    check_parser = sub.add_parser("check")
    check_parser.add_argument("--write-failures", action="store_true")
    corpus_parser = sub.add_parser("corpus")
    corpus_parser.add_argument("dirs", nargs="+")
    corpus_parser.add_argument("--max-len", type=int, default=4096)
    corpus_parser.add_argument("--show", type=int, default=10)
    args = parser.parse_args()
    if args.command == "check":
        return check(args)
    return corpus(args)


if __name__ == "__main__":
    sys.exit(main())
