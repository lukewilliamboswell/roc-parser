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
        so.cmark_node_free(ctypes.c_void_p(address))
    finally:
        cmark.parser_free(parser)
    return out


def cmark_html(text: str) -> str:
    import cmarkgfm
    from cmarkgfm.cmark import Options

    return cmarkgfm.markdown_to_html_with_extensions(text, options=Options.CMARK_OPT_UNSAFE, extensions=EXTENSIONS)


def markdown_it_html(text: str) -> str:
    from markdown_it import MarkdownIt

    return MarkdownIt("commonmark").enable("strikethrough").render(text)


def markdown_it_blocks(text: str) -> list:
    """markdown-it-py's token stream as the same block/inline tree shape."""
    from markdown_it import MarkdownIt

    tokens = MarkdownIt("commonmark").enable("strikethrough").parse(text)
    blocks = []
    for index, token in enumerate(tokens):
        if token.type == "inline" and index > 0 and tokens[index - 1].type == "paragraph_open":
            blocks.append(["p", normalize(mdit_inlines(token.children or []))])
        elif token.type.endswith("_open") and token.type != "paragraph_open" and token.level == 0:
            blocks.append(["other", token.type])
        elif token.type in ("fence", "code_block", "html_block", "hr") and token.level == 0:
            blocks.append(["other", token.type])
    return blocks


def mdit_inlines(tokens: list) -> list:
    root: list = []
    stack = [root]
    for token in tokens:
        out = stack[-1]
        kind = token.type
        if kind in ("text", "text_special"):
            # text_special: an entity or backslash escape, already decoded.
            out.append(["text", token.content])
        elif kind == "softbreak":
            out.append(["text", "\n"])
        elif kind == "hardbreak":
            out.append(["br"])
        elif kind == "code_inline":
            out.append(["code", token.content])
        elif kind == "html_inline":
            out.append(["html", token.content])
        elif kind in ("em_open", "strong_open", "s_open"):
            node = [{"em_open": "emph", "strong_open": "strong", "s_open": "del"}[kind], []]
            out.append(node)
            stack.append(node[1])
        elif kind == "link_open":
            node = ["link", token.attrGet("href") or "", token.attrGet("title") or "", []]
            out.append(node)
            stack.append(node[3])
        elif kind in ("em_close", "strong_close", "s_close", "link_close"):
            stack.pop()
        elif kind == "image":
            out.append(["image", token.attrGet("src") or "", token.attrGet("title") or "", mdit_inlines(token.children or [])])
        else:
            out.append(["unknown", kind])
    return root


def comparable_hrefs(blocks: list) -> list:
    """Percent-decode destinations: markdown-it stores normalized URLs."""
    from urllib.parse import unquote

    def walk(nodes: list) -> list:
        out = []
        for node in nodes:
            if node[0] in ("link", "image"):
                out.append([node[0], unquote(node[1]), node[2], walk(node[3])])
            elif node[0] in ("emph", "strong", "del"):
                out.append([node[0], walk(node[1])])
            else:
                out.append(node)
        return out

    return [[b[0], walk(b[1])] if b[0] == "p" else b for b in blocks]


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


# cmark's XML renderer emits C0 controls verbatim, which XML 1.0 rejects:
# carry them through the XML parser as private-use code points.
_CONTROLS = [c for c in range(32) if c not in (9, 10, 13)]
_TO_PRIVATE = {c: 0xF0000 + c for c in _CONTROLS}
_FROM_PRIVATE = {0xF0000 + c: c for c in _CONTROLS}


def _restore(value):
    if isinstance(value, str):
        return value.translate(_FROM_PRIVATE)
    if isinstance(value, list):
        return [_restore(item) for item in value]
    return value


def cmark_blocks(text: str) -> list:
    doc = ET.fromstring(cmark_xml(text).translate(_TO_PRIVATE))
    blocks = []
    for child in doc:
        tag = child.tag.replace(NS, "")
        if tag == "paragraph":
            blocks.append(["p", _restore(normalize(xml_inlines(child)))])
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


# GFM syntax that markdown-it (CommonMark preset plus `~~` strikethrough) does
# not implement: single-tilde strikethrough and extended autolinks.
GFM_TRIGGERS = re.compile(r"www\.|[A-Za-z]:(?=//)|[A-Za-z0-9._+-]@[A-Za-z0-9]")

# cmark-gfm registers `~` as an "emphasis" special character, so when it
# computes the flanking of `*`/`_` runs it looks through adjacent tildes
# (scan_delims SKIP_CHARS). No spec describes this; the library treats `~` as
# ordinary punctuation, as CommonMark does.
TILDE_ADJACENT = re.compile(r"[*_]~|~[*_]")

# CommonMark examples whose plain-CommonMark output differs once the GFM
# extended-autolink extension is enabled (verified against cmark-gfm).
GFM_SUPERSEDED = {602, 606, 608, 611, 612}


def single_tilde_pairs(text: str) -> bool:
    """Could cmark-gfm form a one-tilde strikethrough (two unescaped `~` runs)?"""
    runs = [m.group(0) for m in re.finditer(r"(?<!\\)~+", text)]
    return runs.count("~") >= 2


def classify(text: str, roc_blocks: list, oracle_blocks: list) -> str:
    if roc_blocks == oracle_blocks:
        return "pass"
    if [b[0] for b in roc_blocks] != [b[0] for b in oracle_blocks]:
        # Paragraph splitting or other block structure differs: block level.
        return "block"
    if single_tilde_pairs(text) or GFM_TRIGGERS.search(text.replace("~", "")):
        if TILDE_ADJACENT.search(text):
            return "oracle-quirk"
        return "fail"
    try:
        adjudicated = markdown_it_blocks(text)
    except Exception:  # pragma: no cover - adjudicator failure
        return "fail"
    if comparable_hrefs(roc_blocks) == comparable_hrefs(adjudicated):
        return "oracle-quirk"
    return "fail"


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------


def neutralize_symbols(text: str) -> str:
    """Replace non-ASCII symbols (S*) by U+00A1, which is punctuation under
    both CommonMark 0.29 (cmark-gfm: P* only) and 0.31.2 (P* and S*)."""
    import unicodedata

    return "".join("¡" if ord(ch) > 127 and unicodedata.category(ch).startswith("S") else ch for ch in text)


def reclassify_symbol_failures(probe: Path, items: list[dict]) -> None:
    """items: dicts with markdown/mode/verdict; failures that only differ
    because cmark-gfm 0.29 does not count symbols as punctuation become
    oracle quirks (the library and cmark-gfm agree once symbols are replaced
    by a character both treat as punctuation)."""
    pending = [item for item in items if item["verdict"] == "fail" and neutralize_symbols(item["markdown"]) != item["markdown"]]
    if not pending:
        return
    texts = [neutralize_symbols(item["markdown"]) for item in pending]
    results = run_probe(probe, [(item["mode"], text) for item, text in zip(pending, texts)])
    for item, text, result in zip(pending, texts, results):
        roc = probe_blocks(result, item["mode"])
        if classify(text, roc, cmark_blocks(text)) in ("pass", "oracle-quirk"):
            item["verdict"] = "oracle-quirk"
            item["quirk"] = "symbol-punctuation"


def load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def check(args) -> int:
    work = Path(args.work)
    probe = Path(args.probe) if args.probe else build_probe(work)
    known = load_json(DATA / "known-failures.json")

    entries = []
    for example in load_json(DATA / "spec-inline.json"):
        expected = example["html"]
        if example["example"] in GFM_SUPERSEDED:
            # The GFM extended-autolink extension (claimed by the library)
            # links these; cmark-gfm's output is the reference.
            expected = cmark_html(example["markdown"])
        entries.append({"id": f"spec-{example['example']}", "markdown": example["markdown"], "html": expected, "section": example["section"]})
    for example in load_json(DATA / "gfm-extensions.json"):
        entries.append({"id": f"gfm-{example['example']}", "markdown": example["markdown"], "html": example["html"], "section": example["section"]})
    for case in load_json(DATA / "cases.json"):
        # "html": expected output written from the spec text, for cases where
        # both oracles predate CommonMark 0.31.2 or are wrong.
        entries.append({"id": f"case-{case['id']}", "markdown": case["markdown"], "html": case.get("html"), "section": case.get("note", ""), "oracle": case.get("oracle", "cmark-gfm")})

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
            # cases.json may name markdown-it where cmark-gfm has a known bug.
            want_blocks = comparable_hrefs(markdown_it_blocks(entry["markdown"])) if entry.get("oracle") == "markdown-it" else cmark_blocks(entry["markdown"])
            roc = comparable_hrefs(roc) if entry.get("oracle") == "markdown-it" else roc
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
    counts = differential(probe, [(name, mode, text) for name, text, mode in selected], work / "corpus-report.json", args.show)
    print(f"corpus: {len(texts)} inputs, {len(selected)} comparable; {counts}")
    return 1 if counts["fail"] else 0


def differential(probe: Path, samples: list[tuple[str, str, str]], report_path: Path, show: int) -> dict:
    """Compare the library with cmark-gfm on (name, mode, markdown) samples,
    write the non-passing ones to report_path and return verdict counts."""
    results = run_probe(probe, [(mode, text) for _, mode, text in samples])
    items = []
    for (name, mode, text), result in zip(samples, results):
        roc = probe_blocks(result, mode)
        oracle = cmark_blocks(text)
        items.append({"file": name, "mode": mode, "markdown": text, "roc": roc, "cmark": oracle, "verdict": classify(text, roc, oracle)})
    reclassify_symbol_failures(probe, items)
    counts = {"pass": 0, "fail": 0, "oracle-quirk": 0, "block": 0}
    shown = 0
    for item in items:
        counts[item["verdict"]] += 1
        if item["verdict"] == "fail" and shown < show:
            shown += 1
            print(f"FAIL {item['file']} ({item['mode']})\n  markdown: {item['markdown']!r}\n  roc:   {json.dumps(item['roc'], ensure_ascii=False)[:1500]}\n  cmark: {json.dumps(item['cmark'], ensure_ascii=False)[:1500]}")
    write_json(report_path, [item for item in items if item["verdict"] != "pass"])
    return counts


def decode_roc_string(literal: str) -> str:
    """Decode a Roc string literal as printed by Str.inspect."""
    assert literal.startswith('"') and literal.endswith('"'), literal[:40]
    body = literal[1:-1]
    out = []
    index = 0
    simple = {"n": "\n", "r": "\r", "t": "\t", '"': '"', "\\": "\\", "$": "$"}
    while index < len(body):
        ch = body[index]
        if ch == "\\":
            nxt = body[index + 1]
            if nxt in simple:
                out.append(simple[nxt])
                index += 2
            elif nxt == "u":
                close = body.index(")", index)
                out.append(chr(int(body[index + 3:close], 16)))
                index = close + 1
            else:
                raise ValueError(f"unknown escape \\{nxt}")
        else:
            out.append(ch)
            index += 1
    return "".join(out)


def generated(args) -> int:
    """Differential check of a typed target's generated Markdown: run the
    target's `show` on every corpus entry, then compare the library with
    cmark-gfm on the shown Markdown (the target itself asserts that the
    library matches the generator's expected tree)."""
    work = Path(args.work)
    probe = Path(args.probe) if args.probe else build_probe(work)
    samples = []
    for path in sorted(Path(args.corpus).iterdir()):
        shown = subprocess.run([args.binary, "show", str(path)], capture_output=True, timeout=60)
        text = shown.stdout.decode("utf-8", "replace") + shown.stderr.decode("utf-8", "replace")
        match = re.search(r'^(inline|doc): (".*")$', text, re.M)
        if not match:
            continue
        samples.append((path.name, match.group(1), decode_roc_string(match.group(2))))
    counts = differential(probe, samples, work / "generated-report.json", args.show)
    print(f"generated: {len(samples)} samples; {counts}")
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
    generated_parser = sub.add_parser("generated")
    generated_parser.add_argument("binary", help="built typed fuzz target (e.g. markdown-inline-ast)")
    generated_parser.add_argument("corpus")
    generated_parser.add_argument("--show", type=int, default=10)
    args = parser.parse_args()
    if args.command == "check":
        return check(args)
    if args.command == "generated":
        return generated(args)
    return corpus(args)


if __name__ == "__main__":
    sys.exit(main())
