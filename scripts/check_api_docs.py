#!/usr/bin/env python3
"""Independently audit already generated API HTML; the Roc driver builds it."""
from __future__ import annotations
import argparse
import html
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
PACKAGE_ENTRY = ROOT / "package/main.roc"
PRIVATE_ENTRIES: dict[str, tuple[str, ...]] = {}
class DocsError(RuntimeError):
    pass

def exposed_modules(entry: Path) -> list[str]:
    """Read the exposed module list from a package or platform header."""
    text = entry.read_text(encoding="utf-8")
    match = re.search(r"(?:exposes\s*|^package\s*)\[([^\]]*)\]", text, re.M)
    if match is None:
        raise DocsError(f"could not find an exposes list in {entry.relative_to(ROOT)}")
    return [name.strip() for name in match.group(1).split(",") if name.strip()]


TAG_PATTERN = re.compile(r"<[^>]+>")
CODE_PATTERN = re.compile(r"<code[^>]*>(.*?)</code>", re.S)
ENTRY_START_PATTERN = re.compile(r'<article class="entry[^"]*" id="([^"]+)">')
ENTRY_DOC_PATTERN = re.compile(r'<div class="entry-doc">(.*?)</div>', re.S)
MODULE_DOC_PATTERN = re.compile(r'<div class="module-doc">(.*?)</div>', re.S)
PARAGRAPH_PATTERN = re.compile(r"<p>(.*?)</p>", re.S)
UNFENCED_CODE_PATTERN = re.compile(r"^[a-z_][A-Za-z0-9_]*[!?]?\s*(=[^=]|\()")


def doc_text(fragment: str) -> str:
    """Rendered doc HTML back to plain text, with `<code>` as backticks."""
    text = CODE_PATTERN.sub(lambda match: f"`{match.group(1)}`", fragment)
    text = TAG_PATTERN.sub("", text)
    return " ".join(html.unescape(text).split())


def entries(source: str) -> list[tuple[str, str]]:
    """Every entry on a page, as (id, the HTML that belongs to it).

    A receiver is rendered as an `<article>` nested inside its nominal's, so
    each entry's own HTML runs from its opening tag to the next entry's, which
    is where its doc lives either way.
    """
    starts = list(ENTRY_START_PATTERN.finditer(source))
    return [
        (
            match.group(1),
            source[match.end() : starts[index + 1].start() if index + 1 < len(starts) else len(source)],
        )
        for index, match in enumerate(starts)
    ]


def module_pages(version_root: Path) -> list[Path]:
    return sorted(version_root.glob("*/index.html"))


def check_modules(version_root: Path, entry: Path) -> list[str]:
    problems: list[str] = []
    if not (version_root / "index.html").is_file():
        problems.append("missing index.html")
    for module in exposed_modules(entry):
        page = version_root / module / "index.html"
        if not page.is_file():
            problems.append(f"{module}: no page for exposed module")
            continue
        docs = MODULE_DOC_PATTERN.findall(page.read_text(encoding="utf-8"))
        if not any(doc_text(body) for body in docs):
            problems.append(f"{module}: module has no doc comment")
    return problems


def check_entries_documented(version_root: Path) -> list[str]:
    """Every exposed type and value carries a doc comment."""
    problems: list[str] = []
    for page in module_pages(version_root):
        for entry_id, body in entries(page.read_text(encoding="utf-8")):
            doc = ENTRY_DOC_PATTERN.search(body)
            if doc is None or not doc_text(doc.group(1)):
                problems.append(f"{page.parent.name}: {entry_id} has no doc comment")
    return problems


def check_cross_links(version_root: Path) -> list[str]:
    """Every relative API documentation link resolves."""
    problems: list[str] = []
    for page in module_pages(version_root):
        for raw in re.findall(r'<a href="([^"]+)"', page.read_text(encoding="utf-8")):
            href = html.unescape(raw).split("#", 1)[0].split("?", 1)[0]
            if not href or href.startswith(("http:", "https:", "mailto:", "//", "/")):
                continue
            target = (page.parent / href).resolve()
            if not target.is_file() and not (target / "index.html").is_file():
                problems.append(f"{page.parent.name}: broken link {raw}")
    return problems


def check_prose_renders(version_root: Path) -> list[str]:
    """Catch doc prose that silently renders as something else.

    A snippet written without a ``` fence renders as a paragraph of run
    together code, and a paragraph that starts with `*`, `-` or `#` shows that
    character literally.
    """
    problems: list[str] = []
    for page in module_pages(version_root):
        source = page.read_text(encoding="utf-8")
        where = page.parent.name
        blocks = [
            (entry_id, body)
            for entry_id, entry in entries(source)
            for body in ENTRY_DOC_PATTERN.findall(entry)
        ]
        blocks += [("module header", body) for body in MODULE_DOC_PATTERN.findall(source)]
        for entry_id, body in blocks:
            for paragraph in PARAGRAPH_PATTERN.findall(body):
                text = doc_text(paragraph)
                if not text:
                    continue
                if UNFENCED_CODE_PATTERN.match(text):
                    problems.append(
                        f"{where}: {entry_id} has a paragraph that reads as unfenced code: {text[:60]!r}"
                    )
                if text[0] in "*-#":
                    problems.append(
                        f"{where}: {entry_id} has a paragraph starting with {text[0]!r}, "
                        f"which renders literally: {text[:60]!r}"
                    )
    return problems


def check_private_entries_hidden(version_root: Path) -> list[str]:
    pages = [version_root / "index.html", *module_pages(version_root)]
    text = "".join(page.read_text(encoding="utf-8") for page in pages)
    return [
        f"private entry leaked into docs: {module}.{name}"
        for module, names in PRIVATE_ENTRIES.items()
        for name in names
        if f"{module}.{name}" in text
    ]


def validate(version_root: Path) -> list[str]:
    problems = check_modules(version_root, PACKAGE_ENTRY)
    problems += check_entries_documented(version_root)
    problems += check_cross_links(version_root)
    problems += check_prose_renders(version_root)
    problems += check_private_entries_hidden(version_root)
    return problems


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    args = parser.parse_args(argv)
    try:
        problems = validate(args.directory)
    except (DocsError, OSError) as error:
        print(f"API audit: {error}", file=sys.stderr)
        return 1
    for problem in problems:
        print(f"API audit: {problem}", file=sys.stderr)
    return int(bool(problems))

if __name__ == "__main__":
    raise SystemExit(main())
