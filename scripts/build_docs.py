#!/usr/bin/env python3
"""Build and validate roc-parser's API reference with `roc docs`.

The reference is written under .docs-api/<version>/. `--check` builds into a
temporary directory without touching it, and fails when the build breaks or
the API audit finds a problem: an exposed module without a page or a module
doc comment, an exposed entry without a doc comment, a broken relative link,
or doc prose that renders as something other than what was meant.

    scripts/build_docs.py --check
    scripts/build_docs.py --version 2.0.0          # into .docs-api/2.0.0
    scripts/build_docs.py --roc ~/roc/roc --check  # another compiler
"""

from __future__ import annotations

import argparse
import html
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PACKAGE_ENTRY = ROOT / "package" / "main.roc"

# Entries that `roc docs` lists but that are not part of the documented API,
# by module: `{"Module": ("name", "Nominal.name")}`. Each is removed from its
# page and from every sidebar, and the audit fails if one leaks back.
PRIVATE_ENTRIES: dict[str, tuple[str, ...]] = {}

# The name `roc docs` shows in the sidebar and the index title is derived from
# the entry file, which would otherwise name the site "package".
PACKAGE_DISPLAY_NAME = "roc-parser"


class DocsError(RuntimeError):
    """A docs generation or validation failure with a user-facing message."""


def run_roc_docs(roc: str, entry: Path, output: Path) -> None:
    if output.exists():
        shutil.rmtree(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    try:
        result = subprocess.run(
            [roc, "docs", str(entry), f"--output={output}"],
            cwd=ROOT,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        raise DocsError(f"Roc compiler not found: {roc}") from None
    if result.returncode != 0:
        raise DocsError(
            f"roc docs failed for {entry.relative_to(ROOT)}:\n{result.stdout}{result.stderr}"
        )


def module_header_doc(entry: Path) -> str:
    """The `##` comment block at the top of an entry module, as plain text.

    `roc docs` renders a module header only for modules that have a page, and
    the entry module of a package has none, so its header would otherwise be
    written for nobody. Reading it here keeps the front page's prose in the
    source file it describes.
    """
    lines: list[str] = []
    for line in entry.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("##"):
            lines.append(stripped[2:].removeprefix(" "))
            continue
        if stripped:
            break
    return "\n".join(lines).strip()


def render_doc_markdown(text: str) -> str:
    """Render the doc-comment subset: paragraphs, backtick spans, ``` fences."""
    out: list[str] = []
    paragraph: list[str] = []
    fence: list[str] | None = None

    def flush_paragraph() -> None:
        if not paragraph:
            return
        body = inline_markdown(" ".join(paragraph))
        out.append(f"                <p>{body}</p>")
        paragraph.clear()

    for line in text.splitlines():
        if line.strip().startswith("```"):
            if fence is None:
                flush_paragraph()
                fence = []
            else:
                code = html.escape("\n".join(fence))
                out.append(f"                <pre><code>{code}</code></pre>")
                fence = None
            continue
        if fence is not None:
            fence.append(line)
        elif line.strip():
            paragraph.append(line.strip())
        else:
            flush_paragraph()
    flush_paragraph()
    return "\n".join(out)


def inline_markdown(text: str) -> str:
    """Escape a paragraph and turn its backtick spans into `<code>`."""
    parts = text.split("`")
    rendered = []
    for index, part in enumerate(parts):
        escaped = html.escape(part)
        rendered.append(f"<code>{escaped}</code>" if index % 2 else escaped)
    return "".join(rendered)


SITE_NAME_PATTERN = re.compile(r'(<h1 class="pkg-full-name"><a href="[^"]*">)([^<]*)(</a></h1>)')
INDEX_MARKER = '        <div class="index-decoration">'


def write_index_body(root: Path, entry: Path) -> bool:
    """Give the site's front page the entry module's header as its body.

    Returns False, leaving the page alone, when the entry has no header.
    """
    header = module_header_doc(entry)
    if not header:
        return False
    page = root / "index.html"
    source = page.read_text(encoding="utf-8")
    if INDEX_MARKER not in source:
        raise DocsError(f"{page}: no index body to fill in")
    block = f'        <div class="module-doc">\n{render_doc_markdown(header)}\n        </div>\n'
    page.write_text(source.replace(INDEX_MARKER, block + INDEX_MARKER, 1), encoding="utf-8")
    return True


def name_site(root: Path, display_name: str) -> None:
    """Replace the filename-derived site name in the title and every sidebar."""
    index = root / "index.html"
    source = index.read_text(encoding="utf-8")
    match = SITE_NAME_PATTERN.search(source)
    if match is None:
        raise DocsError(f"{index}: no site name to replace")
    current = match.group(2)
    escaped = html.escape(display_name)
    for page in root.rglob("index.html"):
        text = page.read_text(encoding="utf-8")
        replaced = SITE_NAME_PATTERN.sub(
            lambda m: m.group(1) + escaped + m.group(3) if m.group(2) == current else m.group(0),
            text,
        )
        replaced = replaced.replace(f"<title>{current} Docs</title>", f"<title>{escaped} Docs</title>", 1)
        if replaced != text:
            page.write_text(replaced, encoding="utf-8")


def strip_module_type_defs(root: Path) -> None:
    """Drop the definition line every module nominal renders under its name.

    Modules are declared as `X :: {}.{ ... }` (or `X := [].{ ... }`) so their
    entries can be receivers. That definition is a compiler-shaped detail;
    printed under the module name as `X :: # (opaque)` or `:= []`, it reads as
    though the module were a type with no values.
    """
    for page in root.glob("*/index.html"):
        module = re.escape(page.parent.name)
        source = page.read_text(encoding="utf-8")
        replaced = re.sub(
            r'(<h1 class="module-name">[^<]*</h1>\s*(?:<!--[^>]*-->\s*)?)'
            r'<pre class="entry-type-def"><code class="entry-type-def-code">'
            rf"(?::= \[\]|{module} :: # \(opaque\))</code></pre>\s*",
            r"\1",
            source,
            count=1,
        )
        if replaced != source:
            page.write_text(replaced, encoding="utf-8")


def hide_private_entries(root: Path) -> None:
    """Remove PRIVATE_ENTRIES from their pages and from every sidebar."""
    pages = [root / "index.html", *root.glob("*/index.html")]
    for module, names in PRIVATE_ENTRIES.items():
        defining_page = root / module / "index.html"
        source = defining_page.read_text(encoding="utf-8")
        for name in names:
            entry_id = re.escape(f"{module}.{name}")
            source = re.sub(
                rf'\s*<article class="entry[^"]*" id="{entry_id}">.*?</article>', "", source, flags=re.S
            )
        defining_page.write_text(source, encoding="utf-8")
        for page in pages:
            source = page.read_text(encoding="utf-8")
            for name in names:
                entry_id = re.escape(f"{module}.{name}")
                source = re.sub(
                    rf'\s*<li[^>]*>\s*<a[^>]*href="(?:\.\./)?{module}/\#{entry_id}".*?</li>',
                    "",
                    source,
                    flags=re.S,
                )
            page.write_text(source, encoding="utf-8")


def exposed_modules(entry: Path) -> list[str]:
    """Read the exposed module list from a package or platform header."""
    text = entry.read_text(encoding="utf-8")
    match = re.search(r"(?:exposes\s*|^package\s*)\[([^\]]*)\]", text, re.M)
    if match is None:
        raise DocsError(f"could not find an exposes list in {entry.relative_to(ROOT)}")
    return [name.strip() for name in match.group(1).split(",") if name.strip()]


def build(roc: str, version_root: Path) -> list[str]:
    """Build the reference; return problems found while post-processing."""
    problems: list[str] = []
    run_roc_docs(roc, PACKAGE_ENTRY, version_root)
    strip_module_type_defs(version_root)
    if not write_index_body(version_root, PACKAGE_ENTRY):
        problems.append(
            f"{PACKAGE_ENTRY.relative_to(ROOT)}: no `##` package header, so the API index page has no introduction"
        )
    name_site(version_root, PACKAGE_DISPLAY_NAME)
    hide_private_entries(version_root)
    return problems


# ------------------------------------------------------------------ the audit

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


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--roc", default="roc", help="Roc compiler binary")
    parser.add_argument("--docs-root", default=".docs-api", help="versioned docs root (default: .docs-api)")
    parser.add_argument("--version", help="release version, e.g. 2.0.0")
    parser.add_argument(
        "--check",
        action="store_true",
        help="build into a temporary directory and validate, leaving --docs-root alone",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        if args.check:
            with tempfile.TemporaryDirectory(prefix="roc_parser_docs_") as temporary:
                version_root = Path(temporary) / "preview"
                problems = build(args.roc, version_root)
                problems += validate(version_root)
                where = "preview build"
        else:
            if not args.version:
                raise DocsError("--version is required unless --check is given")
            version_root = (ROOT / args.docs_root / args.version).resolve()
            problems = build(args.roc, version_root)
            problems += validate(version_root)
            try:
                where = str(version_root.relative_to(ROOT))
            except ValueError:
                where = str(version_root)

        if problems:
            print(f"Docs validation failed for {where}:", file=sys.stderr)
            for problem in problems:
                print(f"  - {problem}", file=sys.stderr)
            return 1

        print(f"Docs built and validated: {where}")
        return 0
    except DocsError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
