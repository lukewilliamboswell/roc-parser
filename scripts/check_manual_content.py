#!/usr/bin/env python3
"""Audit publication links, public anchors and source listing fidelity independently."""
from __future__ import annotations
import argparse
from html.parser import HTMLParser
import json
from pathlib import Path
import re
import sys
import textwrap
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[1]


class Page(HTMLParser):
    def __init__(self, source: str):
        super().__init__(convert_charrefs=True)
        self.ids: set[str] = set()
        self.links: list[str] = []
        self.code: list[str] = []
        self.in_pre = False
        self.feed(source)

    def handle_starttag(self, tag, attrs):
        for key, value in attrs:
            if key == "id":
                self.ids.add(value)
            if key in ("href", "src"):
                self.links.append(value)
        if tag == "pre":
            self.in_pre = True
            self.code.append("")

    def handle_endtag(self, tag):
        if tag == "pre":
            self.in_pre = False

    def handle_data(self, data):
        if self.in_pre:
            self.code[-1] += data


def audit(site: Path, root: Path = ROOT) -> list[str]:
    site = site.resolve()
    pages = {path.resolve(): Page(path.read_text()) for path in site.rglob("*.html")}
    problems = []
    for path, page in pages.items():
        for href in page.links:
            link = urlsplit(href)
            if link.scheme or link.netloc:
                continue
            target = (path.parent / unquote(link.path)).resolve() if link.path else path
            if target.is_dir():
                target /= "index.html"
            if not target.is_relative_to(site) or not target.is_file():
                problems.append(f"{path.relative_to(site)}: unresolved local link {href}")
            elif link.fragment and target in pages and unquote(link.fragment) not in pages[target].ids:
                problems.append(f"{path.relative_to(site)}: missing anchor {href}")
    index = pages.get(site / "index.html")
    if index is None:
        return [*problems, "manual has no index.html"]
    published = json.loads((root / "scripts/docs/published-anchors.json").read_text())
    problems += [f"published manual anchor is missing: {anchor}" for anchor in published if anchor not in index.ids]
    blocks = {source.strip("\n") for source in index.code}
    for chapter in (root / "docs").glob("*.adoc"):
        for path, tag in re.findall(r'include::(examples/[^\[\n]+)\[tag=([^,\]]+)[^\]]*\]', chapter.read_text()):
            selected = []
            active = False
            for line in (root / "docs" / path).read_text().splitlines(keepends=True):
                if "tag::" + tag + "[]" in line:
                    active = True
                elif "end::" + tag + "[]" in line:
                    active = False
                elif active:
                    selected.append(line)
            expected = textwrap.dedent("".join(selected)).strip("\n")
            if not expected or expected not in blocks:
                problems.append(f"{chapter.name}: source listing differs from {path}[tag={tag}]")
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("site", type=Path)
    args = parser.parse_args()
    problems = audit(args.site)
    for problem in problems:
        print(problem, file=sys.stderr)
    if not problems:
        print("Manual links, published anchors and source listings verified.")
    return int(bool(problems))


if __name__ == "__main__":
    raise SystemExit(main())
