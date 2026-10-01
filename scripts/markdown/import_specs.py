#!/usr/bin/env python3
"""Regenerate the vendored inline spec examples used by scripts/review_markdown.py.

    python3 scripts/markdown/import_specs.py SPEC_JSON GFM_SPEC_TXT

SPEC_JSON is https://spec.commonmark.org/0.31.2/spec.json and GFM_SPEC_TXT is
cmark-gfm's test/spec.txt (GitHub Flavored Markdown 0.29-gfm). Both are
licensed CC-BY-SA 4.0 (see scripts/markdown/SPEC-LICENSE).
"""

from __future__ import annotations

import json
from pathlib import Path
import re
import sys

DATA = Path(__file__).resolve().parent

INLINE_SECTIONS = {
    "Backslash escapes",
    "Entity and numeric character references",
    "Code spans",
    "Emphasis and strong emphasis",
    "Links",
    "Images",
    "Autolinks",
    "Raw HTML",
    "Hard line breaks",
    "Soft line breaks",
    "Textual content",
    "Inlines",
}

GFM_SECTIONS = {"Strikethrough (extension)", "Autolinks (extension)"}


def gfm_examples(text: str) -> list[dict]:
    examples = []
    section = ""
    number = 0
    lines = text.split("\n")
    index = 0
    fence = "`" * 32
    while index < len(lines):
        line = lines[index]
        heading = re.match(r"^#{1,6} (.*)$", line)
        if heading:
            section = heading.group(1).strip()
        if line.startswith(fence + " example"):
            number += 1
            body = []
            index += 1
            while lines[index] != fence:
                body.append(lines[index])
                index += 1
            joined = "\n".join(body) + "\n"
            markdown, _, expected = joined.partition("\n.\n")
            if section in GFM_SECTIONS:
                examples.append({
                    "example": number,
                    "section": section,
                    "markdown": (markdown + "\n").replace("→", "\t"),
                    "html": expected.replace("→", "\t"),
                })
        index += 1
    return examples


def main() -> None:
    spec = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
    inline = [
        {key: example[key] for key in ("example", "section", "markdown", "html")}
        for example in spec
        if example["section"] in INLINE_SECTIONS
    ]
    (DATA / "spec-inline.json").write_text(json.dumps(inline, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    gfm = gfm_examples(Path(sys.argv[2]).read_text(encoding="utf-8"))
    (DATA / "gfm-extensions.json").write_text(json.dumps(gfm, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"{len(inline)} CommonMark inline examples, {len(gfm)} GFM extension examples")


if __name__ == "__main__":
    main()
