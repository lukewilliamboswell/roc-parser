from __future__ import annotations

import tempfile
import unittest
import unittest.mock
from pathlib import Path

from scripts import check_api_docs as build_docs

INDEX = """<html><head><title>package Docs</title></head><body>
<nav><h1 class="pkg-full-name"><a href="">package</a></h1></nav>
<main><div class="main-content">
        <div class="index-decoration">logo</div>
</div></main></body></html>
"""

MODULE = """<html><head><title>package Docs</title></head><body>
<nav><h1 class="pkg-full-name"><a href="../">package</a></h1>
<ul><li class="entry"><a href="../Csv/#Csv.hidden">hidden</a></li></ul></nav>
<main>
        <h1 class="module-name">Csv</h1>
<!--!\x1f-->
        <pre class="entry-type-def"><code class="entry-type-def-code">Csv :: # (opaque)</code></pre>
<!--!-->
        <div class="module-doc">
                <p>CSV parsing.</p>
        </div>
        <article class="entry entry-value" id="Csv.parse">
            <div class="entry-signature"><a href="#Csv.parse">parse</a></div>
            <div class="entry-doc">
                <p>Parse a <code>Str</code>.</p>
            </div>
        </article>
        <article class="entry entry-value" id="Csv.hidden">
            <div class="entry-signature">hidden</div>
        </article>
        <article class="entry entry-value" id="Csv.bare">
            <div class="entry-signature">bare</div>
        </article>
        <a href="../Missing/">broken</a>
</main></body></html>
"""


class PostProcessingTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name) / "site"
        (self.root / "Csv").mkdir(parents=True)
        (self.root / "index.html").write_text(INDEX, encoding="utf-8")
        (self.root / "Csv" / "index.html").write_text(MODULE, encoding="utf-8")
        self.entry = Path(self.temporary.name) / "main.roc"
        self.entry.write_text(
            "## Parse text in Roc.\n##\n## ```roc\n## x = 1\n## ```\npackage [Csv] {}\n", encoding="utf-8"
        )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def page(self, *parts: str) -> str:
        return (self.root.joinpath(*parts, "index.html")).read_text(encoding="utf-8")

    def test_exposed_modules(self) -> None:
        self.assertEqual(build_docs.exposed_modules(self.entry), ["Csv"])
        multi = Path(self.temporary.name) / "multi.roc"
        multi.write_text("package\n\t[\n\t\tParser,\n\t\tString,\n\t]\n\t{}\n", encoding="utf-8")
        self.assertEqual(build_docs.exposed_modules(multi), ["Parser", "String"])


class AuditTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        (self.root / "Csv").mkdir()
        (self.root / "index.html").write_text(INDEX, encoding="utf-8")
        (self.root / "Csv" / "index.html").write_text(MODULE, encoding="utf-8")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def test_undocumented_entries_are_reported(self) -> None:
        self.assertEqual(
            build_docs.check_entries_documented(self.root),
            ["Csv: Csv.hidden has no doc comment", "Csv: Csv.bare has no doc comment"],
        )

    def test_missing_module_doc_and_page_are_reported(self) -> None:
        entry = self.root / "main.roc"
        entry.write_text("package [Csv, Yaml] {}\n", encoding="utf-8")
        self.assertEqual(build_docs.check_modules(self.root, entry), ["Yaml: no page for exposed module"])
        page = self.root / "Csv" / "index.html"
        page.write_text(page.read_text(encoding="utf-8").replace("<p>CSV parsing.</p>", ""), encoding="utf-8")
        self.assertEqual(
            build_docs.check_modules(self.root, entry),
            ["Csv: module has no doc comment", "Yaml: no page for exposed module"],
        )

    def test_broken_relative_links_are_reported(self) -> None:
        self.assertEqual(build_docs.check_cross_links(self.root), ["Csv: broken link ../Missing/"])
        page = self.root / "Csv/index.html"
        page.write_text(page.read_text() + '<a href="#missing">broken fragment</a>')
        self.assertIn("Csv: broken fragment #missing", build_docs.check_cross_links(self.root))

    def test_prose_that_renders_wrongly_is_reported(self) -> None:
        page = self.root / "Csv" / "index.html"
        page.write_text(
            page.read_text(encoding="utf-8").replace("<p>CSV parsing.</p>", "<p>- a list item</p><p>x = parse(y)</p>"),
            encoding="utf-8",
        )
        problems = build_docs.check_prose_renders(self.root)
        self.assertEqual(len(problems), 2)
        self.assertIn("renders literally", problems[0])
        self.assertIn("unfenced code", problems[1])

    def test_doc_text_keeps_code_as_backticks(self) -> None:
        self.assertEqual(build_docs.doc_text("<p>Use <code>a &amp; b</code>\n now.</p>"), "Use `a & b` now.")

