from __future__ import annotations
import json
from pathlib import Path
import tempfile
import unittest

from scripts.check_manual_content import audit


class ManualContentAuditTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "scripts/docs").mkdir(parents=True)
        (self.root / "scripts/docs/published-anchors.json").write_text(json.dumps(["published"]))
        (self.root / "docs/examples").mkdir(parents=True)
        (self.root / "docs/chapter.adoc").write_text("include::examples/example.roc[tag=sample,indent=0]\n")
        (self.root / "docs/examples/example.roc").write_text('# tag::sample[]\nvalue = "<&>"\n\tcall(value)\n\n# end::sample[]\n')
        self.site = self.root / "site"
        self.site.mkdir()
        self.page = self.site / "index.html"
        self.html = '<h1 id="published">Title</h1><a href="chapter.html#destination">Next</a><pre><code>value = "&lt;&amp;&gt;"\n\tcall(value)\n\n</code></pre>'
        self.page.write_text(self.html)
        (self.site / "chapter.html").write_text('<h2 id="destination">Next chapter</h2>')

    def test_valid_links_anchors_and_source_bytes_pass(self) -> None:
        self.assertEqual(audit(self.site, self.root), [])

    def test_missing_local_paths_and_fragments_are_rejected(self) -> None:
        self.page.write_text(self.html.replace("#destination", "#missing"))
        self.assertTrue(any("missing anchor" in problem for problem in audit(self.site, self.root)))
        self.page.write_text(self.html.replace("chapter.html", "absent.html"))
        self.assertTrue(any("unresolved local link" in problem for problem in audit(self.site, self.root)))

    def test_listing_whitespace_is_preserved(self) -> None:
        self.page.write_text(self.html.replace("\tcall", " call"))
        self.assertTrue(any("source listing differs" in problem for problem in audit(self.site, self.root)))

    def test_published_anchors_are_required(self) -> None:
        self.page.write_text(self.html.replace('id="published"', 'id="renamed"'))
        self.assertIn("published manual anchor is missing: published", audit(self.site, self.root))
