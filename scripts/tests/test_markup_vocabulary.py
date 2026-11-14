"""Keep enumerable candidate spellings and hot membership matches in agreement."""
from __future__ import annotations

import json
import re
import tempfile
import unittest
from pathlib import Path

from scripts.markup import generate_vocabulary as vocabulary


class MarkupVocabularyTests(unittest.TestCase):
    def test_generated_source_is_fresh(self) -> None:
        self.assertEqual(vocabulary.OUTPUT.read_text(), vocabulary.render(vocabulary.load()))

    def test_each_enumeration_and_match_has_exactly_the_canonical_spellings(self) -> None:
        text = vocabulary.OUTPUT.read_text()
        for table, names in vocabulary.load().items():
            enumerated = re.search(rf"\t{table}_names = (\[[^\n]+\])", text)
            self.assertIsNotNone(enumerated, table)
            self.assertEqual(json.loads(enumerated[1]), names)
            match = re.search(rf"\t{table}_name = \|name\| match name \{{\n(.*?)=> True", text, re.S)
            self.assertIsNotNone(match, table)
            self.assertEqual(re.findall(r'"([^"]+)"', match[1]), names)
        self.assertIn("linearGradient", vocabulary.load()["svg"])
        self.assertNotIn("lineargradient", vocabulary.load()["svg"])
        self.assertIn("aria-label", vocabulary.load()["aria"])

    def test_bad_tables_are_refused(self) -> None:
        for invalid in ([], ["div", "div"], ["span", "div"], ["é"], [""]):
            with self.subTest(invalid=invalid), tempfile.TemporaryDirectory() as directory:
                document = {"schema_version": 1, "tables": vocabulary.load()}
                document["tables"]["html"] = invalid
                path = Path(directory) / "vocabulary.json"
                path.write_text(json.dumps(document))
                with self.assertRaises(ValueError):
                    vocabulary.load(path)

    def test_authoring_membership_uses_the_shared_generated_tables(self) -> None:
        expected = {
            "package/HtmlValidate.roc": ["elements = MarkupVocabulary.html_element_name"],
            "package/MarkupValidate.roc": [
                "svg_elements = MarkupVocabulary.svg_element_name",
                "math_elements = MarkupVocabulary.mathml_element_name",
            ],
            "package/MarkupVocabulary.roc": [
                "html_element_name = MarkupNameTables.html_name",
                "svg_element_name = MarkupNameTables.svg_name",
                "mathml_element_name = MarkupNameTables.mathml_name",
                "aria_name = MarkupNameTables.aria_name",
            ],
        }
        for path, declarations in expected.items():
            text = (vocabulary.ROOT / path).read_text()
            for declaration in declarations:
                self.assertIn(declaration, text, path)


if __name__ == "__main__":
    unittest.main()
