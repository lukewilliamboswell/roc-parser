import importlib.util
import json
from pathlib import Path
import unittest


SPEC = importlib.util.spec_from_file_location("review_xml", Path(__file__).resolve().parents[1] / "review_xml.py")
review = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(review)


class DeclarationVerdictTests(unittest.TestCase):
    def test_invalid_version_uses_the_specification_across_expat_versions(self):
        cases = json.loads((review.DATA / "cases.json").read_text())
        case = next(case for case in cases if case["id"] == "decl/version-2")
        self.assertFalse(case["valid"])
        self.assertEqual(case["specification"], "https://www.w3.org/TR/xml/#NT-VersionNum")
        for status in ("ok", "error"):
            with self.subTest(expat_status=status):
                row = review.compare(case, {"status": "error"}, {"status": status})
                self.assertEqual(row["kind"], "pass")
                self.assertEqual(bool(row["oracle_disagreement"]), status == "ok")
                accepted = review.compare(case, {"status": "ok"}, {"status": status})
                self.assertEqual(accepted["kind"], "invalid_acceptance")
