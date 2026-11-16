"""Regression checks for the independent QIF normalization/review protocol."""
import importlib.util
from pathlib import Path
import random
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("review_qif", Path(__file__).resolve().parents[1] / "review_qif.py")
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)


class QifReviewTests(unittest.TestCase):
    def test_exact_decimal_normalization_preserves_identifiers(self):
        rows = [{"amount": "-1234567890123456789012345678901234567890.340000", "number": "0007", "splits": [{"amount": "+0.000100"}]}]
        normalized = review.normalize(rows)
        self.assertEqual(normalized[0]["amount"], "-1234567890123456789012345678901234567890.34")
        self.assertEqual(normalized[0]["number"], "0007")
        self.assertEqual(normalized[0]["splits"][0]["amount"], "0.0001")

    def test_policy_checks_actual_identifier(self):
        case = {"input": "", "policy": "numeric reference", "expected": "ok", "expected_number": "0007"}
        with patch.object(review, "probe", return_value={"status": "ok", "rows": [{"number": "7"}]}):
            self.assertIsNotNone(review.compare(None, case))

    def test_cross_writer_difference_is_a_failure(self):
        actual = {"status": "ok", "rows": [], "written": "ours"}
        expected = {"status": "ok", "rows": [], "written": "theirs"}
        with patch.object(review, "probe", side_effect=[actual, {"status": "error"}]), patch.object(review, "oracle", side_effect=[expected, actual]):
            self.assertIsNotNone(review.compare(None, {"input": "source"}))

    def test_generator_reproducible_and_terminated(self):
        first = review.generated(random.Random(19), 3)
        self.assertEqual(first, review.generated(random.Random(19), 3))
        self.assertEqual(first.splitlines().count("^"), 3)
        self.assertIn("D27/08/2024", first)


if __name__ == "__main__":
    unittest.main()
