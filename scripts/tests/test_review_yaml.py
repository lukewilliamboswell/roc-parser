from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("review_yaml", Path(__file__).resolve().parents[1] / "review_yaml.py")
review = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(review)


class CoreSchemaTests(unittest.TestCase):
    def test_core_does_not_use_yaml_11_or_loader_extensions(self):
        for text in ("yes", "on", "12:34", "2026-10-01", "tRuE", "nUlL", "1_000", "1e", "-0x10", "<<"):
            self.assertEqual(review.core_scalar(text), ["string", text])

    def test_core_numbers_booleans_and_quotes(self):
        for text, expected in [("012", ["int", "12"]), ("0o12", ["int", "10"]), ("0x12", ["int", "18"]), ("TRUE", ["bool", True]), ("Null", ["null"]), (".NaN", ["float", "nan"]), ("-.Inf", ["float", "-inf"])]:
            self.assertEqual(review.core_scalar(text), expected)
        self.assertEqual(review.core_scalar("true", style='"'), ["string", "true"])
        self.assertEqual(review.core_scalar("123", review.TAG_PREFIX + "str"), ["string", "123"])
        self.assertEqual(review.core_scalar("x", "!application"), ["tagged", "!application", ["string", "x"]])

    def test_mapping_order_is_ignored_but_types_and_sequence_order_survive(self):
        a = ["mapping", [[["string", "a"], ["int", "1"]], [["string", "b"], ["bool", True]]]]
        self.assertEqual(review.canonical(a), review.canonical(["mapping", list(reversed(a[1]))]))
        self.assertNotEqual(review.canonical(["int", "1"]), review.canonical(["bool", True]))
        self.assertNotEqual(review.canonical(["sequence", [["int", "1"], ["int", "2"]]]), review.canonical(["sequence", [["int", "2"], ["int", "1"]]]))
        self.assertNotEqual(review.canonical(["string", "a\n"]), review.canonical(["string", "a"]))

    def test_float_comparison_handles_nonfinite_values_and_signed_zero(self):
        self.assertEqual(review.canonical(["float", "1.5"]), review.canonical(["float", "0x1.8000000000000p+0"]))
        self.assertEqual(review.canonical(["float", "NaN"]), ["float", "nan"])
        self.assertNotEqual(review.canonical(["float", "-0.0"]), review.canonical(["float", "0.0"]))


class SuiteGoldenTests(unittest.TestCase):
    def case(self, events):
        return {"id": "golden", "input": "", "valid": True, "events": events}

    def test_events_keep_escaped_backslashes_and_controls(self):
        self.assertEqual(review.unescape_event(r"a\n\t\b\\n"), "a\n\t\b\\n")

    def test_suite_is_authoritative_when_oracle_rejects_valid_syntax(self):
        case = self.case('+STR\n+DOC\n=VAL |a\\n\n-DOC\n-STR\n')
        row = review.compare(case, {"status": "ok", "value": ["string", "a\n"]}, {"status": "error", "message": "oracle limitation"})
        self.assertEqual(row["kind"], "pass")
        self.assertIsNotNone(row["oracle_disagreement"])

    def test_alias_cycles_and_multi_document_events(self):
        case = self.case('+STR\n+DOC\n+SEQ &a\n=ALI *a\n-SEQ\n-DOC\n+DOC\n=VAL :true\n-DOC\n-STR\n')
        self.assertEqual(review.suite_expectation(case)["value"], ["stream", [["sequence", [["alias", "cycle"]]], ["bool", True]]])

    def test_duplicate_typed_keys_are_construction_errors(self):
        case = self.case('+MAP\n=VAL :1\n=VAL :a\n=VAL :01\n=VAL :b\n-MAP\n')
        self.assertEqual(review.suite_expectation(case)["status"], "error")
        self.assertEqual(review.suite_expectation(case)["phase"], "resolution")

    def test_every_valid_suite_fixture_has_an_interpretable_golden(self):
        cases = json.loads((review.DATA / "suite.json").read_text())["cases"]
        self.assertEqual(len(cases), 402)
        for case in cases:
            with self.subTest(case=case["id"]):
                self.assertIn(review.suite_expectation(case)["status"], ("ok", "error"))


class ProbeTests(unittest.TestCase):
    def completed(self, value, code=0):
        return subprocess.CompletedProcess([], code, json.dumps(value).encode(), b"fault")

    def test_timeout_and_crash_cannot_be_ordinary_parse_errors(self):
        with patch.object(review.subprocess, "run", side_effect=subprocess.TimeoutExpired([], 2)):
            self.assertEqual(review.probe(Path("probe"), "x")["status"], "timeout")
        with patch.object(review.subprocess, "run", return_value=self.completed({}, -11)):
            self.assertEqual(review.probe(Path("probe"), "x")["status"], "crash")

    def test_malformed_values_and_locations_fail_the_protocol(self):
        for output, expected in [({"status": "ok", "value": ["wat"]}, "protocol_error"), ({"status": "ok", "value": ["bool", 1]}, "protocol_error"), ({"status": "error", "line": 0, "column": 1, "message": "x"}, "invalid_diagnostic")]:
            with patch.object(review.subprocess, "run", return_value=self.completed(output)):
                self.assertEqual(review.probe(Path("probe"), "x")["status"], expected)

    def test_wrong_answer_and_invalid_acceptance_are_reported(self):
        row = review.compare({"id": "x", "input": "true"}, {"status": "ok", "value": ["string", "true"]}, {"status": "ok", "value": ["bool", True]})
        self.assertEqual(row["kind"], "wrong_value")
        row = review.compare({"id": "x", "input": "["}, {"status": "ok", "value": ["string", "["]}, {"status": "error"})
        self.assertEqual(row["kind"], "invalid_acceptance")


class ReviewWorkflowTests(unittest.TestCase):
    def test_reduction_preserves_failure_predicate_and_budget(self):
        result, checks = review.minimize_text("abcXYZdef", lambda text: "XYZ" in text, 100)
        self.assertEqual(result, "XYZ")
        self.assertLessEqual(checks, 100)

    def test_generators_are_reproducible_and_bounded(self):
        a, b = review.generated_inputs(80), review.generated_inputs(80)
        for _ in range(100):
            text = next(a)
            self.assertEqual(text, next(b))
            self.assertLessEqual(len(text.encode()), 4096)

    def test_known_failure_changes_and_fixes_require_review(self):
        case = {"id": "x", "input": "true"}
        expected = {"status": "ok", "value": ["bool", True]}
        actual = {"status": "ok", "value": ["string", "true"]}
        row = review.compare(case, actual, expected)
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            baseline = output / "baseline.json"
            review.write_json(baseline, {"failures": {"x": {"signature": review.signature(row)}}})
            args = SimpleNamespace(case=None, roc="roc", parser_root=review.ROOT, output_dir=output, record_known_failures=None, baseline=baseline)
            with patch.object(review, "load_cases", return_value=[case]), patch.object(review, "oracle", return_value=expected), patch.object(review, "metadata", return_value={}), patch.object(review, "probe", return_value=actual):
                self.assertEqual(review.check(args, Path("probe")), 0)
            with patch.object(review, "load_cases", return_value=[case]), patch.object(review, "oracle", return_value=expected), patch.object(review, "metadata", return_value={}), patch.object(review, "probe", return_value=expected):
                self.assertEqual(review.check(args, Path("probe")), 1)


if __name__ == "__main__":
    unittest.main()
