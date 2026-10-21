from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import random
import re
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("review_json", Path(__file__).resolve().parents[1] / "review_json.py")
review = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(review)
sys.setrecursionlimit(max(sys.getrecursionlimit(), 20000))


def parse(text: str | bytes, **options):
    data = text.encode("utf-8") if isinstance(text, str) else text
    return review.oracle(data, options)


def value(text: str | bytes, **options):
    result = parse(text, **options)
    if result["status"] != "ok":
        raise AssertionError(f"{text!r} was rejected: {result['message']}")
    return result["value"]


def rejected(text: str | bytes, **options) -> bool:
    return parse(text, **options)["status"] == "error"


def number(lexeme: str) -> list:
    return ["number", lexeme]


def plain_tree(data):
    """Convert plain Python data (no floats) to the probe's tree form."""
    if isinstance(data, bool) or data is None or isinstance(data, str):
        return data
    if isinstance(data, int):
        return number(str(data))
    if isinstance(data, list):
        return ["array", [plain_tree(item) for item in data]]
    return ["object", [[key, plain_tree(item)] for key, item in data.items()]]


class OptionTests(unittest.TestCase):
    def test_defaults_are_strict_and_typos_are_errors(self):
        options = review.normalize_options({})
        self.assertEqual(options["max_depth"], 128)
        self.assertIsNone(options["max_items"])
        self.assertEqual(options["duplicates"], "Reject")
        for bad in ({"comment": True}, {"duplicates": "keep-last"}, {"max_depth": -1}, {"comments": 1}, {"max_items": "2"}, {"indent": None}):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                review.normalize_options(bad)

    def test_flags_round_trip_and_leave_defaults_out(self):
        self.assertEqual(review.option_flags({}), [])
        self.assertEqual(review.option_flags({"max_depth": 128, "indent": 2}), [])
        options = {"max_depth": 3, "max_items": 0, "max_string_bytes": 4, "max_number_digits": 5, "comments": True, "trailing_commas": True, "bom": True, "non_finite": True,
                   "control_characters": True, "lone_surrogates": "Replace", "duplicates": "CollapseIdentical", "top_level": "ObjectOrArray", "interoperable_numbers": True,
                   "sort_keys": True, "indent": 0}
        flags = review.option_flags(options)
        self.assertIn("--duplicates", flags)
        self.assertEqual(flags[flags.index("--duplicates") + 1], "collapse-identical")
        self.assertEqual(review.options_from_flags(flags), review.normalize_options(options))
        self.assertEqual(review.options_from_flags(" ".join(flags)), review.normalize_options(options))
        for bad in (["--bogus"], ["--max-depth"], ["--max-depth", "x"], ["--duplicates", "KeepLast"]):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                review.options_from_flags(bad)

    def test_every_flag_and_enum_value_is_known_to_the_probe(self):
        probe = (review.DATA / "probe.roc").read_text(encoding="utf-8")
        for key in review.DEFAULT_OPTIONS:
            self.assertIn('"--' + key.replace("_", "-") + '"', probe)
        for names in review.ENUMS.values():
            for flag in names.values():
                self.assertIn('"' + flag + '"', probe)
        # The protocol is written by hand; the writer is called only for the three fields under test.
        self.assertEqual(len(re.findall(r"JsonValue\.to_str\(", probe)), 1)
        self.assertEqual(len(re.findall(r"JsonValue\.to_str_with\(", probe)), 2)


class NumberTests(unittest.TestCase):
    def test_numbers_are_tagged_lexemes_never_python_numbers(self):
        tree = value('[0,-0,1.0,1.10,1E+2,123456789012345678901234567890,1e999999999999]')
        self.assertEqual(tree, ["array", [number("0"), number("-0"), number("1.0"), number("1.10"), number("1E+2"), number("123456789012345678901234567890"), number("1e999999999999")]])

        def walk(node):
            self.assertNotIn(type(node), (int, float))
            if isinstance(node, list):
                for child in node:
                    walk(child)
        walk(value('{"a":[1,2.5,{"b":-3e-2}]}'))

    def test_strict_number_grammar(self):
        for text in ("[01]", "[-01]", "[+1]", "[.5]", "[1.]", "[1e]", "[1e+]", "[-]", "[0x10]", "[1_0]", "[\u0661]", "[NaN]", "[Infinity]", "[-Infinity]"):
            with self.subTest(text=text):
                self.assertTrue(rejected(text))

    def test_non_finite_constants_become_lexemes_only_when_enabled(self):
        self.assertEqual(value("[NaN,Infinity,-Infinity]", non_finite=True), ["array", [number("NaN"), number("Infinity"), number("-Infinity")]])
        for text in ("-NaN", "+Infinity", "nan", "Inf"):
            self.assertTrue(rejected(text, non_finite=True))
        self.assertTrue(rejected("NaN", non_finite=True, interoperable_numbers=True))

    def test_interoperable_number_boundaries(self):
        cases = {
            "9007199254740991": True, "-9007199254740991": True, "9007199254740992": False, "-9007199254740992": False, "1" * 100: False,
            "0.12345678901234567": True, "0.123456789012345678": False, "1.00000000000000000000": True, "0.000000000000000000000012345678901234567": True,
            "1.2345678901234567e10": True, "1.23456789012345678e10": False, "1e308": True, "1e309": False, "-1e309": False, "1e-400": False,
            "0e-400": True, "0.0": True, "-0": True, "5e-324": True, "3e-324": True, "2e-324": False, "1.7976931348623157e308": True,
            "1.7976931348623159e308": False, "100000000000000000000.0": True, "100000000000000000000": False,
        }
        for lexeme, expected in cases.items():
            with self.subTest(lexeme=lexeme):
                self.assertIs(review.interoperable(lexeme), expected)
                self.assertIs(not rejected("[" + lexeme + "]", interoperable_numbers=True), expected)
                self.assertFalse(rejected("[" + lexeme + "]"))

    def test_number_digit_limit_counts_lexeme_characters(self):
        self.assertFalse(rejected("[-12,1.5,1e5]", max_number_digits=3))
        for text in ("[1234]", "[-123]", "[1.25]", "[1e+5]"):
            self.assertTrue(rejected(text, max_number_digits=3))
        self.assertFalse(rejected('["12345"]', max_number_digits=3))


class StringTests(unittest.TestCase):
    def test_escapes_and_raw_text(self):
        self.assertEqual(value('"\\"\\\\\\/\\b\\f\\n\\r\\t\\u00e9\\uD83D\\uDE00\x7f"'), '"\\/\b\f\n\r\té😀\x7f')
        for text in ('"\\x"', '"\\U0041"', '"\\u12"', '"abc', "'a'"):
            self.assertTrue(rejected(text))

    def test_control_characters_follow_the_option(self):
        for raw in ("\x00", "\t", "\n", "\x1f"):
            with self.subTest(raw=raw):
                self.assertTrue(rejected('"' + raw + '"'))
                self.assertEqual(value('"' + raw + '"', control_characters=True), raw)
        self.assertTrue(rejected("[\x01]", control_characters=True))

    def test_lone_surrogates_are_rejected_or_replaced(self):
        for text in ('"\\ud800"', '"\\udc00"', '"\\ud800\\u0041"', '"\\udc00\\ud800"', '{"\\udfff":1}'):
            self.assertTrue(rejected(text))
        self.assertEqual(value('"\\ud800\\ud800\\udc00"', lone_surrogates="Replace"), "\ufffd\U00010000")
        self.assertEqual(value('"\\udc00\\ud800"', lone_surrogates="Replace"), "\ufffd\ufffd")
        self.assertEqual(value('"\\ud83d\\ude00"', lone_surrogates="Replace"), "😀")
        # Replaced keys are compared after replacement.
        self.assertTrue(rejected('{"\\ud800":1,"\\udc00":2}', lone_surrogates="Replace"))
        self.assertEqual(value('{"\\ud800":1,"\ufffd":2}', lone_surrogates="Replace", duplicates="KeepLast"), ["object", [["\ufffd", number("2")]]])

    def test_string_byte_limit_counts_decoded_utf8(self):
        self.assertFalse(rejected('["abc","é","\\n\\n\\n",{"abc":1}]', max_string_bytes=3))
        for text in ('"abcd"', '"éé"', '"\\u00e9\\u00e9"', '"😀"', '{"abcd":1}'):
            self.assertTrue(rejected(text, max_string_bytes=3))
        self.assertFalse(rejected('"\\ud800"', max_string_bytes=3, lone_surrogates="Replace"))
        self.assertTrue(rejected('"\\ud800"', max_string_bytes=2, lone_surrogates="Replace"))

    def test_invalid_utf8_is_rejected(self):
        for data in (b"[\xff]", b'["\xc0\xaf"]', b'["\xed\xa0\x80"]', b'["\xe2\x82'):
            self.assertTrue(rejected(data))
        self.assertTrue(rejected(b"// \xff\n[]", comments=True))


class DocumentTests(unittest.TestCase):
    def test_byte_order_mark(self):
        self.assertTrue(rejected("\ufeff{}"))
        self.assertEqual(value("\ufeff {}", bom=True), ["object", []])
        self.assertEqual(value("{}", bom=True), ["object", []])
        for text in ("\ufeff\ufeff{}", " \ufeff{}", "[\ufeff]"):
            self.assertTrue(rejected(text, bom=True))
        self.assertTrue(rejected("\ufeff[]", bom=True, top_level="Object"))

    def test_top_level_shapes(self):
        expectations = {"Any": ("1", '"s"', "null", "[]", "{}"), "ObjectOrArray": ("[]", "{}"), "Object": ("{}",)}
        for shape, accepted in expectations.items():
            for text in ("1", '"s"', "null", "[]", "{}"):
                with self.subTest(shape=shape, text=text):
                    self.assertEqual(not rejected(text, top_level=shape), text in accepted)

    def test_depth_limit_counts_containers(self):
        self.assertFalse(rejected("[" * 128 + "]" * 128))
        self.assertTrue(rejected("[" * 129 + "]" * 129))
        self.assertFalse(rejected('{"a":' * 128 + "1" + "}" * 128))
        self.assertTrue(rejected('{"a":' * 129 + "1" + "}" * 129))
        self.assertFalse(rejected("1", max_depth=0))
        self.assertTrue(rejected("[]", max_depth=0))
        self.assertFalse(rejected("[[1],[2]]", max_depth=2))
        self.assertFalse(rejected('["' + "[" * 300 + '"]', max_depth=1))
        self.assertTrue(rejected("[" * 100000))

    def test_nesting_depth_ignores_strings(self):
        self.assertEqual(review.nesting_depth('[{"a":"[[{{"}]'), 2)
        self.assertEqual(review.nesting_depth('"\\"[["'), 0)

    def test_items_limit_counts_members_before_merging(self):
        self.assertFalse(rejected("[[1,2],[3,4]]", max_items=2))
        self.assertTrue(rejected("[1,2,3]", max_items=2))
        self.assertTrue(rejected('{"a":1,"b":2,"c":3}', max_items=2))
        self.assertTrue(rejected('{"a":1,"a":2}', max_items=1, duplicates="KeepLast"))
        self.assertFalse(rejected("[]", max_items=0))
        self.assertFalse(rejected("[1,2,]", max_items=2, trailing_commas=True))

    def test_duplicate_modes(self):
        text = '{"a":1,"b":2,"\\u0061":3}'
        self.assertTrue(rejected(text))
        self.assertTrue(rejected(text, duplicates="CollapseIdentical"))
        self.assertEqual(value(text, duplicates="KeepFirst"), ["object", [["a", number("1")], ["b", number("2")]]])
        self.assertEqual(value(text, duplicates="KeepLast"), ["object", [["a", number("3")], ["b", number("2")]]])
        self.assertEqual(value('{"a":[1],"b":0,"a":[1]}', duplicates="CollapseIdentical"), ["object", [["a", ["array", [number("1")]]], ["b", number("0")]]])
        self.assertTrue(rejected('{"a":1.0,"a":1}', duplicates="CollapseIdentical"))
        self.assertTrue(rejected('{"a":{"x":1,"y":2},"a":{"y":2,"x":1}}', duplicates="CollapseIdentical"))
        self.assertTrue(rejected('{"a":1,"a":1}'))
        self.assertFalse(rejected('{"a":1,"A":2,"a\\u0000":3}'))

    def test_vendored_suite_verdicts_match_the_oracle(self):
        for case in review.suite_cases():
            if case["expect"] == "i":
                continue
            with self.subTest(case=case["id"]):
                self.assertEqual(review.oracle(case["data"], case["options"])["status"] == "ok", case["expect"] == "y")


class StripperTests(unittest.TestCase):
    def test_comment_markers_inside_strings_are_text(self):
        self.assertEqual(review.strip_comments('["// a", "/* b */", "\\"//"] // c'), '["// a", "/* b */", "\\"//"]  ')
        self.assertEqual(value('["/*", "*/"] /* x */', comments=True), ["array", ["/*", "*/"]])
        self.assertEqual(value('[1 /* " */, 2]', comments=True), ["array", [number("1"), number("2")]])

    def test_comments_are_whitespace_and_must_close(self):
        self.assertEqual(review.strip_comments("1/**/2"), "1 2")
        self.assertTrue(rejected("[1/**/2]", comments=True))
        self.assertTrue(rejected("[] /* open", comments=True))
        self.assertTrue(rejected("[/*/]", comments=True))
        self.assertTrue(rejected("/* /* */ */[]", comments=True))
        self.assertEqual(value("// c\r[1]", comments=True), ["array", [number("1")]])
        self.assertEqual(value("[1]// end", comments=True), ["array", [number("1")]])
        self.assertTrue(rejected("// c\n[1]"))

    def test_trailing_commas(self):
        self.assertEqual(review.strip_trailing_commas('[1,{"a":[2,],},]'), '[1,{"a":[2]}]')
        self.assertEqual(review.strip_trailing_commas('["a,]",]'), '["a,]"]')
        for text in ("[,]", "{,}", "[1,,]", '{"a":,}', "[,1]", "1,"):
            with self.subTest(text=text):
                self.assertTrue(rejected(text, trailing_commas=True))
        self.assertTrue(rejected("[1,]"))
        self.assertTrue(rejected("[1,/**/]", trailing_commas=True))
        self.assertEqual(value("[1,/**/]", trailing_commas=True, comments=True), ["array", [number("1")]])


class WriterTests(unittest.TestCase):
    def test_render_matches_json_dumps(self):
        samples = [
            {"b": 1, "a": [1, 2, {"d": None, "c": True}], "é": "\x00\x1f\x7f\n\t\"\\/", "à": [], "e": {}, "😀": "\u2028\u0085", "\uffff": False},
            [], {}, [[]], [{}], "x", -5, None, [1, [2, [3, []]]],
        ]
        rng = random.Random(7)
        for _ in range(200):
            samples.append(self.random_data(rng, 0))
        for data in samples:
            for sort_keys in (False, True):
                for indent in (0, 1, 2, 4):
                    with self.subTest(data=data, sort_keys=sort_keys, indent=indent):
                        expected = json.dumps(data, ensure_ascii=False, sort_keys=sort_keys, indent=indent or None, separators=(",", ": ") if indent else (",", ":"))
                        self.assertEqual(review.render(plain_tree(data), sort_keys, indent), expected)

    def random_data(self, rng, depth):
        roll = rng.random()
        if depth > 3 or roll < 0.4:
            return rng.choice([None, True, False, 0, -7, 123456789012345678901234567890, "", "a", "é\n", "\x01\x7f😀", '"\\'])
        if roll < 0.7:
            return [self.random_data(rng, depth + 1) for _ in range(rng.randrange(4))]
        keys = ["a", "B", "é", "😀", "\uffff", "", "a\x00", "z"]
        return {rng.choice(keys): self.random_data(rng, depth + 1) for _ in range(rng.randrange(4))}

    def test_writer_errors(self):
        self.assertEqual(review.write_expectation(["array", [number("NaN")]], False, 0), {"error_any_of": [{"error": "InvalidNumber", "value": "NaN"}]})
        self.assertIsInstance(review.write_expectation(value("[" * 128 + "]" * 128), False, 0), str)
        deep = value("[" * 129 + "]" * 129, max_depth=129)
        self.assertEqual(review.write_expectation(deep, True, 2), {"error_any_of": [{"error": "TooDeep", "value": None}]})
        both = review.write_errors(value("[" * 130 + "NaN" + "]" * 130, max_depth=130, non_finite=True))
        self.assertIn({"error": "TooDeep", "value": None}, both)
        self.assertIn({"error": "InvalidNumber", "value": "NaN"}, both)
        self.assertEqual(review.write_errors(["object", [["a", None], ["a", None]]]), [{"error": "DuplicateKey", "value": "a"}])
        for bad in ("01", "1.", "+1", "1e", "Infinity", "0x1", "\u0661"):
            self.assertEqual(review.write_errors(number(bad)), [{"error": "InvalidNumber", "value": bad}])

    def test_writer_matching(self):
        errors = {"error_any_of": [{"error": "TooDeep", "value": None}, {"error": "InvalidNumber", "value": "NaN"}]}
        self.assertTrue(review.writer_matches({"error": "InvalidNumber", "value": "NaN"}, errors))
        self.assertFalse(review.writer_matches({"error": "InvalidNumber", "value": "Infinity"}, errors))
        self.assertFalse(review.writer_matches("[]", errors))
        self.assertTrue(review.writer_matches("[]", "[]"))
        self.assertFalse(review.writer_matches("[ ]", "[]"))

    def test_oracle_reports_all_three_writers(self):
        result = parse('{"b":[1],"a":"é"}', indent=4, sort_keys=True)
        self.assertEqual(result["compact"], '{"b":[1],"a":"é"}')
        self.assertEqual(result["pretty"], '{\n    "a": "é",\n    "b": [\n        1\n    ]\n}')
        self.assertEqual(result["sorted"], '{"a":"é","b":[1]}')


def ok_output(tree, **extra):
    output = {"status": "ok", "value": tree, "compact": "x", "pretty": "x", "sorted": "x", "parser_agrees": True}
    output.update(extra)
    return output


class ProbeTests(unittest.TestCase):
    def completed(self, output, code=0):
        stdout = output if isinstance(output, bytes) else json.dumps(output).encode()
        return subprocess.CompletedProcess([], code, stdout, b"[ROC CRASHED] JsonValue.parse_with is not implemented yet")

    def run_probe(self, output, data=b"[1]", code=0):
        with patch.object(review.subprocess, "run", return_value=self.completed(output, code)):
            return review.probe(Path("probe"), data, [])

    def test_timeouts_and_crashes_are_not_parse_errors(self):
        with patch.object(review.subprocess, "run", side_effect=subprocess.TimeoutExpired([], 5)):
            self.assertEqual(review.probe(Path("probe"), b"x", [])["status"], "timeout")
        crash = self.run_probe({}, code=1)
        self.assertEqual(crash["status"], "crash")
        self.assertIn("not implemented", crash["stderr"])

    def test_protocol_violations(self):
        for output in (b"not json", {"status": "usage_error", "message": "bad flag"}, ok_output(1), ok_output(["number", 1]), ok_output(["array", [1.5]]),
                       ok_output(["object", [["a"]]]), ok_output(["map", []]), ok_output(None, compact={"error": "Oops"}), ok_output(None, pretty={"error": "TooDeep", "value": "x"}),
                       ok_output(None, parser_agrees="yes"), {"status": "error", "line": 1, "column": 1}):
            with self.subTest(output=output):
                self.assertEqual(self.run_probe(output)["status"], "protocol_error")

    def test_diagnostics_must_point_inside_the_input(self):
        def error(line, column, message="bad", **extra):
            return {"status": "error", "line": line, "column": column, "message": message, "parser_agrees": True, **extra}
        data = b"[\r\n1,\r\n\xc3\xa9?]"
        self.assertEqual(self.run_probe(error(3, 5), data)["status"], "error")
        self.assertEqual(self.run_probe(error(3, 6), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(4, 1), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(0, 1), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(1, 0), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(1, 1, ""), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(1, 1, offset=len(data) + 1), data)["status"], "invalid_diagnostic")
        self.assertEqual(self.run_probe(error(1, 1), b"")["status"], "error")
        self.assertEqual(self.run_probe(error(2, 1), b"[1,\n")["status"], "error")
        self.assertEqual(self.run_probe(error(2, 1), b"[1,\r")["status"], "error")

    def test_valid_output_passes_through(self):
        tree = ["object", [["a", ["array", [["number", "1"], None, True, "s"]]]]]
        result = self.run_probe(ok_output(tree, compact={"error": "TooDeep", "value": None}))
        self.assertEqual(result["value"], tree)


class CompareTests(unittest.TestCase):
    case = {"id": "case/x", "data": b"[1]", "options": {}}
    expected = review.oracle(b"[1]", {})

    def actual(self, **changes):
        result = {"status": "ok", "value": ["array", [number("1")]], "compact": "[1]", "pretty": "[\n  1\n]", "sorted": "[1]", "parser_agrees": True}
        result.update(changes)
        return result

    def kind(self, actual, expected=None, case=None):
        return review.compare(case or self.case, actual, expected or self.expected)["kind"]

    def test_classification(self):
        self.assertEqual(self.kind(self.actual()), "pass")
        self.assertEqual(self.kind(self.actual(value=["array", [number("1.0")]])), "wrong_value")
        self.assertEqual(self.kind(self.actual(pretty="[1]")), "writer_mismatch")
        self.assertEqual(self.kind(self.actual(parser_agrees=False)), "parser_disagreement")
        self.assertEqual(self.kind({"status": "error", "message": "no", "line": 1, "column": 1}), "valid_rejection")
        refused = review.oracle(b"[01]", {})
        self.assertEqual(self.kind(self.actual(), refused), "invalid_acceptance")
        self.assertEqual(self.kind({"status": "error", "message": "no", "line": 1, "column": 2, "parser_agrees": True}, refused), "pass")
        self.assertEqual(self.kind({"status": "error", "message": "no", "line": 1, "column": 2, "parser_agrees": False}, refused), "parser_disagreement")
        for fatal in review.FATAL:
            self.assertEqual(self.kind({"status": fatal}), fatal)

    def test_suite_verdicts(self):
        accepted = self.actual()
        disagreeing = review.compare({**self.case, "expect": "n"}, accepted, self.expected)
        self.assertEqual(disagreeing["kind"], "invalid_acceptance")
        self.assertIsNotNone(disagreeing["oracle_disagreement"])
        implementation = review.compare({**self.case, "expect": "i"}, {"status": "error", "message": "no", "line": 1, "column": 1}, self.expected)
        self.assertEqual(implementation["kind"], "pass")
        self.assertEqual(implementation["implementation_defined"], {"probe": "error", "oracle": "ok", "comparison": "valid_rejection"})
        self.assertEqual(review.compare({**self.case, "expect": "i"}, {"status": "crash"}, self.expected)["kind"], "crash")

    def test_policy_cases_run_under_keep_last(self):
        cases = {case["id"]: case for case in review.suite_cases()}
        for name in ("suite/y_object_duplicated_key", "suite/y_object_duplicated_key_and_value"):
            self.assertEqual(cases[name]["options"], {"duplicates": "KeepLast"})
            self.assertIn("policy deviation", cases[name]["note"])
            self.assertEqual(review.oracle(cases[name]["data"], cases[name]["options"])["status"], "ok")
            self.assertEqual(review.oracle(cases[name]["data"], {})["status"], "error")

    def test_signature_is_stable_and_sensitive(self):
        row = review.compare(self.case, self.actual(value=["array", [number("2")]]), self.expected)
        self.assertEqual(review.signature(row), review.signature(json.loads(json.dumps(row))))
        other_value = review.compare(self.case, self.actual(value=["array", [number("3")]]), self.expected)
        self.assertNotEqual(review.signature(row), review.signature(other_value))
        other_options = review.compare({**self.case, "options": {"comments": True}}, self.actual(value=["array", [number("2")]]), self.expected)
        self.assertNotEqual(review.signature(row), review.signature(other_options))
        reworded = review.compare(self.case, {"status": "error", "message": "one", "line": 1, "column": 1}, self.expected)
        reworded_again = review.compare(self.case, {"status": "error", "message": "two", "line": 1, "column": 2}, self.expected)
        self.assertEqual(review.signature(reworded), review.signature(reworded_again))


class BaselineTests(unittest.TestCase):
    def run_check(self, baseline_failures, actual, record=None):
        case = {"id": "case/x", "group": "case", "data": b"[1]", "options": {}}
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            baseline = work / "baseline.json"
            review.write_json(baseline, {"failures": baseline_failures})
            args = SimpleNamespace(roc="roc", work_dir=work, case=None, no_suite=True, no_documents=True, baseline=None if record else baseline, record_known_failures=record and work / record)
            with patch.object(review, "build", return_value=work / "probe"), patch.object(review, "load_cases", return_value=[case]), \
                    patch.object(review, "probe", return_value=actual), patch.object(review, "metadata", return_value={}), \
                    patch("builtins.print"):
                code = review.check(args)
            recorded = json.loads((work / record).read_text()) if record and (work / record).exists() else None
            return code, recorded

    def test_known_failures_must_match_their_signature(self):
        wrong = {"status": "ok", "value": ["array", [number("2")]], "compact": "[2]", "pretty": "[\n  2\n]", "sorted": "[2]", "parser_agrees": True}
        row = review.compare({"id": "case/x", "data": b"[1]", "options": {}}, wrong, review.oracle(b"[1]", {}))
        self.assertEqual(self.run_check({"case/x": {"signature": review.signature(row)}}, wrong)[0], 0)
        self.assertEqual(self.run_check({"case/x": {"signature": "stale"}}, wrong)[0], 1)
        fixed = {**wrong, "value": ["array", [number("1")]], "compact": "[1]", "pretty": "[\n  1\n]", "sorted": "[1]"}
        self.assertEqual(self.run_check({"case/x": {"signature": review.signature(row)}}, fixed)[0], 1)
        self.assertEqual(self.run_check({}, fixed)[0], 0)
        code, recorded = self.run_check({}, wrong, record="known.json")
        self.assertEqual(code, 0)
        self.assertEqual(recorded["failures"]["case/x"]["kind"], "wrong_value")

    def test_fatal_results_are_never_baselined(self):
        crash = {"status": "crash", "return_code": 1, "stderr": "[ROC CRASHED]"}
        row = review.compare({"id": "case/x", "data": b"[1]", "options": {}}, crash, review.oracle(b"[1]", {}))
        self.assertEqual(self.run_check({"case/x": {"signature": review.signature(row)}}, crash)[0], 1)
        with self.assertRaises(ValueError):
            self.run_check({}, crash, record="known.json")

    def test_record_flag_writes_to_the_baseline_by_default(self):
        seen = []
        with patch.object(review, "check", side_effect=lambda args: seen.append(args.record_known_failures) or 0):
            self.assertEqual(review.main(["check", "--baseline", "known.json", "--record-known-failures"]), 0)
            self.assertEqual(review.main(["check", "--record-known-failures", "other.json"]), 0)
        self.assertEqual(seen, [Path("known.json"), Path("other.json")])
        with patch("sys.stderr"), self.assertRaises(SystemExit):
            review.main(["check", "--record-known-failures"])

    def test_checked_in_baseline_is_well_formed(self):
        baseline = json.loads((review.DATA / "known-failures.json").read_text())
        self.assertIsInstance(baseline["failures"], dict)
        for entry in baseline["failures"].values():
            self.assertTrue(entry["reason"])
            self.assertNotIn(entry["kind"], review.FATAL)


class CorpusTests(unittest.TestCase):
    def test_show_markers(self):
        shown = '"[1,"\nJSON-HEX: 5b312c5d\nJSON-OPTIONS: --trailing-commas --max-depth 3\n'
        self.assertEqual(review.parse_show(shown), {"data": b"[1,]", "options": review.normalize_options({"trailing_commas": True, "max_depth": 3})})
        self.assertEqual(review.parse_show("JSON-HEX: 7b7d")["options"], review.normalize_options({}))
        self.assertEqual(review.parse_show("JSON-HEX: \nJSON-OPTIONS:")["data"], b"")
        self.assertIsNone(review.parse_show("no marker here"))
        self.assertIsNone(review.parse_show("JSON-HEX: 5b3"))
        with self.assertRaises(ValueError):
            review.parse_show("JSON-HEX: 5b5d\nJSON-OPTIONS: { comments: True }")


class DataTests(unittest.TestCase):
    def test_suite_is_complete_and_pinned(self):
        suite = json.loads((review.DATA / "suite.json").read_text())
        self.assertEqual(suite["source"]["repository"], "https://github.com/nst/JSONTestSuite")
        self.assertRegex(suite["source"]["revision"], r"\A[0-9a-f]{40}\Z")
        self.assertEqual(suite["source"]["license"], "MIT")
        counts = {expect: sum(case["expect"] == expect for case in suite["cases"]) for expect in "yni"}
        self.assertEqual(counts, {"y": 95, "n": 188, "i": 35})
        self.assertEqual(len({case["id"] for case in suite["cases"]}), len(suite["cases"]))
        for case in suite["cases"]:
            self.assertRegex(case["id"], r"\Asuite/[yni]_")
            self.assertEqual(case["id"][6], case["expect"])
        self.assertIn("Nicolas Seriot", (review.DATA / "JSONTESTSUITE-LICENSE").read_text())

    def test_cases_exercise_every_option_and_each_way(self):
        cases = review.hand_cases()
        self.assertGreater(len(cases), 300)
        for key, default in review.DEFAULT_OPTIONS.items():
            changed = [case for case in cases if case["options"][key] != default]
            self.assertTrue(changed, key + " is never set")
            verdicts = {review.oracle(case["data"], case["options"])["status"] for case in changed}
            if key not in ("sort_keys", "indent"):
                self.assertEqual(verdicts, {"ok", "error"}, key + " needs accepted and rejected cases")
        for mode in review.ENUMS["duplicates"]:
            self.assertTrue([case for case in cases if case["options"]["duplicates"] == mode and b"\\u0061" in case["data"]], mode + " needs an escaped alias")

    def test_documents_are_pinned(self):
        names = set()
        for document in review.DOCUMENTS:
            self.assertRegex(document["url"], r"\Ahttps://")
            self.assertRegex(document["sha256"], r"\A[0-9a-f]{64}\Z")
            review.normalize_options(document["options"])
            names.add(document["name"])
        self.assertEqual(len(names), len(review.DOCUMENTS))
        spdx3 = next(document for document in review.DOCUMENTS if document["name"].startswith("spdx-3"))
        self.assertEqual(spdx3["options"], {"duplicates": "CollapseIdentical"})

    def test_workflow_runs_the_review(self):
        workflow = (review.ROOT / ".github" / "workflows" / "fuzz.yml").read_text()
        for format_name, command in (("json", "python scripts/review_json.py check --baseline scripts/json/known-failures.json"), ("json-robustness", "python scripts/review_json.py campaign --smoke")):
            row = re.search(r"- format: " + re.escape(format_name) + r"\n((?:\s{12}\S.*\n)*)", workflow)
            self.assertIsNotNone(row, format_name)
            self.assertIn("command: " + command, row[1])
            self.assertNotIn("requirements", row[1])


class GeneratorTests(unittest.TestCase):
    def test_generated_inputs_are_reproducible_bounded_and_valid(self):
        first, second = review.generated_inputs(94), review.generated_inputs(94)
        statuses = set()
        for _ in range(300):
            data, options = next(first)
            self.assertEqual((data, options), next(second))
            self.assertLessEqual(len(data), review.MAX_GENERATED_BYTES)
            self.assertEqual(review.normalize_options(options), options)
            statuses.add(review.oracle(data, options)["status"])
        self.assertEqual(statuses, {"ok", "error"})

    def test_mutations_stay_deterministic(self):
        self.assertEqual(review.mutate(b'{"a":[1,2]}', random.Random(3)), review.mutate(b'{"a":[1,2]}', random.Random(3)))


class CampaignTests(unittest.TestCase):
    def test_repeated_runs_get_an_empty_evidence_folder_and_keep_the_corpus(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            (work / "evidence").mkdir()
            (work / "evidence" / "run.log").write_text("previous run\n")
            (work / "corpus").mkdir()
            (work / "corpus" / "input").write_bytes(b"[1]")
            evidence = review.fresh_evidence_dir(work)
            self.assertEqual(evidence, work / "evidence")
            self.assertFalse(evidence.exists())
            self.assertEqual((work / "corpus" / "input").read_bytes(), b"[1]")
            self.assertEqual(review.fresh_evidence_dir(work), evidence)


if __name__ == "__main__":
    unittest.main()
