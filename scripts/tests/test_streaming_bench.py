"""Streaming comparisons preserve input spelling and fail closed on weak evidence."""
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from scripts import bench, _performance_target as target, _streaming_bench as streaming


class StreamingCorpusTests(unittest.TestCase):
    def test_components_use_the_top_level_member_and_preserve_number_spelling(self):
        text = '{"metadata":{"components":[0]},"components":[{"n":1E5},123456789012345678901234567890]}'
        self.assertEqual(streaming.component_records(text),
                         ['{"n":1E5}', '123456789012345678901234567890'])

    def test_records_are_identical_between_array_and_line_corpora(self):
        docs = streaming.documents(bench, True, ("json",))
        array = next(d for d in docs if d.name == "array-small")
        ndjson = next(d for d in docs if d.name == "ndjson-small")
        self.assertEqual(json.loads(array.data), [json.loads(line) for line in ndjson.data.splitlines()])
        self.assertEqual([(d.id, d.sha256) for d in docs],
                         [(d.id, d.sha256) for d in streaming.documents(bench, True, ("json",))])

    def test_modes_are_never_measured_on_the_wrong_record_framing(self):
        specs = streaming.references(bench, ("rust",), ("json",))
        for doc in streaming.documents(bench, True, ("json",)):
            matching = [s for s in specs if streaming.accepts(s, doc)]
            self.assertEqual(len(matching), 1)
            self.assertIn(doc.name.split("-")[0], matching[0].name)

    def test_header_pipeline_contains_many_messages_without_opaque_bodies(self):
        doc = next(d for d in streaming.documents(bench, True, ("http",)) if d.name == "pipeline-small")
        self.assertGreaterEqual(doc.data.count(b"GET /docs/"), 7)
        self.assertEqual(doc.data.count(b"GET /docs/"), doc.data.count(b"\r\n\r\n"))
        self.assertNotIn(b"Content-Length", doc.data)

    def test_number_spelling_models_qualify_both_consumers_without_changing_input(self):
        doc = bench.Doc("json", "array-test", "sample", b'[1e5,-0,1.20E-03,18446744073709551616,"\xc3\xa9"]')
        roc = bench.Impl("roc", "roc-parser-consume", "json", [])
        rust = bench.Impl("rust", "serde-json-array-consume", "json", [])
        expected = [streaming.json_checksum(doc, serde=False), streaming.json_checksum(doc, serde=True)]
        # Implied '+' adds one byte; serde's integer -0 loses one byte.
        self.assertEqual(expected[0], expected[1])
        simple = bench.Doc("json", "array-test", "sample", b"[1e5]")
        self.assertEqual(streaming.json_checksum(simple, False), 12)
        self.assertEqual(streaming.json_checksum(simple, True), 13)
        for impl in (roc, rust):
            value = streaming.json_checksum(simple, impl.lang == "rust")
            qualified = streaming.qualify_checksum(impl, simple, dict(accepted=True, iterations=3, checksum=3 * value))
            self.assertTrue(qualified["checksum_qualified"])
            bad = streaming.qualify_checksum(impl, simple, dict(accepted=True, iterations=3, checksum=3 * value + 1))
            self.assertIn("error", bad)

    def test_direct_comparator_version_wins_over_a_newer_transitive_dependency(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rust = root / "compare/rust"
            rust.mkdir(parents=True)
            (rust / "Cargo.toml").write_text('[dependencies]\nquick-xml = "=0.38.3"\n')
            (rust / "Cargo.lock").write_text('[[package]]\nname = "quick-xml"\nversion = "0.38.3"\n\n[[package]]\nname = "quick-xml"\nversion = "0.39.4"\n')
            with patch.object(bench, "BENCH", root):
                self.assertEqual(bench.cargo_lock_versions()["quick-xml"], "0.38.3")

    def test_streaming_defaults_and_invalid_performance_targets(self):
        args = bench.parse_args(["--streaming", "--formats", "json,xml,http"])
        self.assertEqual(args.langs, ("roc", "rust"))
        self.assertEqual(args.label, "streaming-full")
        for value in ("0", "-1", "nan", "inf"):
            with self.subTest(value=value), self.assertRaises(SystemExit):
                bench.parse_args(["--max-slowdown", value])


class PerformanceTargetTests(unittest.TestCase):
    def row(self, impl, ns, **kwargs):
        return dict(impl=impl, lang=impl.split("/")[0], doc="xml/medium", kind="medium",
                    accepted=True, median_ns=ns, spread_pct=1, workload="collect",
                    checksum=10, iterations=1, mb_per_s=1000 / ns, **kwargs)

    def test_elapsed_time_threshold_is_inclusive_and_uses_fastest_borrowed_reference(self):
        rows = [self.row("roc/roc-parser", 150), self.row("rust/owned", 120),
                {**self.row("rust/borrowed", 100), "workload": "collect-borrowed"}]
        result = target.evaluate(rows, 1.5)[0]
        self.assertEqual(result["status"], "passed")
        self.assertEqual(result["reference"], "rust/borrowed")
        self.assertEqual(result["slowdown"], 1.5)
        rows[0]["median_ns"] = 151
        self.assertEqual(target.evaluate(rows, 1.5)[0]["status"], "failed")

    def test_missing_or_noisy_fastest_reference_cannot_make_a_slower_library_win(self):
        mine = self.row("roc/roc-parser", 150)
        slow = self.row("go/slow", 200)
        fast = {**self.row("rust/fast", 100), "spread_pct": 30}
        self.assertEqual(target.evaluate([mine, slow, fast], 1.5)[0]["status"], "unqualified")
        unavailable = dict(impl="rust/fast", lang="rust", doc="xml/medium", error="ReferenceUnavailable")
        self.assertEqual(target.evaluate([mine, slow, unavailable], 1.5)[0]["status"], "unqualified")

    def test_collection_cannot_supply_the_streaming_target(self):
        mine = {**self.row("roc/roc-parser-consume", 150), "workload": "consume"}
        self.assertEqual(target.evaluate([mine, self.row("rust/tree", 100)], 1.5)[0]["status"], "unqualified")

    def test_failure_or_noise_in_roc_is_not_a_pass(self):
        ref = self.row("rust/ref", 100)
        for change in ({"accepted": False}, {"error": "crash"}, {"spread_pct": 26}):
            mine = {**self.row("roc/roc-parser", 50), **change}
            self.assertNotEqual(target.evaluate([mine, ref], 1.5)[0]["status"], "passed")

    def test_mismatched_consumer_outputs_cannot_pass_by_being_fast(self):
        mine = {**self.row("roc/roc-parser-consume", 50), "workload": "consume", "checksum": 1}
        ref = {**self.row("rust/events-consume", 100), "workload": "consume", "checksum": 999}
        self.assertEqual(target.evaluate([mine, ref], 1.5)[0]["status"], "unqualified")
        # JSON can use independent expected checksums for each library's number spelling.
        mine["checksum_qualified"] = ref["checksum_qualified"] = True
        self.assertEqual(target.evaluate([mine, ref], 1.5)[0]["status"], "passed")
        row = bench.comparison([mine, ref])
        self.assertEqual(row[0]["documents"], 1)
        del ref["checksum_qualified"]
        row = bench.comparison([mine, ref])
        self.assertEqual(row[0]["documents"], 0)
        self.assertEqual(row[0]["semantic_mismatches"], ["xml/medium"])


if __name__ == "__main__":
    unittest.main()
