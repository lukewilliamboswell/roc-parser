"""Reference selection and cache tests; native commands are always mocked."""
import copy
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import Mock, patch

from scripts import bench, readme_references as references


def arguments(folder, refresh=False):
    return SimpleNamespace(work=folder, reference_cache_dir=folder / "cache", refresh_references=refresh,
                           python="existing-python", quick=False, repetitions=2, target_ms=20,
                           max_iterations=1000, timeout=30, max_spread_pct=25)


def measured(impl, document, args):
    duration = 10 if impl.name == "qif_parser" else 30
    batch = {"iterations": 1, "elapsed_ns": duration, "successes": 1, "checksum": 5, "peak_rss_bytes": 1024}
    batches = [copy.deepcopy(batch) for _ in range(args.repetitions)]
    record = {"impl": impl.id, "lang": impl.lang, "library": impl.name, "doc": document.id,
              "kind": document.kind, "bytes": len(document.data), "workload": impl.workload,
              "iterations": 1, "accepted": True, "checksum": 5, "peak_rss_bytes": 1024,
              "batch_elapsed_ns": [duration] * args.repetitions,
              "receipts": {"command": ["mock-native"], "measured_at": "2026-11-17T00:00:00+00:00",
                           "calibration": batch, "warmup": batch, "batches": batches}}
    record.update(bench.summarize([duration] * args.repetitions, len(document.data)))
    return record


class ReadmeReferenceTests(unittest.TestCase):
    def setup_run(self, folder, refresh=False):
        docs = [bench.Doc(fmt, "representative", "medium", b"data") for fmt in ("csv", "qif", "html")]
        builder = Mock(skipped={})
        for language in references.LANGUAGES:
            specs = bench.reference_specs((language,), ("csv", "qif"))
            getattr(builder, f"{language}_impls").return_value = specs
        extra = Mock()
        extra.specs.return_value = [bench.Impl("python", "dom", "html", [], "owned DOM collection")]
        extra.build.return_value = extra.specs.return_value
        extra.profile.side_effect = lambda impl, args, machine: {"runtime": {"version": "fake"}, "machine": machine, "sources": {"extra": "sha"}}
        extra.fresh_roc_check.return_value = {"status": "not_checked", "notes": ["different checksum contract"]}
        extra.qualification.return_value = {"status": "qualified", "checksum_semantics": "canonical-dom-v1", "ownership": "owned DOM"}
        return docs, builder, extra, references.ReferenceRun(builder, arguments(folder, refresh), docs, extra)

    def profile(self, language, args, machine):
        return {"runtime": {"version": "fake"}, "machine": machine, "sources": {language: "sha"}}

    def roc_results(self, documents):
        return [{"doc": doc.id, "iterations": 2, "checksum": 10} for doc in documents]

    @patch.object(bench, "reference_machine", return_value={"affinity": [7]})
    def test_default_cold_cache_never_builds_or_measures_native_references(self, machine):
        with tempfile.TemporaryDirectory() as tmp:
            docs, builder, extra, run = self.setup_run(Path(tmp))
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=AssertionError("native measurement")) as measure:
                run.build()
                rows, fastest = run.collect(self.roc_results(docs), {"git": {"commit": "roc"}})
        self.assertFalse(fastest)
        self.assertTrue(all(row["status"] == "ReferenceUnavailable" for row in rows))
        measure.assert_not_called()
        for language in references.LANGUAGES:
            getattr(builder, f"{language}_impls").assert_not_called()
        extra.build.assert_not_called()
        extra.qualification.assert_not_called()

    @patch.object(bench, "reference_machine", return_value={"affinity": [7]})
    def test_explicit_refresh_then_hit_preserves_receipts_without_native_work(self, machine):
        with tempfile.TemporaryDirectory() as tmp:
            docs, builder, extra, run = self.setup_run(Path(tmp), True)
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=measured):
                run.build()
                fresh, fastest = run.collect(self.roc_results(docs), {"git": {"commit": "original-roc"}})
            self.assertTrue(all(row["reference_cache"]["status"] == "refreshed" for row in fresh))
            self.assertEqual(fastest["qif"]["library"], "qif_parser")
            self.assertTrue(fastest["qif"]["ownership_differs"])
            self.assertIn("amounts use f64", fastest["qif"]["semantic_note"])
            self.assertEqual(fastest["html"]["semantic_status"], "different_checksum_contract")
            docs, builder, extra, cached = self.setup_run(Path(tmp))
            extra.qualification.side_effect = AssertionError("native qualification rerun on hit")
            extra.fresh_roc_check.return_value = {"status": "match", "notes": ["new Roc canonical output"]}
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=AssertionError("native rerun")):
                cached.build()
                reused, fastest = cached.collect(self.roc_results(docs), {"git": {"commit": "changed-roc"}})
            self.assertTrue(all(row["reference_cache"]["status"] == "hit" for row in reused))
            self.assertTrue(all(row["reference_cache"]["provenance"]["git"]["commit"] == "original-roc" for row in reused))
            self.assertTrue(all(row["receipts"]["command"] == ["mock-native"] for row in reused))
            self.assertEqual(fastest["html"]["semantic_status"], "canonical_equal")
            extra.fresh_roc_check.assert_called_once()
            for path in (Path(tmp) / "cache").glob("*.json"):
                payload = __import__("json").loads(path.read_text())
                self.assertNotIn("fresh_roc_check", payload["record"])
                self.assertNotIn("fresh_roc_check", payload["provenance"].get("qualification", {}))
            self.assertFalse(any(key.endswith("_ratio") or "speedup" in key for row in reused for key in row))
            for language in references.LANGUAGES:
                getattr(builder, f"{language}_impls").assert_not_called()

    @patch.object(bench, "reference_machine", return_value={"affinity": [7]})
    def test_cpu_or_input_change_rejects_cached_reference_without_refresh(self, machine):
        with tempfile.TemporaryDirectory() as tmp:
            docs, _, _, run = self.setup_run(Path(tmp), True)
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=measured):
                run.build()
                run.collect(self.roc_results(docs), {"git": {"commit": "original"}})
            _, _, _, changed = self.setup_run(Path(tmp))
            machine.return_value = {"affinity": [8]}
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=AssertionError("native work")):
                rows, fastest = changed.collect(self.roc_results(docs), {"git": {"commit": "new"}})
            self.assertFalse(fastest)
            self.assertTrue(all(row["status"] == "ReferenceUnavailable" for row in rows))

    @patch.object(bench, "reference_machine", return_value={"affinity": [7]})
    def test_reference_measurement_must_consume_its_canonical_evidence_before_caching(self, machine):
        with tempfile.TemporaryDirectory() as tmp:
            docs, _, extra, run = self.setup_run(Path(tmp), True)
            extra.qualification.return_value = {"status": "qualified", "expected_checksum": 6}
            with patch.object(bench, "reference_profile", side_effect=self.profile), patch.object(bench, "measure", side_effect=measured):
                run.build()
                rows, fastest = run.collect(self.roc_results(docs), {"git": {"commit": "original"}})
            self.assertNotIn("html", fastest)
            self.assertEqual(next(row for row in rows if row["format"] == "html")["status"], "ReferenceUnavailable")
            self.assertEqual(len(list((Path(tmp) / "cache").glob("*.json"))), 5)

    def test_fastest_collection_can_be_borrowed_or_semantically_different_but_is_labeled(self):
        base = {"format": "qif", "accepted": True, "workload": "collect", "median_ns": 30,
                "ownership_differs": False, "semantic_status": "checksum_equal"}
        borrowed = {**base, "workload": "collect-borrowed", "median_ns": 10, "ownership_differs": True,
                    "semantic_status": "checksum_differs"}
        consume = {**base, "workload": "consume", "median_ns": 1}
        failed = {**base, "median_ns": 2, "error": "failed"}
        self.assertEqual(references.fastest_references([base, borrowed, consume, failed]), {"qif": borrowed})

    def test_unstable_or_rejected_reference_is_not_publishable(self):
        document = bench.Doc("csv", "representative", "medium", b"data")
        impl = bench.Impl("python", "csv", "csv", [], "collection")
        args = arguments(Path("/unused"))
        identity = bench.reference_identity(impl, document, args, {"runtime": "fake"})
        original = measured(impl, document, args)
        for change in (lambda row: row.update(accepted=False),
                       lambda row: row["receipts"]["calibration"].update(checksum=6),
                       lambda row: row.update(error="native failure")):
            row = copy.deepcopy(original)
            change(row)
            with self.subTest(change=change), self.assertRaises(bench.ReferenceUnavailable):
                references.qualify_measurement(row, identity, args.max_spread_pct)
