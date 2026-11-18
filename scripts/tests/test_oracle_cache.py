"""Reference caching is independent of the current Roc program or its answers."""
from decimal import Decimal
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from scripts import _oracle_cache as cache


def adapter_one():
    return "adapter one"


def adapter_two():
    return "adapter two"


def load_review(name):
    spec = importlib.util.spec_from_file_location("cache_test_review_" + name, Path(__file__).resolve().parents[1] / ("review_" + name + ".py"))
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


class OracleCacheTests(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory()
        self.addCleanup(self.folder.cleanup)
        self.root = Path(self.folder.name)
        self.calls = 0
        self.cache = cache.OracleCache(self.root)

    def produce(self):
        self.calls += 1
        return {"status": "ok", "bytes": b"\x00\xff\x80", "amount": Decimal("123456789012345678901234567890.0001")}

    def answer(self, instance=None, data=b"source", options=None, adapter=adapter_one, **kwargs):
        return (instance or self.cache).answer("example", data, options or {"strict": True}, self.produce, adapters=(adapter,), **kwargs)

    def entries(self):
        return [path for path in self.root.rglob("*.json") if path.name != "versions.json"]

    def test_answer_bytes_and_exact_decimals_roundtrip_without_producer(self):
        expected = self.answer()
        self.assertEqual(self.answer(), expected)
        self.assertEqual(self.calls, 1)
        self.assertEqual(self.cache.statistics(), {"hits": 1, "misses": 1, "writes": 1})
        self.assertEqual(self.answer()["bytes"], b"\x00\xff\x80")

    def test_returned_mutation_does_not_change_reference(self):
        result = self.answer()
        result["bytes"] = b"changed"
        self.assertEqual(self.answer()["bytes"], b"\x00\xff\x80")

    def test_exact_bytes_configuration_identifier_and_adapter_invalidate(self):
        self.answer()
        self.answer(data=b"source\x00")
        self.answer(options={"strict": False})
        self.answer(adapter=adapter_two)
        self.cache.answer("other", b"source", {"strict": True}, self.produce, adapters=(adapter_one,))
        self.assertEqual(self.calls, 5)

    def test_pins_and_installed_versions_invalidate(self):
        pins = self.root / "requirements.txt"
        pins.write_text("example==1\n")
        one = cache.OracleCache(self.root, version_reader=lambda primary, wanted: {"example": "1"})
        two = cache.OracleCache(self.root, version_reader=lambda primary, wanted: {"example": "2"})
        self.answer(one, requirements=pins, primary="example")
        self.answer(two, requirements=pins, primary="example")
        pins.write_text("example==2\n")
        self.answer(two, requirements=pins, primary="example")
        self.assertEqual(self.calls, 3)

    def test_offline_uses_matching_pins_without_importing_oracle(self):
        pins = self.root / "requirements.txt"
        pins.write_text("example==1\n")
        online = cache.OracleCache(self.root, version_reader=lambda primary, wanted: {"example": "1"})
        self.answer(online, requirements=pins, primary="example")
        offline = cache.OracleCache(self.root, version_reader=lambda primary, wanted: None)
        self.answer(offline, requirements=pins, primary="example")
        self.assertEqual(self.calls, 1)

    def test_offline_never_reuses_an_unrequested_actual_version(self):
        pins = self.root / "requirements.txt"
        pins.write_text("example==1\n")
        wrong = cache.OracleCache(self.root, version_reader=lambda primary, wanted: {"example": "2"})
        self.answer(wrong, requirements=pins, primary="example")
        offline = cache.OracleCache(self.root, version_reader=lambda primary, wanted: None)
        self.answer(offline, requirements=pins, primary="example")
        self.assertEqual(self.calls, 2)
        self.assertEqual(offline.hits, 0)

    def test_known_negative_answers_are_cached(self):
        calls = []
        def reject():
            calls.append(True)
            return {"status": "error", "message": "unterminated input"}
        for _ in range(2):
            answer = self.cache.answer("reject", b"bad", {}, reject, adapters=(adapter_one,))
            self.assertEqual(answer["status"], "error")
        self.assertEqual(len(calls), 1)

    def test_transient_and_unexpected_exceptions_are_never_cached(self):
        for error in (OSError("temporary"), subprocess.TimeoutExpired("oracle", 1), ImportError("missing"), RuntimeError("bug")):
            calls = []
            def fail():
                calls.append(True)
                raise error
            for _ in range(2):
                with self.assertRaises(type(error)):
                    self.cache.answer("transient", b"data", {}, fail, adapters=(adapter_one,))
            self.assertEqual(len(calls), 2)
        self.assertEqual(self.entries(), [])

    def test_corrupt_or_truncated_files_are_misses(self):
        self.answer()
        self.entries()[0].write_text('{"schema":')
        self.answer()
        self.assertEqual(self.calls, 2)

    def test_integrity_identity_schema_and_malformed_decimal_are_misses(self):
        self.answer()
        for mutate in (
            lambda entry: entry.update(identity="wrong"),
            lambda entry: entry.update(schema=999),
            lambda entry: entry.update(payload=["decimal", "not-a-decimal"]),
        ):
            path = self.entries()[0]
            entry = json.loads(path.read_bytes())
            mutate(entry)
            body = {key: entry[key] for key in ("schema", "identity", "payload")}
            entry["sha256"] = cache.digest(body)
            path.write_bytes(cache.canonical(entry))
            self.answer()
        self.assertEqual(self.calls, 4)

    def test_refresh_and_disabled_modes_recompute(self):
        self.answer()
        refresh = cache.OracleCache(self.root, refresh=True)
        self.answer(refresh)
        self.answer(refresh)
        disabled = cache.OracleCache(self.root, enabled=False)
        self.answer(disabled)
        self.answer(disabled)
        self.assertEqual(self.calls, 5)
        self.assertEqual(disabled.writes, 0)

    def test_cache_write_failure_does_not_change_oracle_answer(self):
        with patch.object(cache.os, "replace", side_effect=OSError("read-only")):
            self.assertEqual(self.answer()["status"], "ok")
        self.assertEqual(list(self.root.rglob(".oracle-*")), [])


class ReviewIntegrationTests(unittest.TestCase):
    def test_csv_reuses_reference_but_always_calls_current_roc(self):
        review = load_review("csv")
        with tempfile.TemporaryDirectory() as folder:
            review.ORACLE_CACHE = cache.OracleCache(folder)
            real = review._oracle_uncached
            with patch.object(review, "_oracle_uncached", wraps=real) as calls:
                # Fingerprints intentionally refer to the original adapter, not mock wrappers.
                with patch.object(review, "oracle", side_effect=lambda text: review.ORACLE_CACHE.answer("csv-test", text.encode(), {}, lambda: calls(text), adapters=(real,))):
                    for _ in range(2):
                        self.assertEqual(review.oracle("a,b\n")["records"], [["a", "b"]])
                    self.assertEqual(calls.call_count, 1)
            probes = []
            for binary in ("old-roc", "new-roc"):
                with patch.object(review.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, stdout=b'{"status":"ok","records":[["fresh"]]}', stderr=b"")) as run:
                    probes.append(review.probe(Path(binary), "a,b\n"))
                    self.assertEqual(run.call_count, 1)
            self.assertEqual(probes[0], probes[1])

    def test_qif_repeated_comparison_reuses_both_reference_directions(self):
        review = load_review("qif")
        with tempfile.TemporaryDirectory() as folder:
            review.ORACLE_CACHE = cache.OracleCache(folder, version_reader=lambda primary, pins: pins)
            counts = {"reference": 0, "roc": 0}
            def reference(text):
                counts["reference"] += 1
                return {"status": "ok", "rows": [], "written": "canonical"}
            def current_roc(binary, text):
                counts["roc"] += 1
                return {"status": "ok", "rows": [], "written": "canonical"}
            with patch.object(review, "oracle", side_effect=lambda text: review.ORACLE_CACHE.answer("qif-test", text.encode(), {}, lambda: reference(text), adapters=(adapter_one,))), patch.object(review, "probe", side_effect=current_roc):
                for _ in range(2):
                    self.assertIsNone(review.compare("current", {"input": "original"}))
            self.assertEqual(counts, {"reference": 2, "roc": 4})

    def test_qif_explicit_dates_are_clock_independent_but_partial_dates_are_not(self):
        review = load_review("qif")
        self.assertEqual(review.oracle_date_defaults("!Type:Bank\nD27/08/2024\nT1\n^\n"), {"date_defaults": "unused-complete-four-digit-dates"})
        self.assertIn("local_date", review.oracle_date_defaults("D1/2/24\n"))
        self.assertIn("local_date", review.oracle_date_defaults("DJanuary 2\n"))

    def test_ofx_writer_cache_tracks_primary_normalization_changes(self):
        review = load_review("ofx")
        with tempfile.TemporaryDirectory() as folder:
            review.ORACLE_CACHE = cache.OracleCache(folder, version_reader=lambda primary, pins: pins)
            calls = []
            def writer(data):
                calls.append(data)
                return {"status": "ok", "bytes": b"written"}
            with patch.object(review, "_writer_uncached", writer):
                with patch.object(review, "_oracle_uncached", adapter_one):
                    review.external_writer(b"source")
                    review.external_writer(b"source")
                with patch.object(review, "_oracle_uncached", adapter_two):
                    review.external_writer(b"source")
            self.assertEqual(calls, [b"source", b"source"])

    def test_ofx_all_reference_operations_cached_while_changed_roc_is_rechecked(self):
        review = load_review("ofx")
        with tempfile.TemporaryDirectory() as folder:
            review.ORACLE_CACHE = cache.OracleCache(folder, version_reader=lambda primary, pins: pins)
            counts = {"primary": 0, "secondary": 0, "writer": 0, "roc": 0}
            model = {"status": "ok", "statements": [{"account": {"account_id": "001"}, "transactions": [{"id": "x", "amount": "1.00", "name": "x", "memo": ""}]}]}
            def primary(data):
                counts["primary"] += 1
                return model
            def secondary(data):
                counts["secondary"] += 1
                return {"status": "ok", "value": [{"account_id": "001", "transactions": [{"id": "x", "amount": Decimal("1.00"), "name": "x", "memo": ""}]}]}
            def writer(data):
                counts["writer"] += 1
                return {"status": "ok", "bytes": b"\xff\x80writer", "expected": model, "original_semantics": model, "written_semantics": model, "known_loss": False}
            def current_roc(binary, data, mode="json"):
                counts["roc"] += 1
                if mode != "json":
                    return mode.encode()
                if binary == "regressed-roc":
                    return {"status": "error", "message": "new regression"}
                return model
            with patch.object(review, "_oracle_uncached", primary), patch.object(review, "_secondary_uncached", secondary), patch.object(review, "_writer_uncached", writer), patch.object(review, "probe", current_roc):
                self.assertIsNone(review.check_one("old-roc", b"source"))
                self.assertIsNone(review.check_one("new-roc", b"source"))
                self.assertEqual(counts, {"primary": 3, "secondary": 1, "writer": 1, "roc": 12})
                self.assertIsNotNone(review.check_one("regressed-roc", b"source"))
                self.assertEqual(counts, {"primary": 3, "secondary": 1, "writer": 1, "roc": 13})
