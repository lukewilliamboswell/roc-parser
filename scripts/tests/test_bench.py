import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("bench", SCRIPTS / "bench.py")
bench = importlib.util.module_from_spec(spec)
sys.modules["bench"] = bench
spec.loader.exec_module(bench)


def fake_driver(folder: Path, body: str) -> list[str]:
    """A comparator stand-in speaking the benchmark protocol."""
    script = folder / "driver.py"
    script.write_text("import json, sys\n" + body)
    return [sys.executable, str(script)]


class Corpus(unittest.TestCase):
    def test_corpus_is_deterministic(self):
        first = [(d.id, d.sha256) for d in bench.build_corpus()]
        second = [(d.id, d.sha256) for d in bench.build_corpus()]
        self.assertEqual(first, second)

    def test_every_format_has_every_kind(self):
        docs = bench.build_corpus()
        for fmt in bench.FORMATS:
            kinds = {d.kind for d in docs if d.format == fmt}
            self.assertEqual(kinds, {"sample", "small", "medium", "large", "pathological"}, fmt)

    def test_generated_sizes_reach_their_targets(self):
        for doc in bench.build_corpus():
            if doc.kind in bench.SIZES:
                self.assertGreaterEqual(len(doc.data), bench.SIZES[doc.kind], doc.id)
                self.assertLess(len(doc.data), bench.SIZES[doc.kind] * 1.1, doc.id)

    def test_quick_corpus_is_a_small_subset(self):
        quick = bench.build_corpus(quick=True)
        full = {d.id: d.sha256 for d in bench.build_corpus()}
        self.assertTrue(all(full[d.id] == d.sha256 for d in quick))
        self.assertFalse(any(d.kind in ("medium", "large") for d in quick))
        self.assertEqual(sum(d.kind == "pathological" for d in quick), len(bench.FORMATS))

    def test_ids_are_unique_and_documents_are_utf8(self):
        docs = bench.build_corpus()
        self.assertEqual(len({d.id for d in docs}), len(docs))
        for doc in docs:
            doc.data.decode("utf-8")

    def test_http_framing_is_consistent(self):
        response = bench.HTTP_SAMPLES["sample-api-response"].encode()
        head, body = response.split(b"\r\n\r\n", 1)
        self.assertIn(f"Content-Length: {len(body)}".encode(), head)
        chunked = next(d for d in bench.build_corpus() if d.id == "http/chunked-small").data
        self.assertTrue(chunked.endswith(b"0\r\n\r\n"))

    def test_yaml_nesting_stays_under_the_parser_limit(self):
        patho = bench.pathological()["yaml"]
        self.assertEqual(patho["nested-flow-99"].count("["), 99)


class Measurement(unittest.TestCase):
    def test_calibrate_targets_batch_duration(self):
        self.assertEqual(bench.calibrate(1_000_000, 100_000_000, 10_000), 100)
        self.assertEqual(bench.calibrate(10, 100_000_000, 500), 500)
        self.assertEqual(bench.calibrate(10**10, 100_000_000, 500), 1)

    def test_summarize_reports_median_and_throughput(self):
        stats = bench.summarize([100.0, 200.0, 300.0], 1000)
        self.assertEqual(stats["median_ns"], 200.0)
        self.assertAlmostEqual(stats["mb_per_s"], 5000.0)
        self.assertAlmostEqual(stats["spread_pct"], 100.0)

    def test_run_once_passes_stdin_and_iterations(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = fake_driver(Path(tmp), (
                "n = int(sys.argv[-1]); data = sys.stdin.buffer.read()\n"
                "print(json.dumps({'iterations': n, 'elapsed_ns': len(data), 'successes': n, 'checksum': 0}))\n"))
            record = bench.run_once(command, b"abcd", 3, 30)
        self.assertEqual(record["iterations"], 3)
        self.assertEqual(record["elapsed_ns"], 4)

    def test_run_once_rejects_wrong_iteration_count(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = fake_driver(Path(tmp), "print(json.dumps({'iterations': 1, 'elapsed_ns': 5}))\n")
            with self.assertRaises(bench.RunError):
                bench.run_once(command, b"", 2, 30)

    def test_run_once_reports_crashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = fake_driver(Path(tmp), "sys.exit(3)\n")
            with self.assertRaisesRegex(bench.RunError, "exit 3"):
                bench.run_once(command, b"", 1, 30)

    def test_measure_records_errors_instead_of_raising(self):
        with tempfile.TemporaryDirectory() as tmp:
            impl = bench.Impl("go", "x", "csv", fake_driver(Path(tmp), "sys.exit(1)\n"))
            doc = bench.Doc("csv", "d", "small", b"a\n")
            args = bench.parse_args(["--quick"])
            record = bench.measure(impl, doc, args)
        self.assertIn("error", record)


class Reports(unittest.TestCase):
    def result(self, impl, doc, ns, kind="small", accepted=True):
        return {"impl": impl, "doc": doc, "kind": kind, "median_ns": ns, "accepted": accepted,
                "mb_per_s": 1000 / ns, "lang": impl.split("/")[0]}

    def test_comparison_skips_pathological_and_rejected_documents(self):
        results = [
            self.result("roc/roc-parser", "csv/a", 400), self.result("go/encoding-csv", "csv/a", 100),
            self.result("roc/roc-parser", "csv/b", 100), self.result("go/encoding-csv", "csv/b", 100),
            self.result("roc/roc-parser", "csv/p", 1, kind="pathological"),
            self.result("go/encoding-csv", "csv/p", 1000, kind="pathological"),
            self.result("roc/roc-parser", "csv/r", 1, accepted=False), self.result("go/encoding-csv", "csv/r", 9),
        ]
        rows = bench.comparison(results)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["documents"], 2)
        self.assertAlmostEqual(rows[0]["speedup_vs_roc"], 2.0)

    def test_ratio_wording(self):
        self.assertEqual(bench.fmt_ratio(0.5), "2x slower")
        self.assertEqual(bench.fmt_ratio(3.0), "3x faster")

    def test_markdown_report_renders(self):
        report = {"metadata": {"label": "t", "date": "d", "machine": {"description": "m", "os": "o"},
                               "versions": {"roc": "r"}, "git": {"commit": "c", "dirty": False}, "quick": True,
                               "repetitions": 3, "target_ms": 20, "skipped": {"go": "go not found"}},
                  "results": [dict(self.result("roc/roc-parser", "csv/a", 400), bytes=10, spread_pct=1.0,
                                   peak_rss_bytes=2**20, allocation_calls=7),
                              {"impl": "go/encoding-csv", "doc": "csv/a", "bytes": 10, "error": "boom"}],
                  "comparison": []}
        text = bench.markdown_report(report)
        self.assertIn("7 allocs", text)
        self.assertIn("error: boom", text)
        self.assertIn("Skipped: go", text)
        json.dumps(report)

    def test_corpus_only_writes_manifest_with_hashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual(bench.main(["--quick", "--corpus-only", "--output-dir", tmp, "--formats", "csv"]), 0)
            manifest = json.loads((Path(tmp) / "quick" / "corpus" / "manifest.json").read_text())
        self.assertTrue(manifest and all(len(entry["sha256"]) == 64 for entry in manifest))
        self.assertTrue(all(entry["id"].startswith("csv/") for entry in manifest))

    def test_every_format_has_a_roc_driver_and_comparators(self):
        for fmt in bench.FORMATS:
            self.assertTrue((bench.BENCH / "roc" / f"{fmt}.roc").exists(), fmt)
            for lang in bench.COMPARATORS:
                self.assertTrue(bench.COMPARATORS[lang][fmt], (lang, fmt))


if __name__ == "__main__":
    unittest.main()
