"""README throughput must come from successful, stable full-document parses."""
import copy
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from scripts import readme_throughput as throughput


class ReadmeThroughputTests(unittest.TestCase):
    def test_workloads_cover_each_concrete_readme_format_once(self):
        documents = throughput.documents()
        expected = {"csv", "qif", "ofx", "yaml", "xml", "html", "svg", "mathml", "json",
                    "markdown", "asciidoc", "http", "roc-syntax"}
        self.assertEqual({document.format for document in documents}, expected)
        self.assertEqual(len(documents), len(expected))
        self.assertTrue(all(65536 <= len(document.data) < 67000 for document in documents if document.format != "http"))
        request = next(document for document in documents if document.format == "http")
        self.assertEqual(len(request.data), 555)
        self.assertTrue(request.data.endswith(b"\r\n\r\n"))
        self.assertEqual([document.sha256 for document in documents],
                         [document.sha256 for document in throughput.documents()])

    def test_rejected_failed_noisy_or_inconsistent_measurements_are_not_publishable(self):
        document = throughput.bench.Doc("csv", "test", "medium", b"a,b\n")
        implementation = throughput.bench.Impl("roc", "roc-parser", "csv", ["unused"])
        batch = {"iterations": 2, "successes": 2, "checksum": 10}
        original = {"accepted": True, "iterations": 2, "checksum": 10, "spread_pct": 1,
                    "receipts": {"calibration": {"iterations": 1, "successes": 1, "checksum": 5},
                                 "warmup": batch, "batches": [batch]}}
        arguments = SimpleNamespace(max_spread_pct=25)
        with patch.object(throughput.bench, "measure", return_value=original):
            self.assertEqual(throughput.qualified_measure(implementation, document, arguments)["input_sha256"], document.sha256)
        for change in (lambda row: row.update(accepted=False),
                       lambda row: row.update(error="build failed"),
                       lambda row: row.update(spread_pct=26),
                       lambda row: row["receipts"]["calibration"].update(checksum=6),
                       lambda row: row["receipts"]["warmup"].update(successes=1)):
            row = copy.deepcopy(original)
            change(row)
            with self.subTest(change=change), patch.object(throughput.bench, "measure", return_value=row):
                with self.assertRaises(RuntimeError):
                    throughput.qualified_measure(implementation, document, arguments)
