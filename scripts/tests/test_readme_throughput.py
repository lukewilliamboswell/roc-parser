"""README throughput must come from successful, stable full-document parses."""
import copy
from contextlib import ExitStack, redirect_stdout
import io
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

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

    def test_runner_uses_identical_pinned_inputs_and_preserves_reference_results(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp)
            binary = folder / "fake-roc"
            binary.write_bytes(b"not executable; mocked measurements")
            document = throughput.bench.Doc("csv", "same-input", "medium", b"a,b\n")
            driver = throughput.bench.Impl("roc", "roc-parser", "csv", [str(binary)])
            fresh_roc = {"doc": document.id, "median_ns": 100, "mb_per_s": 40, "spread_pct": 1}
            reference = {"impl": "rust/csv", "median_ns": 50, "mb_per_s": 80, "ownership": "owned fields",
                         "semantic_note": "result checksum agrees", "reference_cache": {"status": "hit"}}
            current_affinity = {7, 8}
            def set_affinity(pid, values):
                current_affinity.clear()
                current_affinity.update(values)
            fake_references = Mock()
            fake_references.cache.folder = folder / "shared-cache"
            def collect(records, metadata):
                self.assertEqual(current_affinity, {7})
                self.assertEqual(records, [fresh_roc])
                self.assertEqual(metadata["inputs"][0]["sha256"], document.sha256)
                return [reference], {"csv": reference}
            fake_references.collect.side_effect = collect
            with ExitStack() as stack:
                stack.enter_context(redirect_stdout(io.StringIO()))
                stack.enter_context(patch.object(throughput, "documents", return_value=[document]))
                stack.enter_context(patch.object(throughput, "source_identity", return_value="roc-source"))
                stack.enter_context(patch.object(throughput, "build_drivers", return_value={"csv": driver}))
                stack.enter_context(patch.object(throughput, "qualified_measure", return_value=fresh_roc))
                stack.enter_context(patch.object(throughput.bench, "_install_wait_hook"))
                stack.enter_context(patch.object(throughput.bench, "machine", return_value={"cpu": "fake"}))
                stack.enter_context(patch.object(throughput.bench, "git_state", return_value={"commit": "new-roc"}))
                builder = stack.enter_context(patch.object(throughput.bench, "Builder"))
                builder.return_value.versions = {"roc": "fake-nightly"}
                reference_class = stack.enter_context(patch.object(throughput.readme_references, "ReferenceRun", return_value=fake_references))
                stack.enter_context(patch.object(throughput.os, "sched_getaffinity", side_effect=lambda pid: set(current_affinity)))
                stack.enter_context(patch.object(throughput.os, "sched_setaffinity", side_effect=set_affinity))
                self.assertEqual(throughput.main(["--cpu", "7", "--work", str(folder), "--python", "existing-python",
                                                 "--reference-cache-dir", str(folder / "shared-cache")]), 0)
                arguments = reference_class.call_args.args[1]
                self.assertFalse(arguments.refresh_references)
                self.assertEqual(arguments.python, "existing-python")
                self.assertEqual(reference_class.call_args.args[2], [document])
                self.assertEqual(builder.call_args.args[2], "existing-python")
            self.assertEqual(current_affinity, {7, 8})
            report = json.loads((folder / "results.json").read_text())
            self.assertEqual(report["reference_results"], [reference])
            self.assertEqual(report["fastest_references"], {"csv": reference})
