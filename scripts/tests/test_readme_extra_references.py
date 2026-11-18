"""Supplementary references: test protocols/identity without builds or timings."""
from pathlib import Path
from types import SimpleNamespace
import hashlib
import json
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from scripts import readme_extra_references as extra
from scripts import readme_reference_proxy as proxy


class WorkerProtocolTests(unittest.TestCase):
    def test_asciidoc_request_has_no_internal_warmup_or_external_include_io(self):
        item = proxy.request("asciidoc", "= Café\n".encode(), 7)
        self.assertEqual(item["iterations"], 7)
        self.assertEqual(item["warmup"], 0)
        self.assertFalse(item["expand"])
        self.assertEqual(item["includes"], [])

    def test_syntax_request_uses_the_same_exact_source(self):
        data = b"module []\nx = 1\n"
        item = proxy.request("roc-syntax", data, 9)
        self.assertEqual(item["source"].encode(), data)
        self.assertEqual(item["benchmark_iterations"], 9)
        self.assertEqual(item["benchmark_warmup"], 0)
        self.assertNotIn("benchmark_iterations", proxy.request("roc-syntax", data, 0, correctness=True))

    def test_elapsed_time_is_worker_parse_time_not_ipc_time(self):
        item = {"version": 2, "id": "readme-reference", "iterations": 3, "warmup": 0,
                "successes": 3, "checksum": 42, "elapsed_ns": 123, "phase": "timing", "mode": "parse", "oracle_version": "2.0.23"}
        self.assertEqual(proxy.response("asciidoc", item, 3), {"iterations": 3, "successes": 3, "checksum": 42, "elapsed_ns": 123})
        for change in ({"warmup": 1}, {"oracle_version": "other"}, {"successes": 2}, {"elapsed_ns": True}):
            with self.assertRaises(ValueError):
                proxy.response("asciidoc", {**item, **change}, 3)

    def test_capped_or_unconsumed_syntax_runs_are_never_measurements(self):
        item = {"version": 1, "id": "readme-reference", "iterations": 3, "requested_iterations": 3,
                "warmup": 0, "successes": 3, "consumed": 3, "capped": False, "checksum": 42, "elapsed_ns": 123}
        self.assertEqual(proxy.response("roc-syntax", item, 3)["elapsed_ns"], 123)
        for change in ({"capped": True}, {"consumed": 2}, {"requested_iterations": 4}):
            with self.assertRaises(ValueError):
                proxy.response("roc-syntax", {**item, **change}, 3)

    def test_single_worker_request_and_reply_outside_timer(self):
        answer = {"id": "readme-reference", "iterations": 1}
        completed = subprocess.CompletedProcess([], 0, stdout=json.dumps(answer).encode() + b"\n", stderr=b"")
        with patch.object(proxy.subprocess, "run", return_value=completed) as run:
            self.assertEqual(proxy.worker_response("asciidoc", b"= Title", 1, worker="ruby", script=Path("worker.rb"), gem_home=Path("isolated-gems")), answer)
            call = run.call_args
            self.assertEqual(call.args[0], ["ruby", "worker.rb"])
            self.assertEqual(json.loads(call.kwargs["input"])["source"], "= Title")
            self.assertEqual(call.kwargs["env"]["GEM_HOME"], "isolated-gems")


class SupplementarySpecsTests(unittest.TestCase):
    def test_specs_only_describe_references_without_running_or_building(self):
        with patch.object(extra.bench, "reference_probe", side_effect=AssertionError("must not execute")):
            specs = extra.specs()
        self.assertEqual({item.format for item in specs}, set(extra.EXTRA_FORMATS))
        self.assertTrue(all(not item.command and item.lang != "roc" for item in specs))
        self.assertTrue(any("Borrowed" in item.notes for item in specs))

    def test_exact_namespace_roots_required(self):
        make = lambda fmt, text: extra.bench.Doc(fmt, "test", "medium", text.encode())
        with self.assertRaises(ValueError):
            extra.expected_markup(make("svg", "<g/>"))
        svg = extra.expected_markup(make("svg", '<svg xmlns="http://www.w3.org/2000/svg"><path d="M0 0"/></svg>'))
        self.assertEqual(svg[0][1], "http://www.w3.org/2000/svg")
        math = extra.expected_markup(make("mathml", '<math xmlns="http://www.w3.org/1998/Math/MathML"><mi>x</mi></math>'))
        self.assertEqual(math[0][1], "http://www.w3.org/1998/Math/MathML")

    def test_explicit_html_fixture_projection_decodes_entities_once(self):
        doc = extra.bench.Doc("html", "test", "medium", b'<div><span data-x="&amp;">&amp;lt;</span></div>')
        tree = extra.expected_markup(doc)
        self.assertEqual(tree[0][4][0][3], [["", "data-x", "&"]])
        self.assertEqual(tree[0][4][0][4], [["text", "&lt;"]])

    def test_profile_contains_only_reference_sources_and_runtime(self):
        impl = next(value for value in extra.specs() if value.lang == "rust")
        args = SimpleNamespace(work=Path("scratch"), python="provided")
        with patch.object(extra.bench, "reference_probe", return_value="fixed runtime"), patch.object(extra.bench, "git_state", side_effect=AssertionError("candidate identity must not be used")):
            result = extra.profile(impl, args, {"cpu": "fixed"})
        self.assertEqual(result["checksum_semantics"], "namespace-canonical-dom-v1")
        self.assertTrue(all(not key.startswith("package/") and not key.startswith("bench/roc/") for key in result["sources"]))
        self.assertNotIn("roc", result["runtime"])

    def test_builds_are_serial_reference_only_and_python_environment_isolated(self):
        commands = []
        with tempfile.TemporaryDirectory() as folder:
            builder = SimpleNamespace(out=Path(folder), python="/provided/python", run=lambda name, command, cwd: commands.append(command) or True)
            result = extra.build(builder, ("svg",))
        self.assertTrue(result)
        self.assertFalse(any("roc" == part for command in commands for part in command))
        self.assertTrue(any("--jobs" in command and command[command.index("--jobs") + 1] == "1" for command in commands))
        pip = next(command for command in commands if "pip" in command)
        self.assertNotEqual(pip[0], "/provided/python")
        self.assertIn("--require-hashes", pip)

    def test_reference_qualification_does_not_run_current_roc(self):
        impl = next(value for value in extra.specs() if value.format == "html")
        impl.command = ["reference-only"]
        doc = extra.bench.Doc("html", "test", "medium", b"<div>text</div>")
        tree = extra.expected_markup(doc)
        with patch.object(extra.markup, "canonical", return_value=tree) as canonical, patch.object(extra.bench, "run_once", return_value={"successes": 1, "checksum": extra.markup.model_checksum(tree)}):
            evidence = extra.qualification(impl, doc, SimpleNamespace(timeout=1), SimpleNamespace(out=Path("scratch")))
        self.assertEqual(evidence["status"], "qualified")
        self.assertNotIn("canonical", evidence)
        self.assertEqual(evidence["canonical_sha256"], hashlib.sha256(json.dumps(tree, ensure_ascii=False, separators=(",", ":")).encode()).hexdigest())
        self.assertEqual(canonical.call_args.args[0]["command"], ["reference-only"])

    def test_current_roc_dom_check_is_separate_from_cached_reference_evidence(self):
        impl = next(value for value in extra.specs() if value.format == "html")
        doc = extra.bench.Doc("html", "test", "medium", b"<div>text</div>")
        tree = extra.expected_markup(doc)
        evidence = {"canonical_sha256": hashlib.sha256(json.dumps(tree, ensure_ascii=False, separators=(",", ":")).encode()).hexdigest()}
        with tempfile.TemporaryDirectory() as folder:
            binary = Path(folder) / "bin/roc-markup-canonical"
            binary.parent.mkdir()
            binary.write_bytes(b"placeholder")
            with patch.object(extra.markup, "canonical", return_value=tree) as run:
                result = extra.fresh_roc_check(impl, doc, None, SimpleNamespace(out=Path(folder)), evidence)
                self.assertEqual(result["status"], "match")
                self.assertEqual(run.call_args.args[0]["command"], [str(binary), "html-fragment-full"])
            with patch.object(extra.markup, "canonical", return_value=[]):
                with self.assertRaises(extra.bench.ReferenceUnavailable):
                    extra.fresh_roc_check(impl, doc, None, SimpleNamespace(out=Path(folder)), evidence)
        self.assertEqual(set(evidence), {"canonical_sha256"})

    def test_syntax_profile_identifies_pinned_reference_not_current_roc(self):
        impl = next(value for value in extra.specs() if value.format == "roc-syntax")
        with patch.object(extra.bench, "reference_probe", return_value="0.17.0"):
            result = extra.profile(impl, SimpleNamespace(work=Path("scratch"), python=None), {"cpu": "fixed"})
        self.assertEqual(result["runtime"]["reference_revision"], extra.syntax_build.ROC_REVISION)
        self.assertEqual(result["runtime"]["optimize"], "ReleaseFast")
        self.assertNotIn("compiler_sha256", result)

    def test_asciidoc_evidence_checks_reference_title_and_section_order(self):
        impl = next(value for value in extra.specs() if value.format == "asciidoc")
        impl.command = ["reference-only"]
        doc = extra.bench.Doc("asciidoc", "test", "medium", b"= Title\n\n== First\n\nWords\n\n== Second\n")
        response = {"oracle_version": "2.0.23", "oracle_code_sha256": "a" * 64,
                    "projection": {"title": "Title", "blocks": [{"title": "First"}, {"title": "Second"}]}, "operation_checksum": 8}
        with patch.object(extra.proxy, "worker_response", return_value=response):
            evidence = extra.qualification(impl, doc, None, SimpleNamespace(out=Path("scratch")))
            self.assertEqual(evidence["expected_checksum"], 8)
            self.assertNotIn("projection", evidence)
            self.assertEqual(evidence["title"], "Title")
            self.assertEqual(evidence["sections"], ["First", "Second"])
        with patch.object(extra.proxy, "worker_response", return_value={**response, "projection": {"title": "wrong", "blocks": []}}):
            with self.assertRaises(extra.bench.ReferenceUnavailable):
                extra.qualification(impl, doc, None, SimpleNamespace(out=Path("scratch")))
