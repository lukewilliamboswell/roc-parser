"""Small mocked cache tests: no compiler, library build, or benchmark runs."""

import copy
from contextlib import ExitStack, redirect_stderr
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS))
import bench
from _reference_cache import ReferenceCache, ReferenceUnavailable, digest, validate_measurement


PROFILE = {"runtime": {"compiler": "reference 1", "libraries": {"csv": "1"}},
           "sources": {"comparator": "source-sha"}, "machine": {"cpu": "test", "affinity": [0]},
           "build_recipe_sha256": "recipe", "measurement_protocol_sha256": "protocol"}
DOC = bench.Doc("csv", "fixture", "small", b"a,b\n")


def measurement(impl, doc, args):
    iterations = 2
    def batch(elapsed, count=iterations):
        return {"iterations": count, "elapsed_ns": elapsed, "successes": count,
                "checksum": 5 * count, "peak_rss_bytes": 4096}
    batches = [batch(200 + index * 20) for index in range(args.repetitions)]
    record = {"impl": impl.id, "lang": impl.lang, "library": impl.name, "doc": doc.id,
              "kind": doc.kind, "bytes": len(doc.data), "workload": impl.workload,
              "iterations": iterations, "accepted": True, "checksum": 10, "peak_rss_bytes": 4096,
              "batch_elapsed_ns": [row["elapsed_ns"] for row in batches],
              "receipts": {"command": ["original-reference-driver"], "measured_at": "2026-11-17T00:00:00+00:00",
                           "calibration": batch(100, 1), "warmup": batch(220), "batches": batches}}
    record.update(bench.summarize([row["elapsed_ns"] / iterations for row in batches], len(doc.data)))
    return record


def provenance(record):
    return {"report": "/original/refresh/results.json", "git": {"commit": "original-reference-revision"},
            "measured_at": record["receipts"]["measured_at"]}


class BenchmarkReferenceCache(unittest.TestCase):
    def arguments(self, folder, *extra):
        return ["--quick", "--formats", "csv", "--langs", "roc,rust,go,python", "--output-dir", str(folder), *extra]

    def seed(self, folder, argv):
        args = bench.parse_args(argv)
        cache = ReferenceCache(folder / "reference-cache")
        for impl in bench.reference_specs(args.langs, args.formats):
            row = measurement(impl, DOC, args)
            cache.save(bench.reference_identity(impl, DOC, args, PROFILE), row, provenance(row))

    def execute(self, folder, extra=(), refresh=False):
        calls = []
        roc = bench.Impl("roc", "roc-parser", "csv", ["fake-roc"])
        with ExitStack() as stack:
            stack.enter_context(redirect_stderr(io.StringIO()))
            stack.enter_context(patch.object(bench, "build_corpus", return_value=[DOC]))
            stack.enter_context(patch.object(bench, "reference_profile", return_value=copy.deepcopy(PROFILE)))
            stack.enter_context(patch.object(bench, "reference_machine", return_value={"cpu": "test"}))
            stack.enter_context(patch.object(bench, "machine", return_value={"description": "test", "os": "test"}))
            stack.enter_context(patch.object(bench, "environment", return_value={"blueprint": False}))
            stack.enter_context(patch.object(bench, "git_state", return_value={"commit": "new-roc-revision", "dirty": True}))
            stack.enter_context(patch.object(bench, "_install_wait_hook"))
            roc_build = stack.enter_context(patch.object(bench.Builder, "roc_impls", return_value=[roc]))
            reference_builds = []
            for lang in ("rust", "go", "python"):
                references = bench.reference_specs((lang,), ("csv",))
                mock = stack.enter_context(patch.object(bench.Builder, f"{lang}_impls", return_value=references))
                if not refresh:
                    mock.side_effect = AssertionError("reference builder called without explicit refresh")
                reference_builds.append(mock)
            def measured(impl, doc, args):
                calls.append(impl.lang)
                if impl.lang != "roc" and not refresh:
                    raise AssertionError("reference measurement called on cache reuse")
                return measurement(impl, doc, args)
            stack.enter_context(patch.object(bench, "measure", side_effect=measured))
            argv = self.arguments(folder, *extra, *(["--refresh-references"] if refresh else []))
            self.assertEqual(bench.main(argv), 0)
            roc_build.assert_called_once()
            for mock in reference_builds:
                if refresh:
                    mock.assert_called_once()
                else:
                    mock.assert_not_called()
        label = bench.parse_args(argv).label
        return calls, json.loads((folder / label / "results.json").read_text())

    def test_hits_never_build_or_measure_references_but_measure_roc(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp)
            self.seed(folder, self.arguments(folder))
            calls, report = self.execute(folder, extra=("--label", "changed-roc"))
        self.assertEqual(calls, ["roc"])
        references = [row for row in report["results"] if row["lang"] != "roc"]
        self.assertEqual(len(references), 6)
        self.assertTrue(all(row["reference_cache"]["status"] == "hit" for row in references))
        self.assertTrue(all(row["reference_cache"]["provenance"]["git"]["commit"] == "original-reference-revision" for row in references))
        self.assertTrue(all(row["receipts"]["command"] == ["original-reference-driver"] for row in references))

    def test_cold_cache_is_unavailable_without_reference_work(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls, report = self.execute(Path(tmp))
        self.assertEqual(calls, ["roc"])
        self.assertTrue(all(row["status"] == "ReferenceUnavailable" for row in report["results"] if row["lang"] != "roc"))

    def test_explicit_refresh_writes_cache_then_reuses_without_work(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp)
            calls, fresh = self.execute(folder, refresh=True)
            self.assertEqual(len(calls), 7)
            self.assertEqual(len(list((folder / "reference-cache").glob("*.json"))), 6)
            self.assertTrue(all(row["reference_cache"]["status"] == "refreshed" for row in fresh["results"] if row["lang"] != "roc"))
            reused, report = self.execute(folder, extra=("--label", "after-roc-change"))
        self.assertEqual(reused, ["roc"])
        self.assertTrue(all(row["reference_cache"]["status"] == "hit" for row in report["results"] if row["lang"] != "roc"))

    def test_identity_changes_invalidate_without_fallback(self):
        args = bench.parse_args(["--quick", "--formats", "csv"])
        impl = bench.reference_specs(("go",), ("csv",))[0]
        identity = bench.reference_identity(impl, DOC, args, PROFILE)
        changes = [("profile", "runtime"), ("profile", "sources"), ("profile", "machine"),
                   ("profile", "build_recipe_sha256"), ("profile", "measurement_protocol_sha256"),
                   ("implementation", "workload"), ("implementation", "ownership"),
                   ("document", "sha256"), ("document", "bytes"),
                   *[("settings", key) for key in ("quick", "repetitions", "target_ms", "timeout", "max_iterations")]]
        with tempfile.TemporaryDirectory() as tmp:
            cache = ReferenceCache(Path(tmp))
            row = measurement(impl, DOC, args)
            cache.save(identity, row, provenance(row))
            for section, key in changes:
                changed = copy.deepcopy(identity)
                changed[section][key] = "changed"
                with self.subTest(section=section, key=key), self.assertRaises(ReferenceUnavailable):
                    cache.load(changed)

    def test_corrupt_stale_and_unqualified_entries_are_rejected(self):
        args = bench.parse_args(["--quick"])
        impl = bench.reference_specs(("go",), ("csv",))[0]
        identity = bench.reference_identity(impl, DOC, args, PROFILE)
        row = measurement(impl, DOC, args)
        with tempfile.TemporaryDirectory() as tmp:
            cache = ReferenceCache(Path(tmp))
            cache.save(identity, row, provenance(row))
            path = cache.path(identity)
            original = path.read_text()
            for corrupt in ("not json", "[]", original.replace('"checksum": 10', '"checksum": 11')):
                path.write_text(corrupt)
                with self.assertRaises(ReferenceUnavailable):
                    cache.load(identity)
            payload = json.loads(original)
            payload["schema"] = 0
            payload["payload_sha256"] = digest({key: payload[key] for key in ("schema", "key", "identity", "record", "provenance")})
            path.write_text(json.dumps(payload))
            with self.assertRaises(ReferenceUnavailable):
                cache.load(identity)
            unqualified = copy.deepcopy(row)
            del unqualified["receipts"]
            with self.assertRaises(ReferenceUnavailable):
                cache.save(identity, unqualified, provenance(row))

    def test_calibration_checksum_must_agree_with_measured_iterations(self):
        args = bench.parse_args(["--quick"])
        impl = bench.reference_specs(("go",), ("csv",))[0]
        row = measurement(impl, DOC, args)
        row["receipts"]["calibration"]["checksum"] += 1
        with self.assertRaisesRegex(ReferenceUnavailable, "calibration checksum"):
            validate_measurement(row, bench.reference_identity(impl, DOC, args, PROFILE))

    def test_roc_source_and_revision_are_excluded_but_reference_code_and_pins_are_not(self):
        args = bench.parse_args(["--quick"])
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in ("scripts/bench_python.py", "bench/compare/python/requirements.txt", "package/CSV.roc", "bench/roc/csv.roc"):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("original")
            with patch.object(bench, "ROOT", root), patch.object(bench, "BENCH", root / "bench"), patch.object(bench, "reference_runtime", return_value={"python": "fake"}):
                before = bench.reference_profile("python", args, {"cpu": "test"})
                (root / "package/CSV.roc").write_text("changed Roc parser")
                (root / "bench/roc/csv.roc").write_text("changed Roc driver")
                self.assertEqual(before, bench.reference_profile("python", args, {"cpu": "test"}))
                (root / "scripts/bench_python.py").write_text("changed reference comparator")
                code_changed = bench.reference_profile("python", args, {"cpu": "test"})
                self.assertNotEqual(before, code_changed)
                (root / "bench/compare/python/requirements.txt").write_text("new pins")
                self.assertNotEqual(code_changed, bench.reference_profile("python", args, {"cpu": "test"}))

    def test_go_identity_and_explicit_build_cannot_download_toolchains(self):
        def probe(command, cwd=bench.ROOT, env=None):
            self.assertEqual(env["GOTOOLCHAIN"], "local")
            return "{}" if "env" in command else "go version fake"
        with patch.object(bench, "reference_probe", side_effect=probe), patch.object(bench, "go_mod_versions", return_value={}):
            bench.reference_runtime("go", None)
            builder = bench.Builder(Path("/not-created"), "fake-roc", None, lambda _: None)
            with patch.object(bench.shutil, "which", return_value="fake-go"), patch.object(builder, "run", return_value=True) as build:
                builder.go_impls(("csv",))
            self.assertEqual(build.call_args.kwargs["env"]["GOTOOLCHAIN"], "local")

    def test_reference_source_walk_prunes_build_artifacts(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "bench/compare/rust/src"
            source.mkdir(parents=True)
            (source / "main.rs").write_text("source")
            (source.parent / "Cargo.lock").write_text("pins")
            def walk(folder):
                directories = ["src", "target", ".git"]
                yield str(folder), directories, ["Cargo.lock"]
                self.assertEqual(directories, ["src"])
                yield str(source), [], ["main.rs"]
            with patch.object(bench, "ROOT", root), patch.object(bench, "BENCH", root / "bench"), patch.object(bench.os, "walk", side_effect=walk):
                self.assertEqual(len(bench.reference_sources("rust")), 2)

    def test_rust_identity_probes_disable_automatic_toolchain_installation(self):
        def probe(command, cwd=bench.ROOT, env=None):
            self.assertEqual(env["RUSTUP_AUTO_INSTALL"], "0")
            return "installed rust toolchain"
        with patch.object(bench, "reference_probe", side_effect=probe), patch.object(bench, "cargo_lock_versions", return_value={}):
            bench.reference_runtime("rust", None)


if __name__ == "__main__":
    unittest.main()
