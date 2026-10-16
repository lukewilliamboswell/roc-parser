from __future__ import annotations

import json
import hashlib
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from scripts import check_roc_syntax as check


class ValidationTests(unittest.TestCase):
    def test_source_digest_tracks_contents_and_names(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in ["package/A.roc", "bench/roc/roc-syntax.roc", "bench/roc/roc-syntax-alloc.roc", ".roc-version"]:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("initial")
            before = check.source_identity(root)
            (root / "package/A.roc").write_text("changed")
            changed = check.source_identity(root)
            self.assertNotEqual(before, changed)
            (root / "package/A.roc").rename(root / "package/B.roc")
            self.assertNotEqual(changed, check.source_identity(root))

    def test_wrong_compiler_stops_and_preserves_evidence(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp)
            response = subprocess.CompletedProcess(["roc", "version"], 0, "Roc compiler version wrong\n", "")
            with patch.object(check.subprocess, "run", return_value=response) as run:
                self.assertEqual(check.main(["--output", tmp]), 1)
            self.assertEqual(run.call_count, 1)
            record = json.loads((output / "validation.json").read_text())
            self.assertEqual(record["status"], "failed")
            self.assertIn("pinned executable", record["error"])
            self.assertEqual(record["steps"][0]["name"], "compiler-version")

    def test_command_failure_keeps_stdout_stderr_and_exit_status(self):
        with tempfile.TemporaryDirectory() as tmp:
            version = "Roc compiler version " + (check.ROOT / ".roc-version").read_text().strip()
            replies = [subprocess.CompletedProcess([], 0, version, ""),
                       subprocess.CompletedProcess([], 2, "check output", "check error")]
            with patch.object(check.subprocess, "run", side_effect=replies):
                self.assertEqual(check.main(["--output", tmp]), 1)
            output = Path(tmp)
            record = json.loads((output / "validation.json").read_text())
            self.assertEqual(record["steps"][-1]["returncode"], 2)
            self.assertEqual((output / "package-check.stdout").read_text(), "check output")
            self.assertEqual((output / "package-check.stderr").read_text(), "check error")
            self.assertEqual(record["status"], "failed")

    def test_full_corpus_failure_prevents_fuzz_and_success(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp)
            oracle = output / 'oracle/install/bin/oracle'
            oracle.parent.mkdir(parents=True)
            oracle.write_bytes(b'fresh oracle')
            oracle.with_suffix('.json').write_text(json.dumps({'binary_sha256': hashlib.sha256(oracle.read_bytes()).hexdigest(), 'source_git': str(output / 'compiler.git')}))
            version = 'Roc compiler version ' + (check.ROOT / '.roc-version').read_text().strip()
            calls = []
            def command(args, **kwargs):
                calls.append(args)
                if args[1:] == ['version']:
                    return subprocess.CompletedProcess(args, 0, version, '')
                if 'scripts/roc_syntax/build_oracle.py' in args:
                    return subprocess.CompletedProcess(args, 0, str(oracle), '')
                if 'scripts/check_roc_syntax_corpus.py' in args:
                    return subprocess.CompletedProcess(args, 1, '{"missing_grammar":1}', 'corpus rejection')
                if 'build' in args:
                    for item in args:
                        if item.startswith('--output='):
                            Path(item.removeprefix('--output=')).write_bytes(b'fresh probe')
                return subprocess.CompletedProcess(args, 0, '', '')
            with patch.object(check.subprocess, 'run', side_effect=command):
                self.assertEqual(check.main(['--output', tmp, '--roc-source', str(output / 'compiler.git')]), 1)
            record = json.loads((output / 'validation.json').read_text())
            self.assertEqual(record['status'], 'failed')
            self.assertIn('type-differential', [step['name'] for step in record['steps']])
            self.assertIn('platform-differential', [step['name'] for step in record['steps']])
            self.assertEqual(record['corpus_repository'], str(output / 'compiler.git'))
            self.assertEqual(record['steps'][-1]['name'], 'full-corpus')
            self.assertFalse(any('scripts/run_fuzz.py' in call for call in calls))
            self.assertEqual((output / 'full-corpus.stderr').read_text(), 'corpus rejection')
            typed = next(call for call in calls if 'scripts/roc_syntax/type-fixtures.json' in call)
            self.assertEqual(typed[typed.index('--generated') + 1], '0')


if __name__ == "__main__":
    unittest.main()
