from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import unittest
import tempfile
import subprocess
import sys
from unittest.mock import patch

from scripts import import_roc_syntax_snapshots, review_roc_syntax

ROOT = Path(__file__).resolve().parents[2]


class SyntaxPinTests(unittest.TestCase):
    def test_fixture_importer_supervisor_and_oracle_share_reference_pin(self):
        path = ROOT / "scripts/roc_syntax/build_oracle.py"
        spec = importlib.util.spec_from_file_location("pin_checked_build_oracle", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        fixture = json.loads((ROOT / "scripts/roc_syntax/fixtures.json").read_text())
        self.assertEqual(module.ROC_REVISION, fixture["reference_commit"])
        for name in ['type-fixtures.json', 'platform-fixtures.json', 'grammar-fixtures.json']:
            extra = json.loads((ROOT / 'scripts/roc_syntax' / name).read_text())
            self.assertEqual(module.ROC_REVISION, extra['reference_commit'], name)
        self.assertEqual(review_roc_syntax.REFERENCE_COMMIT, module.ROC_REVISION)
        self.assertEqual(import_roc_syntax_snapshots.REFERENCE_COMMIT, module.ROC_REVISION)
        self.assertEqual(len(module.ROC_REVISION), 40)
        self.assertEqual(fixture["version"], review_roc_syntax.PROTOCOL_VERSION)

    def test_ci_uses_the_oracle_toolchain_version(self):
        path = ROOT / "scripts/roc_syntax/build_oracle.py"
        spec = importlib.util.spec_from_file_location("ci_checked_build_oracle", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        # Blueprint downloads Zig for the environment the syntax checks run in.
        blueprint = (ROOT / "Blueprint.roc").read_text()
        self.assertIn(f"ziglang.org/download/{module.ZIG_VERSION}", blueprint)
        self.assertIn(f"zig-x86_64-linux-{module.ZIG_VERSION}.tar.xz", blueprint)
        self.assertIn('Task("roc-syntax", [Use("oracles"), Run(["python3", "scripts/check_roc_syntax.py"])])', blueprint)
        self.assertIn('Task("check", [Use("oracles")', blueprint)
        workflow = (ROOT / ".github/workflows/fuzz.yml").read_text()
        job = workflow.split("  roc-syntax:\n", 1)[1].split("  asciidoc:\n", 1)[0]
        self.assertIn("blueprint run roc-syntax -- ", job)

    def test_old_sibling_falls_back_to_owned_cache_without_fetching_sibling(self):
        path = ROOT / 'scripts/roc_syntax/build_oracle.py'
        spec = importlib.util.spec_from_file_location('source_checked_build_oracle', path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'parser'
            sibling = Path(tmp) / 'roc'
            (sibling / '.git').mkdir(parents=True)
            for available, expected in [(False, None), (True, sibling)]:
                with patch.object(module, 'has_pinned_revision', return_value=available) as lookup:
                    self.assertEqual(module.select_git_source(None, root), expected)
                    lookup.assert_called_once_with(sibling)
            explicit = Path(tmp) / 'old-objects.git'
            with patch.object(module, 'has_pinned_revision', return_value=False):
                with self.assertRaisesRegex(ValueError, 'does not contain pinned revision'):
                    module.select_git_source(explicit, root)
            missing = Path(tmp) / 'no-sibling' / 'parser'
            with patch.object(module, 'has_pinned_revision') as lookup:
                self.assertIsNone(module.select_git_source(None, missing))
                lookup.assert_not_called()

    def test_zig17_build_uses_isolated_cache_environment_and_records_pin(self):
        path = ROOT / 'scripts/roc_syntax/build_oracle.py'
        spec = importlib.util.spec_from_file_location('cache_checked_build_oracle', path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp).resolve()
            binary = cache / 'install/bin/roc-syntax-oracle'
            calls = []
            def command(args, **kwargs):
                calls.append((args, kwargs))
                if args[-1] == 'version':
                    return subprocess.CompletedProcess(args, 0, module.ZIG_VERSION + '\n', '')
                binary.parent.mkdir(parents=True)
                binary.write_bytes(b'newly built oracle')
                self.assertNotIn('--global-cache-dir', args)
                self.assertEqual(kwargs['env']['ZIG_GLOBAL_CACHE_DIR'], str(cache / 'zig-global-cache'))
                return subprocess.CompletedProcess(args, 0, '', '')
            with patch.object(module, 'run', side_effect=command), patch.object(module, 'source_tree', return_value=cache / 'src') as source, patch.object(module, 'select_git_source', return_value=None), patch.object(sys, 'argv', ['build_oracle.py', '--cache-dir', str(cache)]):
                module.main()
            receipt = json.loads(binary.with_suffix('.json').read_text())
            self.assertEqual(receipt['compiler_revision'], import_roc_syntax_snapshots.REFERENCE_COMMIT)
            self.assertEqual(receipt['zig_version'], '0.17.0')
            self.assertEqual(receipt['source_git'], str(cache / 'upstream.git'))
            source.assert_called_once_with(cache, None)
            self.assertEqual(len(calls), 2)


if __name__ == "__main__":
    unittest.main()
