import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / 'asciidoc'
spec = importlib.util.spec_from_file_location('asciidoc_models', SCRIPT / 'models.py')
models = importlib.util.module_from_spec(spec)
spec.loader.exec_module(models)


class AsciiDocModels(unittest.TestCase):
    def test_independent_nested_projection_and_rendering(self):
        model = {'title': 'Notes', 'children': [
            {'kind': 'paragraph', 'text': 'Before'},
            {'kind': 'section', 'level': 1, 'title': 'Topic', 'children': [
                {'kind': 'listing', 'text': 'code\n  more'},
                {'kind': 'section', 'level': 2, 'title': 'Detail', 'children': [
                    {'kind': 'paragraph', 'text': 'After'}]}]}]}
        self.assertEqual(models.render(model), '= Notes\n\nBefore\n\n== Topic\n\n----\ncode\n  more\n----\n\n=== Detail\n\nAfter\n')
        self.assertEqual(models.project(model), {'title': 'Notes', 'blocks': [
            {'context': 'paragraph', 'level': 0, 'title': '', 'source': 'Before', 'blocks': []},
            {'context': 'section', 'level': 1, 'title': 'Topic', 'source': '', 'blocks': [
                {'context': 'listing', 'level': 0, 'title': '', 'source': 'code\n  more', 'blocks': []},
                {'context': 'section', 'level': 2, 'title': 'Detail', 'source': '', 'blocks': [
                    {'context': 'paragraph', 'level': 0, 'title': '', 'source': 'After', 'blocks': []}]}]}]})

    def test_generation_determinism_domains_and_budgets(self):
        cases = models.generate_cases(73, 200)
        self.assertEqual(cases, models.generate_cases(73, 200))
        self.assertNotEqual(cases, models.generate_cases(74, 200))
        self.assertEqual(len({case['id'] for case in cases}), 200)
        contexts = set()
        def walk(blocks):
            for b in blocks:
                contexts.add(b['context'])
                self.assertLessEqual(b['level'], 3)
                walk(b['blocks'])
        for case in cases:
            walk(case['expected']['blocks'])
            self.assertLess(len(case['source']), 30000)
        self.assertEqual(contexts, {'paragraph', 'listing', 'literal', 'section', 'thematic_break'})
        self.assertTrue(any('\r\n' in c['source'] for c in cases))
        self.assertEqual(models.generate_cases(0, 0), [])
        for count in (-1, 100001, 0.5):
            with self.assertRaises(ValueError):
                models.generate_cases(0, count)

    def test_corpus_hashes_deduplication_order_and_link_exclusion(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'docs').mkdir()
            (root / 'a.adoc').write_bytes(b'cafe\r\n')
            (root / 'docs' / 'duplicate.adoc').write_bytes(b'cafe\r\n')
            (root / 'docs' / 'b.adoc').write_text('東京', encoding='utf-8')
            (root / 'ignore.md').write_text('ignored')
            (root / '.hidden').mkdir()
            (root / '.hidden' / 'secret.adoc').write_text('hidden')
            (root / 'link.adoc').symlink_to(root / 'a.adoc')
            (root / 'linked-dir').symlink_to(root / 'docs', target_is_directory=True)
            cases = models.harvest_corpus(root)
            self.assertEqual([c['path'] for c in cases], ['a.adoc', 'docs/b.adoc'])
            self.assertEqual(cases[0]['source'], 'cafe\r\n')
            self.assertEqual(cases[0]['sha256'], hashlib.sha256(b'cafe\r\n').hexdigest())
            self.assertEqual(cases[1]['size_bytes'], len('東京'.encode()))
            self.assertEqual(cases, models.harvest_corpus(root))

    def test_corpus_never_expands_includes(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            source = 'include::/etc/passwd[]\ninclude::https://example.org/[]\n'
            (root / 'include.adoc').write_text(source)
            self.assertEqual(models.harvest_corpus(root)[0]['source'], source)
            (root / 'bad.adoc').write_bytes(b'\xff')
            with self.assertRaises(UnicodeDecodeError):
                models.harvest_corpus(root)

    def test_virtual_fixtures_supply_only_explicit_inputs(self):
        suite = json.loads((SCRIPT / 'fixtures.json').read_text())
        cases = [c for c in suite['cases'] if c['category'] == 'virtual-includes']
        self.assertGreaterEqual(len(cases), 16)
        for case in cases:
            self.assertIsInstance(case['includes'], dict)
            self.assertIs(case['reject'], True)
            self.assertEqual(case['support'], 'future')
            self.assertEqual(case['expect']['warning_severities'], [])
            for path, source in case['includes'].items():
                self.assertFalse(path.startswith('/'))
                self.assertNotIn('..', path.split('/'))
                self.assertNotIn('://', path)
                self.assertIsInstance(source, str)
        ids = {c['id'] for c in cases}
        for suffix in ('basic', 'nested-relative', 'parent-relative', 'conditional-enabled',
                       'conditional-disabled', 'attribute-target', 'attribute-flow',
                       'inside-listing', 'inside-literal', 'unicode-crlf'):
            self.assertIn('virtual-include-' + suffix, ids)

    def test_fixture_coverage_is_explicit_and_semantic(self):
        suite = json.loads((SCRIPT / 'fixtures.json').read_text())
        self.assertEqual(suite['oracle_version'], '2.0.23')
        cases = suite['cases']
        ids = [c['id'] for c in cases]
        self.assertEqual(len(ids), len(set(ids)))
        self.assertGreaterEqual(len(cases), 128)
        self.assertGreaterEqual(len(suite['coverage']), 16)
        self.assertEqual(sorted(ids), sorted(i for group in suite['coverage'] for i in group['required_cases']))
        for case in cases:
            self.assertTrue(case['expect'], case['id'])
            self.assertTrue(case['features'], case['id'])
            self.assertTrue(case['references'], case['id'])
            self.assertIn(case['support'], ('minimal', 'future'))
            if case.get('reject'):
                self.assertEqual(case['support'], 'future')
            if 'warnings_contains' in case['expect']:
                self.assertTrue(case['expect']['warnings_contains'])
                self.assertEqual(case['expect']['warning_severities'], ['WARN'])
            if case['support'] == 'minimal':
                self.assertIn('projection', case['expect'])
        self.assertGreaterEqual(sum('warnings_contains' in c['expect'] for c in cases), 6)
        self.assertGreaterEqual(sum(c.get('reject') is True for c in cases), 60)
        for group in suite['coverage']:
            self.assertEqual(set(group['required_cases']), {c['id'] for c in cases if c['category'] == group['category']})


if __name__ == '__main__':
    unittest.main()
