import hashlib
import os
import json
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2]


class CoverageEvidenceTests(unittest.TestCase):
    def test_checklist_references_live_fixture_contracts(self):
        document = (ROOT / 'docs/roc-syntax-coverage.adoc').read_text()
        fixtures = json.loads((ROOT / 'scripts/roc_syntax/fixtures.json').read_text())
        cases = {case['id']: case for filename in ('fixtures.json', 'type-fixtures.json', 'platform-fixtures.json', 'grammar-fixtures.json')
                 for case in json.loads((ROOT / 'scripts/roc_syntax' / filename).read_text())['cases']}
        self.assertIn(fixtures['reference_commit'], document)
        evidence = re.findall(r'fixture `([^`]+)` \(`([^`]+)`, `([^`]+)`\)', document)
        self.assertTrue(evidence, 'checklist must expose concrete regression evidence')
        for id, rule, coverage in evidence:
            with self.subTest(fixture=id):
                self.assertIn(id, cases)
                self.assertEqual((rule, coverage), (cases[id]['rule'], cases[id]['coverage']))

    def test_all_107_variants_have_constructed_or_unconstructed_evidence(self):
        ledger = json.loads((ROOT / 'scripts/roc_syntax/grammar-evidence.json').read_text())
        fixtures = json.loads((ROOT / 'scripts/roc_syntax/fixtures.json').read_text())
        self.assertEqual(ledger['reference_commit'], fixtures['reference_commit'])
        entries = ledger['variants']
        self.assertEqual(len(entries), 107)
        self.assertEqual(len({(e['category'], e['variant']) for e in entries}), 107)
        self.assertEqual({e['category'] for e in entries}, {'Header', 'Statement', 'Expr', 'Pattern', 'TypeAnno', 'WhereClause', 'ExposedItem', 'TargetFile', 'TargetConfigValue'})
        unconstructed = {(e['category'], e['variant']) for e in entries if e['status'] == 'compiler-only-unconstructed'}
        self.assertEqual(unconstructed, {('Expr', 'record_updater'), ('Header', 'default_app')})
        generic = {e['category'] for e in entries if e['status'] == 'generic-recovery'}
        self.assertEqual(generic, {'Header', 'Statement', 'Expr', 'Pattern', 'TypeAnno', 'WhereClause', 'ExposedItem', 'TargetFile'})
        for entry in entries:
            with self.subTest(category=entry['category'], variant=entry['variant']):
                if entry['status'] == 'compiler-only-unconstructed':
                    self.assertFalse(entry['constructors'])
                    continue
                self.assertTrue(entry['constructors'])
                self.assertTrue(entry['fixtures'])
                for reference in entry['fixtures']:
                    suite = json.loads((ROOT / 'scripts/roc_syntax' / reference['suite']).read_text())
                    self.assertEqual(suite['reference_commit'], ledger['reference_commit'])
                    case = next(c for c in suite['cases'] if c['id'] == reference['id'])
                    self.assertEqual(case['coverage'], 'recovery' if entry['variant'] == 'malformed' else 'supported')

    def test_native_observed_witnesses_keep_exact_inputs_and_fixture_provenance(self):
        ledger = json.loads((ROOT / 'scripts/roc_syntax/grammar-evidence.json').read_text())
        audit = ledger['execution_audit']
        self.assertEqual(audit['inputs'], 3052)
        self.assertEqual(audit['counts'], {'accepted': 2599, 'rejected': 453, 'failed': 0})
        self.assertEqual(audit['observed_valid_variants'], 95)
        observed = [entry for entry in ledger['variants'] if entry.get('observed_witnesses')]
        self.assertEqual(len(observed), 95)
        remaining = {(entry['category'], entry['variant']) for entry in ledger['variants'] if not entry.get('observed_witnesses')}
        self.assertEqual(remaining, {(entry['category'], entry['variant']) for entry in ledger['variants']
                                    if entry['variant'] == 'malformed' or entry['status'] == 'compiler-only-unconstructed'} | {('Expr', 'string_part')})
        for entry in observed:
            for witness in entry['observed_witnesses']:
                self.assertTrue(witness['accepted'])
                self.assertTrue(witness['structural_match'])
                self.assertEqual(witness['source_sha256'], hashlib.sha256(witness['source'].encode()).hexdigest())
                if 'fixture' in witness:
                    ref = witness['fixture']
                    suite = json.loads((ROOT / 'scripts/roc_syntax' / ref['suite']).read_text())
                    case = next(c for c in suite['cases'] if c['id'] == ref['id'])
                    self.assertEqual((case['mode'], case['source']), (witness['mode'], witness['source']))

    @unittest.skipUnless(os.environ.get('ROC_SYNTAX_REFERENCE_SOURCE'), 'set ROC_SYNTAX_REFERENCE_SOURCE for immutable source-construction audit')
    def test_pinned_source_constructor_receipts(self):
        ledger = json.loads((ROOT / 'scripts/roc_syntax/grammar-evidence.json').read_text())
        source = Path(os.environ['ROC_SYNTAX_REFERENCE_SOURCE']) / 'src/parse'
        for name, digest in ledger['authority'].items():
            self.assertEqual(hashlib.sha256((source / name).read_bytes()).hexdigest(), digest, name)
        parser = (source / 'Parser.zig').read_text()
        for variant in ('record_updater', 'default_app'):
            self.assertNotRegex(parser, r'\.' + variant + r'\b')
        direct = set(re.findall(r'add(Expr|Pattern|Statement|Header|TypeAnno|WhereClause|ExposedItem|TargetFile|TargetConfigValue)\(\.\{\s*\.(@"[^"]+"|\w+)\s*=', parser))
        self.assertEqual(len(direct), 95)
        documented = {(e['category'], e['variant']) for e in ledger['variants']}
        self.assertLessEqual(direct, documented)
        for entry in ledger['variants']:
            for constructor in entry['constructors']:
                lines = (source / constructor['file']).read_text().splitlines()
                line = lines[constructor['line'] - 1]
                self.assertTrue('add' in line or 'pushMalformed' in line, (entry, line))
