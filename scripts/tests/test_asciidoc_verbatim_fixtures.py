"""Retained authored callout controls: exact semantic requests and default discovery."""
import hashlib
import json
from pathlib import Path
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
import check_asciidoc_performance as perf
import review_asciidoc as review

EXPECTED_REQUESTS = {'verbatim-missing-closer-default-missing-closer': 'da860db8324e451592ad0a549e54e0e1192279f89614ac139f3b3050180c9f50', 'verbatim-missing-closer-default-escaped-missing-closer': 'c5bddc9bed9a9f9e584701add7d88acd184897a71895f6122585ffa0ea56708e', 'verbatim-missing-closer-default-unicode-missing-closer': 'a5d8b593e9c1abe77127016ca5ea749629d2b2cc94de1f5198f6defbde85a0de', 'verbatim-missing-closer-default-multiline-missing-closer': 'ff3871e664d601e665979147036279317734f2cdbd499c9ca35c8e6c6a271092', 'verbatim-missing-closer-explicit-specialcharacters': '1d98716da6e025aed6a73e13602ca15ba4ec920d47be5d21de9f7af3868590bd', 'verbatim-missing-closer-custom-quotes': 'e0827b8fe2d3d739b43df8652df5c3c71e0fd9df09c6f39663a4dc91ec6a5fb0', 'verbatim-missing-closer-modified-quotes': '4b8961a3e747902fb0945a3c1a8d283f16c556363f641a39f25d7d00357e88c5', 'verbatim-missing-closer-modified-no-callouts': 'baab6eeda08c79529fb65a23444d612083d8f2f8c22fa9f14323329f67e54c51', 'verbatim-missing-closer-custom-none': '8c0318f86808360d0401dac4e3d05f29b5b9e45afa3960628720a7757a14d1d1', 'verbatim-missing-closer-valid-normal-callout': '967f6fdc2859bccf49f92b82bc3533dc675ff90a1aab47c55cd50f27887451dc', 'verbatim-missing-closer-valid-comment-callout': '183096ac4b2ea4f4e5b7086331636a2fa58bf3e098c5f8d635c14ef75489cc06', 'verbatim-missing-closer-valid-escaped-callout': '024d919e4093a88c59497ba4574655e521f19db010045bbc6244696d96d1e4eb'}

class VerbatimCalloutFixtures(unittest.TestCase):
    def test_all_twelve_verified_behaviors_remain_exact_and_deduplicated(self):
        path = ROOT / 'scripts/asciidoc/verbatim-callout-regressions-fixtures.json'
        value = json.loads(path.read_text())
        cases = review.validate_fixture(value)
        self.assertEqual(len(cases), 11)
        self.assertTrue(all(c['category'] == 'delimited-blocks' and c['features'] == ['listing-raw'] for c in cases))
        retained = value['provenance']['retained_behaviors']
        self.assertEqual({r['original_id']: r['request_sha256'] for r in retained}, EXPECTED_REQUESTS)
        self.assertEqual(sum(r['action'] == 'deduplicated-exact-request' for r in retained), 1)
        self.assertEqual(len({r['request_sha256'] for r in retained}), 12)
        self.assertEqual(len(value['provenance']['verification_report_sha256']), 64)
        self.assertEqual(len(value['provenance']['candidate_source_identity']), 64)
        for item in retained:
            with self.subTest(case=item['original_id']):
                target = json.loads((ROOT / 'scripts/asciidoc' / item['target_fixture']).read_text())
                case = next(c for c in target['cases'] if c['id'] == item['target_id'])
                self.assertEqual(perf.case_identity(case), item['request_sha256'])
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(), item['source_sha256'])
                self.assertEqual(item['authored_expect']['contexts'], ['listing'])
                if item['action'] == 'retained-new':
                    self.assertEqual(case['expect'], item['authored_expect'])
                else:
                    # Existing comment control has a stronger literal HTML predicate.
                    for predicate in item['authored_expect']['html_contains']:
                        self.assertTrue(any(predicate in existing for existing in case['expect']['html_contains']))

    def test_default_authored_discovery_includes_the_eleven_new_controls(self):
        args = perf.parser().parse_args(['--root', str(ROOT), '--generated', '0'])
        args.sizes = [1]
        cases = list(perf.historical_corpus(args))
        retained = json.loads((ROOT / 'scripts/asciidoc/verbatim-callout-regressions-fixtures.json').read_text())['provenance']['retained_behaviors']
        for item in retained:
            matched = [c for c in cases if c['id'] == item['target_id']]
            self.assertEqual(len(matched), 1)
            self.assertEqual(perf.case_identity(matched[0]), item['request_sha256'])

if __name__ == '__main__':
    unittest.main()
