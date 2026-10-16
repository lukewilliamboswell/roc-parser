import contextlib
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS))
import mutate_roc_syntax as mutation


class MutationTests(unittest.TestCase):
    def test_determinism_bounds_and_utf8_edit_replay(self):
        seed = {'id': 'unicode', 'mode': 'expr', 'source': 'λé + (123)'}
        cases = [mutation.mutate(seed, 42, index, 32) for index in range(100)]
        self.assertEqual(cases, [mutation.mutate(seed, 42, index, 32) for index in range(100)])
        self.assertNotEqual(cases[0], mutation.mutate(seed, 43, 0, 32))
        self.assertEqual({edit['kind'] for case in cases for edit in case['mutation']['edits']}, {'insert', 'delete', 'replace'})
        for case in cases:
            self.assertLessEqual(len(case['request']['source'].encode()), 32)
            self.assertEqual(mutation.replay_edits(seed['source'], case['mutation']['edits']), case['request']['source'])
        for source in ['', 'é', 'a' * 32]:
            for index in range(20):
                case = mutation.mutate(dict(seed, source=source), 42, index, 32)
                self.assertTrue(case['mutation']['edits'])
                self.assertLessEqual(len(case['request']['source'].encode()), 32)

    def response(self, ok=True, shape=None):
        return {'ok': ok, 'consumed': ok, 'tokens': [{'kind': 'EndOfFile', 'start': 0, 'end': 0}],
                'shape': ['number', '1'] if shape is None else shape}

    def test_acceptance_tree_and_unknown_are_real_failures(self):
        request = {'source': ''}
        self.assertEqual(mutation.compare(request, self.response(), self.response(False))[0]['field'], 'acceptance')
        self.assertEqual(mutation.compare(request, self.response(shape=['number', '2']), self.response())[0]['field'], 'shape')
        self.assertEqual(mutation.compare(request, self.response(shape=['unsupported', 'Future']), self.response())[0]['field'], 'projection_gap')

    def test_rejected_recovery_shapes_may_differ_but_invariants_may_not(self):
        request = {'source': ''}
        self.assertEqual(mutation.compare(request, self.response(False, ['recovery']), self.response(False, [])), [])
        bad = self.response(False)
        bad['tokens'][0]['end'] = 3
        self.assertEqual(mutation.compare(request, bad, self.response(False))[0]['field'], 'invariant')

    def test_crash_is_retained_and_persistent_worker_recovers(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / 'worker.py'
            path.write_text('import json,sys,os\nfor line in sys.stdin:\n r=json.loads(line)\n if r["source"]=="crash": sys.exit(9)\n print(json.dumps({"version":1,"id":r["id"],"ok":True,"consumed":True,"tokens":[{"kind":"EndOfFile","start":0,"end":0}],"shape":[],"pid":os.getpid()}),flush=True)\n')
            command = [sys.executable, '-u', str(path)]
            campaign = mutation.Campaign(command, command, .5)
            try:
                def case(source, index):
                    return {'request': {'id': str(index), 'mode': 'expr', 'source': source}, 'mutation': {'seed': 9}}
                bad = campaign.evaluate(case('crash', 0))
                first = campaign.evaluate(case('', 1))
                second = campaign.evaluate(case('', 2))
                self.assertFalse(bad['passed'])
                self.assertEqual(bad['mutation']['seed'], 9)
                self.assertTrue(first['passed'])
                self.assertEqual(first['candidate']['pid'], second['candidate']['pid'])
            finally:
                campaign.close()

    def test_cli_filters_seeds_checks_raw_rejections_and_replays_failure(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            worker = root / 'worker.py'
            worker.write_text('import json,sys\nfor line in sys.stdin:\n r=json.loads(line); n=len(r["source"].encode()); ok=r["source"]!="bad"\n tokens=([{"kind":"Unknown","start":0,"end":n}] if n else [])+[{"kind":"EndOfFile","start":n,"end":n}]\n print(json.dumps({"version":1,"id":r["id"],"ok":ok,"consumed":ok,"tokens":tokens,"shape":[sys.argv[-1]]}),flush=True)\n')
            fixture = root / 'fixtures.json'
            fixture.write_text(json.dumps({'version': 1, 'reference_commit': mutation.review.REFERENCE_COMMIT,
                'coverage': ['test'], 'cases': [{'id': source or 'empty', 'source': source, 'mode': 'expr', 'coverage': 'exploratory', 'rule': 'test'} for source in ['', 'bad']]}))
            corpus = root / 'corpus'
            corpus.mkdir()
            (corpus / 'rejected').write_text('bad')
            (corpus / 'invalid').write_bytes(b'\xff')
            command = f'{sys.executable} -u {worker}'
            args = ['--probe-command', command + ' candidate', '--oracle-command', command + ' oracle',
                    '--fixtures', str(fixture), '--corpus', str(corpus), '--runs', '3', '--seed', '7', '--output', str(root / 'out')]
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertTrue(mutation.main(args))
            report = json.loads((root / 'out/report.json').read_text())
            self.assertEqual(report['counts']['accepted_seeds'], 1)
            self.assertEqual(report['counts']['rejected_seeds'], 1)
            self.assertEqual(report['counts']['corpus_invalid_utf8_excluded'], 1)
            self.assertEqual(report['counts']['total'], 4)
            self.assertEqual(report['counts']['passed'], 1)  # rejected raw corpus differs in recovery shape
            failure = next((root / 'out/failures').glob('*.json'))
            replay = args[:4] + ['--replay', str(failure), '--output', str(root / 'replay')]
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertTrue(mutation.main(replay))
            saved = json.loads((root / 'replay/cases.jsonl').read_text())
            self.assertEqual(saved['mutation'], json.loads(failure.read_text())['mutation'])

    def test_timeout_is_a_failure(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / 'worker.py'
            path.write_text('import time\ntime.sleep(5)\n')
            command = [sys.executable, '-u', str(path)]
            campaign = mutation.Campaign(command, command, .03)
            try:
                result = campaign.evaluate({'request': {'id': 'slow', 'mode': 'expr', 'source': ''}, 'mutation': {}})
                self.assertFalse(result['passed'])
                self.assertTrue(all('timeout' in error['error'] for error in result['differences']))
            finally:
                campaign.close()


if __name__ == '__main__':
    unittest.main()
