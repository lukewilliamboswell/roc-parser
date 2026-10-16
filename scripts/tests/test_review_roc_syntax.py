import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('review_roc_syntax', SCRIPTS / 'review_roc_syntax.py')
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)

class WorkerTests(unittest.TestCase):
    def worker(self, folder, body):
        path = Path(folder) / 'worker.py'
        path.write_text('import json,sys,time\n' + body)
        return [sys.executable, '-u', str(path)]

    def test_multiple_requests_use_one_process(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'for line in sys.stdin:\n r=json.loads(line); print(json.dumps({"version":1,"id":r["id"],"pid":__import__("os").getpid()}),flush=True)\n')
            with review.Worker(command) as worker:
                first = worker.request({'id': 'one', 'source':'multi\nline'})
                second = worker.request({'id':'two','source':'λ'})
                self.assertEqual(first['pid'], second['pid'])
                self.assertEqual(worker.requests, 2)

    def test_crash_is_reported(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'sys.exit(9)\n')
            with review.Worker(command) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'exited'):
                    worker.request({'id':'crash'})

    def test_timeout_terminates_worker(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'time.sleep(5)\n')
            with review.Worker(command, timeout=.05) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'timeout'):
                    worker.request({'id':'slow'})
            self.assertIsNotNone(worker.proc.poll())

    def test_wrong_id_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'print(json.dumps({"version":1,"id":"wrong"}),flush=True)\n')
            with review.Worker(command) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'ID'):
                    worker.request({'id':'correct'})

    def test_mismatch_retained_and_request_replayable(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'for line in sys.stdin:\n r=json.loads(line); print(json.dumps({"version":1,"id":r["id"],"ok":True,"consumed":True,"tokens":[],"shape":["wrong"]}),flush=True)\n')
            case = {'id':'deliberate/mismatch','source':'1','mode':'expr','model_shape':[['number','1']]}
            result = review.run_cases([case], command, command, Path(tmp)/'failures')
            self.assertFalse(result[0]['pass'])
            saved = json.loads(next((Path(tmp)/'failures').glob('*.json')).read_text())
            self.assertEqual(saved['request']['source'], '1')
            self.assertEqual(saved['model_shape'], case['model_shape'])
            self.assertEqual(saved['differences'][0]['field'], 'actual_model_shape')

class ProjectionTests(unittest.TestCase):
    def test_precedence_disagreement_detected(self):
        first = {'ok':True,'consumed':True,'tokens':[], 'shape':['binary','+',1,['binary','*',2,3]]}
        second = {**first,'shape':['binary','*',['binary','+',1,2],3]}
        self.assertEqual(review.compare({'source':'1+2*3'},first,second)[0]['field'], 'shape')

    def test_call_adjacency_preserved(self):
        first={'tokens':[{'kind':'NoSpaceOpenRound','start':1,'end':2}]}
        second={'tokens':[{'kind':'OpenRound','start':1,'end':2}]}
        self.assertNotEqual(review.normalize_tokens(first,'f('),review.normalize_tokens(second,'f('))

    def test_generated_models_reproducible_and_varied(self):
        first=review.generated_cases(100,7)
        self.assertEqual(first,review.generated_cases(100,7))
        self.assertGreater(len({case['source'] for case in first}),80)
        self.assertTrue(any('binary' in str(c['model_shape']) for c in first))

    def test_fixture_rule_inventory_checked(self):
        metadata,cases=review.load_cases(review.DATA/'fixtures.json')
        self.assertEqual(set(metadata['coverage']),{c['rule'] for c in cases})
