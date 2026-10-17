"""Schema-2 correctness boundaries and compact timing authentication."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
spec = importlib.util.spec_from_file_location('performance_protocol', ROOT / 'scripts/check_asciidoc_performance.py')
p = importlib.util.module_from_spec(spec)
spec.loader.exec_module(p)
CASE = {'id': 'x', 'source': 'Text'}
IDENTITY = {'oracle_version': '2.0.23', 'oracle_code_sha256': 'a'*64,
            'include_io': 'caller-map-memory', 'include_path_metadata': 'memory'}


def zero(version=2, metric=7, mode='render', html='<p>Text</p>'):
    value = {'version': version, 'id': 'x', 'iterations': 0, 'successes': 0,
             'elapsed_ns': 0, 'checksum': 0, 'projection': {'title': '', 'blocks': []}, 'html': html, **IDENTITY}
    if version == 2:
        value.update(phase='correctness', mode=mode, warmup=0, operation_checksum=metric)
    return value


class Engine:
    def __init__(self, version=2, metric=7):
        self.version, self.metric = version, metric
        self.requests = []
    def request(self, request):
        self.requests.append(request)
        value = zero(self.version, self.metric, request['mode'])
        value.update(iterations=request['iterations'], successes=request['iterations'],
                     elapsed_ns=1000, checksum=(request['iterations']+request['warmup'])*self.metric)
        if self.version == 2:
            value.update(phase='timing', warmup=request['warmup'])
            value.pop('projection'); value.pop('html'); value.pop('operation_checksum')
        return value


class Protocol(unittest.TestCase):
    def checked(self, engine):
        return p.CheckedTimingWorker(engine, CASE, 'render', zero(engine.version, engine.metric))

    def test_transport_defaults_to_v1_and_requires_explicit_v2_opt_in(self):
        command=[sys.executable,'-c',"import sys,json; r=json.loads(sys.stdin.readline()); print(json.dumps({'id':r['id'],'version':2}))"]
        legacy=p.JsonlWorker(command,timeout=5)
        try:
            with self.assertRaisesRegex(RuntimeError,'protocol error'):
                legacy.request({'id':'x'})
        finally: legacy.close()
        modern=p.Worker(command,timeout=5)
        try: self.assertEqual(modern.request({'id':'x'})['version'],2)
        finally: modern.close()

    def test_zero_requires_full_output_and_fresh_operation_metric(self):
        for version in (1, 2):
            self.assertEqual(p.validate_reply(zero(version), CASE, 0)['version'], version)
        for field in ('projection', 'html', 'operation_checksum'):
            value = zero(); value.pop(field)
            with self.subTest(field=field), self.assertRaises(ValueError):
                p.validate_reply(value, CASE, 0)

    def test_boolean_numeric_fields_are_never_accepted_as_integers(self):
        for field in ('version', 'iterations', 'successes', 'elapsed_ns', 'checksum', 'warmup', 'operation_checksum'):
            value = zero(); value[field] = False if field != 'version' else True
            with self.subTest(field=field), self.assertRaises(ValueError):
                p.validate_reply(value, CASE, 0)

    def test_phase_mode_and_warmup_are_bound_to_request(self):
        for change in ({'phase':'timing'}, {'mode':'parse'}, {'warmup':1}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                p.validate_reply({**zero(), **change}, CASE, 0, mode='render', warmup=0)

    def test_compact_reply_needs_authentication_not_a_json_flag(self):
        value = Engine().request(p.request(CASE, 'render', 1))
        for marker in (None, True, 'checked'):
            value['_checked_timing'] = marker
            with self.subTest(marker=marker), self.assertRaisesRegex(ValueError, 'checked context'):
                p.validate_reply(value, CASE, 1)
            with self.assertRaisesRegex(ValueError, 'checked context'):
                p.equivalent(value, zero())

    def test_checksum_is_exact_and_separate_for_each_engine(self):
        for metric in (7, 19):
            engine = Engine(metric=metric)
            value = self.checked(engine).request(p.request(CASE, 'render', 3, 2))
            self.assertEqual(value['checksum'], 5*metric)
            p.validate_reply(value, CASE, 3)
            p.equivalent(value, zero(metric=999))  # no cross-engine metric comparison

    def test_bad_checksum_expires_context(self):
        engine = Engine()
        original = engine.request
        engine.request = lambda request: {**original(request), 'checksum': 1}
        checked = self.checked(engine)
        with self.assertRaisesRegex(ValueError, 'checksum'):
            checked.request(p.request(CASE, 'render', 1))
        with self.assertRaisesRegex(ValueError, 'expired'):
            checked.request(p.request(CASE, 'render', 1))

    def test_case_options_mode_and_worker_instance_are_bound(self):
        changed = [{**CASE,'source':'Other'}, {**CASE,'attributes':[{'name':'a','value':'b'}]},
                   {**CASE,'includes':{'part.adoc':'Other'}}]
        for case in changed:
            checked = self.checked(Engine())
            with self.subTest(case=case), self.assertRaisesRegex(ValueError, 'checked case'):
                checked.request(p.request(case, 'render', 1))
        with self.assertRaisesRegex(ValueError, 'checked case'):
            self.checked(Engine()).request(p.request(CASE, 'parse', 1))
        checked = self.checked(Engine()); checked.worker = Engine()
        with self.assertRaisesRegex(ValueError, 'expired'):
            checked.request(p.request(CASE, 'render', 1))

    def test_underlying_transport_and_process_replacement_expire_context(self):
        class Wrapper:
            def __init__(self):
                self.worker=Engine()
                self.worker.proc=SimpleNamespace(pid=123)
            def request(self,value): return self.worker.request(value)
        for replacement in ('transport','process','pid'):
            wrapper=Wrapper()
            checked=p.CheckedTimingWorker(wrapper,CASE,'render',zero())
            if replacement=='transport': wrapper.worker=Engine()
            elif replacement=='process': wrapper.worker.proc=SimpleNamespace(pid=123)
            else: wrapper.worker.proc.pid=124
            with self.subTest(replacement=replacement), self.assertRaisesRegex(ValueError,'expired'):
                checked.request(p.request(CASE,'render',1))

    def test_reference_identity_cannot_change_during_timing(self):
        engine = Engine(); original = engine.request
        engine.request = lambda request: {**original(request), 'oracle_code_sha256':'b'*64}
        with self.assertRaisesRegex(ValueError, 'identity changed'):
            self.checked(engine).request(p.request(CASE, 'render', 1))

    def test_legacy_full_output_drift_is_still_rejected(self):
        engine = Engine(version=1); original = engine.request
        engine.request = lambda request: {**original(request), 'html':'<p>Changed</p>'}
        with self.assertRaisesRegex(ValueError, 'DOM differs'):
            self.checked(engine).request(p.request(CASE, 'render', 1))

    def test_mixed_versions_calibrate_and_sample_with_original_gates(self):
        engines = [Engine(2, 7), Engine(1, 19)]
        workers = [self.checked(engine) for engine in engines]
        expected = zero(1, 19)
        count = p.calibrate_common(workers, CASE, 'render', 1, 100, expected)
        measured, attempts = p.sample_pair_per_engine(workers, CASE, 'render', [count,count], 1, 3, 100, expected)
        self.assertEqual(len(attempts), 1)
        self.assertEqual([len(value['samples_ns']) for value in measured], [3,3])
        self.assertTrue(all(request['iterations'] > 0 for engine in engines for request in engine.requests))

    def test_after_checks_are_fresh_self_and_cross_engine_proofs(self):
        class CorrectnessEngine:
            def __init__(self, reply): self.reply, self.requests = reply, []
            def request(self, request): self.requests.append(request); return dict(self.reply)
        engines = [CorrectnessEngine(zero(metric=7)), CorrectnessEngine(zero(metric=19))]
        before = [zero(metric=7), zero(metric=19)]
        after=[]
        proof = p.finish_correctness(engines, CASE, 'render', before, after)
        self.assertEqual([item['operation_checksum'] for item in proof],[7,19])
        self.assertTrue(all(engine.requests == [p.request(CASE,'render',0)] for engine in engines))
        engines[0].reply = zero(html='<p>Changed</p>')
        with self.assertRaisesRegex(ValueError, 'DOM differs'):
            p.finish_correctness(engines, CASE, 'render', before, [])
        self.assertEqual(len(engines[1].requests),2)  # both fresh replies fetched before comparisons

    def test_invalid_post_reply_is_retained_before_validation_raises(self):
        bad = {**zero(), 'operation_checksum': False}
        engine = SimpleNamespace(request=lambda request: bad)
        after=[]
        with self.assertRaisesRegex(ValueError, 'operation checksum'):
            p.finish_correctness([engine,engine], CASE, 'render', [zero(),zero()], after)
        self.assertEqual(after,[bad])

    def run_main(self, worker, output, sampler=None):
        measured = p.distribution([1000]*3,1,1)
        if sampler is None:
            sampler = lambda *args, **kwargs: ([measured,measured],[{'qualification':'both-duration-floor'}])
        with mock.patch.multiple(p, Worker=worker,
                 build=mock.Mock(return_value=(Path('/fake'),{'source_identity':'same'})),
                 source_identity=mock.Mock(return_value='same'), digest=mock.Mock(return_value='a'*64),
                 corpus=mock.Mock(return_value=[CASE]), calibrate=mock.Mock(return_value=1),
                 sample_pair_per_engine=sampler), \
             mock.patch.object(p.platform,'platform',return_value='test OS'), \
             mock.patch.object(p.platform,'processor',return_value='test CPU'), \
             mock.patch.object(p.subprocess,'run',return_value=SimpleNamespace(stdout='Ruby')):
            return p.main(['--smoke','--modes','render','--output',output])

    def test_failed_initial_correctness_prevents_all_timing_requests(self):
        requests=[]
        class Worker:
            def __init__(self,command,*args,**kwargs): self.oracle=command[0]=='ruby'
            def request(self,request):
                requests.append(request)
                return zero(html='<p>Other</p>' if self.oracle else '<p>Text</p>')
            def close(self): pass
        with tempfile.TemporaryDirectory() as output:
            self.assertEqual(self.run_main(Worker,output),1)
            row=json.loads((Path(output)/'results.json').read_text())['records'][0]
            self.assertEqual(row['status'],'error')
            self.assertTrue(Path(row['correctness_artifact']).exists())
        self.assertTrue(requests)
        self.assertTrue(all(request['iterations']==0 for request in requests))

    def test_post_drift_fails_and_retains_completed_timing_attempts(self):
        class Worker:
            def __init__(self,*args,**kwargs): self.zeros=0
            def request(self,request):
                self.zeros+=1
                return zero(html='<p>Text</p>' if self.zeros==1 else '<p>Drift</p>')
            def close(self): pass
        with tempfile.TemporaryDirectory() as output:
            self.assertEqual(self.run_main(Worker,output),1)
            row=json.loads((Path(output)/'results.json').read_text())['records'][0]
            self.assertEqual(row['status'],'error')
            self.assertIn('DOM differs',row['error'])
            self.assertEqual(row['measurement_attempts'][0]['qualification'],'both-duration-floor')
            proof=json.loads(Path(row['correctness_artifact']).read_text())
            self.assertEqual(len(proof['before']),2); self.assertEqual(len(proof['after']),2)
            self.assertIn('correctness_policy',proof['run'])

    @unittest.skipUnless(os.environ.get('ROC_ASCIIDOC_PERFORMANCE_BINARY'),'set ROC_ASCIIDOC_PERFORMANCE_BINARY for live v2 native checks')
    def test_native_zero_and_compact_positive_checksums(self):
        engine=p.Worker([os.environ['ROC_ASCIIDOC_PERFORMANCE_BINARY']],timeout=30)
        try:
            for mode in ('parse','render'):
                before=p.validate_reply(engine.request(p.request(CASE,mode,0)),CASE,0,mode=mode,warmup=0)
                self.assertEqual(before['version'],2)
                checked=p.CheckedTimingWorker(engine,CASE,mode,before)
                value=checked.request(p.request(CASE,mode,3,2))
                self.assertNotIn('html',value); self.assertNotIn('projection',value)
                self.assertEqual(value['checksum'],5*before['operation_checksum'])
                after=p.validate_reply(engine.request(p.request(CASE,mode,0)),CASE,0,mode=mode,warmup=0)
                p.equivalent(before,after)
                self.assertEqual(before['operation_checksum'],after['operation_checksum'])
        finally: engine.close()

    @unittest.skipUnless(os.environ.get('ASCIIDOC_ORACLE_GEM_HOME'),'set ASCIIDOC_ORACLE_GEM_HOME for instrumented live Ruby checks')
    def test_ruby_positive_calls_each_operation_without_extra_output_conversion(self):
        # Instrument the real protocol, leaving both operation implementations
        # and timed loops intact. The tiny document converts exactly once per
        # render operation, so extra post-timer loads/converts are observable.
        harness=r'''
path=ARGV.fetch(0)
parts=File.read(path).split(/^STDOUT.sync = true$/,2)
eval(parts[0],TOPLEVEL_BINDING,path,1)
$loads=$operations=$conversions=0
alias original_load_document load_document
alias original_operation operation
def load_document(*args); $loads+=1; original_load_document(*args); end
def operation(*args); $operations+=1; original_operation(*args); end
module CountConvert
 def convert(*args,**kwargs,&block); $conversions+=1; super; end
end
Asciidoctor::Document.prepend(CountConvert)
eval("STDOUT.sync = true\n"+parts[1],TOPLEVEL_BINDING,path,1)
STDERR.puts(JSON.generate(loads:$loads,operations:$operations,conversions:$conversions))
'''
        for mode in ('parse','render'):
            requests=[p.request(CASE,mode,0),p.request(CASE,mode,3,2),p.request(CASE,mode,0)]
            result=subprocess.run(['ruby','-e',harness,str(ROOT/'scripts/asciidoc/performance_oracle.rb')],
                input=''.join(json.dumps(value)+'\n' for value in requests),text=True,capture_output=True,check=True,timeout=30,
                env=p.oracle_environment(Path(os.environ['ASCIIDOC_ORACLE_GEM_HOME'])))
            replies=[json.loads(line) for line in result.stdout.splitlines()]
            self.assertEqual([value['phase'] for value in replies],['correctness','timing','correctness'])
            self.assertNotIn('html',replies[1])
            self.assertEqual(replies[1]['checksum'],5*replies[0]['operation_checksum'])
            counters=json.loads(result.stderr.splitlines()[-1])
            self.assertEqual(counters,{'loads':9,'operations':7,'conversions':9 if mode=='render' else 2})


if __name__=='__main__': unittest.main()
