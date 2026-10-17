"""Host-only tests; no Ruby, compiler or native process required."""
import copy, hashlib, importlib.util, json, statistics, subprocess, sys, tempfile, types, unittest
from pathlib import Path
from unittest.mock import patch

# When integrated, both modules live in scripts/. In preparation use this file's directory.
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE if (HERE/'check_asciidoc_performance_cached.py').exists() else HERE.parent))
import check_asciidoc_performance_cached as c

class FakeP:
    @staticmethod
    def case_identity(case):
        return c.value_sha({'source':case['source'],'includes':case.get('includes',{}),'expand':'includes'in case,'attributes':case.get('attributes',[])})
    @staticmethod
    def request(case,mode,n,warmup=0):return {'id':case['id'],'source':case['source'],'mode':mode,'iterations':n,'warmup':warmup}
    @staticmethod
    def validate_reply(reply,case,n,**kwargs):
        if reply.get('id')!=case['id'] or reply.get('iterations')!=n:raise ValueError('protocol drift')
    @staticmethod
    def correctness_proof(replies):
        return [{'projection_sha256':c.value_sha(v['projection']),'rendered_dom_sha256':c.value_sha(v['html']),'html_sha256':hashlib.sha256(v['html'].encode()).hexdigest()} for v in replies]
    @staticmethod
    def distribution(raw,count,window):
        v=[x/count for x in raw]
        return {'iterations':count,'samples_ns':v,'median_ns':statistics.median(v),'min_ns':min(v),'max_ns':max(v),'coefficient_of_variation':statistics.pstdev(v)/statistics.mean(v),'minimum_window_ns':window,'observed_minimum_window_ns':min(raw)}
    @staticmethod
    def ratchet(a,b,limit):
        ratio=a['median_ns']/b['median_ns']
        return {'ratio':ratio,'regression':ratio>limit,'noisy':a['coefficient_of_variation']>.25}
    @staticmethod
    def scaling(rows,limit):return []
    @staticmethod
    def source_identity(root):return 'current-source'
    @staticmethod
    def calibrate(checked,case,mode,window,cap,before):return 1
    class CheckedTimingWorker:
        instances=[]
        def __init__(self,worker,case,mode,before):self.worker=worker;self.active=True;self.__class__.instances.append(self)
        def request(self,req):
            if not self.active:raise ValueError('expired')
            return self.worker.request(req)
        def invalidate(self):self.active=False

CASE={'id':'original','source':'x','attributes':[{'name':'x','value':'a'},{'name':'x','value':'b'}]}
def reply(case=CASE,n=0):return {'id':case['id'],'iterations':n,'version':2,'phase':'correctness'if n==0 else'timing','operation_checksum':7,'projection':{'blocks':['x']},'html':'<p>x</p>','elapsed_ns':30_000_000}
def baseline_row(case=CASE):
    proof=FakeP.correctness_proof([reply()])[0]
    return {'case':copy.deepcopy(case),'mode':'parse','request_sha256':FakeP.case_identity(case),'status':'slow','reference':FakeP.distribution([30e6]*5,1,20_000_000),'correctness':{'before':[{},proof],'after':[{},copy.deepcopy(proof)]}}

class CacheTests(unittest.TestCase):
    def test_request_key_is_semantic_not_id(self):
        a=copy.deepcopy(CASE);a['id']='renamed'
        self.assertEqual(c.key(a,'parse',FakeP),c.key(CASE,'parse',FakeP))
        self.assertNotEqual(c.key(a,'parse',FakeP),c.key(a,'render',FakeP))
        a['attributes'].reverse();self.assertNotEqual(c.key(a,'parse',FakeP),c.key(CASE,'parse',FakeP))
        a=copy.deepcopy(CASE);a['includes']={};self.assertNotEqual(c.key(a,'parse',FakeP),c.key(CASE,'parse',FakeP))
    def test_candidate_failure_does_not_reject_reference(self):
        row=baseline_row();row['status']='noisy'
        c.usable_reference(row)
        row['reference']['coefficient_of_variation']=.5;row['reference']['observed_minimum_window_ns']=1
        self.assertEqual(len(c.usable_reference(row)[1]),2)
    def test_missing_reference_samples_are_not_passes(self):
        row=baseline_row();row.pop('reference');row['status']='error'
        with self.assertRaisesRegex(ValueError,'samples unavailable'):c.usable_reference(row)
    def test_reference_drift_rejected(self):
        row=baseline_row();row['correctness']['after'][1]['html_sha256']='0'*64
        with self.assertRaisesRegex(ValueError,'drifted'):c.usable_reference(row)
    def test_native_semantic_drift_rejected(self):
        proof=c.reference_proof(baseline_row());v=reply();v['html']='changed'
        with self.assertRaisesRegex(ValueError,'DOM differs'):c.check_native(v,CASE,'parse',proof,FakeP)
    def test_duplicate_semantic_conflict_is_not_silently_selected(self):
        a=baseline_row();b=copy.deepcopy(a)
        b['correctness']['before'][1]['projection_sha256']='0'*64;b['correctness']['after'][1]['projection_sha256']='0'*64
        with self.assertRaisesRegex(ValueError,'disagree'):c.select_reference([a,b])
    def test_native_context_expires_even_on_sampling_error(self):
        worker=types.SimpleNamespace(request=lambda req: (_ for _ in ()).throw(ValueError('broken')))
        with self.assertRaises(ValueError):c.native_samples(worker,CASE,'parse',reply(),FakeP,20_000_000,5,100000)
        self.assertFalse(FakeP.CheckedTimingWorker.instances[-1].active)
    def test_main_uses_only_current_native_worker_and_cached_reference(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);(root/'scripts/asciidoc').mkdir(parents=True)
            for rel in ['scripts/check_asciidoc_performance.py','scripts/review_asciidoc.py','scripts/review_roc_syntax.py','scripts/asciidoc/oracle.rb','scripts/asciidoc/performance_oracle.rb']:(root/rel).write_text('fixture')
            binary=root/'native';binary.write_bytes(b'native');compiler=root/'compiler';compiler.write_bytes(b'compiler')
            receipt={'source_identity':'current-source','binary_sha256':c.sha(binary),'build_flags':['--opt=speed'],'compiler':{'path':str(compiler),'sha256':c.sha(compiler)}}
            Path(str(binary)+'.build.json').write_text(json.dumps(receipt))
            row=baseline_row();saved={'schema_version':2,'smoke':False,'candidate_build':{'source_identity':'old-native'},'reference':{'version':'2.0.23','gem_code_sha256':'old-code','worker_sha256':c.sha(root/'scripts/asciidoc/performance_oracle.rb'),'oracle_sha256':c.sha(root/'scripts/asciidoc/oracle.rb')},'machine':c.machine(),'records':[row]}
            missing=baseline_row({'id':'unavailable','source':'other'});missing.pop('reference');missing['status']='error'
            saved['records'].append(missing);saved['full_campaign_complete']=False
            baseline=root/'saved.json';baseline.write_text(json.dumps(saved));commands=[]
            class Worker:
                def __init__(self,command,**kwargs):commands.append(command)
                def request(self,req):return reply(CASE,req['iterations'])
                def close(self):pass
            def build(args,out):
                self.assertEqual(args.roc,'pinned-roc')
                target=out/'performance-native';target.write_bytes(b'native')
                target.with_suffix('.build.json').write_text(json.dumps(receipt))
                return target,receipt
            loader=types.SimpleNamespace(exec_module=lambda mod:mod.__dict__.update({k:getattr(FakeP,k) for k in dir(FakeP) if not k.startswith('_')},Worker=Worker,build=build,__file__=str(root/'scripts/check_asciidoc_performance.py')))
            spec=types.SimpleNamespace(loader=loader)
            with patch.object(c.importlib.util,'spec_from_file_location',return_value=spec),patch.object(c.importlib.util,'module_from_spec',return_value=types.ModuleType('fake')),patch.object(subprocess,'Popen',side_effect=AssertionError('No external process allowed')):
                code=c.main(['--root',str(root),'--baseline',str(baseline),'--baseline-sha256',c.sha(baseline),'--candidate-binary',str(binary),'--output',str(root/'out'),'--modes','parse'])
            self.assertEqual(code,0);self.assertEqual(commands,[[str(binary.resolve())]])
            result=json.loads((root/'out/results.json').read_text());self.assertFalse(result['fresh_strict_oracle_qualification']);self.assertEqual(result['reference'],saved['reference']);self.assertEqual(result['records'][0]['reference'],row['reference'])
            self.assertEqual(result['baseline_report_sha256'],c.sha(baseline));self.assertTrue(result['exact_coverage']);self.assertEqual(len(result['reference_unavailable']),1);self.assertEqual(result['reference_unavailable'][0]['case']['id'],'unavailable');self.assertEqual(result['selected_requests'],2);self.assertFalse(result['original_run_metadata']['full_campaign_complete'])
            with patch.object(c.importlib.util,'spec_from_file_location',return_value=spec),patch.object(c.importlib.util,'module_from_spec',return_value=types.ModuleType('fake2')),patch.object(subprocess,'Popen',side_effect=AssertionError('No external process allowed')):
                code=c.main(['--root',str(root),'--baseline',str(baseline),'--roc','pinned-roc','--output',str(root/'auto'),'--modes','parse'])
            self.assertEqual(code,0);self.assertEqual(commands[-1],[str((root/'auto/performance-native').resolve())])

class RequestIdentityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        scripts=Path(c.__file__).resolve().parent
        sys.path.insert(0,str(scripts))
        spec=importlib.util.spec_from_file_location('real_cached_test_driver',scripts/'check_asciidoc_performance.py')
        cls.p=importlib.util.module_from_spec(spec);spec.loader.exec_module(cls.p)
    def test_real_request_hash_preserves_ordered_attrs_and_include_expansion(self):
        a={'id':'a','source':'{x}','includes':{'b':'B','a':'A'},'attributes':[{'name':'x','value':'first'},{'name':'x','value':'last'}]}
        b=copy.deepcopy(a);b['id']='renamed';b['includes']={'a':'A','b':'B'}
        self.assertEqual(self.p.case_identity(a),self.p.case_identity(b))
        self.assertEqual(self.p.request_identity(self.p.request(a,'render',0)),self.p.case_identity(a))
        b['attributes'].reverse();self.assertNotEqual(self.p.case_identity(a),self.p.case_identity(b))
        self.assertNotEqual(self.p.case_identity({'id':'a','source':'x'}),self.p.case_identity({'id':'a','source':'x','includes':{}}))
        self.assertNotEqual(c.key(a,'parse',self.p),c.key(a,'render',self.p))

if __name__=='__main__':unittest.main()
