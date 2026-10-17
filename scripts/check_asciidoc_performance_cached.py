#!/usr/bin/env python3
"""Fast LOCAL guidance: fresh native samples against an immutable saved Ruby run.

No Ruby process is launched. Ratios are approximate; scaling warnings matter more
than tiny parity differences. Use the fresh driver when refreshing the baseline.
"""
import argparse, hashlib, importlib.util, json, math, os, platform, sys
from pathlib import Path

PURPOSE='cached-reference-development'
def sha(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as f:
        for chunk in iter(lambda:f.read(1024*1024),b''):h.update(chunk)
    return h.hexdigest()
def encoded(v):return json.dumps(v,sort_keys=True,ensure_ascii=False,separators=(',',':')).encode()
def value_sha(v):return hashlib.sha256(encoded(v)).hexdigest()
def require(ok,message):
    if not ok:raise ValueError(message)
def machine():return {'system':platform.platform(),'processor':platform.processor(),'cores':os.cpu_count()}
def key(case,mode,p):return (p.case_identity(case),mode)
def reference_proof(row):
    c=row.get('correctness',{});before=c.get('before');after=c.get('after')
    require(isinstance(before,list) and isinstance(after,list) and len(before)==len(after)==2,'cached reference before/after proofs unavailable')
    b=before[1];require(b==after[1] and not c.get('after_error'),'cached reference semantic proof drifted')
    require(all(isinstance(b.get(k),str) and len(b[k])==64 for k in ('projection_sha256','rendered_dom_sha256','html_sha256')),'reference correctness hashes missing')
    return b

def usable_reference(row):
    """Reference quality is annotated, not gated on the old candidate's speed."""
    proof=reference_proof(row);ref=row.get('reference',{});samples=ref.get('samples_ns',[])
    require(isinstance(samples,list) and samples and all(type(x) in (int,float) and math.isfinite(x) and x>0 for x in samples),'usable positive reference samples unavailable; refresh this request')
    require(type(ref.get('median_ns')) in (int,float) and math.isfinite(ref['median_ns']) and ref['median_ns']>0,'reference median unavailable')
    warnings=[]
    if ref.get('coefficient_of_variation',1)>.25:warnings.append('saved Ruby samples are noisy')
    if ref.get('observed_minimum_window_ns',0)<ref.get('minimum_window_ns',1):warnings.append('saved Ruby samples are short')
    if row.get('status')=='error':warnings.append('original row errored; only its present reference samples/proofs are reused')
    return proof,warnings

def baseline_index(report,p):
    require(report.get('schema_version')==2 and report.get('smoke') is False,'use a saved non-smoke performance results.json')
    require(report.get('reference',{}).get('version')=='2.0.23' and report['reference'].get('gem_code_sha256'),'reference identity missing')
    index={}
    for row in report.get('records',[]):
        k=key(row['case'],row['mode'],p)
        require(row.get('request_sha256')==k[0],'baseline fingerprint contradicts actual request')
        index.setdefault(k,[]).append(row)
    return index

def select_reference(rows):
    available=[];unavailable=[]
    for row in rows:
        try:
            proof,warnings=usable_reference(row);available.append((row,proof,warnings))
        except ValueError as e:unavailable.append(str(e))
    require(available,'No reusable Ruby measurement for this exact request/mode: '+'; '.join(unavailable))
    require(all(v[1]==available[0][1] for v in available),'duplicate cached requests disagree semantically')
    row,proof,warnings=available[0]
    return row,proof,warnings,{'matching_rows':len(rows),'unavailable_rows':unavailable,'selection':'first usable measurement in immutable report order'}

def check_native(reply,case,mode,proof,p):
    p.validate_reply(reply,case,0,mode=mode,warmup=0)
    actual=p.correctness_proof([reply])[0]
    require(actual['projection_sha256']==proof['projection_sha256'],'fresh native projection differs from saved Ruby')
    require(actual['rendered_dom_sha256']==proof['rendered_dom_sha256'],'fresh native DOM differs from saved Ruby')
    return actual

def native_samples(worker,case,mode,before,p,window,samples,cap):
    checked=p.CheckedTimingWorker(worker,case,mode,before)
    try:
        count=p.calibrate(checked,case,mode,window,cap,before)
        checked.request(p.request(case,mode,count))
        raw=[checked.request(p.request(case,mode,count))['elapsed_ns'] for _ in range(samples)]
        return p.distribution(raw,count,window),{'iterations':count,'elapsed_ns':raw}
    finally:checked.invalidate()

def main(argv=None):
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--root',type=Path,default=Path(__file__).resolve().parents[1])
    ap.add_argument('--baseline',required=True,type=Path)
    ap.add_argument('--baseline-sha256',help='optional expected content address; actual SHA is always recorded')
    ap.add_argument('--candidate-binary',type=Path,help='reuse genuine optimized binary; omitted builds native only')
    ap.add_argument('--roc',default='roc')
    ap.add_argument('--build-timeout',type=float,default=180)
    ap.add_argument('--output',required=True,type=Path)
    ap.add_argument('--replay',type=Path)
    ap.add_argument('--id',action='append',default=[])
    ap.add_argument('--modes',nargs='+',choices=['parse','render'],default=['parse','render'])
    ap.add_argument('--window-ms',type=float,default=20)
    ap.add_argument('--samples',type=int,default=5)
    ap.add_argument('--max-iterations',type=int,default=100000)
    a=ap.parse_args(argv);a.root=a.root.resolve()
    require(not a.output.exists(),'new output required')
    require(math.isfinite(a.window_ms) and a.window_ms>0 and math.isfinite(a.build_timeout) and a.build_timeout>0 and 1<=a.samples<=101 and 1<=a.max_iterations<=100000,'invalid sample policy')
    actual_baseline_sha=sha(a.baseline)
    require(a.baseline_sha256 is None or actual_baseline_sha==a.baseline_sha256,'saved report content address changed')
    a.baseline_sha256=actual_baseline_sha
    sys.path.insert(0,str(a.root/'scripts'))
    spec=importlib.util.spec_from_file_location('cached_dev_driver',a.root/'scripts/check_asciidoc_performance.py');p=importlib.util.module_from_spec(spec);spec.loader.exec_module(p)
    report=json.loads(a.baseline.read_text());index=baseline_index(report,p)
    require('candidate_build' in report and 'machine' in report,'saved provenance missing')
    cases=json.loads(a.replay.read_text())['cases'] if a.replay else [r['case'] for r in report['records'] if r['mode']==a.modes[0]]
    if a.id:
        wanted=set(a.id);cases=[c for c in cases if c['id'] in wanted]
        require({c['id'] for c in cases}==wanted,'requested ID missing')
    require(cases,'no selected cases')
    selected=[];unavailable=[]
    for case in cases:
        for mode in a.modes:
            k=key(case,mode,p)
            require(k in index,'new/changed request is absent from cache: '+case['id'])
            # Preserve explicit replay IDs; duplicates are disclosed, never identity keys.
            try:
                row,proof,quality,selection=select_reference(index[k])
                selected.append((case,mode,row,proof,quality,selection))
            except ValueError as error:
                unavailable.append({'case':case,'mode':mode,'request_sha256':k[0],'status':'reference-unavailable','reason':str(error)})
                print(case['id'],mode,'REFERENCE UNAVAILABLE',str(error),flush=True)
    require(selected,'No usable saved reference records; refresh selected requests')
    reference=report['reference'];original_build=report['candidate_build'];old_machine=report['machine']
    original_scope={k:v for k,v in report.items() if k not in ('records','scaling','failures')}
    del report,index,cases
    a.output.mkdir()
    if a.candidate_binary is None:
        a.smoke=False
        a.candidate_binary,_=p.build(a,a.output)
    receipt_path=a.candidate_binary.with_suffix('.build.json');build=json.loads(receipt_path.read_text())
    require(build['source_identity']==p.source_identity(a.root) and build['binary_sha256']==sha(a.candidate_binary),'candidate receipt/source mismatch')
    require(build['build_flags']==['--opt=speed'] and sha(build['compiler']['path'])==build['compiler']['sha256'],'candidate build flags/compiler mismatch')
    tracked=[Path(__file__),a.baseline,a.candidate_binary,receipt_path,Path(p.__file__),a.root/'scripts/review_roc_syntax.py',a.root/'scripts/review_asciidoc.py',Path(build['compiler']['path'])]
    if a.replay:tracked.append(a.replay)
    inputs={str(f):sha(f) for f in tracked};source=build['source_identity']
    require(inputs[str(a.baseline)]==actual_baseline_sha,'saved report changed during setup')
    warnings=['Saved Ruby timings are historical, not a fresh paired or strict qualification. Host load, runtime and thermal differences can shift rough ratios.']
    if old_machine!=machine():warnings.append('Current machine description differs from saved Ruby run')
    for name,rel in [('worker_sha256','scripts/asciidoc/performance_oracle.rb'),('oracle_sha256','scripts/asciidoc/oracle.rb')]:
        if reference.get(name)!=sha(a.root/rel):warnings.append('Current '+rel+' differs from saved reference code; refresh advised')
    out={'purpose':PURPOSE,'fresh_strict_oracle_qualification':False,'candidate_build':build,'original_candidate_build':original_build,'reference':reference,'baseline_report_sha256':a.baseline_sha256,'baseline_machine':old_machine,'current_machine':machine(),'original_run_metadata':original_scope,'reference_unavailable':unavailable,'warnings':warnings,'records':[],'errors':[],'checked_inputs_before':inputs}
    worker=None
    try:
        worker=p.Worker([str(a.candidate_binary.resolve())],timeout=180)
        with (a.output/'records.jsonl').open('x') as journal:
            for ordinal,(case,mode,old,proof,quality,selection) in enumerate(selected):
                row={'index':ordinal,'case':case,'mode':mode,'request_sha256':key(case,mode,p)[0],'reference':old['reference'],'reference_proof':proof,'reference_quality_warnings':quality,'reference_selection':selection,'original_status':old.get('status'),'original_error':old.get('error'),'original_qualification':old.get('timing_qualification'),'original_attempts':old.get('measurement_attempts')}
                row['reference_content_key']=value_sha({'report':a.baseline_sha256,'request':row['request_sha256'],'mode':mode,'reference':row['reference'],'proof':proof})
                before=None
                try:
                    before=worker.request(p.request(case,mode,0));row['before']=before
                    row['fresh_before_proof']=check_native(before,case,mode,proof,p)
                    row['candidate'],row['native_attempt']=native_samples(worker,case,mode,before,p,int(a.window_ms*1e6),a.samples,a.max_iterations)
                    row['comparison']=p.ratchet(row['candidate'],row['reference'],1.0)
                    row['native_quality_warnings']=['quick native samples are noisy'] if row['candidate']['coefficient_of_variation']>.25 else []
                    ratio=row['comparison']['ratio']
                    row['guidance']='much faster' if ratio<.75 else 'much slower' if ratio>1.25 else 'near parity; do not chase small differences'
                    row['status']='measured'
                except Exception as e:row.update(status='error',error=repr(e))
                finally:
                    try:
                        after=worker.request(p.request(case,mode,0));row['after']=after
                        row['fresh_after_proof']=check_native(after,case,mode,proof,p)
                        require(before is not None and before['projection']==after['projection'] and before['html']==after['html'],'native self drift')
                        require(before['version']==after['version'] and before.get('operation_checksum')==after.get('operation_checksum'),'native operation checksum drift')
                    except Exception as e:row.update(status='error',post_check_error=repr(e))
                if row['status']=='error':out['errors'].append({'id':case['id'],'mode':mode,'error':row.get('error'),'post_check_error':row.get('post_check_error')})
                # Compare full outputs first, then retain their full-field hashes to bound memory.
                if row['status']=='measured':
                    for field in ('before','after'):
                        v=row[field];row[field]={'full_reply_sha256':value_sha(v),'rpc_metadata':{k:z for k,z in v.items() if k not in ('html','projection')}}
                out['records'].append(row);journal.write(json.dumps(row,ensure_ascii=False)+'\n');journal.flush()
                print(case['id'],mode,row.get('guidance',row.get('error')),row.get('comparison',{}).get('ratio'),flush=True)
    finally:
        if worker:worker.close()
        out['checked_inputs_after']={str(f):sha(f) for f in tracked}
        out['inputs_unchanged']=out['checked_inputs_after']==inputs
        out['source_unchanged']=p.source_identity(a.root)==source
        out['exact_coverage']=[(r['index'],r['request_sha256'],r['mode']) for r in out['records']]==[(i,key(c,m,p)[0],m) for i,(c,m,*_) in enumerate(selected)]
        out['selected_requests']=len(selected)+len(unavailable)
        out['attempted_requests']=len(out['records'])
        out['measured_requests']=sum(r['status']=='measured' for r in out['records'])
        out['scaling']=p.scaling([r for r in out['records'] if r['status']=='measured'],1.3)
        out['scaling_warnings']=[c for c in out['scaling'] if c['regression']]
        out['status']='complete' if out['inputs_unchanged'] and out['source_unchanged'] and out['exact_coverage'] and not out['errors'] else 'failed'
        (a.output/'results.json').write_text(json.dumps(out,indent=2,ensure_ascii=False)+'\n')
        print('SCALING WARNINGS',json.dumps(out['scaling_warnings']),flush=True)
    return 0 if out['status']=='complete' else 1
if __name__=='__main__':raise SystemExit(main())
