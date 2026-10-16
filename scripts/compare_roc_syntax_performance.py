#!/usr/bin/env python3
"""Local per-input Roc/reference parse timing ratchet; --smoke checks protocol only."""
from __future__ import annotations
import argparse
import hashlib
import json
import math
from pathlib import Path
import shlex
import statistics
import subprocess
import sys
import bench_roc_syntax as geometric
import review_roc_syntax as review
from roc_syntax.build_oracle import ROC_REVISION

ROOT = Path(__file__).resolve().parents[1]


def inputs(sizes, document_bytes, fixtures=(), smoke=False):
    if smoke:
        return [{'id': 'smoke/file', 'mode': 'file', 'source': 'module []\nanswer = 42\n'},
                {'id': 'smoke/expression', 'mode': 'expr', 'source': 'add(1, 2 * 3)'}]
    result = [{'id': f'{family}/{size}', 'mode': 'file', 'source': data.decode()}
              for family, size, data in geometric.corpus(sizes)]
    for target in document_bytes:
        lines = ['module []\n']; length = len(lines[0]); index = 0
        while length < target:
            line = f'x{index} = {index} + 2 * 3 # representative source padding for sibling scaling\n'
            lines.append(line); length += len(line); index += 1
        result.append({'id': f'document/{target}', 'mode': 'file', 'source': ''.join(lines)})
    for path in fixtures:
        content = json.loads(Path(path).read_text())
        for case in content['cases']:
            result.append({key: case[key] for key in ('id', 'mode', 'source')})
    # Identical sources in different fixtures need one timing, with mode preserved.
    seen = set(); unique = []
    for case in result:
        key = case['mode'], case['source']
        if key not in seen:
            seen.add(key); unique.append(case)
    return unique


def shape_agrees(candidate, reference):
    pending = [(candidate, reference)]
    cutoffs = 0
    limits = {'depth_limit', 'type_depth_limit', 'statement_depth_limit', 'pattern_depth_limit', 'target_depth_limit'}
    while pending:
        a, b = pending.pop()
        if isinstance(b, list) and len(b) == 2 and b[0] == 'unsupported' and isinstance(b[1], str) and b[1] in limits:
            cutoffs += 1
            continue
        if isinstance(a, list) and isinstance(b, list):
            if len(a) != len(b) or (a and a[0] == 'unsupported') or (b and b[0] == 'unsupported'):
                raise ValueError('candidate/reference AST projections differ or are incomplete')
            pending.extend(zip(a, b))
        elif a != b:
            raise ValueError('candidate/reference AST projections differ')
    return cutoffs


def correctness(case, candidate, reference):
    for label, value in [('candidate', candidate), ('reference', reference)]:
        if value.get('error') or not value.get('consumed'):
            raise ValueError(f'{label} did not fully consume input: {value.get("error")}')
    if candidate.get('ok') != reference.get('ok'):
        raise ValueError('candidate/reference acceptance differs')
    review.validate_spans(candidate, case['source'], lossless=True)
    cutoffs = 0
    if candidate['ok']:
        cutoffs = shape_agrees(review.syntax_shape(candidate, case['source']), review.syntax_shape(reference, case['source']))
    return {'accepted': candidate['ok'], 'projection_depth_cutoffs': cutoffs,
            'structural_check': 'partial_depth_limited' if cutoffs else 'complete' if candidate['ok'] else 'rejected_input'}


def measure(worker, case, accepted, window_ns, samples, max_iterations=100000):
    def batch(iterations, warmup=0):
        request = dict(case, benchmark_iterations=iterations, benchmark_warmup=warmup)
        value = worker.request(request)
        if value.get('error'):
            raise ValueError(f'benchmark protocol error: {value["error"]}')
        if value.get('iterations') != iterations or value.get('successes') != (iterations if accepted else 0) or value.get('consumed') != iterations:
            raise ValueError(f'benchmark correctness/count changed: {value}')
        elapsed = value.get('elapsed_ns')
        if not isinstance(elapsed, int) or elapsed < 0:
            raise ValueError(f'invalid elapsed time: {elapsed}')
        return elapsed
    iterations = 1
    while True:
        elapsed = batch(iterations, 2)
        if elapsed >= window_ns:
            break
        if iterations == max_iterations:
            raise ValueError('measurement window unavailable at iteration cap')
        iterations = min(max_iterations, max(iterations + 1, iterations * 2 if elapsed == 0 else int(iterations * window_ns / elapsed * 1.1)))
    batch(iterations)  # Full calibrated warm-up batch, excluded from samples.
    values = [batch(iterations) / iterations for _ in range(samples)]
    if min(values) <= 0:
        raise ValueError('zero-resolution measurement after calibration')
    return {'iterations': iterations, 'samples_ns': values, 'median_ns': statistics.median(values),
            'minimum_window_ns': window_ns}


def ratios(candidate, reference, max_ratio, floor_ns):
    # Gate the conservative lower bound, so a noisy reference sample cannot
    # alone fail the ratchet. Retain the median/raw values for local analysis.
    median = candidate['median_ns'] / max(reference['median_ns'], floor_ns)
    lower = min(candidate['samples_ns']) / max(max(reference['samples_ns']), floor_ns)
    return {'reference_ratio': median, 'reference_ratio_lower_bound': lower,
            'reference_ratio_limit': max_ratio, 'reference_floor_ns': floor_ns,
            'regression': lower > max_ratio}


def positive_finite(text):
    value = float(text)
    if not math.isfinite(value) or value <= 0:
        raise argparse.ArgumentTypeError('must be finite and positive')
    return value


def bounded_samples(text):
    value = int(text)
    if not 1 <= value <= 101:
        raise argparse.ArgumentTypeError('must be between 1 and 101')
    return value


def parser():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--roc', default='roc')
    ap.add_argument('--candidate-binary', type=Path, help='reuse optimized parse-only Roc driver')
    ap.add_argument('--probe-command', required=True, help='persistent correctness worker')
    ap.add_argument('--oracle-command', required=True, help='optimized reference correctness/benchmark worker')
    ap.add_argument('--reference-build-info', type=Path, help='oracle build metadata; defaults to executable sibling .json')
    ap.add_argument('--replay', type=Path, action='append', default=[], help='time only retained failure request(s)')
    ap.add_argument('--fixtures', type=Path, action='append', default=[])
    ap.add_argument('--sizes', type=int, nargs='+', default=[256, 512, 1024, 2048, 4096])
    ap.add_argument('--document-bytes', type=int, nargs='+', default=[4096, 65536, 262144])
    ap.add_argument('--max-reference-ratio', type=positive_finite, default=1.5)
    ap.add_argument('--reference-floor-ns', type=positive_finite, default=1000.0)
    ap.add_argument('--window-ms', type=positive_finite, default=20.0)
    ap.add_argument('--samples', type=bounded_samples, default=5)
    ap.add_argument('--smoke', action='store_true', help='two tiny inputs, one 1ms sample, no timing-ratio assertion')
    ap.add_argument('--timeout', type=positive_finite, default=30.0)
    ap.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp/reference-performance/results.json')
    return ap


def main(argv=None):
    args = parser().parse_args(argv)
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 30000))
    if not all(math.isfinite(value) and value > 0 for value in [args.max_reference_ratio, args.reference_floor_ns, args.window_ms, args.timeout]) or not 1 <= args.samples <= 101:
        raise SystemExit('ratio, timing floor, window and samples must be positive')
    reference_command = shlex.split(args.oracle_command)
    metadata_path = args.reference_build_info or Path(reference_command[0]).with_suffix('.json')
    reference_metadata = json.loads(metadata_path.read_text())
    reference_binary = Path(reference_command[0])
    if reference_metadata.get('compiler_revision') != ROC_REVISION:
        raise SystemExit('reference compiler revision differs from the immutable syntax pin')
    if reference_metadata.get('binary_sha256') != hashlib.sha256(reference_binary.read_bytes()).hexdigest():
        raise SystemExit('reference executable does not match build metadata')
    expected_optimization = {'ReleaseSafe', 'ReleaseFast'} if args.smoke else {'ReleaseFast'}
    if reference_metadata.get('optimize') not in expected_optimization:
        raise SystemExit('reference performance comparison requires a verified ReleaseFast oracle (smoke also permits ReleaseSafe)')
    source_hash = geometric.source_identity(ROOT)
    binary = (args.candidate_binary or ROOT / '.roc-parser-tmp/reference-performance/driver').resolve()
    if not args.candidate_binary:
        binary.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([args.roc, 'build', '--no-cache', '--opt=speed', str(ROOT/'bench/roc/roc-syntax-compare.roc'), f'--output={binary}'], check=True)
    cases = inputs(sorted(args.sizes), args.document_bytes, args.fixtures, args.smoke)
    if args.replay and not args.smoke:
        cases = [json.loads(path.read_text())['request'] for path in args.replay]
    try:
        compiler = subprocess.run([args.roc, 'version'], capture_output=True, text=True, check=True).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        compiler = None
    report = {'version': 1, 'hardware': geometric.machine_identity(), 'roc_compiler': compiler, 'timing_scope': 'tokenize + parse + result disposal; shared source bytes; O(1) result observation; excludes JSON, encoding, startup and I/O',
              'source_ownership': 'file/expr shared immutable UTF-8 byte list prepared before timing; statement/header string API; repl compiler-compatible dispatch',
              'ratio_assertion': not args.smoke, 'smoke': args.smoke, 'source_identity': source_hash,
              'candidate_binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
              'candidate_reused': bool(args.candidate_binary), 'candidate_build_flags': None if args.candidate_binary else ['--no-cache', '--opt=speed'],
              'oracle_command': reference_command, 'reference_build': reference_metadata, 'records': [], 'failures': []}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with review.Worker(shlex.split(args.probe_command), timeout=args.timeout) as probe, review.Worker(shlex.split(args.oracle_command), timeout=args.timeout) as oracle, review.Worker([str(binary)], timeout=args.timeout) as candidate:
        for index, case in enumerate(cases):
            digest = hashlib.sha256(case['source'].encode()).hexdigest()
            record = {'id': case['id'], 'mode': case['mode'], 'bytes': len(case['source'].encode()), 'sha256': digest}
            fatal = False
            try:
                record.update(correctness(case, probe.request(case), oracle.request(case)))
                accepted = record['accepted']
                window = 1_000_000 if args.smoke else args.window_ms * 1e6
                samples = 1 if args.smoke else args.samples
                # Alternate ordering to avoid always giving one parser a warm CPU.
                order = [('candidate', candidate), ('reference', oracle)]
                if index % 2: order.reverse()
                for label, worker in order:
                    record[label] = measure(worker, case, accepted, window, samples)
                record.update(ratios(record['candidate'], record['reference'], args.max_reference_ratio, args.reference_floor_ns))
                if record['regression'] and not args.smoke:
                    raise ValueError(f'parse runtime ratio {record["reference_ratio_lower_bound"]:.3f} exceeds {args.max_reference_ratio}')
            except Exception as error:
                fatal = isinstance(error, review.WorkerError)
                failure = dict(record, error=str(error))
                artifact = args.output.parent / 'failures' / f'{digest}-{case["mode"]}.json'
                artifact.parent.mkdir(parents=True, exist_ok=True)
                artifact.write_text(json.dumps({'request': case, 'failure': failure}, indent=2) + '\n')
                failure['reproducer'] = str(artifact)
                report['failures'].append(failure)
            report['records'].append(record)
            if fatal:
                break  # A timed-out stream cannot safely serve another request.
    if source_hash != geometric.source_identity(ROOT):
        report['failures'].append({'error': 'candidate source changed during benchmark'})
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'cases': len(report['records']), 'requested_cases': len(cases), 'failures': len(report['failures']), 'ratio_assertion': not args.smoke, 'output': str(args.output)}))
    return 1 if report['failures'] else 0

if __name__ == '__main__':
    sys.exit(main())
