#!/usr/bin/env python3
"""Optimized Roc-syntax timing/allocation baseline and geometric scaling lane."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]

def source_identity(root=ROOT):
    identity = hashlib.sha256()
    paths = sorted([*(root / 'package').glob('*.roc'), *(root / 'bench' / 'roc').glob('roc-syntax*.roc')])
    for path in paths:
        identity.update(path.relative_to(root).as_posix().encode() + b'\0' + path.read_bytes() + b'\0')
    return identity.hexdigest()

def corpus(sizes=(256, 512, 1024, 2048)):
    for family, make in {
        'definitions': lambda n: 'module []\n' + ''.join(f'x{i} = {i} + 2 * 3\n' for i in range(n)),
        'comment': lambda n: 'module []\n#' + 'x' * n + '\nanswer = 1\n',
        'adjacent_upper_calls': lambda n: 'main = { ' + 'A() ' * n + '0 }\n',
        'adjacent_parenthesized': lambda n: 'main = { ' + '(x) ' * n + '0 }\n',
        'operators': lambda n: 'module []\nanswer = ' + ' + '.join(['1'] * n) + '\n',
        'unclosed': lambda n: 'module []\nanswer = ' + '(' * n + '1\n',
    }.items():
        for size in sizes:
            yield family, size, make(size).encode()

def measure(binary, data, repetitions, target_ms):
    def batch(iterations):
        run = subprocess.run([str(binary), str(iterations)], input=data, capture_output=True, timeout=30)
        if run.returncode:
            raise RuntimeError(f'benchmark exit {run.returncode}: {run.stderr[:500]!r}')
        record = json.loads(run.stdout)
        if record['iterations'] != iterations or record['elapsed_ns'] <= 0:
            raise RuntimeError('invalid benchmark timing record')
        return record
    first = batch(1)
    iterations = max(1, min(100000, int(target_ms * 1e6 / first['elapsed_ns'])))
    batch(iterations)
    records = [batch(iterations) for _ in range(repetitions)]
    values = [record['elapsed_ns'] / iterations for record in records]
    median = statistics.median(values)
    return {'median_ns': median, 'samples_ns': values, 'iterations': iterations,
            'bytes_per_second': len(data) * 1e9 / median,
            'accepted': records[0]['successes'] == iterations, 'status': records[0]['status']}

def scaling_failures(records, max_ratio):
    failures = []
    prior = {}
    for record in records:
        family = record['family']
        valid_outcome = (record.get('status') in {'Recovered', 'Limited'} and record.get('accepted') is False
                         if family == 'unclosed'
                         else record.get('status') == 'Complete' and record.get('accepted') is True)
        if not valid_outcome:
            failures.append({'family': family, 'size': record['size'], 'metric': 'parse_outcome',
                             'status': record.get('status'), 'accepted': record.get('accepted'),
                             'expected': 'rejected incomplete source' if family == 'unclosed' else 'Complete and accepted'})
        if family in prior:
            previous = prior[family]
            # Normalize by actual source-byte growth, not generator parameter.
            if family == 'unclosed' and (record.get('status') == 'Limited' or previous.get('status') == 'Limited'):
                prior[family] = record
                continue
            ratio = record['median_ns'] / previous['median_ns']
            growth = record['bytes'] / previous['bytes']
            if ratio > growth * max_ratio:
                failures.append({'family': family, 'size': record['size'], 'time_ratio': ratio, 'byte_ratio': growth})
        prior[family] = record
    return failures

def baseline_failures(records, baseline, max_regression, max_allocation_regression=1.25, *, comparison=None):
    old = {(r['family'], r['size']): r for r in baseline['records']}
    failures = []
    matched = 0
    for record in records:
        previous = old.get((record['family'], record['size']))
        if previous and previous['sha256'] == record['sha256']:
            matched += 1
            if record['median_ns'] > previous['median_ns'] * max_regression:
                failures.append({'family': record['family'], 'size': record['size'],
                                 'metric': 'median_ns', 'ratio': record['median_ns'] / previous['median_ns']})
            if 'allocation_calls' in record and 'allocation_calls' in previous:
                before, after = previous['allocation_calls'], record['allocation_calls']
                if after > before * max_allocation_regression:
                    failures.append({'family': record['family'], 'size': record['size'], 'metric': 'allocation_calls',
                                     'before': before, 'after': after, 'ratio': after / before if before else None})
    counts = {'matched': matched, 'skipped': len(records) - matched, 'baseline_records': len(old)}
    if comparison is not None:
        comparison.update(counts)
    if matched == 0:
        failures.append({'metric': 'baseline_corpus_overlap', **counts,
                         'error': 'baseline has no matching family, size and source checksum'})
    return failures

def allocations(binary, data):
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / 'input'; path.write_bytes(b'roc-syntax\n' + data)
        result = subprocess.run([str(binary), 'replay', str(path)], cwd=tmp, capture_output=True, timeout=30)
    import re
    found = re.search(rb'ALLOCATION_DIAGNOSTIC allocations=(\d+)', result.stderr)
    if result.returncode != 77 or not found:
        raise RuntimeError(f'allocation diagnostic missing: {result.stderr[:500]!r}')
    return int(found[1])

def machine_identity():
    model = None
    system = platform.system()
    if system == 'Darwin':
        try:
            result = subprocess.run(['sysctl', '-n', 'machdep.cpu.brand_string'], capture_output=True, text=True, check=False)
            if result.returncode == 0:
                model = result.stdout.strip() or None
        except OSError:
            pass
    elif system == 'Linux':
        try:
            fields = {}
            for line in Path('/proc/cpuinfo').read_text().splitlines():
                if ':' in line:
                    key, value = line.split(':', 1)
                    fields.setdefault(key.strip(), value.strip())
            model = fields.get('model name') or fields.get('Hardware')
            if not model and fields.get('CPU implementer'):
                model = fields['CPU implementer'] + ':' + fields.get('CPU part', 'unknown')
        except OSError:
            pass
    cores = os.cpu_count()
    return {'os': system, 'release': platform.release(), 'architecture': platform.machine(),
            'cpu_model': model, 'logical_cpus': cores,
            'strength': 'model_and_core_count' if model and cores else 'weak'}


def validate_baseline(metadata, previous, allow_unverified=False):
    current_hardware, old_hardware = metadata.get('hardware'), previous.get('hardware')
    if not allow_unverified and (not current_hardware or not old_hardware or current_hardware.get('strength') == 'weak' or old_hardware.get('strength') == 'weak'):
        raise RuntimeError('baseline hardware identity is weak or unavailable; record a new baseline or explicitly allow unverified comparison')
    if current_hardware != old_hardware or metadata['compiler'] != previous['compiler']:
        raise RuntimeError('baseline CPU/core count/OS/compiler differs; record a separate baseline')
    if not allow_unverified and (metadata.get('reused_binary') or previous.get('reused_binary') or not metadata.get('build_flags') or not previous.get('build_flags')):
        raise RuntimeError('baseline build configuration is unverified; rebuild or explicitly allow unverified comparison')
    if metadata.get('build_flags') and previous.get('build_flags') and metadata['build_flags'] != previous['build_flags']:
        raise RuntimeError('baseline build flags differ; record a separate baseline')


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--roc', default='roc')
    ap.add_argument('--binary', type=Path, help='reuse optimized driver')
    ap.add_argument('--allocation-binary', type=Path, help='existing bench/roc/alloc.roc --fuzz executable')
    ap.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp/roc-syntax-bench/results.json')
    ap.add_argument('--baseline', type=Path)
    ap.add_argument('--allow-unverified-baseline', action='store_true', help='explicitly compare weak hardware/reused binary metadata')
    ap.add_argument('--max-regression', type=float, default=1.5)
    ap.add_argument('--max-allocation-regression', type=float, default=1.25)
    ap.add_argument('--max-scaling-overhead', type=float, default=1.75, help='allowed time growth beyond byte growth')
    ap.add_argument('--sizes', type=int, nargs='+', default=[256, 512, 1024, 2048])
    ap.add_argument('--document-bytes', type=int, nargs='+', default=[4096, 65536, 262144])
    ap.add_argument('--repetitions', type=int, default=5)
    ap.add_argument('--target-ms', type=float, default=50)
    args = ap.parse_args(argv)
    source_hash = source_identity(ROOT)
    binary = args.binary or ROOT / '.roc-parser-tmp/roc-syntax-bench/driver'
    binary = binary.resolve()
    if not args.binary:
        binary.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([args.roc, 'build', '--no-cache', '--opt=speed', str(ROOT/'bench/roc/roc-syntax.roc'), f'--output={binary}'], check=True)
    records = []
    documents = list(corpus(sorted(args.sizes)))
    for target in args.document_bytes:
        lines = ['module []\n']; length = len(lines[0]); index = 0
        while length < target:
            line = f'x{index} = {index} + 2 * 3 # representative source padding for sibling scaling\n'; lines.append(line); length += len(line); index += 1
        documents.append(('sibling-document', target, ''.join(lines).encode()))
    for family, size, data in documents:
        record = {'family': family, 'size': size, 'bytes': len(data), 'sha256': hashlib.sha256(data).hexdigest(),
                  **measure(binary, data, args.repetitions, args.target_ms)}
        if args.allocation_binary:
            record['allocation_calls'] = allocations(args.allocation_binary.resolve(), data)
        records.append(record)
    metadata = {'machine': platform.platform(), 'hardware': machine_identity(), 'cpu': platform.machine(), 'timestamp': time.time(),
                'binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
                'compiler': subprocess.run([args.roc, 'version'], capture_output=True, text=True, check=True).stdout.strip(),
                'build_flags': None if args.binary else ['--no-cache', '--opt=speed'],
                'candidate_source_sha256': source_hash,
                'candidate_source_relationship': 'workspace source; reused binary relationship unverified' if args.binary else 'built from workspace source in this run',
                'candidate_commit': subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=ROOT, capture_output=True, text=True, check=True).stdout.strip(),
                'reused_binary': bool(args.binary), 'input_ownership': 'shared in-memory source across iterations; fresh parse result each iteration', 'allocation_metric': 'roc_alloc + roc_realloc calls; not byte volume or peak memory'}
    failures = scaling_failures(records, args.max_scaling_overhead)
    baseline_comparison = None
    if args.baseline:
        baseline_comparison = {}
        baseline = json.loads(args.baseline.read_text())
        validate_baseline(metadata, baseline['metadata'], args.allow_unverified_baseline)
        failures += baseline_failures(records, baseline, args.max_regression, args.max_allocation_regression, comparison=baseline_comparison)
    if source_hash != source_identity(ROOT):
        raise RuntimeError('candidate source changed during benchmark; rebuild and rerun')
    report = {'metadata': metadata, 'records': records, 'baseline_comparison': baseline_comparison, 'failures': failures}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'cases': len(records), 'failures': len(failures), 'output': str(args.output)}))
    return int(bool(failures))

if __name__ == '__main__':
    raise SystemExit(main())
