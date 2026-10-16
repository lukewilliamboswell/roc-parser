#!/usr/bin/env python3
"""Optimized Roc-syntax timing/allocation baseline and geometric scaling lane."""
from __future__ import annotations
import argparse
import hashlib
import json
from pathlib import Path
import platform
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]

def corpus(sizes=(256, 512, 1024, 2048)):
    for family, make in {
        'definitions': lambda n: 'module []\n' + ''.join(f'x{i} = {i} + 2 * 3\n' for i in range(n)),
        'comment': lambda n: 'module []\n#' + 'x' * n + '\nanswer = 1\n',
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
        if family in prior:
            previous = prior[family]
            # Normalize by actual source-byte growth, not generator parameter.
            if record.get('status') == 'Limited' or previous.get('status') == 'Limited':
                prior[family] = record
                continue
            ratio = record['median_ns'] / previous['median_ns']
            growth = record['bytes'] / previous['bytes']
            if ratio > growth * max_ratio:
                failures.append({'family': family, 'size': record['size'], 'time_ratio': ratio, 'byte_ratio': growth})
        prior[family] = record
    return failures

def baseline_failures(records, baseline, max_regression):
    old = {(r['family'], r['size']): r for r in baseline['records']}
    failures = []
    for record in records:
        previous = old.get((record['family'], record['size']))
        if previous and previous['sha256'] == record['sha256']:
            if record['median_ns'] > previous['median_ns'] * max_regression:
                failures.append({'family': record['family'], 'size': record['size'],
                                 'ratio': record['median_ns'] / previous['median_ns']})
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

def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--roc', default='roc')
    ap.add_argument('--binary', type=Path, help='reuse optimized driver')
    ap.add_argument('--allocation-binary', type=Path, help='existing bench/roc/alloc.roc --fuzz executable')
    ap.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp/roc-syntax-bench/results.json')
    ap.add_argument('--baseline', type=Path)
    ap.add_argument('--max-regression', type=float, default=1.5)
    ap.add_argument('--max-scaling-overhead', type=float, default=1.75, help='allowed time growth beyond byte growth')
    ap.add_argument('--sizes', type=int, nargs='+', default=[256, 512, 1024, 2048])
    ap.add_argument('--document-bytes', type=int, nargs='+', default=[4096, 65536, 262144])
    ap.add_argument('--repetitions', type=int, default=5)
    ap.add_argument('--target-ms', type=float, default=50)
    args = ap.parse_args(argv)
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
    metadata = {'machine': platform.platform(), 'cpu': platform.machine(), 'timestamp': time.time(),
                'binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
                'compiler': subprocess.run([args.roc, 'version'], capture_output=True, text=True, check=True).stdout.strip(),
                'build_flags': None if args.binary else ['--no-cache', '--opt=speed'],
                'candidate_source_sha256': hashlib.sha256((ROOT / 'package/RocSyntax.roc').read_bytes()).hexdigest(),
                'candidate_commit': subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=ROOT, capture_output=True, text=True, check=True).stdout.strip(),
                'reused_binary': bool(args.binary), 'input_ownership': 'shared in-memory source across iterations; fresh parse result each iteration', 'allocation_metric': 'roc_alloc + roc_realloc calls; not byte volume or peak memory'}
    failures = scaling_failures(records, args.max_scaling_overhead)
    if args.baseline:
        baseline = json.loads(args.baseline.read_text())
        if baseline['metadata']['machine'] != metadata['machine'] or baseline['metadata']['compiler'] != metadata['compiler']:
            raise RuntimeError('baseline machine/compiler differs; record a separate baseline')
        failures += baseline_failures(records, baseline, args.max_regression)
    report = {'metadata': metadata, 'records': records, 'failures': failures}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'cases': len(records), 'failures': len(failures), 'output': str(args.output)}))
    return int(bool(failures))

if __name__ == '__main__':
    raise SystemExit(main())
