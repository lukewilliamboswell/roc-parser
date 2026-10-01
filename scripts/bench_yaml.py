#!/usr/bin/env python3
"""Bounded YAML parse+consume benchmarks. JSON records include source and fixture hashes.

Build optimized debug binaries, then use samply record --save-only -o profile.json
<binary> <iterations> < <fixture> to profile an individual workload.
"""
import argparse
import hashlib
import json
import platform
import re
import statistics
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ROC = Path.home() / 'roc_nightly-macos_apple_silicon-2026-09-29-7f11a82/roc'


def fixtures(n):
    return {
        'mapping': ''.join(f'key{i}: value{i}\n' for i in range(n)),
        'sequence': ''.join(f'- value{i}\n' for i in range(n)),
        'flow': '[' + ', '.join(str(i) for i in range(n)) + ']\n',
        'quoted': 'value: "' + ('abcdefgh ' * n) + '"\n',
        'literal': 'value: |\n' + '  abcdefgh\n' * n,
        'folded': 'value: >\n' + '  abcdefgh\n' * n,
        'repeated_blocks': ''.join(f'key{i}: |-\n  abcdefgh\n' for i in range(n)),
        'repeated_quoted': ''.join(f'key{i}: "abcdefgh"\n' for i in range(n)),
        'invalid_late': ''.join(f'key{i}: value\n' for i in range(n)) + 'broken: [1, 2\n',
        'mixed': ''.join(f'item{i}:\n  enabled: true\n  count: {i}\n  ratio: 1.25\n  values: [one, two, null]\n' for i in range(n)),
        'nested_mapping': ''.join('  ' * i + f'level{i}:\n' for i in range(min(n, 101))) + '  ' * min(n, 101) + 'leaf: value\n',
        'nested_flow': '[' * min(n, 260) + '0' + ']' * min(n, 260) + '\n',
    }


def run_case(binary, data, iterations, timeout):
    result = subprocess.run([str(binary), str(iterations)], input=data,
                            capture_output=True, timeout=timeout, check=True)
    record = json.loads(result.stdout)
    if record['iterations'] != iterations or record['elapsed_ns'] <= 0:
        raise ValueError('Invalid benchmark timing record')
    return record


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--roc', type=Path, default=ROC)
    ap.add_argument('--parser-root', type=Path, default=ROOT / 'package')
    ap.add_argument('--label', default='current')
    ap.add_argument('--output-dir', type=Path, default=ROOT / '.roc-parser-tmp/yaml-bench')
    ap.add_argument('--sizes', default='16,128,512')
    ap.add_argument('--repetitions', type=int, default=5)
    ap.add_argument('--iterations', type=int, default=10)
    ap.add_argument('--timeout', type=float, default=20)
    ap.add_argument('--only', default='')
    ap.add_argument('--allocations', action='store_true', help='Replay diagnostic target and capture allocation calls; expected diagnostic exit is 77')
    args = ap.parse_args()
    if args.repetitions < 1 or args.iterations < 1:
        ap.error('repetitions and iterations must be positive')
    out = args.output_dir.resolve() / args.label
    out.mkdir(parents=True, exist_ok=True)
    parser_root = args.parser_root.resolve()
    if parser_root.is_dir() and (parser_root / 'package').is_dir():
        parser_root /= 'package'
    source = parser_root / 'main.roc' if parser_root.is_dir() else parser_root
    binary = out / 'benchmark'
    cmd = [str(args.roc.expanduser()), 'build', '--debug', '--opt=speed',
           str(ROOT / 'scripts/yaml/benchmark.roc'), '--output=' + str(binary),
          ]
    if source != ROOT / 'package/main.roc':
        cmd += ['--replace-dep', str(ROOT / 'package/main.roc'), str(source)]
    build = subprocess.run(cmd, capture_output=True, text=True)
    (out / 'build.log').write_text(build.stdout + build.stderr)
    if build.returncode:
        raise RuntimeError((build.stdout + build.stderr)[-6000:])
    alloc_binary = out / 'alloc'
    if args.allocations:
        alloc_cmd = [str(args.roc.expanduser()), 'build', '--fuzz', str(ROOT / 'fuzz/yaml-alloc.roc'), '--output=' + str(alloc_binary)]
        if source != ROOT / 'package/main.roc':
            alloc_cmd += ['--replace-dep', str(ROOT / 'package/main.roc'), str(source)]
        alloc_build = subprocess.run(alloc_cmd, capture_output=True, text=True)
        (out / 'alloc-build.log').write_text(alloc_build.stdout + alloc_build.stderr)
        if alloc_build.returncode:
            raise RuntimeError('Allocation diagnostic build failed: ' + alloc_build.stderr[-3000:])
    metadata = {'label': args.label, 'compiler': str(args.roc),
                'compiler_version': subprocess.run([str(args.roc.expanduser()), '--version'], capture_output=True, text=True).stdout.strip(),
                'host': platform.platform(), 'harness_sha256': hashlib.sha256((ROOT / 'scripts/yaml/benchmark.roc').read_bytes()).hexdigest(), 'build_command': cmd,
                'parser_root': str(source), 'source_sha256': hashlib.sha256((source.parent / 'Yaml.roc').read_bytes()).hexdigest(),
                'measurement': 'parse plus recursive result consumption; stdin and JSON output excluded; UTC clock',
                'repetitions': args.repetitions, 'iterations': args.iterations}
    records = []
    for size in map(int, args.sizes.split(',')):
        for name, text in fixtures(size).items():
            if args.only and name not in args.only.split(','):
                continue
            data = text.encode()
            fixture = out / f'{name}-{size}.yaml'
            fixture.write_bytes(data)
            try:
                run_case(binary, data, 1, args.timeout)  # warmup
                samples = [run_case(binary, data, args.iterations, args.timeout) for _ in range(args.repetitions)]
                per_parse = [r['elapsed_ns'] / args.iterations for r in samples]
                record = {'name': name, 'size': size, 'bytes': len(data), 'input_sha256': hashlib.sha256(data).hexdigest(),
                          'median_ns': statistics.median(per_parse), 'samples': samples, 'fixture': str(fixture)}
            except (subprocess.TimeoutExpired, subprocess.CalledProcessError, ValueError) as error:
                record = {'name': name, 'size': size, 'bytes': len(data), 'error': str(error)}
            if args.allocations:
                try:
                    diagnostic = subprocess.run([str(alloc_binary), 'replay', str(fixture)], capture_output=True, text=True, timeout=args.timeout)
                    log = diagnostic.stdout + diagnostic.stderr
                    (out / f'alloc-{name}-{size}.log').write_text(log)
                    match = re.search(r'ALLOCATION_DIAGNOSTIC allocations=(\d+) result=(\w+)', log)
                    if diagnostic.returncode != 77 or not match:
                        record['allocation_error'] = f'Unexpected diagnostic result: exit {diagnostic.returncode}'
                    else:
                        record['allocation_calls'] = int(match[1])
                        record['allocation_result'] = match[2]
                except subprocess.TimeoutExpired as error:
                    record['allocation_error'] = str(error)
            records.append(record)
            print(json.dumps(record), flush=True)
            (out / 'results.json').write_text(json.dumps({'metadata': metadata, 'results': records}, indent=2) + '\n')


if __name__ == '__main__':
    main()
