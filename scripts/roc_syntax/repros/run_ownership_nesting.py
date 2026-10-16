#!/usr/bin/env python3
"""Build native, compare exact models, and retain the ownership reproduction."""
import argparse
import hashlib
import json
import platform
from pathlib import Path
import re
import statistics
import subprocess
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
PIN = 'nightly-2026-10-15-38e6c42'
DIAGNOSTIC = re.compile(rb'OWNERSHIP allocations=(\d+) checksum=(\d+) nodes=(\d+) edges=(\d+)')


def expected(depth):
    # Independent postorder model: no Roc parser/worker output is reused.
    position = nodes = edges = checksum = 0
    def visit(level):
        nonlocal position, nodes, edges, checksum
        start = position
        if level:
            left = visit(level - 1)
            right = visit(level - 1)
            child_start = edges
            edges += 2
            checksum += left + right
        else:
            position += 1
            child_start = edges
        index = nodes
        nodes += 1
        checksum += 7 + start + position + child_start + (2 if level else 0)
        return index
    visit(depth)
    return {'checksum': checksum, 'nodes': nodes, 'edges': edges}


def timing_source(body, function):
    return '''app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst" }
import cli.OsStr
import cli.Stdout
import cli.Monotonic
main! : List(OsStr) => Try({}, _)
main! = |args| {
 mode = U8.from_str(OsStr.display(args.get(0) ?? OsStr.from_str("0"))) ?? 0
 depth = U64.from_str(OsStr.display(args.get(1) ?? OsStr.from_str("10"))) ?? 10
 iterations = U64.from_str(OsStr.display(args.get(2) ?? OsStr.from_str("1"))) ?? 1
 source = List.repeat(7.U8, 65536)
 var $sum = 0.U64
 var $index = 0.U64
 start = Monotonic.now!()
 while $index < iterations {
  output = FUNCTION(mode, source, depth)
  $sum = $sum + checksum(output)
  $index = $index + 1
 }
 elapsed_ns = Monotonic.now!() - start
 Stdout.line!(Json.to_str({ elapsed_ns, iterations, checksum: $sum }))?
 Ok({})
}
'''.replace('FUNCTION', function) + '\n' + body


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc', default='roc')
    parser.add_argument('--allow-other-compiler', action='store_true', help='record a different version, e.g. to verify a future compiler fix')
    parser.add_argument('--matrix', action='store_true', help='run all eight input/result/helper controls')
    parser.add_argument('--timings', action='store_true', help='also build an in-process optimized timing driver')
    parser.add_argument('--depths', type=int, nargs='+', default=[8, 10, 12])
    parser.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp/ownership-repro')
    args = parser.parse_args(argv)
    if not args.depths or any(not 0 <= depth <= 16 for depth in args.depths):
        parser.error('depths must be in 0..16')
    version = subprocess.check_output([args.roc, '--version'], text=True).strip()
    if PIN not in version and not args.allow_other_compiler:
        parser.error(f'expected {PIN}; use --allow-other-compiler to compare another version')
    source = HERE / ('ownership-variants.roc' if args.matrix else 'ownership-nesting.roc')
    source_hash = digest(source)
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    binary = out / 'alloc'
    flags = ['build', '--no-cache', '--opt=speed', '--fuzz']
    subprocess.run([args.roc, *flags, str(source), f'--output={binary}'], check=True)
    report = {'compiler': version, 'build_flags': flags, 'platform': platform.platform(), 'machine': platform.machine(),
              'source': source.name, 'source_sha256': source_hash, 'runner_sha256': digest(Path(__file__)), 'worker_sha256': digest(binary),
              'allocation_metric': 'roc_alloc + roc_realloc calls inside output construction; source preparation and checksum excluded',
              'records': []}
    timing_binary = out / 'timing'
    if args.timings:
        text = source.read_text()
        marker = 'FlatResult :' if args.matrix else '# Ownership model:'
        generated = out / 'timing.roc'
        generated.write_text(timing_source(text[text.index(marker):], 'run_variant' if args.matrix else 'run_model'))
        subprocess.run([args.roc, 'build', '--no-cache', '--opt=speed', str(generated), f'--output={timing_binary}'], check=True)
        report['timing_driver_sha256'] = digest(timing_binary)
        report['timing_source_sha256'] = digest(generated)
        report['timing_scope'] = 'recursive output construction plus complete output checksum, source prepared outside timer'
    for depth in args.depths:
        model = expected(depth)
        for mode in range(8 if args.matrix else 2):
            seed = out / f'input-{mode}-{depth}'
            seed.write_bytes(bytes([mode, depth]))
            result = subprocess.run([str(binary), 'replay', str(seed)], capture_output=True, timeout=60)
            match = DIAGNOSTIC.search(result.stderr)
            if result.returncode != 77 or not match:
                raise RuntimeError(f'worker protocol failure: exit={result.returncode}, stderr={result.stderr[:2000]!r}')
            allocations, checksum, nodes, edges = map(int, match.groups())
            actual = {'checksum': checksum, 'nodes': nodes, 'edges': edges}
            if actual != model:
                raise RuntimeError(f'output model mismatch for mode={mode}, depth={depth}: {actual} != {model}')
            record = {'mode': mode, 'depth': depth, 'allocations': allocations, **actual}
            if args.timings:
                def batch(iterations):
                    item = json.loads(subprocess.check_output([str(timing_binary), str(mode), str(depth), str(iterations)], timeout=60))
                    if item['iterations'] != iterations or item['checksum'] != model['checksum'] * iterations:
                        raise RuntimeError('timing worker output mismatch')
                    return item['elapsed_ns'] / iterations
                first = batch(1)
                iterations = max(1, min(1000, int(20_000_000 / max(first, 1))))
                batch(iterations)
                values = [batch(iterations) for _ in range(5)]
                record.update(median_ns=statistics.median(values), samples_ns=values, iterations=iterations)
            report['records'].append(record)
            print(json.dumps(record), flush=True)
    contrasts = [next(r['allocations'] for r in report['records'] if r['depth'] == depth and r['mode'] == (3 if args.matrix else 0)) >
                 4 * next(r['allocations'] for r in report['records'] if r['depth'] == depth and r['mode'] == (4 if args.matrix else 1))
                 for depth in args.depths if depth >= 8]
    report['reproduced'] = bool(contrasts) and all(contrasts)
    if digest(source) != source_hash:
        raise RuntimeError('source changed during measurement')
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
