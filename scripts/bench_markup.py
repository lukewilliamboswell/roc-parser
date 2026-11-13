#!/usr/bin/env python3
"""Local-only markup parsing, validation, recovery and binding scaling measurements."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_WORK = ROOT / '.roc-parser-tmp/markup/bench'


def source_identity() -> str:
    identity = hashlib.sha256()
    for path in sorted([*(ROOT / 'package').glob('*.roc'), ROOT / 'bench/roc/markup.roc', ROOT / 'fuzz/markup-alloc.roc']):
        identity.update(path.relative_to(ROOT).as_posix().encode() + b'\0' + path.read_bytes() + b'\0')
    return identity.hexdigest()


def workloads(sizes):
    for size in sizes:
        yield 'svg-siblings', 'svg', size, '<g>' + '<path d="M0 0L10 10Z" pathLength="1"/>' * size + '</g>'
        yield 'svg-long-text', 'svg', size, '<text>' + 'alpha&amp;é' * (size * 4) + '</text>'
        yield 'svg-path-commands', 'svg', size, '<path d="M0 0' + ' L10 20' * size + '"/>'
        yield 'mathml-fractions', 'mathml', size, '<mrow>' + '<mfrac><mn>1</mn><mn>2</mn></mfrac>' * size + '</mrow>'
        yield 'html-siblings', 'html', size, '<div>' + '<span data-x="v">text</span>' * size + '</div>'
        yield 'html-attributes', 'html', size, '<div ' + ' '.join(f'data-a{i}="value"' for i in range(size)) + '></div>'
        yield 'html-adoption', 'recover', size, '<div>' + '<p><b><i>one</b>two</i></p>' * size + '</div>'
        yield 'lossless-source', 'source', size, '<a x="&amp;">text<!-- comment --></a>' * size
        if size <= 512:
            yield 'xml-nesting', 'svg', size, '<g>' * size + '<path d="M0 0"/>' + '</g>' * size
        segments = ['<g>']
        for _ in range(size):
            segments[-1] += '<text x="'
            segments += ['">', '</text>']
        segments[-1] += '</g>'
        transport = '\0'.join(segments)
        yield 'hole-planning', 'segments', size, transport
        yield 'typed-binding', 'bind', size, transport


def sample(binary: Path, operation: str, data: bytes, repetitions: int, target_ms: float) -> dict:
    def batch(iterations):
        result = subprocess.run([str(binary), operation, str(iterations)], input=data, capture_output=True, timeout=60, check=False)
        if result.returncode:
            raise RuntimeError(f'{operation} measurement exit {result.returncode}: {result.stderr[-1000:]!r}')
        value = json.loads(result.stdout)
        if value['iterations'] != iterations or value['elapsed_ns'] <= 0 or value['successes'] != iterations:
            raise RuntimeError(f'{operation} workload was rejected or timer/protocol invalid: {value}')
        return value
    first = batch(1)
    iterations = max(1, min(100000, int(target_ms * 1e6 / first['elapsed_ns'])))
    batch(iterations)
    values = [batch(iterations)['elapsed_ns'] / iterations for _ in range(repetitions)]
    median = statistics.median(values)
    return {'iterations': iterations, 'samples_ns': values, 'median_ns': median,
            'bytes_per_second': len(data) * 1e9 / median}


def scaling(records, slack=1.5):
    failures, previous = [], {}
    for record in records:
        name = record['family']
        if name in previous:
            old = previous[name]
            growth = record['bytes'] / old['bytes']
            # A lower-bound ratio avoids treating one noisy median as proof of
            # excess work. Binding scales with holes, not serialized byte count.
            lower = min(record['samples_ns']) / max(old['samples_ns'])
            if lower > growth * slack:
                failures.append({'family': name, 'size': record['size'], 'time_ratio_lower_bound': lower, 'input_growth': growth, 'slack': slack})
        previous[name] = record
    return failures


def allocations(binary: Path, operation: str, data: bytes, work: Path) -> dict:
    codes = {'svg': b's', 'mathml': b'm', 'html': b'h', 'recover': b'r', 'source': b'o', 'segments': b'p', 'bind': b'b'}
    framed = codes[operation] + data
    digest = hashlib.sha256(framed).hexdigest()
    source = work / 'inputs' / (digest + '.bin')
    source.parent.mkdir(parents=True, exist_ok=True)
    source.write_bytes(framed)
    result = subprocess.run([str(binary), 'measure', str(source)], capture_output=True, check=False, timeout=60)
    if result.returncode:
        raise RuntimeError(f'allocation observation failed: {result.stderr[-1000:]!r}')
    observations = json.loads(result.stdout)
    return next(item['stats'] for item in observations if item['name'] == 'parse')


def compile_literals(roc: str, work: Path, sizes: list[int], repetitions: int) -> list[dict]:
    records = []
    platform_url = 'https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst'
    for size in sizes:
        document = '<g>' + '<path d="M0 0L10 10Z" pathLength="1"/>' * size + '</g>'
        for variant in ['literal', 'runtime']:
            folder = work / 'compile' / variant / str(size)
            folder.mkdir(parents=True, exist_ok=True)
            header = 'app [main!] { cli: platform ' + json.dumps(platform_url) + ', parser: ' + json.dumps(str(ROOT / 'package/main.roc')) + ' }\nimport cli.Stdout\nimport cli.Stdin\nimport parser.Svg\n'
            if variant == 'literal':
                body = 'drawing : Svg.Fragment\ndrawing = ' + json.dumps(document) + '\nmain! = |_| Stdout.line!(drawing.parsed.tree.nodes.len().to_str())\n'
            else:
                body = 'main! = |_| {\n input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""\n match Svg.parse_fragment(input, {}) {\n Ok(drawing) => Stdout.line!(drawing.parsed.tree.nodes.len().to_str())?\n Err(error) => crash Str.inspect(error)\n }\n Ok({})\n}\n'
            source = folder / 'main.roc'
            source.write_text(header + body)
            binary = folder / 'main'
            times = []
            for index in range(repetitions):
                start = time.monotonic_ns()
                result = subprocess.run([roc, 'build', '--no-cache', '--opt=speed', str(source), '--output=' + str(binary)], capture_output=True, text=True, check=False, timeout=300)
                times.append(time.monotonic_ns() - start)
                (folder / f'build-{index}.log').write_text(result.stdout + result.stderr)
                if result.returncode:
                    raise RuntimeError(f'compile-time fixture build failed: {folder}')
            result = subprocess.run([str(binary)], input=document.encode(), capture_output=True, check=False, timeout=30)
            if result.returncode or result.stdout.strip() != str(size + 1).encode():
                raise RuntimeError(f'compile-time fixture output differs: {folder}: {result.stdout!r}')
            records.append({'variant': variant, 'size': size, 'input_bytes': len(document.encode()), 'input_sha256': hashlib.sha256(document.encode()).hexdigest(),
                            'compile_samples_ns': times, 'median_compile_ns': statistics.median(times), 'artifact_bytes': binary.stat().st_size,
                            'artifact_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(), 'stdout': result.stdout.decode()})
    return records


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc', default=os.environ.get('ROC', 'roc'))
    parser.add_argument('--work', type=Path, default=DEFAULT_WORK)
    parser.add_argument('--sizes', nargs='+', type=int, default=[32, 64, 128, 256])
    parser.add_argument('--repetitions', type=int, default=3)
    parser.add_argument('--target-ms', type=float, default=20)
    parser.add_argument('--check-scaling', action='store_true')
    parser.add_argument('--allocations', action='store_true', help='build the local roc-fuzz diagnostic and retain full named allocation observations')
    parser.add_argument('--families', nargs='*')
    parser.add_argument('--compile-literals', action='store_true', help='measure checked-literal versus runtime-input compilation and artifact size separately')
    parser.add_argument('--compile-repetitions', type=int, default=1)
    args = parser.parse_args(argv)
    if any(n <= 0 for n in args.sizes) or args.repetitions <= 0:
        parser.error('sizes and repetitions must be positive')
    args.work = args.work.resolve()
    args.work.mkdir(parents=True, exist_ok=True)
    source_before = source_identity()
    binary = args.work / 'markup-bench'
    command = [args.roc, 'build', '--no-cache', '--opt=speed', str(ROOT / 'bench/roc/markup.roc'), '--output=' + str(binary)]
    build = subprocess.run(command, capture_output=True, text=True, check=False)
    (args.work / 'build.log').write_text(build.stdout + build.stderr)
    if build.returncode:
        raise RuntimeError(f'benchmark build failed: {args.work / "build.log"}')
    alloc_binary = args.work / 'markup-alloc'
    if args.allocations:
        build = subprocess.run([args.roc, 'build', '--no-cache', '--opt=speed', '--fuzz', str(ROOT / 'fuzz/markup-alloc.roc'), '--output=' + str(alloc_binary)], capture_output=True, text=True, check=False)
        (args.work / 'allocation-build.log').write_text(build.stdout + build.stderr)
        if build.returncode:
            raise RuntimeError(f'allocation diagnostic build failed: {args.work / "allocation-build.log"}')
    if source_before != source_identity():
        raise RuntimeError('sources changed during build; discard binaries and rerun against a stable worktree')
    records = []
    for family, operation, size, source in workloads(sorted(set(args.sizes))):
        if args.families and family not in args.families:
            continue
        data = source.encode()
        record = {'family': family, 'operation': operation, 'size': size, 'bytes': len(data), 'sha256': hashlib.sha256(data).hexdigest(),
                  **sample(binary, operation, data, args.repetitions, args.target_ms)}
        if args.allocations:
            record['allocations'] = allocations(alloc_binary, operation, data, args.work)
        records.append(record)
        print(json.dumps(record), flush=True)
    failures = scaling(records)
    compile_records = compile_literals(args.roc, args.work, sorted(set(args.sizes)), args.compile_repetitions) if args.compile_literals else []
    if source_before != source_identity():
        raise RuntimeError('sources changed during measurement; discard observations and rerun')
    report = {'schema_version': 1, 'compiler': subprocess.check_output([args.roc, 'version'], text=True).strip(),
              'build_flags': ['--no-cache', '--opt=speed'], 'binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
              'source_sha256': source_before, 'source_stable': True, 'compiler_sha256': hashlib.sha256(Path(shutil.which(args.roc) or args.roc).read_bytes()).hexdigest(), 'compile_records': compile_records, 'machine': platform.platform(),
              'records': records, 'scaling_failures': failures,
              'limits': 'Local wall-clock scaling observations are not a proof of asymptotic bounds. Allocation ceilings are checked by the markup-limits roc-fuzz target.'}
    (args.work / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
    return int(args.check_scaling and bool(failures))


if __name__ == '__main__':
    raise SystemExit(main())
