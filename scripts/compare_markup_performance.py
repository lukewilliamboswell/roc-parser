#!/usr/bin/env python3
"""Local, identity-checked DOM comparison; preparation and timing are separate commands."""
from __future__ import annotations
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import random
import shutil
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_WORK = ROOT / '.roc-parser-tmp/markup/dom-compare'
RUST = ROOT / 'bench/compare/markup-rust'
GO = ROOT / 'bench/compare/markup-go'
DRIVER = ROOT / 'bench/roc/markup-compare.roc'
PYTHON_DRIVER = ROOT / 'scripts/markup/compare_python.py'
REQUIREMENTS = ROOT / 'bench/compare/markup-python-requirements.txt'


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def source_paths():
    return sorted([*(ROOT / 'package').glob('*.roc'), DRIVER, PYTHON_DRIVER, Path(__file__), REQUIREMENTS,
                   *RUST.glob('Cargo.*'), *RUST.glob('src/*.rs'), *GO.glob('*.go'), *GO.glob('go.*')])


def source_identity():
    identity = hashlib.sha256()
    for path in source_paths():
        identity.update(path.relative_to(ROOT).as_posix().encode() + b'\0' + path.read_bytes() + b'\0')
    return identity.hexdigest()


def repository_identity():
    return {'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
            'source_files': {p.relative_to(ROOT).as_posix(): digest(p) for p in source_paths()}}


def machine_identity():
    model = platform.processor()
    cpuinfo = Path('/proc/cpuinfo')
    if cpuinfo.exists():
        model = next((line.split(':', 1)[1].strip() for line in cpuinfo.read_text().splitlines()
                      if line.startswith('model name')), model)
    return {'platform': platform.platform(), 'cpu_model': model, 'logical_cpus': os.cpu_count(),
            'affinity': sorted(os.sched_getaffinity(0)) if hasattr(os, 'sched_getaffinity') else None}


def fixtures():
    result = []
    for count in [8, 32, 128]:
        result.append({'id': f'svg-paths-{count}', 'format': 'svg', 'mode': 'xml', 'input':
                       '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 400">' +
                       ''.join(f'<path id="p{i}" d="M0 0L10 20Z" fill="#336699"/>' for i in range(count)) + '</svg>'})
        result.append({'id': f'mathml-fractions-{count}', 'format': 'mathml', 'mode': 'xml', 'input':
                       '<math xmlns="http://www.w3.org/1998/Math/MathML"><mrow>' +
                       '<mfrac><mi>x</mi><mn>2</mn></mfrac>' * count + '</mrow></math>'})
        result.append({'id': f'html-cards-{count}', 'format': 'html', 'mode': 'fragment', 'input':
                       '<section class="cards">' + ''.join(f'<article id="c{i}"><h2>Card {i}</h2><p>Hello &amp; welcome.</p><a href="/item/{i}">Read more</a></article>' for i in range(count)) + '</section>'})
    result.extend([
        {'id': 'svg-drawing', 'format': 'svg', 'mode': 'xml', 'input': '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="320" height="180" viewBox="0 0 320 180"><defs><linearGradient id="g"><stop offset="0" stop-color="#123456"/><stop offset="1" stop-color="#abcdef"/></linearGradient></defs><rect width="320" height="180" rx="12" fill="url(#g)"/><g transform="translate(20 20)" stroke="#ffffff" stroke-width="2"><circle cx="40" cy="40" r="25"/><path d="M80 60 Q120 0 160 60 T240 60" fill="none"/><text x="20" y="120" font-size="18">Hello &amp; SVG</text><use xlink:href="#g" x="10"/></g></svg>'},
        {'id': 'mathml-formula', 'format': 'mathml', 'mode': 'xml', 'input': '<math xmlns="http://www.w3.org/1998/Math/MathML" xml:lang="en"><mrow><mi>x</mi><mo>=</mo><mfrac><mrow><mo>−</mo><mi>b</mi><mo>±</mo><msqrt><mrow><msup><mi>b</mi><mn>2</mn></msup><mo>−</mo><mn>4</mn><mi>a</mi><mi>c</mi></mrow></msqrt></mrow><mrow><mn>2</mn><mi>a</mi></mrow></mfrac></mrow></math>'},
        {'id': 'html-snippet', 'format': 'html', 'mode': 'fragment', 'input': '<main><h1>Parser examples</h1><p>A <strong>typed</strong> tree &amp; reusable values.</p><ul><li>SVG</li><li>MathML</li><li>HTML</li></ul><form action="/search"><label for="q">Search</label><input id="q" name="q" type="search"><button type="submit">Find</button></form></main>'},
        {'id': 'html-document', 'format': 'html', 'mode': 'document', 'input': '<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Example page</title></head><body><header><h1>News</h1><nav><a href="/">Home</a></nav></header><main><article><h2>A story</h2><p>Words &amp; more words.</p><table><tbody><tr><th>Item</th><th>Count</th></tr><tr><td>Examples</td><td>12</td></tr></tbody></table></article></main><footer>Copyright 2026</footer></body></html>'},
    ])
    return result


def command(args, work, name):
    log = work / f'{name}.log'
    start = time.monotonic()
    result = subprocess.run(args, cwd=ROOT, capture_output=True, text=True, check=False)
    log.write_text(result.stdout + result.stderr)
    if result.returncode:
        raise RuntimeError(f'{name} failed: {log}')
    return {'command': [str(x) for x in args], 'elapsed_seconds': time.monotonic() - start, 'log': str(log)}


def prepare(args):
    work = args.work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    before = source_identity()
    python = work / 'python/bin/python'
    builds = []
    if not python.exists():
        builds.append(command([args.python_base, '-m', 'venv', str(work / 'python')], work, 'python-venv'))
    builds.append(command([str(python), '-m', 'pip', 'install', '--require-hashes', '-r', str(REQUIREMENTS), '--report', str(work / 'python-install.json')], work, 'python-install'))
    builds.append(command(['cargo', 'build', '--locked', '--release', '--manifest-path', str(RUST / 'Cargo.toml'), '--target-dir', str(work / 'rust-target')], work, 'rust-build'))
    go = work / 'go-markup'
    builds.append(build_go(work, go))
    roc = work / 'roc-markup'
    builds.append(command([args.roc, 'build', '--no-cache', '--opt=speed', '--debug', str(DRIVER), '--output=' + str(roc)], work, 'roc-build'))
    if before != source_identity():
        raise RuntimeError('sources changed during preparation; rerun prepare')
    binaries = {'roc': roc, 'rust': work / 'rust-target/release/markup-dom-compare', 'go': go, 'python': python}
    manifest = {'source_sha256': before, 'repository': repository_identity(),
                'prepared_at': datetime.now(timezone.utc).isoformat(),
                'binaries': {k: {'path': str(p), 'sha256': digest(p),
                                 'format': subprocess.check_output(['file', '--brief', str(p)], text=True).strip() if shutil.which('file') else None}
                             for k, p in binaries.items()},
                'compiler_sha256': digest(shutil.which(args.roc) or args.roc), 'compiler': subprocess.check_output([args.roc, 'version'], text=True).strip(),
                'rust_version': subprocess.check_output(['rustc', '--version'], text=True).strip(), 'go_version': subprocess.check_output(['go', 'version'], text=True).strip(),
                'python_versions': json.loads(subprocess.check_output([str(python), '-c', 'import sys,json,importlib.metadata as m;print(json.dumps({"python":sys.version,**{n:m.version(n) for n in ["lxml","html5lib","selectolax"]}}))'], text=True)),
                'builds': builds, 'roc_flags': ['--no-cache', '--opt=speed', '--debug'], 'compiler_build_mode': args.compiler_build_mode,
                'roc_platform_runtime': 'basic-cli 0.28: Linux musl; glibc target unsupported' if platform.system() == 'Linux' else 'basic-cli 0.28 native target',
                'rust_flags': 'release opt-level=3 lto=thin codegen-units=1 debug=2 strip=false', 'go_flags': ['-trimpath'],
                'locks': {str(p.relative_to(ROOT)): digest(p) for p in [RUST / 'Cargo.lock', GO / 'go.sum', REQUIREMENTS]}}
    (work / 'build.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print(json.dumps({'prepared': str(work / 'build.json'), 'source_sha256': before}))
    return 0


def build_go(work, output):
    result = subprocess.run(['go', 'build', '-trimpath', '-o', str(output), '.'], cwd=GO, capture_output=True, text=True, check=False)
    log = work / 'go-build.log'
    log.write_text(result.stdout + result.stderr)
    if result.returncode:
        raise RuntimeError(f'Go build failed: {log}')
    return {'command': ['go', 'build', '-trimpath', '-o', str(output), '.'], 'cwd': str(GO), 'log': str(log)}


def implementations(case, manifest):
    bins = {k: v['path'] for k, v in manifest['binaries'].items()}
    result = []
    def add(name, language, adapter, eligible=True, note=''):
        prefix = [bins[language]] + ([str(PYTHON_DRIVER)] if language == 'python' else [])
        result.append({'name': name, 'language': language, 'command': [*prefix, adapter], 'eligible': eligible, 'semantics': note})
    if case['mode'] == 'xml':
        add('roc-full', 'roc', case['format'] + '-full', False, 'Public format parser: XML syntax, namespaces, source/tokens/spans, structural and value validation.')
        add('roc-raw', 'roc', 'xml-raw', False, 'XML syntax, namespaces and retained source/tokens/spans; no SVG/MathML authoring validation.')
        add('rust-roxmltree', 'rust', 'roxmltree', note='Borrowed immutable namespace-resolved DOM; no format authoring validation.')
        add('rust-quick-xml-dom', 'rust', 'quick-xml', note='Owned element/attribute/text tree built from pull events; no format validation; namespace-resolved owned DOM.')
        add('go-encoding-xml-dom', 'go', 'xml', note='Owned tree built from namespace-resolved tokens; no format validation.')
        add('python-etree', 'python', 'etree', note='ElementTree DOM; no format validation.')
        add('python-lxml', 'python', 'lxml-xml', note='libxml2 DOM; no format validation.')
    else:
        mode = case['mode']
        add('roc-full', 'roc', f'html-{mode}-full', False, 'Public strict HTML parser with source tracking and authoring validation.')
        add('roc-raw', 'roc', f'html-{mode}-raw', False, 'HTML browser recovery and source tracking; no authoring validation.')
        add('rust-html5ever-dom', 'rust', f'html-{mode}', note='WHATWG HTML tree construction, RcDom.')
        add('go-x-net-html-dom', 'go', f'html-{mode}', note='WHATWG HTML tree construction, linked DOM.')
        add('python-html5lib', 'python', f'html5lib-{mode}', note='WHATWG tree construction, ElementTree DOM.')
        add('python-lxml-html', 'python', f'lxml-{mode}', note='Legacy libxml2 HTML semantics; common-case checksum must agree before eligibility.')
        add('python-selectolax-lexbor', 'python', f'selectolax-{mode}', note='Lexbor HTML DOM; fragment roots consumed from implied body.')
    return result


def batch(implementation, data, iterations):
    result = subprocess.run([*implementation['command'], str(iterations)], input=data, capture_output=True, timeout=120, check=False)
    if result.returncode:
        raise RuntimeError(f"{implementation['name']} failed: {result.stderr[-2000:]!r}")
    record = json.loads(result.stdout)
    if record['iterations'] != iterations or record['successes'] != iterations or record['elapsed_ns'] <= 0:
        raise RuntimeError(f"{implementation['name']} rejected workload or returned invalid timing: {record}")
    return record


def normalize_tree(roots):
    if not isinstance(roots, list):
        raise ValueError('canonical roots must be a list')
    result = []
    for node in roots:
        if not isinstance(node, list) or not node:
            raise ValueError('invalid canonical node')
        if node[0] in {'doctype', 'comment', 'pi'}:
            continue
        if node[0] == 'text':
            if len(node) != 2 or not isinstance(node[1], str):
                raise ValueError('invalid canonical text')
            result.append(node)
        elif node[0] == 'element':
            if len(node) != 5 or not all(isinstance(x, str) for x in node[1:3]):
                raise ValueError('invalid canonical element')
            attrs = []
            for attr in node[3]:
                if len(attr) != 3 or not all(isinstance(x, str) for x in attr):
                    raise ValueError('invalid canonical attribute')
                if attr[0] != 'http://www.w3.org/2000/xmlns/' and attr[1] != 'xmlns':
                    attrs.append(attr)
            result.append([*node[:3], sorted(attrs), normalize_tree(node[4])])
        else:
            raise ValueError('unknown canonical node')
    return result


def canonical(implementation, data):
    result = subprocess.run([*implementation['command'], '0'], input=data, capture_output=True, timeout=120, check=False)
    if result.returncode:
        raise RuntimeError(f"canonical {implementation['name']} failed: {result.stderr[-2000:]!r}")
    return normalize_tree(json.loads(result.stdout))


def model_checksum(roots):
    total = 0
    for node in roots:
        if node[0] == 'text':
            total += 1 + len(node[1].encode())
        else:
            total += 1 + len(node[2].encode()) + sum(1 + len(a[1].encode()) + len(a[2].encode()) for a in node[3])
            total += model_checksum(node[4])
    return total


def ratios(rows, threshold):
    baseline = min((r for r in rows if r['eligible']), key=lambda r: r['median_ns'])
    return {'best': baseline['name'], 'best_ns': baseline['median_ns'], 'threshold': threshold,
            **{name: next(r for r in rows if r['name'] == name)['median_ns'] / baseline['median_ns'] for name in ['roc-full', 'roc-raw']}}


def run(args):
    work = args.work.resolve()
    manifest = json.loads((work / 'build.json').read_text())
    if manifest['source_sha256'] != source_identity():
        raise RuntimeError('stale source identity; run prepare on this exact snapshot')
    for item in manifest['binaries'].values():
        if digest(item['path']) != item['sha256']:
            raise RuntimeError('stale binary identity: ' + item['path'])
    all_rows, comparisons = [], []
    for case in fixtures():
        if args.cases and not any(value in case['id'] for value in args.cases):
            continue
        data = case['input'].encode()
        folder = work / 'inputs'
        folder.mkdir(exist_ok=True)
        (folder / (case['id'] + '.txt')).write_bytes(data)
        impls = implementations(case, manifest)
        reference_name = 'python-etree' if case['mode'] == 'xml' else 'python-html5lib'
        reference = next(impl for impl in impls if impl['name'] == reference_name)
        expected_tree = canonical(reference, data)
        expected = model_checksum(expected_tree)
        canonical_folder = work / 'canonical' / case['id']
        canonical_folder.mkdir(parents=True, exist_ok=True)
        encoded_tree = json.dumps(expected_tree, ensure_ascii=False, separators=(',', ':')).encode()
        expected_tree_sha256 = hashlib.sha256(encoded_tree).hexdigest()
        (canonical_folder / 'expected.json').write_bytes(encoded_tree)
        records = []
        for impl in impls:
            tree = canonical(impl, data)
            (canonical_folder / (impl['name'] + '.json')).write_text(json.dumps(tree, ensure_ascii=False) + '\n')
            if tree != expected_tree:
                raise RuntimeError(f"canonical tree mismatch {case['id']}/{impl['name']}: see {canonical_folder}")
            first = batch(impl, data, 1)
            if first['checksum'] != expected:
                raise RuntimeError(f"tree-consumption mismatch {case['id']}/{impl['name']}: {first['checksum']} != {expected}")
            count = max(1, min(1_000_000, int(args.target_ms * 1e6 / first['elapsed_ns'])))
            warm = batch(impl, data, count)
            if warm['checksum'] != expected * count:
                raise RuntimeError('warmup checksum changed')
            records.append({**impl, 'iterations': count, 'samples_ns': []})
        rng = random.Random(args.seed)
        for _ in range(args.repetitions):
            order = list(records)
            rng.shuffle(order)
            for record in order:
                sample = batch(record, data, record['iterations'])
                if sample['checksum'] != expected * record['iterations']:
                    raise RuntimeError('measured checksum changed')
                record['samples_ns'].append(sample['elapsed_ns'] / record['iterations'])
        for record in records:
            record.update({'case': case['id'], 'bytes': len(data), 'input_sha256': hashlib.sha256(data).hexdigest(),
                           'checksum_per_parse': expected, 'canonical_sha256': expected_tree_sha256, 'canonical_reference': reference_name,
                           'median_ns': statistics.median(record['samples_ns'])})
            record['bytes_per_second'] = len(data) * 1e9 / record['median_ns']
            all_rows.append(record)
        comparison = {'case': case['id'], 'format': case['format'], 'mode': case['mode'], **ratios(records, args.threshold)}
        comparison['pass'] = comparison['roc-full'] <= args.threshold
        comparisons.append(comparison)
        print(json.dumps(comparison), flush=True)
    if manifest['source_sha256'] != source_identity():
        raise RuntimeError('sources changed during measurement; discard results')
    if not comparisons:
        raise RuntimeError('no selected common cases; an empty comparison cannot pass')
    failures = [row for row in comparisons if not row['pass']]
    report = {'schema_version': 1, 'source_stable': True, 'build': manifest, 'machine': machine_identity(),
              'measured_at': datetime.now(timezone.utc).isoformat(),
              'seed': args.seed, 'repetitions': args.repetitions, 'target_ms': args.target_ms, 'comparisons': comparisons,
              'records': all_rows, 'failures': failures, 'contract': 'Every adapter builds a DOM and consumes element local names, attribute local names/values and text UTF-8 lengths. Untimed complete canonical trees compare namespace URI, local names, attribute values, exact text and tree shape against independent ElementTree/XML or html5lib/HTML reference models. Namespace-declaration attributes, comments, processing instructions and doctype metadata are excluded consistently; common fixtures contain no comments/PI and HTML documents use the standard doctype. Startup, imports, stdin and decoding precede timing. Allocation and consumption are timed; destruction/garbage collection follows normal runtime behavior without forced per-parse collection. Full Roc source tracking and authoring checks remain visible; raw Roc timings are diagnostic and never replace the full gate.'}
    (work / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'cases': len(comparisons), 'failures': len(failures), 'report': str(work / 'results.json')}))
    return int(bool(failures))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work', type=Path, default=DEFAULT_WORK)
    parser.add_argument('--roc', default=os.environ.get('ROC', 'roc'))
    parser.add_argument('--python-base', default=os.environ.get('MARKUP_PYTHON', sys.executable))
    parser.add_argument('--compiler-build-mode', choices=['ReleaseFast', 'ReleaseSafe', 'Debug', 'unknown'], default='unknown')
    commands = parser.add_subparsers(dest='command', required=True)
    commands.add_parser('prepare')
    measure = commands.add_parser('run')
    measure.add_argument('--cases', nargs='*')
    measure.add_argument('--repetitions', type=int, default=7)
    measure.add_argument('--target-ms', type=float, default=50)
    measure.add_argument('--threshold', type=float, default=1.5)
    measure.add_argument('--seed', type=int, default=1)
    args = parser.parse_args(argv)
    if args.command == 'run' and (args.repetitions <= 0 or args.target_ms <= 0 or args.threshold <= 0):
        parser.error('repetitions, target-ms and threshold must be positive')
    return prepare(args) if args.command == 'prepare' else run(args)


if __name__ == '__main__':
    raise SystemExit(main())
