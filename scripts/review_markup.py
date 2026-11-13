#!/usr/bin/env python3
"""Independent local HTML/SVG/MathML conformance, mutation and browser reviews.

Tree oracles and semantic validators have distinct responsibilities. Browser or
parse5 acceptance does not establish valid SVG, MathML, or HTML authoring.
"""
from __future__ import annotations
import argparse
from collections import Counter
import hashlib
import html
import importlib.util
import json
import os
from pathlib import Path
import random
import re
import selectors
import shutil
import subprocess
import sys
import time
from xml.parsers import expat

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / 'scripts/markup'
DEFAULT_WORK = ROOT / '.roc-parser-tmp/markup'
NS = {'html': 'http://www.w3.org/1999/xhtml', 'svg': 'http://www.w3.org/2000/svg', 'mathml': 'http://www.w3.org/1998/Math/MathML'}
FATAL = {'crash', 'timeout', 'protocol_error', 'oracle_error', 'invalid_diagnostic'}
_spec = importlib.util.spec_from_file_location('markup_setup', DATA / 'setup_oracle.py')
setup = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(setup)


def write_json(path: Path, value) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')


def request(case: dict) -> dict:
    return {key: value for key, value in case.items() if key in {'input', 'format', 'syntax', 'fragment', 'context', 'scripting', 'include_bindings'}}


def xml_oracle(case: dict) -> dict:
    parser = expat.ParserCreate(namespace_separator='\x1f')
    parser.ordered_attributes = True
    parser.buffer_text = True
    roots, stack = [], []
    def name(value):
        parts = value.split('\x1f')
        return parts if len(parts) == 2 else ['', value]
    def add(node):
        (stack[-1][4] if stack else roots).append(node)
    def start(tag, attributes):
        node = ['element', *name(tag), [[*name(key), value] for key, value in zip(attributes[::2], attributes[1::2])], []]
        add(node)
        stack.append(node)
    def text(value):
        children = stack[-1][4] if stack else roots
        if children and children[-1][0] == 'text':
            children[-1][1] += value
        elif value:
            children.append(['text', value])
    parser.StartElementHandler = start
    parser.EndElementHandler = lambda _: stack.pop()
    parser.CharacterDataHandler = text
    parser.CommentHandler = lambda value: add(['comment', value])
    parser.ProcessingInstructionHandler = lambda target, data: add(['pi', target, data])
    parser.StartDoctypeDeclHandler = lambda *_: (_ for _ in ()).throw(ValueError('DOCTYPE unsupported by XML profile'))
    source = case['input']
    if case.get('fragment'):
        namespace = case.get('context', {}).get('namespace', NS[case['format']])
        source = '<oracle-fragment xmlns="' + html.escape(namespace, quote=True) + '">' + source + '</oracle-fragment>'
    try:
        parser.Parse(source, True)
    except (expat.ExpatError, ValueError) as error:
        return {'status': 'error', 'diagnostics': [{'code': 'xml-error', 'message': str(error)}]}
    return {'status': 'ok', 'roots': roots[0][4] if case.get('fragment') else roots, 'diagnostics': []}


class NodeOracle:
    def __init__(self, cache: Path = setup.DEFAULT_CACHE, timeout: float = 20):
        node_root = setup.ensure_node(cache)
        self.timeout = timeout
        self.process = subprocess.Popen(['node', str(DATA / 'oracle.mjs'), str(node_root)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                        env={**os.environ, 'PLAYWRIGHT_BROWSERS_PATH': str(cache / 'browsers')})
    def __enter__(self):
        return self
    def __exit__(self, *_):
        if self.process.stdin:
            self.process.stdin.close()
        try:
            self.process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait()
        self.process.stdout.close()
        self.process.stderr.close()
    def parse(self, value: dict) -> dict:
        self.process.stdin.write((json.dumps(value) + '\n').encode())
        self.process.stdin.flush()
        selector = selectors.DefaultSelector()
        try:
            selector.register(self.process.stdout, selectors.EVENT_READ)
            if not selector.select(self.timeout):
                raise TimeoutError('Node oracle deadline exceeded')
            line = self.process.stdout.readline()
        finally:
            selector.close()
        if not line:
            raise RuntimeError('Node oracle exited: ' + self.process.stderr.read().decode(errors='replace')[-2000:])
        result = json.loads(line)
        if result.get('status') == 'oracle_error':
            raise RuntimeError(result['message'])
        return result


def expected(case: dict, node: NodeOracle) -> dict:
    if case.get('syntax', 'html' if case['format'] == 'html' else 'xml') == 'xml':
        result = xml_oracle(case)
    else:
        result = node.parse(request(case))
        if case.get('recovery', 'strict') == 'strict' and result['diagnostics']:
            result = {**result, 'status': 'error'}
    # Spec-derived authoring verdicts are independent of syntax-tree acceptance.
    if case.get('accepted') is False or (case.get('valid') is False and case.get('accepted') is not True):
        result = {**result, 'status': 'error'}
    return result


def probe_identity(roc: str) -> dict:
    identity = hashlib.sha256()
    for path in sorted([*(ROOT / 'package').glob('*.roc'), *DATA.glob('*.roc')]):
        identity.update(path.relative_to(ROOT).as_posix().encode() + b'\0' + path.read_bytes() + b'\0')
    compiler = Path(shutil.which(roc) or roc)
    return {'source_sha256': identity.hexdigest(), 'compiler_sha256': hashlib.sha256(compiler.read_bytes()).hexdigest(), 'flags': ['--no-cache', '--opt=dev']}


def build_probe(roc: str, work: Path) -> Path:
    executable = work.resolve() / 'bin/probe'
    executable.parent.mkdir(parents=True, exist_ok=True)
    manifest = executable.with_suffix('.build.json')
    identity = probe_identity(roc)
    if executable.is_file() and manifest.is_file():
        try:
            saved = json.loads(manifest.read_text())
            if saved['identity'] == identity and saved['binary_sha256'] == setup.sha256(executable):
                return executable
        except (ValueError, KeyError):
            pass
    result = subprocess.run([roc, 'build', '--no-cache', '--opt=dev', str(DATA / 'probe.roc'), '--output=' + str(executable)], cwd=ROOT, capture_output=True, text=True, check=False)
    log = work / 'probe-build.log'
    log.write_text(result.stdout + result.stderr)
    if result.returncode:
        raise RuntimeError(f'markup probe build failed: {log}')
    if identity != probe_identity(roc):
        raise RuntimeError('markup sources changed during probe build; rebuild before reviewing')
    write_json(manifest, {'identity': identity, 'binary_sha256': setup.sha256(executable)})
    return executable


def resolve_probe(args) -> Path:
    if not args.probe:
        return build_probe(args.roc, args.work)
    executable = args.probe.resolve()
    try:
        saved = json.loads(executable.with_suffix('.build.json').read_text())
        if saved['identity'] != probe_identity(args.roc) or saved['binary_sha256'] != setup.sha256(executable):
            raise ValueError('identity mismatch')
    except (OSError, ValueError, KeyError) as error:
        raise RuntimeError('explicit markup probe has missing/stale build identity; rerun without --probe using the intended --roc compiler') from error
    return executable


def probe(executable: Path, case: dict, timeout: float = 10) -> dict:
    arguments = [case['format'], case.get('syntax', 'html' if case['format'] == 'html' else 'xml'),
                 'fragment' if case.get('fragment') else 'document', case.get('recovery', 'strict'),
                 'validate' if case.get('validate', True) else 'structural',
                 case.get('context', {}).get('namespace', NS[case['format']]), case.get('context', {}).get('local', 'body' if case['format'] == 'html' else 'svg' if case['format'] == 'svg' else 'math'), 'on' if case.get('scripting') else 'off', 'bindings' if case.get('include_bindings') else 'semantic', *[('on' if case[key] else 'off') if key in case else 'default' for key in ['allow_unknown_elements', 'allow_unknown_attributes', 'check_values']]]
    try:
        result = subprocess.run([str(executable), *arguments], input=case['input'].encode(), capture_output=True, timeout=timeout, check=False)
    except subprocess.TimeoutExpired:
        return {'status': 'timeout'}
    if result.returncode:
        return {'status': 'crash', 'return_code': result.returncode, 'stderr': result.stderr.decode(errors='replace')[-4000:]}
    try:
        actual = json.loads(result.stdout)
        if actual['status'] not in {'ok', 'error'}:
            raise ValueError('unknown status')
        if actual['status'] == 'ok':
            validate_tree(actual['roots'])
        else:
            error = actual.get('error', {})
            lines = re.split(r'\r\n|\r|\n', case['input'])
            line, column = error.get('line', 0), error.get('column', 0)
            if not isinstance(error.get('code'), str) or not isinstance(line, int) or not isinstance(column, int) or not 1 <= line <= len(lines) or not 1 <= column <= len(lines[line - 1].encode()) + 1:
                return {'status': 'invalid_diagnostic', 'actual': actual}
        return actual
    except (ValueError, KeyError, TypeError) as error:
        return {'status': 'protocol_error', 'message': str(error), 'stdout': result.stdout.decode(errors='replace')[-4000:]}


def validate_tree(roots: list) -> None:
    if not isinstance(roots, list):
        raise ValueError('roots must be a list')
    pending = list(roots)
    while pending:
        node = pending.pop()
        if not isinstance(node, list) or not node:
            raise ValueError('invalid node')
        if node[0] == 'element':
            if len(node) != 5 or not all(isinstance(x, str) for x in node[1:3]) or not isinstance(node[3], list) or not isinstance(node[4], list):
                raise ValueError('invalid element')
            if any(not isinstance(a, list) or len(a) != 3 or not all(isinstance(v, str) for v in a) for a in node[3]):
                raise ValueError('invalid attribute')
            pending.extend(node[4])
        elif node[0] in {'text', 'comment'}:
            if len(node) != 2 or not isinstance(node[1], str):
                raise ValueError('invalid character node')
        elif node[0] == 'doctype':
            if len(node) != 4 or not all(isinstance(x, str) for x in node[1:]):
                raise ValueError('invalid doctype')
        elif node[0] == 'pi':
            if len(node) != 3 or not all(isinstance(x, str) for x in node[1:]):
                raise ValueError('invalid processing instruction')
        else:
            raise ValueError('unknown node kind')


def compare(case: dict, actual: dict, reference: dict) -> dict:
    if actual['status'] in FATAL:
        kind = actual['status']
    elif actual['status'] != reference['status']:
        kind = 'valid_rejection' if reference['status'] == 'ok' else 'invalid_acceptance'
    elif reference['status'] == 'ok' and actual['roots'] != reference['roots']:
        kind = 'wrong_tree'
    elif reference['status'] == 'error' and case.get('problem') and case['problem'] not in actual.get('error', {}).get('code', ''):
        kind = 'wrong_diagnostic'
    elif reference['status'] == 'ok' and case.get('diagnostic_contains') and not any(case['diagnostic_contains'] in issue.get('code', '') for issue in actual.get('diagnostics', [])):
        kind = 'missing_relaxation_diagnostic'
    else:
        kind = 'pass'
    return {'id': case['id'], 'kind': kind, 'case': case, 'actual': actual, 'expected': reference,
            'input_sha256': hashlib.sha256(case['input'].encode()).hexdigest()}


def generated(seed: int, count: int):
    rng = random.Random(seed)
    for index in range(count):
        format = rng.choice(['html', 'svg', 'mathml'])
        value = rng.choice(['x<&é', 'line\ntext', '日本', '42', '\tvalue', '😀'])
        if format == 'html':
            source = '<div data-value="' + html.escape(value, quote=True) + '"><span>' + html.escape(value) + '</span></div>'
        elif format == 'svg':
            source = '<g id="x' + str(index) + '"><text>' + html.escape(value) + '</text><path d="M0 0L10 10Z"/></g>'
        else:
            source = '<mrow><mi>' + html.escape(value) + '</mi><mo>+</mo><mn>' + str(rng.randrange(10000)) + '</mn></mrow>'
        yield {'id': f'generated-{seed}-{index}', 'format': format, 'fragment': True, 'input': source, 'validate': False}


def mutate(case: dict, rng: random.Random, index: int) -> dict:
    source = case['input']
    at = rng.randrange(len(source) + 1)
    operation = rng.randrange(4)
    source = source[:at] + (rng.choice(['<', '>', '&', '\x00', '/', '"', "'"]) if operation == 0 else source[at:at+1] * 2 if operation == 1 else '</x>' if operation == 2 else '') + source[at + (1 if operation in {1, 3} and at < len(source) else 0):]
    return {**case, 'id': f'mutation-{index}', 'input': source, 'validate': False, 'recovery': 'browser'}


def retain(row: dict, work: Path) -> None:
    digest = hashlib.sha256(json.dumps(row['case'], sort_keys=True).encode()).hexdigest()
    write_json(work / 'reproducers' / (digest + '.json'), row)


def load_cases(formats: list[str]) -> list[dict]:
    return [case for case in json.loads((DATA / 'cases.json').read_text())['cases'] if case['format'] in formats]


def run_review(args) -> int:
    executable = resolve_probe(args)
    cases = load_cases(args.formats)
    if args.command == 'replay':
        value = json.loads(args.reproducer.read_text())
        cases = [value.get('case', value)]
    elif args.command == 'corpus':
        cases = [{'id': path.name, 'format': args.format, 'fragment': args.fragment, 'validate': False,
                  'recovery': 'browser', 'input': path.read_bytes().decode('utf-8')} for path in sorted(args.corpus.iterdir()) if path.is_file()]
    elif args.command == 'campaign':
        cases += list(generated(args.seed, 30 if args.smoke else args.generated))
        rng = random.Random(args.seed)
        base = list(cases)
        cases += [mutate(rng.choice(base), rng, index) for index in range(30 if args.smoke else args.mutations)]
    rows = []
    deadline = time.monotonic() + args.seconds if args.command == 'campaign' and not args.smoke else float('inf')
    with NodeOracle(args.cache) as node:
        for case in cases:
            if time.monotonic() >= deadline:
                break
            row = compare(case, probe(executable, case), expected(case, node))
            rows.append(row)
            if row['kind'] != 'pass':
                retain(row, args.work)
    counts = dict(Counter(row['kind'] for row in rows))
    report = {'schema_version': 1, 'command': args.command, 'seed': getattr(args, 'seed', None), 'counts': counts, 'total': len(rows), 'rows': rows,
              'oracles': json.loads((DATA / 'oracles.json').read_text()), 'node_lock_sha256': setup.sha256(DATA / 'oracle/package-lock.json')}
    write_json(args.work / (args.command + '.json'), report)
    print(json.dumps({key: report[key] for key in ['command', 'total', 'counts']}))
    return int(not rows or any(row['kind'] != 'pass' for row in rows))


def browser_review(args) -> int:
    setup.ensure_browsers(args.engines, args.cache, with_deps=args.with_deps)
    executable = resolve_probe(args)
    rows = []
    with NodeOracle(args.cache, timeout=60) as node:
        for case in load_cases(args.formats):
            if case.get('valid') is False or case.get('syntax', 'html' if case['format'] == 'html' else 'xml') != 'html':
                continue
            actual = probe(executable, {**case, 'recovery': 'browser', 'validate': False})
            for engine in args.engines:
                reference = node.parse({**request(case), 'engine': engine})
                row = compare({**case, 'id': case['id'] + '/' + engine}, actual, reference)
                rows.append(row)
                if row['kind'] != 'pass':
                    retain(row, args.work)
    write_json(args.work / 'browser.json', {'counts': dict(Counter(row['kind'] for row in rows)), 'rows': rows, 'engines': args.engines})
    print(json.dumps({'total': len(rows), 'counts': dict(Counter(row['kind'] for row in rows))}))
    return int(not rows or any(row['kind'] != 'pass' for row in rows))


def consumer_review(args) -> int:
    setup.ensure_browsers(args.engines, args.cache, with_deps=args.with_deps)
    executable = resolve_probe(args)
    cases = [case for case in load_cases(args.formats) if case.get('consumer')]
    templates = []
    for operation in ['template', 'template-html']:
        template_process = subprocess.run([str(executable), operation], input=b'<path/>', capture_output=True, check=False, timeout=10)
        if template_process.returncode:
            raise RuntimeError(operation + ' consumer probe failed: ' + template_process.stderr.decode(errors='replace'))
        template = json.loads(template_process.stdout)
        validate_tree(template['roots'])
        templates.append((operation, template))
    rows = []
    with NodeOracle(args.cache, timeout=60) as node:
        for case in cases:
            actual = probe(executable, case)
            if actual['status'] != 'ok':
                rows.append({'id': case['id'], 'kind': actual['status'], 'actual': actual})
                continue
            for engine in args.engines:
                constructed = node.parse({**request(case), 'engine': engine, 'construct': actual['roots']})
                reference = node.parse({**request(case), 'engine': engine})
                row = compare({**case, 'id': case['id'] + '/' + engine}, constructed, reference)
                required = ['linearGradient', 'pathLength', 'foreignObject'] if case['format'] == 'svg' else ['mathml']
                if not all(constructed.get('checks', {}).get(key) is True for key in required) or constructed.get('interfaces') != reference.get('interfaces'):
                    row['kind'] = 'wrong_dom_interface'
                row['checks'] = constructed.get('checks', {})
                row['interfaces'] = constructed.get('interfaces', {})
                row['browser_version'] = constructed.get('browser_version')
                rows.append(row)
        for operation, template in templates:
            for engine in args.engines:
                case = {'id': 'consumer-' + operation + '/' + engine, 'format': 'svg', 'syntax': 'xml',
                        'input': '<svg xmlns="' + NS['svg'] + '"><circle cx="12" cy="0" r="1"/><text>&lt;path/&gt;</text></svg>'}
                if operation == 'template-html':
                    case.update(syntax='html', fragment=True, format='html', input='<script>if (a < b) x = 1;</script><svg><circle cx="12" cy="0" r="1"/><text>&lt;path/&gt;</text></svg>')
                constructed = node.parse({**request(case), 'engine': engine, 'construct': template['roots'], 'bindings': template['bindings']})
                reference = node.parse({**request(case), 'engine': engine})
                row = compare(case, constructed, reference)
                if not all(constructed.get('checks', {}).get(key) is True for key in ['textBinding', 'attributeBinding']):
                    row['kind'] = 'wrong_binding_behavior'
                row['checks'] = constructed.get('checks', {})
                row['browser_version'] = constructed.get('browser_version')
                rows.append(row)
    report = {'schema_version': 1, 'engines': args.engines, 'counts': dict(Counter(row['kind'] for row in rows)), 'rows': rows,
              'boundary': 'DOM construction consumes Roc trees and typed bindings using namespace-aware DOM constructors; it never reparses source or dynamic binding text.'}
    write_json(args.work / 'consumer.json', report)
    print(json.dumps({'total': len(rows), 'counts': report['counts']}))
    return int(not rows or any(row['kind'] != 'pass' for row in rows))


def dat_tree(roots: list) -> str:
    # WPT's fixture representation sorts attributes and flattens template's
    # separate DocumentFragment through a printed `content` marker.
    namespaces = {NS['svg']: 'svg ', NS['mathml']: 'math ', NS['html']: '', '': ''}
    attr_namespaces = {'http://www.w3.org/XML/1998/namespace': 'xml ', 'http://www.w3.org/1999/xlink': 'xlink ', 'http://www.w3.org/2000/xmlns/': 'xmlns '}
    lines = []
    def visit(node, depth):
        prefix = '| ' + '  ' * depth
        if node[0] == 'element':
            lines.append(prefix + '<' + namespaces.get(node[1], node[1] + ' ') + node[2] + '>')
            for uri, name, value in sorted(node[3], key=lambda attribute: (attr_namespaces.get(attribute[0], '') + attribute[1]).encode('utf-16-be', errors='surrogatepass')):
                lines.append(prefix + '  ' + attr_namespaces.get(uri, '') + name + '="' + value + '"')
            child_depth = depth + 1
            if node[1] == NS['html'] and node[2] == 'template':
                lines.append(prefix + '  content')
                child_depth += 1
            for child in node[4]:
                visit(child, child_depth)
        elif node[0] == 'text':
            lines.append(prefix + '"' + node[1] + '"')
        elif node[0] == 'comment':
            lines.append(prefix + '<!-- ' + node[1] + ' -->')
        elif node[0] == 'doctype':
            identifiers = (' "' + node[2] + '" "' + node[3] + '"') if node[2] or node[3] else ''
            lines.append(prefix + '<!DOCTYPE ' + node[1] + identifiers + '>')
        elif node[0] == 'pi':
            lines.append(prefix + '<?' + node[1] + (' ' + node[2]) + '?>')
    for root in roots:
        visit(root, 0)
    return '\n'.join(lines)


def normalize_dat(value: str) -> str:
    import re
    return '\n'.join(line for line in value.split('\n') if not re.match(r'^\|\s+(?:xmlns )?xmlns(?:[: ][^=]+)?="', line))


def suite_review(args) -> int:
    executable = resolve_probe(args)
    inventory = json.loads((DATA / 'tree-suite.json').read_text())
    cases = inventory['cases']
    if args.file:
        cases = [case for case in cases if args.file in case['id']]
    cases = [{**case, 'id': case['id'] + ('/script-on' if scripting else '/script-off'), 'scripting': scripting} for case in cases for scripting in case.get('scripting_modes', [case.get('scripting', False)])]
    if args.limit:
        cases = cases[:args.limit]
    rows = []
    with NodeOracle(args.cache) as node:
        for case in cases:
            actual = probe(executable, case)
            reference = node.parse(request(case))
            expected_dat = case['expected_dat'] if case.get('include_bindings') else normalize_dat(case['expected_dat'])
            actual_dat = dat_tree(actual['roots']) if actual['status'] == 'ok' else None
            oracle_dat = dat_tree(reference['roots'])
            findings = []
            if actual['status'] in FATAL:
                findings.append(actual['status'])
            elif actual_dat != expected_dat:
                findings.append('wrong_tree')
            has_errors = case['expected_error_count'] > 0
            if actual['status'] == 'ok' and bool(actual.get('diagnostics')) != has_errors:
                findings.append('wrong_diagnostic_presence')
            strict = None
            if not args.trees_only:
                strict = probe(executable, {**case, 'recovery': 'strict'})
                expected_status = 'error' if has_errors else 'ok'
                if strict['status'] in FATAL:
                    findings.append('strict_' + strict['status'])
                elif strict['status'] != expected_status:
                    findings.append('wrong_strict_verdict')
            kind = findings[0] if findings else 'pass'
            row = {'id': case['id'], 'kind': kind, 'findings': findings, 'case': case, 'actual': actual, 'strict': strict, 'actual_dat': actual_dat,
                   'expected_dat': expected_dat, 'parse5_dat': oracle_dat, 'oracle_disagreement': oracle_dat != expected_dat}
            rows.append(row)
            if kind != 'pass':
                retain(row, args.work)
    report = {'source': inventory['source'], 'counts': dict(Counter(row['kind'] for row in rows)),
              'oracle_disagreements': sum(row['oracle_disagreement'] for row in rows), 'finding_counts': dict(Counter(finding for row in rows for finding in row['findings'])), 'strict_checked': not args.trees_only, 'total': len(rows), 'rows': rows}
    write_json(args.work / 'suite.json', report)
    print(json.dumps({key: report[key] for key in ['total', 'counts', 'finding_counts', 'strict_checked', 'oracle_disagreements']}))
    return int(not rows or any(row['kind'] != 'pass' for row in rows))


def semantic_review(args) -> int:
    suppliers = setup.ensure_semantic(args.cache)
    python = setup.ensure_semantic_python(args.cache)
    executable = resolve_probe(args)
    rows = []
    for case in load_cases(args.formats):
        if not case.get('semantic_oracle'):
            continue
        result = subprocess.run([str(python), str(DATA / 'semantic_oracle.py'), str(suppliers['nu']), str(suppliers['schemas'])],
                                input=json.dumps(case), capture_output=True, text=True, check=False, timeout=40)
        if result.returncode:
            raise RuntimeError('semantic oracle failed: ' + result.stdout + result.stderr)
        reference = json.loads(result.stdout)
        actual = probe(executable, case)
        if reference['valid'] != case['valid']:
            kind = 'oracle_disagreement'
        elif actual['status'] in FATAL:
            kind = actual['status']
        elif (actual['status'] == 'ok') != reference['valid']:
            kind = 'valid_rejection' if reference['valid'] else 'invalid_acceptance'
        else:
            kind = 'pass'
        row = {'id': case['id'], 'kind': kind, 'case': case, 'expected': reference, 'actual': actual}
        rows.append(row)
        if kind != 'pass':
            retain(row, args.work)
    write_json(args.work / 'semantic.json', {'counts': dict(Counter(row['kind'] for row in rows)), 'rows': rows})
    print(json.dumps({'total': len(rows), 'counts': dict(Counter(row['kind'] for row in rows))}))
    return int(not rows or any(row['kind'] != 'pass' for row in rows))


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc', default=os.environ.get('ROC', 'roc'))
    parser.add_argument('--probe', type=Path)
    parser.add_argument('--work', type=Path, default=DEFAULT_WORK / 'review')
    parser.add_argument('--cache', type=Path, default=setup.DEFAULT_CACHE)
    parser.add_argument('--formats', nargs='+', choices=list(NS), default=list(NS))
    commands = parser.add_subparsers(dest='command', required=True)
    commands.add_parser('check')
    commands.add_parser('semantic')
    suite = commands.add_parser('suite')
    suite.add_argument('--limit', type=int)
    suite.add_argument('--trees-only', action='store_true', help='diagnostic lane: omit strict re-execution; still check parse-diagnostic presence')
    suite.add_argument('--file', help='select upstream source filename substring')
    campaign = commands.add_parser('campaign')
    campaign.add_argument('--smoke', action='store_true')
    campaign.add_argument('--seed', type=int, default=1)
    campaign.add_argument('--seconds', type=float, default=60)
    campaign.add_argument('--generated', type=int, default=1000)
    campaign.add_argument('--mutations', type=int, default=1000)
    corpus = commands.add_parser('corpus')
    corpus.add_argument('format', choices=list(NS))
    corpus.add_argument('corpus', type=Path)
    corpus.add_argument('--fragment', action='store_true')
    replay = commands.add_parser('replay')
    replay.add_argument('reproducer', type=Path)
    for command in ['browser', 'consumer']:
        browser = commands.add_parser(command)
        browser.add_argument('--engines', nargs='+', choices=['chromium', 'firefox', 'webkit'], default=['chromium'])
        browser.add_argument('--with-deps', action='store_true')
    args = parser.parse_args(argv)
    try:
        return consumer_review(args) if args.command == 'consumer' else browser_review(args) if args.command == 'browser' else semantic_review(args) if args.command == 'semantic' else suite_review(args) if args.command == 'suite' else run_review(args)
    except (OSError, ValueError, RuntimeError, TimeoutError, subprocess.SubprocessError) as error:
        print(f'markup review infrastructure error: {error}', file=sys.stderr)
        return 2


if __name__ == '__main__':
    raise SystemExit(main())
