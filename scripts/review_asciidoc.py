#!/usr/bin/env python3
"""Bounded Asciidoctor oracle, authored fixtures, models, and differential checks.

Run scripts/asciidoc/setup_oracle.py first. Future features are reported as
unsupported, never counted as conformance passes. No baselines hide failures.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
from html.parser import HTMLParser
import importlib.util
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys
import time

SCRIPT_PATH = Path(__file__).resolve()
ROOT = SCRIPT_PATH.parents[1]
DATA = ROOT / 'scripts/asciidoc'
VERSION = '2.0.23'
MAX_SOURCE = 262144
MAX_OUTPUT = 8 * 1024 * 1024
DEFAULT_HOME = ROOT / '.roc-parser-tmp/asciidoc/gems'
FATAL = {'timeout', 'crash', 'protocol_error', 'oracle_error', 'oracle_policy_disagreement', 'oracle_disagreement', 'wrong_projection', 'wrong_html', 'missing_html', 'coverage_gap', 'unexpected_acceptance', 'valid_rejection'}


class HtmlEvents(HTMLParser):
    """Keep semantic DOM events, ignoring only inter-block indentation."""
    VOID = {'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', 'param', 'source', 'track', 'wbr'}
    TEXT = {'p', 'pre', 'code', 'samp', 'span', 'a', 'strong', 'em', 'b', 'i', 'mark', 'sup', 'sub', 'kbd', 'td', 'th', 'dt', 'dd', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'}

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.events = []
        self.stack = []

    def handle_starttag(self, tag, attrs):
        canonical = tuple(sorted((name, ' '.join(sorted(value.split())) if name == 'class' and value else value) for name, value in attrs))
        self.events.append(('start', tag, canonical))
        if tag not in self.VOID:
            self.stack.append(tag)

    def handle_startendtag(self, tag, attrs):
        self.handle_starttag(tag, attrs)
        if tag not in self.VOID:
            self.handle_endtag(tag)

    def handle_endtag(self, tag):
        if tag in self.VOID:
            return
        self.events.append(('end', tag))
        if self.stack and self.stack[-1] == tag:
            self.stack.pop()

    def handle_data(self, data):
        if not data or (data.isspace() and '\n' in data and not any(tag in self.TEXT for tag in self.stack)):
            return
        if self.events and self.events[-1][0] == 'text':
            self.events[-1] = ('text', self.events[-1][1] + data)
        else:
            self.events.append(('text', data))

    def handle_comment(self, data):
        self.events.append(('comment', data))


def canonical_html(value):
    parser = HtmlEvents()
    parser.feed(value)
    parser.close()
    return parser.events


def probe_request(source, includes=None):
    """Length-frame virtual sources without a JSON parser dependency in Roc."""
    if includes is None:
        return [], source.encode('utf-8')
    fields = [b'ADOC1\n', str(len(includes)).encode() + b'\n']
    for path, content in sorted(includes.items()):
        for value in (path, content):
            raw = value.encode('utf-8')
            fields.extend((str(len(raw)).encode() + b'\n', raw))
    fields.append(source.encode('utf-8'))
    return ['--request'], b''.join(fields)


def bounded_exchange(process, payload: bytes, timeout: float, *, line: bool = False, max_output: int = MAX_OUTPUT) -> bytes:
    """Drain both streams while writing, bounding bytes and elapsed time."""
    selector = selectors.DefaultSelector()
    output = bytearray()
    errors = bytearray()
    offset = 0
    deadline = time.monotonic() + timeout
    for stream, events, name in ((process.stdin, selectors.EVENT_WRITE, 'in'), (process.stdout, selectors.EVENT_READ, 'out'), (process.stderr, selectors.EVENT_READ, 'err')):
        os.set_blocking(stream.fileno(), False)
        selector.register(stream, events, name)
    try:
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError('process response deadline exceeded')
            for key, _ in selector.select(min(remaining, .1)):
                if key.data == 'in':
                    try:
                        written = os.write(key.fd, payload[offset:offset + 65536])
                    except BrokenPipeError:
                        written = 0
                        offset = len(payload)
                    offset += written
                    if offset >= len(payload):
                        selector.unregister(key.fileobj)
                        if not line:
                            key.fileobj.close()
                else:
                    chunk = os.read(key.fd, 65536)
                    if not chunk:
                        selector.unregister(key.fileobj)
                        continue
                    destination = output if key.data == 'out' else errors
                    destination.extend(chunk)
                    if len(output) + len(errors) > max_output:
                        raise ValueError('process output limit exceeded')
            if line and b'\n' in output:
                if not output.endswith(b'\n') or output.count(b'\n') != 1:
                    raise ValueError('oracle emitted multiple protocol records')
                return bytes(output)
            if process.poll() is not None and not any(key.data == 'out' for key in selector.get_map().values()):
                if process.returncode:
                    raise RuntimeError(f'process exited {process.returncode}: {errors.decode(errors="replace")[:2000]}')
                if line:
                    raise ValueError('oracle exited without a response')
                return bytes(output)
    finally:
        selector.close()


def stop(process):
    if process.poll() is None:
        process.kill()
    process.wait(timeout=5)
    for stream in (process.stdin, process.stdout, process.stderr):
        if stream and not stream.closed:
            stream.close()


def canonical_projection(value):
    if not isinstance(value, dict) or set(value) != {'title', 'blocks'} or not isinstance(value['title'], str) or not isinstance(value['blocks'], list):
        raise ValueError('invalid document projection')
    count = 0
    def node(block, depth):
        nonlocal count
        count += 1
        if depth > 128 or count > 50000:
            raise ValueError('projection tree limit exceeded')
        if not isinstance(block, dict) or set(block) != {'context', 'level', 'title', 'source', 'blocks'}:
            raise ValueError('invalid block projection fields')
        if any(not isinstance(block[key], str) for key in ('context', 'title', 'source')) or not block['context'] or type(block['level']) is not int or block['level'] < 0 or not isinstance(block['blocks'], list):
            raise ValueError('invalid block projection types')
        for child in block['blocks']:
            node(child, depth + 1)
    for block in value['blocks']:
        node(block, 0)
    return value


class Oracle:
    def __init__(self, gem_home=DEFAULT_HOME, timeout=5, command=None):
        self.timeout = timeout
        self.next_id = 0
        env = {key: value for key, value in os.environ.items() if key not in ('RUBYOPT', 'RUBYLIB', 'GEM_HOME', 'GEM_PATH', 'BUNDLE_GEMFILE')}
        env.update(GEM_HOME=str(Path(gem_home).resolve()), GEM_PATH=str(Path(gem_home).resolve()), TZ='UTC', LANG='C.UTF-8', LC_ALL='C.UTF-8', SOURCE_DATE_EPOCH='946684800')
        self.process = subprocess.Popen(command or ['ruby', str(DATA / 'oracle.rb')], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env, cwd=ROOT, bufsize=0)

    def __enter__(self):
        return self

    def __exit__(self, *args):
        stop(self.process)

    def request(self, source, includes=None):
        if not isinstance(source, str) or len(source.encode()) > MAX_SOURCE:
            return {'status': 'protocol_error', 'message': 'source size/type limit exceeded'}
        request_id = self.next_id
        self.next_id += 1
        try:
            request = {'id': request_id, 'source': source}
            if includes is not None:
                if not isinstance(includes, dict) or len(includes) > 128 or any(not isinstance(path, str) or not isinstance(text, str) for path, text in includes.items()) or len(source.encode()) + sum(len(path.encode()) + len(text.encode()) for path, text in includes.items()) > MAX_SOURCE:
                    raise ValueError('virtual includes type/count/total-byte limit exceeded')
                request['includes'] = includes
            payload = (json.dumps(request, ensure_ascii=False) + '\n').encode()
            result = json.loads(bounded_exchange(self.process, payload, self.timeout, line=True))
            if not isinstance(result, dict) or type(result.get('id')) is not int or result['id'] != request_id:
                raise ValueError('oracle response id mismatch')
            if result.get('status') == 'error':
                return {**result, 'status': 'oracle_policy_rejection' if result.get('error') == 'VirtualIncludePolicyError' and result.get('error_kind') == 'virtual_include_policy' else 'oracle_error'}
            if result.get('status') != 'ok' or result.get('version') != VERSION or not isinstance(result.get('html'), str) or not isinstance(result.get('warnings'), list) or not isinstance(result.get('ast'), dict) or not isinstance(result.get('unsupported'), list):
                raise ValueError('invalid oracle response envelope')
            canonical_projection(result['projection'])
            if any(not isinstance(warning, dict) or not isinstance(warning.get('message'), str) or not isinstance(warning.get('severity'), str) for warning in result['warnings']):
                raise ValueError('invalid oracle warnings')
            return result
        except TimeoutError as error:
            stop(self.process)
            return {'status': 'timeout', 'message': str(error)}
        except (ValueError, TypeError, KeyError, RecursionError, OSError, RuntimeError) as error:
            stop(self.process)
            return {'status': 'protocol_error', 'message': str(error)}


def run_probe(executable, source, timeout=5, includes=None):
    process = None
    try:
        arguments, payload = probe_request(source, includes)
        process = subprocess.Popen([str(Path(executable).resolve()), *arguments], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=ROOT, bufsize=0)
        result = json.loads(bounded_exchange(process, payload, timeout))
        if isinstance(result, dict) and 'title' in result:
            result = {'status': 'ok', 'projection': result}
        if not isinstance(result, dict) or result.get('status') not in ('ok', 'unsupported', 'error'):
            raise ValueError('invalid probe response status')
        if result['status'] == 'ok':
            canonical_projection(result['projection'])
        elif not isinstance(result.get('reason', result.get('message')), str):
            raise ValueError('probe rejection needs a reason')
        return result
    except TimeoutError as error:
        return {'status': 'timeout', 'message': str(error)}
    except RuntimeError as error:
        return {'status': 'crash', 'message': str(error)}
    except (ValueError, TypeError, KeyError, RecursionError, OSError) as error:
        return {'status': 'protocol_error', 'message': str(error)}
    finally:
        if process is not None:
            stop(process)


def contexts(projection):
    def visit(block):
        return [block['context']] + [item for child in block['blocks'] for item in visit(child)]
    return [item for block in projection['blocks'] for item in visit(block)]


def expectation_errors(case, oracle):
    expected = case.get('expect', {})
    if 'expected' in case:
        expected = {'projection': case['expected']}
    if 'title' in expected and oracle['projection']['title'] != expected['title']:
        return ['title disagrees']
    errors = []
    if 'projection' in expected and oracle['projection'] != expected['projection']:
        errors.append('authored projection disagrees')
    if 'contexts' in expected and contexts(oracle['projection']) != expected['contexts']:
        errors.append('authored contexts disagree')
    for text in expected.get('html_contains', []):
        if text not in oracle['html']:
            errors.append(f'HTML missing {text!r}')
    for text in expected.get('html_excludes', []):
        if text in oracle['html']:
            errors.append(f'HTML unexpectedly contains {text!r}')
    for text in expected.get('warnings_contains', []):
        if not any(text in warning['message'] for warning in oracle['warnings']):
            errors.append(f'missing warning {text!r}')
    if 'warning_severities' in expected and [warning['severity'] for warning in oracle['warnings']] != expected['warning_severities']:
        errors.append('warning severities disagree')
    return errors


def validate_fixture(fixture):
    if fixture.get('schema_version') != 1 or fixture.get('oracle_version') != VERSION:
        raise ValueError('fixture schema/oracle version mismatch')
    cases = fixture['cases']
    if not cases or len({case['id'] for case in cases}) != len(cases):
        raise ValueError('empty suite or duplicate fixture ids')
    coverage = fixture.get('coverage', [])
    if not coverage or len({group['category'] for group in coverage}) != len(coverage):
        raise ValueError('missing or duplicate coverage categories')
    categories = {group['category']: set(group['required_cases']) for group in coverage}
    actual = {}
    for case in cases:
        if not isinstance(case.get('source'), str) or case.get('support') not in ('minimal', 'complete', 'full', 'future'):
            raise ValueError('fixture needs source and explicit implementation scope')
        expected = case.get('expect')
        if not isinstance(expected, dict) or not expected or set(expected) - {'title', 'projection', 'contexts', 'html_contains', 'html_excludes', 'warnings_contains', 'warning_severities'}:
            raise ValueError('fixture needs known authored expectations')
        if case['support'] == 'minimal' and 'projection' not in expected:
            raise ValueError('minimal fixture needs full authored projection')
        if 'projection' in expected:
            canonical_projection(expected['projection'])
        for key in ('contexts', 'html_contains', 'html_excludes', 'warnings_contains', 'warning_severities'):
            if key in expected and (not isinstance(expected[key], list) or any(not isinstance(value, str) for value in expected[key])):
                raise ValueError('expectation predicates must be lists of strings')
        actual.setdefault(case['category'], set()).add(case['id'])
    if set(actual) != set(categories) or any(not required or not required.issubset(actual[category]) for category, required in categories.items()):
        raise ValueError('coverage manifest does not match fixture categories/required cases')
    return cases


def classify(case, oracle, actual=None, *, require_complete=False, compare_html=False):
    if oracle['status'] == 'oracle_policy_rejection':
        if actual is not None and actual['status'] in ('timeout', 'crash', 'protocol_error'):
            return actual['status'], []
        if case.get('support') == 'explore' and not case.get('expect') and 'expected' not in case:
            return 'oracle_policy_rejection', []
        return 'oracle_policy_disagreement', ['authored/model case exceeds the explicit virtual include policy']
    if oracle['status'] != 'ok':
        return oracle['status'], []
    errors = expectation_errors(case, oracle)
    if errors:
        return 'oracle_disagreement', errors
    if actual is None:
        return 'oracle_verified', []
    if actual['status'] in ('timeout', 'crash', 'protocol_error'):
        return actual['status'], []
    if actual['status'] != 'ok':
        if require_complete:
            return 'coverage_gap', [actual.get('reason', actual.get('message', 'parser rejected reference input'))]
        return ('valid_rejection' if case.get('support') in ('minimal', 'complete', 'full') else 'unsupported'), []
    if actual['projection'] != oracle['projection']:
        return 'wrong_projection', []
    if compare_html:
        if not isinstance(actual.get('html'), str):
            return 'missing_html', ['probe did not return rendered HTML']
        if canonical_html(actual['html']) != canonical_html(oracle['html']):
            return 'wrong_html', ['rendered DOM events disagree']
    if case.get('reject') is True and not require_complete:
        return 'unexpected_acceptance', []
    if case.get('support') == 'future' and not require_complete:
        return 'structural_only', []
    return 'pass', []


def load_module(name):
    spec = importlib.util.spec_from_file_location('asciidoc_' + name, DATA / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def file_sha256(path):
    try:
        return hashlib.sha256(path.read_bytes()).hexdigest() if path else None
    except OSError:
        return None


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + '\n')


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['check', 'replay', 'differential'])
    parser.add_argument('input', nargs='?', type=Path)
    parser.add_argument('--probe', type=Path)
    parser.add_argument('--oracle-only', action='store_true')
    parser.add_argument('--require-complete', action='store_true', help='fail reference-accepted parser rejection; supersedes scaffold future/reject annotations')
    parser.add_argument('--compare-html', action='store_true', help='require DOM-equivalent HTML as well as block source')
    parser.add_argument('--gem-home', type=Path, default=DEFAULT_HOME)
    parser.add_argument('--fixtures', type=Path, default=DATA / 'fixtures.json')
    parser.add_argument('--generated', type=int, default=0)
    parser.add_argument('--mutations', type=int, default=0)
    parser.add_argument('--seed', type=int, default=1)
    parser.add_argument('--corpus', type=Path)
    parser.add_argument('--case', action='append')
    parser.add_argument('--timeout', type=float, default=5)
    parser.add_argument('--output-dir', type=Path, default=ROOT / '.roc-parser-tmp/asciidoc/review')
    args = parser.parse_args(argv)
    if args.timeout <= 0 or args.generated < 0 or args.mutations < 0:
        parser.error('timeout must be positive and campaign counts nonnegative')
    if not args.oracle_only and not args.probe:
        parser.error('--probe PATH or --oracle-only is required')
    if args.oracle_only and (args.require_complete or args.compare_html):
        parser.error('complete/HTML comparison requires a probe')
    if args.command == 'check':
        fixture_paths = [args.fixtures]
        if args.fixtures.resolve() == (DATA / 'fixtures.json').resolve():
            fixture_paths += sorted(DATA.glob('*-fixtures.json'))
        cases = []
        try:
            for fixture_path in fixture_paths:
                cases += validate_fixture(json.loads(fixture_path.read_text()))
            if len({case['id'] for case in cases}) != len(cases):
                raise ValueError('duplicate case ids across fixture suites')
        except (ValueError, KeyError, TypeError) as error:
            parser.error(str(error))
    elif args.command == 'replay':
        if not args.input:
            parser.error('replay requires an input file')
        if args.input.suffix == '.json':
            replay = json.loads(args.input.read_text())
            case = replay.get('case', replay)
            if not isinstance(case, dict) or not isinstance(case.get('source'), str) or not isinstance(case.get('id'), str):
                parser.error('replay JSON needs a saved case with id/source/expectation')
            cases = [case]
        else:
            cases = [{'id': args.input.name, 'source': args.input.read_bytes().decode('utf-8'), 'support': 'explore'}]
    else:
        if not args.input or not args.input.is_dir():
            parser.error('differential requires a corpus directory')
        cases = load_module('models').harvest_corpus(args.input)
        for case in cases:
            case['support'] = 'explore'
    if args.generated:
        cases += load_module('models').generate_cases(args.seed, args.generated)
    if args.corpus:
        cases += [{**case, 'support': 'explore'} for case in load_module('models').harvest_corpus(args.corpus)]
    if args.mutations:
        cases += load_module('mutations').mutate_cases(cases, args.mutations, args.seed)
    if args.case:
        missing = set(args.case) - {case['id'] for case in cases}
        if missing:
            parser.error('unknown case ids: ' + ', '.join(sorted(missing)))
        cases = [case for case in cases if case['id'] in args.case]
    if not cases:
        parser.error('refusing empty campaign')
    rows = []
    with Oracle(args.gem_home, args.timeout) as oracle:
        for case in cases:
            expected = oracle.request(case['source'], case.get('includes'))
            actual = run_probe(args.probe, case['source'], args.timeout, case.get('includes')) if args.probe else None
            kind, errors = classify(case, expected, actual, require_complete=args.require_complete, compare_html=args.compare_html)
            request_sha256 = hashlib.sha256(json.dumps({'source': case['source'], 'includes': case.get('includes')}, ensure_ascii=False, sort_keys=True).encode()).hexdigest()
            row = {'request_sha256': request_sha256, 'case': case, 'id': case['id'], 'category': case.get('category', 'generated'), 'support': case.get('support', 'minimal'), 'kind': kind, 'source': case['source'], 'sha256': hashlib.sha256(case['source'].encode()).hexdigest(), 'oracle': expected, 'actual': actual, 'expectation_errors': errors}
            rows.append(row)
            if kind in FATAL:
                artifact = args.output_dir / 'failures' / ((request_sha256 if 'includes' in case else row['sha256']) + '.adoc')
                artifact.parent.mkdir(parents=True, exist_ok=True)
                artifact.write_bytes(case['source'].encode('utf-8'))
                write_json(artifact.with_suffix('.json'), row)
    gem_sha256 = load_module('setup_oracle').SHA256
    revision = subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=ROOT, capture_output=True, text=True)
    ruby = subprocess.run(['ruby', '--version'], capture_output=True, text=True)
    parser_sources = {path.name: file_sha256(path) for path in sorted((ROOT / 'package').glob('AsciiDoc*.roc'))}
    report = {'require_complete': args.require_complete, 'compare_html': args.compare_html, 'comparison_scope': 'block-source-and-rendered-dom' if args.compare_html else 'block-source-projection', 'virtual_include_policy': 'caller maps only; native line/tag selectors with confined SAFE include reading; SECURE conversion', 'command': [sys.executable, str(SCRIPT_PATH)] + (argv if argv is not None else sys.argv[1:]), 'probe_command': [str(args.probe.resolve())] if args.probe else None, 'git_head': revision.stdout.strip() if revision.returncode == 0 else None, 'ruby_version': ruby.stdout.strip(), 'gem_sha256': gem_sha256, 'schema_version': 1, 'oracle_version': VERSION, 'oracle_sha256': hashlib.sha256((DATA / 'oracle.rb').read_bytes()).hexdigest(), 'fixture_sha256': hashlib.sha256(args.fixtures.read_bytes()).hexdigest() if args.fixtures.exists() else None, 'probe_sha256': file_sha256(args.probe), 'parser_sha256': hashlib.sha256((ROOT / 'package/AsciiDoc.roc').read_bytes()).hexdigest() if (ROOT / 'package/AsciiDoc.roc').exists() else None, 'seed': args.seed, 'total': len(rows), 'counts': dict(Counter(row['kind'] for row in rows)), 'categories': dict(Counter(row['category'] for row in rows)), 'cases': rows}
    report['parser_sources_sha256'] = parser_sources
    report['coverage_counts'] = {category: dict(Counter(row['kind'] for row in rows if row['category'] == category)) for category in sorted({row['category'] for row in rows})}
    report['fixture_suites'] = {str(path.relative_to(ROOT) if path.is_relative_to(ROOT) else path): file_sha256(path) for path in fixture_paths} if args.command == 'check' else {}
    write_json(args.output_dir / (args.command + '.json'), report)
    print(json.dumps({key: report[key] for key in ('total', 'counts', 'categories')}, indent=2))
    return int(any(row['kind'] in FATAL for row in rows))


if __name__ == '__main__':
    raise SystemExit(main())
