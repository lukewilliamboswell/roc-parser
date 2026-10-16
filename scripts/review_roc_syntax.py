#!/usr/bin/env python3
"""Compare persistent Roc/Zig syntax workers; retain every mismatch for replay."""
from __future__ import annotations
import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import selectors
import shlex
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / 'scripts' / 'roc_syntax'
PROTOCOL_VERSION = 1
REFERENCE_COMMIT = '7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5'

class WorkerError(RuntimeError):
    pass

class Worker:
    """A bounded, persistent JSONL exchange. stderr cannot fill a pipe."""
    def __init__(self, command, timeout=5.0, max_response=16 * 1024 * 1024):
        self.command = list(command)
        self.timeout = timeout
        self.max_response = max_response
        self.stderr = tempfile.TemporaryFile()
        self.proc = subprocess.Popen(self.command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=self.stderr, bufsize=0)
        self.selector = selectors.DefaultSelector()
        self.selector.register(self.proc.stdout, selectors.EVENT_READ)
        self.pending = b''
        self.requests = 0

    def request(self, value):
        if self.pending:
            raise WorkerError('unsolicited worker output')
        payload = (json.dumps(value, ensure_ascii=True) + '\n').encode()
        # The protocol's source limit also bounds write-side pipe pressure.
        deadline = time.monotonic() + self.timeout
        os.set_blocking(self.proc.stdin.fileno(), False)
        sent = 0
        while sent < len(payload):
            if time.monotonic() > deadline:
                raise WorkerError('worker request write timeout')
            try:
                sent += os.write(self.proc.stdin.fileno(), payload[sent:])
            except BlockingIOError:
                import select
                select.select([], [self.proc.stdin], [], min(.05, max(0, deadline-time.monotonic())))
            except BrokenPipeError as error:
                raise WorkerError(f'worker exited {self.proc.poll()} writing request') from error
        while b'\n' not in self.pending:
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not self.selector.select(remaining):
                raise WorkerError('worker response timeout')
            chunk = os.read(self.proc.stdout.fileno(), 65536)
            if not chunk:
                self.stderr.seek(0)
                detail = self.stderr.read(4096).decode(errors='replace')
                raise WorkerError(f'worker exited {self.proc.poll()}: {detail}')
            self.pending += chunk
            if len(self.pending) > self.max_response:
                raise WorkerError('worker response exceeds limit')
        line, self.pending = self.pending.split(b'\n', 1)
        try:
            result = json.loads(line)
        except (ValueError, UnicodeError) as error:
            raise WorkerError(f'invalid worker JSON: {line[:200]!r}') from error
        if not isinstance(result, dict) or result.get('id') != value['id']:
            raise WorkerError('worker response ID mismatch')
        if result.get('version') != PROTOCOL_VERSION or 'error' in result:
            raise WorkerError(f'worker protocol error: {result}')
        self.requests += 1
        return result

    def close(self):
        self.selector.close()
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=1)
            except subprocess.TimeoutExpired:
                self.proc.kill()
                self.proc.wait()
        self.proc.stdin.close()
        self.proc.stdout.close()
        self.stderr.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()


def normalize_tokens(response, source):
    """Compare lossless token boundaries and lexical classes, excluding trivia."""
    out = []
    raw = source.encode()
    for token in response['tokens']:
        kind = token['kind']
        if kind in {'Whitespace', 'Comment', 'Newline', 'EndOfFile'}:
            continue
        start, end = token['start'], token['end']
        if not isinstance(start, int) or not isinstance(end, int) or not 0 <= start <= end <= len(raw):
            raise WorkerError(f'invalid token span: {token}')
        if kind in {'Int', 'Float', 'Number', 'MalformedNumber', 'MalformedUnicodeEscape'}:
            kind = 'Number'
        elif (kind.startswith('Op') and kind[2:3].isupper()) or kind == 'Operator':
            kind = 'Operator'
        elif kind in {'Comma', 'Dot', 'Colon'}:
            kind = 'Punctuation'
        elif kind.startswith('Kw'):
            kind = 'Keyword'
        out.append([kind, start, end])
    return out


def syntax_shape(response, source):
    """Parent/child structure is compared, so swapping precedence changes output."""
    if 'shape' in response:
        return response['shape']
    nodes, children = response.get('nodes', []), response.get('children', [])
    if not nodes:
        return [fact.get('shape', ['unsupported', fact.get('node_kind', fact['kind'])])
                for fact in response.get('facts', [])]
    active = set()
    def visit(index):
        if index in active or not 0 <= index < len(nodes):
            raise WorkerError('invalid or cyclic syntax children')
        active.add(index)
        node = nodes[index]
        start, length = node['children_start'], node['children_len']
        if start < 0 or length < 0 or start + length > len(children):
            raise WorkerError('invalid children span')
        kind = node['kind']
        branch = [visit(child) for child in children[start:start+length]]
        active.remove(index)
        if kind in {'File', 'Module'}:
            return branch
        # Parentheses are concrete syntax; grouping already determines topology.
        if kind in {'Group', 'Parenthesized'} and len(branch) == 1:
            return branch[0]
        text = source.encode()[node['start']:node['end']].decode(errors='replace')
        if kind in {'Number', 'NumberLiteral', 'Int', 'Integer'}:
            return ['number', text]
        if kind in {'Identifier', 'LowerIdent', 'UpperIdent'}:
            return ['identifier', text]
        if kind == 'Declaration':
            return branch[-1]
        if kind == 'ExpressionStatement':
            return branch[0]
        if kind == 'Binary':
            ids = children[start:start+length]
            left, right = nodes[ids[0]], nodes[ids[1]]
            gap = source.encode()[left['end']:right['start']].decode()
            # Comments between operands are trivia; the operator is lexical.
            operators = [t for t in response['tokens'] if t['start'] >= left['end'] and t['end'] <= right['start'] and t['kind'] == 'Operator']
            operator = ''.join(source.encode()[t['start']:t['end']].decode() for t in operators)
            return ['binary', operator, *branch]
        if kind == 'Unary':
            first = nodes[children[start]]
            operator = source.encode()[node['start']:first['start']].decode().strip()
            return ['unary', operator, *branch]
        if kind == 'Call':
            return ['call', branch[0], branch[1:]]
        return [kind.lower(), *branch]
    return visit(response['root'])


def compare(case, actual, expected):
    differences = []
    for field in ('ok', 'consumed'):
        if actual.get(field) != expected.get(field):
            differences.append({'field': field, 'actual': actual.get(field), 'expected': expected.get(field)})
    for field, projector in [('tokens', normalize_tokens), ('shape', syntax_shape)]:
        left, right = projector(actual, case['source']), projector(expected, case['source'])
        if left != right:
            differences.append({'field': field, 'actual': left, 'expected': right})
    if 'model_shape' in case:
        for label, response in [('actual', actual), ('expected', expected)]:
            shape = syntax_shape(response, case['source'])
            if shape != case['model_shape']:
                differences.append({'field': label + '_model_shape', 'actual': shape, 'expected': case['model_shape']})
    return differences


def load_cases(path):
    document = json.loads(Path(path).read_text())
    if document.get('version') != PROTOCOL_VERSION or document.get('reference_commit') != REFERENCE_COMMIT:
        raise ValueError('fixture protocol/reference pin differs')
    cases = document['cases']
    ids = [case['id'] for case in cases]
    if len(ids) != len(set(ids)):
        raise ValueError('duplicate fixture IDs')
    if {case['rule'] for case in cases} != set(document['coverage']):
        raise ValueError('coverage inventory differs from fixture rules')
    for case in cases:
        if case['coverage'] not in {'supported', 'recovery', 'unsupported', 'exploratory'}:
            raise ValueError('unknown coverage classification')
        if case['mode'] not in {'file', 'expr'}:
            raise ValueError('unknown parse mode')
    return document, cases


def run_cases(cases, probe_command, oracle_command, artifact_dir, timeout=5):
    artifact_dir = Path(artifact_dir)
    artifact_dir.mkdir(parents=True, exist_ok=True)
    results = []
    workers = {}
    try:
        for case in cases:
            request = {key: case[key] for key in ('id', 'mode', 'source')}
            record = {'id': case['id'], 'rule': case.get('rule'), 'coverage': case.get('coverage', 'replay'),
                      'source_sha256': hashlib.sha256(case['source'].encode()).hexdigest(), 'request': request}
            if 'model_shape' in case:
                record['model_shape'] = case['model_shape']
            for name, command in [('actual', probe_command), ('expected', oracle_command)]:
                try:
                    if name not in workers:
                        workers[name] = Worker(command, timeout=timeout)
                    record[name] = workers[name].request(request)
                except (WorkerError, OSError) as error:
                    record[name] = {'worker_error': str(error)}
                    if name in workers:
                        workers.pop(name).close()
            if any('worker_error' in record[name] for name in ('actual', 'expected')):
                record['differences'] = [{'field': 'worker', 'actual': record['actual'], 'expected': record['expected']}]
            else:
                try:
                    record['differences'] = compare(case, record['actual'], record['expected'])
                except (WorkerError, KeyError, TypeError, IndexError) as error:
                    record['differences'] = [{'field': 'protocol', 'error': str(error)}]
            record['pass'] = not record['differences']
            results.append(record)
            if not record['pass']:
                # Hash IDs rather than trusting paths from fixture files.
                stem = hashlib.sha256(case['id'].encode()).hexdigest()[:16]
                (artifact_dir / f'{stem}.json').write_text(json.dumps(record, indent=2) + '\n')
    finally:
        for worker in workers.values():
            worker.close()
    return results


def generated_cases(count, seed):
    import random
    rng = random.Random(seed)
    def expression(depth):
        choice = rng.randrange(5 if depth else 2)
        if choice == 0:
            value = str(rng.randrange(100000))
            return value, ['number', value]
        if choice == 1:
            value = 'v' + str(rng.randrange(100))
            return value, ['identifier', value]
        if choice in {2, 3}:
            left, lshape = expression(depth - 1)
            right, rshape = expression(depth - 1)
            op = '+' if choice == 2 else '*'
            return f'({left} {op} {right})', ['binary', op, lshape, rshape]
        arg, ashape = expression(depth - 1)
        return f'f({arg})', ['call', ['identifier', 'f'], [ashape]]
    result = []
    for index in range(count):
        source, shape = expression(rng.randrange(1, 5))
        result.append({'id': f'generated-{seed}-{index}', 'mode': 'expr', 'source': source,
                       'rule': 'generated-expressions', 'coverage': 'supported', 'model_shape': [shape]})
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe-command', required=True, help='persistent probe command, shell quoting accepted without shell execution')
    parser.add_argument('--oracle-command', required=True)
    parser.add_argument('--fixtures', type=Path, default=DATA / 'fixtures.json')
    parser.add_argument('--generated', type=int, default=0, help='independent deterministic expression cases')
    parser.add_argument('--seed', type=int, default=1)
    parser.add_argument('--replay', type=Path, help='replay a retained mismatch artifact')
    parser.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp' / 'roc-syntax-review')
    parser.add_argument('--timeout', type=float, default=5)
    args = parser.parse_args(argv)
    metadata, cases = load_cases(args.fixtures)
    if args.replay:
        saved = json.loads(args.replay.read_text())
        cases = [{**saved['request'], 'coverage': saved.get('coverage', 'replay'), 'rule': saved.get('rule')}]
        if 'model_shape' in saved:
            cases[0]['model_shape'] = saved['model_shape']
    cases += generated_cases(args.generated, args.seed)
    started = time.monotonic()
    results = run_cases(cases, shlex.split(args.probe_command), shlex.split(args.oracle_command), args.output / 'failures', args.timeout)
    elapsed = time.monotonic() - started
    identities = {}
    for name, command in [('probe', args.probe_command), ('oracle', args.oracle_command)]:
        path = Path(shlex.split(command)[0])
        identities[name] = {'sha256': hashlib.sha256(path.read_bytes()).hexdigest()} if path.is_file() else {'command': str(path)}
    report = {'metadata': metadata, 'worker_identities': identities, 'elapsed_seconds': elapsed, 'cases_per_second': len(cases) / elapsed, 'commands': {'probe': shlex.split(args.probe_command), 'oracle': shlex.split(args.oracle_command)},
              'coverage_counts': dict(Counter(item['coverage'] for item in results)),
              'counts': {'total': len(results), 'passed': sum(item['pass'] for item in results),
                         'failed': sum(not item['pass'] for item in results)}, 'results': results}
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report['counts']))
    return int(any(not item['pass'] for item in results))

if __name__ == '__main__':
    raise SystemExit(main())
