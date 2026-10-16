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
REFERENCE_COMMIT = '0d4b0ab02f9239448d034db17b4e63051a7c57ae'

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
        self.pending = bytearray()
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
            self.pending.extend(chunk)
            if len(self.pending) > self.max_response:
                raise WorkerError('worker response exceeds limit')
        line, self.pending = self.pending.split(b'\n', 1)
        try:
            result = json.loads(line)
        except (ValueError, UnicodeError, RecursionError) as error:
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
        # The compiler emits an empty StringPart for \"\"; the lossless
        # candidate omits zero-byte tokens to retain its progress invariant.
        if kind == 'StringPart' and start == end:
            continue
        if kind in {'DotQuestionLowerIdent', 'NoSpaceDotQuestionLowerIdent'}:
            out.append(['Punctuation', start, start + 2])
            out.append(['LowerIdent', start + 2, end])
            continue
        if kind in {'DotInt', 'NoSpaceDotInt'}:
            out.append(['Punctuation', start, start + 1])
            out.append(['Number', start + 1, end])
            continue
        if kind in {'DotLowerIdent', 'DotUpperIdent', 'NoSpaceDotLowerIdent', 'NoSpaceDotUpperIdent'}:
            out.append(['Punctuation', start, start + 1])
            out.append(['LowerIdent' if kind.endswith('LowerIdent') else 'UpperIdent', start + 1, end])
            continue
        if kind in {'Int', 'Float', 'Number', 'MalformedNumber', 'MalformedUnicodeEscape'}:
            kind = 'Number'
        elif kind in {'OpColon', 'OpColonEqual', 'OpDoubleColon'}:
            kind = 'Punctuation'
        elif (kind.startswith('Op') and kind[2:3].isupper()) or kind in {'Operator', 'NoSpaceOpQuestion'}:
            kind = 'Operator'
        elif kind in {'Comma', 'Dot', 'Colon', 'DoubleDot', 'TripleDot', 'DotStar'}:
            kind = 'Punctuation'
        elif kind in {'Underscore', 'NamedUnderscore'}:
            kind = 'LowerIdent'
        elif kind == 'OpaqueName':
            kind = 'UpperIdent'
        elif kind == 'OpenStringInterpolation':
            kind = 'InterpolationStart'
        elif kind == 'CloseStringInterpolation':
            kind = 'InterpolationEnd'
        elif kind.startswith('Kw'):
            kind = 'Keyword'
        out.append([kind, start, end])
    return out


def syntax_shape(response, source):
    """Parent/child structure is compared, so swapping precedence changes output."""
    if 'shape' in response:
        return response['shape']
    raw = source.encode()
    import bisect
    operators = [t for t in response.get('tokens', []) if t['kind'] == 'Operator']
    operator_starts = [t['start'] for t in operators]
    source_tokens = response.get('tokens', [])
    token_starts = [token['start'] for token in source_tokens]
    name_tokens = [token for token in source_tokens if token['kind'] in {'UpperIdent', 'LowerIdent'}]
    name_starts = [token['start'] for token in name_tokens]
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
        text = raw[node['start']:node['end']].decode(errors='replace')
        if kind in {'Number', 'NumberLiteral', 'Int', 'Integer'}:
            return ['number', text]
        if kind in {'Identifier', 'LowerIdent', 'UpperIdent'}:
            return ['identifier', text]
        if kind == 'TypeName':
            lo = bisect.bisect_left(name_starts, node['start'])
            hi = bisect.bisect_left(name_starts, node['end'])
            names = [raw[token['start']:token['end']].decode() for token in name_tokens[lo:hi]]
            if not names:
                return ['malformed_type_name', text]
            qualified = ['identifier', names[0]]
            for name in names[1:]:
                qualified = ['field', qualified, name]
            return ['t_named', qualified]
        if kind == 'TypeVariable':
            return ['t_var', text]
        if kind == 'TypeWildcard':
            return ['t_wildcard']
        if kind == 'TypeParenthesized':
            return branch[0]
        if kind == 'TypeApply':
            return ['t_apply', branch]
        if kind == 'TypeTuple':
            return ['t_tuple', branch]
        if kind == 'TypeFunction':
            ids = children[start:start+length]
            return_start = nodes[ids[-1]]['start']
            gap_start = nodes[ids[-2]]['end'] if length > 1 else node['start']
            lo = bisect.bisect_left(operator_starts, gap_start)
            hi = bisect.bisect_left(operator_starts, return_start)
            effectful = any(raw[token['start']:token['end']] == b'=>' for token in operators[lo:hi])
            return ['t_fn', effectful, branch[:-1], branch[-1]]
        if kind in {'TypeRecord', 'TypeTagUnion'}:
            extension = branch[-1] if branch and branch[-1][0] in {'t_open', 't_extension'} else None
            fields = branch[:-1] if extension is not None else branch
            return ['t_record' if kind == 'TypeRecord' else 't_union', fields, extension]
        if kind == 'TypeExtension':
            return ['t_extension', branch[0]] if branch else ['t_open']
        if kind == 'TypeField':
            ids = children[start:start+length]
            name_node, type_node = nodes[ids[0]], nodes[ids[1]]
            lo = bisect.bisect_left(operator_starts, name_node['end'])
            hi = bisect.bisect_left(operator_starts, type_node['start'])
            optional = any(raw[token['start']:token['end']] == b'?' for token in operators[lo:hi])
            return ['t_field', branch[0][1], optional, branch[1], branch[2] if length > 2 else None]
        if kind == 'TypeHeader':
            name_node = nodes[children[start]]
            name = raw[name_node['start']:name_node['end']].decode()
            return ['type_header', name, branch[1:]]
        if kind == 'WhereMethod':
            return ['where_method', branch[0], branch[1][1], branch[2]]
        if kind == 'WhereAlias':
            return ['where_alias', *branch]
        if kind == 'WhereClause':
            return ['where', branch]
        if kind == 'AssociatedBlock':
            return ['associated', branch]
        if kind == 'TypeAnnotation':
            first_token = source_tokens[bisect.bisect_left(token_starts, node['start'])]
            is_var = raw[first_token['start']:first_token['end']] == b'var'
            return ['annotation', branch[0][1], branch[1], branch[2] if length > 2 else None, is_var]
        if kind == 'TypeDeclaration':
            ids = children[start:start+length]
            lo = bisect.bisect_left(token_starts, nodes[ids[0]]['end'])
            hi = bisect.bisect_left(token_starts, nodes[ids[1]]['start'])
            spellings = {raw[token['start']:token['end']] for token in source_tokens[lo:hi] if token['kind'] in {'Operator', 'Punctuation'}}
            declaration_kind = 'nominal' if b':=' in spellings else 'opaque' if b'::' in spellings else 'alias'
            first_token = source_tokens[bisect.bisect_left(token_starts, node['start'])]
            if first_token['kind'] == 'LowerIdent':
                declaration_kind = 'where_alias'
            where = next((item for item in branch[2:] if item[0] == 'where'), None)
            associated = next((item for item in branch[2:] if item[0] == 'associated'), None)
            return ['type_decl', declaration_kind, branch[0], branch[1], where, associated]

        if kind == 'Declaration':
            return ['decl', branch[0], branch[-1]]
        if kind == 'VariableDeclaration':
            return ['var', branch[0][1], branch[1] if len(branch) > 1 else None]
        if kind == 'Expect':
            return ['expect', branch[0]]
        if kind == 'While':
            return ['while', *branch]
        if kind == 'Header':
            first_token = source_tokens[bisect.bisect_left(token_starts, node['start'])] if text.strip() else None
            keyword = raw[first_token['start']:first_token['end']].decode() if first_token else 'type_module'
            if keyword in {'app', 'package'} and (not branch or branch[-1][0] != 'dependencies'):
                branch.append(['dependencies', []])
            return ['header', keyword, branch]
        if kind == 'Dependencies':
            return ['dependencies', branch]
        if kind == 'DependencyField':
            return ['dependency', branch[0][1], branch[1] if len(branch) > 1 else None]
        if kind == 'PlatformReference':
            return ['platform', branch[0]]
        if kind in {'PlatformRequires', 'PlatformProvides', 'PlatformHosted'}:
            tag = {'PlatformRequires': 'platform_requires', 'PlatformProvides': 'platform_provides', 'PlatformHosted': 'platform_hosted'}[kind]
            return [tag, branch]
        if kind == 'RequiredValue':
            return ['required_value', branch[:-2], branch[-2][1], branch[-1]]
        if kind == 'RequiredType':
            return ['required_type', *branch]
        if kind == 'SymbolMapping':
            return ['symbol_mapping', branch[0][1], branch[1][1]]
        if kind == 'PlatformTargets':
            # The compiler retains the last inputs_dir and stores it separately
            # from target entries. The candidate tree still retains every node.
            directories = [entry for entry in branch if entry[0] == 'target_inputs_directory']
            entries = [entry for entry in branch if entry[0] != 'target_inputs_directory']
            return ['platform_targets', directories[-1:] + entries]
        if kind == 'TargetInputsDirectory':
            return ['target_inputs_directory', branch[0]]
        if kind == 'TargetEntry':
            return ['target_entry', branch[0][1], branch[1]]
        if kind == 'TargetConfig':
            return ['target_config', branch]
        if kind == 'TargetField':
            return ['target_field', branch[0][1], branch[1] if len(branch) > 1 else None]
        if kind == 'TargetList':
            return ['target_list', branch]
        if kind == 'TargetFile':
            return ['target_file', branch[0]]
        if kind == 'ImportPath':
            return ['import_path', text]
        if kind == 'ImportAlias':
            return ['import_alias', branch[0][1]]
        if kind == 'Import':
            alias = next((value[1] for value in branch[1:] if value[0] == 'import_alias'), None)
            exposed = [value for value in branch[1:] if value[0] in {'exposed', 'exposed_wildcard'}]
            return ['import', branch[0][1], alias, exposed]
        if kind == 'ExposedItem':
            return ['exposed', branch[0], branch[1][1] if len(branch) > 1 else None]
        if kind == 'ExposedWildcard':
            return ['exposed_wildcard', branch[0]]
        if kind == 'FileImport':
            type_node = nodes[children[start+2]]
            type_name = ''.join(raw[type_node['start']:type_node['end']].decode().split())
            return ['file_import', branch[0][1][1:-1], branch[1][1], type_name]
        if kind == 'ExpressionStatement':
            return branch[0]
        if kind == 'Binary':
            ids = children[start:start+length]
            left, right = nodes[ids[0]], nodes[ids[1]]
            gap = raw[left['end']:right['start']].decode()
            # Comments between operands are trivia; the operator is lexical.
            lo = bisect.bisect_left(operator_starts, left['end'])
            hi = bisect.bisect_left(operator_starts, right['start'])
            operator = ''.join(raw[t['start']:t['end']].decode() for t in operators[lo:hi])
            return ['binary', operator, *branch]
        if kind == 'Unary':
            first = nodes[children[start]]
            operator = raw[node['start']:first['start']].decode().strip()
            return ['unary', operator, *branch]
        if kind in {'StringLiteral', 'MultilineStringLiteral'}:
            interpolated = any(nodes[index]['kind'] == 'Interpolation' for index in children[start:start+length])
            if kind == 'StringLiteral' and not interpolated:
                return ['string', text]
            return ['multiline_string' if kind == 'MultilineStringLiteral' else 'interpolated_string', branch]
        if kind == 'StringSegment':
            return ['text', text]
        if kind == 'PatternStringLiteral':
            return ['p_string_parts', branch]
        if kind == 'CapturePattern':
            return ['capture', branch[0][1] if branch[0][0] == 'p_identifier' else None]
        if kind == 'Interpolation':
            return ['interpolation', branch[0]]
        if kind == 'TypedLiteral':
            return ['typed', branch[0], [name[1] for name in branch[1:]]]
        if kind == 'CharacterLiteral':
            return ['character', text]
        if kind == 'Suffix':
            return ['suffix', '?', *branch]
        if kind == 'ListLiteral':
            return ['list', branch]
        if kind == 'Tuple':
            return branch[0] if len(branch) == 1 else ['tuple', branch]
        if kind == 'Lambda':
            return ['lambda', branch[:-1], branch[-1]]
        if kind == 'Pattern':
            return text.strip()
        if kind == 'IdentifierPattern':
            return ['p_identifier', text]
        if kind == 'WildcardPattern':
            return ['p_wildcard']
        if kind == 'MutablePattern':
            return ['p_mutable', branch[0][1]]
        if kind in {'TagPattern', 'NominalPattern'}:
            called = text.rstrip().endswith(')') or (kind == 'NominalPattern' and text.rstrip().endswith('}'))
            result = ['p_nominal' if kind == 'NominalPattern' else 'p_tag', branch[0], called, branch[1:]]
            if kind == 'NominalPattern':
                # A nominal record shorthand is `Type.{...}`; the general
                # backing pattern spelling `Type.({...})` retains its parens.
                result.append(text.rstrip().endswith('}'))
            return result
        if kind in {'TuplePattern', 'ListPattern', 'RecordPattern', 'AlternativePattern'}:
            name = {'TuplePattern': 'p_tuple', 'ListPattern': 'p_list',
                    'RecordPattern': 'p_record', 'AlternativePattern': 'p_alternative'}[kind]
            return [name, branch]
        if kind == 'PatternField':
            return ['p_field', branch[0][1], branch[1] if len(branch) > 1 else None]
        if kind == 'RestPattern':
            return ['p_rest', branch[0][1] if branch else None]
        if kind == 'AliasPattern':
            return ['p_alias', branch[0], branch[1][1]]
        if kind == 'If':
            return ['if', *branch]
        if kind == 'RecordField':
            ids = children[start:start+length]
            name_node = nodes[ids[0]]
            name = raw[name_node['start']:name_node['end']].decode()
            return [name, branch[-1] if len(branch) > 1 else ['identifier', name]]
        if kind == 'Record':
            return ['record', branch]
        if kind == 'RecordUpdate':
            return ['record_update', branch[0], branch[1:]]
        if kind == 'UnsetField':
            return [branch[0][1], ['unset']]
        if kind == 'RecordBuilder':
            return ['record_builder', branch[0], branch[1:]]
        if kind == 'NominalRecord':
            return ['nominal_record', *branch]
        if kind == 'NominalApply':
            return ['nominal_apply', branch[0], branch[1:]]
        if kind == 'Block':
            return ['block', branch]
        if kind in {'FieldAccess', 'OptionalFieldAccess'}:
            return ['optional_field' if kind == 'OptionalFieldAccess' else 'field', branch[0], raw[nodes[children[start+1]]['start']:nodes[children[start+1]]['end']].decode()]
        if kind in {'Debug', 'Crash', 'Return'}:
            return [{'Debug': 'dbg', 'Crash': 'crash', 'Return': 'return'}[kind], *branch]
        if kind in {'Break', 'Ellipsis'}:
            return [kind.lower()]
        if kind == 'MatchBranch':
            return ['branch', branch[0], branch[1] if len(branch) == 3 else None, branch[-1]]
        if kind == 'Match':
            return ['match', branch[0], branch[1:]]
        if kind == 'Call':
            return ['call', branch[0], branch[1:]]
        if kind in {'For', 'ForStream'}:
            return ['for' if kind == 'For' else 'for!', *branch]
        if kind in {'ArrowCall', 'MethodArrowCall'}:
            left, right = [nodes[index] for index in children[start:start+length]]
            lo = bisect.bisect_left(operator_starts, left['end'])
            hi = bisect.bisect_left(operator_starts, right['start'])
            operator = ''.join(raw[token['start']:token['end']].decode() for token in operators[lo:hi])
            return ['arrow', operator, *branch, 'method_call' if kind == 'MethodArrowCall' else 'ordinary']
        return ['unsupported', kind]
    try:
        return visit(response['root'])
    except RecursionError as error:
        raise WorkerError('syntax projection exceeds Python recursion limit') from error


def has_unknown_shape(value):
    if isinstance(value, list):
        return (bool(value) and value[0] == 'unsupported') or any(has_unknown_shape(child) for child in value)
    return False


def validate_spans(response, source, lossless=False):
    size = len(source.encode())
    cursor = 0
    for token in response.get('tokens', []):
        start, end = token['start'], token['end']
        if not 0 <= start <= end <= size:
            raise WorkerError(f'invalid token span: {token}')
        if lossless and start != cursor:
            raise WorkerError('candidate token stream is not lossless')
        if lossless and end == start and token['kind'] != 'EndOfFile':
            raise WorkerError('non-EOF token has no progress')
        cursor = end
    if lossless and cursor != size:
        raise WorkerError('candidate token stream omits source bytes')
    for item in response.get('diagnostics', []) + response.get('nodes', []):
        if not 0 <= item['start'] <= item['end'] <= size:
            raise WorkerError(f'invalid syntax/diagnostic span: {item}')
    nodes, children = response.get('nodes', []), response.get('children', [])
    for node in nodes:
        start, length = node['children_start'], node['children_len']
        if start < 0 or length < 0 or start + length > len(children):
            raise WorkerError('child range outside child table')
        for index in children[start:start+length]:
            if not isinstance(index, int) or not 0 <= index < len(nodes):
                raise WorkerError('child index outside node table')
            child = nodes[index]
            if child['start'] < node['start'] or child['end'] > node['end']:
                raise WorkerError('child span outside parent')
    if lossless:
        tokens = response.get('tokens', [])
        if not tokens or tokens[-1]['kind'] != 'EndOfFile' or tokens[-1]['start'] != size:
            raise WorkerError('missing terminal EOF')
    # Recursion also validates child indices and detects cycles.
    if response.get('nodes'):
        syntax_shape(response, source)


def compare(case, actual, expected):
    differences = []
    for label, response in [('actual', actual), ('expected', expected)]:
        try:
            validate_spans(response, case['source'], lossless=label == 'actual')
        except WorkerError as error:
            differences.append({'field': label + '_invariants', 'error': str(error)})
    contract = case.get('coverage', 'supported')
    if contract == 'unsupported':
        # Unsupported is an explicit coverage gap, never a validity answer.
        # A completed implementation makes this inventory entry stale.
        if actual.get('status') != 'Unsupported':
            differences.append({'field': 'unsupported_status', 'actual': actual.get('status'), 'expected': 'Unsupported'})
        return differences
    if contract == 'exploratory' and actual.get('status') in {'Unsupported', 'Limited'}:
        differences.append({'field': 'coverage_gap', 'status': actual['status']})
        return differences
    if contract == 'recovery':
        if actual.get('ok') or expected.get('ok'):
            differences.append({'field': 'recovery_acceptance', 'actual': actual.get('ok'), 'expected': False})
        if 'expect_consumed' in case:
            for label, response in [('actual', actual), ('expected', expected)]:
                if response.get('consumed') != case['expect_consumed']:
                    differences.append({'field': label + '_consumed', 'actual': response.get('consumed'), 'expected': case['expect_consumed']})
        # Recovery tree placements are implementation-specific. Each worker's
        # bounds/progress invariants still apply; rejection must agree.
        return differences
    for field in ('ok', 'consumed'):
        if actual.get(field) != expected.get(field):
            differences.append({'field': field, 'actual': actual.get(field), 'expected': expected.get(field)})
    if contract == 'supported' and (not actual.get('ok') or not expected.get('ok')):
        differences.append({'field': 'supported_acceptance', 'actual': actual.get('ok'), 'expected': expected.get('ok')})
    for field, projector in [('tokens', normalize_tokens), ('shape', syntax_shape)]:
        left, right = projector(actual, case['source']), projector(expected, case['source'])
        if field == 'shape' and (has_unknown_shape(left) or has_unknown_shape(right)):
            differences.append({'field': 'projection_gap', 'actual': left, 'expected': right})
        elif left != right:
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
        if case['mode'] not in {'file', 'expr', 'header', 'statement', 'repl'}:
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
            for contract_field in ('model_shape', 'expect_consumed'):
                if contract_field in case:
                    record[contract_field] = case[contract_field]
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
        # Build the model before printing source. Every recursive constructor can
        # contain every other constructor, without deriving expectations from
        # either implementation under test. Depth bounds total work per case.
        choice = rng.randrange(12 if depth else 3)
        if choice == 0:
            value = str(rng.randrange(100000))
            return value, ['number', value]
        if choice == 1:
            value = 'v' + str(rng.randrange(100))
            return value, ['identifier', value]
        if choice == 2:
            value = json.dumps(rng.choice(['', 'hello', 'two words', 'λ', 'line\\nend']), ensure_ascii=False)
            return value, ['string', value]
        left, lshape = expression(depth - 1)
        if choice == 3:
            return f'f({left})', ['call', ['identifier', 'f'], [lshape]]
        if choice == 4:
            return f'(|x| {left})', ['lambda', [['p_identifier', 'x']], lshape]
        if choice == 5:
            return f'({left}).value', ['field', lshape, 'value']
        right, rshape = expression(depth - 1)
        if choice in {6, 7}:
            op = '+' if choice == 6 else '*'
            return f'({left} {op} {right})', ['binary', op, lshape, rshape]
        if choice == 8:
            return f'[{left}, {right}]', ['list', [lshape, rshape]]
        if choice == 9:
            return f'({left}, {right})', ['tuple', [lshape, rshape]]
        if choice == 10:
            return f'(if True ({left}) else ({right}))', ['if', ['identifier', 'True'], lshape, rshape]
        return f'{{ first: {left}, second: {right} }}', ['record', [['first', lshape], ['second', rshape]]]
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
        for contract_field in ('model_shape', 'expect_consumed'):
            if contract_field in saved:
                cases[0][contract_field] = saved[contract_field]
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
              'candidate_status_counts': dict(Counter(item['actual'].get('status', 'worker_error') for item in results)),
              'difference_counts': dict(Counter(d['field'] for item in results for d in item['differences'])),
              'counts': {'total': len(results), 'coverage_gaps': sum(item['coverage'] == 'unsupported' for item in results), 'passed': sum(item['pass'] for item in results),
                         'failed': sum(not item['pass'] for item in results)}, 'results': results}
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report['counts']))
    return int(any(not item['pass'] for item in results))

if __name__ == '__main__':
    raise SystemExit(main())
