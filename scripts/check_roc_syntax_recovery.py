#!/usr/bin/env python3
"""Check editor recovery contracts independently of compiler recovery placement."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import shlex

try:
    from . import review_roc_syntax as review
except ImportError:
    import review_roc_syntax as review


def cases():
    # Each edit has an explicit damaged region. Complete declarations on both
    # sides must retain their source spans and independently specified shapes.
    edits = [
        ('unexpected-close', 'bad = )'),
        ('mismatched-list', 'bad = [1, )]'),
        ('mismatched-record', 'bad = { field: ) }'),
        ('missing-operand', 'bad = 1 +'),
        ('unclosed-string', 'bad = "oops'),
        ('unclosed-list', 'bad = [1,'),
        ('missing-else-body', 'bad = if True 1 else )'),
        ('closed-missing-operand', 'bad = (1 + )'),
        ('extra-comma', 'bad = [1,,2]'),
    ]
    result = []
    survivors = [('before = 0', ['decl', ['p_identifier', 'before'], ['number', '0']]),
                 ('after = 42', ['decl', ['p_identifier', 'after'], ['number', '42']]),
                 ('last = after + 1', ['decl', ['p_identifier', 'last'],
                                       ['binary', '+', ['identifier', 'after'], ['number', '1']]])]
    for name, damaged in edits:
        for prelude in ('', '## λ editor buffer\nmodule []\n'):
            prefix = prelude + survivors[0][0] + '\n'
            source = prefix + damaged + '\n' + '\n'.join(s[0] for s in survivors[1:]) + '\n'
            expected = []
            raw = source.encode()
            for text, shape in survivors:
                start = raw.index(text.encode())
                expected.append({'start': start, 'end': start + len(text.encode()), 'shape': shape})
            result.append({'id': name + ('-unicode-header' if prelude else ''),
                           'mode': 'file', 'source': source,
                           'damaged': [len(prefix.encode()), len((prefix + damaged).encode())],
                           'survivors': expected})
    return result


def check(case, response):
    review.validate_spans(response, case['source'], lossless=True)
    differences = []
    if response.get('ok') or not response.get('diagnostics'):
        differences.append({'field': 'malformed_rejection'})
    start, end = case['damaged']
    for diagnostic in response.get('diagnostics', []):
        if diagnostic['start'] < start or diagnostic['end'] > end:
            differences.append({'field': 'diagnostic_locality', 'diagnostic': diagnostic,
                                'allowed': [start, end]})
    root = response['nodes'][response['root']]
    indices = response['children'][root['children_start']:root['children_start'] + root['children_len']]
    for survivor in case['survivors']:
        matches = [index for index in indices if all(response['nodes'][index][key] == survivor[key]
                                                    for key in ('start', 'end'))]
        if len(matches) != 1:
            differences.append({'field': 'surviving_declaration', 'expected': survivor})
        else:
            shape = review.syntax_shape({**response, 'root': matches[0]}, case['source'])
            if shape != survivor['shape']:
                differences.append({'field': 'surviving_shape', 'actual': shape, 'expected': survivor})
    return differences


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe-command', required=True)
    parser.add_argument('--oracle-command', required=True)
    parser.add_argument('--timeout', type=float, default=5)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    workers = []
    records = []
    try:
        probe = review.Worker(shlex.split(args.probe_command), timeout=args.timeout)
        workers.append(probe)
        oracle = review.Worker(shlex.split(args.oracle_command), timeout=args.timeout)
        workers.append(oracle)
        for case in cases():
            record = {'case': case, 'source_sha256': hashlib.sha256(case['source'].encode()).hexdigest()}
            try:
                request = {key: case[key] for key in ('id', 'mode', 'source')}
                actual, expected = probe.request(request), oracle.request(request)
                record.update(actual=actual, oracle=expected)
                differences = check(case, actual)
                if expected.get('ok'):
                    differences.append({'field': 'fixture_valid_in_reference'})
                record['differences'] = differences
            except (review.WorkerError, OSError, KeyError, TypeError, IndexError) as error:
                record['differences'] = [{'field': 'worker_or_protocol', 'error': str(error)}]
            records.append(record)
    finally:
        for worker in workers:
            worker.close()
    report = {'reference_commit': review.REFERENCE_COMMIT, 'cases': len(records),
              'failed': sum(bool(r['differences']) for r in records), 'results': records}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({key: report[key] for key in ('cases', 'failed')}))
    return int(report['failed'] != 0)


if __name__ == '__main__':
    raise SystemExit(main())
