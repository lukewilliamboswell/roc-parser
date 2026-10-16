#!/usr/bin/env python3
"""Inventory every pinned snapshot and gate candidate acceptance against the oracle.

This lane establishes acceptance and source bounds, not AST equivalence. Oracle
rejection includes intentional errors and obsolete examples; filenames and saved
snapshot expectations never decide validity. Unhandled inputs remain failures.
"""
from __future__ import annotations
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import shlex
import sys

from import_roc_syntax_snapshots import import_snapshots, REFERENCE_COMMIT
from review_roc_syntax import Worker, WorkerError, validate_spans


def outcome(case, actual, expected):
    for name, response in [('candidate', actual), ('oracle', expected)]:
        if 'worker_error' in response:
            return 'unknown', name + ': ' + response['worker_error']
        if type(response.get('ok')) is not bool or type(response.get('consumed')) is not bool:
            return 'unknown', name + ': nonboolean acceptance or consumption'
        try:
            validate_spans(response, case['source'], lossless=name == 'candidate')
        except (WorkerError, KeyError, TypeError, IndexError) as error:
            return 'unknown', name + ': ' + str(error)
    if expected['ok']:
        if not expected['consumed']:
            return 'unknown', 'oracle accepted unconsumed source'
        if actual['ok'] and actual['consumed'] and actual.get('status') == 'Complete':
            return 'accepted', None
        return 'missing_grammar', None
    if actual['ok']:
        return 'false_acceptance', None
    return 'rejected', None


def run_corpus(document, probe_command, oracle_command, timeout=5):
    results = []
    workers = {}
    try:
        for case in document['cases']:
            record = {'id': case['id'], 'mode': case['mode'], 'upstream': case['upstream'],
                      'source_sha256': case['source_sha256'], 'source': case['source']}
            for name, command in [('candidate', probe_command), ('oracle', oracle_command)]:
                try:
                    if name not in workers:
                        workers[name] = Worker(command, timeout=timeout)
                    record[name] = workers[name].request({k: case[k] for k in ('id', 'mode', 'source')})
                except (WorkerError, OSError) as error:
                    record[name] = {'worker_error': str(error)}
                    if name in workers:
                        workers.pop(name).close()
            record['classification'], record['error'] = outcome(case, record['candidate'], record['oracle'])
            results.append(record)
    finally:
        for worker in workers.values():
            worker.close()
    unknown_imports = [item for item in document['provenance']['skipped'] if item['reason'] != 'documentation']
    counts = Counter({name: 0 for name in ('accepted', 'missing_grammar', 'rejected', 'false_acceptance', 'unknown')})
    counts.update(item['classification'] for item in results)
    counts['unknown_imports'] = len(unknown_imports)
    if not results:
        counts['unknown'] += 1
    return {'reference_commit': REFERENCE_COMMIT, 'contract': 'acceptance and source invariants; no structural equivalence claim',
            'counts': dict(counts), 'inventory': {'imported': len(results),
            'documentation': sum(item['reason'] == 'documentation' for item in document['provenance']['skipped']),
            'unhandled': len(unknown_imports)}, 'unknown_imports': unknown_imports, 'provenance': document['provenance'],
            'passed': not unknown_imports and not any(counts[k] for k in ('unknown', 'missing_grammar', 'false_acceptance')),
            'results': results}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc-source', required=True, type=Path)
    parser.add_argument('--probe-command', required=True)
    parser.add_argument('--oracle-command', required=True)
    parser.add_argument('--timeout', type=float, default=5)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    # Import here, rather than accepting a filtered or truncated saved inventory.
    document = import_snapshots(args.roc_source)
    commands = [shlex.split(args.probe_command), shlex.split(args.oracle_command)]
    report = run_corpus(document, *commands, timeout=args.timeout)
    report['workers'] = {name: {'command': command, 'sha256': hashlib.sha256(Path(command[0]).read_bytes()).hexdigest()}
                         for name, command in zip(('candidate', 'oracle'), commands)}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'passed': report['passed'], 'counts': report['counts'], 'output': str(args.output)}))
    return int(not report['passed'])


if __name__ == '__main__':
    sys.exit(main())
