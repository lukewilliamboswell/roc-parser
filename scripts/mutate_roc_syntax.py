#!/usr/bin/env python3
"""Bounded, replayable oracle differential mutation campaign (valid UTF-8 transport)."""
import argparse
from collections import Counter
import hashlib
import json
import itertools
from pathlib import Path
import random
import shlex
import time

import review_roc_syntax as review

FRAGMENTS = (' ', '\n', '# comment\n', '(', ')', '[', ']', '{', '}', ',', '.', ':', ':=', '::',
             '?', '??', '..', '...', '->', '=>', '|', '+', '-', '=', '"', '${', '0', '0xff',
             'name', 'Tag', '_', 'if ', 'else ', 'match ', 'import ', 'where ', 'λ', 'é',
             '\x00', '\ufeff', '\u202e')


def digest(source):
    return hashlib.sha256(source.encode()).hexdigest()


def replay_edits(original, edits):
    data = original.encode()
    for edit in edits:
        start, end = edit['start'], edit['end']
        if data[start:end].decode() != edit['removed']:
            raise ValueError('mutation provenance does not match original bytes')
        data = data[:start] + edit['replacement'].encode() + data[end:]
    return data.decode()


def mutate(seed_case, campaign_seed, index, max_bytes, max_edits=3):
    # Per-case RNG makes a saved index reproducible independently of earlier requests.
    rng = random.Random(f'{campaign_seed}:{index}:{digest(seed_case["source"])}')
    source = seed_case['source']
    edits = []
    for step in range(rng.randint(1, max_edits)):
        kind = ('insert', 'delete', 'replace')[(index + step) % 3]
        if not source:
            kind = 'insert'
        start = rng.randrange(len(source) + (kind == 'insert'))
        end = start if kind == 'insert' else min(len(source), start + rng.randint(1, 8))
        replacement = rng.choice(FRAGMENTS) if kind != 'delete' else ''
        # Respect the byte bound without truncating a Unicode scalar.
        remaining = max_bytes - len((source[:start] + source[end:]).encode())
        while len(replacement.encode()) > remaining:
            replacement = replacement[:-1]
        if source[start:end] == replacement:
            continue
        edit = {'kind': kind, 'start': len(source[:start].encode()), 'end': len(source[:end].encode()),
                'removed': source[start:end], 'replacement': replacement}
        source = source[:start] + replacement + source[end:]
        edits.append(edit)
    # A full-size seed can turn an insertion into a no-op; force a bounded deletion.
    if not edits:
        if not source:
            source = ' '
            edits = [{'kind': 'insert', 'start': 0, 'end': 0, 'removed': '', 'replacement': ' '}]
        else:
            edits = [{'kind': 'delete', 'start': 0, 'end': len(source[0].encode()), 'removed': source[0], 'replacement': ''}]
            source = source[1:]
    return {'request': {'id': f'mutation/{campaign_seed}/{index}', 'mode': seed_case['mode'], 'source': source},
            'mutation': {'seed': campaign_seed, 'index': index, 'original': seed_case,
                         'original_sha256': digest(seed_case['source']), 'source_sha256': digest(source),
                         'max_bytes': max_bytes, 'max_edits': max_edits, 'edits': edits}}


def compare(request, actual, expected):
    source = request['source']
    differences = []
    for side, response in [('candidate', actual), ('oracle', expected)]:
        try:
            review.validate_spans(response, source, lossless=side == 'candidate')
            if type(response.get('ok')) is not bool or type(response.get('consumed')) is not bool:
                raise review.WorkerError('missing boolean acceptance/consumption')
            if response['ok'] and not response['consumed']:
                raise review.WorkerError('accepted prefix without consuming input')
        except (review.WorkerError, KeyError, TypeError, IndexError, ValueError) as error:
            differences.append({'field': 'invariant', 'side': side, 'error': str(error)})
    if differences:
        return differences
    if actual['ok'] != expected['ok']:
        return [{'field': 'acceptance', 'candidate': actual['ok'], 'oracle': expected['ok']}]
    if not expected['ok']:
        return []  # Recovery trees need not match; both must satisfy all invariants above.
    for field, project in [('tokens', review.normalize_tokens), ('shape', review.syntax_shape)]:
        left, right = project(actual, source), project(expected, source)
        if field == 'shape' and (review.has_unknown_shape(left) or review.has_unknown_shape(right)):
            differences.append({'field': 'projection_gap', 'candidate': left, 'oracle': right})
        elif left != right:
            differences.append({'field': field, 'candidate': left, 'oracle': right})
    return differences


class Campaign:
    def __init__(self, probe, oracle, timeout):
        self.commands = {'candidate': probe, 'oracle': oracle}
        self.timeout = timeout
        self.workers = {}

    def request(self, side, request):
        try:
            if side not in self.workers:
                self.workers[side] = review.Worker(self.commands[side], timeout=self.timeout)
            return self.workers[side].request(request)
        except (review.WorkerError, OSError):
            worker = self.workers.pop(side, None)
            if worker:
                worker.close()
            raise

    def close(self):
        for worker in self.workers.values():
            worker.close()

    def evaluate(self, case):
        record = dict(case)
        errors = []
        for side in self.commands:
            try:
                record[side] = self.request(side, case['request'])
            except (review.WorkerError, OSError) as error:
                errors.append({'field': 'worker', 'side': side, 'error': str(error)})
        if not errors:
            try:
                errors = compare(case['request'], record['candidate'], record['oracle'])
            except (review.WorkerError, KeyError, TypeError, IndexError, ValueError, RecursionError) as error:
                errors = [{'field': 'projection', 'error': str(error)}]
        record.update(differences=errors, passed=not errors)
        return record


def positive(value):
    value = int(value)
    if value <= 0:
        raise argparse.ArgumentTypeError('must be positive')
    return value


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe-command', required=True)
    parser.add_argument('--oracle-command', required=True)
    parser.add_argument('--fixtures', type=Path, action='append', help='repeat for curated fixtures and imported corpus')
    parser.add_argument('--corpus', type=Path, help='raw-fuzz corpus directory: compare unchanged UTF-8 entries, including rejected inputs')
    parser.add_argument('--max-corpus-files', type=positive, default=10000)
    parser.add_argument('--runs', type=positive, default=1000)
    parser.add_argument('--seed', type=int, default=1)
    parser.add_argument('--max-source-bytes', type=positive, default=16384)
    parser.add_argument('--max-edits', type=positive, default=3)
    parser.add_argument('--timeout', type=float, default=5)
    parser.add_argument('--replay', type=Path, help='saved failure JSON; requires no original corpus')
    parser.add_argument('--output', type=Path, default=review.ROOT / '.roc-parser-tmp/syntax-mutations')
    args = parser.parse_args(argv)
    if not 0 < args.timeout < float('inf'):
        parser.error('--timeout must be finite and positive')
    if args.corpus and not args.corpus.is_dir():
        parser.error('--corpus must be an existing directory')
    args.output.mkdir(parents=True, exist_ok=True)
    failures = args.output / 'failures'
    failures.mkdir(exist_ok=True)
    commands = {'candidate': shlex.split(args.probe_command), 'oracle': shlex.split(args.oracle_command)}
    identities = {side: [{'path': item, 'sha256': hashlib.sha256(Path(item).read_bytes()).hexdigest()}
                         for item in command if Path(item).is_file()] for side, command in commands.items()}
    campaign = Campaign(commands['candidate'], commands['oracle'], args.timeout)
    counts, differences, modes, operations = Counter(), Counter(), Counter(), Counter()
    seed_records = []
    failure_files = []
    start = time.monotonic()
    try:
        if args.replay:
            saved = json.loads(args.replay.read_text())
            cases = [{'request': saved['request'], 'mutation': saved['mutation']}]
            mutation = cases[0]['mutation']
            if replay_edits(mutation['original']['source'], mutation['edits']) != cases[0]['request']['source']:
                raise ValueError('saved request differs from mutation provenance')
        else:
            seeds = []
            for path in args.fixtures or [review.DATA / 'fixtures.json']:
                _, inputs = review.load_cases(path)
                for case in inputs:
                    if len(case['source'].encode()) > args.max_source_bytes:
                        counts['oversize_seeds'] += 1
                        continue
                    original = {key: case[key] for key in ('id', 'mode', 'source')}
                    original['fixture'] = str(path.resolve())
                    request = {'id': f'seed/{len(seed_records)}', 'mode': case['mode'], 'source': case['source']}
                    try:
                        response = campaign.request('oracle', request)
                    except (review.WorkerError, OSError) as error:
                        (failures / 'seed-worker.json').write_text(json.dumps({'request': request, 'original': original, 'error': str(error)}, indent=2))
                        raise
                    review.validate_spans(response, case['source'])
                    if type(response.get('ok')) is not bool:
                        raise review.WorkerError('seed response lacks boolean ok')
                    seed_records.append({'original': original, 'accepted': response['ok']})
                    if response['ok']:
                        if not response.get('consumed') or review.has_unknown_shape(review.syntax_shape(response, case['source'])):
                            raise review.WorkerError(f'accepted seed has incomplete projection: {case["id"]}')
                        seeds.append(original)
                    else:
                        counts['rejected_seeds'] += 1
            seeds.sort(key=lambda case: (case['mode'], case['id'], digest(case['source'])))
            if not seeds:
                raise ValueError('no oracle-accepted bounded seed inputs')
            counts['accepted_seeds'] = len(seeds)
            rng = random.Random(args.seed)
            generated = (mutate(rng.choice(seeds), args.seed, index, args.max_source_bytes, args.max_edits) for index in range(args.runs))
            corpus_cases = []
            if args.corpus:
                for index, path in enumerate(sorted(path for path in args.corpus.rglob('*') if path.is_file())):
                    if index >= args.max_corpus_files:
                        counts['corpus_file_limit_excluded'] += 1
                        continue
                    if path.stat().st_size > args.max_source_bytes:
                        counts['corpus_oversize_excluded'] += 1
                        continue
                    try:
                        source = path.read_bytes().decode('utf-8', errors='strict')
                    except UnicodeDecodeError:
                        counts['corpus_invalid_utf8_excluded'] += 1
                        continue
                    original = {'id': str(path.relative_to(args.corpus)), 'mode': 'file', 'source': source, 'fixture': str(path.resolve())}
                    corpus_cases.append({'request': {'id': f'corpus/{index}', 'mode': 'file', 'source': source},
                                         'mutation': {'kind': 'unchanged_corpus', 'seed': args.seed, 'index': index, 'original': original,
                                                      'original_sha256': digest(source), 'source_sha256': digest(source), 'edits': []}})
                counts['corpus_compared'] = len(corpus_cases)
            cases = itertools.chain(corpus_cases, generated)
        (args.output / 'seeds.json').write_text(json.dumps(seed_records, ensure_ascii=True, indent=2) + '\n')
        with (args.output / 'cases.jsonl').open('w') as requests:
            for case in cases:
                requests.write(json.dumps(case, ensure_ascii=True) + '\n')
                requests.flush()
                record = campaign.evaluate(case)
                counts['total'] += 1
                counts['passed' if record['passed'] else 'failed'] += 1
                if 'oracle' in record:
                    oracle_ok = record['oracle'].get('ok')
                    counts['oracle_accepted' if oracle_ok is True else 'oracle_rejected' if oracle_ok is False else 'oracle_invalid_response'] += 1
                modes[case['request']['mode']] += 1
                operations.update(edit['kind'] for edit in case['mutation']['edits'])
                differences.update(error['field'] for error in record['differences'])
                if not record['passed']:
                    failure = failures / f'{digest(case["request"]["id"])}.json'
                    failure.write_text(json.dumps(record, ensure_ascii=True, indent=2) + '\n')
                    failure_files.append(str(failure))
    finally:
        campaign.close()
    report = {'version': 1, 'reference_commit': review.REFERENCE_COMMIT, 'seed': args.seed,
              'bounds': {'runs': args.runs, 'max_source_bytes': args.max_source_bytes, 'max_edits': args.max_edits, 'max_corpus_files': args.max_corpus_files, 'timeout': args.timeout},
              'commands': commands, 'worker_identities': identities, 'counts': dict(counts), 'differences': dict(differences),
              'failure_files': failure_files, 'modes': dict(modes), 'operations': dict(operations), 'elapsed_seconds': time.monotonic() - start}
    (args.output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))
    return bool(counts['failed'])


if __name__ == '__main__':
    raise SystemExit(main())
