#!/usr/bin/env python3
"""Local AsciiDoc parse/render ratchet against pinned Asciidoctor (default 1.0x).

Persistent workers time only parsing or parsing plus conversion. Correctness,
IPC, source maps, warmup and JSON serialization stay outside measured windows.
Noisy duration-qualified batches double their common iteration count within
a four-attempt budget; persistent noise remains an explicit failure.
--smoke checks two tiny protocol/correctness cases and never asserts timings.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import random
import shlex
import shutil
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
sys.setrecursionlimit(10000)
sys.path.insert(0, str(ROOT / 'scripts'))
from review_roc_syntax import Worker
from review_asciidoc import DEFAULT_HOME, canonical_projection, canonical_html


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def enforce_deadline(deadline):
    if time.monotonic() >= deadline:
        raise TimeoutError('full-run deadline exceeded; campaign not qualified')


class DeadlineWorker:
    """Bound every protocol exchange by the remaining campaign budget."""
    def __init__(self, command, timeout, deadline, env=None):
        self.worker = Worker(command, timeout=timeout, env=env)
        self.timeout = timeout
        self.deadline = deadline

    def request(self, value):
        enforce_deadline(self.deadline)
        self.worker.timeout = min(self.timeout, self.deadline - time.monotonic())
        try:
            result = self.worker.request(value)
        except Exception:
            enforce_deadline(self.deadline)
            raise
        enforce_deadline(self.deadline)
        return result

    def close(self):
        self.worker.close()


def oracle_environment(gem_home):
    env = {key: value for key, value in os.environ.items()
           if key not in ('RUBYOPT', 'RUBYLIB', 'GEM_HOME', 'GEM_PATH', 'BUNDLE_GEMFILE')}
    env.update(TZ='UTC', LANG='C.UTF-8', LC_ALL='C.UTF-8', SOURCE_DATE_EPOCH='946684800')
    if gem_home:
        env.update(GEM_HOME=str(gem_home.resolve()), GEM_PATH=str(gem_home.resolve()))
    return env


def retained_run(report):
    return {key: report[key] for key in (
        'command', 'machine', 'seed', 'driver_sha256', 'worker_transport_sha256',
        'candidate_build', 'reference', 'measurement_policy', 'batch_policy', 'progress', 'max_ratio', 'deadline_seconds') if key in report}


def source_identity(root):
    paths = sorted((root / 'package').glob('*.roc')) + [root / 'scripts/asciidoc/performance.roc', root / 'scripts/asciidoc/performance_oracle.rb', root / 'scripts/asciidoc/oracle.rb', root / 'scripts/review_asciidoc.py']
    h = hashlib.sha256()
    for path in paths:
        h.update(path.relative_to(root).as_posix().encode() + b'\0' + path.read_bytes() + b'\0')
    return h.hexdigest()


def generated(seed, count):
    """Independent printable document models, plus reproducible raw mutations."""
    rng = random.Random(seed)
    words = ['amber', 'café', '東京', 'quiet river', 'Delta', 'Roc']
    for index in range(count):
        blocks = []
        for _ in range(rng.randint(1, 12)):
            word = rng.choice(words) + str(rng.randrange(10000))
            kind = rng.randrange(10)
            blocks.append([
                word, '* ' + word + '\n* sibling', '----\n' + word + '\n----',
                '....\n' + word + '\n....', 'NOTE: ' + word, '== ' + word,
                ':value: ' + word + '\n\n{value}', 'A *bold* _word_ ' + word,
                '|===\n|one |two\n|' + word + ' |four\n|===',
                'ifdef::backend-html5[' + word + ']'][kind])
        model_source = '\n\n'.join(blocks)
        yield {'id': f'model-{seed}-{index}', 'family': 'generated-model', 'source': model_source,
               'provenance': {'seed': seed, 'index': index, 'model_blocks': len(blocks)}}
        # Mutations are separate, retained inputs, not assumed valid.
        if index % 4 == 0:
            position = rng.randrange(len(model_source) + 1)
            insert = rng.choice(['\\', '[', ']', '{missing}', '\n\n', '----\n', '*'])
            yield {'id': f'mutation-{seed}-{index}', 'family': 'raw-mutation',
                   'source': model_source[:position] + insert + model_source[position:],
                   'provenance': {'seed': seed, 'parent': f'model-{seed}-{index}', 'position': position, 'insert': insert}}


def conditional_document(size, *, values):
    definitions = '\n'.join(f':key{i}: value{i}' for i in range(size))
    name = f'key{size - 1}'
    directive = (f'ifeval::["{{{name}}}" == "value{size - 1}"]'
                 if values else f'ifdef::{name}[]')
    return definitions + '\n\n' + f'{directive}\nword\nendif::[]\n' * size


def named_footnotes(size, *, blocks):
    separator = '\n\n' if blocks else ' '
    definitions = separator.join(f'footnote:note{i}[Note {i}.]' for i in range(size))
    references = ' '.join(f'footnote:note{i}[]' for i in range(size))
    return definitions + separator + references


def counter_document(size, *, blocks, formatted=False, underscored=False):
    separator = '\n\n' if blocks else ' '
    names = [f'key_{i}' if underscored else f'key{i}' for i in range(size)]
    prefix = '*bold* ' if formatted else ''
    counters = separator.join(prefix + f'{{counter:{name}}}' for name in names)
    references = ' '.join(f'{{{name}}}' for name in names)
    return counters + '\n\n' + references


def attribute_aliases(size):
    definitions = ':key0: value\n' + '\n'.join(f':key{i}: {{key{i - 1}}}' for i in range(1, size))
    return definitions + f'\n\n{{key{size - 1}}}'


def substitution_counter_blocks(size, *, passthrough):
    blocks = [f'pass:a[{{counter:key{i}}}]' if passthrough else
              f'[subs="attributes"]\n{{counter:key{i}}}' for i in range(size)]
    return '\n\n'.join(blocks) + f'\n\n{{key0}} {{key{size - 1}}}'


def explicit_macro_footnote_blocks(size):
    blocks = [f'[subs="macros"]\nfootnote:note{i}[Note {i}.]' for i in range(size)]
    return '\n\n'.join(blocks) + f'\n\n[subs="macros"]\nfootnote:note0[] footnote:note{size - 1}[]'


def counter_macro_blocks(size, macro):
    blocks = [f'{{counter:key{i}}} {macro}' for i in range(size)]
    return '\n\n'.join(blocks) + f'\n\n{{key0}} {{key{size - 1}}}'


def set_passthrough_blocks(size):
    blocks = [f'{{set:key{i}:value}} pass:a[{{key{i}}}]' for i in range(size)]
    return '\n\n'.join(blocks) + f'\n\n{{key0}} {{key{size - 1}}}'


def mutating_preview_counter_blocks(size):
    blocks = [f'pass:a[{{counter:key{i}}}] {{set:last:value}}' for i in range(size)]
    return '\n\n'.join(blocks) + f'\n\n{{key0}} {{key{size - 1}}} {{last}}'


def static_attribute_url_blocks(size):
    definitions = '\n'.join(f':key{i}: value' for i in range(size))
    return definitions + '\n\n' + '\n\n'.join('https://example.test' for _ in range(size)) + f'\n\n{{key0}} {{key{size - 1}}}'


def static_attribute_nonparagraph(size, context):
    definitions = '\n'.join(f':k{i}: v' for i in range(size))
    if context == 'list':
        body = '* https://a.b\n' * size
    elif context == 'listing':
        body = '----\nx < y & z\n----\n\n' * size
    elif context == 'image':
        body = 'image::cat.png[Cat]\n\n' * size
    elif context == 'varied-image':
        body = ''.join(f'image::cat{i}.png[Cat {i}]\n\n' for i in range(size))
    elif context == 'asciidoc-table':
        body = '[cols="a",options=noheader]\n|===\n' + '|https://a.b\n\n' * size + '|===\n'
    else:
        body = ''.join(f'== *S{i}*\n\n' for i in range(size))
    return definitions + '\n\n' + body


def static_base_revisions(size, *, varied=False, urls=False):
    definitions = '\n'.join(f':k{i}: initial' for i in range(size))
    if varied:
        body = ''.join(f':k{i}!:\n:k{i}: v{i}\n\n{{k{i}}}\n\n' for i in range(size))
    else:
        paragraph = 'https://a.b' if urls else 'Value {k0}.'
        body = ''.join(f':k0: value{i}\n\n{paragraph}\n\n' for i in range(size))
    return definitions + '\n\n' + body


def static_many_tables(size):
    definitions = '\n'.join(f':k{i}: initial' for i in range(size))
    return definitions + '\n\n' + '[cols="1",options=noheader]\n|===\n|x\n|===\n\n' * size


def geometric(sizes):
    families = {
        'long-paragraph': lambda n: 'alpha café ' * n,
        'long-prefix-strong': lambda n: 'alpha café ' * n + '*strong*',
        'long-prefix-link': lambda n: 'alpha café ' * n + 'link:https://example.test[label]',
        'long-prefix-url': lambda n: 'ordinary ASCII ' * n + 'https://example.test',
        'many-paragraphs': lambda n: '\n\n'.join('paragraph ' + str(i) for i in range(n)),
        'many-sections': lambda n: '\n\n'.join('== Section ' + str(i) + '\n\nBody.' for i in range(n)),
        'many-footnotes': lambda n: ' '.join('word footnote:[note ' + str(i) + ']' for i in range(n)),
        'named-footnotes': lambda n: named_footnotes(n, blocks=False),
        'named-footnote-blocks': lambda n: named_footnotes(n, blocks=True),
        'distinct-counters': lambda n: counter_document(n, blocks=False),
        'counter-blocks': lambda n: counter_document(n, blocks=True),
        'formatted-counter-blocks': lambda n: counter_document(n, blocks=True, formatted=True),
        'underscored-counter-blocks': lambda n: counter_document(n, blocks=True, underscored=True),
        'explicit-attribute-counter-blocks': lambda n: substitution_counter_blocks(n, passthrough=False),
        'passthrough-counter-blocks': lambda n: substitution_counter_blocks(n, passthrough=True),
        'explicit-macro-footnote-blocks': explicit_macro_footnote_blocks,
        'immediate-quote-pass-counter-blocks': lambda n: counter_macro_blocks(n, 'pass:q[*word*]'),
        'immediate-math-counter-blocks': lambda n: counter_macro_blocks(n, 'stem:[x]'),
        'set-passthrough-blocks': set_passthrough_blocks,
        'url-counter-blocks': lambda n: counter_macro_blocks(n, 'https://example.test'),
        'mutating-preview-counter-blocks': mutating_preview_counter_blocks,
        'static-attributes-url-blocks': static_attribute_url_blocks,
        'static-attributes-url-list-items': lambda n: static_attribute_nonparagraph(n, 'list'),
        'static-attributes-listing-blocks': lambda n: static_attribute_nonparagraph(n, 'listing'),
        'static-attributes-formatted-sections': lambda n: static_attribute_nonparagraph(n, 'section'),
        'static-attributes-asciidoc-table-cells': lambda n: static_attribute_nonparagraph(n, 'asciidoc-table'),
        'static-attributes-image-blocks': lambda n: static_attribute_nonparagraph(n, 'image'),
        'static-attributes-varied-image-blocks': lambda n: static_attribute_nonparagraph(n, 'varied-image'),
        'static-base-attribute-revisions-url': lambda n: static_base_revisions(n, urls=True),
        'static-base-attribute-revisions-text': static_base_revisions,
        'static-base-attribute-revisions-varied-unset-readd': lambda n: static_base_revisions(n, varied=True),
        'static-attributes-many-tables': static_many_tables,
        'mailto-positional-query-counter-blocks': lambda n: counter_macro_blocks(n, 'mailto:x@y.z[N,S,B]'),
        'attribute-aliases': attribute_aliases,
        'many-crossrefs': lambda n: '\n\n'.join('[[target' + str(i) + ']]\n== Section ' + str(i) + '\n\nSee <<target' + str(i) + '>>.' for i in range(n)),
        'attribute-redefinitions': lambda n: '\n\n'.join(':name: value' + str(i) + '\n\n{name}' for i in range(n)),
        'plain-punctuation-adversary': lambda n: '( . - = ' * n,
        'many-list-items': lambda n: '\n'.join('* item ' + str(i) for i in range(n)),
        'nested-lists': lambda n: '\n'.join('*' * (i + 1) + ' item' for i in range(n)),
        'table-cells': lambda n: '|===\n' + '\n'.join('|one |two' for _ in range(n)) + '\n|===',
        'table-footnotes': lambda n: '[cols="a"]\n|===\n' + '\n'.join(f'|footnote:[Note {i}.]' for i in range(n)) + '\n|===',
        'many-delimiters': lambda n: '\n\n'.join('----\nline\n----' for _ in range(n)),
        'attribute-definitions': lambda n: '\n'.join(':key' + str(i) + ': value' for i in range(n)) + '\n\n{key0}',
        'conditional-presence': lambda n: conditional_document(n, values=False),
        'conditional-value': lambda n: conditional_document(n, values=True),
        'formatting-adversary': lambda n: ('*a _b `c ' * n) + 'tail',
        'unclosed-delimiter': lambda n: '----\n' + 'x\n' * n,
        'nested-containers': lambda n: '\n'.join('=' * (4 + i) for i in range(n)) + '\ntext\n' + '\n'.join('=' * (4 + i) for i in reversed(range(n))),
    }
    priority = ['many-sections','many-footnotes','many-crossrefs','attribute-redefinitions','plain-punctuation-adversary',
                'conditional-presence','conditional-value','distinct-counters','counter-blocks',
                'formatted-counter-blocks','underscored-counter-blocks',
                'named-footnotes','named-footnote-blocks','attribute-aliases','table-footnotes',
                'explicit-attribute-counter-blocks','passthrough-counter-blocks','explicit-macro-footnote-blocks',
                'immediate-quote-pass-counter-blocks','immediate-math-counter-blocks',
                'set-passthrough-blocks','url-counter-blocks',
                'mutating-preview-counter-blocks','static-attributes-url-blocks',
                'static-attributes-url-list-items','static-attributes-listing-blocks',
                'static-attributes-formatted-sections','static-attributes-asciidoc-table-cells',
                'static-attributes-image-blocks','static-attributes-varied-image-blocks',
                'static-base-attribute-revisions-url','static-base-attribute-revisions-text',
                'static-base-attribute-revisions-varied-unset-readd','static-attributes-many-tables',
                'mailto-positional-query-counter-blocks']
    for family in [*priority,*(name for name in families if name not in priority)]:
        make = families[family]
        # Depth uses a separate legal geometric range. Size ratchets must time
        # accepted documents; the parser's explicit 64-level rejection contract
        # is exercised by package tests, rather than mistaken for oracle drift.
        family_sizes = [2,4,8,16,32] if family in ('nested-lists','nested-containers') else sizes
        if family in ('many-footnotes','named-footnotes','named-footnote-blocks',
                      'distinct-counters','counter-blocks','formatted-counter-blocks',
                      'underscored-counter-blocks','attribute-aliases','table-footnotes',
                      'explicit-attribute-counter-blocks','passthrough-counter-blocks',
                      'explicit-macro-footnote-blocks','immediate-quote-pass-counter-blocks',
                      'immediate-math-counter-blocks','set-passthrough-blocks',
                      'url-counter-blocks','mutating-preview-counter-blocks',
                      'static-attributes-url-blocks','static-attributes-url-list-items',
                      'static-attributes-listing-blocks','static-attributes-formatted-sections',
                      'static-attributes-asciidoc-table-cells','static-attributes-image-blocks',
                      'static-attributes-varied-image-blocks',
                      'mailto-positional-query-counter-blocks') and max(sizes) >= 1024:
            family_sizes = sorted(set(sizes) | {2048,4096})
        if family in ('static-base-attribute-revisions-url','static-base-attribute-revisions-text',
                      'static-base-attribute-revisions-varied-unset-readd','static-attributes-many-tables'):
            # Larger runs wait for qualification of the persistent snapshot fix.
            family_sizes = [size for size in sizes if size in (128,256,512)]
        if family == 'table-footnotes' and max(sizes) >= 1024:
            # The footer's repeated prior-note scans become clear at 8,192
            # cells, still within the default 256 KiB source bound.
            family_sizes = sorted(set(family_sizes) | {8192})
        for size in family_sizes:
            yield {'id': f'scaling-{family}-{size}', 'family': family, 'size': size, 'source': make(size),
                   'provenance': {'generator': family, 'parameter': size, 'geometric': True}}


def corpus(args):
    if args.replay:
        for path in args.replay:
            value = json.loads(path.read_text())
            if isinstance(value,dict) and isinstance(value.get('cases'),list):
                yield from value['cases']
            else:
                yield value.get('case', value)
        return
    if args.smoke:
        yield {'id': 'smoke-paragraph', 'family': 'smoke', 'source': 'Hello *Roc*.'}
        yield {'id': 'smoke-include-selectors', 'family': 'smoke',
               'source': '----\ninclude::part.adoc[lines=2..4]\ninclude::part.adoc[tag=chosen]\n----',
               'includes': {'part.adoc': 'omitted\n// tag::chosen[]\n<code>\n// end::chosen[]\nomitted'}}
        return
    yield from geometric(args.sizes)
    yield from generated(args.seed, args.generated)
    fixture_paths = args.fixtures
    if fixture_paths is None:
        directory = args.root / 'scripts/asciidoc'
        fixture_paths = ([directory/'fixtures.json'] if (directory/'fixtures.json').exists() else []) + sorted(directory.glob('*-fixtures.json'))
    for path in fixture_paths:
        for case in json.loads(path.read_text())['cases']:
            yield {**case, 'family': 'fixture', 'provenance': {'fixture': str(path), 'fixture_sha256': digest(path)}}
    for path in args.corpus:
        for source in sorted(path.rglob('*.adoc')):
            if source.is_file() and not source.is_symlink() and not any(p.startswith('.') for p in source.relative_to(path).parts):
                yield {'id': 'corpus-' + digest(source), 'family': 'corpus', 'source': source.read_text(),
                       'provenance': {'path': str(source), 'sha256': digest(source)}}


def request(case, mode, iterations, warmup=0):
    return {'id': case['id'], 'source': case['source'], 'includes': [{'path': path, 'source': source} for path, source in sorted(case.get('includes', {}).items())],
            'expand': 'includes' in case, 'mode': mode, 'iterations': iterations, 'warmup': warmup}



def case_identity(case):
    """Include virtual sources and expansion policy in retained-input keys."""
    value = request(case, 'parse', 0)
    payload = {key: value[key] for key in ('source', 'includes', 'expand')}
    return hashlib.sha256(json.dumps(payload, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()


def validate_reply(value, case, iterations):
    if value.get('error'):
        raise ValueError('worker error: ' + json.dumps(value))
    if value.get('version') != 1 or value.get('id') != case['id'] or value.get('iterations') != iterations or value.get('successes') != iterations:
        raise ValueError('worker protocol/count mismatch')
    if not isinstance(value.get('elapsed_ns'), int) or value['elapsed_ns'] < 0:
        raise ValueError('invalid elapsed time')
    if not isinstance(value.get('checksum'), int) or value['checksum'] < iterations:
        raise ValueError('operation result was not consumed')
    if not isinstance(value.get('projection'), dict) or not isinstance(value.get('html'), str):
        raise ValueError('correctness result missing')
    return value


def equivalent(candidate, reference):
    if canonical_projection(candidate['projection']) != canonical_projection(reference['projection']):
        raise ValueError('candidate/reference block projections differ')
    if candidate['html'] == reference['html']:
        return
    if canonical_html(candidate['html']) != canonical_html(reference['html']):
        raise ValueError('candidate/reference rendered DOM differs')


def calibrate(worker, case, mode, window_ns, max_iterations, expected, *, progress=None, engine=None):
    iterations = 1
    for _ in range(24):
        if progress: progress(f'calibration engine={engine} iterations={iterations} warmup=2')
        value = validate_reply(worker.request(request(case, mode, iterations, 2)), case, iterations)
        equivalent(value, expected)
        elapsed = value['elapsed_ns']
        if elapsed >= window_ns:
            return iterations
        if iterations == max_iterations:
            raise ValueError('timing noise: minimum measurement window unavailable at iteration cap')
        iterations = min(max_iterations, max(iterations + 1, int(iterations * window_ns / max(elapsed, 1) * 1.1)))
    raise ValueError('calibration iteration limit')


def calibrate_common(workers, case, mode, window_ns, max_iterations, expected, *, progress=None):
    """Calibrate shared work from the slower pilot, avoiding huge oracle batches."""
    elapsed = []
    for index,worker in enumerate(workers):
        if progress: progress(f'calibration engine={"candidate" if index==0 else "reference"} iterations=1 warmup=2')
        value = validate_reply(worker.request(request(case,mode,1,2)),case,1)
        equivalent(value,expected)
        elapsed.append(value['elapsed_ns'])
    return min(max_iterations,max(1,math.ceil(window_ns / max(max(elapsed),1) * 1.15)))


class TimingQualificationError(ValueError):
    """A failed timing qualification with its completed raw sample attempts."""

    def __init__(self, message, measurement_attempts):
        super().__init__(message)
        self.measurement_attempts = measurement_attempts


def sample_pair(workers, case, mode, iterations, window_ns, samples, max_iterations, expected,
                *, screen=False, clock_floor_ns=100000, max_ratio=1.0, progress=None):
    """Use short common batches only with stable, conservatively clear headroom.

    Near the threshold, noisy data, replay inputs and geometric families require
    the full duration floor for both engines. Noisy qualified batches double
    shared iterations within the four-attempt budget before failing. All pilot
    samples remain evidence.
    """
    attempts = []
    for attempt in range(4):
        for index,worker in enumerate(workers):
            if progress: progress(f'attempt={attempt+1} preheat engine={"candidate" if index==0 else "reference"} iterations={iterations}')
            validate_reply(worker.request(request(case,mode,iterations)),case,iterations)
        elapsed = [[],[]]
        for sample in range(samples):
            for index in ([0,1] if sample % 2 == 0 else [1,0]):
                if progress: progress(f'attempt={attempt+1} sample={sample+1}/{samples} engine={"candidate" if index==0 else "reference"} iterations={iterations}')
                value = validate_reply(workers[index].request(request(case,mode,iterations)),case,iterations)
                equivalent(value,expected)
                elapsed[index].append(value['elapsed_ns'])
        measured = [distribution(values,iterations,window_ns) for values in elapsed]
        comparison = ratchet(*measured,max_ratio)
        minimum = min(elapsed[0] + elapsed[1])
        slower_minimum = max(min(elapsed[0]),min(elapsed[1]))
        both_qualified = minimum >= window_ns
        screen_qualified = (screen and slower_minimum >= window_ns and minimum >= clock_floor_ns
                            and not comparison['noisy'] and comparison['ratio_interval'][1] <= max_ratio)
        noisy_qualified = both_qualified and comparison['noisy']
        noise_limit = attempt == 3 or iterations >= max_iterations
        qualification = ('both-duration-floor-noisy' if noise_limit else 'refine-noise') if noisy_qualified else 'both-duration-floor' if both_qualified else 'stable-headroom-screen' if screen_qualified else 'refine-duration'
        attempts.append({'iterations':iterations,'iteration_counts':{'candidate':iterations,'reference':iterations},'candidate_elapsed_ns':elapsed[0],'reference_elapsed_ns':elapsed[1],
                         'qualification':qualification,'ratio_interval':comparison['ratio_interval'],
                         'noisy':comparison['noisy'],'coefficients_of_variation':[item['coefficient_of_variation']for item in measured],
                         'headroom_minimum_batch_ns':clock_floor_ns if screen else None})
        if progress: progress(f'attempt={attempt+1} qualification={qualification} ratio={comparison["ratio"]:.6g}')
        if (both_qualified and not comparison['noisy']) or screen_qualified or (noisy_qualified and noise_limit):
            return measured, attempts
        # Clear headroom may only need a slightly larger slower-engine or clock
        # window. Unclear ratios and noise always escalate to full duration.
        clear = screen and not comparison['noisy'] and comparison['ratio_interval'][1] <= max_ratio
        factor = (max(window_ns / max(slower_minimum,1),clock_floor_ns / max(minimum,1))
                  if clear else window_ns / max(minimum,1))
        increased = min(max_iterations,max(iterations+1,iterations*2 if noisy_qualified else math.ceil(iterations * factor * 1.15)))
        if increased <= iterations:
            raise TimingQualificationError('timing noise: qualified sample duration unavailable at iteration cap', attempts)
        iterations = increased
    raise TimingQualificationError('timing noise: repeated samples could not qualify duration/headroom', attempts)


def sample_pair_per_engine(workers, case, mode, iterations, window_ns, samples, max_iterations, expected,
                           *, screen=False, clock_floor_ns=100000, max_ratio=1.0, progress=None):
    """Qualify independently sized batches without changing acceptance gates.

    Every retry collects a fresh alternating sample set from both engines.
    Duration-deficient or noisy engines increase work within the same four
    attempts and per-engine iteration caps; all completed attempts survive errors.
    """
    counts = list(iterations)
    if len(counts) != 2 or any(not 1 <= count <= max_iterations for count in counts):
        raise ValueError('invalid per-engine iteration counts')
    attempts = []
    for attempt in range(4):
        for index, worker in enumerate(workers):
            if progress: progress(f'attempt={attempt+1} preheat engine={"candidate" if index==0 else "reference"} iterations={counts[index]}')
            value = validate_reply(worker.request(request(case,mode,counts[index])),case,counts[index])
            equivalent(value,expected)
        elapsed = [[],[]]
        for sample in range(samples):
            for index in ([0,1] if sample % 2 == 0 else [1,0]):
                if progress: progress(f'attempt={attempt+1} sample={sample+1}/{samples} engine={"candidate" if index==0 else "reference"} iterations={counts[index]}')
                value = validate_reply(workers[index].request(request(case,mode,counts[index])),case,counts[index])
                equivalent(value,expected)
                elapsed[index].append(value['elapsed_ns'])
        measured = [distribution(values,counts[index],window_ns) for index,values in enumerate(elapsed)]
        comparison = ratchet(*measured,max_ratio)
        minima = [min(values) for values in elapsed]
        minimum = min(minima)
        slower_minimum = max(minima)
        both_qualified = minimum >= window_ns
        screen_qualified = (screen and slower_minimum >= window_ns and minimum >= clock_floor_ns
                            and not comparison['noisy'] and comparison['ratio_interval'][1] <= max_ratio)
        noisy_indices = [index for index,item in enumerate(measured) if item['coefficient_of_variation'] > 0.25]
        noisy_qualified = both_qualified and comparison['noisy']
        noise_limit = attempt == 3 or any(counts[index] >= max_iterations for index in noisy_indices)
        qualification = ('both-duration-floor-noisy' if noise_limit else 'refine-noise') if noisy_qualified else 'both-duration-floor' if both_qualified else 'stable-headroom-screen' if screen_qualified else 'refine-duration'
        attempts.append({'iteration_counts':{'candidate':counts[0],'reference':counts[1]},
                         'candidate_elapsed_ns':elapsed[0],'reference_elapsed_ns':elapsed[1],
                         'qualification':qualification,'ratio_interval':comparison['ratio_interval'],
                         'noisy':comparison['noisy'],'coefficients_of_variation':[item['coefficient_of_variation']for item in measured],
                         'headroom_minimum_batch_ns':clock_floor_ns if screen else None})
        if progress: progress(f'attempt={attempt+1} qualification={qualification} ratio={comparison["ratio"]:.6g}')
        if (both_qualified and not comparison['noisy']) or screen_qualified or (noisy_qualified and noise_limit):
            return measured, attempts
        clear = screen and not comparison['noisy'] and comparison['ratio_interval'][1] <= max_ratio
        targets = [window_ns,window_ns]
        if clear:
            targets = [clock_floor_ns,clock_floor_ns]
            targets[minima.index(slower_minimum)] = window_ns
        increased = list(counts)
        for index in range(2):
            if minima[index] < targets[index]:
                increased[index] = max(counts[index]+1,math.ceil(counts[index]*targets[index]/max(minima[index],1)*1.15))
            if index in noisy_indices:
                increased[index] = max(increased[index],counts[index]*2)
            increased[index] = min(max_iterations,increased[index])
            if increased[index] <= counts[index] and (minima[index] < targets[index] or index in noisy_indices):
                raise TimingQualificationError('timing noise: qualified sample duration unavailable at iteration cap', attempts)
        counts = increased
    raise TimingQualificationError('timing noise: repeated samples could not qualify duration/headroom', attempts)


def distribution(values, iterations, window_ns):
    per_operation = [value / iterations for value in values]
    if not per_operation or min(per_operation) <= 0:
        raise ValueError('zero-resolution timing')
    mean = statistics.mean(per_operation)
    return {'iterations': iterations, 'samples_ns': per_operation, 'median_ns': statistics.median(per_operation),
            'min_ns': min(per_operation), 'max_ns': max(per_operation),
            'coefficient_of_variation': statistics.pstdev(per_operation) / mean,
            'minimum_window_ns': window_ns, 'observed_minimum_window_ns': min(values)}


def ratchet(candidate, reference, max_ratio=1.0):
    ratio = candidate['median_ns'] / reference['median_ns']
    return {'ratio': ratio, 'max_ratio': max_ratio, 'regression': ratio > max_ratio,
            'ratio_interval': [candidate['min_ns'] / reference['max_ns'], candidate['max_ns'] / reference['min_ns']],
            'noisy': max(candidate['coefficient_of_variation'], reference['coefficient_of_variation']) > 0.25}


def scaling(records, max_exponent=1.3):
    previous = {}; checks = []
    for row in sorted(records, key=lambda item: (item['mode'], item['case'].get('family',''), len(item['case']['source'].encode()))):
        case = row['case']
        if not case.get('provenance', {}).get('geometric') or 'candidate' not in row:
            continue
        key = row['mode'], case['family']
        before = previous.get(key); previous[key] = row
        if before:
            growth = len(case['source'].encode()) / len(before['case']['source'].encode())
            if growth <= 1:
                continue
            ratio = row['candidate']['median_ns'] / before['candidate']['median_ns']
            exponent = math.log(ratio) / math.log(growth)
            checks.append({'mode': row['mode'], 'family': case['family'], 'from': before['case']['id'], 'to': case['id'],
                           'byte_growth': growth, 'time_growth': ratio, 'exponent': exponent,
                           'max_exponent': max_exponent, 'regression': exponent > max_exponent})
    return checks


def compiler_identity(command):
    executable = shutil.which(command)
    if not executable:
        raise ValueError('compiler not found: ' + command)
    version = subprocess.run([executable, 'version'], check=True, text=True, capture_output=True).stdout.strip()
    return {'path': executable, 'version': version, 'sha256': digest(executable)}


def build(args, output):
    identity = source_identity(args.root)
    binary = args.candidate_binary or output / 'performance-native'
    receipt_path = binary.with_suffix('.build.json')
    if args.candidate_binary:
        if not receipt_path.exists():
            if not args.smoke:
                raise ValueError('reused binary requires sibling .build.json provenance receipt; build fresh first')
            return binary, {'provenance': 'unknown-smoke-only', 'binary_sha256': digest(binary)}
        receipt = json.loads(receipt_path.read_text())
        if receipt.get('schema_version') != 1 or not isinstance(receipt.get('compiler'), dict) or receipt.get('binary_sha256') != digest(binary) or receipt.get('source_identity') != identity or receipt.get('build_flags') != ['--opt=speed']:
            raise ValueError('reused binary build receipt does not match optimized current sources')
        return binary, receipt
    compiler = compiler_identity(args.roc)
    command = [compiler['path'], 'build', '--opt=speed', str(args.root / 'scripts/asciidoc/performance.roc'), '--output=' + str(binary)]
    binary.unlink(missing_ok=True)
    result = subprocess.run(command, capture_output=True, text=True, timeout=args.build_timeout)
    if result.returncode != 0 or not binary.exists():
        raise ValueError('native build failed: ' + result.stdout + result.stderr)
    if source_identity(args.root) != identity or compiler_identity(args.roc) != compiler:
        raise ValueError('build inputs changed during compilation')
    receipt = {'schema_version': 1, 'compiler': compiler, 'build_flags': ['--opt=speed'], 'source_identity': identity,
               'binary_sha256': digest(binary), 'command': command}
    receipt_path.write_text(json.dumps(receipt, indent=2) + '\n')
    return binary, receipt


def positive(text):
    value = float(text)
    if not math.isfinite(value) or value <= 0:
        raise argparse.ArgumentTypeError('must be finite and positive')
    return value


def parser():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--root', type=Path, default=ROOT, help='source root; defaults to this checkout')
    ap.add_argument('--roc', default='roc')
    ap.add_argument('--candidate-binary', type=Path)
    ap.add_argument('--gem-home', type=Path, default=DEFAULT_HOME, help='pinned gem installation from scripts/asciidoc/setup_oracle.py')
    ap.add_argument('--ruby', default='ruby')
    ap.add_argument('--generated', type=int, default=2000)
    ap.add_argument('--seed', type=int, default=719)
    ap.add_argument('--sizes', type=int, nargs='+', default=[32,64,128,256,512,1024])
    ap.add_argument('--modes', choices=['parse','render'], nargs='+', default=['parse','render'])
    ap.add_argument('--samples', type=int, default=5)
    ap.add_argument('--batch-policy', choices=['common','per-engine'], default='per-engine',
                    help='calibrate independent engine counts, or retain historical shared-count batches')
    ap.add_argument('--window-ms', type=positive, default=20.0)
    ap.add_argument('--max-ratio', type=positive, default=1.0)
    ap.add_argument('--max-scaling-exponent', type=positive, default=1.3)
    ap.add_argument('--max-iterations', type=int, default=100000)
    ap.add_argument('--timeout', type=positive, default=180.0,
                    help='per-response wall budget including oracle warmup and fresh correctness checks; independent of the speed ceiling')
    ap.add_argument('--build-timeout', type=positive, default=180.0)
    ap.add_argument('--deadline-seconds', type=positive, default=7200.0)
    ap.add_argument('--fixtures', type=Path, action='append', help='override default: main fixtures plus all *-fixtures.json suites')
    ap.add_argument('--corpus', type=Path, action='append', default=[])
    ap.add_argument('--replay', type=Path, action='append', default=[])
    ap.add_argument('--output', type=Path, default=ROOT / '.roc-parser-tmp/asciidoc-performance')
    ap.add_argument('--stable-all', action='store_true', help='require full duration for both engines on every input; default screens clear headroom and escalates near-threshold/noisy cases')
    ap.add_argument('--smoke', action='store_true')
    ap.add_argument('--progress', action='store_true', help='flush case, engine, calibration and sample stages to stderr')
    return ap



def replay_command(args, retained, modes):
    """Retain the original confidence windows and strict limits on replay."""
    command = [sys.executable, str(Path(__file__).resolve()), '--root', str(args.root),
               '--replay', str(retained), '--modes', *modes, '--roc', args.roc,
               '--ruby', args.ruby, '--window-ms', str(args.window_ms),
               '--samples', str(args.samples), '--batch-policy', args.batch_policy, '--max-ratio', str(args.max_ratio),
               '--max-scaling-exponent', str(args.max_scaling_exponent),
               '--max-iterations', str(args.max_iterations), '--timeout', str(args.timeout),
               '--build-timeout', str(args.build_timeout), '--deadline-seconds', str(args.deadline_seconds),
               '--output', str(args.output / 'replay')]
    if args.progress:
        command.append('--progress')
    if args.gem_home:
        command.extend(['--gem-home', str(args.gem_home)])
    return shlex.join(command)


def main(argv=None):
    args = parser().parse_args(argv)
    if args.max_ratio > 1.0:
        raise SystemExit('--max-ratio cannot exceed the required 1.0x ceiling')
    if not 0 <= args.generated <= 100000 or not 1 <= args.samples <= 101 or not 1 <= args.max_iterations <= 100000 or any(s <= 0 for s in args.sizes):
        raise SystemExit('invalid generation/sample/iteration/size bound')
    args.root = args.root.resolve(); args.output.mkdir(parents=True, exist_ok=True)
    journal = args.output / 'measurements.jsonl'
    journal.write_text('')
    report = {'schema_version': 2, 'batch_policy': args.batch_policy, 'progress': args.progress, 'smoke': args.smoke, 'max_ratio': args.max_ratio,
              'timed_operations': args.modes, 'deadline_seconds': args.deadline_seconds,
              'resource_policy': 'source-revision and many-small-table guards use bounded sizes 128/256/512 pending larger qualification; nested timing families use depths 2..32; package tests verify explicit rejection beyond the 64-level parser limit', 'seed': args.seed, 'records': [], 'scaling': [], 'failures': [],
              'command': [sys.executable, str(Path(__file__).resolve()), *(argv if argv is not None else sys.argv[1:])],
              'machine': {'system': platform.platform(), 'processor': platform.processor(), 'cores': os.cpu_count()},
              'measurement_policy': 'strict per-input1.0x median gate; recorded batch_policy selects shared or independent counts, each elapsed duration divided by its own operation count; short batches pass only with CV<=0.25, conservative ratio interval upper<=limit and >=100us minimum; near-threshold/noisy/replay/geometric inputs use full duration; noisy qualified batches refine within four attempts before failing; all attempts retained; warmup/correctness/IPC/serialization excluded; smoke never asserts timings',
              'driver_sha256': digest(__file__), 'worker_transport_sha256': digest(ROOT / 'scripts/review_roc_syntax.py'),
              'host_monotonic_nominal_resolution_ns': max(1,math.ceil(time.get_clock_info('monotonic').resolution*1000000000))}
    workers = []
    started = time.monotonic()
    deadline = started + args.deadline_seconds
    try:
        binary, build_info = build(args, args.output)
        report['candidate_build'] = build_info
        enforce_deadline(deadline)
        oracle = args.root / 'scripts/asciidoc/performance_oracle.rb'
        reference_env = oracle_environment(args.gem_home)
        report['reference'] = {'version': '2.0.23', 'worker_sha256': digest(oracle), 'oracle_sha256': digest(oracle.with_name('oracle.rb')),
                               'ruby': subprocess.run([args.ruby,'--version'], capture_output=True,text=True,check=True,env=reference_env,
                                                      timeout=max(1e-9, min(args.timeout, deadline-time.monotonic()))).stdout.strip(),
                               'environment': {key: reference_env.get(key) for key in ('GEM_HOME','GEM_PATH','TZ','LANG','LC_ALL','SOURCE_DATE_EPOCH','RUBYOPT','RUBYLIB','BUNDLE_GEMFILE')}}
        commands = [[str(binary.resolve())], [args.ruby,str(oracle)]]
        def start_workers():
            enforce_deadline(deadline)
            return [DeadlineWorker(command, args.timeout, deadline, env=reference_env if index == 1 else None)
                    for index, command in enumerate(commands)]
        workers = start_workers()
        for case in corpus(args):
            for mode in args.modes:
                row = {'case': case, 'mode': mode, 'source_sha256': hashlib.sha256(case['source'].encode()).hexdigest(), 'request_sha256': case_identity(case)}
                progress = (lambda message: print(f"AsciiDoc performance [{case['id']} {mode}]: {message}", file=sys.stderr, flush=True)) if args.progress else None
                if progress: progress('begin')
                try:
                    enforce_deadline(deadline)
                    checks = []
                    for index,worker in enumerate(workers):
                        if progress: progress(f'correctness engine={"candidate" if index==0 else "reference"} iterations=0')
                        checks.append(validate_reply(worker.request(request(case,mode,0)),case,0))
                    equivalent(*checks)
                    if checks[1].get('oracle_version') != '2.0.23' or len(checks[1].get('oracle_code_sha256','')) != 64:
                        raise ValueError('pinned reference identity missing or wrong version')
                    report['reference']['gem_code_sha256'] = checks[1]['oracle_code_sha256']
                    if checks[1].get('include_io') != 'caller-map-memory':
                        raise ValueError('reference caller-map memory IO contract missing')
                    report['reference']['include_io'] = checks[1]['include_io']
                    report['reference']['include_path_metadata'] = checks[1]['include_path_metadata']
                    row['correctness'] = {'projection': True, 'rendered_dom': True}
                    window = int((1 if args.smoke else args.window_ms) * 1000000)
                    stable = args.smoke or args.stable_all or bool(args.replay) or case.get('provenance',{}).get('geometric',False)
                    row['measurement_strategy'] = 'full-duration' if stable else 'adaptive-headroom-screen'
                    if stable:
                        counts = [calibrate(worker,case,mode,window,args.max_iterations,checks[1],progress=progress,engine='candidate' if index==0 else 'reference') for index,worker in enumerate(workers)]
                    else:
                        count = calibrate_common(workers,case,mode,window,args.max_iterations,checks[1],progress=progress)
                        counts = [count,count]
                    sampler = sample_pair_per_engine if args.batch_policy == 'per-engine' else sample_pair
                    iterations = counts if args.batch_policy == 'per-engine' else max(counts)
                    measured, attempts = sampler(workers,case,mode,iterations,window,1 if args.smoke else args.samples,args.max_iterations,checks[1],screen=not stable,max_ratio=args.max_ratio,progress=progress)
                    row['candidate'],row['reference'] = measured
                    row['measurement_attempts'] = attempts
                    row['timing_qualification'] = attempts[-1]['qualification']
                    row['comparison'] = ratchet(row['candidate'],row['reference'],args.max_ratio)
                    row['status'] = 'protocol-smoke' if args.smoke else 'noisy' if row['comparison']['noisy'] else 'slow' if row['comparison']['regression'] else 'pass'
                    enforce_deadline(deadline)
                    if not args.smoke and row['status'] != 'pass':
                        report['failures'].append({'id':case['id'],'mode':mode,'kind':row['status']})
                except Exception as error:
                    row['status'] = 'error'; row['error'] = str(error)
                    if isinstance(error, TimingQualificationError):
                        row['measurement_attempts'] = error.measurement_attempts
                    report['failures'].append({'id':case['id'],'mode':mode,'kind':'error','message':str(error)})
                    for worker in workers:
                        worker.close()
                    workers = [] if row['error'].startswith('full-run deadline') else start_workers()
                if progress: progress(f'complete status={row["status"]}' + (f' error={row["error"]}' if 'error' in row else ''))
                report['records'].append(row)
                if row['status'] in ('error','slow','noisy'):
                    failure = args.output / 'retained' / (row['request_sha256'] + '-' + mode + '.json')
                    row['replay'] = replay_command(args,failure,[mode])
                    failure.parent.mkdir(exist_ok=True)
                    failure.write_text(json.dumps({'case':case,'measurement':row,'run':retained_run(report)},indent=2)+'\n')
                # Append durable partial evidence without rewriting an O(n²) report.
                with journal.open('a') as checkpoint:
                    checkpoint.write(json.dumps(row)+'\n')
                if not args.smoke and len(report['records']) % 100 == 0:
                    print(f"AsciiDoc performance: {len(report['records'])} records; {time.monotonic()-started:.1f}s elapsed; {len(report['failures'])} failures; {case['id']} {mode}", file=sys.stderr, flush=True)
                if row.get('error','').startswith('full-run deadline'):
                    raise TimeoutError(row['error'])
        report['scaling'] = scaling(report['records'],args.max_scaling_exponent)
        if not args.smoke:
            report['failures'] += [{'kind':'scaling',**check} for check in report['scaling'] if check['regression']]
            for check in report['scaling']:
                if check['regression']:
                    row = next(row for row in report['records'] if row['mode']==check['mode'] and row['case']['id']==check['to'])
                    retained = args.output / 'retained' / (row['request_sha256'] + '-scaling-' + row['mode'] + '.json')
                    retained.parent.mkdir(exist_ok=True)
                    before = next(item for item in report['records'] if item['mode']==check['mode'] and item['case']['id']==check['from'])
                    check['retained'] = str(retained)
                    check['replay'] = replay_command(args,retained,[row['mode']])
                    retained.write_text(json.dumps({'cases':[before['case'],row['case']],'measurement':row,'scaling':check,'run':retained_run(report)},indent=2)+'\n')
        enforce_deadline(deadline)
    except Exception as error:
        report['failures'].append({'kind':'workflow','message':str(error)})
    finally:
        for worker in workers:
            worker.close()
    report['workspace_source_identity_at_end'] = source_identity(args.root)
    if 'candidate_build' in report and report['candidate_build'].get('source_identity') and report['candidate_build']['source_identity'] != report['workspace_source_identity_at_end']:
        report['failures'].append({'kind':'workflow','message':'sources changed after native build; current checkout not verified'})
    if time.monotonic() >= deadline and not any('full-run deadline' in item.get('message','') for item in report['failures']):
        report['failures'].append({'kind':'workflow','message':'full-run deadline exceeded; campaign not qualified'})
    (args.output/'results.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({'records':len(report['records']),'failures':len(report['failures']),'report':str(args.output/'results.json'),'smoke':args.smoke}))
    return 1 if report['failures'] else 0

if __name__ == '__main__':
    raise SystemExit(main())
