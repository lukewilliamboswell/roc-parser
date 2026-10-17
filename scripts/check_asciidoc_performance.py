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
from review_roc_syntax import Worker as JsonlWorker
from review_asciidoc import DEFAULT_HOME, canonical_projection, canonical_html


class Worker(JsonlWorker):
    """Performance accepts both reply protocols; syntax workers remain v1."""
    def __init__(self, command, timeout=5.0, max_response=16 * 1024 * 1024, env=None):
        super().__init__(command, timeout, max_response, env=env, protocol_versions=(1, 2))


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
        'candidate_build', 'reference', 'measurement_policy', 'scaling_policy', 'correctness_policy', 'worker_protocols', 'batch_policy', 'progress', 'max_ratio', 'deadline_seconds') if key in report}


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


def live_dynamic_source_redefinitions(size, kind):
    sets = kind == 'sets'
    seed = ' '.join(f'{{set:key{i}:value{i}}}' if sets else f'{{counter:key{i}}}' for i in range(size))
    source = 'Begin.\n\nSeed ' + seed + (' done.\n\n' if sets else '.\n\n')
    blocks = []
    for index in range(size):
        reset = kind == 'counter-reset' and index == size // 2
        definition = f':unrelated: {index}\n' + (':key0: 40\n' if reset else '')
        first = '{counter:key0}' if reset else '{key0}'
        blocks.append(definition + f'\nProbe {first} {{key{size - 1}}} {{unrelated}}.\n\n')
    final = f'Final {{key0}} {{key{size - 1}}} {{unrelated}}.\n' if sets else f'Final {{counter:key0}} {{counter:key{size - 1}}} {{unrelated}}.\n'
    return source + ''.join(blocks) + final


def each_live_counter_reset(size):
    seed = ' '.join(f'{{counter:key{i}}}' for i in range(size))
    body = ''.join(f':key{i}: 40\n\nProbe {{counter:key{i}}}.\n\n' for i in range(size))
    return 'Begin.\n\nSeed ' + seed + '.\n\n' + body + f'Final {{counter:key0}} {{counter:key{size - 1}}}.\n'


def alternating_rich_simple_counters(size):
    blocks = ''.join(f'Counter {{counter:key{i}}}.\n\nBody.\n\n' for i in range(size))
    return blocks + f'Final {{counter:key0}} {{counter:key{size - 1}}}.\n'


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
        'live-dynamic-source-redefinitions-counters': lambda n: live_dynamic_source_redefinitions(n, 'counters'),
        'live-dynamic-source-redefinitions-sets': lambda n: live_dynamic_source_redefinitions(n, 'sets'),
        'live-dynamic-source-redefinitions-counter-reset': lambda n: live_dynamic_source_redefinitions(n, 'counter-reset'),
        'live-dynamic-source-redefinitions-each-counter-reset': each_live_counter_reset,
        'alternating-rich-simple-counters': alternating_rich_simple_counters,
        'alternating-rich-simple-counters-option': alternating_rich_simple_counters,
        'alternating-rich-simple-counters-restored-standard-option': alternating_rich_simple_counters,
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
        'default-callout-comment': lambda n: '----\n' + 'code <!--1-->\n' * n + '----',
        'default-callout-escaped': lambda n: '----\n' + 'code \\<1>\n' * n + '----',
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
                'live-dynamic-source-redefinitions-counters','live-dynamic-source-redefinitions-sets',
                'live-dynamic-source-redefinitions-counter-reset','live-dynamic-source-redefinitions-each-counter-reset',
                'alternating-rich-simple-counters','alternating-rich-simple-counters-option',
                'alternating-rich-simple-counters-restored-standard-option',
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
                      'mailto-positional-query-counter-blocks','default-callout-comment',
                      'default-callout-escaped') and max(sizes) >= 1024:
            family_sizes = sorted(set(sizes) | {2048,4096})
        if family in ('static-base-attribute-revisions-url','static-base-attribute-revisions-text',
                      'static-base-attribute-revisions-varied-unset-readd','static-attributes-many-tables',
                      'live-dynamic-source-redefinitions-counters','live-dynamic-source-redefinitions-sets',
                      'live-dynamic-source-redefinitions-counter-reset','live-dynamic-source-redefinitions-each-counter-reset',
                      'alternating-rich-simple-counters','alternating-rich-simple-counters-option',
                      'alternating-rich-simple-counters-restored-standard-option'):
            # Larger runs wait for qualification of the persistent snapshot fix.
            family_sizes = [size for size in sizes if size in (128,256,512)]
        if family == 'table-footnotes' and max(sizes) >= 1024:
            # The footer's repeated prior-note scans become clear at 8,192
            # cells, still within the default 256 KiB source bound.
            family_sizes = sorted(set(family_sizes) | {8192})
        for size in family_sizes:
            case = {'id': f'scaling-{family}-{size}', 'family': family, 'size': size, 'source': make(size),
                    'provenance': {'generator': family, 'parameter': size, 'geometric': True}}
            if family in ('alternating-rich-simple-counters','alternating-rich-simple-counters-option',
                          'alternating-rich-simple-counters-restored-standard-option'):
                case['expect'] = {'html_contains':['<p>Counter 1.</p>','<p>Body.</p>','<p>Final 2 2.</p>']}
            if family == 'default-callout-comment':
                case['expect'] = {'contexts':['listing'], 'html_contains':['code &lt;!--<b class=\"conum\">(1)</b>--&gt;']}
            elif family == 'default-callout-escaped':
                case['expect'] = {'contexts':['listing'], 'html_contains':['code &lt;1&gt;'], 'html_excludes':['class=\"conum\"']}
            if family == 'alternating-rich-simple-counters-option':
                case['id'] = f'scaling-alternating-rich-simple-counters-{size}-same-standard-option'
                case['attributes'] = [{'name':'outfilesuffix','value':'.html'}]
                case['provenance'].update(options_probe='same-value-standard-option', measurement_status='measured-regression')
            elif family == 'alternating-rich-simple-counters-restored-standard-option':
                case['id'] = f'scaling-alternating-rich-simple-counters-{size}-restored-standard-option'
                case['attributes'] = [{'name':'outfilesuffix','value':'.x'}, {'name':'outfilesuffix','value':'.html'}]
                case['provenance'].update(options_probe='changed-then-restored-standard-option', measurement_status='unmeasured')
            yield case



def measured_regressions(sizes):
    """Append bounded measured regressions without reordering historical inputs."""
    selected = [size for size in (128, 256, 512) if size in sizes]
    for family in ('header-first-section-quote-counters',
                   'header-first-section-mutating-previews'):
        for size in selected:
            if family == 'header-first-section-quote-counters':
                title = ' '.join(f'{{counter:key{i}}} pass:q[*word*]' for i in range(size))
            else:
                title = ' '.join(f'pass:a[{{counter:key{i}}}] {{set:last:value}}' for i in range(size))
            yield {'id': f'scaling-{family}-body-{size}', 'family': family, 'size': size,
                   'source': '= Header {project}\n:project: Roc\n\n== ' + title + '\n\nBody.',
                   'expect': {'html_contains': ['<p>Body.</p>']},
                   'provenance': {'geometric': True, 'reason': 'First-body section title grows live counters during immediate pass/preview operations, with a static body. The separately retained counter-observing body reveals an inherited native/Ruby title-side-effect difference.'}}
    for size in selected:
        bits = size.bit_length() - 1
        names = ['k' + ''.join('c8' if bit == '1' else 'az' for bit in f'{i:0{bits}b}')
                 for i in range(size)]
        name_hash = 5381
        for byte in names[0].encode():
            name_hash = (name_hash * 33 + byte) % 4294967296
        yield {'id': f'scaling-source-store-colliding-attribute-names-{size}',
               'family': 'source-store-colliding-attribute-names', 'size': size,
               'source': '\n'.join(f':{name}: value{i}' for i, name in enumerate(names))
                         + '\n\nBody {' + names[0] + '} {' + names[-1] + '}.',
               'expect': {'html_contains': [f'<p>Body value0 value{size-1}.</p>']},
               'provenance': {'geometric': True, 'reason': 'All valid lowercase/digit attribute names collide under the current uint32 DJB ordering hash; az and c8 equal polynomial contributions. Measures exact-name collision fallback, never hash identity.',
                              'name_length': len(names[0]), 'name_hash': name_hash}}


def historical_corpus(args):
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
    yield from measured_regressions(args.sizes)


def fixed_performance_guards(args):
    """Load retained measurement inputs without flattening their families."""
    path = args.root / 'scripts/asciidoc/performance-guards.json'
    if not path.exists():
        raise ValueError('required performance guard file missing: ' + str(path))
    value = json.loads(path.read_text())
    if not isinstance(value, dict) or value.get('schema_version') != 1 or not isinstance(value.get('cases'), list):
        raise ValueError('invalid performance guard schema: ' + str(path))
    guard_sha256 = digest(path)
    expanded = max(args.sizes, default=0) >= 1024
    for case in value['cases']:
        if (case.get('size', 0) > 512 and case.get('scaling_input', 'source') == 'source'
                and not expanded):
            continue
        yield {**case, 'provenance': {**case.get('provenance', {}),
               'performance_guard': str(path), 'performance_guard_sha256': guard_sha256}}


def corpus(args):
    # Replay and smoke keep their historical early returns, without opening
    # the guard file or changing their input inventory.
    if args.replay or args.smoke:
        yield from historical_corpus(args)
        return
    # Fail missing, malformed or unsupported guard data before any expensive
    # historical request runs. Retained guards still appear only at the end.
    guards = list(fixed_performance_guards(args))
    seen = {}
    for case in historical_corpus(args):
        seen.setdefault(case['id'], set()).add(case_identity(case))
        yield case
    for case in guards:
        fingerprint = case_identity(case)
        if case['id'] in seen:
            if seen[case['id']] != {fingerprint}:
                raise ValueError('performance guard duplicate ID has different request fingerprint: ' + case['id'])
            continue
        seen[case['id']] = {fingerprint}
        yield case


def request(case, mode, iterations, warmup=0):
    value = {'id': case['id'], 'source': case['source'], 'includes': [{'path': path, 'source': source} for path, source in sorted(case.get('includes', {}).items())],
            'expand': 'includes' in case, 'mode': mode, 'iterations': iterations, 'warmup': warmup}
    attributes = case.get('attributes', [])
    if not isinstance(attributes, list) or any(not isinstance(a, dict) or set(a) != {'name', 'value'} or not isinstance(a['name'], str) or not isinstance(a['value'], str) for a in attributes):
        raise ValueError('attributes must be an ordered list of string name/value records')
    if attributes:
        value['attributes'] = attributes
    return value



def request_identity(value):
    payload = {key: value[key] for key in ('source', 'includes', 'expand')}
    if value.get('attributes'):
        payload['attributes'] = value['attributes']
    return hashlib.sha256(json.dumps(payload, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()


def case_identity(case):
    """The historical source/options fingerprint excludes measurement fields."""
    return request_identity(request(case, 'parse', 0))


_CHECKED_TIMING = object()


def validate_reply(value, case, iterations, *, mode=None, warmup=None, allow_unchecked_timing=False):
    if value.get('error'):
        raise ValueError('worker error: ' + json.dumps(value))
    version = value.get('version')
    if (type(version) is not int or version not in (1, 2) or value.get('id') != case['id']
            or type(value.get('iterations')) is not int or value['iterations'] != iterations
            or type(value.get('successes')) is not int or value['successes'] != iterations):
        raise ValueError('worker protocol/count mismatch')
    if type(value.get('elapsed_ns')) is not int or value['elapsed_ns'] < 0:
        raise ValueError('invalid elapsed time')
    if type(value.get('checksum')) is not int or value['checksum'] < iterations:
        raise ValueError('operation result was not consumed')
    if version == 2:
        phase = 'correctness' if iterations == 0 else 'timing'
        if value.get('phase') != phase or value.get('mode') not in ('parse', 'render') or (mode is not None and value['mode'] != mode):
            raise ValueError('worker protocol phase/mode mismatch')
        if type(value.get('warmup')) is not int or value['warmup'] < 0 or (warmup is not None and value['warmup'] != warmup):
            raise ValueError('worker warmup count mismatch')
        if iterations == 0:
            metric = value.get('operation_checksum')
            if type(metric) is not int or metric < 1 or value['checksum'] != value['warmup'] * metric:
                raise ValueError('invalid fresh operation checksum')
        elif not allow_unchecked_timing and value.get('_checked_timing') is not _CHECKED_TIMING:
            raise ValueError('compact timing requires a fresh checked context')
    if version == 1 or iterations == 0:
        if not isinstance(value.get('projection'), dict) or not isinstance(value.get('html'), str):
            raise ValueError('correctness result missing')
    return value


def equivalent(candidate, reference):
    if candidate.get('version') == 2 and candidate.get('phase') == 'timing':
        if candidate.get('_checked_timing') is not _CHECKED_TIMING:
            raise ValueError('compact timing requires a fresh checked context')
        return
    if canonical_projection(candidate['projection']) != canonical_projection(reference['projection']):
        raise ValueError('candidate/reference block projections differ')
    if candidate['html'] == reference['html']:
        return
    if canonical_html(candidate['html']) != canonical_html(reference['html']):
        raise ValueError('candidate/reference rendered DOM differs')


def worker_stamp(worker):
    transport = getattr(worker, 'worker', worker)
    process = getattr(transport, 'proc', None)
    return (id(worker), id(transport), id(process), getattr(process, 'pid', None))


class CheckedTimingWorker:
    """One case/mode and one worker instance, authenticated by a full zero check.

    No output is synthesized for compact replies. The private marker merely
    records that the exact request/count/engine checksum was validated here.
    """
    def __init__(self, worker, case, mode, before):
        validate_reply(before, case, 0, mode=mode, warmup=0)
        self.worker = worker
        self.instance = worker_stamp(worker)
        # Retain the original transport identity even if a wrapper swaps it.
        self.transport = getattr(worker, 'worker', worker)
        self.case = case
        self.fingerprint = case_identity(case)
        self.mode = mode
        self.before = before
        self.active = True

    def request(self, value):
        try:
            if not self.active or worker_stamp(self.worker) != self.instance:
                raise ValueError('checked worker context expired')
            if request_identity(value) != self.fingerprint or value.get('mode') != self.mode or type(value.get('iterations')) is not int or value['iterations'] <= 0:
                raise ValueError('timing request differs from checked case/mode')
            reply = validate_reply(self.worker.request(value), self.case, value['iterations'], mode=self.mode,
                                   warmup=value['warmup'], allow_unchecked_timing=True)
            if worker_stamp(self.worker) != self.instance:
                raise ValueError('checked worker context expired during request')
            if reply['version'] != self.before['version']:
                raise ValueError('worker protocol changed within checked context')
            if reply['version'] == 2:
                if reply['checksum'] != (value['iterations'] + value['warmup']) * self.before['operation_checksum']:
                    raise ValueError('timing checksum differs from fresh per-engine operation')
                for key in ('oracle_version', 'oracle_code_sha256', 'include_io'):
                    if key in self.before and reply.get(key) != self.before[key]:
                        raise ValueError('reference identity changed during timing')
                reply['_checked_timing'] = _CHECKED_TIMING
            else:
                equivalent(reply, self.before)
            return reply
        except Exception:
            self.active = False
            raise

    def invalidate(self):
        self.active = False


def reference_identity(reply, before=None):
    if reply.get('oracle_version') != '2.0.23' or not isinstance(reply.get('oracle_code_sha256'), str) or len(reply['oracle_code_sha256']) != 64:
        raise ValueError('pinned reference identity missing or wrong version')
    if reply.get('include_io') != 'caller-map-memory':
        raise ValueError('reference caller-map memory IO contract missing')
    if before is not None:
        for key in ('oracle_version', 'oracle_code_sha256', 'include_io', 'include_path_metadata'):
            if reply.get(key) != before.get(key):
                raise ValueError('reference identity changed after timing')


def correctness_proof(checks):
    def digest_value(value):
        return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    return [{'version': value['version'], 'phase': value.get('phase', 'legacy-full'),
             'operation_checksum': value.get('operation_checksum'),
             'projection_sha256': digest_value(canonical_projection(value['projection'])),
             'rendered_dom_sha256': digest_value(canonical_html(value['html'])),
             'html_sha256': hashlib.sha256(value['html'].encode()).hexdigest()}
            for value in checks]


def finish_correctness(workers, case, mode, before, after):
    """Fresh self/cross checks even when a timing qualification failed.

    `after` is filled incrementally, so a partial failure remains retainable.
    The bounded worker/deadline policy still applies to each fresh operation.
    """
    for index, worker in enumerate(workers):
        value = worker.request(request(case, mode, 0))
        after.append(value)
        validate_reply(value, case, 0, mode=mode, warmup=0)
    for index, value in enumerate(after):
        if value['version'] != before[index]['version']:
            raise ValueError('worker protocol changed after timing')
        equivalent(value, before[index])
        if value['version'] == 2 and value['operation_checksum'] != before[index]['operation_checksum']:
            raise ValueError('fresh per-engine operation checksum drifted after timing')
    equivalent(*after)
    reference_identity(after[1], before[1])
    return correctness_proof(after)


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



def scaling_measure(case):
    """Measure source bytes by default, or the canonical semantic request.

    Request size uses compact sorted-key UTF-8 JSON of source, includes,
    expand and nonempty ordered attributes. Measurement fields and IDs are
    excluded. A case cannot supply its own numeric input-size override.
    """
    basis = case.get('scaling_input', 'source')
    if basis not in ('source', 'request') or not isinstance(basis, str):
        raise ValueError('scaling_input must be source or request')
    if basis == 'source':
        size = len(case['source'].encode())
    else:
        value = request(case, 'parse', 0)
        payload = {key: value[key] for key in ('source', 'includes', 'expand')}
        if value.get('attributes'):
            payload['attributes'] = value['attributes']
        size = len(json.dumps(payload, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode())
    return {'scaling_input': basis, 'scaling_input_bytes': size}


def scaling(records, max_exponent=1.3):
    previous = {}; checks = []; bases = {}
    measured = [(row, scaling_measure(row['case'])) for row in records]
    for row, measure in sorted(measured, key=lambda item: (item[0]['mode'], item[0]['case'].get('family',''), item[1]['scaling_input_bytes'])):
        case = row['case']
        if not case.get('provenance', {}).get('geometric') or 'candidate' not in row:
            continue
        family = case['family']
        basis = measure['scaling_input']
        if family in bases and bases[family] != basis:
            raise ValueError('mixed scaling_input metrics in family: ' + family)
        bases[family] = basis
        key = row['mode'], family
        before = previous.get(key); previous[key] = row, measure
        if before:
            before_row, before_measure = before
            if before_measure['scaling_input_bytes'] <= 0:
                raise ValueError('geometric scaling input must have positive byte length')
            growth = measure['scaling_input_bytes'] / before_measure['scaling_input_bytes']
            if growth <= 1:
                continue
            ratio = row['candidate']['median_ns'] / before_row['candidate']['median_ns']
            exponent = math.log(ratio) / math.log(growth)
            checks.append({'mode': row['mode'], 'family': family, 'from': before_row['case']['id'], 'to': case['id'],
                           'scaling_input': basis, 'from_input_bytes': before_measure['scaling_input_bytes'], 'to_input_bytes': measure['scaling_input_bytes'],
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
              'resource_policy': 'source-revision, live-dynamic/source-redefinition, alternating Rich/simple and many-small-table guards use bounded sizes 128/256/512 pending larger qualification; nested timing families use depths 2..32; package tests verify explicit rejection beyond the 64-level parser limit', 'seed': args.seed, 'records': [], 'scaling': [], 'failures': [],
              'command': [sys.executable, str(Path(__file__).resolve()), *(argv if argv is not None else sys.argv[1:])],
              'machine': {'system': platform.platform(), 'processor': platform.processor(), 'cores': os.cpu_count()},
              'measurement_policy': 'strict per-input1.0x median gate; recorded batch_policy selects shared or independent counts, each elapsed duration divided by its own operation count; short batches pass only with CV<=0.25, conservative ratio interval upper<=limit and >=100us minimum; near-threshold/noisy/replay/geometric inputs use full duration; noisy qualified batches refine within four attempts before failing; all attempts retained; warmup/correctness/IPC/serialization excluded; smoke never asserts timings',
              'correctness_policy': 'fresh full zero-iteration candidate/reference projection+DOM checks before AND after every case/mode timing; self+cross drift checks; v2 exact per-engine checksum from separate fresh operation; v1 full output checked on every positive response; contexts expire on error/restart',
              'scaling_policy': 'default source UTF-8 bytes; explicit scaling_input=request uses canonical compact sorted-key UTF-8 JSON of source/includes/expand and nonempty ordered attributes; excludes id/mode/iterations/warmup; mixed family metrics rejected; exponent ceiling unchanged',
              'worker_protocols': [1, 2],
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
                row = {'case': case, 'mode': mode, 'source_sha256': hashlib.sha256(case['source'].encode()).hexdigest(), 'request_sha256': case_identity(case), **scaling_measure(case)}
                progress = (lambda message: print(f"AsciiDoc performance [{case['id']} {mode}]: {message}", file=sys.stderr, flush=True)) if args.progress else None
                if progress: progress('begin')
                checks, after_checks, timing_workers = [], [], []
                try:
                    enforce_deadline(deadline)
                    for index,worker in enumerate(workers):
                        if progress: progress(f'correctness engine={"candidate" if index==0 else "reference"} iterations=0')
                        value = worker.request(request(case,mode,0))
                        checks.append(value)
                        validate_reply(value,case,0,mode=mode,warmup=0)
                    equivalent(*checks)
                    reference_identity(checks[1])
                    report['reference']['gem_code_sha256'] = checks[1]['oracle_code_sha256']
                    report['reference']['include_io'] = checks[1]['include_io']
                    report['reference']['include_path_metadata'] = checks[1]['include_path_metadata']
                    row['correctness'] = {'projection': True, 'rendered_dom': True,
                                          'request_sha256': row['request_sha256'], 'mode': mode,
                                          'before': correctness_proof(checks), 'after': None}
                    timing_workers = [CheckedTimingWorker(worker, case, mode, checks[index]) for index,worker in enumerate(workers)]
                    window = int((1 if args.smoke else args.window_ms) * 1000000)
                    stable = args.smoke or args.stable_all or bool(args.replay) or case.get('provenance',{}).get('geometric',False)
                    row['measurement_strategy'] = 'full-duration' if stable else 'adaptive-headroom-screen'
                    if stable:
                        counts = [calibrate(worker,case,mode,window,args.max_iterations,checks[1],progress=progress,engine='candidate' if index==0 else 'reference') for index,worker in enumerate(timing_workers)]
                    else:
                        count = calibrate_common(timing_workers,case,mode,window,args.max_iterations,checks[1],progress=progress)
                        counts = [count,count]
                    sampler = sample_pair_per_engine if args.batch_policy == 'per-engine' else sample_pair
                    iterations = counts if args.batch_policy == 'per-engine' else max(counts)
                    measured, attempts = sampler(timing_workers,case,mode,iterations,window,1 if args.smoke else args.samples,args.max_iterations,checks[1],screen=not stable,max_ratio=args.max_ratio,progress=progress)
                    row['candidate'],row['reference'] = measured
                    row['measurement_attempts'] = attempts
                    if progress: progress('post-timing fresh correctness')
                    row['correctness']['after'] = finish_correctness(workers, case, mode, checks, after_checks)
                    for worker in timing_workers: worker.invalidate()
                    row['timing_qualification'] = attempts[-1]['qualification']
                    row['comparison'] = ratchet(row['candidate'],row['reference'],args.max_ratio)
                    row['status'] = 'protocol-smoke' if args.smoke else 'noisy' if row['comparison']['noisy'] else 'slow' if row['comparison']['regression'] else 'pass'
                    enforce_deadline(deadline)
                    if not args.smoke and row['status'] != 'pass':
                        report['failures'].append({'id':case['id'],'mode':mode,'kind':row['status']})
                except Exception as error:
                    # Failed timings cannot pass without post-checks either.
                    # Keep the original qualification error/attempts if a
                    # bounded post-check also fails or the worker has died.
                    if timing_workers and not after_checks:
                        try:
                            row['correctness']['after'] = finish_correctness(workers, case, mode, checks, after_checks)
                        except Exception as after_error:
                            row.setdefault('correctness', {})['after_error'] = str(after_error)
                    for worker in timing_workers: worker.invalidate()
                    if checks or after_checks:
                        artifact = args.output / 'correctness' / (row['request_sha256'] + '-' + mode + '-correctness.json')
                        artifact.parent.mkdir(exist_ok=True)
                        artifact.write_text(json.dumps({'request': request(case, mode, 0), 'before': checks, 'after': after_checks,
                                                        'error': str(error), 'run': retained_run(report)}, indent=2)+'\n')
                        row['correctness_artifact'] = str(artifact)
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
