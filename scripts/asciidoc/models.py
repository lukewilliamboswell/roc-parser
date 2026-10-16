"""Independent AsciiDoc models and provenance-preserving repository harvesting.

Models are created before source printing and projection. Neither operation calls
an oracle or reparses source. Only the explicitly supported structural subset is
generated; broad semantic coverage lives in fixtures.json.
"""
import hashlib
from pathlib import Path
import random


def block(context, source='', *, title='', level=0, blocks=()):
    return dict(context=context, level=level, title=title, source=source,
                blocks=list(blocks))


def project(model):
    def visit(item):
        kind = item['kind']
        if kind == 'section':
            return block(kind, title=item['title'], level=item['level'],
                         blocks=[visit(x) for x in item['children']])
        return block(kind, item.get('text', ''))
    return {'title': model['title'], 'blocks': [visit(x) for x in model['children']]}


def render(model, newline='\n', trailing_newline=True):
    """Print a model with blank boundaries, without deriving expected output."""
    def visit(item):
        kind = item['kind']
        if kind == 'section':
            return '=' * (item['level'] + 1) + ' ' + item['title'] + '\n\n' + '\n\n'.join(visit(x) for x in item['children'])
        if kind in ('listing', 'literal'):
            delim = '----' if kind == 'listing' else '....'
            return delim + '\n' + item['text'] + '\n' + delim
        if kind == 'thematic_break':
            return "'''"
        return item['text']
    parts = [visit(x) for x in model['children']]
    if model['title']:
        parts.insert(0, '= ' + model['title'])
    source = '\n\n'.join(parts)
    if trailing_newline and source:
        source += '\n'
    return source.replace('\n', newline)


def generate_cases(seed, count):
    """Return deterministic, bounded source/expected pairs for the core subset."""
    if not isinstance(count, int) or not 0 <= count <= 100000:
        raise ValueError('count must be between 0 and 100000')
    rng = random.Random(seed)
    words = ['amber', 'quiet river', 'café', '東京', 'Δοκιμή', '🦊', 'naïve façade']
    def text():
        return rng.choice(words) + ' ' + str(rng.randrange(10000))
    def children(depth):
        result = []
        for _ in range(rng.randrange(1, 5)):
            kinds = ['paragraph', 'paragraph', 'listing', 'literal', 'thematic_break']
            if depth < 3:
                kinds.append('section')
            kind = rng.choice(kinds)
            if kind == 'section':
                result.append(dict(kind=kind, level=depth + 1, title=text(), children=children(depth + 1)))
            elif kind == 'thematic_break':
                result.append(dict(kind=kind))
            else:
                result.append(dict(kind=kind, text='\n'.join(text() for _ in range(rng.randrange(1, 4)))))
        # AsciiDoc sections own subsequent peers until another section. Put
        # body blocks before sections rather than printing structurally ambiguous trees.
        return sorted(result, key=lambda item: item['kind'] == 'section')
    cases = []
    for i in range(count):
        model = {'title': text() if rng.choice([False, True]) else '', 'children': children(0)}
        cases.append({'id': f'model-{seed}-{i}', 'category': 'generated-model',
                      'support': 'minimal', 'model': model,
                      'source': render(model, rng.choice(['\n', '\r\n']), rng.choice([False, True])),
                      'expected': project(model)})
    return cases


def harvest_corpus(root):
    """Read unique UTF-8 .adoc files; never follow links or expand includes.

    Hashes cover original bytes (before UTF-8 decoding), paths are relative to
    root, and the first lexicographic path wins when contents are duplicates.
    Invalid UTF-8 raises instead of silently changing the evidence.
    """
    root = Path(root).resolve(strict=True)
    seen = set()
    cases = []
    for path in sorted(root.rglob('*.adoc')):
        relative = path.relative_to(root)
        if any(part.startswith('.') for part in relative.parts):
            continue
        if path.is_symlink() or any(parent.is_symlink() for parent in path.parents if parent != root and root in parent.parents):
            continue
        if not path.is_file() or not path.resolve().is_relative_to(root):
            continue
        data = path.read_bytes()
        digest = hashlib.sha256(data).hexdigest()
        if digest in seen:
            continue
        seen.add(digest)
        cases.append({'id': 'corpus-' + digest, 'category': 'repository-corpus',
                      'support': 'future', 'path': relative.as_posix(),
                      'sha256': digest, 'size_bytes': len(data),
                      'source': data.decode('utf-8')})
    return cases
