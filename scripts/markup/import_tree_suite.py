#!/usr/bin/env python3
"""Fetch and generate the pinned WPT tree corpus into ignored local storage."""
from __future__ import annotations
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
from pathlib import Path
import tempfile
import urllib.request

PREFIX = 'html/syntax/parsing/resources/'
DATA = Path(__file__).resolve().parent
MANIFEST = DATA / 'tree-suite-manifest.json'
DEFAULT_CACHE = DATA.parents[1] / '.roc-parser-tmp/markup/oracles/tree-suite'
SECTIONS = {'#description', '#data', '#errors', '#new-errors', '#errors-new', '#document-fragment', '#script-on', '#script-off', '#document'}


def download(url: str) -> bytes:
    with urllib.request.urlopen(urllib.request.Request(url, headers={'User-Agent': 'roc-parser-oracle'}), timeout=60) as response:
        return response.read()


def parse_dat(name: str, raw: str) -> list[dict]:
    cases = []
    current, section = {}, None
    def finish():
        if not current:
            return
        if '#data' not in current or '#document' not in current:
            raise ValueError(f'{name}: incomplete fixture')
        context = ''.join(current.get('#document-fragment', [])).strip()
        case = {'id': f'wpt/{name}/{len(cases)}', 'format': 'html', 'syntax': 'html', 'validate': False, 'recovery': 'browser',
                'input': ''.join(current['#data']).removesuffix('\n'), 'expected_dat': ''.join(current['#document']).rstrip('\n'),
                'expected_error_count': len([line for section in ['#errors', '#new-errors', '#errors-new'] for line in current.get(section, []) if line.strip()]),
                'raw_error_sections': {section: ''.join(current[section]).rstrip('\n') for section in ['#errors', '#new-errors', '#errors-new'] if section in current}, 'errors_section_present': any(section in current for section in ['#errors', '#new-errors', '#errors-new']), 'scripting': '#script-on' in current, 'scripting_modes': [True] if '#script-on' in current else [False] if '#script-off' in current else [False, True], 'include_bindings': True}
        if context:
            namespace, local = ('svg', context[4:]) if context.startswith('svg ') else ('mathml', context[5:]) if context.startswith('math ') else ('html', context)
            case.update(fragment=True, context={'namespace': {'html':'http://www.w3.org/1999/xhtml','svg':'http://www.w3.org/2000/svg','mathml':'http://www.w3.org/1998/Math/MathML'}[namespace], 'local': local})
        if current.get('#description'):
            case['description'] = ''.join(current['#description']).strip()
        cases.append(case)
    for line in raw.splitlines(keepends=True):
        marker = line.rstrip('\r\n')
        if marker == '#data' and '#data' in current:
            finish()
            current = {}
        if marker in SECTIONS:
            section = marker
            current.setdefault(section, [])
        elif section:
            # Empty lines separate fixtures after the expected tree, but remain
            # authored bytes inside data/text blocks.
            current[section].append(line)
    finish()
    return cases


def cases_digest(cases: list[dict]) -> str:
    return hashlib.sha256(json.dumps(cases, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()


def validate_inventory(value: dict, manifest: dict) -> None:
    cases = value['cases']
    if (value['source'] != manifest['source'] or len(cases) != manifest['cases']['count']
            or cases_digest(cases) != manifest['cases']['sha256']
            or sum(len(case['scripting_modes']) for case in cases) != manifest['cases']['executions']):
        raise ValueError('generated WPT corpus does not match its pinned manifest')


def write_atomic(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as handle:
        temporary = Path(handle.name)
        handle.write(data)
    try:
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def ensure_suite(cache: Path = DEFAULT_CACHE, manifest_path: Path = MANIFEST) -> Path:
    manifest = json.loads(manifest_path.read_text(encoding='utf-8'))
    source = manifest['source']
    license_bytes = (manifest_path.parent / 'WPT-LICENSE.md').read_bytes()
    if hashlib.sha256(license_bytes).hexdigest() != source['license']['sha256']:
        raise ValueError('WPT license does not match its pinned hash')
    output = cache / 'tree-suite.json'
    if output.is_file():
        try:
            validate_inventory(json.loads(output.read_text(encoding='utf-8')), manifest)
            return output
        except (KeyError, TypeError, ValueError):
            pass  # Regenerate stale or damaged cache entries from verified inputs.

    base = f"https://raw.githubusercontent.com/web-platform-tests/wpt/{source['revision']}/"
    excluded = {item['path'] for item in source['excluded']}
    def fetch(item):
        name = item['path'].removeprefix(PREFIX)
        if not item['path'].startswith(PREFIX) or Path(name).name != name or not name.endswith('.dat'):
            raise ValueError('WPT manifest contains an invalid source path')
        path = cache / 'sources' / (item['sha256'] + '.dat')
        raw = path.read_bytes() if path.is_file() else None
        if raw is None or hashlib.sha256(raw).hexdigest() != item['sha256']:
            raw = download(base + item['path'])
            if hashlib.sha256(raw).hexdigest() != item['sha256']:
                raise ValueError(f"WPT source hash mismatch: {item['path']}")
            write_atomic(path, raw)
        return [] if item['path'] in excluded else parse_dat(name, raw.decode('utf-8'))

    with ThreadPoolExecutor(max_workers=8) as workers:
        cases = [case for group in workers.map(fetch, source['files']) for case in group]
    value = {'source': source, 'cases': cases}
    validate_inventory(value, manifest)
    write_atomic(output, (json.dumps(value, ensure_ascii=False, indent=2) + '\n').encode())
    return output


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache', type=Path, default=DEFAULT_CACHE)
    args = parser.parse_args()
    path = ensure_suite(args.cache)
    print(f'Pinned WPT corpus ready: {path}')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
