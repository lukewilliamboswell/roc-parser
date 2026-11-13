#!/usr/bin/env python3
"""Vendor the pinned WPT/html5lib parser-only tree construction corpus."""
from __future__ import annotations
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
from pathlib import Path
import urllib.request

REVISION = '4aa56d3beafc5655fa55491169eb14845b44eab3'
PREFIX = 'html/syntax/parsing/resources/'
BASE = f'https://raw.githubusercontent.com/web-platform-tests/wpt/{REVISION}/'
DATA = Path(__file__).resolve().parent
SECTIONS = {'#description', '#data', '#errors', '#new-errors', '#document-fragment', '#script-on', '#script-off', '#document'}


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
                'expected_error_count': len([line for line in current.get('#errors', []) + current.get('#new-errors', []) if line.strip()]),
                'scripting': '#script-on' in current, 'scripting_modes': [True] if '#script-on' in current else [False] if '#script-off' in current else [False, True], 'include_bindings': True}
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


def main() -> int:
    listing = json.loads(download(f'https://api.github.com/repos/web-platform-tests/wpt/contents/{PREFIX}?ref={REVISION}'))
    names = sorted(item['name'] for item in listing if item['name'].endswith('.dat'))
    sources, cases, excluded = [], [], []
    def fetch(name):
        return name, download(BASE + PREFIX + name)
    for name, raw in ThreadPoolExecutor(max_workers=8).map(fetch, names):
        sources.append({'path': PREFIX + name, 'sha256': hashlib.sha256(raw).hexdigest()})
        if name.startswith('scripted_'):
            excluded.append({'path': PREFIX + name, 'reason': 'Expected output requires JavaScript execution and DOM mutation during parsing; embedded JavaScript execution is outside this pure parser contract.'})
            continue
        cases.extend(parse_dat(name, raw.decode('utf-8')))
    license_bytes = download(BASE + 'LICENSE.md')
    (DATA / 'WPT-LICENSE.md').write_bytes(license_bytes)
    value = {'source': {'repository': 'https://github.com/web-platform-tests/wpt', 'revision': REVISION,
                        'upstream_format': BASE + PREFIX + 'README.md', 'files': sources,
                        'license': {'path': 'LICENSE.md', 'sha256': hashlib.sha256(license_bytes).hexdigest()}, 'excluded': excluded}, 'cases': cases}
    (DATA / 'tree-suite.json').write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')
    print(f'vendored {len(cases)} parser-only fixtures from {len(sources)} source files; {len(excluded)} JavaScript-executing files recorded separately')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
