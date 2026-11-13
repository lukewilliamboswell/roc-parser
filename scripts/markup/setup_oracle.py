#!/usr/bin/env python3
"""Install reviewed markup oracle dependencies in ignored scratch storage."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / 'scripts/markup'
DEFAULT_CACHE = ROOT / '.roc-parser-tmp/markup/oracles'


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fetch_verified(url: str, expected: str, destination: Path) -> Path:
    if destination.is_file() and sha256(destination) == expected:
        return destination
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False) as handle:
        temporary = Path(handle.name)
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers={'User-Agent': 'roc-parser-oracle'}), timeout=120) as response:
            with temporary.open('wb') as output:
                shutil.copyfileobj(response, output)
        actual = sha256(temporary)
        if actual != expected:
            raise ValueError(f'oracle supplier digest mismatch: {url}: {actual} != {expected}')
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)
    return destination


def ensure_node(cache: Path = DEFAULT_CACHE) -> Path:
    source = DATA / 'oracle'
    node_root = cache / 'node'
    identity = hashlib.sha256((source / 'package.json').read_bytes() + (source / 'package-lock.json').read_bytes()).hexdigest()
    stamp = node_root / 'reviewed-lock.sha256'
    if not (stamp.is_file() and stamp.read_text().strip() == identity and (node_root / 'node_modules/parse5/dist/index.js').is_file()):
        node_root.mkdir(parents=True, exist_ok=True)
        for name in ['package.json', 'package-lock.json']:
            shutil.copyfile(source / name, node_root / name)
        result = subprocess.run(['npm', 'ci', '--ignore-scripts', '--no-audit', '--no-fund', '--prefix', str(node_root)], capture_output=True, text=True, check=False, timeout=180)
        (node_root / 'install.log').write_text(result.stdout + result.stderr)
        if result.returncode:
            raise RuntimeError(f'pinned Node oracle installation failed: {node_root / "install.log"}')
        stamp.write_text(identity + '\n')
    return node_root


def ensure_semantic(cache: Path = DEFAULT_CACHE) -> dict:
    manifest = json.loads((DATA / 'oracles.json').read_text())
    nu = manifest['nu']
    jar = fetch_verified(nu['url'], nu['sha256'], cache / 'nu' / (nu['sha256'] + '.jar'))
    schemas = cache / 'mathml' / manifest['mathml']['revision']
    for name, item in manifest['mathml']['files'].items():
        if Path(name).name != name:
            raise ValueError('schema inventory must use local basenames')
        fetch_verified(item['url'], item['sha256'], schemas / name)
    return {'nu': jar, 'schemas': schemas}


def ensure_semantic_python(cache: Path = DEFAULT_CACHE) -> Path:
    requirements = DATA / 'requirements.txt'
    identity = sha256(requirements)
    venv = cache / 'python'
    python = venv / ('Scripts/python.exe' if os.name == 'nt' else 'bin/python')
    stamp = venv / 'requirements.sha256'
    if not (python.is_file() and stamp.is_file() and stamp.read_text().strip() == identity):
        import sys
        subprocess.run([os.environ.get('MARKUP_PYTHON', sys.executable), '-m', 'venv', '--clear', str(venv)], check=True, timeout=60)
        subprocess.run([str(python), '-m', 'pip', 'install', '--disable-pip-version-check', '--only-binary=:all:', '--require-hashes', '--no-deps', '-r', str(requirements)], check=True, timeout=180)
        stamp.write_text(identity + '\n')
    return python


def ensure_browsers(engines: list[str], cache: Path = DEFAULT_CACHE, *, with_deps: bool = False) -> Path:
    node_root = ensure_node(cache)
    command = ['node', str(node_root / 'node_modules/playwright/cli.js'), 'install']
    if with_deps:
        command.append('--with-deps')
    command.extend(engines)
    environment = {**os.environ, 'PLAYWRIGHT_BROWSERS_PATH': str(cache / 'browsers')}
    subprocess.run(command, env=environment, check=True, timeout=600)
    return cache / 'browsers'


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache', type=Path, default=DEFAULT_CACHE)
    parser.add_argument('--semantic', action='store_true')
    parser.add_argument('--browsers', nargs='*', choices=['chromium', 'firefox', 'webkit'])
    parser.add_argument('--with-deps', action='store_true', help='install browser system packages on a provisioned CI host')
    args = parser.parse_args()
    result = {'node': str(ensure_node(args.cache))}
    if args.semantic:
        result.update({key: str(value) for key, value in ensure_semantic(args.cache).items()})
        result['python'] = str(ensure_semantic_python(args.cache))
    if args.browsers:
        result['browsers'] = str(ensure_browsers(args.browsers, args.cache, with_deps=args.with_deps))
    print(json.dumps(result))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
