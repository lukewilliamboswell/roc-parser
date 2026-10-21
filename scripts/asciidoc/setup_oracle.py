#!/usr/bin/env python3
"""Install the hash-pinned, dependency-free Asciidoctor oracle locally."""
import argparse
import hashlib
import os
from pathlib import Path
import subprocess
import urllib.request

VERSION = '2.0.23'
SHA256 = '52208807f237dfa0ca29882f8b13d60b820496116ad191cf197ca56f2b7fddf3'
ROOT = Path(__file__).resolve().parents[2]
DEFAULT_HOME = ROOT / '.roc-parser-tmp/asciidoc/gems'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--gem-home', type=Path, default=DEFAULT_HOME)
    parser.add_argument('--gem-file', type=Path, help='Use an already downloaded gem (offline setup)')
    args = parser.parse_args()
    archive = args.gem_file or args.gem_home.parent / f'asciidoctor-{VERSION}.gem'
    archive.parent.mkdir(parents=True, exist_ok=True)
    if not archive.exists():
        with urllib.request.urlopen(f'https://rubygems.org/downloads/asciidoctor-{VERSION}.gem', timeout=30) as response:
            archive.write_bytes(response.read(2_000_000))
    if hashlib.sha256(archive.read_bytes()).hexdigest() != SHA256:
        raise SystemExit('Asciidoctor gem SHA-256 mismatch')
    env = {key: value for key, value in os.environ.items() if key not in ('RUBYOPT', 'RUBYLIB', 'GEM_HOME', 'GEM_PATH')}
    subprocess.run(['gem', 'install', '--local', str(archive.resolve()), '--install-dir', str(args.gem_home.resolve()), '--no-document', '--ignore-dependencies'], env=env, check=True)
    print(f'Asciidoctor {VERSION}; GEM_HOME={args.gem_home.resolve()}')


if __name__ == '__main__':
    main()
