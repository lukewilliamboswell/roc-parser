#!/usr/bin/env python3
"""Compare each format parser with its pinned reference implementation.

    check_conformance.py                 # the fast checks every pull request runs
    check_conformance.py yaml json       # chosen reviews
    check_conformance.py all             # every review, including the robustness campaigns
    check_conformance.py --list

The Python oracles are installed, at the versions their requirements files pin,
into one virtual environment under .roc-parser-tmp/, which is reused while
those files are unchanged. Each review builds its own Roc probe with `roc`
($ROC or `roc` on PATH). See docs/conformance.adoc.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

try:
    from ._common import ROOT
except ImportError:
    from _common import ROOT


VENV = ROOT / ".roc-parser-tmp" / "oracle-venv"


@dataclass(frozen=True)
class Review:
    command: tuple[str, ...]
    requirements: tuple[str, ...] = ()
    fast: bool = True


YAML = ("scripts/yaml/requirements.txt",)
MARKDOWN = ("scripts/markdown/requirements.txt",)
HTTP = ("scripts/http/requirements.txt",)

REVIEWS = {
    "yaml": Review(("scripts/review_yaml.py", "check", "--baseline", "scripts/yaml/known-failures.json"), YAML),
    "yaml-robustness": Review(("scripts/review_yaml.py", "campaign", "--smoke"), YAML, fast=False),
    "markdown-inline": Review(("scripts/review_markdown.py", "check"), MARKDOWN),
    "markdown-blocks": Review(
        ("scripts/review_markdown_blocks.py", "check", "--baseline", "scripts/markdown/blocks/known-failures.json"),
        MARKDOWN,
    ),
    "xml": Review(("scripts/review_xml.py", "check", "--baseline", "scripts/xml/known-failures.json")),
    "json": Review(("scripts/review_json.py", "check", "--baseline", "scripts/json/known-failures.json")),
    "json-robustness": Review(("scripts/review_json.py", "campaign", "--smoke"), fast=False),
    "csv": Review(("scripts/review_csv.py",)),
    "http": Review(("scripts/review_http.py",), HTTP),
}


def select(requested: Sequence[str]) -> list[str]:
    if not requested:
        return [name for name, review in REVIEWS.items() if review.fast]
    if list(requested) == ["all"]:
        return list(REVIEWS)
    unknown = sorted(set(requested) - set(REVIEWS))
    if unknown or "all" in requested:
        raise ValueError(f"unknown review(s): {', '.join(unknown or ['all'])}; choose from {', '.join(REVIEWS)}, or all alone")
    return list(dict.fromkeys(requested))


def requirements_for(names: Sequence[str]) -> list[str]:
    return sorted({path for name in names for path in REVIEWS[name].requirements})


def venv_python(venv: Path = VENV) -> Path:
    return venv / ("Scripts/python.exe" if os.name == "nt" else "bin/python")


def ensure_oracles(requirements: Sequence[str], venv: Path = VENV) -> Path:
    """A virtual environment holding exactly the pinned oracles, made once."""
    digest = hashlib.sha256()
    for path in requirements:
        digest.update(path.encode() + b"\0" + (ROOT / path).read_bytes() + b"\0")
    stamp = venv / "roc-parser-requirements.sha256"
    python = venv_python(venv)
    if python.is_file() and stamp.is_file() and stamp.read_text().strip() == digest.hexdigest():
        return python
    subprocess.run([sys.executable, "-m", "venv", "--clear", str(venv)], check=True)
    if requirements:
        install = [str(python), "-m", "pip", "install", "--quiet", "--disable-pip-version-check", "--no-deps"]
        for path in requirements:
            install += ["-r", str(ROOT / path)]
        print(f"+ {shlex.join(install)}", flush=True)
        subprocess.run(install, check=True, cwd=ROOT)
    stamp.write_text(digest.hexdigest() + "\n")
    return python


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("reviews", nargs="*", metavar="REVIEW", help="reviews to run (default: the fast checks)")
    parser.add_argument("--list", action="store_true", help="list the reviews and stop")
    args = parser.parse_args(argv)
    if args.list:
        for name, review in REVIEWS.items():
            print(f"{name:16} {'fast' if review.fast else 'scheduled':9} {shlex.join(review.command)}")
        return 0
    try:
        names = select(args.reviews)
    except ValueError as error:
        parser.error(str(error))

    # Every review is run with the requirements of all of them, so that one
    # environment serves them and is not rebuilt for a different selection.
    python = ensure_oracles(requirements_for(list(REVIEWS)))
    env = os.environ.copy()
    env["PATH"] = f"{python.parent}{os.pathsep}{env.get('PATH', '')}"
    failed = []
    for name in names:
        command = [str(python), *REVIEWS[name].command]
        print(f"\n== {name}: {shlex.join(REVIEWS[name].command)}", flush=True)
        started = time.monotonic()
        status = subprocess.run(command, cwd=ROOT, env=env, check=False).returncode
        print(f"== {name}: {'passed' if status == 0 else f'failed ({status})'} in {time.monotonic() - started:.1f}s", flush=True)
        if status:
            failed.append(name)
    if failed:
        print(f"conformance reviews failed: {', '.join(failed)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
