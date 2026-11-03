#!/usr/bin/env python3
"""Python virtualenv adapter for the pinned independent conformance oracles.

The Roc driver selects and runs reviews. This adapter owns Python's venv/pip
lifecycle and fingerprints the complete reviewed requirements set.
"""
from __future__ import annotations
import hashlib
import os
from pathlib import Path
import shlex
import subprocess
import sys
from typing import Sequence

ROOT = Path(__file__).resolve().parents[1]
VENV = ROOT / ".roc-parser-tmp/oracle-venv"

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


if __name__ == "__main__":
    print(ensure_oracles(sys.argv[1:]))
