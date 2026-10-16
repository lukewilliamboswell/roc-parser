#!/usr/bin/env python3
"""Build the parse-only oracle from immutable Roc Git objects, without LLVM.

Print only the binary path on stdout. Build diagnostics go to stderr.
The local --roc-repo is an object source: its working tree is never read.
"""
from __future__ import annotations

import argparse
import io
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import sys
import tarfile
import tempfile

ROC_REVISION = "0d4b0ab02f9239448d034db17b4e63051a7c57ae"
ZIG_VERSION = "0.17.0"
REPOSITORY = "https://github.com/roc-lang/roc.git"
ROOT = Path(__file__).resolve().parents[2]


def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess:
    return subprocess.run(args, check=True, **kwargs)


def extract_source(archive: bytes, destination: Path) -> None:
    """Accept only regular files/directories beneath src; reject traversal/links."""
    with tarfile.open(fileobj=io.BytesIO(archive), mode="r:") as bundle:
        members = bundle.getmembers()
        for member in members:
            path = PurePosixPath(member.name)
            if path.is_absolute() or ".." in path.parts or not path.parts or path.parts[0] != "src":
                raise ValueError(f"Unsafe source archive path: {member.name}")
            if not (member.isfile() or member.isdir()):
                raise ValueError(f"Unsupported source archive member: {member.name}")
        # The checks above also support Python versions without tarfile's data filter.
        for member in members:
            target = destination / member.name
            if member.isdir():
                target.mkdir(parents=True, exist_ok=True)
            else:
                target.parent.mkdir(parents=True, exist_ok=True)
                stream = bundle.extractfile(member)
                assert stream is not None
                with stream, target.open("wb") as out:
                    shutil.copyfileobj(stream, out)


def source_tree(cache: Path, local_repo: Path | None) -> Path:
    destination = cache / f"roc-{ROC_REVISION}"
    if local_repo is None:
        local_repo = cache / "upstream.git"
        if not local_repo.exists():
            run(["git", "init", "--bare", str(local_repo)], stdout=sys.stderr)
        run(["git", "-C", str(local_repo), "fetch", "--depth=1", REPOSITORY, ROC_REVISION], stdout=sys.stderr)
    actual = run(["git", "-C", str(local_repo), "rev-parse", f"{ROC_REVISION}^{{commit}}"], capture_output=True, text=True).stdout.strip()
    if actual != ROC_REVISION:
        raise ValueError(f"Unexpected compiler commit: {actual}")
    archive = run(["git", "-C", str(local_repo), "archive", ROC_REVISION, "src"], capture_output=True).stdout
    with tempfile.TemporaryDirectory(prefix="extract-", dir=cache) as tmp:
        staged = Path(tmp) / "source"
        staged.mkdir()
        extract_source(archive, staged)
        (staged / ".revision").write_text(ROC_REVISION + "\n")
        if destination.exists():
            shutil.rmtree(destination)
        staged.rename(destination)
    return destination


def has_pinned_revision(repository: Path) -> bool:
    result = subprocess.run(['git', '-C', str(repository), 'cat-file', '-e', ROC_REVISION + '^{commit}'],
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
    return result.returncode == 0


def select_git_source(explicit: Path | None, root: Path = ROOT) -> Path | None:
    """Select only existing trusted objects; fetching belongs to our own cache."""
    if explicit is not None:
        selected = explicit.resolve()
        if not has_pinned_revision(selected):
            raise ValueError(f'Compiler Git object source {selected} does not contain pinned revision {ROC_REVISION}; provide another source or omit --roc-repo to fetch into the isolated cache')
        return selected
    sibling = root.parent / 'roc'
    return sibling if (sibling / '.git').exists() and has_pinned_revision(sibling) else None

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc-repo", type=Path, help="Local Git object source (working tree ignored)")
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--cache-dir", type=Path, default=ROOT / ".roc-parser-tmp" / "syntax-oracle")
    args = parser.parse_args()
    version = run([args.zig, "version"], capture_output=True, text=True).stdout.strip()
    if version != ZIG_VERSION:
        parser.error(f"Zig {ZIG_VERSION} required; found {version}")
    cache = args.cache_dir.resolve()
    cache.mkdir(parents=True, exist_ok=True)
    try:
        local_repo = select_git_source(args.roc_repo)
    except ValueError as error:
        parser.error(str(error))
    source = source_tree(cache, local_repo.resolve() if local_repo else None)
    install = cache / "install"
    build_env = {**os.environ, "ZIG_GLOBAL_CACHE_DIR": str(cache / "zig-global-cache")}
    run([args.zig, "build", "--build-file", str(Path(__file__).parent / "oracle" / "build.zig"), f"-Droc-source={source}", "-Doptimize=ReleaseSafe", "--cache-dir", str(cache / "zig-cache"), "--prefix", str(install)], stdout=sys.stderr, env=build_env)
    binary = install / "bin" / "roc-syntax-oracle"
    worker_files = [Path(__file__), Path(__file__).parent / "oracle" / "main.zig", Path(__file__).parent / "oracle" / "build.zig"]
    worker_hash = hashlib.sha256()
    for path in worker_files:
        worker_hash.update(path.name.encode() + b"\0" + path.read_bytes() + b"\0")
    metadata = {
        "schema_version": 1,
        "compiler_revision": ROC_REVISION,
        "source_git": str(local_repo or cache / "upstream.git"),
        "zig_version": version,
        "optimize": "ReleaseSafe",
        "worker_sha256": worker_hash.hexdigest(),
        "binary_sha256": hashlib.sha256(binary.read_bytes()).hexdigest(),
    }
    binary.with_suffix(".json").write_text(json.dumps(metadata, indent=2) + "\n")
    print(binary)


if __name__ == "__main__":
    main()
