#!/usr/bin/env python3
"""Run the examples and compare what they print with tests/examples.json.

    test_bundle_examples.py --checkout            # the repository's examples, on the working tree
    test_bundle_examples.py --checkout --update   # rewrite the expected output
    test_bundle_examples.py                       # release dry run, see below
    test_bundle_examples.py --bundle-path B --archive A --version V

Without --checkout this tests a release's examples archive the way a release
does. It bundles the package (or takes --bundle-path), builds the archive with
the bundle's real release URL for --version (or takes --archive), checks the
archive's layout and spec.json, and that its examples differ from the
repository's only in the `parser` dependency. It then serves the bundle on a
loopback address and checks, builds and runs every case with
`--replace-dep <release URL> <loopback URL>`, starting from an empty Roc
package cache. The archive's files are never changed.
"""
from __future__ import annotations

import argparse
import difflib
import functools
import http.server
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import zipfile
from pathlib import Path, PurePosixPath
from typing import Any, Sequence

try:
    from ._common import ROOT, display_command, roc_command
    from . import release_helpers, workflow_helpers
except ImportError:
    from _common import ROOT, display_command, roc_command
    import release_helpers
    import workflow_helpers


SPEC = ROOT / "tests" / "examples.json"
DRY_RUN_VERSION = "0.0.1-dry-run"
CASE_FIELDS = {"name": str, "args": list, "stdin": str, "stdout": str, "stderr": str, "exit_code": int}
CASE_TIMEOUT_SECONDS = 60


def run(command: Sequence[str], *, cwd: Path = ROOT, env: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    normalized = [str(part) for part in command]
    print("+", display_command(normalized), flush=True)
    completed = subprocess.run(normalized, cwd=cwd, env=env, text=True, capture_output=True, check=False)
    if completed.returncode != 0:
        if completed.stdout:
            print(completed.stdout)
        if completed.stderr:
            print(completed.stderr, file=sys.stderr)
        raise SystemExit(f"command failed with exit code {completed.returncode}: {display_command(normalized)}")
    return completed


# ------------------------------------------------------------------ the spec


def validate_apps(apps: Any, where: str) -> list[dict]:
    """The `apps` of a spec: unique paths under examples/, each with uniquely
    named cases whose every field is present."""
    if not isinstance(apps, list) or not apps:
        raise ValueError(f"{where}: apps must be a non-empty list")
    paths = set()
    for app in apps:
        path = app.get("path") if isinstance(app, dict) else None
        if not isinstance(path, str) or not re.fullmatch(r"examples/[A-Za-z0-9._/-]+\.roc", path) or ".." in path:
            raise ValueError(f"{where}: invalid app path {path!r}")
        if path in paths:
            raise ValueError(f"{where}: {path} is listed twice")
        paths.add(path)
        cases = app.get("cases")
        if not isinstance(cases, list) or not cases:
            raise ValueError(f"{where}: {path} has no cases")
        names = set()
        for case in cases:
            if not isinstance(case, dict) or set(case) != set(CASE_FIELDS):
                raise ValueError(f"{where}: {path} case must have exactly {sorted(CASE_FIELDS)}")
            for field, kind in CASE_FIELDS.items():
                if not isinstance(case[field], kind) or (kind is int and isinstance(case[field], bool)):
                    raise ValueError(f"{where}: {path} case {case.get('name')!r} field {field} must be {kind.__name__}")
            if not all(isinstance(argument, str) for argument in case["args"]):
                raise ValueError(f"{where}: {path} case {case['name']!r} args must be strings")
            if case["name"] in names:
                raise ValueError(f"{where}: {path} repeats case {case['name']!r}")
            names.add(case["name"])
    return apps


def load_spec(path: Path | None = None, examples_dir: Path = ROOT / "examples") -> list[dict]:
    """tests/examples.json, which must list every app in examples/."""
    path = path or SPEC
    spec = json.loads(path.read_text(encoding="utf-8"))
    apps = validate_apps(spec.get("apps") if isinstance(spec, dict) else None, str(path))
    listed = {app["path"] for app in apps}
    present = {f"examples/{app.name}" for app in examples_dir.glob("*.roc")}
    if listed != present:
        raise ValueError(f"{path} must list exactly the apps in examples/: "
                         f"missing {sorted(present - listed)}, unknown {sorted(listed - present)}")
    return apps


def run_case(binary: Path, case: dict, cwd: Path, env: dict[str, str] | None = None) -> dict:
    print("+", display_command([binary, *case["args"]]), flush=True)
    completed = subprocess.run([str(binary), *case["args"]], cwd=cwd, env=env, input=case["stdin"],
                               text=True, capture_output=True, timeout=CASE_TIMEOUT_SECONDS, check=False)
    return {**case, "stdout": completed.stdout, "stderr": completed.stderr, "exit_code": completed.returncode}


def differences(app: str, expected: dict, actual: dict) -> list[str]:
    problems = []
    for stream in ("stdout", "stderr"):
        if expected[stream] != actual[stream]:
            diff = difflib.unified_diff(expected[stream].splitlines(keepends=True), actual[stream].splitlines(keepends=True),
                                        f"expected {stream}", f"actual {stream}")
            problems.append(f"{app} [{expected['name']}] {stream} differs:\n{''.join(diff)}")
    if expected["exit_code"] != actual["exit_code"]:
        problems.append(f"{app} [{expected['name']}] exited {actual['exit_code']}, expected {expected['exit_code']}")
    return problems


def run_apps(apps: list[dict], root: Path, build_dir: Path, roc: str, roc_args: Sequence[str],
             env: dict[str, str] | None = None) -> tuple[list[dict], list[str]]:
    """Check, build and run each app under `root`; return the actual cases and
    every difference from the expected ones."""
    build_dir.mkdir(parents=True, exist_ok=True)
    actual_apps, problems = [], []
    for app in apps:
        source = root / app["path"]
        binary = build_dir / (source.stem + (".exe" if os.name == "nt" else ""))
        run([roc, "check", source.name, *roc_args], cwd=source.parent, env=env)
        run([roc, "build", source.name, f"--output={binary}", *roc_args], cwd=source.parent, env=env)
        cases = [run_case(binary, case, source.parent, env) for case in app["cases"]]
        for expected, actual in zip(app["cases"], cases):
            problems += differences(app["path"], expected, actual)
        actual_apps.append({"path": app["path"], "cases": cases})
    return actual_apps, problems


# ------------------------------------------------------------- the checkout


def checkout_examples(source_dir: Path = ROOT / "examples") -> list[Path]:
    """The examples in the checkout, which must use the checkout's package."""
    try:
        workflow_helpers.validate_examples(source_dir)
    except ValueError as error:
        raise SystemExit(str(error)) from error
    examples = sorted(source_dir.glob("*.roc"))
    if not examples:
        raise SystemExit("No examples found")
    return examples


def test_checkout(update: bool, tmp_dir: Path) -> int:
    checkout_examples()
    apps = load_spec()
    roc = roc_command()
    print("Testing the checkout's examples against the checkout's package")
    actual, problems = run_apps(apps, ROOT, tmp_dir / "build", roc, ["--no-cache"])
    if update:
        SPEC.write_text(json.dumps({"apps": actual}, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
        print(f"Wrote {SPEC.relative_to(ROOT)}")
        return 0
    if problems:
        print("\n".join(problems), file=sys.stderr)
        print("If the change is intended, run scripts/test_bundle_examples.py --checkout --update", file=sys.stderr)
        return 1
    print(f"{sum(len(app['cases']) for app in apps)} example cases match {SPEC.relative_to(ROOT)}")
    return 0


# -------------------------------------------------------------- the archive


def bundle_package(bundle_dir: Path) -> Path:
    completed = run([sys.executable, "scripts/bundle.py", "--output-dir", str(bundle_dir)])
    match = re.search(r"^Created:\s+(.+\.tar\.zst)\s*$", completed.stdout, re.MULTILINE)
    if match is None:
        raise SystemExit("Could not find bundle path in roc bundle output")
    # `roc bundle` runs in package/ and newer compilers print a path relative to it.
    bundle_path = (ROOT / "package" / match.group(1)).resolve()
    if not bundle_path.is_file():
        raise SystemExit(f"Bundle was not created: {bundle_path}")
    return bundle_path


def start_server(directory: Path) -> tuple[http.server.ThreadingHTTPServer, str]:
    handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=str(directory))
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    port = int(server.server_address[1])
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    return server, f"http://127.0.0.1:{port}"


def check_archive(archive: Path, version: str, bundle_url: str, examples_dir: Path = ROOT / "examples") -> list[dict]:
    """The canonical archive: one root named after the file, README.md,
    spec.json and examples/, whose files equal the repository's except for
    the `parser` dependency, which is `bundle_url`. Returns the spec's apps."""
    root = archive.name.removesuffix(".zip")
    with zipfile.ZipFile(archive) as bundle:
        names = [info.filename for info in bundle.infolist() if not info.is_dir()]
        for name in names:
            path = PurePosixPath(name)
            if path.parts[0] != root or ".." in path.parts or not re.fullmatch(r"[A-Za-z0-9._/-]+", name):
                raise ValueError(f"{archive.name}: member {name!r} is outside {root}/ or has an unsafe name")
        if len(set(names)) != len(names):
            raise ValueError(f"{archive.name}: repeated members")
        files = {str(PurePosixPath(name).relative_to(root)): bundle.read(name) for name in names}
    expected = {f"examples/{path.relative_to(examples_dir).as_posix()}"
                for path in examples_dir.rglob("*") if path.is_file()}
    if set(files) != expected | {"README.md", "spec.json"}:
        raise ValueError(f"{archive.name}: files differ from examples/ plus README.md and spec.json: "
                         f"{sorted(set(files) ^ (expected | {'README.md', 'spec.json'}))}")
    for name in sorted(expected):
        original = (examples_dir / name.removeprefix("examples/")).read_bytes()
        if name.endswith(".roc") and PurePosixPath(name).parent == PurePosixPath("examples"):
            original = release_helpers.rewrite_package_dependency(original.decode("utf-8"), bundle_url, name).encode("utf-8")
        if files[name] != original:
            raise ValueError(f"{archive.name}: {name} differs from the repository's beyond its parser dependency")
    spec = json.loads(files["spec.json"])
    if spec.get("schema_version") != 1 or spec.get("release") != version or spec.get("dependencies") != [bundle_url]:
        raise ValueError(f"{archive.name}: spec.json must have schema_version 1, release {version!r} "
                         f"and the one dependency {bundle_url!r}")
    if set(spec) != {"schema_version", "release", "dependencies", "apps"}:
        raise ValueError(f"{archive.name}: spec.json has unexpected fields {sorted(set(spec))}")
    apps = validate_apps(spec["apps"], f"{archive.name}/spec.json")
    if [app["path"] for app in apps] != [app["path"] for app in load_spec(examples_dir=examples_dir)]:
        raise ValueError(f"{archive.name}: spec.json lists other apps than tests/examples.json")
    return apps


def test_archive(bundle_path: Path | None, archive: Path | None, version: str, repo: str, tmp_dir: Path) -> int:
    roc = roc_command()
    bundle_dir = tmp_dir / "bundle"
    bundle_dir.mkdir()
    if bundle_path is None:
        bundle_path = bundle_package(bundle_dir)
    else:
        source = bundle_path.resolve()
        if not source.is_file():
            raise SystemExit(f"Bundle does not exist: {source}")
        bundle_path = bundle_dir / source.name
        shutil.copy2(source, bundle_path)

    bundle_url = release_helpers.release_asset_url(repo, version, bundle_path.name)
    if archive is None:
        archive = release_helpers.package_examples(ROOT / "examples", bundle_url, version, tmp_dir / release_helpers.asset_name("examples-zip", version), repo)
    try:
        apps = check_archive(archive, version, bundle_url)
    except (ValueError, KeyError, json.JSONDecodeError, zipfile.BadZipFile) as error:
        raise SystemExit(str(error)) from error
    with zipfile.ZipFile(archive) as bundle:
        bundle.extractall(tmp_dir / "archive")
    suite = tmp_dir / "archive" / archive.name.removesuffix(".zip")

    # An empty package cache, so the bundle is fetched from the loopback
    # address and verified against the hash in its name.
    env = os.environ.copy()
    env["XDG_CACHE_HOME"] = str(tmp_dir / "cache")
    server, base_url = start_server(bundle_dir)
    try:
        loopback = f"{base_url}/{bundle_path.name}"
        print(f"Testing {archive.name} with {bundle_url} served from {loopback}")
        _, problems = run_apps(apps, suite, tmp_dir / "build", roc, ["--replace-dep", bundle_url, loopback], env)
    finally:
        server.shutdown()
        server.server_close()
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    print(f"{sum(len(app['cases']) for app in apps)} cases in {archive.name} match its spec.json")
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--checkout", action="store_true",
                        help="test the repository's examples against the working tree (../package/main.roc)")
    parser.add_argument("--update", action="store_true", help="with --checkout, rewrite tests/examples.json")
    parser.add_argument("--bundle-path", type=Path, help="use an existing bundle instead of creating one")
    parser.add_argument("--archive", type=Path, help="test an existing examples archive instead of creating one")
    parser.add_argument("--version", default=DRY_RUN_VERSION, help=f"release version the archive names (default: {DRY_RUN_VERSION})")
    parser.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY", release_helpers.DEFAULT_REPO))
    args = parser.parse_args(argv)
    if args.checkout and (args.bundle_path or args.archive):
        parser.error("--checkout cannot be combined with --bundle-path or --archive")
    if args.update and not args.checkout:
        parser.error("--update needs --checkout")

    tmp_parent = Path(os.environ.get("ROC_PARSER_TMPDIR", ROOT / ".roc-parser-tmp")).resolve()
    tmp_parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="roc-parser-examples-", dir=tmp_parent) as tmp:
        if args.checkout:
            return test_checkout(args.update, Path(tmp))
        return test_archive(args.bundle_path, args.archive and args.archive.resolve(), args.version, args.repo, Path(tmp))


if __name__ == "__main__":
    raise SystemExit(main())
