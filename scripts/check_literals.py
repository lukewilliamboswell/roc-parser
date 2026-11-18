#!/usr/bin/env python3
"""Check compiler quotation contracts, including failures and imported assets.

Fixtures contain Roc declarations; this runner supplies the app header and
demands their `value`. Positive fixtures define a Bool assertion. Negative
fixtures must fail check AND build with the intended diagnostic, never merely
with an unrelated compiler error. All generated sources and binaries are scratch.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

try:
    from ._common import ROOT, roc_command
except ImportError:
    from _common import ROOT, roc_command


FIXTURES = ROOT / "scripts" / "literals" / "cases.json"
PLATFORM = "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst"


def app_source(body: str) -> str:
    header = f'''app [main!] {{
    cli: platform "{PLATFORM}",
    parser: "{(ROOT / 'package' / 'main.roc').as_posix()}",
}}
import cli.Stdout
'''
    roots = ('Parser', 'Yaml', 'Xml', 'CSV', 'QIF', 'OFX', 'Markdown', 'AsciiDoc', 'HTTP', 'RocSyntax', 'JsonValue', 'Html', 'Svg', 'MathML', 'Markup')
    # Each fixture imports only the public owners it exercises. Unrelated
    # quotation hooks and format constructors need no per-application work.
    header += ''.join(f'import parser.{root}\n' for root in roots if re.search(rf'\b{root}\b', body))
    main = '''main! = |_args| Stdout.line!(Str.inspect(value))
'''
    return header + "\n" + body + "\n" + main


def assert_compiler_result(result: subprocess.CompletedProcess[str], errors: list[str]) -> None:
    output = (result.stdout or "") + (result.stderr or "")
    lower = " ".join(output.casefold().split())
    if result.returncode < 0 or any(marker in lower for marker in (
        "segmentation fault", "internal compiler error", "thread panic", "panic:",
    )):
        raise AssertionError(f"compiler crashed (exit {result.returncode}):\n{output}")
    if not errors:
        if result.returncode != 0:
            raise AssertionError(f"expected success, got {result.returncode}:\n{output}")
        return
    if result.returncode == 0:
        raise AssertionError(f"compiler accepted an invalid literal:\n{output}")
    missing = [fragment for fragment in errors if " ".join(fragment.casefold().split()) not in lower]
    if missing:
        raise AssertionError(f"missing intended diagnostics {missing}:\n{output}")


def invoke(command: list[str], directory: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=directory, text=True, capture_output=True, timeout=180)


def check_case(roc: str, case: dict, directory: Path) -> None:
    directory.mkdir()
    errors = case.get("errors", [])
    for name, content in case.get("assets", {}).items():
        path = directory / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    (directory / "main.roc").write_text(app_source(case["body"]), encoding="utf-8")
    executable = directory / ("app.exe" if os.name == "nt" else "app")
    for command in (
        [roc, "check", "main.roc"],
        [roc, "build", "--opt=dev", f"--output={executable}", "main.roc"],
    ):
        assert_compiler_result(invoke(command, directory), errors)
    if not errors:
        # An embedded file is a compile-time input, not a runtime dependency.
        for name in case.get("assets", {}):
            (directory / name).unlink()
        result = invoke([str(executable)], directory)
        if result.returncode != 0 or result.stdout != "True\n":
            raise AssertionError(f"compiled value mismatch:\n{result.stdout}{result.stderr}")


def check_invalidation(roc: str, directory: Path) -> None:
    directory.mkdir()
    source = '''import "settings.yaml" as text : Str
value = Parser.Quotation.require(Yaml.validate({ source: text, name: "settings.yaml" }))
'''
    (directory / "main.roc").write_text(app_source(source), encoding="utf-8")
    asset = directory / "settings.yaml"
    executable = directory / ("cached.exe" if os.name == "nt" else "cached")
    for content, errors in (
        ("port: 8080\n", []),
        ("port: [\n", ["compile time crash", "settings.yaml", "YAML"]),
        ("port: [\n", ["compile time crash", "settings.yaml", "YAML"]),
        ("port: 9090\n", []),
    ):
        asset.write_text(content, encoding="utf-8")
        for command in (
            [roc, "check", "main.roc"],
            [roc, "build", "--opt=dev", f"--output={executable}", "main.roc"],
        ):
            assert_compiler_result(invoke(command, directory), errors)
    asset.unlink()
    result = invoke([str(executable)], directory)
    if result.returncode != 0 or "9090" not in result.stdout or "8080" in result.stdout:
        raise AssertionError(f"cached build retained old imported data:\n{result.stdout}{result.stderr}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--filter", default="", help="select fixture names (also matches cache-invalidation)")
    parser.add_argument("--jobs", type=int, default=os.environ.get("ROC_PARSER_LITERAL_JOBS", "2"),
                        help="maximum concurrent compiler contracts (default: ROC_PARSER_LITERAL_JOBS or 2)")
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error("--jobs must be positive")
    cases = json.loads(FIXTURES.read_text(encoding="utf-8"))
    selected = [case for case in cases if args.filter in case["name"]]
    include_cache = args.filter in "cache-invalidation"
    if not selected and not include_cache:
        parser.error("no matching literal contracts")
    scratch = Path(os.environ.get("ROC_PARSER_TMPDIR", ROOT / ".roc-parser-tmp")).resolve()
    scratch.mkdir(parents=True, exist_ok=True)
    roc = roc_command()
    failures = []
    with tempfile.TemporaryDirectory(prefix="literals-", dir=scratch) as temporary:
        def run_case(case: dict) -> tuple[str, str | None]:
            try:
                check_case(roc, case, Path(temporary) / case["name"])
            except (AssertionError, subprocess.TimeoutExpired) as error:
                return case["name"], str(error)
            return case["name"], None

        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            for name, error in pool.map(run_case, selected):
                print(f"literal {name} ... {'FAILED' if error else 'ok'}", flush=True)
                if error:
                    failures.append(name)
                    print(error, file=sys.stderr)
        if include_cache:
            print("literal cache-invalidation ...", flush=True)
            try:
                check_invalidation(roc, Path(temporary) / "cache-invalidation")
            except (AssertionError, subprocess.TimeoutExpired) as error:
                failures.append("cache-invalidation")
                print(error, file=sys.stderr)
    if failures:
        print("Failed literal contracts: " + ", ".join(failures), file=sys.stderr)
        return 1
    print(f"All {len(selected) + int(include_cache)} literal contracts passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
