#!/usr/bin/env python3
"""Check package/consumer compatibility with an identified nightly compiler."""
from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[1]


def gh_json(endpoint: str):
    return json.loads(subprocess.check_output(["gh", "api", endpoint], text=True))


def nightly_revision(tag: str) -> str:
    match = re.fullmatch(r"nightly-\d{4}-\d{2}-\d{2}-([0-9a-f]{7,40})", tag)
    if not match:
        raise ValueError(f"Unexpected nightly tag: {tag!r}")
    return match[1]


def assert_clean(result: subprocess.CompletedProcess[str]) -> None:
    output = (result.stdout or "") + (result.stderr or "")
    if result.returncode != 0 or re.search(
        r"thread .*panic|checked artifact invariant violated|segmentation fault|internal compiler error|\b[1-9][0-9]* warnings?\b",
        output, re.IGNORECASE,
    ):
        raise AssertionError(f"Compiler command did not complete cleanly (exit {result.returncode}):\n{output}")


def resolve() -> None:
    tag = gh_json("repos/roc-lang/nightlies/releases/latest")["tag_name"]
    short = nightly_revision(tag)
    revision = gh_json(f"repos/roc-lang/roc/commits/{short}")["sha"]
    if not re.fullmatch(r"[0-9a-f]{40}", revision) or not revision.startswith(short):
        raise ValueError("Nightly tag and source revision disagree")
    encoded = gh_json(f"repos/roc-lang/roc/contents/build.zig.zon?ref={revision}")["content"]
    zon = base64.b64decode(encoded).decode()
    match = re.search(r'\.minimum_zig_version\s*=\s*"([^"]+)"', zon)
    if not match or not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+(?:-dev\.[0-9]+\+[0-9a-f]+)?", match[1]):
        raise ValueError("Cannot identify the compiler's minimum Zig version")
    outputs = {"nightly": tag, "revision": revision, "zig": match[1]}
    if os.environ.get("GITHUB_OUTPUT"):
        with Path(os.environ["GITHUB_OUTPUT"]).open("a") as stream:
            for key, value in outputs.items():
                stream.write(f"{key}={value}\n")
    print(json.dumps(outputs, indent=2))


def source_identity() -> dict[str, str]:
    files = [*(ROOT / "package").glob("*.roc"), *(ROOT / "examples").rglob("*")]
    return {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(files) if path.is_file()}


def check(args) -> None:
    short = nightly_revision(args.nightly)
    if not re.fullmatch(r"[0-9a-f]{40}", args.revision) or not args.revision.startswith(short):
        raise ValueError("Nightly and expected source revision disagree")
    machine = platform.machine()
    if args.expect_machine and machine != args.expect_machine:
        raise ValueError(f"Expected {args.expect_machine}, got {machine}")
    compiler = Path(shutil.which(args.roc) or args.roc).resolve()
    if args.build_mode == "ReleaseSafe":
        if args.source is None:
            raise ValueError("ReleaseSafe validation requires an identified source checkout")
        source = args.source.resolve()
        actual = subprocess.check_output(["git", "-C", str(source), "rev-parse", "HEAD"], text=True).strip()
        if actual != args.revision:
            raise ValueError("Compiler checkout is not the selected nightly revision")
        subprocess.run(["git", "-C", str(source), "diff", "--exit-code", "HEAD"], check=True)
    args.output.mkdir(parents=True, exist_ok=True)
    rows = []
    commands = [["version"], ["check", "--no-cache", "package/main.roc"],
                ["test", "--no-cache", "package/main.roc"],
                ["check", "--no-cache", "examples/compile-time-parsing.roc"],
                ["check", "--no-cache", "examples/dom-construction.roc"],
                ["check", "--no-cache", "examples/markup-literals.roc"],
                ["check", "--no-cache", "examples/roc-syntax.roc"]]
    identity = source_identity()
    receipt = {"nightly": args.nightly, "source_revision": args.revision,
               "build_mode": args.build_mode, "system": platform.system(), "machine": machine,
               "compiler": str(compiler), "compiler_sha256": hashlib.sha256(compiler.read_bytes()).hexdigest(),
               "package_commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
               "source_sha256": identity, "commands": rows}
    try:
        for index, command in enumerate(commands):
            print("+", compiler, *command, flush=True)
            result = subprocess.run([str(compiler), *command], cwd=ROOT, text=True,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=1200)
            print(result.stdout, end="", flush=True)
            path = args.output / f"{index}.log"
            path.write_text(result.stdout)
            rows.append({"arguments": command, "return_code": result.returncode, "log": str(path)})
            assert_clean(result)
            if index == 0:
                receipt["version"] = result.stdout.strip()
                if args.build_mode == "Official" and args.nightly not in result.stdout:
                    raise ValueError("Official compiler version differs from the selected nightly")
        if identity != source_identity():
            raise ValueError("Package or consumer source changed during validation")
        receipt["passed"] = True
    finally:
        (args.output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_subparsers(dest="command", required=True)
    modes.add_parser("resolve")
    checks = modes.add_parser("check")
    checks.add_argument("--roc", default="roc")
    checks.add_argument("--nightly", required=True)
    checks.add_argument("--revision", required=True)
    checks.add_argument("--build-mode", choices=["Official", "ReleaseSafe"], required=True)
    checks.add_argument("--source", type=Path)
    checks.add_argument("--expect-machine")
    checks.add_argument("--output", type=Path, default=ROOT / ".roc-parser-tmp" / "nightly-compatibility")
    args = parser.parse_args()
    if args.command == "resolve":
        resolve()
    else:
        check(args)


if __name__ == "__main__":
    main()
