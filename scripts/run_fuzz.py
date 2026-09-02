#!/usr/bin/env python3
"""Build and run the roc-parser roc-fuzz targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from dataclasses import dataclass
from pathlib import Path
import shlex
import subprocess
import sys
from typing import Sequence


ROOT = Path(__file__).resolve().parents[1]
FUZZ_ROOT = ROOT / "fuzz"
WORK_ROOT = ROOT / ".roc-parser-tmp" / "fuzz"
ROC_FUZZ_RELEASE = {
    "version": "0.3.0",
    "commit": "ec137edcf0fa2530e3dbb175fec4ddff5281cc6d",
    "bundle": "FTcKnkDxL1ZXfKsxeLmNKZ6XKnuKDd47Gv79ThxLYSfw.tar.zst",
    "sha256": "f9be31a5d7f0ba2e7e13ec804e6827513af9e9e548137d780a904fdbb5793ee5",
}


@dataclass(frozen=True)
class TargetConfig:
    source: Path
    seeds: Path
    dictionary: Path


TARGET_ORDER = (
    "markdown-document",
    "markdown-inline",
    "yaml",
    "xml",
    "csv",
    "http-request",
    "http-response",
)

TARGETS = {
    name: TargetConfig(
        source=FUZZ_ROOT / f"{name}.roc",
        seeds=FUZZ_ROOT / "seeds" / f"{name}.json",
        dictionary=FUZZ_ROOT / "dictionaries" / f"{name}.dict",
    )
    for name in TARGET_ORDER
}


def encode_fuzz_str(value: str) -> bytes:
    """Encode a reviewed seed for roc-fuzz 0.3.0's Fuzz.str generator.

    For inputs of at most 255 bytes, Arbitrary.arbitrary_str consumes one
    trailing byte to choose the string length. Setting it to the UTF-8 byte
    length makes the generator select the complete preceding string.
    """

    encoded = value.encode("utf-8")
    if len(encoded) > 255:
        raise ValueError(
            "Fuzz.str seed exceeds the supported 255-byte reviewable seed limit"
        )
    return encoded + bytes([len(encoded)])


def load_seed_values(path: Path) -> list[str]:
    try:
        values = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as problem:
        raise ValueError(f"could not read seeds from {path}: {problem}") from problem

    if not isinstance(values, list) or any(not isinstance(item, str) for item in values):
        raise ValueError(f"{path} must contain a JSON array of strings")
    return values


def prepare_corpus(target_name: str, corpus_dir: Path | None = None) -> Path:
    config = TARGETS[target_name]
    destination = corpus_dir or WORK_ROOT / "corpus" / target_name
    destination.mkdir(parents=True, exist_ok=True)

    for value in load_seed_values(config.seeds):
        raw = encode_fuzz_str(value)
        digest = hashlib.sha256(raw).hexdigest()
        seed_path = destination / f"seed-{digest}"
        if not seed_path.exists():
            seed_path.write_bytes(raw)

    return destination


def resolve_targets(requested: Sequence[str]) -> list[str]:
    if not requested or list(requested) == ["all"]:
        return list(TARGET_ORDER)
    if "all" in requested:
        raise ValueError("`all` cannot be combined with individual target names")

    unknown = sorted(set(requested) - set(TARGETS))
    if unknown:
        raise ValueError(
            f"unknown fuzz target(s): {', '.join(unknown)}; "
            f"choose from {', '.join(TARGET_ORDER)}"
        )

    seen: set[str] = set()
    return [name for name in requested if not (name in seen or seen.add(name))]


def command_output(command: Sequence[str], cwd: Path) -> str:
    try:
        completed = subprocess.run(
            command,
            cwd=cwd,
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError as problem:
        return f"unavailable: {problem}"

    output = completed.stdout.strip() or completed.stderr.strip()
    return output or f"command exited with status {completed.returncode}"


def binary_path(target_name: str) -> Path:
    return WORK_ROOT / "bin" / target_name


def run_directory(target_name: str) -> Path:
    return WORK_ROOT / "runs" / target_name


def build_target(target_name: str, roc: str, no_build: bool) -> tuple[int, Path]:
    config = TARGETS[target_name]
    output = binary_path(target_name)

    if no_build:
        if not output.is_file():
            print(
                f"error: --no-build requested but {output} does not exist",
                file=sys.stderr,
            )
            return 2, output
        return 0, output

    output.parent.mkdir(parents=True, exist_ok=True)
    command = [roc, "build", "--fuzz", str(config.source), f"--output={output}"]
    print(f"+ {shlex.join(command)}", flush=True)
    try:
        completed = subprocess.run(command, cwd=ROOT, check=False)
    except OSError as problem:
        print(f"error: could not run Roc: {problem}", file=sys.stderr)
        return 2, output
    return completed.returncode, output


def write_metadata(
    target_name: str,
    roc: str,
    command: Sequence[str],
    status: str,
    return_code: int | None,
) -> Path:
    destination = run_directory(target_name)
    destination.mkdir(parents=True, exist_ok=True)
    metadata_path = destination / "metadata.json"
    metadata = {
        "target": target_name,
        "status": status,
        "return_code": return_code,
        "command": list(command),
        "command_display": shlex.join(command),
        "git_commit": command_output(["git", "rev-parse", "HEAD"], ROOT),
        "roc_version": command_output([roc, "version"], ROOT),
        "roc_fuzz": ROC_FUZZ_RELEASE,
    }
    metadata_path.write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return metadata_path


def execute_target_command(
    target_name: str,
    roc: str,
    no_build: bool,
    arguments: Sequence[str],
) -> int:
    build_status, executable = build_target(target_name, roc, no_build)
    if build_status != 0:
        return build_status

    cwd = run_directory(target_name)
    cwd.mkdir(parents=True, exist_ok=True)
    command = [str(executable), *arguments]
    metadata_path = write_metadata(target_name, roc, command, "running", None)
    print(f"+ {shlex.join(command)}", flush=True)
    print(f"  working directory: {cwd}", flush=True)

    try:
        completed = subprocess.run(command, cwd=cwd, check=False)
        return_code = completed.returncode
    except OSError as problem:
        print(f"error: could not run fuzz target: {problem}", file=sys.stderr)
        return_code = 2

    write_metadata(target_name, roc, command, "complete", return_code)
    if return_code != 0:
        print(f"fuzz failure metadata: {metadata_path}", file=sys.stderr)
        print(f"fuzz artifacts: {cwd / '.roc-fuzz'}", file=sys.stderr)
    return return_code


def run_fuzz_mode(args: argparse.Namespace) -> int:
    target_names = resolve_targets(args.targets)
    overall_status = 0

    for target_name in target_names:
        config = TARGETS[target_name]
        corpus = prepare_corpus(target_name)
        if not config.dictionary.is_file():
            raise ValueError(f"missing fuzz dictionary: {config.dictionary}")

        options = [
            "run",
            str(corpus),
            f"--max-input-size={args.max_input_size}",
            f"--timeout={args.timeout}",
            f"--memory-limit={args.memory_limit}",
            f"--dictionary={config.dictionary}",
            "--print-final-stats",
        ]
        if args.command == "smoke":
            options.append(f"--runs={args.runs}")
        else:
            options.append(f"--time={args.seconds}")

        status = execute_target_command(
            target_name,
            args.roc,
            args.no_build,
            options,
        )
        if status != 0 and overall_status == 0:
            overall_status = status

    return overall_status


def resolve_input(path_text: str) -> Path:
    path = Path(path_text).expanduser()
    if not path.is_absolute():
        path = Path.cwd() / path
    path = path.resolve()
    if not path.is_file():
        raise ValueError(f"fuzz input does not exist: {path}")
    return path


def run_artifact_command(args: argparse.Namespace) -> int:
    input_path = resolve_input(args.input)
    arguments = [args.command, str(input_path)]

    if args.command == "minimize":
        output_path = Path(args.output).expanduser()
        if not output_path.is_absolute():
            output_path = Path.cwd() / output_path
        output_path = output_path.resolve()
        if output_path.exists():
            raise ValueError(f"refusing to overwrite minimize output: {output_path}")
        output_path.parent.mkdir(parents=True, exist_ok=True)
        arguments.append(str(output_path))

    return execute_target_command(
        args.target,
        args.roc,
        args.no_build,
        arguments,
    )


def positive_int(value: str) -> int:
    try:
        number = int(value)
    except ValueError as problem:
        raise argparse.ArgumentTypeError("must be an integer") from problem
    if number <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return number


def add_build_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--roc",
        default=os.environ.get("ROC", "roc"),
        help="Roc compiler executable (default: $ROC or roc)",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="reuse an existing target binary under .roc-parser-tmp/fuzz/bin",
    )


def add_limits(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--max-input-size", type=positive_int, default=4096)
    parser.add_argument("--timeout", type=positive_int, default=5)
    parser.add_argument("--memory-limit", type=positive_int, default=2048)


def make_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    smoke = subparsers.add_parser("smoke", help="run a bounded iteration smoke test")
    smoke.add_argument("targets", nargs="*", metavar="TARGET", help="target(s), or all")
    smoke.add_argument("--runs", type=positive_int, default=2000)
    add_limits(smoke)
    add_build_options(smoke)

    campaign = subparsers.add_parser("campaign", help="run a time-bounded campaign")
    campaign.add_argument("targets", nargs="*", metavar="TARGET", help="target(s), or all")
    campaign.add_argument("--seconds", type=positive_int, default=600)
    add_limits(campaign)
    add_build_options(campaign)

    for command in ("show", "replay"):
        operation = subparsers.add_parser(command, help=f"{command} one saved fuzz input")
        operation.add_argument("target", choices=TARGET_ORDER)
        operation.add_argument("input")
        add_build_options(operation)

    minimize = subparsers.add_parser("minimize", help="minimize one saved failure")
    minimize.add_argument("target", choices=TARGET_ORDER)
    minimize.add_argument("input")
    minimize.add_argument("output")
    add_build_options(minimize)

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = make_argument_parser()
    args = parser.parse_args(argv)
    try:
        if args.command in ("smoke", "campaign"):
            return run_fuzz_mode(args)
        return run_artifact_command(args)
    except ValueError as problem:
        parser.error(str(problem))
    except KeyboardInterrupt:
        print("interrupted", file=sys.stderr)
        return 130
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
