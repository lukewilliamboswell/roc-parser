#!/usr/bin/env python3
"""Build and run the roc-parser roc-fuzz targets."""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from dataclasses import dataclass
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import threading
from typing import Sequence


ROOT = Path(__file__).resolve().parents[1]
FUZZ_ROOT = ROOT / "fuzz"
WORK_ROOT = ROOT / ".roc-parser-tmp" / "fuzz"
ROC_FUZZ_RELEASE = {
    "version": "0.7.0-rc1",
    "commit": "ba275d7fa43b542ec967a570e5700fea3eb674e1",
    "bundle": "HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
    "sha256": "94130c93aa2abb204beb21f06338ecc39c5808e1358d905f219769ecf260e5f2",
}

# How reviewed JSON seeds become corpus entries:
# "fuzz-str" targets decode their input with Fuzz.str, "raw" targets read the
# UTF-8 bytes directly, and structured generators (None) start from an empty
# corpus because their bytes are choices rather than text.
SeedEncoding = str | None


@dataclass(frozen=True)
class TargetConfig:
    source: Path
    property_source: Path
    family: str
    seeds: Path | None
    seed_encoding: SeedEncoding
    dictionary: Path | None
    max_input_size: int = 4096
    timeout: int = 5
    # Whether the target declares examples, which `check` runs before
    # exploring; exploration alone only replays the corpus.
    examples: bool = False
    # New markup investigations are local-only; existing scheduled campaigns stay unchanged.
    scheduled: bool = True


def target(
    name: str,
    seeds: str | None = None,
    encoding: SeedEncoding = None,
    dictionary: str | None = None,
    max_input_size: int = 4096,
    timeout: int = 5,
    examples: bool = False,
    scheduled: bool = True,
    *,
    family: str,
) -> tuple[str, TargetConfig]:
    module = "".join(part.title() for part in name.split("-"))
    return name, TargetConfig(
        source=FUZZ_ROOT / f"{family}Properties.roc",
        property_source=FUZZ_ROOT / f"{module}Property.roc",
        family=family,
        seeds=FUZZ_ROOT / "seeds" / f"{seeds}.json" if seeds else None,
        seed_encoding=encoding,
        dictionary=FUZZ_ROOT / "dictionaries" / f"{dictionary}.dict" if dictionary else None,
        max_input_size=max_input_size,
        timeout=timeout,
        examples=examples,
        scheduled=scheduled,
    )


TARGETS = dict(
    [
        target("asciidoc-raw", "asciidoc", "raw", "asciidoc", family="Text"),
        target("asciidoc-structured", max_input_size=256, family="Text"),
        target("markdown-document", "markdown-document", "raw", "markdown-document", family="Text"),
        target("markdown-blocks", max_input_size=1024, family="Text"),
        target("markdown-refdefs", max_input_size=256, family="Text"),
        target("markdown-inline", "markdown-inline", "raw", "markdown-inline", family="Text"),
        target("markdown-inline-ast", max_input_size=1024, family="Text"),
        target("markdown-inline-pathological", "markdown-inline-pathological", "raw", max_input_size=64, timeout=10, family="Text"),
        target("yaml", "yaml", "fuzz-str", "yaml", family="Text"),
        target("yaml-raw", "yaml", "raw", "yaml", family="Text"),
        target("yaml-source", "yaml", "raw", "yaml", family="Text"),
        target("yaml-roundtrip", max_input_size=512, timeout=10, family="Text"),
        target("yaml-block", max_input_size=64, family="Text"),
        target("xml", "xml", "fuzz-str", "xml", family="Xml"),
        target("xml-raw", "xml", "raw", "xml", timeout=10, family="Xml"),
        target("xml-roundtrip", max_input_size=1024, family="Xml"),
        target("xml-malformed", max_input_size=1024, family="Xml"),
        target("xml-reader", "xml", "raw", "xml", timeout=10, family="Xml"),
        target("xml-stream", "xml-stream", "raw", "xml", max_input_size=2048, timeout=10, family="Xml"),
        target("xml-stream-ceilings", max_input_size=1, timeout=20, examples=True, family="Xml"),
        # The first three bytes choose the limits; the rest is the document.
        target("xml-limits", "xml-limits", "raw", "xml", max_input_size=1024, family="Xml"),
        # Two bytes choose a shape and a size; its examples hold the ceilings.
        target("xml-ceilings", "xml-ceilings", "raw", max_input_size=2, timeout=20, examples=True, family="Xml"),
        target("xmlns-raw", "xmlns", "raw", "xmlns", timeout=10, family="Xml"),
        target("xmlns-roundtrip", max_input_size=1024, timeout=10, family="Xml"),
        target("svg-path", max_input_size=64, timeout=10, scheduled=False, family="Markup"),
        target("svg-raw", "markup", "raw", "markup", timeout=10, scheduled=False, family="Markup"),
        target("mathml-raw", "markup", "raw", "markup", timeout=10, scheduled=False, family="Markup"),
        target("html-raw", "markup", "raw", "markup", timeout=10, scheduled=False, family="Markup"),
        target("markup-source", "markup", "raw", "markup", timeout=10, scheduled=False, family="Markup"),
        target("dom-construction", max_input_size=12, timeout=10, examples=True, scheduled=False, family="Markup"),
        target("markup-structured", max_input_size=128, timeout=10, scheduled=False, family="Markup"),
        target("markup-limits", max_input_size=8, timeout=10, scheduled=False, family="Markup"),
        target("markup-segments", max_input_size=256, timeout=10, examples=True, scheduled=False, family="Markup"),
        target("json-raw", "json", "raw", "json", timeout=10, family="Json"),
        # The first two bytes choose the options, so its seeds carry them.
        target("json-options", "json-options", "raw", "json", timeout=10, family="Json"),
        target("json-roundtrip", max_input_size=1024, timeout=10, family="Json"),
        target("json-stream", "json", "raw", "json", max_input_size=1024, timeout=10, examples=True, family="Json"),
        target("json-stream-ceilings", max_input_size=1, timeout=20, examples=True, family="Json"),
        target("ofx-raw", "ofx", "raw", "ofx", max_input_size=8192, timeout=10, family="Records"),
        target("ofx-stream", max_input_size=256, timeout=10, family="Records"),
        target("ofx-roundtrip", max_input_size=256, timeout=10, family="Records"),
        target("ofx-malformed", max_input_size=256, timeout=10, family="Records"),
        target("ofx-ceilings", max_input_size=1, timeout=20, examples=True, family="Records"),
        target("qif-raw", "qif", "raw", "qif", timeout=10, family="Records"),
        target("qif-stream", "qif", "raw", "qif", timeout=10, family="Records"),
        target("qif-roundtrip", max_input_size=256, timeout=10, family="Records"),
        target("qif-malformed", max_input_size=256, timeout=10, family="Records"),
        target("qif-ceilings", max_input_size=1, timeout=20, examples=True, family="Records"),
        target("stream-contract", max_input_size=256, family="Records"),
        target("stream-ceilings", max_input_size=1, examples=True, family="Records"),
        target("csv", "csv", "fuzz-str", "csv", family="Records"),
        target("csv-raw", "csv", "raw", "csv", timeout=10, family="Records"),
        target("csv-roundtrip", timeout=10, family="Records"),
        target("csv-decode", timeout=10, family="Records"),
        target("csv-stream-raw", "csv", "raw", "csv", timeout=10, examples=True, family="Records"),
        target("csv-stream-ceilings", max_input_size=1, timeout=20, examples=True, family="CsvRetention"),
        target("http-request", "http-request", "fuzz-str", "http-request", timeout=10, family="Http"),
        target("http-response", "http-response", "fuzz-str", "http-response", timeout=10, family="Http"),
        target("http-raw", "http-request", "raw", "http-raw", max_input_size=65536, timeout=10, family="Http"),
        target("http-roundtrip", timeout=10, family="Http"),
        target("http-smuggling", timeout=10, family="Http"),
        target("http-stream", max_input_size=1024, timeout=10, examples=True, family="Http"),
        target("http-stream-ceilings", max_input_size=1, timeout=20, examples=True, family="Http"),
        target("parser-combinators", max_input_size=256, examples=True, family="Core"),
        # The first byte chooses the format and the second an offset.
        target("validate", "validate", "raw", max_input_size=1024, timeout=10, family="Core"),
        target("diagnostic-lifetimes", max_input_size=2, timeout=20, examples=True, scheduled=False, family="Diagnostics"),
        target("string-primitives", max_input_size=256, timeout=10, family="Core"),
        target("byte-scan", max_input_size=256, timeout=10, family="Core"),
        target("roc-syntax-raw", "roc-syntax", "raw", "roc-syntax", max_input_size=4096, family="Core"),
        target("roc-syntax-structured", max_input_size=256, family="Core"),
        target("roc-syntax-spans", "roc-syntax", "raw", "roc-syntax", max_input_size=4096, family="Core"),
        target("roc-syntax-span-model", max_input_size=512, family="Core"),
    ]
)

FAMILIES = {
    family: tuple(name for name, config in TARGETS.items() if config.family == family)
    for family in dict.fromkeys(config.family for config in TARGETS.values())
}
TARGET_ORDER = tuple(TARGETS)
SCHEDULED_TARGET_ORDER = tuple(name for name, config in TARGETS.items() if config.scheduled)


def encode_fuzz_str(value: str) -> bytes:
    """Encode a reviewed seed for roc-fuzz's Fuzz.str generator.

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

    if config.seeds is None:
        return destination

    for value in load_seed_values(config.seeds):
        raw = encode_fuzz_str(value) if config.seed_encoding == "fuzz-str" else value.encode("utf-8")
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

    expanded = [member for requested_name in requested for member in FAMILIES.get(requested_name, (requested_name,))]
    unknown = sorted(set(expanded) - set(TARGETS))
    if unknown:
        raise ValueError(
            f"unknown fuzz target(s): {', '.join(unknown)}; "
            f"choose from {', '.join(TARGET_ORDER)}"
        )

    seen: set[str] = set()
    return [name for name in expanded if not (name in seen or seen.add(name))]


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
    return WORK_ROOT / "bin" / TARGETS[target_name].family


def run_directory(target_name: str) -> Path:
    return WORK_ROOT / "runs" / target_name


def build_flags(target_name: str) -> list[str]:
    flags = ["--fuzz"]
    if TARGETS[target_name].family == "Xml" and sys.platform == "linux" and os.uname().machine == "x86_64":
        # Cached XML suite lowering reproducibly SIGSEGVs in spec_constr on
        # nightly-2026-11-15-f25da45 (Actions run 44223623400, attempts 2/3).
        # Remove after its cached build and all XML properties pass on Linux
        # with the validation nightly. The underlying cause is uncertain.
        flags.append("--no-cache")
    return flags


def build_identity(target_name: str, roc: str, platform: Path | None = None) -> dict:
    config = TARGETS[target_name]
    digest = hashlib.sha256()
    for path in sorted({config.source, *FUZZ_ROOT.glob("*.roc"), *(ROOT / "package").glob("*.roc")}):
        digest.update(str(path.relative_to(ROOT)).encode())
        digest.update(path.read_bytes())
    if platform is not None:
        for path in sorted({*platform.parent.glob("*.roc"), *platform.parent.glob("targets/*/SHA256SUMS"), *platform.parent.glob("targets/*/libhost.a")}):
            digest.update(str(path).encode())
            digest.update(path.read_bytes())
    compiler_path = Path(shutil.which(roc) or roc)
    compiler_sha256 = hashlib.sha256(compiler_path.read_bytes()).hexdigest() if compiler_path.is_file() else None
    return {"source_sha256": digest.hexdigest(), "compiler": command_output([roc, "version"], ROOT), "compiler_sha256": compiler_sha256,
            "flags": build_flags(target_name), "roc_fuzz": ROC_FUZZ_RELEASE, "platform_override": str(platform.resolve()) if platform else None}


PRINT_LOCK = threading.Lock()


def build_target(target_name: str, roc: str, no_build: bool, *, capture: bool = False, platform: Path | None = None) -> tuple[int, Path]:
    """Build one target. With `capture`, the compiler's output is printed in
    one piece when it finishes, so that concurrent builds stay readable."""
    config = TARGETS[target_name]
    output = binary_path(target_name)

    manifest_path = output.with_suffix(".build.json")
    identity = build_identity(target_name, roc, platform)
    if no_build:
        if not output.is_file():
            print(
                f"error: --no-build requested but {output} does not exist",
                file=sys.stderr,
            )
            return 2, output
        try:
            saved = json.loads(manifest_path.read_text())
        except (OSError, ValueError):
            print("error: fuzz binary has no build identity; rebuild without --no-build", file=sys.stderr)
            return 2, output
        actual_hash = hashlib.sha256(output.read_bytes()).hexdigest()
        if saved.get("identity") != identity or saved.get("binary_sha256") != actual_hash:
            print("error: fuzz binary identity differs from source/compiler; rebuild without --no-build", file=sys.stderr)
            return 2, output
        output.chmod(output.stat().st_mode | 0o111)
        return 0, output

    output.parent.mkdir(parents=True, exist_ok=True)
    # Coverage instrumentation requires LLVM; ordinary CI builds use dev.
    command = [roc, "build", *build_flags(target_name), str(config.source), f"--output={output}"]
    if platform is not None:
        command.extend(["--replace-dep", f"https://github.com/lukewilliamboswell/roc-fuzz/releases/download/{ROC_FUZZ_RELEASE['version']}/{ROC_FUZZ_RELEASE['bundle']}", str(platform.resolve())])
    if not capture:
        print(f"+ {shlex.join(command)}", flush=True)
    try:
        completed = subprocess.run(
            command, cwd=ROOT, check=False, text=True,
            stdout=subprocess.PIPE if capture else None,
            stderr=subprocess.STDOUT if capture else None,
        )
    except OSError as problem:
        print(f"error: could not run Roc: {problem}", file=sys.stderr)
        return 2, output
    if capture:
        with PRINT_LOCK:
            print(f"+ {shlex.join(command)}\n{completed.stdout.rstrip()}", flush=True)
    if completed.returncode == 0:
        if identity != build_identity(target_name, roc, platform):
            print("error: fuzz sources changed while building; rebuild", file=sys.stderr)
            return 2, output
        manifest_path.write_text(json.dumps({"identity": identity, "binary_sha256": hashlib.sha256(output.read_bytes()).hexdigest()}, indent=2) + "\n")
    return completed.returncode, output


def print_failure_report(cwd: Path) -> None:
    """Show the failure itself; roc-fuzz keeps it in a report directory."""
    reports = sorted((cwd / ".roc-fuzz" / "reports").glob("*/report.json"), key=lambda path: path.stat().st_mtime)
    if not reports:
        return
    report = reports[-1].parent
    log = report / "run.log"
    if log.is_file():
        print(log.read_text(encoding="utf-8", errors="replace")[-4000:], file=sys.stderr)
    print(f"fuzz report: {report}", file=sys.stderr)
    original = report / "original-input.bin"
    if original.is_file():
        print(f"fuzz input: {original}", file=sys.stderr)


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
    platform: Path | None = None,
) -> int:
    build_status, executable = build_target(target_name, roc, no_build, platform=platform)
    if build_status != 0:
        return build_status

    cwd = run_directory(target_name)
    cwd.mkdir(parents=True, exist_ok=True)
    command = [str(executable), f"--target={target_name}", *arguments]
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
        print_failure_report(cwd)
        print(f"fuzz failure metadata: {metadata_path}", file=sys.stderr)
    return return_code


def build_targets(target_names: Sequence[str], roc: str, jobs: int, platform: Path | None = None) -> dict[str, int]:
    """Compile each family once; properties reuse its binary and build receipt."""
    representatives = {}
    for name in target_names:
        representatives.setdefault(TARGETS[name].family, name)
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        statuses = list(pool.map(lambda name: build_target(name, roc, False, capture=True, platform=platform)[0], representatives.values()))
    by_family = dict(zip(representatives, statuses))
    return {name: by_family[TARGETS[name].family] for name in target_names}


def run_fuzz_mode(args: argparse.Namespace) -> int:
    target_names = resolve_targets(args.targets)
    if getattr(args, "ci", False):
        target_names = [name for name in target_names if TARGETS[name].scheduled]
        if not target_names:
            raise ValueError("selection contains no CI-enabled targets")
    overall_status = 0

    prebuilt: dict[str, int] = {}
    if not args.no_build:
        prebuilt = build_targets(target_names, args.roc, args.jobs, args.platform)

    for target_name in target_names:
        if prebuilt.get(target_name, 0) != 0:
            print(f"error: {target_name} did not build", file=sys.stderr)
            overall_status = overall_status or prebuilt[target_name]
            continue
        config = TARGETS[target_name]
        corpus = prepare_corpus(target_name)
        if config.dictionary is not None and not config.dictionary.is_file():
            raise ValueError(f"missing fuzz dictionary: {config.dictionary}")

        options = [
            "explore",
            str(corpus),
            f"--max-input-size={args.max_input_size or config.max_input_size}",
            f"--timeout={args.timeout or config.timeout}",
            f"--memory-limit={args.memory_limit}",
        ]
        if config.dictionary is not None:
            options.append(f"--dictionary={config.dictionary}")
        if args.command == "smoke":
            options.append(f"--runs={args.runs}")
        else:
            options.append(f"--time={args.seconds}")

        no_build = args.no_build or target_name in prebuilt
        if config.examples:
            status = execute_target_command(target_name, args.roc, no_build, ["check", f"--timeout={args.timeout or config.timeout}"], args.platform)
            if status != 0:
                overall_status = overall_status or status
                continue
            no_build = True
        status = execute_target_command(
            target_name,
            args.roc,
            no_build,
            options,
            args.platform,
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
        args.platform,
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
    parser.add_argument("--platform", type=Path, help="local roc-fuzz platform entry point for upstream development")
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
    parser.add_argument(
        "--max-input-size", type=positive_int, help="override the target's input size bound"
    )
    parser.add_argument("--timeout", type=positive_int, help="override the target's per-input timeout")
    parser.add_argument("--memory-limit", type=positive_int, default=2048)
    parser.add_argument(
        "--jobs", type=positive_int, default=1,
        help="targets to build at once before running them in turn (default: 1)",
    )


def make_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    smoke = subparsers.add_parser("smoke", help="run a bounded iteration smoke test")
    smoke.add_argument("targets", nargs="*", metavar="TARGET", help="target(s), or all")
    smoke.add_argument("--runs", type=positive_int, default=2000)
    smoke.add_argument("--ci", action="store_true", help="run only targets enabled for existing CI/scheduled campaigns")
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

    build = subparsers.add_parser("build", help="compile shared family binaries without running properties")
    build.add_argument("targets", nargs="*", metavar="TARGET", help="property, family, or all")
    build.add_argument("--ci", action="store_true")
    add_limits(build)
    add_build_options(build)

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = make_argument_parser()
    args = parser.parse_args(argv)
    try:
        if args.command == "build":
            selected = resolve_targets(args.targets)
            if args.ci:
                selected = [name for name in selected if TARGETS[name].scheduled]
            statuses = build_targets(selected, args.roc, args.jobs, args.platform)
            return next((status for status in statuses.values() if status), 0)
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
