#!/usr/bin/env python3
"""Build and validate Roc syntax correctness, fuzz properties and performance.

All artifacts live under .roc-parser-tmp/roc-syntax-validation by default.
Each run rebuilds its probes; a prior executable is never silently reused.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import shlex
import subprocess
import sys
import time

try:
    from ._common import ROOT, resolve_command
except ImportError:
    from _common import ROOT, resolve_command


def positive(value: str) -> int:
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be positive")
    return number


def positive_ratio(value: str) -> float:
    number = float(value)
    if not math.isfinite(number) or number <= 0:
        raise argparse.ArgumentTypeError("must be finite and positive")
    return number


def source_identity(root: Path) -> str:
    """Hash every source file affecting the candidate and its build drivers."""
    digest = hashlib.sha256()
    paths = sorted({*root.joinpath("package").glob("*.roc"),
                    *root.joinpath("scripts/roc_syntax").rglob("*.roc"),
                    root / "bench/roc/roc-syntax.roc", root / "bench/roc/roc-syntax-alloc.roc",
                    root / ".roc-version"})
    for path in paths:
        digest.update(path.relative_to(root).as_posix().encode() + b"\0")
        digest.update(path.read_bytes() + b"\0")
    return digest.hexdigest()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--roc-source", type=Path, help="optional compiler Git object source")
    parser.add_argument("--output", type=Path, default=ROOT / ".roc-parser-tmp/roc-syntax-validation")
    parser.add_argument("--generated", type=positive, default=1000)
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--mutation-runs", type=positive, default=1000)
    parser.add_argument("--expression-depth", type=int, choices=range(9), default=5)
    parser.add_argument("--pattern-depth", type=int, choices=range(7), default=3)
    parser.add_argument("--type-depth", type=int, choices=range(7), default=4)
    parser.add_argument("--statements", type=int, choices=range(1, 17), default=4)
    parser.add_argument("--fuzz-runs", type=positive, default=2000)
    parser.add_argument("--skip-fuzz", action="store_true", help="omit campaigns already run separately")
    parser.add_argument("--allocations", action="store_true", help="also build/run the allocation probe")
    parser.add_argument("--reference-performance", choices=("smoke", "full"),
                        help="optional compiler speed comparison; smoke checks protocol only, full applies the local ratio ratchet")
    parser.add_argument("--max-reference-ratio", type=positive_ratio, default=1.5)
    parser.add_argument("--baseline", type=Path, help="performance baseline from this machine/compiler")
    args = parser.parse_args(argv)
    roc = resolve_command(args.roc)
    zig = resolve_command(args.zig)
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env["ROC"] = roc
    env.setdefault("ROC_CACHE_DIR", str(output / "roc-cache"))
    manifest = {"schema": 1, "started": time.time(), "source_sha256": source_identity(ROOT),
                "generated": args.generated, "seed": args.seed, "mutation_runs": args.mutation_runs, "fuzz_runs": args.fuzz_runs,
                "fuzz_omitted": args.skip_fuzz, "allocations": args.allocations, "steps": []}
    manifest["generation_budgets"] = {key: getattr(args, key) for key in
                                      ("expression_depth", "pattern_depth", "type_depth", "statements")}
    manifest["reference_performance"] = args.reference_performance
    manifest["max_reference_ratio"] = args.max_reference_ratio

    def save() -> None:
        (output / "validation.json").write_text(json.dumps(manifest, indent=2) + "\n")

    def step(name: str, command: list[str], *, extra_env: dict | None = None) -> subprocess.CompletedProcess:
        print(f"{name}: {shlex.join(command)}", flush=True)
        started = time.monotonic()
        result = subprocess.run(command, cwd=ROOT, env={**env, **(extra_env or {})},
                                capture_output=True, text=True)
        (output / f"{name}.stdout").write_text(result.stdout)
        (output / f"{name}.stderr").write_text(result.stderr)
        manifest["steps"].append({"name": name, "command": command,
                                  "elapsed_seconds": time.monotonic() - started,
                                  "returncode": result.returncode})
        save()
        if result.returncode:
            print(result.stdout, end="", file=sys.stderr)
            print(result.stderr, end="", file=sys.stderr)
            raise subprocess.CalledProcessError(result.returncode, command)
        return result

    save()
    try:
        version = step("compiler-version", [roc, "version"]).stdout.strip()
        expected = "Roc compiler version " + (ROOT / ".roc-version").read_text().strip()
        if version != expected:
            raise ValueError(f"Expected {expected}; found {version}. Set ROC to the pinned executable.")
        manifest["compiler"] = version
        step("package-check", [roc, "check", "package/main.roc"])
        step("package-tests", [roc, "test", "package/main.roc"])
        command = [sys.executable, "scripts/roc_syntax/build_oracle.py", "--zig", zig,
                   "--cache-dir", str(output / "oracle")]
        if args.roc_source:
            command += ["--roc-repo", str(args.roc_source.resolve())]
        oracle = Path(step("oracle-build", command).stdout.strip())
        probe = output / "probe"
        step("probe-build", [roc, "build", "--opt=speed", "--no-cache", "scripts/roc_syntax/probe.roc", f"--output={probe}"])
        manifest["binary_sha256"] = {name: hashlib.sha256(path.read_bytes()).hexdigest()
                                     for name, path in [("probe", probe), ("oracle", oracle)]}
        metadata = oracle.with_suffix(".json")
        manifest["oracle_build"] = json.loads(metadata.read_text())
        if manifest["oracle_build"]["binary_sha256"] != manifest["binary_sha256"]["oracle"]:
            raise ValueError("Oracle executable does not match its build metadata.")
        step("oracle-contract", [sys.executable, "-m", "unittest", "discover", "-s",
                                 "scripts/roc_syntax/oracle", "-p", "test_*.py"],
             extra_env={"ORACLE_BINARY": str(oracle)})
        step("differential", [sys.executable, "scripts/review_roc_syntax.py", "--probe-command", shlex.quote(str(probe)),
                              "--oracle-command", shlex.quote(str(oracle)), "--generated", str(args.generated),
                              "--seed", str(args.seed), "--expression-depth", str(args.expression_depth),
                              "--pattern-depth", str(args.pattern_depth), "--type-depth", str(args.type_depth),
                              "--statements", str(args.statements), "--output", str(output / "review")])
        step("type-differential", [sys.executable, "scripts/review_roc_syntax.py", "--probe-command", shlex.quote(str(probe)),
                                   "--oracle-command", shlex.quote(str(oracle)), "--fixtures", "scripts/roc_syntax/type-fixtures.json",
                                   "--generated", "0", "--output", str(output / "types")])
        step("platform-differential", [sys.executable, "scripts/review_roc_syntax.py", "--probe-command", shlex.quote(str(probe)),
                                       "--oracle-command", shlex.quote(str(oracle)), "--fixtures", "scripts/roc_syntax/platform-fixtures.json",
                                       "--generated", "0", "--output", str(output / "platforms")])
        step("grammar-differential", [sys.executable, "scripts/review_roc_syntax.py", "--probe-command", shlex.quote(str(probe)),
                                      "--oracle-command", shlex.quote(str(oracle)), "--fixtures", "scripts/roc_syntax/grammar-fixtures.json",
                                      "--generated", "0", "--output", str(output / "grammar")])
        source_git = manifest["oracle_build"].get("source_git")
        if not source_git:
            raise ValueError("Oracle build receipt lacks its Git object source; rebuild the oracle.")
        corpus_repo = Path(source_git)
        manifest["corpus_repository"] = str(corpus_repo)
        step("full-corpus", [sys.executable, "scripts/check_roc_syntax_corpus.py", "--roc-source", str(corpus_repo),
                             "--probe-command", shlex.quote(str(probe)), "--oracle-command", shlex.quote(str(oracle)),
                             "--fixtures-output", str(output / "upstream-fixtures.json"),
                             "--output", str(output / "corpus.json")])
        step("editor-recovery", [sys.executable, "scripts/check_roc_syntax_recovery.py",
                                "--probe-command", shlex.quote(str(probe)), "--oracle-command", shlex.quote(str(oracle)),
                                "--output", str(output / "recovery.json")])
        if not args.skip_fuzz:
            step("fuzz", [sys.executable, "scripts/run_fuzz.py", "smoke", "roc-syntax-raw",
                          "roc-syntax-structured", "--runs", str(args.fuzz_runs)])
        mutations = [sys.executable, "scripts/mutate_roc_syntax.py", "--probe-command", shlex.quote(str(probe)),
                     "--oracle-command", shlex.quote(str(oracle)), "--runs", str(args.mutation_runs),
                     "--seed", str(args.seed), "--output", str(output / "mutations")]
        for fixtures in ["scripts/roc_syntax/fixtures.json", "scripts/roc_syntax/type-fixtures.json",
                         "scripts/roc_syntax/platform-fixtures.json", "scripts/roc_syntax/grammar-fixtures.json",
                         str(output / "upstream-fixtures.json")]:
            mutations += ["--fixtures", fixtures]
        raw_corpus = ROOT / ".roc-parser-tmp/fuzz/corpus/roc-syntax-raw"
        if raw_corpus.is_dir():
            mutations += ["--corpus", str(raw_corpus)]
        step("oracle-mutations", mutations)
        benchmark = [sys.executable, "scripts/bench_roc_syntax.py", "--roc", roc,
                     "--output", str(output / "performance.json")]
        if args.allocations:
            alloc = output / "alloc"
            step("allocation-build", [roc, "build", "--opt=speed", "--no-cache", "--fuzz",
                                      "bench/roc/roc-syntax-alloc.roc", f"--output={alloc}"])
            benchmark += ["--allocation-binary", str(alloc)]
        if args.baseline:
            benchmark += ["--baseline", str(args.baseline.resolve())]
        step("performance", benchmark)
        if args.reference_performance:
            reference = Path(step("performance-oracle-build", [sys.executable, "scripts/roc_syntax/build_oracle.py",
                             "--zig", zig, "--roc-repo", str(corpus_repo), "--optimize", "ReleaseFast",
                             "--cache-dir", str(output / "performance-oracle")]).stdout.strip())
            comparison = [sys.executable, "scripts/compare_roc_syntax_performance.py", "--roc", roc,
                          "--probe-command", shlex.quote(str(probe)), "--oracle-command", shlex.quote(str(reference)),
                          "--max-reference-ratio", str(args.max_reference_ratio),
                          "--output", str(output / "reference-performance")]
            if args.reference_performance == "smoke":
                comparison += ["--smoke"]
            step("reference-performance", comparison)
        if source_identity(ROOT) != manifest["source_sha256"]:
            raise ValueError("Candidate sources changed during validation; rerun against a stable revision.")
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        manifest["status"] = "failed"
        manifest["error"] = str(error)
        save()
        print(f"Validation failed: {error}\nEvidence: {output / 'validation.json'}", file=sys.stderr)
        return 1
    manifest["status"] = "passed"
    manifest["finished"] = time.time()
    save()
    print(f"Roc syntax validation passed. Evidence: {output / 'validation.json'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
