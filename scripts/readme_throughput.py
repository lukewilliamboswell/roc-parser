#!/usr/bin/env python3
"""Measure the README's representative whole-document parsing workloads.

Build drivers serially, then pin all timed runs to one logical CPU. Only Roc
is measured; reference answers and measurements remain in their own caches.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import sys

try:
    from . import bench
except ImportError:
    import bench

ROOT = Path(__file__).resolve().parents[1]
TARGET_BYTES = 64 * 1024


def documents() -> list[bench.Doc]:
    selected = [doc for doc in bench.build_corpus() if doc.kind == "medium"
                and (doc.format != "http" or doc.name == "post-medium")]
    for name, head, unit, tail in (
        ("html", "<div>", '<span data-x="v">text</span>', "</div>"),
        ("svg", "<g>", '<path d="M0 0L10 10Z" pathLength="1"/>', "</g>"),
        ("mathml", "<mrow>", "<mfrac><mn>1</mn><mn>2</mn></mfrac>", "</mrow>"),
    ):
        selected.append(bench.Doc(name, "siblings-medium", "medium",
                                  bench.fill(TARGET_BYTES, lambda _, text=unit: text, head, tail).encode()))
    selected.append(bench.Doc("roc-syntax", "sibling-document-medium", "medium",
                              bench.fill(TARGET_BYTES, lambda i: f"x{i} = {i} + 2 * 3 # representative source padding for sibling scaling\n", "module []\n").encode()))
    selected.append(bench.Doc("asciidoc", "sections-medium", "medium", bench.fill(
        TARGET_BYTES,
        lambda i: f"== Section {i}\n\nA paragraph with *strong* and _emphasized_ text, café and 日本.\n\n* First item\n* Second item\n\n[source,roc]\n----\nanswer = 42\n----\n\n",
        "= Representative document\n\n",
    ).encode()))
    return selected


def source_identity() -> str:
    digest = hashlib.sha256()
    for path in sorted([*(ROOT / "package").glob("*.roc"), *(ROOT / "bench/roc").glob("*.roc"),
                        ROOT / "scripts/bench.py", Path(__file__).resolve()]):
        digest.update(path.relative_to(ROOT).as_posix().encode() + b"\0" + path.read_bytes() + b"\0")
    return digest.hexdigest()


def build_drivers(builder: bench.Builder) -> dict[str, bench.Impl]:
    implementations = {impl.format: impl for impl in builder.roc_impls(bench.FORMATS)
                       if impl.workload == "collect"}
    for driver, operations in (("markup", ("html", "svg", "mathml")),
                               ("roc-syntax", ("roc-syntax",)), ("asciidoc", ("asciidoc",))):
        binary = builder.out / "bin" / f"roc-{driver}"
        binary.parent.mkdir(parents=True, exist_ok=True)
        if not builder.run(f"roc-{driver}", [builder.roc, "build", "--opt=speed", "--debug",
                                           str(ROOT / "bench/roc" / f"{driver}.roc"), f"--output={binary}"]):
            raise RuntimeError(f"driver build failed: {driver}; see {builder.out / 'logs'}")
        for operation in operations:
            command = [str(binary)] + ([operation] if driver == "markup" else [])
            implementations[operation] = bench.Impl("roc", "roc-parser", operation, command,
                                                    "collect; complete parse and observable model checksum")
    if set(implementations) != {doc.format for doc in documents()}:
        raise RuntimeError(f"missing drivers: {builder.skipped}")
    return implementations


def qualified_measure(implementation: bench.Impl, document: bench.Doc, args) -> dict:
    record = bench.measure(implementation, document, args)
    if "error" in record or not record.get("accepted"):
        raise RuntimeError(f"benchmark workload did not parse successfully: {document.id}: {record}")
    iterations = record["iterations"]
    receipts = record["receipts"]
    for batch in (receipts["calibration"], receipts["warmup"], *receipts["batches"]):
        if (batch["successes"] != batch["iterations"]
                or batch["checksum"] * iterations != record["checksum"] * batch["iterations"]):
            raise RuntimeError(f"unstable acceptance or model checksum: {document.id}")
    if record["spread_pct"] > args.max_spread_pct:
        raise RuntimeError(f"noisy measurements for {document.id}: {record['spread_pct']:.1f}% spread; rerun on an idle core")
    return {**record, "input_sha256": document.sha256}


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--cpu", type=int, required=True, help="logical CPU for the timed runs; Linux sched_setaffinity required")
    parser.add_argument("--work", type=Path, default=ROOT / ".roc-parser-tmp/readme-throughput")
    parser.add_argument("--repetitions", type=int, default=7)
    parser.add_argument("--target-ms", type=float, default=200)
    parser.add_argument("--max-spread-pct", type=float, default=25)
    parser.add_argument("--max-iterations", type=int, default=100000)
    parser.add_argument("--timeout", type=float, default=60)
    args = parser.parse_args(argv)
    if not hasattr(os, "sched_setaffinity") or args.cpu not in os.sched_getaffinity(0):
        parser.error("choose an available logical CPU on Linux")
    if min(args.repetitions, args.target_ms, args.max_iterations, args.timeout, args.max_spread_pct) <= 0:
        parser.error("measurement settings must be positive")
    args.work = args.work.resolve()
    args.work.mkdir(parents=True, exist_ok=True)
    identity = source_identity()
    bench._install_wait_hook()
    builder = bench.Builder(args.work, args.roc, None, lambda message: print(message, file=sys.stderr, flush=True))
    drivers = build_drivers(builder)
    corpus = documents()
    bench.write_corpus(corpus, args.work / "corpus")
    affinity = os.sched_getaffinity(0)
    os.sched_setaffinity(0, {args.cpu})
    report = {"metadata": {"machine": bench.machine(), "git": bench.git_state(), "compiler": builder.versions["roc"],
                            "compiler_zig_build_mode": "unknown (official nightly)",
                            "build_flags": ["--opt=speed", "--debug"], "source_sha256": identity,
                            "cpu": args.cpu, "affinity": sorted(os.sched_getaffinity(0)),
                            "measurement": "in-memory whole-document parse, model construction, checksum and destruction; stdin/output excluded",
                            "settings": {key: getattr(args, key) for key in ("repetitions", "target_ms", "max_iterations", "timeout", "max_spread_pct")},
                            "inputs": [{"format": doc.format, "id": doc.id, "bytes": len(doc.data), "sha256": doc.sha256} for doc in corpus],
                            "binaries": {name: hashlib.sha256(Path(impl.command[0]).read_bytes()).hexdigest() for name, impl in drivers.items()}},
              "results": []}
    try:
        for document in corpus:
            record = qualified_measure(drivers[document.format], document, args)
            report["results"].append(record)
            (args.work / "results.json").write_text(json.dumps(report, indent=2) + "\n")
            print(f"{document.format}: {record['mb_per_s']:.2g} MB/s ({record['spread_pct']:.1f}% spread)", flush=True)
    finally:
        os.sched_setaffinity(0, affinity)
    if source_identity() != identity:
        raise RuntimeError("sources changed during measurement; discard this run")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
