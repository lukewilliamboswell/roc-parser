#!/usr/bin/env python3
"""Measure the README's representative whole-document parsing workloads.

Build drivers serially, then pin all timed runs to one logical CPU. Only Roc
is rebuilt/measured by default; qualified reference measurements are reused.
"""
from __future__ import annotations

import argparse
import hashlib
import importlib
import json
import os
from pathlib import Path
import sys

try:
    from . import bench, readme_references
except ImportError:
    import bench
    import readme_references

ROOT = Path(__file__).resolve().parents[1]
TARGET_BYTES = 64 * 1024


def documents() -> list[bench.Doc]:
    selected = [doc for doc in bench.build_corpus() if doc.kind == "medium" and doc.format != "http"]
    # HTTP bodies are opaque bytes: a large body would inflate the advertised
    # rate without adding parsing work. Measure the realistic browser head.
    selected.append(bench.Doc("http", "sample-browser-get", "sample", bench.samples()["http"]["sample-browser-get"]))
    for name, head, unit, tail in (
        ("html", "<div>", '<span data-x="v">text</span>', "</div>"),
        ("svg", '<svg xmlns="http://www.w3.org/2000/svg">', '<path d="M0 0L10 10Z" pathLength="1"/>', "</svg>"),
        ("mathml", '<math xmlns="http://www.w3.org/1998/Math/MathML">', "<mfrac><mn>1</mn><mn>2</mn></mfrac>", "</math>"),
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


def build_drivers(builder: bench.Builder, corpus: list[bench.Doc] | None = None) -> dict[str, bench.Impl]:
    implementations = {impl.format: impl for impl in builder.roc_impls(bench.FORMATS)
                       if impl.workload == "collect"}
    for driver, operations in (("markup", ("html", "svg", "mathml")),
                               ("roc-syntax", ("roc-syntax",)), ("asciidoc", ("asciidoc",))):
        binary = builder.out / "bin" / f"roc-{driver}"
        binary.parent.mkdir(parents=True, exist_ok=True)
        if not builder.run(f"roc-{driver}", [builder.roc, "build", "--debug",
                                           str(ROOT / "bench/roc" / f"{driver}.roc"), f"--output={binary}"]):
            raise RuntimeError(f"driver build failed: {driver}; see {builder.out / 'logs'}")
        for operation in operations:
            command = [str(binary)] + ([operation] if driver == "markup" else [])
            implementations[operation] = bench.Impl("roc", "roc-parser", operation, command,
                                                    "collect; complete parse and observable model checksum")
    canonical = builder.out / "bin" / "roc-markup-canonical"
    if not builder.run("roc-markup-canonical", [builder.roc, "build", "--debug",
                                               str(ROOT / "bench/roc/markup-compare.roc"), f"--output={canonical}"]):
        raise RuntimeError(f"canonical markup probe build failed; see {builder.out / 'logs'}")
    if set(implementations) != {doc.format for doc in (documents() if corpus is None else corpus)}:
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
    parser.add_argument("--refresh-references", action="store_true", help="explicitly build/measure collection references; default reuses their cache only")
    parser.add_argument("--reference-cache-dir", type=Path, default=bench.DEFAULT_OUT / "reference-cache", help="qualified reference measurements, shareable across worktrees")
    parser.add_argument("--python", default=None, help="existing interpreter with the pinned comparator packages")
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
    args.quick = False
    args.work = args.work.resolve()
    args.work.mkdir(parents=True, exist_ok=True)
    identity = source_identity()
    corpus = documents()
    for document in corpus:
        path = args.work / "corpus" / document.format / f"{document.name}.txt"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(document.data)
    bench._install_wait_hook()
    builder = bench.Builder(args.work, args.roc, args.python, lambda message: print(message, file=sys.stderr, flush=True))
    drivers = build_drivers(builder, corpus)
    extra_module = f"{__package__}.readme_extra_references" if __package__ else "readme_extra_references"
    try:
        extra_references = importlib.import_module(extra_module)
    except ModuleNotFoundError as error:
        if error.name != extra_module:
            raise
        extra_references = None
    references = readme_references.ReferenceRun(builder, args, corpus, extra_references)
    references.build()
    affinity = os.sched_getaffinity(0)
    os.sched_setaffinity(0, {args.cpu})
    report = {"metadata": {"machine": bench.machine(), "git": bench.git_state(), "compiler": builder.versions["roc"],
                            "compiler_zig_build_mode": "unknown (official nightly)",
                            "build_flags": ["--debug"], "source_sha256": identity,
                            "reference_cache": {"mode": "refresh" if args.refresh_references else "reuse",
                                                "directory": str(references.cache.folder)},
                            "reference_selection": "fastest successful collection reference; borrowed ownership and model/checksum differences labeled; no ratios implied",
                            "cpu": args.cpu, "affinity": sorted(os.sched_getaffinity(0)),
                            "measurement": "in-memory whole-document parse, model construction, checksum and destruction; stdin/output excluded",
                            "settings": {key: getattr(args, key) for key in ("repetitions", "target_ms", "max_iterations", "timeout", "max_spread_pct")},
                            "inputs": [{"format": doc.format, "id": doc.id, "bytes": len(doc.data), "sha256": doc.sha256} for doc in corpus],
                            "binaries": {name: hashlib.sha256(Path(impl.command[0]).read_bytes()).hexdigest() for name, impl in drivers.items()}},
              "results": [], "reference_results": [], "fastest_references": {}}
    try:
        for document in corpus:
            record = qualified_measure(drivers[document.format], document, args)
            report["results"].append(record)
            (args.work / "results.json").write_text(json.dumps(report, indent=2) + "\n")
            print(f"{document.format}: {record['mb_per_s']:.2g} MB/s ({record['spread_pct']:.1f}% spread)", flush=True)
        report["reference_results"], report["fastest_references"] = references.collect(report["results"], report["metadata"])
        (args.work / "results.json").write_text(json.dumps(report, indent=2) + "\n")
        for document in corpus:
            fastest = report["fastest_references"].get(document.format)
            if fastest:
                details = [fastest["ownership"], fastest["semantic_note"]]
                print(f"{document.format} fastest measured collection reference: {fastest['impl']} "
                      f"{fastest['mb_per_s']:.2g} MB/s ({'; '.join(details)})", flush=True)
            else:
                print(f"{document.format}: ReferenceUnavailable (no qualified collection measurement)", flush=True)
    finally:
        os.sched_setaffinity(0, affinity)
    if source_identity() != identity:
        raise RuntimeError("sources changed during measurement; discard this run")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
