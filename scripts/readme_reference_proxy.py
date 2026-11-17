#!/usr/bin/env python3
"""Adapt pinned JSONL workers to the common parse-only benchmark protocol.

The worker's monotonic elapsed_ns is preserved: process startup, JSON input,
UTF-8 decoding, response projection and IPC are outside that worker's timer.
"""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys


def request(operation, data, iterations, *, correctness=False):
    source = data.decode("utf-8")
    if operation == "asciidoc":
        return {"id": "readme-reference", "source": source, "iterations": 0 if correctness else iterations,
                "warmup": 0, "mode": "parse", "expand": False, "includes": [], "attributes": []}
    result = {"version": 1, "id": "readme-reference", "mode": "file", "source": source}
    if not correctness:
        result.update(benchmark_iterations=iterations, benchmark_warmup=0)
    return result


def response(operation, result, iterations):
    if "error" in result:
        raise ValueError(f"reference worker failed: {result}")
    if result.get("id") != "readme-reference" or result.get("iterations") != iterations or result.get("successes") != iterations:
        raise ValueError("reference worker did not accept every requested iteration")
    if result.get("warmup") != 0:
        raise ValueError("unexpected internal warmup contaminates the checksum")
    if operation == "roc-syntax" and (result.get("version") != 1 or result.get("requested_iterations") != iterations
                                      or result.get("capped") is not False or result.get("consumed") != iterations):
        raise ValueError("compiler reference capped or failed to consume the input")
    if operation == "asciidoc" and (result.get("version") != 2 or result.get("phase") != "timing"
                                    or result.get("mode") != "parse" or result.get("oracle_version") != "2.0.23"):
        raise ValueError("incompatible Asciidoctor worker protocol/version")
    for field in ("elapsed_ns", "checksum"):
        if type(result.get(field)) is not int or result[field] < (1 if field == "elapsed_ns" else 0):
            raise ValueError(f"invalid reference {field}")
    return {key: result[key] for key in ("iterations", "elapsed_ns", "successes", "checksum")}


def worker_response(operation, data, iterations, *, worker, script=None, gem_home=None, correctness=False):
    command = [worker] + ([str(script)] if script else [])
    env = dict(os.environ)
    if gem_home is not None:
        for name in ("RUBYOPT", "RUBYLIB", "GEM_HOME", "GEM_PATH"):
            env.pop(name, None)
        env.update(GEM_HOME=str(gem_home), GEM_PATH=str(gem_home))
    payload = json.dumps(request(operation, data, iterations, correctness=correctness), ensure_ascii=False).encode() + b"\n"
    completed = subprocess.run(command, input=payload, capture_output=True, timeout=120, env=env)
    if completed.returncode:
        raise ValueError(f"reference worker exited {completed.returncode}: {completed.stderr[-1500:]!r}")
    lines = completed.stdout.splitlines()
    if len(lines) != 1:
        raise ValueError("reference worker must return exactly one JSON line")
    result = json.loads(lines[0])
    if not isinstance(result, dict) or result.get("id") != "readme-reference" or "error" in result:
        raise ValueError(f"invalid reference worker reply: {result}")
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("asciidoc", "roc-syntax"))
    parser.add_argument("--worker", required=True)
    parser.add_argument("--script", type=Path)
    parser.add_argument("--gem-home", type=Path)
    parser.add_argument("iterations", type=int)
    args = parser.parse_args(argv)
    if not 1 <= args.iterations <= 100000:
        parser.error("iterations must be 1–100000")
    data = sys.stdin.buffer.read()
    result = worker_response(args.operation, data, args.iterations, worker=args.worker, script=args.script, gem_home=args.gem_home)
    print(json.dumps(response(args.operation, result, args.iterations)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
