#!/usr/bin/env python3
"""Benchmark roc-parser's format parsers against Go, Rust and Python libraries.

Every implementation is a small program speaking one protocol: it reads one
document from stdin, parses it N times (its last argument) with the clock
running only around the parse loop, and prints
{"iterations", "elapsed_ns", "successes", "checksum"} as one JSON line. The
harness builds the programs, generates deterministic corpora, calibrates N so
each batch runs long enough to time, and reports medians, spread, throughput,
peak RSS and (for Roc, with --allocations) allocation counts.

Reports go to .roc-parser-tmp/bench/<label>/results.json and report.md.
See docs/benchmarks.adoc.
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime
import hashlib
import json
import math
import os
import platform
import random
import re
import shutil
import statistics
import subprocess
import sys
import time
from pathlib import Path
from typing import Callable

ROOT = Path(__file__).resolve().parents[1]
BENCH = ROOT / "bench"
DEFAULT_OUT = ROOT / ".roc-parser-tmp" / "bench"
FORMATS = ("csv", "yaml", "xml", "markdown", "http")
EXTENSIONS = {"csv": "csv", "yaml": "yaml", "xml": "xml", "markdown": "md", "http": "http"}
SIZES = {"small": 4 * 1024, "medium": 64 * 1024, "large": 1024 * 1024}
SCHEMA_VERSION = 1


# ---------------------------------------------------------------------------
# Corpora

@dataclasses.dataclass(frozen=True)
class Doc:
    format: str
    name: str
    kind: str  # small | medium | large | sample | pathological
    data: bytes

    @property
    def id(self) -> str:
        return f"{self.format}/{self.name}"

    @property
    def sha256(self) -> str:
        return hashlib.sha256(self.data).hexdigest()


WORDS = ("alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel",
         "india", "juliet", "kilo", "lima", "parser", "combinator", "roc", "value",
         "stream", "token", "record", "field", "élan", "naïve", "日本")
NAMES = ("Ada Lovelace", "Grace Hopper", "Alan Turing", "Edsger Dijkstra", "Barbara Liskov",
         "Donald Knuth", "Margaret Hamilton", "Radia Perlman", "Ken Thompson", "Frances Allen")


def words(rng: random.Random, low: int, high: int) -> str:
    return " ".join(rng.choice(WORDS) for _ in range(rng.randint(low, high)))


def fill(target: int, unit: Callable[[int], str], head: str = "", tail: str = "") -> str:
    parts, size, i = [head], len(head.encode()), 0
    while size < target:
        part = unit(i)
        parts.append(part)
        size += len(part.encode())
        i += 1
    parts.append(tail)
    return "".join(parts)


def csv_doc(rng: random.Random, target: int) -> str:
    def row(i: int) -> str:
        note = rng.choice(["", words(rng, 1, 4), f'"{words(rng, 1, 3)}, {words(rng, 1, 2)}"',
                           f'"said ""{rng.choice(WORDS)}"""', f'"line one\nline {i}"'])
        return (f"{i},{rng.choice(NAMES)},{rng.choice(WORDS)}@example.com,"
                f"{rng.randint(1, 99)},{rng.uniform(0, 1000):.2f},2026-{rng.randint(1, 12):02d}-"
                f"{rng.randint(1, 28):02d},{rng.choice(['true', 'false'])},{note}\n")
    return fill(target, row, "id,name,email,quantity,amount,date,active,note\n")


def yaml_doc(rng: random.Random, target: int) -> str:
    def service(i: int) -> str:
        env = "".join(f"      - name: KEY_{j}\n        value: {words(rng, 1, 3)}\n" for j in range(rng.randint(1, 3)))
        return (f"  service-{i}:\n"
                f"    image: \"registry.example.com/{rng.choice(WORDS)}:{rng.randint(1, 9)}.{rng.randint(0, 20)}\"\n"
                f"    replicas: {rng.randint(1, 12)}\n"
                f"    enabled: {rng.choice(['true', 'false'])}\n"
                f"    ratio: {rng.uniform(0, 1):.3f}\n"
                f"    ports: [{rng.randint(1000, 9999)}, {rng.randint(1000, 9999)}]\n"
                f"    labels: {{tier: {rng.choice(['web', 'db', 'cache'])}, owner: '{rng.choice(NAMES)}'}}\n"
                f"    env:\n{env}"
                f"    description: |\n      {words(rng, 3, 8)}\n      {words(rng, 3, 8)}\n")
    return fill(target, service, "version: 3\nservices:\n")


def xml_escape(text: str) -> str:
    return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def xml_doc(rng: random.Random, target: int) -> str:
    def book(i: int) -> str:
        return (f'  <book id="bk{i}" lang="{rng.choice(["en", "fr", "ja"])}" available="{rng.choice(["yes", "no"])}">\n'
                f"    <title>{xml_escape(words(rng, 2, 5).title())}</title>\n"
                f"    <author>{rng.choice(NAMES)}</author>\n"
                f'    <price currency="USD">{rng.uniform(5, 80):.2f}</price>\n'
                f"    <description>{xml_escape(words(rng, 8, 20))} &amp; &#x2014; {rng.choice(WORDS)}</description>\n"
                f"    <tags>{''.join(f'<tag>{rng.choice(WORDS)}</tag>' for _ in range(rng.randint(1, 4)))}</tags>\n"
                f"  </book>\n")
    return fill(target, book, '<?xml version="1.0" encoding="UTF-8"?>\n<catalog>\n', "</catalog>\n")


def markdown_doc(rng: random.Random, target: int) -> str:
    def section(i: int) -> str:
        w = rng.choice(WORDS)
        parts = [f"## Section {i}: {words(rng, 1, 3)}\n\n",
                 f"{words(rng, 5, 12)} *{w}* and **{rng.choice(WORDS)}** with `code_{i}` and a "
                 f"[link](https://example.com/{w}/{i} \"title\"). {words(rng, 5, 15)}\n{words(rng, 4, 10)}.\n\n"]
        kind = i % 4
        if kind == 0:
            parts.append("".join(f"- {words(rng, 2, 6)}\n" for _ in range(rng.randint(2, 5))) + "\n")
        elif kind == 1:
            parts.append(f"```roc\nmain = {rng.randint(1, 99)}\nvalue = \"{w}\"\n```\n\n")
        elif kind == 2:
            parts.append(f"> {words(rng, 4, 10)}\n> {words(rng, 2, 6)}\n\n")
        else:
            rows = "".join(f"| {rng.choice(WORDS)} | {rng.randint(0, 999)} |\n" for _ in range(3))
            parts.append(f"| Name | Count |\n| :--- | ---: |\n{rows}\n")
        return "".join(parts)
    return fill(target, section, "# Generated document\n\n")


BROWSER_HEADERS = (
    "Host: www.example.com\r\n"
    "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:131.0) Gecko/20100101 Firefox/131.0\r\n"
    "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
    "Accept-Language: en-GB,en;q=0.5\r\n"
    "Accept-Encoding: gzip, deflate, br, zstd\r\n"
    "Referer: https://www.example.com/docs/\r\n"
    "Connection: keep-alive\r\n"
    "Cookie: session=3f9a1c2b7d; theme=dark; consent=yes\r\n"
    "Upgrade-Insecure-Requests: 1\r\n"
    "Sec-Fetch-Dest: document\r\n"
    "Sec-Fetch-Mode: navigate\r\n"
    "Sec-Fetch-Site: same-origin\r\n"
    "Priority: u=0, i\r\n"
)

API_BODY = ('{"id":42,"name":"roc-parser","stars":1234,"topics":["parsing","roc","combinators"],'
            '"owner":{"login":"example","type":"User"},"updated_at":"2026-10-01T09:00:00Z"}')

#: Real-world-shaped HTTP captures. Kept in code so the CRLFs cannot be lost.
HTTP_SAMPLES = {
    "sample-browser-get": "GET /docs/parser/combinators.html?lang=en HTTP/1.1\r\n" + BROWSER_HEADERS + "\r\n",
    "sample-api-response": (
        "HTTP/1.1 200 OK\r\n"
        "Date: Wed, 01 Oct 2026 09:00:00 GMT\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        f"Content-Length: {len(API_BODY)}\r\n"
        "Cache-Control: no-store\r\n"
        "X-Request-Id: 7c1d9a0e-5b3f-4f8e-9a2b-1c3d5e7f9a0b\r\n"
        "Strict-Transport-Security: max-age=63072000; includeSubDomains\r\n"
        "Vary: Accept-Encoding\r\n"
        "\r\n" + API_BODY
    ),
}


def http_post(rng: random.Random, target: int) -> str:
    body = fill(target, lambda i: f'{{"id":{i},"name":"{rng.choice(NAMES)}","score":{rng.random():.4f}}},', "[", "{}]")
    return ("POST /api/v1/records HTTP/1.1\r\nHost: api.example.com\r\nContent-Type: application/json\r\n"
            f"Authorization: Bearer abc.def.ghi\r\nContent-Length: {len(body.encode())}\r\n\r\n{body}")


def http_chunked(rng: random.Random, target: int, chunk: int = 8192) -> str:
    body = fill(target, lambda i: f"<li>{words(rng, 2, 6)}</li>\n")
    chunks = "".join(f"{len(body[i:i + chunk].encode()):x}\r\n{body[i:i + chunk]}\r\n" for i in range(0, len(body), chunk))
    return ("HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nTransfer-Encoding: chunked\r\n"
            f"Server: example\r\n\r\n{chunks}0\r\n\r\n")


GENERATORS: dict[str, Callable[[random.Random, int], str]] = {
    "csv": csv_doc, "yaml": yaml_doc, "xml": xml_doc, "markdown": markdown_doc,
}


def seed_for(name: str) -> int:
    """A fixed seed per document, independent of Python's hash randomisation."""
    return int.from_bytes(hashlib.sha256(name.encode()).digest()[:8], "big")


def pathological() -> dict[str, dict[str, str]]:
    """Adversarial shapes that once were (or commonly are) super-linear."""
    return {
        "csv": {
            "wide-row": ",".join(f"f{i}" for i in range(20000)) + "\n",
            "escaped-quotes": '"' + '""' * 50000 + '"\n',
            "quoted-newlines": '"' + "a\n" * 30000 + '"\n',
        },
        "yaml": {
            "nested-mapping-100": "".join("  " * i + f"k{i}:\n" for i in range(99)) + "  " * 99 + "leaf: v\n",
            "nested-flow-99": "[" * 99 + "0" + "]" * 99 + "\n",
            "long-flow-sequence": "[" + ", ".join(str(i) for i in range(20000)) + "]\n",
            "many-block-scalars": "".join(f"key{i}: |-\n  abcdefgh\n" for i in range(2000)),
        },
        "xml": {
            "deep-nesting": "<a>" * 5000 + "x" + "</a>" * 5000,
            "many-attributes": "<a " + " ".join(f'k{i}="{i}"' for i in range(5000)) + "/>",
            "entity-heavy": "<a>" + "&amp;&lt;&#65;&#x42;" * 10000 + "</a>",
        },
        "markdown": {
            "unclosed-emphasis": "*a " * 10000 + "\n",
            "nested-brackets": "[" * 5000 + "a" + "]" * 5000 + "\n",
            "deep-blockquote": ">" * 1000 + " a\n",
            "many-backticks": "`a``" * 5000 + "\n",
        },
        "http": {
            "many-headers": "GET / HTTP/1.1\r\nHost: example.com\r\n"
                            + "".join(f"X-Header-{i}: value-{i}\r\n" for i in range(100)) + "\r\n",
            "tiny-chunks": "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" + "1\r\na\r\n" * 20000 + "0\r\n\r\n",
        },
    }


def samples() -> dict[str, dict[str, bytes]]:
    folder = BENCH / "corpus" / "samples"
    return {
        "csv": {"sample-orders-export": (folder / "orders-export.csv").read_bytes()},
        "yaml": {"sample-github-actions": (folder / "github-actions.yaml").read_bytes()},
        "xml": {"sample-svg": (folder / "logo.svg").read_bytes()},
        "markdown": {"sample-readme": (folder / "project-readme.md").read_bytes()},
        "http": {name: text.encode() for name, text in HTTP_SAMPLES.items()},
    }


def build_corpus(quick: bool = False, formats: tuple[str, ...] = FORMATS) -> list[Doc]:
    """Every document, deterministically. --quick keeps small, sample and one
    pathological document per format."""
    sizes = {"small": SIZES["small"]} if quick else SIZES
    docs: list[Doc] = []
    patho = pathological()
    for fmt in formats:
        for name, data in samples()[fmt].items():
            docs.append(Doc(fmt, name, "sample", data))
        for kind, target in sizes.items():
            name = f"generated-{kind}"
            rng = random.Random(seed_for(f"{fmt}/{name}"))
            if fmt == "http":
                docs.append(Doc(fmt, f"post-{kind}", kind, http_post(rng, target).encode()))
                docs.append(Doc(fmt, f"chunked-{kind}", kind, http_chunked(rng, target).encode()))
            else:
                docs.append(Doc(fmt, name, kind, GENERATORS[fmt](rng, target).encode()))
        for i, (name, text) in enumerate(patho[fmt].items()):
            if quick and i > 0:
                break
            docs.append(Doc(fmt, name, "pathological", text.encode()))
    return docs


# ---------------------------------------------------------------------------
# Implementations

@dataclasses.dataclass
class Impl:
    lang: str      # roc | rust | go | python
    name: str      # library name, e.g. "pulldown-cmark"
    format: str
    command: list[str]
    notes: str = ""

    @property
    def id(self) -> str:
        return f"{self.lang}/{self.name}"


#: Comparator libraries per language and format, with what each one measures.
COMPARATORS = {
    "rust": {
        "csv": [("csv", "csv 1.3 StringRecords copied into Vec<Vec<String>>")],
        "yaml": [("yaml-rust2", "yaml-rust2 YamlLoader tree"), ("serde_yaml", "serde_yaml Value tree (unsafe-libyaml)")],
        "xml": [("quick-xml", "quick-xml pull events built into an owned tree"), ("roxmltree", "roxmltree read-only DOM")],
        "markdown": [("pulldown-cmark", "pulldown-cmark events made 'static and collected"), ("comrak", "comrak arena AST")],
        "http": [("httparse", "httparse head plus Content-Length/chunked body framing, copied")],
    },
    "go": {
        "csv": [("encoding-csv", "encoding/csv ReadAll")],
        "yaml": [("yaml.v3", "gopkg.in/yaml.v3 Unmarshal into interface{}")],
        "xml": [("encoding-xml", "encoding/xml tokens built into a tree")],
        "markdown": [("goldmark", "goldmark AST with GFM table/strikethrough/tasklist")],
        "http": [("net-http", "net/http ReadRequest/ReadResponse plus io.ReadAll body")],
    },
    "python": {
        "csv": [("csv", "stdlib csv.reader (C) into lists")],
        "yaml": [("pyyaml", "PyYAML SafeLoader, pure Python"), ("pyyaml-libyaml", "PyYAML CSafeLoader (libyaml)")],
        "xml": [("etree", "xml.etree.ElementTree.fromstring (expat)")],
        "markdown": [("markdown-it-py", "markdown-it-py token stream, commonmark + table")],
        "http": [("h11", "h11 connection state machine")],
    },
}


def tool_version(command: list[str]) -> str:
    try:
        result = subprocess.run(command, capture_output=True, text=True, timeout=60)
    except (OSError, subprocess.TimeoutExpired):
        return ""
    return (result.stdout + result.stderr).strip().splitlines()[0] if result.returncode == 0 else ""


class Builder:
    def __init__(self, out: Path, roc: str, python: str | None, log: Callable[[str], None]):
        self.out = out
        self.roc = roc
        self.python = python
        self.log = log
        self.skipped: dict[str, str] = {}
        self.versions: dict[str, object] = {}

    def run(self, name: str, command: list[str], cwd: Path = ROOT) -> bool:
        self.log(f"build {name}: {' '.join(command)}")
        result = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
        (self.out / "logs").mkdir(parents=True, exist_ok=True)
        (self.out / "logs" / f"build-{name}.log").write_text(result.stdout + result.stderr)
        if result.returncode:
            self.skipped[name] = f"build failed: {(result.stdout + result.stderr)[-2000:]}"
        return result.returncode == 0

    def roc_impls(self, formats: tuple[str, ...]) -> list[Impl]:
        impls = []
        self.versions["roc"] = tool_version([self.roc, "version"]) or tool_version([self.roc, "--version"])
        for fmt in formats:
            binary = self.out / "bin" / f"roc-{fmt}"
            binary.parent.mkdir(parents=True, exist_ok=True)
            if self.run(f"roc-{fmt}", [self.roc, "build", "--opt=speed", str(BENCH / "roc" / f"{fmt}.roc"),
                                        f"--output={binary}"]):
                impls.append(Impl("roc", "roc-parser", fmt, [str(binary)], "roc build --opt=speed"))
        return impls

    def rust_impls(self, formats: tuple[str, ...]) -> list[Impl]:
        if not shutil.which("cargo"):
            self.skipped["rust"] = "cargo not found"
            return []
        target = self.out / "rust-target"
        if not self.run("rust", ["cargo", "build", "--release", "--locked", "--target-dir", str(target)],
                        cwd=BENCH / "compare" / "rust"):
            return []
        self.versions["rustc"] = tool_version(["rustc", "--version"])
        self.versions["rust_crates"] = cargo_lock_versions()
        return self._impls("rust", [str(target / "release" / "compare")], formats)

    def go_impls(self, formats: tuple[str, ...]) -> list[Impl]:
        if not shutil.which("go"):
            self.skipped["go"] = "go not found"
            return []
        binary = self.out / "bin" / "go-compare"
        if not self.run("go", ["go", "build", "-mod=readonly", "-o", str(binary), "."], cwd=BENCH / "compare" / "go"):
            return []
        self.versions["go"] = tool_version(["go", "version"])
        self.versions["go_modules"] = go_mod_versions()
        return self._impls("go", [str(binary)], formats)

    def python_impls(self, formats: tuple[str, ...]) -> list[Impl]:
        python = self.python
        if python is None:
            # Outside the checkout: repository policy rejects Python files
            # anywhere but scripts/, and a venv is full of them.
            cache = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
            # One venv per interpreter, so a blueprint (Nix) run and a plain
            # run never share packages built for another Python.
            key = hashlib.sha256(str(Path(sys.executable).resolve()).encode()).hexdigest()[:12]
            venv = cache / "roc-parser" / f"bench-venv-{key}"
            python = str(venv / "bin" / "python")
            if not Path(python).exists():
                if not self.run("python-venv", [sys.executable, "-m", "venv", str(venv)]):
                    return []
            if not self.run("python", [python, "-m", "pip", "install", "--quiet", "--disable-pip-version-check",
                                       "-r", str(BENCH / "compare" / "python" / "requirements.txt")]):
                return []
        freeze = subprocess.run([python, "-m", "pip", "freeze"], capture_output=True, text=True)
        self.versions["python"] = tool_version([python, "--version"])
        self.versions["python_packages"] = sorted(freeze.stdout.split())
        return self._impls("python", [python, str(ROOT / "scripts" / "bench_python.py")], formats)

    def _impls(self, lang: str, command: list[str], formats: tuple[str, ...]) -> list[Impl]:
        return [Impl(lang, name, fmt, command + [name], notes)
                for fmt in formats for name, notes in COMPARATORS[lang][fmt]]

    def alloc_binary(self) -> Path | None:
        binary = self.out / "bin" / "roc-alloc"
        if self.run("roc-alloc", [self.roc, "build", "--fuzz", str(BENCH / "roc" / "alloc.roc"), f"--output={binary}"]):
            return binary
        return None


def cargo_lock_versions() -> dict[str, str]:
    lock = (BENCH / "compare" / "rust" / "Cargo.lock").read_text()
    wanted = {name for lang in ["rust"] for fmt in COMPARATORS[lang].values() for name, _ in fmt}
    wanted |= {"pulldown-cmark", "unsafe-libyaml"}
    found = {}
    for name, version in re.findall(r'name = "([^"]+)"\nversion = "([^"]+)"', lock):
        if name in wanted:
            found[name] = version
    return found


def go_mod_versions() -> dict[str, str]:
    text = (BENCH / "compare" / "go" / "go.mod").read_text()
    return dict(re.findall(r"^\s+(\S+) (v\S+)", text, re.M))


# ---------------------------------------------------------------------------
# Measurement

class RunError(Exception):
    pass


def run_once(command: list[str], data: bytes, iterations: int, timeout: float) -> dict:
    """Run one batch, returning its timing record plus the child's peak RSS."""
    with subprocess.Popen(command + [str(iterations)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE) as proc:
        try:
            stdout, stderr = proc.communicate(data, timeout=timeout)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.communicate()
            raise RunError(f"timed out after {timeout}s with {iterations} iterations")
        rss = peak_rss_bytes(proc.pid)
        if proc.returncode:
            raise RunError(f"exit {proc.returncode}: {stderr.decode(errors='replace')[-500:]}")
    try:
        record = json.loads(stdout)
    except json.JSONDecodeError as error:
        raise RunError(f"bad output {stdout[:200]!r}") from error
    if record.get("iterations") != iterations or record.get("elapsed_ns", 0) <= 0:
        raise RunError(f"invalid timing record {record}")
    record["peak_rss_bytes"] = rss
    return record


_RUSAGE: dict[int, int] = {}


def peak_rss_bytes(pid: int) -> int | None:
    return _RUSAGE.pop(pid, None)


def _install_wait_hook() -> None:
    """Popen reaps children with waitpid; wrap it with wait4 to keep rusage."""
    if not hasattr(os, "wait4"):
        return
    original = os.waitpid

    def waitpid(pid: int, options: int):
        if pid > 0:
            got, status, usage = os.wait4(pid, options)
            if got:
                scale = 1 if sys.platform == "darwin" else 1024
                _RUSAGE[got] = usage.ru_maxrss * scale
            return got, status
        return original(pid, options)

    subprocess.os.waitpid = waitpid  # type: ignore[attr-defined]


def calibrate(first_ns: int, target_ns: int, max_iterations: int) -> int:
    """Iterations per batch so one batch takes about target_ns."""
    return max(1, min(max_iterations, math.ceil(target_ns / max(first_ns, 1))))


def summarize(per_parse_ns: list[float], size: int) -> dict:
    median = statistics.median(per_parse_ns)
    return {
        "median_ns": median,
        "min_ns": min(per_parse_ns),
        "max_ns": max(per_parse_ns),
        "stdev_ns": statistics.stdev(per_parse_ns) if len(per_parse_ns) > 1 else 0.0,
        "spread_pct": 100 * (max(per_parse_ns) - min(per_parse_ns)) / median if median else 0.0,
        "mb_per_s": (size / 1e6) / (median / 1e9) if median else None,
    }


def measure(impl: Impl, doc: Doc, args) -> dict:
    record = {"impl": impl.id, "lang": impl.lang, "library": impl.name, "doc": doc.id, "kind": doc.kind,
              "bytes": len(doc.data)}
    try:
        first = run_once(impl.command, doc.data, 1, args.timeout)
        iterations = calibrate(first["elapsed_ns"], int(args.target_ms * 1e6), args.max_iterations)
        run_once(impl.command, doc.data, iterations, args.timeout)  # warmup batch, discarded
        batches = [run_once(impl.command, doc.data, iterations, args.timeout) for _ in range(args.repetitions)]
    except RunError as error:
        record["error"] = str(error)
        return record
    record.update(summarize([b["elapsed_ns"] / iterations for b in batches], len(doc.data)))
    record["iterations"] = iterations
    record["accepted"] = batches[0]["successes"] == iterations
    record["checksum"] = batches[0]["checksum"]
    rss = [b["peak_rss_bytes"] for b in batches if b.get("peak_rss_bytes")]
    record["peak_rss_bytes"] = max(rss) if rss else None
    record["batch_elapsed_ns"] = [b["elapsed_ns"] for b in batches]
    return record


def count_allocations(binary: Path, doc: Doc, scratch: Path, timeout: float) -> dict:
    path = scratch / f"alloc-{doc.format}-{doc.name}.in"
    path.write_bytes(doc.format.encode() + b"\n" + doc.data)
    try:
        result = subprocess.run([str(binary), "replay", str(path)], capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return {"allocation_error": "timed out"}
    match = re.search(r"ALLOCATION_DIAGNOSTIC allocations=(\d+) result=(\w+)", result.stdout + result.stderr)
    if result.returncode != 77 or not match:
        return {"allocation_error": f"unexpected diagnostic exit {result.returncode}"}
    return {"allocation_calls": int(match[1])}


# ---------------------------------------------------------------------------
# Reports

def geomean(values: list[float]) -> float | None:
    values = [v for v in values if v and v > 0]
    return math.exp(sum(map(math.log, values)) / len(values)) if values else None


def comparison(results: list[dict]) -> list[dict]:
    """Per format and comparator: the geometric mean over non-pathological
    documents both accepted of (comparator throughput / roc throughput)."""
    by_key = {(r["impl"], r["doc"]): r for r in results if "median_ns" in r}
    rows = []
    for fmt in FORMATS:
        roc = {doc: r for (impl, doc), r in by_key.items() if impl == "roc/roc-parser" and doc.startswith(fmt + "/")}
        impls = sorted({impl for (impl, doc) in by_key if doc.startswith(fmt + "/") and impl != "roc/roc-parser"})
        for impl in impls:
            ratios, docs = [], []
            for doc, mine in roc.items():
                other = by_key.get((impl, doc))
                if other and mine["kind"] != "pathological" and mine["accepted"] and other["accepted"]:
                    ratios.append(mine["median_ns"] / other["median_ns"])
                    docs.append(doc)
            mbps = [by_key[(impl, d)]["mb_per_s"] for d in docs]
            rows.append({"format": fmt, "impl": impl, "documents": len(docs),
                         "speedup_vs_roc": geomean(ratios),
                         "impl_mb_per_s": geomean(mbps),
                         "roc_mb_per_s": geomean([roc[d]["mb_per_s"] for d in docs])})
    return rows


def fmt_ns(ns: float | None) -> str:
    if ns is None:
        return "-"
    for unit, scale in (("s", 1e9), ("ms", 1e6), ("µs", 1e3)):
        if ns >= scale:
            return f"{ns / scale:.3g} {unit}"
    return f"{ns:.3g} ns"


def fmt_ratio(ratio: float | None) -> str:
    if ratio is None:
        return "-"
    return f"{ratio:.2g}x faster" if ratio >= 1 else f"{1 / ratio:.2g}x slower"


def markdown_report(report: dict) -> str:
    meta = report["metadata"]
    lines = [f"# roc-parser benchmarks: {meta['label']}", "",
             f"- Date: {meta['date']}", f"- Machine: {meta['machine']['description']}",
             f"- OS: {meta['machine']['os']}", f"- Roc: {meta['versions'].get('roc', '?')}",
             f"- Environment: {describe_environment(meta.get('environment'))}",
             f"- roc-parser commit: {meta['git']['commit']}{' (dirty)' if meta['git']['dirty'] else ''}",
             f"- Mode: {'quick' if meta['quick'] else 'full'}, {meta['repetitions']} batches of about "
             f"{meta['target_ms']} ms after a warmup batch", ""]
    if meta["skipped"]:
        lines += ["Skipped: " + "; ".join(f"{k} ({v.splitlines()[0][:120]})" for k, v in meta["skipped"].items()), ""]
    lines += ["## Comparison", "",
              "Geometric mean over the non-pathological documents both accepted. "
              "\"3x slower\" means roc-parser takes three times as long as that library.", "",
              "| Format | Library | Docs | Library MB/s | roc-parser MB/s | roc-parser is |",
              "| --- | --- | ---: | ---: | ---: | --- |"]
    for row in report["comparison"]:
        lib_mb = f"{row['impl_mb_per_s']:.3g}" if row["impl_mb_per_s"] else "-"
        roc_mb = f"{row['roc_mb_per_s']:.3g}" if row["roc_mb_per_s"] else "-"
        lines.append(f"| {row['format']} | {row['impl']} | {row['documents']} | {lib_mb} | {roc_mb} | "
                     f"{fmt_ratio(1 / row['speedup_vs_roc']) if row['speedup_vs_roc'] else '-'} |")
    lines += ["", "## Every measurement", "",
              "| Document | Bytes | Implementation | Median | Spread | MB/s | Peak RSS | Result |",
              "| --- | ---: | --- | ---: | ---: | ---: | ---: | --- |"]
    for r in report["results"]:
        if "error" in r:
            lines.append(f"| {r['doc']} | {r['bytes']} | {r['impl']} | - | - | - | - | error: {r['error'][:80]} |")
            continue
        rss = f"{r['peak_rss_bytes'] / 2**20:.1f} MiB" if r.get("peak_rss_bytes") else "-"
        result = "accepted" if r["accepted"] else "rejected"
        if "allocation_calls" in r:
            result += f", {r['allocation_calls']} allocs"
        lines.append(f"| {r['doc']} | {r['bytes']} | {r['impl']} | {fmt_ns(r['median_ns'])} | "
                     f"{r['spread_pct']:.0f}% | {r['mb_per_s']:.3g} | {rss} | {result} |")
    return "\n".join(lines) + "\n"


def machine() -> dict:
    cpu = platform.processor()
    if sys.platform == "darwin":
        cpu = tool_version(["sysctl", "-n", "machdep.cpu.brand_string"]) or cpu
    elif Path("/proc/cpuinfo").exists():
        found = re.search(r"model name\s*:\s*(.*)", Path("/proc/cpuinfo").read_text())
        cpu = found[1] if found else cpu
    return {"description": f"{cpu} ({os.cpu_count()} CPUs, {platform.machine()})",
            "cpu": cpu, "cpus": os.cpu_count(), "arch": platform.machine(),
            "os": platform.platform(), "python": platform.python_version()}


BLUEPRINT_ENV = "ROC_PARSER_BLUEPRINT"


def environment(env: dict[str, str] | None = None, lock: Path = ROOT / "Blueprint.lock") -> dict:
    """Where the toolchains came from: a blueprint shell (Blueprint.roc sets
    ROC_PARSER_BLUEPRINT=1), the Blueprint.lock that pinned it, and tool paths."""
    env = os.environ if env is None else env
    info: dict = {"blueprint": env.get(BLUEPRINT_ENV) == "1",
                  "tools": {t: shutil.which(t, path=env.get("PATH")) for t in ("roc", "go", "cargo", "rustc", "python3")}}
    if info["blueprint"]:
        try:
            data = lock.read_bytes()
            nodes = json.loads(data)["nodes"]
            inputs = {name: nodes[node]["locked"] for name, node in nodes["root"]["inputs"].items()}
            info["lock"] = {"path": lock.name, "sha256": hashlib.sha256(data).hexdigest(),
                            "inputs": {name: f"{v.get('owner')}/{v.get('repo')}@{v.get('rev')}"
                                       for name, v in inputs.items()}}
        except (OSError, ValueError, KeyError, TypeError, AttributeError) as error:
            info["lock"] = {"path": lock.name, "error": str(error)}
    return info


def describe_environment(env: dict | None) -> str:
    if not env or not env.get("blueprint"):
        return "outside blueprint"
    lock = env.get("lock", {})
    if "sha256" not in lock:
        return f"blueprint, {lock.get('path', 'no lock')} unreadable"
    pins = ", ".join(f"{k} {v.rsplit('@', 1)[-1][:12]}" for k, v in sorted(lock["inputs"].items()))
    return f"blueprint, {lock['path']} sha256 {lock['sha256'][:12]} ({pins})"


def git_state() -> dict:
    commit = tool_version(["git", "-C", str(ROOT), "rev-parse", "HEAD"])
    status = subprocess.run(["git", "-C", str(ROOT), "status", "--porcelain", "--", "package", "bench", "scripts"],
                            capture_output=True, text=True).stdout
    return {"commit": commit, "dirty": bool(status.strip())}


def write_corpus(docs: list[Doc], folder: Path) -> list[dict]:
    manifest = []
    for doc in docs:
        path = folder / doc.format / f"{doc.name}.{EXTENSIONS[doc.format]}"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(doc.data)
        manifest.append({"id": doc.id, "kind": doc.kind, "bytes": len(doc.data), "sha256": doc.sha256,
                         "path": str(path.relative_to(folder))})
    return manifest


# ---------------------------------------------------------------------------
# Command line

def parse_args(argv: list[str] | None = None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--quick", action="store_true", help="small corpus and short batches, for CI smoke runs")
    ap.add_argument("--label", default=None, help="report folder name (default: full or quick)")
    ap.add_argument("--output-dir", type=Path, default=DEFAULT_OUT)
    ap.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    ap.add_argument("--formats", default=",".join(FORMATS))
    ap.add_argument("--langs", default="roc,rust,go,python", help="comma-separated: roc,rust,go,python")
    ap.add_argument("--only", default="", help="comma-separated document names or kinds to keep")
    ap.add_argument("--python", default=None, help="interpreter with the comparator packages (default: a venv)")
    ap.add_argument("--repetitions", type=int, default=None)
    ap.add_argument("--target-ms", type=float, default=None, help="approximate duration of one batch")
    ap.add_argument("--max-iterations", type=int, default=100000)
    ap.add_argument("--timeout", type=float, default=120.0, help="seconds allowed for one batch")
    ap.add_argument("--allocations", action="store_true", help="also count Roc allocations (builds bench/roc/alloc.roc)")
    ap.add_argument("--fail-on-error", action="store_true", help="exit 1 if any roc-parser measurement errored")
    ap.add_argument("--corpus-only", action="store_true", help="write the corpus and its manifest, then stop")
    args = ap.parse_args(argv)
    args.repetitions = args.repetitions or (3 if args.quick else 7)
    args.target_ms = args.target_ms or (20.0 if args.quick else 200.0)
    args.label = args.label or ("quick" if args.quick else "full")
    if args.repetitions < 1 or args.target_ms <= 0 or args.max_iterations < 1:
        ap.error("repetitions, target-ms and max-iterations must be positive")
    args.formats = tuple(f for f in args.formats.split(",") if f)
    unknown = set(args.formats) - set(FORMATS)
    if unknown:
        ap.error(f"unknown formats {sorted(unknown)}")
    args.langs = tuple(x for x in args.langs.split(",") if x)
    return args


def select(docs: list[Doc], only: str) -> list[Doc]:
    keep = {x for x in only.split(",") if x}
    return [d for d in docs if not keep or d.name in keep or d.kind in keep or d.id in keep]


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    out = args.output_dir.resolve() / args.label
    out.mkdir(parents=True, exist_ok=True)

    def log(message: str) -> None:
        print(message, file=sys.stderr, flush=True)

    docs = select(build_corpus(args.quick, args.formats), args.only)
    manifest = write_corpus(docs, out / "corpus")
    (out / "corpus" / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    if args.corpus_only:
        return 0

    _install_wait_hook()
    builder = Builder(out, args.roc, args.python, log)
    impls: list[Impl] = []
    for lang in args.langs:
        impls += getattr(builder, f"{lang}_impls")(args.formats)
    alloc = builder.alloc_binary() if args.allocations and "roc" in args.langs else None

    metadata = {
        "schema": SCHEMA_VERSION, "label": args.label, "quick": args.quick,
        "date": datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds"),
        "machine": machine(), "git": git_state(), "environment": environment(), "versions": builder.versions,
        "skipped": builder.skipped, "repetitions": args.repetitions, "target_ms": args.target_ms,
        "implementations": [{"id": i.id, "format": i.format, "notes": i.notes} for i in impls],
        "corpus": manifest,
        "measurement": "in-process parse loop around an in-memory document; stdin, process start and "
                       "output are outside the timed region; median of per-parse time over batches",
    }
    results: list[dict] = []
    report = {"metadata": metadata, "results": results, "comparison": []}
    for doc in docs:
        for impl in (i for i in impls if i.format == doc.format):
            record = measure(impl, doc, args)
            if alloc and impl.lang == "roc":
                record.update(count_allocations(alloc, doc, out / "logs", args.timeout))
            results.append(record)
            log(f"{doc.id:40} {impl.id:28} " + (f"error: {record['error'][:80]}" if "error" in record else
                f"{fmt_ns(record['median_ns']):>10} {record['mb_per_s']:8.3g} MB/s ±{record['spread_pct']:.0f}%"))
            report["comparison"] = comparison(results)
            (out / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    (out / "report.md").write_text(markdown_report(report))
    log(f"wrote {out / 'results.json'} and {out / 'report.md'}")
    roc_errors = [r for r in results if r["lang"] == "roc" and "error" in r]
    roc_missing = [s for s in builder.skipped if s.startswith("roc-")]
    return 1 if args.fail_on_error and (roc_errors or roc_missing) else 0


if __name__ == "__main__":
    sys.exit(main())
