#!/usr/bin/env python3
"""Python comparators for scripts/bench.py.

Usage: bench_python.py <impl> <iterations> < document

Speaks the same protocol as the Roc drivers in bench/roc/: the document is read
from stdin before the clock starts, parsed <iterations> times into an in-memory
structure, and one JSON line is printed. The file lives under scripts/ because
repository policy keeps every Python file there; see docs/benchmarks.adoc.
"""
from __future__ import annotations

import csv
import io
import json
import sys
import time
import xml.etree.ElementTree as ElementTree


def parse_csv(text: str) -> int:
    rows = list(csv.reader(io.StringIO(text, newline=""), strict=True))
    return sum(1 + sum(len(field) for field in row) for row in rows)


def yaml_walk(value) -> int:
    if isinstance(value, dict):
        return 7 + sum(len(str(k)) + yaml_walk(v) for k, v in value.items())
    if isinstance(value, list):
        return 6 + sum(yaml_walk(v) for v in value)
    if isinstance(value, str):
        return 5 + len(value)
    return 1


def parse_yaml_with(loader_name: str):
    import yaml

    loader = getattr(yaml, loader_name)

    def parse(text: str) -> int:
        return yaml_walk(yaml.load(text, Loader=loader))

    return parse


def xml_walk(element) -> int:
    total = len(element.tag) + len(element.text or "") + len(element.tail or "")
    total += sum(len(k) + len(v) for k, v in element.attrib.items())
    return total + sum(xml_walk(child) for child in element)


def parse_xml(text: str) -> int:
    return xml_walk(ElementTree.fromstring(text))


def markdown_parser():
    from markdown_it import MarkdownIt

    md = MarkdownIt("commonmark").enable("table").enable("strikethrough")

    def parse(text: str) -> int:
        return len(md.parse(text)) + 1

    return parse


def http_parser():
    import h11

    def parse(text: str) -> int:
        data = text.encode("utf-8")
        role = h11.CLIENT if data.startswith(b"HTTP/") else h11.SERVER
        conn = h11.Connection(our_role=role)
        if role is h11.CLIENT:
            # A client must send a request before it can read a response.
            conn.send(h11.Request(method="GET", target="/", headers=[("Host", "x")]))
            conn.send(h11.EndOfMessage())
        conn.receive_data(data)
        conn.receive_data(b"")
        total, body = 0, bytearray()
        while True:
            event = conn.next_event()
            if isinstance(event, (h11.Request, h11.Response)):
                total += len(event.headers)
            elif isinstance(event, h11.Data):
                body += event.data
            elif event is h11.NEED_DATA or isinstance(event, (h11.EndOfMessage, h11.ConnectionClosed)):
                break
        return total + len(body) + 1

    return parse


def implementation(name: str):
    if name == "csv":
        return parse_csv
    if name == "pyyaml":
        return parse_yaml_with("SafeLoader")
    if name == "pyyaml-libyaml":
        return parse_yaml_with("CSafeLoader")
    if name == "etree":
        return parse_xml
    if name == "markdown-it-py":
        return markdown_parser()
    if name == "h11":
        return http_parser()
    raise SystemExit(f"unknown implementation {name}")


def main() -> None:
    parse = implementation(sys.argv[1])
    iterations = int(sys.argv[2])
    text = sys.stdin.buffer.read().decode("utf-8")
    checksum = successes = 0
    start = time.perf_counter_ns()
    for _ in range(iterations):
        try:
            checksum += parse(text)
            successes += 1
        except Exception:  # noqa: BLE001 - a rejection is a measured outcome
            checksum += 1
    elapsed = time.perf_counter_ns() - start
    print(json.dumps({"iterations": iterations, "elapsed_ns": max(elapsed, 1),
                      "successes": successes, "checksum": checksum}))


if __name__ == "__main__":
    main()
