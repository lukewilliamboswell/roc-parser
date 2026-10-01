#!/usr/bin/env python3
"""HTTP/1.1 message parsing review against the h11 reference parser.

Every case in scripts/http/cases.json is fed to the Roc probe
(scripts/http/probe.roc) and to h11 (pinned in scripts/http/requirements.txt),
and the normalized results are compared: accept/reject verdict, start line,
field lines, decoded body, and the bytes left for the next message.

Where this library deliberately follows RFC 9112 more strictly than h11 (h11
is lenient about obs-fold, bare LF, control characters in field values, ...),
the case is listed in scripts/http/known-failures.json with the reason and
the RFC section or llhttp behaviour it follows. Discovery never edits the
baseline: a new divergence fails the run, and a listed case that starts to
agree is reported as stale.

    python3 -m venv .roc-parser-tmp/http/venv
    .roc-parser-tmp/http/venv/bin/pip install -r scripts/http/requirements.txt
    .roc-parser-tmp/http/venv/bin/python scripts/review_http.py [--corpus DIR]

`--corpus DIR` additionally cross-checks raw fuzz corpus files (the first
byte selects the parser: Q request, S response) and prints a disagreement
summary without consulting the baseline.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import Any

import h11

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "scripts" / "http"
WORK = ROOT / ".roc-parser-tmp" / "http"
ROC = Path.home() / "roc_nightly-macos_apple_silicon-2026-09-29-7f11a82" / "roc"
METHODS = {
    b"OPTIONS": "Options", b"GET": "Get", b"POST": "Post", b"PUT": "Put", b"DELETE": "Delete",
    b"HEAD": "Head", b"TRACE": "Trace", b"CONNECT": "Connect", b"PATCH": "Patch",
}


def build_probe(roc: Path) -> Path:
    out = WORK / "probe"
    WORK.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(roc), "build", str(DATA / "probe.roc"), f"--output={out}"], check=True, cwd=ROOT,
                   stdout=subprocess.DEVNULL)
    return out


def run_probe(probe: Path, mode: str, data: bytes) -> dict[str, Any]:
    proc = subprocess.run([str(probe)], input=mode.encode() + data, capture_output=True, timeout=20)
    if proc.returncode != 0:
        return {"status": "crash", "message": (proc.stdout + proc.stderr).decode(errors="replace")[-300:]}
    result = json.loads(proc.stdout)
    if result["status"] == "error":
        return {"status": "error"}
    return result


def h11_request(data: bytes) -> dict[str, Any]:
    conn = h11.Connection(h11.SERVER)
    conn.receive_data(data)
    return collect(conn, data, request=True)


def h11_response(data: bytes) -> dict[str, Any]:
    conn = h11.Connection(h11.CLIENT)
    conn.send(h11.Request(method="GET", target="/", headers=[("Host", "example")]))
    conn.send(h11.EndOfMessage())
    conn.receive_data(data)
    return collect(conn, data, request=False)


def collect(conn: h11.Connection, data: bytes, request: bool) -> dict[str, Any]:
    head: Any = None
    body = b""
    try:
        while True:
            event = conn.next_event()
            if event is h11.NEED_DATA:
                if head is not None and not request and conn.their_state is h11.SEND_BODY:
                    # Responses without framing are delimited by connection close.
                    conn.receive_data(b"")
                    continue
                return {"status": "error"}
            if isinstance(event, h11.InformationalResponse):
                head = event
                break
            if isinstance(event, (h11.Request, h11.Response)):
                head = event
            elif isinstance(event, h11.Data):
                body += event.data
            elif isinstance(event, h11.EndOfMessage):
                break
            elif isinstance(event, h11.ConnectionClosed):
                break
    except h11.ProtocolError:
        return {"status": "error"}
    if head is None:
        return {"status": "error"}
    rest = conn.trailing_data[0] if not isinstance(head, h11.InformationalResponse) else None
    out: dict[str, Any] = {
        "status": "ok",
        "version": head.http_version.decode(),
        "headers": [[n.decode("utf-8", "replace"), v.decode("utf-8", "replace")] for n, v, in
                    ((raw, value) for raw, _, value in head.headers._full_items)],
        "body": body.hex(),
    }
    if rest is not None:
        out["rest"] = bytes(rest).hex()
    if request:
        out["method"] = METHODS.get(head.method, head.method.decode(errors="replace"))
        out["target"] = head.target.decode(errors="replace")
    else:
        out["code"] = head.status_code
        out["reason"] = head.reason.decode("utf-8", "replace")
    return out


def h11_normalized_headers(headers: list[list[str]]) -> list[list[str]]:
    """Apply h11's own rewriting of framing fields (it keeps one Content-Length
    with a single value and lowercases Transfer-Encoding) so only semantic
    differences are reported."""
    out, seen_length = [], False
    for name, value in headers:
        lower = name.lower()
        if lower == "content-length":
            if seen_length:
                continue
            seen_length = True
            value = value.split(",")[0].strip(" \t")
        elif lower == "transfer-encoding":
            value = value.lower()
        out.append([name, value])
    return out


def compare(mine: dict[str, Any], oracle: dict[str, Any]) -> bool:
    if mine["status"] != oracle["status"]:
        return False
    if mine["status"] != "ok":
        return True
    mine = dict(mine, headers=h11_normalized_headers(mine["headers"]))
    return all(mine.get(key) == value for key, value in oracle.items())


def oracle_for(mode: str, data: bytes) -> dict[str, Any]:
    return h11_request(data) if mode == "Q" else h11_response(data)


def review(probe: Path) -> int:
    cases = json.loads((DATA / "cases.json").read_text())
    known = json.loads((DATA / "known-failures.json").read_text())
    failures, stale, crashes = [], [], []
    for case in cases:
        data = case["input"].encode("latin-1")
        mine = run_probe(probe, case["mode"], data)
        oracle = oracle_for(case["mode"], data)
        if mine["status"] == "crash":
            crashes.append((case["id"], mine["message"]))
            continue
        agree = compare(mine, oracle)
        listed = case["id"] in known
        if "expect" in case and mine["status"] != case["expect"]:
            failures.append((case["id"], f"expected {case['expect']}", mine, oracle))
        elif not agree and not listed:
            failures.append((case["id"], "disagrees with h11", mine, oracle))
        elif agree and listed:
            stale.append(case["id"])
    for case_id, message in crashes:
        print(f"CRASH {case_id}: {message}")
    for case_id, why, mine, oracle in failures:
        print(f"FAIL {case_id}: {why}\n  roc: {json.dumps(mine)}\n  h11: {json.dumps(oracle)}")
    for case_id in stale:
        print(f"STALE known failure (now agrees with h11): {case_id}")
    print(f"{len(cases)} cases, {len(known)} known divergences, {len(failures)} failures, "
          f"{len(crashes)} crashes, {len(stale)} stale (h11 {h11.__version__})")
    return 1 if failures or crashes or stale else 0


def corpus(probe: Path, directory: Path) -> int:
    counts: dict[str, int] = {}
    examples: dict[str, bytes] = {}
    for path in sorted(directory.iterdir()):
        raw = path.read_bytes()
        if not raw or chr(raw[0]) not in "QS":
            continue
        mode, data = chr(raw[0]), raw[1:]
        mine, oracle = run_probe(probe, mode, data), oracle_for(mode, data)
        key = "agree" if compare(mine, oracle) else f"{mode} roc={mine['status']} h11={oracle['status']}"
        counts[key] = counts.get(key, 0) + 1
        examples.setdefault(key, data)
    for key, count in sorted(counts.items()):
        print(f"{count:6d}  {key}  e.g. {examples[key][:120]!r}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--roc", type=Path, default=ROC)
    parser.add_argument("--corpus", type=Path)
    args = parser.parse_args()
    probe = build_probe(args.roc)
    if args.corpus:
        return corpus(probe, args.corpus)
    return review(probe)


if __name__ == "__main__":
    sys.exit(main())
