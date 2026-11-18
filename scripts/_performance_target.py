"""Evaluate elapsed-time targets without treating missing/noisy data as a pass."""
from __future__ import annotations

import math


def evaluate(results: list[dict], maximum: float, spread_limit: float = 25) -> list[dict]:
    rows = []
    for mine in results:
        if mine["lang"] != "roc" or mine["kind"] == "pathological":
            continue
        row = {"doc": mine["doc"], "implementation": mine["impl"], "maximum_slowdown": maximum}
        if "error" in mine or not mine.get("accepted"):
            rows.append({**row, "status": "failed", "reason": "Roc did not successfully consume the input"})
            continue
        if mine.get("spread_pct", math.inf) > spread_limit:
            rows.append({**row, "status": "unqualified", "reason": "Roc timing spread exceeds the limit"})
            continue
        workload = mine.get("workload", "collect")
        candidates = [r for r in results if r["doc"] == mine["doc"] and r["lang"] != "roc"
                      and r.get("workload", "collect") in ({"consume"} if workload == "consume" else {"collect", "collect-borrowed"})]
        if any("error" in r for r in candidates):
            rows.append({**row, "status": "unqualified", "reason": "the configured reference set is incomplete"})
            continue
        references = [r for r in candidates if r.get("accepted") and
                      math.isfinite(r.get("median_ns", math.inf)) and r.get("median_ns", 0) > 0]
        if not references:
            rows.append({**row, "status": "unqualified", "reason": "no successful reference with qualified timing"})
            continue
        fastest = min(references, key=lambda r: r["median_ns"])
        if fastest.get("spread_pct", math.inf) > spread_limit:
            rows.append({**row, "status": "unqualified", "reference": fastest["impl"],
                         "reason": "the fastest reference timing is noisy; rerun instead of selecting a slower library"})
            continue
        slowdown = mine["median_ns"] / fastest["median_ns"]
        checksum_equal = None
        if all("checksum" in r and "iterations" in r for r in (mine, fastest)):
            checksum_equal = (mine["checksum"] * fastest["iterations"]
                              == fastest["checksum"] * mine["iterations"])
        if workload == "consume" and not (checksum_equal is True or
                (mine.get("checksum_qualified") is True and fastest.get("checksum_qualified") is True)):
            rows.append({**row, "status": "unqualified", "reference": fastest["impl"],
                         "reason": "consumer checksums disagree without independent model qualification"})
            continue
        rows.append({**row, "status": "passed" if slowdown <= maximum else "failed",
                     "reference": fastest["impl"], "slowdown": slowdown,
                     "roc_ns": mine["median_ns"], "reference_ns": fastest["median_ns"],
                     "reference_workload": fastest.get("workload", "collect"),
                     "checksum_equal": checksum_equal})
    return rows
