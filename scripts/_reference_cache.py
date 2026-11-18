"""Validated benchmark measurements, distinct from deterministic oracle answers."""

from __future__ import annotations

import datetime
import hashlib
import json
import math
import os
from pathlib import Path
import statistics
import tempfile

SCHEMA = 1


class ReferenceUnavailable(Exception):
    """A reference cannot be reused without an explicit refresh."""


def digest(value: object) -> str:
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":"),
                                     allow_nan=False).encode()).hexdigest()


def _integer(value: object, minimum: int = 0) -> bool:
    return type(value) is int and value >= minimum


def _finite(value: object, minimum: float = 0) -> bool:
    return type(value) in (int, float) and math.isfinite(value) and value >= minimum


def validate_measurement(record: dict, identity: dict) -> None:
    """Require complete, internally consistent original measurement receipts."""
    if not isinstance(record, dict) or "error" in record:
        raise ReferenceUnavailable("reference measurement was not qualified")
    impl, document, settings = (identity[key] for key in ("implementation", "document", "settings"))
    for field, expected in {"impl": impl["id"], "lang": impl["lang"], "library": impl["name"],
                            "workload": impl["workload"], "doc": document["id"],
                            "kind": document["kind"], "bytes": document["bytes"]}.items():
        if record.get(field) != expected:
            raise ReferenceUnavailable(f"reference measurement has incompatible {field}")
    iterations = record.get("iterations")
    if not _integer(iterations, 1) or iterations > settings["max_iterations"]:
        raise ReferenceUnavailable("invalid reference iteration count")
    if type(record.get("accepted")) is not bool or not _integer(record.get("checksum")):
        raise ReferenceUnavailable("invalid reference acceptance or checksum")
    receipts = record.get("receipts")
    if not isinstance(receipts, dict):
        raise ReferenceUnavailable("missing original measurement receipts")
    command = receipts.get("command")
    if not isinstance(command, list) or not command or not all(isinstance(arg, str) for arg in command):
        raise ReferenceUnavailable("missing original comparator command")
    try:
        timestamp = datetime.datetime.fromisoformat(receipts["measured_at"])
        if timestamp.tzinfo is None:
            raise ValueError("missing timezone")
    except (KeyError, TypeError, ValueError):
        raise ReferenceUnavailable("missing original measurement timestamp") from None
    batches = receipts.get("batches")
    if not isinstance(batches, list) or len(batches) != settings["repetitions"]:
        raise ReferenceUnavailable("missing original measurement batches")
    expected_successes = iterations if record["accepted"] else 0
    for index, batch in enumerate([receipts.get("calibration"), receipts.get("warmup"), *batches]):
        count = 1 if index == 0 else iterations
        if not isinstance(batch, dict) or not _integer(batch.get("iterations"), 1) or batch["iterations"] != count:
            raise ReferenceUnavailable("invalid original batch iterations")
        if not _integer(batch.get("elapsed_ns"), 1) or not _integer(batch.get("checksum")):
            raise ReferenceUnavailable("invalid original batch timing or checksum")
        if not _integer(batch.get("successes")) or batch["successes"] != (count if record["accepted"] else 0):
            raise ReferenceUnavailable("unstable original reference acceptance")
        if index > 0 and (batch["successes"] != expected_successes or batch["checksum"] != record["checksum"]):
            raise ReferenceUnavailable("unstable original reference checksum")
        rss = batch.get("peak_rss_bytes")
        if rss is not None and not _integer(rss, 1):
            raise ReferenceUnavailable("invalid original batch RSS")
    if receipts["calibration"]["checksum"] * iterations != record["checksum"]:
        raise ReferenceUnavailable("calibration checksum differs from measured batches")
    times = [batch["elapsed_ns"] / iterations for batch in batches]
    median = statistics.median(times)
    expected_summary = {
        "median_ns": median, "min_ns": min(times), "max_ns": max(times),
        "stdev_ns": statistics.stdev(times) if len(times) > 1 else 0.0,
        "spread_pct": 100 * (max(times) - min(times)) / median,
        "mb_per_s": (document["bytes"] / 1e6) / (median / 1e9),
    }
    for field, expected in expected_summary.items():
        if not _finite(record.get(field)) or not math.isclose(record[field], expected, rel_tol=1e-10, abs_tol=1e-10):
            raise ReferenceUnavailable(f"inconsistent original reference {field}")
    if record.get("batch_elapsed_ns") != [batch["elapsed_ns"] for batch in batches]:
        raise ReferenceUnavailable("inconsistent original batch receipt")
    rss = [batch["peak_rss_bytes"] for batch in batches if batch.get("peak_rss_bytes")]
    if record.get("peak_rss_bytes") != (max(rss) if rss else None):
        raise ReferenceUnavailable("inconsistent original RSS receipt")


class ReferenceCache:
    def __init__(self, folder: Path):
        self.folder = folder

    def path(self, identity: dict) -> Path:
        return self.folder / f"{digest(identity)}.json"

    def load(self, identity: dict) -> tuple[dict, dict]:
        path = self.path(identity)
        try:
            envelope = json.loads(path.read_text())
        except FileNotFoundError:
            raise ReferenceUnavailable("no compatible cached measurement; use --refresh-references") from None
        except (OSError, UnicodeError, ValueError):
            raise ReferenceUnavailable("cached reference is unreadable or corrupt; use --refresh-references") from None
        try:
            payload = {key: envelope[key] for key in ("schema", "key", "identity", "record", "provenance")}
            if payload["schema"] != SCHEMA or payload["key"] != digest(identity) or payload["identity"] != identity:
                raise ReferenceUnavailable("cached reference identity is stale")
            if envelope.get("payload_sha256") != digest(payload):
                raise ReferenceUnavailable("cached reference payload is corrupt")
            if not isinstance(payload["provenance"], dict) or not payload["provenance"].get("report") or not payload["provenance"].get("git"):
                raise ReferenceUnavailable("cached reference lacks original provenance")
            validate_measurement(payload["record"], identity)
            if payload["provenance"].get("measured_at") != payload["record"]["receipts"]["measured_at"]:
                raise ReferenceUnavailable("original provenance timestamp disagrees with receipts")
            return payload["record"], payload["provenance"]
        except (KeyError, TypeError, ValueError, OverflowError):
            raise ReferenceUnavailable("cached reference is incomplete or unqualified") from None

    def save(self, identity: dict, record: dict, provenance: dict) -> None:
        validate_measurement(record, identity)
        payload = {"schema": SCHEMA, "key": digest(identity), "identity": identity,
                   "record": record, "provenance": provenance}
        envelope = {**payload, "payload_sha256": digest(payload)}
        self.folder.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(mode="w", dir=self.folder, prefix=".reference-", delete=False) as temporary:
            temporary.write(json.dumps(envelope, indent=2, allow_nan=False) + "\n")
            name = temporary.name
        try:
            os.replace(name, self.path(identity))
        finally:
            Path(name).unlink(missing_ok=True)
