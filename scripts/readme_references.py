"""Cached collection references for the representative README workloads.

Only explicit refresh builds or executes reference parsers. Fastest-reference
selection keeps ownership and semantic qualifications and never invents ratios.
"""
from __future__ import annotations

from pathlib import Path

try:
    from . import bench
    from ._reference_cache import ReferenceCache, ReferenceUnavailable, digest, validate_measurement
except ImportError:
    import bench
    from _reference_cache import ReferenceCache, ReferenceUnavailable, digest, validate_measurement


LANGUAGES = ("rust", "go", "python")
COLLECTION_WORKLOADS = {"collect", "collect-borrowed"}


def qualify_measurement(record: dict, identity: dict, max_spread_pct: float) -> None:
    validate_measurement(record, identity)
    if not record["accepted"]:
        raise ReferenceUnavailable("reference rejected the representative input")
    if record["spread_pct"] > max_spread_pct:
        raise ReferenceUnavailable(f"reference spread exceeds {max_spread_pct:g}%; explicitly refresh on an idle core")


def qualify_evidence(record: dict, evidence: dict) -> None:
    expected = evidence.get("expected_checksum")
    if expected is not None and (type(expected) is not int or expected < 0
            or record["checksum"] != expected * record["iterations"]):
        raise ReferenceUnavailable("timed reference checksum differs from its canonical qualification evidence")


def qualification_labels(impl: bench.Impl, record: dict, roc_record: dict | None, qualification: dict, fresh_roc_check: dict | None = None) -> dict:
    ownership = qualification.get("ownership") or (
        "borrowed strings" if impl.name == "qif_parser" else
        "borrowed read-only DOM" if impl.name == "roxmltree" else "collection; see comparator notes")
    borrowed = impl.workload == "collect-borrowed" or impl.name == "roxmltree" or "borrow" in ownership.lower()
    contract = qualification.get("checksum_semantics")
    if contract and contract not in {"bench-model-checksum", "same-as-roc"}:
        semantic_status = "different_checksum_contract"
        semantic_note = "different model/checksum contract; semantic equivalence is not established"
    elif roc_record and "checksum" in roc_record and "iterations" in roc_record:
        equal = record["checksum"] * roc_record["iterations"] == roc_record["checksum"] * record["iterations"]
        semantic_status = "checksum_equal" if equal else "checksum_differs"
        semantic_note = "result checksum agrees" if equal else "different result checksum; semantic equivalence is not established"
    else:
        semantic_status = "not_checked"
        semantic_note = "no successful Roc result for a checksum comparison"
    if qualification.get("status") == "different":
        semantic_status = "different"
        semantic_note = "reference qualification reports different semantics"
    if fresh_roc_check and fresh_roc_check.get("status") in {"match", "different"}:
        same = fresh_roc_check["status"] == "match"
        semantic_status = "canonical_equal" if same else "canonical_differs"
        semantic_note = ("fresh Roc canonical model matches cached reference evidence" if same else
                         "fresh Roc canonical model differs from reference evidence")
    elif fresh_roc_check and fresh_roc_check.get("status") == "failed":
        semantic_status = "fresh_check_failed"
        semantic_note = "fresh Roc semantic check failed; equivalence is not established"
    if impl.name == "qif_parser":
        semantic_note += "; amounts use f64, unlike Roc exact monetary values"
    return {"ownership": ownership, "ownership_differs": borrowed,
            "semantic_status": semantic_status, "semantic_note": semantic_note,
            "qualification": qualification, "fresh_roc_check": fresh_roc_check, "notes": impl.notes}


def fastest_references(results: list[dict]) -> dict[str, dict]:
    """Choose the fastest qualified collection result, including labeled borrowed models."""
    fastest = {}
    for row in results:
        if "error" in row or not row.get("accepted") or row.get("workload") not in COLLECTION_WORKLOADS:
            continue
        fmt = row["format"]
        if fmt not in fastest or row["median_ns"] < fastest[fmt]["median_ns"]:
            fastest[fmt] = row
    return fastest


class ReferenceRun:
    def __init__(self, builder: bench.Builder, args, documents: list[bench.Doc], extra=None):
        self.builder = builder
        self.args = args
        self.documents = documents
        self.extra = extra
        self.formats = tuple(dict.fromkeys(doc.format for doc in documents if doc.format in bench.FORMATS))
        self.standard = [impl for impl in bench.reference_specs(LANGUAGES, self.formats)
                         if impl.workload in COLLECTION_WORKLOADS]
        self.additional = [impl for impl in extra.specs(args.python)
                           if impl.workload in COLLECTION_WORKLOADS
                           and impl.format in {doc.format for doc in documents}] if extra else []
        self.specs = self.standard + self.additional
        self.fresh = {}
        self.cache = ReferenceCache(Path(args.reference_cache_dir).resolve())

    def build(self) -> None:
        """Called before pinning: default reuse performs no reference builds."""
        if not self.args.refresh_references:
            return
        for lang in LANGUAGES:
            if not any(impl.lang == lang for impl in self.standard):
                continue
            implementations = getattr(self.builder, f"{lang}_impls")(self.formats)
            self.fresh.update({(impl.id, impl.format): impl for impl in implementations})
        if self.extra and self.additional:
            formats = tuple(dict.fromkeys(impl.format for impl in self.additional))
            self.fresh.update({(impl.id, impl.format): impl for impl in self.extra.build(self.builder, formats)})

    def collect(self, roc_results: list[dict], metadata: dict) -> tuple[list[dict], dict]:
        """Called with the exact timing CPU affinity already set by the README runner."""
        machine = bench.reference_machine()
        profiles, errors = {}, {}
        standard_keys = {(impl.id, impl.format) for impl in self.standard}
        for impl in self.specs:
            key = impl.lang if (impl.id, impl.format) in standard_keys else (impl.id, impl.format)
            if key in profiles or key in errors:
                continue
            try:
                profiles[key] = (bench.reference_profile(impl.lang, self.args, machine) if key == impl.lang
                                 else self.extra.profile(impl, self.args, machine))
            except (ReferenceUnavailable, OSError, ValueError, RuntimeError) as error:
                errors[key] = str(error)
        roc_by_format = {row["doc"].split("/", 1)[0]: row for row in roc_results}
        results = []
        for document in self.documents:
            implementations = [impl for impl in self.specs if impl.format == document.format]
            if not implementations:
                results.append({"format": document.format, "doc": document.id,
                                "status": "ReferenceUnavailable", "error": "ReferenceUnavailable: no collection comparator configured"})
                continue
            for impl in implementations:
                key = impl.lang if (impl.id, impl.format) in standard_keys else (impl.id, impl.format)
                identity = None
                try:
                    if key in errors:
                        raise ReferenceUnavailable(errors[key])
                    identity = bench.reference_identity(impl, document, self.args, profiles[key])
                    if self.args.refresh_references:
                        actual = self.fresh.get((impl.id, impl.format))
                        if actual is None:
                            raise ReferenceUnavailable(self.builder.skipped.get(impl.lang, "reference build failed"))
                        qualifier = getattr(self.extra, "qualification", None) if key != impl.lang else None
                        qualification = qualifier(actual, document, self.args, self.builder) if qualifier else {}
                        if qualification.get("status") in {"unsupported", "failed"}:
                            raise ReferenceUnavailable(str(qualification))
                        record = bench.measure(actual, document, self.args)
                        qualify_measurement(record, identity, self.args.max_spread_pct)
                        qualify_evidence(record, qualification)
                        provenance = {"report": str(self.args.work / "results.json"), "git": metadata["git"],
                                      "measured_at": record["receipts"]["measured_at"], "qualification": qualification}
                        self.cache.save(identity, record, provenance)
                        status = "refreshed"
                    else:
                        record, provenance = self.cache.load(identity)
                        qualify_measurement(record, identity, self.args.max_spread_pct)
                        qualification = provenance.get("qualification", {})
                        if key != impl.lang and (not qualification or qualification.get("status") in {"unsupported", "failed"}):
                            raise ReferenceUnavailable("extra reference lacks a successful original semantic qualification")
                        status = "hit"
                    qualify_evidence(record, qualification)
                    fresh_check = None
                    checker = getattr(self.extra, "fresh_roc_check", None) if key != impl.lang else None
                    if checker and qualification.get("status") != "different":
                        try:
                            fresh_check = checker(impl, document, self.args, self.builder, qualification)
                        except (ReferenceUnavailable, OSError, ValueError, RuntimeError) as error:
                            fresh_check = {"status": "failed", "notes": [str(error)]}
                    record = {**record, "format": document.format, "input_sha256": document.sha256,
                              "reference_cache": {"status": status, "key": digest(identity), "provenance": provenance},
                              **qualification_labels(impl, record, roc_by_format.get(document.format), qualification, fresh_check)}
                except (ReferenceUnavailable, OSError, ValueError, RuntimeError) as error:
                    record = {**bench.reference_unavailable(impl, document, str(error), identity),
                              "format": document.format, "input_sha256": document.sha256, "notes": impl.notes}
                results.append(record)
        return results, fastest_references(results)
