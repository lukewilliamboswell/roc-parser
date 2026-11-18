"""Corpora and drivers for bounded record/event consumption benchmarks."""
from __future__ import annotations

import json

FORMATS = ("json", "xml", "http")


def component_records(source: str) -> list[str]:
    """Extract top-level SBOM component spans without rewriting number text."""
    decoder = json.JSONDecoder(parse_int=str, parse_float=str)
    pos = source.index("{") + 1
    while True:
        pos = skip_space(source, pos)
        key, pos = decoder.raw_decode(source, pos)
        pos = skip_space(source, pos)
        if source[pos] != ":":
            raise ValueError("expected an SBOM member separator")
        pos = skip_space(source, pos + 1)
        if key == "components":
            if source[pos] != "[":
                raise ValueError("SBOM components must be an array")
            records = []
            pos = skip_space(source, pos + 1)
            while source[pos] != "]":
                start = pos
                _, pos = decoder.raw_decode(source, pos)
                records.append(source[start:pos])
                pos = skip_space(source, pos)
                if source[pos] == "]":
                    break
                if source[pos] != ",":
                    raise ValueError("expected a component separator")
                pos = skip_space(source, pos + 1)
            return records
        _, pos = decoder.raw_decode(source, pos)
        pos = skip_space(source, pos)
        if source[pos] != ",":
            raise ValueError("SBOM has no top-level components array")
        pos += 1


def skip_space(source: str, pos: int) -> int:
    while pos < len(source) and source[pos] in " \t\r\n":
        pos += 1
    return pos


def documents(api, quick: bool, formats: tuple[str, ...]):
    docs = []
    for doc in api.build_corpus(quick, formats):
        if doc.format != "json":
            docs.append(doc)
        elif doc.kind in api.SIZES:
            records = component_records(doc.data.decode())
            for mode, data in (("array", "[" + ",".join(records) + "]\n"),
                               ("ndjson", "\n".join(records) + "\n")):
                docs.append(api.Doc("json", f"{mode}-{doc.kind}", doc.kind, data.encode()))
    if "http" in formats:
        sizes = {"small": api.SIZES["small"]} if quick else api.SIZES
        for size, count in sizes.items():
            data = api.fill(count, lambda i: f"GET /docs/{i} HTTP/1.1\r\n{api.BROWSER_HEADERS}\r\n")
            docs.append(api.Doc("http", f"pipeline-{size}", size, data.encode()))
    return docs


def roc_implementations(api, builder, formats, chunk_bytes):
    builder.versions["roc"] = api.tool_version([builder.roc, "version"])
    result = []
    for fmt in formats:
        driver = {"json": "json-stream", "xml": "xml-events", "http": "http-stream"}[fmt]
        binary = builder.out / "bin" / f"roc-{driver}"
        binary.parent.mkdir(parents=True, exist_ok=True)
        if not builder.run(f"roc-{driver}", [builder.roc, "build", *builder.roc_flags,
                str(api.BENCH / "roc" / f"{driver}.roc"), f"--output={binary}"]):
            continue
        modes = {"json": (("array", str(chunk_bytes)), ("ndjson", str(chunk_bytes))),
                 "xml": (("chunked", str(chunk_bytes)), ("reader",)),
                 "http": ((str(chunk_bytes), "auto"),)}[fmt]
        for mode in modes:
            name = "roc-reader-consume" if mode[0] == "reader" else "roc-parser-consume"
            result.append(api.Impl("roc", name, fmt, [str(binary), *mode],
                f"consume: {fmt} {' '.join(mode)}; preloaded source, discard completed values/events; {builder.optimization} build --debug"))
    return result


def references(api, languages, formats):
    if "rust" not in languages:
        return []
    names = {"json": (("serde-json-array-consume", "serde_json SeqAccess; owned ordered lossless items, discarded"),
                      ("serde-json-ndjson-consume", "serde_json per-line owned ordered lossless values, discarded")),
             "xml": (("quick-xml-consume", "quick-xml borrowed events; decoded/coalesced XML text checksum, no tree"),),
             "http": (("httparse-consume", "httparse borrowed heads; pipeline/body/trailer framing, no retained messages"),)}
    return [api.Impl("rust", name, fmt, [], "consume: " + notes)
            for fmt in formats for name, notes in names[fmt]]


def accepts(impl, doc):
    if doc.format != "json":
        return True
    mode = "ndjson" if "ndjson" in impl.name or "ndjson" in impl.command else "array"
    return doc.name.startswith(mode + "-")


class _Number(str):
    pass


class _Object(list):
    pass


def json_checksum(doc, serde: bool) -> int:
    """Independent untimed model for serde's documented number normalization."""
    def walk(value):
        if isinstance(value, _Number):
            spelling = str(value)
            if serde:
                if not any(ch in spelling for ch in ".eE"):
                    number = int(spelling)
                    if -(2 ** 63) <= number <= 2 ** 64 - 1:
                        spelling = str(number)
                for marker in ("e", "E"):
                    if marker in spelling:
                        mantissa, exponent = spelling.split(marker)
                        spelling = mantissa + "e" + ("" if exponent.startswith(("+", "-")) else "+") + exponent
                        break
            return 3 + len(spelling)
        if isinstance(value, _Object):
            return 7 + sum(len(key.encode()) + walk(item) for key, item in value)
        if isinstance(value, list):
            return 6 + sum(map(walk, value))
        if isinstance(value, str):
            return 5 + len(value.encode())
        return 1 if value is None else 2

    options = dict(parse_int=_Number, parse_float=_Number, object_pairs_hook=_Object)
    source = doc.data.decode()
    if doc.name.startswith("ndjson-"):
        return sum(walk(json.loads(line, **options)) for line in source.split("\n") if line.strip(" \t\r"))
    return walk(json.loads(source, **options))


def qualify_checksum(impl, doc, record):
    if doc.format != "json" or "error" in record or not record.get("accepted"):
        return record
    expected = json_checksum(doc, serde=impl.lang == "rust")
    qualified = record["checksum"] == expected * record["iterations"]
    record = {**record, "expected_checksum": expected, "checksum_qualified": qualified,
              "checksum_model": "serde-number-spelling" if impl.lang == "rust" else "original-number-spelling"}
    if not qualified:
        record["error"] = "JSON consume checksum differs from the independent input model"
    return record
