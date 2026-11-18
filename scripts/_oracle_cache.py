"""Content-addressed deterministic reference answers, independent of Roc builds.

Only producers' returned answers are stored; exceptions are never cached. Oracle
adapters must translate known deterministic parse rejections into answer values
and let import, I/O, timeout and unexpected runtime failures propagate.
"""
from __future__ import annotations

import base64
from decimal import Decimal
import hashlib
import importlib.metadata
import inspect
import json
import math
import os
from pathlib import Path
import platform
import re
import sys
import tempfile

SCHEMA = 1


def canonical(value):
    return json.dumps(value, ensure_ascii=True, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()


def digest(value):
    return hashlib.sha256(canonical(value)).hexdigest()


def pack(value):
    """Tagged JSON preserves byte buffers, Decimal values and dictionary keys."""
    if value is None:
        return ["none"]
    if isinstance(value, bool):
        return ["bool", value]
    if isinstance(value, str):
        return ["str", value]
    if isinstance(value, int):
        return ["int", value]
    if isinstance(value, float) and math.isfinite(value):
        return ["float", value.hex()]
    if isinstance(value, bytes):
        return ["bytes", base64.b64encode(value).decode("ascii")]
    if isinstance(value, Decimal) and value.is_finite():
        return ["decimal", str(value)]
    if isinstance(value, list):
        return ["list", [pack(item) for item in value]]
    if isinstance(value, dict) and all(isinstance(key, str) for key in value):
        return ["dict", [[key, pack(item)] for key, item in sorted(value.items())]]
    raise TypeError(f"unsupported oracle answer type: {type(value).__name__}")


def unpack(node):
    if not isinstance(node, list) or not node or not isinstance(node[0], str):
        raise ValueError("invalid cached answer node")
    kind = node[0]
    if kind == "none" and len(node) == 1:
        return None
    if len(node) != 2:
        raise ValueError("invalid cached answer arity")
    value = node[1]
    if kind == "str" and isinstance(value, str):
        return value
    if kind == "bool" and isinstance(value, bool):
        return value
    if kind == "int" and type(value) is int:
        return value
    if kind == "float" and isinstance(value, str):
        result = float.fromhex(value)
        if math.isfinite(result):
            return result
    if kind == "bytes" and isinstance(value, str):
        return base64.b64decode(value, validate=True)
    if kind == "decimal" and isinstance(value, str):
        result = Decimal(value)
        if result.is_finite():
            return result
    if kind == "list" and isinstance(value, list):
        return [unpack(item) for item in value]
    if kind == "dict" and isinstance(value, list):
        result = {}
        for pair in value:
            if not isinstance(pair, list) or len(pair) != 2 or not isinstance(pair[0], str) or pair[0] in result:
                raise ValueError("invalid cached dictionary")
            result[pair[0]] = unpack(pair[1])
        return result
    raise ValueError("invalid cached answer type")


def adapter_fingerprint(functions):
    """Hash adapter functions and their same-module helper dependencies, not probes."""
    sources = {}
    pending = list(functions)
    while pending:
        function = pending.pop()
        if function.__qualname__ in sources:
            continue
        sources[function.__qualname__] = inspect.getsource(function)
        for name in function.__code__.co_names:
            dependency = function.__globals__.get(name)
            if inspect.isfunction(dependency) and dependency.__module__ == function.__module__:
                pending.append(dependency)
    return digest(sources)


def pinned_versions(requirements):
    if requirements is None:
        return {}, "stdlib"
    data = Path(requirements).read_bytes()
    pins = {}
    for original in data.decode().splitlines():
        line = original.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.fullmatch(r"([A-Za-z0-9_.-]+)==([^\s;]+)", line)
        if not match:
            raise ValueError(f"oracle cache requires exact dependency pins: {original}")
        pins[re.sub(r"[-_.]+", "-", match[1]).lower()] = match[2]
    return pins, hashlib.sha256(data).hexdigest()


def installed_versions(primary, pins):
    """Metadata lookup only; never import or execute an oracle on a hit."""
    if primary is None:
        return {}
    try:
        importlib.metadata.version(primary)
    except importlib.metadata.PackageNotFoundError:
        return None
    versions = {}
    for name in pins:
        try:
            versions[name] = importlib.metadata.version(name)
        except importlib.metadata.PackageNotFoundError:
            versions[name] = "missing"
    return versions


def read_checked(path, identity):
    try:
        entry = json.loads(path.read_bytes())
        if not isinstance(entry, dict) or set(entry) != {"schema", "identity", "payload", "sha256"}:
            return None
        body = {key: entry[key] for key in ("schema", "identity", "payload")}
        if type(entry["schema"]) is not int or entry["schema"] != SCHEMA or entry["identity"] != identity or digest(body) != entry["sha256"]:
            return None
        return entry["payload"]
    except (OSError, ValueError, TypeError, RecursionError):
        return None


def write_checked(path, identity, payload):
    body = {"schema": SCHEMA, "identity": identity, "payload": payload}
    encoded = canonical({**body, "sha256": digest(body)})
    temporary = None
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".oracle-", delete=False) as output:
            temporary = Path(output.name)
            output.write(encoded)
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
        temporary = None
    except OSError:
        # A full/read-only cache must not change the current oracle answer.
        pass
    finally:
        if temporary is not None:
            try:
                temporary.unlink(missing_ok=True)
            except OSError:
                pass


class OracleCache:
    def __init__(self, root, *, enabled=True, refresh=False, version_reader=installed_versions):
        self.root = Path(root)
        self.enabled = enabled
        self.refresh = refresh
        self.version_reader = version_reader
        self.hits = 0
        self.misses = 0
        self.writes = 0
        self._identities = {}

    def answer(self, identifier, data, config, compute, *, adapters, requirements=None, primary=None):
        if not self.enabled:
            self.misses += 1
            return compute()
        if not isinstance(data, bytes):
            raise TypeError("oracle cache inputs must be exact byte buffers")
        pins, requirements_hash = pinned_versions(requirements)
        specification = (identifier, canonical(config), tuple(adapters), requirements_hash, primary)
        if specification not in self._identities:
            base = {"oracle": identifier, "configuration": config, "requirements": requirements_hash,
                    "adapter": adapter_fingerprint(adapters),
                    "python": {"implementation": platform.python_implementation(), "version": list(sys.version_info[:3])}}
            namespace = digest(base)
            receipt_path = self.root / namespace / "versions.json"
            versions = self.version_reader(primary, pins)
            if versions is None:
                versions = read_checked(receipt_path, namespace)
                if versions != pins:
                    versions = pins
            self._identities[specification] = (base, namespace, receipt_path, versions)
        base, namespace, receipt_path, versions = self._identities[specification]
        identity = digest({**base, "versions": versions, "input_sha256": hashlib.sha256(data).hexdigest(), "input_bytes": len(data)})
        path = self.root / namespace / digest(versions) / (identity + ".json")
        if not self.refresh:
            cached = read_checked(path, identity)
            if cached is not None:
                try:
                    result = unpack(cached)
                except (ValueError, TypeError, ArithmeticError, RecursionError):
                    pass
                else:
                    self.hits += 1
                    return result
        self.misses += 1
        result = compute()  # Import, I/O, timeout and other exceptions never reach storage.
        packed = pack(result)
        write_checked(receipt_path, namespace, versions)
        write_checked(path, identity, packed)
        self.writes += 1
        return result

    def statistics(self):
        return {"hits": self.hits, "misses": self.misses, "writes": self.writes}


def add_arguments(parser):
    parser.add_argument("--no-oracle-cache", action="store_true", help="compute oracle answers without reading or writing the reference cache")
    parser.add_argument("--refresh-oracles", action="store_true", help="recompute and replace reference answers for the exact current inputs")


def from_arguments(args, repository):
    return OracleCache(Path(repository) / ".roc-parser-tmp/oracle-cache", enabled=not args.no_oracle_cache, refresh=args.refresh_oracles)
