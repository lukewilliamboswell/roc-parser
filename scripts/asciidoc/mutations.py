"""Reproducible exploratory edits; an edit makes no validity claim.

The differential runner must compare every accepted Roc result with the
oracle. Rejected mutations are reported separately, never counted as matches.
"""

from __future__ import annotations

import hashlib
import json
import random


TOKENS = (
    "\n", "\n\n", "\r\n", "= ", "== ", "=== ", "// ", "////\n",
    "----\n", "....\n", "****\n", "====\n", "|===\n", "|", "* ",
    ". ", "+\n", "[source,roc]\n", ":name: value\n", "{name}",
    "include::missing.adoc[]\n", "ifdef::name[]\n", "endif::[]\n",
    "<<target>>", "[[target]]\n", "*bold*", "`code`", "\\*", "é", "日", "😀",
)


def mutate_cases(cases: list[dict], count: int, seed: int) -> list[dict]:
    """Return exactly count edits with reproducible ids and content hashes.

    Positions are Unicode scalar boundaries so transport remains valid UTF-8;
    raw invalid-byte robustness is the native fuzz target's responsibility.
    Each edit has its own seed/index random stream. Parents use content-keyed
    rendezvous selection: inventory growth only changes slots won by a new
    parent, without excluding any eligible source or weighting by its length.
    """
    if count < 0:
        raise ValueError("mutation count must be nonnegative")
    parents = [case for case in cases if isinstance(case.get("source"), str)]
    if count and not parents:
        raise ValueError("mutations need at least one source seed")
    # Rendezvous selection gives each existing parent a score independent of
    # the inventory's order and length. Adding a parent can only claim slots
    # where its score exceeds the previous winner; other requests stay exact.
    keyed_parents = []
    for parent in parents:
        identity = json.dumps(
            [parent.get("id", "unnamed"), parent["source"], parent.get("includes")],
            ensure_ascii=False, sort_keys=True, separators=(",", ":"),
        ).encode("utf-8")
        keyed_parents.append((hashlib.sha256(identity).digest(), parent))
    results = []
    for index in range(count):
        slot = f"asciidoc-mutation-v2:{seed}:{index}:".encode("ascii")
        _, parent = max(keyed_parents, key=lambda entry: (
            hashlib.sha256(slot + b"parent:" + entry[0]).digest(), entry[0]))
        # Separate each edit's random stream so changing another slot's parent,
        # source length or operation cannot consume random choices here.
        rng = random.Random(int.from_bytes(hashlib.sha256(slot + b"edit").digest(), "big"))
        source = parent["source"]
        start = rng.randrange(len(source) + 1)
        stop = rng.randrange(start, len(source) + 1)
        operation = rng.choice(("insert", "delete", "replace", "duplicate", "truncate"))
        token = rng.choice(TOKENS)
        if operation == "insert":
            changed = source[:start] + token + source[start:]
        elif operation == "delete":
            changed = source[:start] + source[stop:]
        elif operation == "replace":
            changed = source[:start] + token + source[stop:]
        elif operation == "duplicate":
            changed = source[:stop] + source[start:stop] + source[stop:]
        else:
            changed = source[:start]
        digest = hashlib.sha256(changed.encode("utf-8")).hexdigest()
        result = {
            "id": f"mutation-{seed}-{index:06d}",
            "source": changed,
            "parent_id": parent.get("id", "unnamed"),
            "sha256": digest,
            "operation": operation,
            "category": "mutation",
            "support": "explore",
        }
        if "includes" in parent:
            result["includes"] = dict(parent["includes"])
        results.append(result)
    return results
