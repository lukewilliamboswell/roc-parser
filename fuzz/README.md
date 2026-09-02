# Coverage-guided property quality tests

These targets use
[`roc-fuzz` 0.3.0](https://github.com/lukewilliamboswell/roc-fuzz/releases/tag/0.3.0)
as a property-testing runner for every public text parser. Successful parses and
ordinary parse errors are both valid outcomes; unexpected crashes, hangs, and
resource regressions fail the property. These are library quality tests, not a
security program. The platform URL in each target is content-addressed. Its release asset
has SHA-256
`f9be31a5d7f0ba2e7e13ec804e6827513af9e9e548137d780a904fdbb5793ee5`.

Run every target for a short, iteration-bounded check:

```sh
python3 scripts/run_fuzz.py smoke
```

Run one longer campaign:

```sh
python3 scripts/run_fuzz.py campaign markdown-document --seconds 600
```

The runner builds binaries under `.roc-parser-tmp/fuzz/bin`, maintains a
separate corpus for each target under `.roc-parser-tmp/fuzz/corpus`, and keeps
runner metadata and failures under `.roc-parser-tmp/fuzz/runs`.

Inspect and reproduce a saved failure with the matching target:

```sh
python3 scripts/run_fuzz.py show TARGET INPUT --no-build
python3 scripts/run_fuzz.py replay TARGET INPUT --no-build
python3 scripts/run_fuzz.py minimize TARGET INPUT OUTPUT --no-build
```

## Seeds and dictionaries

The JSON files under `seeds/` are deliberately reviewable parser inputs rather
than opaque libFuzzer files. At runtime the Python runner encodes each string
for the pinned `Fuzz.str` generator and adds it to the target's working corpus.
Seed strings are limited to 255 UTF-8 bytes because this encoding uses the
generator's one-byte length selector.

The dictionaries contain format-specific syntax tokens. New regression cases
belong in the relevant JSON seed file after the minimized failure has been
rendered and understood. Keep the raw minimized input and its original target
binary when exact reproduction of a historical failure matters.

## OpenSSF tooling note

`roc-fuzz` embeds libFuzzer and provides real coverage-guided fuzzing, but the
current OpenSSF Scorecard Fuzzing check does not recognize Roc fuzz targets.
It currently detects OSS-Fuzz, ClusterFuzzLite, and selected language-specific
frameworks. Treat a low Scorecard Fuzzing result as a known detection gap, not
as evidence that these campaigns did not run.
