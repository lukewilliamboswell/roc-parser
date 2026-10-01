# Property-test targets

Each `*.roc` file here is a [roc-fuzz](https://github.com/lukewilliamboswell/roc-fuzz)
property-test target for one of the package's parsers. `seeds/` holds reviewable
starting inputs and `dictionaries/` holds format-specific syntax tokens.

Run every target briefly with the compiler pinned in `.roc-version`:

```sh
python3 scripts/run_fuzz.py smoke all
```

How the targets work, how to reproduce and minimise a failure, and how to add a
regression are described in the
[property testing chapter of the manual](../docs/property-testing.adoc).
