# Property-test targets

Each `*.roc` file here is a [roc-fuzz](https://github.com/lukewilliamboswell/roc-fuzz)
property-test target for one of the package's parsers. `seeds/` holds reviewable
starting inputs and `dictionaries/` holds format-specific syntax tokens.

Run every target briefly with the latest Roc nightly (`blueprint run fuzz-smoke`):

```sh
python3 scripts/run_fuzz.py smoke all
```

How the targets work, how to reproduce and minimise a failure, and how to add a
regression are described in the
[property testing chapter of the manual](../docs/property-testing.adoc).

`dom-construction` checks generated explicit HTML child/text placements against
actual browser-recovery tree construction and typed repair diagnostics. Its
independent model includes the whole intended tree and unique node attributes
to detect movement or formatting clones. Curated examples cover scope boundaries,
formatting markers and table modes. Namespace identity is checked in package
unit tests because HTML source spelling infers namespaces differently from DOM
construction. Like the other new markup targets, this target runs locally and
is excluded from scheduled and CI smoke campaigns:

```sh
python3 scripts/run_fuzz.py smoke dom-construction --runs 100000
```
