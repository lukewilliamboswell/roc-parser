# Property-test targets

Each `*Property.roc` module owns one parser property. `*Properties.roc` apps use
[roc-fuzz 0.7.0](https://github.com/lukewilliamboswell/roc-fuzz/releases/tag/0.7.0)
`Fuzz.suite` to group properties into shared executables. The native runner builds
each suite once and selects its members with `--target=NAME`, preserving each
property's corpus, examples, limits and reports. `seeds/` holds reviewable starting
inputs and `dictionaries/` holds format-specific syntax tokens.

Text, XML, JSON, Records, HTTP and Core properties share suites. Markup has a
local suite. CSV and OFX allocation checks use separate executables while compiler
limitations prevent them from passing unchanged in combined suites.

Run every target briefly with the latest Roc nightly (`blueprint run fuzz-smoke`):

```sh
scripts/fuzz.roc smoke all
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
scripts/fuzz.roc smoke dom-construction --runs 100000
```
