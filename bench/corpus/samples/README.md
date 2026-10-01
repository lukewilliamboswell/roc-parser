# Benchmark samples

Small real-world-shaped documents measured by `scripts/bench.py` alongside the
generated corpora. All of them are covered by this repository's licence:

- `project-readme.md` and `github-actions.yaml` are frozen copies of this
  repository's own `README.md` and `.github/workflows/fuzz.yml`.
- `orders-export.csv` and `logo.svg` were written for the benchmarks in the
  shape of a spreadsheet export and a design-tool SVG.

The HTTP request and response captures are defined in `scripts/bench.py`
(`HTTP_SAMPLES`) rather than here, so an editor or line-ending conversion
cannot change their CRLF line endings.

The files are frozen so corpus hashes stay stable; do not update them to track
the originals. See docs/benchmarks.adoc.
