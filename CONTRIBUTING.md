# Contributing to roc-parser

Contributions are welcome through GitHub issues and pull requests. Keep changes
focused, explain the user-visible effect, and add tests for changed parser
behavior.

## Development setup

Use the Roc nightly named in `.roc-version`. To run the same validation used by
CI, set `ROC` to that compiler and run:

```sh
ROC=/path/to/roc python3 scripts/all_tests.py
```

Run the Python unit tests separately with:

```sh
python3 -m unittest discover -s scripts/tests -p "test_*.py"
```

Before opening a pull request:

- format changed Roc files with `roc fmt path/to/file.roc` using the compiler
  pinned in `.roc-version`;
- make sure the full test suite passes;
- add or update tests for success cases, invalid input, and relevant boundary
  cases;
- update documentation when a public parser API or supported syntax changes;
- follow the existing module and public API naming patterns;
- avoid unrelated formatting or refactoring changes;
- keep generated artifacts and local build output out of the commit; and
- sign commits so they satisfy the protected-branch policy.

Pull requests require passing CI. Human reviews are encouraged; address review
conversations before requesting another review after substantial changes.

## Property-based quality tests

The project uses `roc-fuzz` as a coverage-guided property-testing runner. Run
every parser quality target with a short bounded campaign:

```sh
python3 scripts/run_fuzz.py smoke all
```

Use `campaign`, `show`, `replay`, and `minimize` for longer investigations and
saved failures. The runner keeps binaries, corpora, and reproduction metadata
under `.roc-parser-tmp/fuzz/`.

By contributing, you agree that your contribution is licensed under the
[Universal Permissive License v1.0](LICENSE).

## Security reports

Do not report suspected vulnerabilities in a public issue or pull request.
Follow the private process in [SECURITY.md](SECURITY.md).
