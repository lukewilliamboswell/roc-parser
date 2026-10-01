# Contributing to roc-parser

Contributions are welcome through GitHub issues and pull requests. Keep changes
focused, explain the user-visible effect, and add tests for changed parser
behaviour. Open an issue first for a change to a public API or to the syntax a
format accepts.

The manual describes the whole process:

- [Contributing](docs/contributing.adoc): development setup with the Roc
  nightly from `.roc-version`, running the tests, the repository policy,
  building the documentation, and the pull request checklist.
- [Property tests and conformance reviews](docs/property-testing.adoc): the
  fuzz targets, the reference implementations each format is compared with,
  and how to reproduce a failure and add a regression.
- [Add a format module](docs/adding-a-format.adoc): the steps for a new parser.
- [Compiler updates, releases and security reports](docs/maintaining.adoc):
  for maintainers.

The quickest full check before you open a pull request:

```sh
ROC=/path/to/roc python3 scripts/all_tests.py
python3 -m unittest discover -s scripts/tests -p "test_*.py"
```

Pull requests need passing CI and signed commits.

## License

By contributing, you agree that your contribution is licensed under the
[Universal Permissive License v1.0](LICENSE).

## Security reports

Do not report suspected vulnerabilities in a public issue or pull request.
Follow the private process in [SECURITY.md](SECURITY.md).
