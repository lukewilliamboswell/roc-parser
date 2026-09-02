# A Parser for Roc

[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/14421/badge)](https://www.bestpractices.dev/projects/14421)
[![Roc-Lang][roc_badge]][roc_link]

A simple [Parser Combinator](https://en.wikipedia.org/wiki/Parser_combinator) package for Roc.

```roc
color : Parser(String.Utf8, [Red, Green, Blue])
color = 
	String.one_of(
		[
			Parser.const(Red).skip(String.string("red")),
			Parser.const(Green).skip(String.string("green")),
			Parser.const(Blue).skip(String.string("blue")),
		],
	)

expect String.parse_str(color, "green") == Ok(Green)
```

Includes modules to parse the following (with various levels of maturity):

- Utf-8 Strings
- CSV
- XML
- Markdown
- HTTP
- YAML configuration files and Markdown frontmatter

The YAML module supports a practical single-document subset: nested block
mappings and sequences, flow collections, comments, quoted strings, and common
scalar values. It deliberately rejects advanced features such as anchors,
aliases, tags, directives, block scalars, complex keys, and document streams.

## Installation

Roc packages are obtained directly from a bundle URL. Add the latest released
`roc-parser` bundle to the package section of your application's header:

```roc
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.22.0/F1JVZPYfWP71s8vk6tHcV1Qx1Ef6CZkwswGoCn8VHZmL.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.1.0/AcowGJvjA8U2gCEf7E8QYNUePBdw7dzdRqSvERKaJZ53.tar.zst",
}

import parser.Parser
import parser.String
```

You can then use the imported modules to define a parser:

```roc
color : Parser(String.Utf8, [Red, Green, Blue])
color =
    String.one_of([
        Parser.const(Red).skip(String.string("red")),
        Parser.const(Green).skip(String.string("green")),
        Parser.const(Blue).skip(String.string("blue")),
    ])

expect String.parse_str(color, "green") == Ok(Green)
```

The [GitHub releases page](https://github.com/lukewilliamboswell/roc-parser/releases/latest)
is the source of truth for the latest version and bundle URL. Existing complete
programs are available in [`examples/`](examples/).

## Documentation

See [lukewilliamboswell.github.io/roc-parser/](https://lukewilliamboswell.github.io/roc-parser/)

## Compatibility and maturity

The project uses semantic versioning for `roc-parser` releases. Parser modules
have different levels of maturity, and their supported syntax is described in
the module documentation and release notes. In particular, YAML intentionally
implements the subset described above.

Roc is still evolving and this package currently pins a nightly compiler in
`.roc-version`. Compiler migrations can therefore require package API changes;
check the release notes before upgrading. The `main` branch and latest release
are supported, as described in [SECURITY.md](SECURITY.md).

Locally generate versioned docs using:

```sh
python3 scripts/generate_docs.py 1.1.0
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development, testing, and
coverage-guided property-testing workflow.
Security vulnerabilities must be reported privately as described in
[SECURITY.md](SECURITY.md), not in a public issue.

## Tests

Run the Python automation tests and full repository check locally:

```sh
python3 -m unittest discover -s scripts/tests -p "test_*.py"
python3 scripts/all_tests.py
```

CI temporarily skips `package/HTTP.roc` tests because the latest Roc nightly segfaults in the compiler while running that module's tests.

CI skips `examples/xml-svg.roc` because it depends on a migrated `roc-html` package release that is not available yet.

## Packaging

Bundle the package for distribution using:

```sh
python3 scripts/bundle.py --output-dir dist
```

Run the release workflow from GitHub Actions with a release version such as `0.11.0`. It builds and tests the bundle, creates the GitHub release, generates versioned docs, commits the generated `www/` update, and publishes the docs to GitHub Pages.

Each new release also publishes an SPDX SBOM and signed provenance. Verify a
downloaded bundle with `gh release verify-asset VERSION PATH` and
`gh attestation verify PATH --repo lukewilliamboswell/roc-parser`.

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Fpastebin.com%2Fraw%2FcFzuCCd7
[roc_link]: https://github.com/roc-lang/roc
