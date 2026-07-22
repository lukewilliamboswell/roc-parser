# A Parser for Roc

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

Includes modules to parse the following (with various levels of maturity);
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

## Documentation

See [lukewilliamboswell.github.io/roc-parser/](https://lukewilliamboswell.github.io/roc-parser/)

Locally generate versioned docs using `./docs.sh 0.11.0`.

## Contributing

If you see anything that could be improved please create an Issue or Pull Request.

## Tests

Run the full CI check locally with `./ci/all_tests.sh`.

CI temporarily skips `package/HTTP.roc` tests because the latest Roc nightly segfaults in the compiler while running that module's tests.

CI skips `examples/xml-svg.roc` because it depends on a migrated `roc-html` package release that is not available yet.

## Packaging

Bundle the package for distribution using `scripts/bundle.sh --output-dir dist`.

Run the release workflow from GitHub Actions with a release version such as `0.11.0`. It builds and tests the bundle, creates the GitHub release, generates versioned docs, commits the generated `www/` update, and publishes the docs to GitHub Pages.
