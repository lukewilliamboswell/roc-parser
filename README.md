# roc-parser

[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/14421/badge)](https://www.bestpractices.dev/projects/14421)
[![Roc-Lang][roc_badge]][roc_link]

roc-parser turns text into typed [Roc](https://www.roc-lang.org/) values. It
provides ready-made parsers for common formats and the
[parser combinators](https://en.wikipedia.org/wiki/Parser_combinator) they are
built from, so you can also describe formats of your own. It is pure Roc and
works with any platform. Invalid input is returned as an error value, never a
crash.

| Format | Specification | Manual |
| --- | --- | --- |
| CSV | RFC 4180, with common relaxations | [Reading CSV](https://lukewilliamboswell.github.io/roc-parser/#csv) |
| YAML | A configuration subset of YAML 1.2 | [Reading YAML](https://lukewilliamboswell.github.io/roc-parser/#yaml) |
| XML | XML 1.0 well-formedness, without DTDs | [Reading XML](https://lukewilliamboswell.github.io/roc-parser/#xml) |
| Markdown | CommonMark 0.31.2 with GFM extensions | [Reading Markdown](https://lukewilliamboswell.github.io/roc-parser/#markdown) |
| HTTP/1.1 | RFC 9112 and RFC 9110 message syntax | [Reading HTTP](https://lukewilliamboswell.github.io/roc-parser/#http) |
| Your own | Parser combinators | [Your first parser](https://lukewilliamboswell.github.io/roc-parser/#combinator-primer) |

It reads complete inputs held in memory. It does not stream, and it does not
write formats back out.

## Try it

To run the complete examples:

1. Open the [latest release](https://github.com/lukewilliamboswell/roc-parser/releases/latest)
   and install the Roc nightly its notes name.
2. Download that release's `roc-parser-examples-<version>.zip` and unzip it.
3. In the `examples/` directory inside, run:

```bash
roc version
roc csv-movies.roc
```

To use the package in your own app, copy the `.tar.zst` link from the newest
[release](https://github.com/lukewilliamboswell/roc-parser/releases) into your
app's header, then decode some CSV into records:

```roc
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/<version>/<hash>.tar.zst",
}

import cli.Stdout
import parser.CSV

Planet : { name : Str, moons : U64 }

input =
	\\name,moons
	\\Mercury,0
	\\Earth,1
	\\Mars,2

planets : Str -> Try(List(Planet), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
planets = |text| CSV.parse(text)

describe : Str -> Str
describe = |text| {
	match planets(text) {
		Ok(rows) => Str.join_with(rows.map(|p| "${p.name}: ${p.moons.to_str()} ${if p.moons == 1 "moon" else "moons"}"), "\n")
		Err(InvalidCsv(error)) => "line ${error.line.to_str()}: ${error.message}"
		Err(MissingRequiredField(name)) => "no ${name} column"
	}
}

main! = |_args| {
	Stdout.line!(describe(input))
}
```

Running it with `roc main.roc` prints:

```text
Mercury: 0 moons
Earth: 1 moon
Mars: 2 moons
```

This is [`docs/examples/getting-started-csv.roc`](docs/examples/getting-started-csv.roc),
which CI runs. [Getting started](https://lukewilliamboswell.github.io/roc-parser/#getting-started)
explains it line by line.

## Documentation

- [The roc-parser manual](https://lukewilliamboswell.github.io/roc-parser/)
  ([PDF](https://lukewilliamboswell.github.io/roc-parser/roc-parser.pdf))
- [API reference](https://lukewilliamboswell.github.io/roc-parser/api/)
- Complete programs in [`examples/`](examples/), which use the package
  source; each release attaches them pinned to its bundle as
  `roc-parser-examples-<version>.zip`

## Compatibility

Roc is still changing, so roc-parser tracks recent nightly builds of the Roc
compiler. The nightly the `main` branch is tested with is pinned in
[`.roc-version`](.roc-version), and each release names the nightly it was
built with. Releases follow semantic versioning; 2.0.0 is a breaking release,
so check its release notes before upgrading.

## Contributing and security

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development, testing and
fuzzing workflow. Report security vulnerabilities privately as described in
[SECURITY.md](SECURITY.md), not in a public issue.

## License

[UPL-1.0](LICENSE)

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Froc-lang.org%2Fbadge%2Froc.json
[roc_link]: https://github.com/roc-lang/roc
