# roc-parser

[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/14421/badge)](https://www.bestpractices.dev/projects/14421)
[![Roc-Lang][roc_badge]][roc_link]

roc-parser turns text into typed [Roc](https://www.roc-lang.org/) values. It
provides ready-made parsers for common formats and the
[parser combinators](https://en.wikipedia.org/wiki/Parser_combinator) they are
built from, so you can also describe formats of your own. It is pure Roc and
works with any platform. Runtime parsers return invalid input as an error value.
Checked literals and imported constants can reject invalid input during
compilation, with configurable validation checks.

| Format | Specification | Manual | Throughput |
| --- | --- | --- | ---: |
| CSV | RFC 4180, with common relaxations | [Reading CSV](https://lukewilliamboswell.github.io/roc-parser/#csv) | 51 MB/s<br>vs encoding/csv (Go) 420 MB/s |
| QIF | Quicken bank/cash/card records | [Reading QIF](docs/qif.adoc) | 23 MB/s<br>vs qif_parser (Rust)† 190 MB/s |
| OFX | Banking/card statements in SGML and XML | [Reading OFX](docs/ofx.adoc) | 4.4 MB/s<br>vs ofx-rs (Rust) 120 MB/s |
| YAML | A configuration subset of YAML 1.2 | [Reading YAML](https://lukewilliamboswell.github.io/roc-parser/#yaml) | 54 MB/s<br>vs yaml-rust2 (Rust) 59 MB/s |
| XML | XML 1.0 well-formedness, without DTDs | [Reading XML](https://lukewilliamboswell.github.io/roc-parser/#xml) | 150 MB/s<br>vs roxmltree (Rust)† 300 MB/s |
| HTML | HTML document and fragment parsing, with explicit recovery | [Reading markup](docs/markup.adoc) | 60 MB/s<br>vs x/net/html (Go)‡ 75 MB/s |
| SVG | SVG vocabulary and common value grammars in XML or HTML syntax | [Reading markup](docs/markup.adoc) | 46 MB/s<br>vs roxmltree (Rust)†‡ 450 MB/s |
| MathML | Core and common Presentation MathML | [Reading markup](docs/markup.adoc) | 22 MB/s<br>vs roxmltree (Rust)†‡ 210 MB/s |
| JSON | RFC 8259, with opt-in relaxations and checks; reads and writes | [Reading JSON](https://lukewilliamboswell.github.io/roc-parser/#json) | 94 MB/s<br>vs serde_json (Rust) 180 MB/s |
| Markdown | CommonMark 0.31.2 with GFM extensions | [Reading Markdown](https://lukewilliamboswell.github.io/roc-parser/#markdown) | 51 MB/s<br>vs pulldown-cmark (Rust) 200 MB/s |
| AsciiDoc | Block parsing, preprocessing and embedded HTML | [Reading AsciiDoc](docs/asciidoc.adoc) | 9 MB/s<br>vs Asciidoctor (Ruby) 2 MB/s |
| HTTP/1.1 | RFC 9112 and RFC 9110 message syntax | [Reading HTTP](https://lukewilliamboswell.github.io/roc-parser/#http) | 350 MB/s<br>vs httparse (Rust) 1700 MB/s |
| Roc source | Lossless tokens and indexed syntax trees | [Parsing Roc source](docs/roc-syntax.adoc) | 230 MB/s<br>vs Roc compiler parser (Zig) 270 MB/s |
| Your own | Parser combinators | [Your first parser](https://lukewilliamboswell.github.io/roc-parser/#combinator-primer) | Varies |

Each cell shows Roc versus the fastest measured reference parser. Measured
on an AMD Ryzen 7 9700X under Linux using one pinned logical core. Roc apps
were built with `nightly-2026-11-15-f25da45` and `--opt=speed --debug`. Figures are
medians of seven calibrated batches targeting 200 ms, including parsing,
result construction and checksum traversal, excluding I/O. Inputs are
representative ~64 KiB documents (HTTP: a 555-byte request head).
MB/s uses 1,000,000 bytes per MB.

Reference libraries construct different result models. † Uses caller-bound
borrowing; Roc supports shared slices with reference-counted ownership.
`qif_parser` also uses floating-point amounts. ‡ Builds a DOM; Roc also
retains source information and validates vocabulary and values. See the
[method and reproduction command](docs/benchmarks.adoc#_reproduce_the_readme_throughput_table).
Reproduced reports include library versions and full measurement receipts.

CSV, QIF, OFX and JSON record streams support
[chunked reading and writing](docs/streaming.adoc) through Roc’s builtin `Iter`
and `Stream`. JSON supports NDJSON records and top-level array items through
`JsonStream`; complete JSON documents use `JsonValue`. XML, including namespaces,
and HTTP/1.1 support chunked event reading. The other format parsers require
complete input in memory. JSON and namespace-aware XML also support
whole-document writing, with `JsonValue.to_str` and `XmlNs.render`.

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
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
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

Compile-time parsing is an enduring goal for every supported format. See
[quoted documents and imported files](docs/compile-time-parsing.adoc) for typed
multiline literals, checked file imports, and validation props with defaults.

- [The roc-parser manual](https://lukewilliamboswell.github.io/roc-parser/)
  ([PDF](https://lukewilliamboswell.github.io/roc-parser/roc-parser.pdf))
- [API reference](https://lukewilliamboswell.github.io/roc-parser/api/)
- Complete programs in [`examples/`](examples/), which use the package
  source; each release attaches them pinned to its bundle as
  `roc-parser-examples-<version>.zip`

## Compatibility

Roc is still changing, so roc-parser follows the latest nightly build of the
Roc compiler: every change is tested with the nightly of the day, and each
release names the nightly it was built with. Releases follow semantic
versioning; read the target release’s notes before upgrading.

## Contributing and security

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development, testing and
fuzzing workflow. Report security vulnerabilities privately as described in
[SECURITY.md](SECURITY.md), not in a public issue.

## License

[UPL-1.0](LICENSE)

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Froc-lang.org%2Fbadge%2Froc.json
[roc_link]: https://github.com/roc-lang/roc
