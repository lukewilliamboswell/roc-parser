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

Copy the `.tar.zst` link from the newest
[release](https://github.com/lukewilliamboswell/roc-parser/releases) into your
app's header, then parse some CSV into records:

```roc
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/<version>/<hash>.tar.zst",
}

import cli.Stdout
import parser.CSV
import parser.Parser

Planet : { name : Str, moons : U64 }

planet : Parser(CSV.CSVRecord, Planet)
planet =
    CSV.record(|name| |moons| { name, moons })
        .keep(CSV.field(CSV.string))
        .keep(CSV.field(CSV.u64))

input =
    \\Mercury,0
    \\Earth,1
    \\Mars,2

describe : Str -> Str
describe = |text| {
    match CSV.parse_str(planet, text) {
        Ok(planets) => planets.map(|p| "${p.name}: ${p.moons.to_str()} moons") |> Str.join_with("\n")
        Err(_) => "The CSV did not match the expected columns"
    }
}

main! = |_args| {
    Stdout.line!(describe(input))
}
```

Running it with `roc main.roc` prints:

```text
Mercury: 0 moons
Earth: 1 moons
Mars: 2 moons
```

This is [`docs/examples/getting-started-csv.roc`](docs/examples/getting-started-csv.roc),
which CI runs. [Getting started](https://lukewilliamboswell.github.io/roc-parser/#getting-started)
explains it line by line.

## Documentation

- [The roc-parser manual](https://lukewilliamboswell.github.io/roc-parser/)
  ([PDF](https://lukewilliamboswell.github.io/roc-parser/roc-parser.pdf))
- [API reference](https://lukewilliamboswell.github.io/roc-parser/api/)
- Complete programs in [`examples/`](examples/)

## Compatibility

Roc is still changing, so roc-parser tracks recent nightly builds of the Roc
compiler. The nightly the `main` branch is tested with is pinned in
[`.roc-version`](.roc-version), and each release names the nightly it was
built with. Releases follow semantic versioning, and version 2.0.0 is in
preparation. Check the release notes before upgrading.

## Contributing and security

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development, testing and
fuzzing workflow. Report security vulnerabilities privately as described in
[SECURITY.md](SECURITY.md), not in a public issue.

## License

[UPL-1.0](LICENSE)

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Froc-lang.org%2Fbadge%2Froc.json
[roc_link]: https://github.com/roc-lang/roc
