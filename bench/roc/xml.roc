app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Xml
import Bench

## Benchmark driver: Xml.parse_str, then a walk over the whole tree.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match Xml.parse_str(input) {
		Ok(xml) => Ok(consume(xml.root))
		Err(XmlError(error)) => Err(error.line + error.column)
	}
}

consume : Xml.Node -> U64
consume = |node| match node {
	Text(text) => text.count_utf8_bytes().to_u64() + 1
	Element(name, attributes, children) => {
		attrs = attributes.fold(0, |total, attr| total + attr.name.count_utf8_bytes().to_u64() + attr.value.count_utf8_bytes().to_u64())
		children.fold(name.count_utf8_bytes().to_u64() + attrs, |total, child| total + consume(child))
	}
}
