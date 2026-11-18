app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import parser.Parser
import parser.Xml
import cli.OsStr
import Bench

## Namespace-aware tree construction and checksum traversal. The checksum
## consumes expanded names, declarations and values; this is a separate model
## from xml.roc's names-as-written tree and the event-consumption workload.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| match Xml.Namespaced.parse_str(input, {}) {
	Ok(xml) => Ok(consume(xml.root))
	Err(InvalidXml(error)) => Err(source_position(error.location).line + source_position(error.location).column)
	Err(InvalidNamespace(error)) => Err(source_position(error.location).line + source_position(error.location).column)
}

name_size : Xml.Namespaced.Name -> U64
name_size = |name| name.local.count_utf8_bytes() + match name.namespace {
	Uri(uri) => uri.count_utf8_bytes()
	NoNamespace => 0
}

consume : Xml.Namespaced.Node -> U64
consume = |node| match node {
	Text(text) => text.count_utf8_bytes() + 1
	Element({ name, bindings, attributes, children }) => {
		attrs = attributes.fold(0.U64, |total, attr| total + name_size(attr.name) + attr.value.count_utf8_bytes())
		declared = bindings.fold(
			0.U64,
			|total, binding| total + match binding.namespace {
				Uri(uri) => uri.count_utf8_bytes()
				NoNamespace => 0
			},
		)
		children.fold(name_size(name) + attrs + declared, |total, child| total + consume(child))
	}
}

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source coordinates"
}
