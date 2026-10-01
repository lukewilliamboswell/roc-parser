app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Yaml
import Bench

## Benchmark driver: Yaml.parse_str, then a walk over the whole value.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match Yaml.parse_str(input) {
		Ok(value) => Ok(consume(value))
		Err(YamlError(error)) => Err(error.line + error.column)
	}
}

consume : Yaml -> U64
consume = |value| match value {
	Null => 1
	Bool(_) => 2
	Int(_) => 3
	Float(_) => 4
	String(text) => text.count_utf8_bytes().to_u64() + 5
	Sequence(values) => values.fold(6, |total, child| total + consume(child))
	Mapping(entries) => entries.fold(7, |total, entry| total + entry.key.count_utf8_bytes().to_u64() + consume(entry.value))
}
