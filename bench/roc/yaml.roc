app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Yaml
import parser.Parser
import Bench

## Benchmark driver: Yaml.parse_str, then a walk over the whole value.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match Yaml.parse_str(input, {}) {
		Ok(value) => Ok(consume(value))
		Err(InvalidYaml(error)) => Err(source_position(error.location).line + source_position(error.location).column)
	}
}

consume : Yaml -> U64
consume = |value| match value {
	Null => 1
	Bool(_) => 2
	Int(_) => 3
	Float(_) => 4
	Text(text) => text.count_utf8_bytes().to_u64() + 5
	Sequence(values) => values.fold(6, |total, child| total + consume(child))
	Mapping(entries) => entries.fold(7, |total, entry| total + entry.key.count_utf8_bytes().to_u64() + consume(entry.value))
}

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(position) => { line: position.line, column: position.column }
	_ => crash "expected retained YAML source text coordinates"
}
