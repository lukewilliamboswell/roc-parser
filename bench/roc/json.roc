app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.JsonValue
import Bench

## Benchmark driver: JsonValue.parse_str, then a walk over the whole value.
## The checksum weights match the Rust (serde_json) and Go (encoding/json)
## comparators, so the three agree on every document they all accept.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match JsonValue.parse_str(input) {
		Ok(value) => Ok(consume(value))
		Err(InvalidJsonValue(error)) => Err(error.line + error.column)
	}
}

consume : JsonValue -> U64
consume = |value| match value {
	Null => 1
	Bool(_) => 2
	Number(text) => text.count_utf8_bytes().to_u64() + 3
	String(text) => text.count_utf8_bytes().to_u64() + 5
	Array(items) => items.fold(6, |total, item| total + consume(item))
	Object(members) => members.fold(7, |total, member| total + member.key.count_utf8_bytes().to_u64() + consume(member.value))
}
