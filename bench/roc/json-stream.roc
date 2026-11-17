app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.JsonValue
import parser.Parser
import Bench

## Parse-only driver: json-stream array|ndjson CHUNK_BYTES ITERATIONS.
## Input is preloaded by Bench; each run discards records after checksum walking.
## Array checksums include the outer array's weight, matching json.roc.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	format = if ((args.get(0).map_ok(OsStr.display) ?? "array") == "ndjson") Ndjson else Array
	width = U64.max(1, U64.from_str(args.get(1).map_ok(OsStr.display) ?? "65536") ?? 65_536)
	Bench.run!(args, |input| parse(input, format, width))
}

parse : Str, [Ndjson, Array], U64 -> Try(U64, U64)
parse = |input, format, width| {
	bytes = input.to_utf8()
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|position| {
			if position >= bytes.len() {
				Err(NoMore)
			}
			else {
				len = U64.min(width, bytes.len() - position)
				Ok((bytes.sublist({ start: position, len }), position + len))
			}
		},
	)
	var $checksum = if (format == Array) 6.U64 else 0
	for result in (JsonValue.record_decoder({ framing: format })).read_iter(chunks) {
		match result {
			Ok(Value(value)) => {
				$checksum = $checksum + consume(value)
			}
			Ok(Finished(_)) => return Ok($checksum)
			_ => return Err(1)
		}
	}
	Err(1)
}

consume : JsonValue -> U64
consume = |value| match value {
	Null => 1
	Bool(_) => 2
	Number(text) => text.count_utf8_bytes() + 3
	String(text) => text.count_utf8_bytes() + 5
	Array(items) => items.fold(6, |total, item| total + consume(item))
	Object(members) => members.fold(7, |total, member| total + member.key.count_utf8_bytes() + consume(member.value))
}
