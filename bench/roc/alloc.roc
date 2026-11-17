app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.HTTP
import parser.JsonValue
import parser.Markdown
import parser.RocSyntax
import parser.Parser
import parser.Xml
import parser.Yaml

## Allocation counter for scripts/bench.py --allocations; never a fuzz target.
## The input's first line names the format and the rest is the document. The
## target parses once inside Fuzz.measure!, and `measure` reports the count.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	{ before, after } = bytes.split_first('\n') ?? { before: [], after: [] }
	format = Str.from_utf8(before) ?? ""
	input = Str.from_utf8(after) ?? ""
	_measured = Fuzz.measure!("parse", || parse(format, input))
	Fuzz.keep
}

parse : Str, Str -> Str
parse = |format, input| {
	accepted = match format {
		"roc-syntax" => RocSyntax.parse_str(input, {}).status == Complete
		"csv" => CSV.rows_parser({ row: Parser.many(CSV.field(CSV.string({}))) }).parse_str(input).is_ok()
		"yaml" => Yaml.parse_str(input, {}).is_ok()
		"xml" => Xml.parse_str(input, {}).is_ok()
		"json" => JsonValue.parse_str(input, {}).is_ok()
		"markdown" => Markdown.parse_str(input, {}).len() + 1 > 0
		"http" =>
			if input.starts_with("HTTP/") {
				HTTP.Response.parse_bytes(input.to_utf8(), {}).is_ok()
			} else {
				HTTP.Request.parse_bytes(input.to_utf8(), {}).is_ok()
			}
		_ => False
	}
	if (accepted) "ok" else "error"
}

target = Fuzz.target_with!({
	examples: [],
	name: "bench-alloc-diagnostic",
	generator: Fuzz.raw_bytes,
	test!,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
