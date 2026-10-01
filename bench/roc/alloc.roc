app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.HTTP
import parser.Markdown
import parser.Parser
import parser.Utf8
import parser.Xml
import parser.Yaml

## Allocation counter for scripts/bench.py --allocations; never a fuzz target.
## The input's first line names the format and the rest is the document. The
## target parses once inside Fuzz.measure_allocs! and then crashes on purpose
## to report the count, so exit code 77 is the expected result.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	{ before, after } = bytes.split_first('\n') ?? { before: [], after: [] }
	format = Str.from_utf8(before) ?? ""
	input = Str.from_utf8(after) ?? ""
	measured = Fuzz.measure_allocs!(|{}| parse(format, input))
	crash "ALLOCATION_DIAGNOSTIC allocations=${measured.allocations.to_str()} result=${measured.value}"
}

parse : Str, Str -> Str
parse = |format, input| {
	accepted = match format {
		"csv" => CSV.parse_with(Parser.many(CSV.field(CSV.string)), input).is_ok()
		"yaml" => Yaml.parse_str(input).is_ok()
		"xml" => Xml.parse_str(input).is_ok()
		"markdown" => Markdown.parse_str(input).len() + 1 > 0
		"http" =>
			if input.starts_with("HTTP/") {
				HTTP.parse_response(input.to_utf8()).map_ok(|r| r.rest) == Ok([])
			} else {
				HTTP.parse_request(input.to_utf8()).map_ok(|r| r.rest) == Ok([])
			}
		_ => False
	}
	if accepted "ok" else "error"
}

target = Fuzz.target_with!({
	name: "bench-alloc-diagnostic",
	generator: Fuzz.raw_bytes,
	test!,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
