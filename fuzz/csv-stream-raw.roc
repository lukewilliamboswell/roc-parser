app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Parser

## Independent whole-buffer and incremental scanners must agree on arbitrary
## bytes. A second pass writes accepted byte records with another dialect,
## splits every encoded chunk again, and recovers the identical records.
chunks : List(U8), U64 -> Iter(List(U8))
chunks = |bytes, width| Iter.custom(
	(0.U64, False),
	Err(Unknown),
	|state| {
		if state.0 >= bytes.len() {
			Err(NoMore)
		}
		else if !state.1 {
			Ok(([], (state.0, True)))
		}
		else {
			len = U64.min(width, bytes.len() - state.0)
			Ok((bytes.sublist({ start: state.0, len }), (state.0 + len, False)))
		}
	},
)

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match Str.from_utf8(bytes) {
		Ok(text) => {
			expected_text = CSV.parse_str(text, {}).map_ok(|document| document.records).map_ok(|rows| rows.map(|row| row.map(Str.from_utf8_lossy)))
			generic_agrees = match (CSV.rows_parser({ row: Parser.many(CSV.field(CSV.string({}))) }).parse_str(text), expected_text) {
				(Ok(actual), Ok(wanted)) => actual == wanted
				(Err(ParseError({ problem: InvalidCsv(actual), .. })), Err(InvalidCsv(wanted))) => actual == wanted
				_ => False
			}
			if CSV.parse_string_records(text, {}) != expected_text or !generic_agrees {
				crash "direct, raw, and generic CSV text collection disagree"
			}
		}
		Err(_) => {}
	}
	width = (bytes.first() ?? 0).to_u64() % 31 + 1
	expected = CSV.parse_bytes(bytes, {}).map_ok(|document| document.records)
	actual = (CSV.decoder({})).collect(chunks(bytes, width))
	match (expected, actual) {
		(Ok(rows), Ok(found)) => {
			if rows != found {
				crash "streaming CSV changed raw records"
			}
			props : CSV.WriteProps
			props = {
				quoting: if (width % 2 == 0) Always else Necessary,
				line_ending: match width % 3 {
					0 => CrLf
					1 => Lf
					_ => Cr
				},
			}
			var $encoded = []
			for part in (CSV.encoder({ ..props, chunk_bytes: width })).write_iter(rows.iter()) {
				match part {
					Ok(chunk) => {
						if chunk.len() > width {
							crash "writer exceeded chunk size"
						}
						$encoded = $encoded.concat(chunk)
					}
					Err(_) => crash "accepted CSV records failed to encode"
				}
			}
			if (CSV.decoder({})).collect(chunks($encoded, 1)) != Ok(rows) {
				crash "streaming byte CSV round trip changed records"
			}
			match Str.from_utf8($encoded) {
				Ok(text) if CSV.parse_string_records(text, {}) != Ok(rows.map(|row| row.map(Str.from_utf8_lossy))) => crash "direct text CSV round trip changed records"
				_ => {}
			}
		}
		(Err(InvalidCsv(wanted)), Err(InvalidCsv(problem))) => {
			if wanted != problem {
				crash "whole-buffer and streaming CSV diagnostic disagree"
			}
			logical = logical_position(problem.path)
			at = source_position(problem.location)
			if logical.record == 0 or logical.field == 0 or at.line == 0 or at.column == 0 {
				crash "invalid streaming CSV diagnostic"
			}
		}
		_ => crash "whole-buffer and streaming CSV disagree on acceptance"
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "csv-stream-raw",
	examples: ["a,\"b\"\"c\"\r\nx,y".to_utf8(), "é,\"😀\"\"q\",\r\n".to_utf8(), "x\n\"a\"b,c".to_utf8(), [0xFF.U8, ',', 0x80]].map(|bytes| Fuzz.example({ name: Str.inspect(bytes), revision: "1", make: || bytes })),
	generator: Fuzz.raw_bytes,
	test,
	show: Str.inspect,
})

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source text coordinates"
}

logical_position : List(Parser.PathSegment) -> { record : U64, field : U64 }
logical_position = |path| path.fold(
	{ record: 0.U64, field: 0.U64 },
	|position, segment| match segment {
		Record(index) => { ..position, record: index }
		Field(index) => { ..position, field: index }
		_ => position
	},
)
