app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Streaming
import parser.Utf8

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
	width = (bytes.first() ?? 0).to_u64() % 31 + 1
	expected = Utf8.parse_bytes(CSV.parser({}), bytes)
	actual = Streaming.collect(Streaming.read_iter(chunks(bytes, width), CSV.decoder({})))
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
			for part in Streaming.write_iter(rows.iter(), CSV.encoder(props).with_chunk_bytes(width)) {
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
			if Streaming.collect(Streaming.read_iter(chunks($encoded, 1), CSV.decoder({}))) != Ok(rows) {
				crash "streaming byte CSV round trip changed records"
			}
		}
		(Err(_), Err(ReadFailed(ParseErr(problem)))) => {
			if problem.record == 0 or problem.field == 0 or problem.line == 0 or problem.column == 0 {
				crash "invalid streaming CSV diagnostic"
			}
		}
		_ => crash "whole-buffer and streaming CSV disagree on acceptance"
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "csv-stream-raw",
	examples: ["a,\"b\"\"c\"\r\nx,y".to_utf8(), "x\n\"a\"b,c".to_utf8(), [0xFF.U8, ',', 0x80]].map(|bytes| Fuzz.example({ name: Str.inspect(bytes), revision: "1", make: || bytes })),
	generator: Fuzz.raw_bytes,
	test,
	show: Str.inspect,
})
