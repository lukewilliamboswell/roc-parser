app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.JsonValue
import parser.JsonStream
import parser.Streaming

chunks : List(U8), U64 -> Iter(List(U8))
chunks = |bytes, width| Iter.custom(
	(0.U64, False),
	Err(Unknown),
	|state| {
		if !state.1 {
			Ok(([], (state.0, True)))
		}
		else if state.0 >= bytes.len() {
			Err(NoMore)
		}
		else {
			len = U64.min(width, bytes.len() - state.0)
			Ok((bytes.sublist({ start: state.0, len }), (state.0 + len, False)))
		}
	},
)

# Independent whole-document model; the wrapper's depth is excluded from the
# per-item max_depth, and max_items applies within values, not to the stream.
oracle : List(U8), JsonStream.Format, JsonValue.Options -> Try(List(JsonValue), [Invalid])
oracle = |bytes, format, json| {
	text = Str.from_utf8(bytes).map_err(|_| Invalid)?
	if format == Array {
		value = JsonValue.parse_with(text, { ..json, max_depth: json.max_depth + 1 }).map_err(|_| Invalid)?
		match value {
			Array(values) => Ok(values)
			_ => Err(Invalid)
		}
	} else {
		var $values = []
		for line in text.split_on("\n") {
			if !line.to_utf8().all(|byte| byte == ' ' or byte == '\t' or byte == '\r') {
				$values = $values.append(JsonValue.parse_with(line, json).map_err(|_| Invalid)?)
			}
		}
		Ok($values)
	}
}

check : List(U8) -> {}
check = |bytes| {
	width = (bytes.first() ?? 0).to_u64() % 31 + 1
	options : List(JsonValue.Options)
	options = [{}, { comments: True, trailing_commas: True, bom: True }]
	for format in [Ndjson, Array] {
		for json in options {
			props : JsonStream.Props
			props = { format, json }
			whole = List.from_iter(JsonStream.parse_iter([bytes].iter(), props))
			if List.from_iter(JsonStream.parse_iter(chunks(bytes, width), props)) != whole {
				crash "JSON split changed values or diagnostics"
			}
			actual = Streaming.collect(JsonStream.parse_iter(chunks(bytes, width), props))
			match (oracle(bytes, format, json), actual) {
				(Ok(want), Ok(found)) => {
					if want != found {
						crash "JSON stream differs from independent whole-input parser"
					}
				}
				(Err(_), Err(_)) => {}
				_ => crash "JSON stream differs from whole-input acceptance"
			}
			limited : JsonStream.Props
			limited = { ..props, max_record_bytes: width, json: { ..json, max_depth: width % 4, max_items: width % 4, max_string_bytes: width, max_number_digits: width }, errors: { error_policy: ReportInvalidRecords, max_errors: 2 } }
			if List.from_iter(JsonStream.parse_iter([bytes].iter(), limited)) != List.from_iter(JsonStream.parse_iter(chunks(bytes, 1), limited)) {
				crash "JSON limit outcome depends on chunks"
			}
		}
		values = [String(Str.from_utf8_lossy(bytes)), Number(bytes.len().to_str()), Object([{ key: "nested", value: Array([Null, Bool(True)]) }])]
		var $encoded = []
		for part in JsonStream.write_iter(values.iter(), { format: format }, width) {
			match part {
				Ok(chunk) => {
					if chunk.is_empty() or chunk.len() > width {
						crash "JSON writer emitted invalid chunk size"
					}
					$encoded = $encoded.concat(chunk)
				}
				Err(_) => crash "valid JSON values failed to encode"
			}
		}
		if Streaming.collect(JsonStream.parse_iter(chunks($encoded, 1), { format: format })) != Ok(values) {
			crash "JSON streaming roundtrip changed values"
		}
	}
}

test! = |bytes| {
	check(bytes)
	# A successful value precedes a source error, but no Finished may follow.
	source : Stream(Try(List(U8), [Injected]))
	source = [Ok("[1,".to_utf8()), Err(Injected), Ok("2]".to_utf8())].iter().stream()
	var $read = []
	for! item in JsonStream.parse_stream(source, { format: Array }) {
		$read = $read.append(item)
	}
	if $read != [Ok(Value(Number("1"))), Err(SourceErr(Injected))] {
		crash "JSON source failure finalized or lost its original error"
	}
	values : Stream(Try(JsonValue, [Injected]))
	values = [Ok(Number("1")), Err(Injected), Ok(Number("2"))].iter().stream()
	var $written = []
	for! item in JsonStream.write_stream(values, { format: Array }, 8) {
		$written = $written.append(item)
	}
	if $written != [Ok("[1".to_utf8()), Err(SourceErr(Injected))] {
		crash "JSON writer finalized a failed source"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	name: "json-stream",
	examples: ["[1,{\"x\":[true,\"é😀\"]},-1e+20]".to_utf8(), "1\r\n\"é😀\"\nfalse".to_utf8(), "\u(FEFF)/* ],{ */[\"x\\\\\",true /* ] */,]//end".to_utf8(), "[1 2]".to_utf8(), "[1]x".to_utf8(), "[/*".to_utf8().concat([0xFF]).concat("*/1]".to_utf8()), [0xF0.U8, 0x90]].map(|bytes| Fuzz.example({ name: Str.inspect(bytes), revision: "1", make: || bytes })),
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
})
