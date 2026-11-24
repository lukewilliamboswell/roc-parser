import fuzz.Fuzz
import parser.JsonValue
import parser.Parser
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
oracle : List(U8), [Ndjson, Array], JsonValue.Props -> Try(List(JsonValue), [Invalid])
oracle = |bytes, format, json| {
	text = Str.from_utf8(bytes).map_err(|_| Invalid)?
	if format == Array {
		value = JsonValue.parse_str(text, { ..json, max_depth: json.max_depth + 1 }).map_err(|_| Invalid)?
		match value {
			Array(values) => Ok(values)
			_ => Err(Invalid)
		}
	} else {
		var $values = []
		for line in text.split_on("\n") {
			if !line.to_utf8().all(|byte| byte == ' ' or byte == '\t' or byte == '\r') {
				$values = $values.append(JsonValue.parse_str(line, json).map_err(|_| Invalid)?)
			}
		}
		Ok($values)
	}
}

check : List(U8) -> {}
check = |bytes| {
	width = (bytes.first() ?? 0).to_u64() % 31 + 1
	options : List(JsonValue.Props)
	options = [JsonValue.Props.{}, JsonValue.Props.{ comments: True, trailing_commas: True, bom: True }]
	for format in [Ndjson, Array] {
		for json in options {
			props : JsonValue.RecordProps
			props = JsonValue.RecordProps.{ framing: format, json }
			whole = List.from_iter((JsonValue.record_decoder(props)).read_iter([bytes].iter()))
			if List.from_iter((JsonValue.record_decoder(props)).read_iter(chunks(bytes, width))) != whole {
				crash "JSON split changed values or diagnostics"
			}
			actual = (JsonValue.record_decoder(props)).collect(chunks(bytes, width))
			match (oracle(bytes, format, json), actual) {
				(Ok(want), Ok(found)) => {
					if want != found {
						crash "JSON stream differs from independent whole-input parser"
					}
				}
				(Err(_), Err(_)) => {}
				_ => crash "JSON stream differs from whole-input acceptance"
			}
			limited : JsonValue.RecordProps
			limited = { ..props, max_record_bytes: width, json: { ..json, max_depth: width % 4, max_items: width % 4, max_string_bytes: width, max_number_digits: width } }
			if List.from_iter((JsonValue.record_decoder({ ..limited, errors: { policy: ReportInvalidRecords, max_errors: 2 } })).read_iter([bytes].iter())) != List.from_iter((JsonValue.record_decoder({ ..limited, errors: { policy: ReportInvalidRecords, max_errors: 2 } })).read_iter(chunks(bytes, 1))) {
				crash "JSON limit outcome depends on chunks"
			}
		}
		values = [String(Str.from_utf8_lossy(bytes)), Number(bytes.len().to_str()), Object([{ key: "nested", value: Array([Null, Bool(True)]) }])]
		var $encoded = []
		for part in (JsonValue.record_encoder({ framing: format, chunk_bytes: width })).write_iter(values.iter()) {
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
		if (JsonValue.record_decoder({ framing: format })).collect(chunks($encoded, 1)) != Ok(values) {
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
	for! item in (JsonValue.record_decoder({ framing: Array })).read_stream(source) {
		$read = $read.append(item)
	}
	if $read != [Ok(Value(Number("1"))), Err(SourceErr(Injected))] {
		crash "JSON source failure finalized or lost its original error"
	}
	values : Stream(Try(JsonValue, [Injected]))
	values = [Ok(Number("1")), Err(Injected), Ok(Number("2"))].iter().stream()
	var $written = []
	for! item in (JsonValue.record_encoder({ framing: Array, chunk_bytes: 8 })).write_stream(values) {
		$written = $written.append(item)
	}
	if $written != [Ok("[1".to_utf8()), Err(SourceErr(Injected))] {
		crash "JSON writer finalized a failed source"
	}
	Fuzz.keep
}

property_target = Fuzz.target_with!({
	name: "json-stream",
	examples: ["[1,{\"x\":[true,\"é😀\"]},-1e+20]".to_utf8(), "1\r\n\"é😀\"\nfalse".to_utf8(), "\u(FEFF)/* ],{ */[\"x\\\\\",true /* ] */,]//end".to_utf8(), "[1 2]".to_utf8(), "[1]x".to_utf8(), "[/*".to_utf8().concat([0xFF]).concat("*/1]".to_utf8()), [0xF0.U8, 0x90]].map(|bytes| Fuzz.example({ name: Str.inspect(bytes), revision: "1", make: || bytes })),
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
})

JsonStreamProperty :: [].{
	target = property_target
}
