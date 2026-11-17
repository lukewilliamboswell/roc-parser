import JsonValue
import Streaming

## Incremental UTF-8 JSON record streams. Each value owns at most its record's
## bytes. Array framing and invalid encoding are terminal. Independently framed
## NDJSON records may be rejected and skipped.
JsonStream :: [].{

	## NDJSON has one value per physical line. Array emits outer-array items.
	Format : [Ndjson, Array]

	## JSON options apply to each emitted value, including top_level and limits.
	## Array wrapper comments, a BOM and trailing commas follow those options.
	## max_record_bytes bounds encoded unfinished values before JSON decoding.
	Props := { format : Format ?? Ndjson, json : JsonValue.Options ?? {}, errors : Streaming.ReadProps ?? {}, max_record_bytes : U64 ?? 4_194_304 }

	## Original one-based line/byte column and one-based record number.
	Error : { record : U64, line : U64, column : U64, message : Str }

	## Writers validate a complete value before emitting it. Memory includes one
	## encoded record, bounded by max_record_bytes before rendering.
	WriteProps := { format : Format ?? Ndjson, sort_keys : Bool ?? False, max_record_bytes : U64 ?? 4_194_304 }

	## A value cannot be represented as strict JSON within the writer limits.
	WriteError : [InvalidNumber(Str), DuplicateKey(Str), TooDeep, TooLarge(U64)]

	## Persistent framing state. Keep only its latest version to release records.
	Decoder : State

	## Start a decoder without pulling input.
	decoder : Props -> Decoder
	decoder = |props| { props, phase: Start, record: [], number: 1, line: 1, column: 1, record_line: 1, record_column: 1, previous_cr: False, string: False, escape: False, stack: [], comment: Normal, bom: 0, utf8_remaining: 0, utf8_low: 0x80, utf8_high: 0xBF }

	## Consume at most one value. Only Final establishes document completion.
	step : Decoder, List(U8), Streaming.InputEnd -> Streaming.Step(Decoder, JsonValue, Error)
	step = advance

	## Pull pure chunks lazily, including a final Finished validation event.
	parse_iter : Iter(List(U8)), Props -> _
	parse_iter = |chunks, props| Streaming.iter(chunks, decoder(props), step, props.errors)

	## Pull effectful chunks. Source failures stop without finalizing JSON.
	parse_stream : Stream(Try(List(U8), source_error)), Props -> _
	parse_stream = |chunks, props| Streaming.stream(chunks, decoder(props), step, props.errors)

	## Write compact values separated by LF or enclosed in one JSON array.
	write_iter : Iter(JsonValue), WriteProps, U64 -> _
	write_iter = |values, props, chunk_bytes| Streaming.encode_state_iter(values, { props, started: False }, write, finish, chunk_bytes)

	## Source failure never emits the closing array bracket.
	write_stream : Stream(Try(JsonValue, source_error)), WriteProps, U64 -> _
	write_stream = |values, props, chunk_bytes| Streaming.encode_state_stream(values, { props, started: False }, write, finish, chunk_bytes)
}

State : {
	props : JsonStream.Props,
	phase : [Start, First, Next, End, Bom],
	record : List(U8),
	number : U64,
	line : U64,
	column : U64,
	record_line : U64,
	record_column : U64,
	previous_cr : Bool,
	string : Bool,
	escape : Bool,
	stack : List(U8),
	comment : [Normal, Slash, LineComment, BlockComment, BlockStar],
	bom : U8,
	utf8_remaining : U8,
	utf8_low : U8,
	utf8_high : U8,
}

problem : State, Str -> JsonStream.Error
problem = |state, message| { record: state.number, line: state.line, column: state.column, message }

move : State, U8 -> State
move = |state, byte| {
	if byte == '\r' {
		{ ..state, line: state.line + 1, column: 1, previous_cr: True }
	} else if byte == '\n' {
		{ ..state, line: state.line + if (state.previous_cr) 0 else 1, column: 1, previous_cr: False }
	} else {
		{ ..state, column: state.column + 1, previous_cr: False }
	}
}

# Validate every byte, including discarded wrapper comments, without retaining it.
utf8 : State, U8 -> Try(State, JsonStream.Error)
utf8 = |state, byte| {
	if state.utf8_remaining > 0 {
		if byte < state.utf8_low or byte > state.utf8_high {
			return Err(problem(state, "invalid UTF-8"))
		}
		return Ok({ ..state, utf8_remaining: state.utf8_remaining - 1, utf8_low: 0x80, utf8_high: 0xBF })
	}
	if byte < 0x80 {
		return Ok(state)
	}
	if byte >= 0xC2 and byte <= 0xDF {
		return Ok({ ..state, utf8_remaining: 1 })
	}
	if byte >= 0xE0 and byte <= 0xEF {
		return Ok({ ..state, utf8_remaining: 2, utf8_low: if (byte == 0xE0) 0xA0 else 0x80, utf8_high: if (byte == 0xED) 0x9F else 0xBF })
	}
	if byte >= 0xF0 and byte <= 0xF4 {
		return Ok({ ..state, utf8_remaining: 3, utf8_low: if (byte == 0xF0) 0x90 else 0x80, utf8_high: if (byte == 0xF4) 0x8F else 0xBF })
	}
	Err(problem(state, "invalid UTF-8"))
}

whitespace : U8 -> Bool
whitespace = |byte| byte == ' ' or byte == '\t' or byte == '\r' or byte == '\n'

reset : State -> State
reset = |state| { ..state, record: [], number: state.number + 1, stack: [], string: False, escape: False, comment: Normal, record_line: state.line, record_column: state.column }

# JsonValue currently reports limits through its public error message. Keep its
# six resource diagnostics terminal even when NDJSON recovery is requested.
limit_error : Str -> Bool
limit_error = |message| ["document exceeds the limit of ", "nesting exceeds the limit of ", "string exceeds the limit of ", "number exceeds the limit of ", "array has more than ", "object has more than "].any(|prefix| message.starts_with(prefix))

record_result : State, U64 -> Streaming.Step(State, JsonValue, JsonStream.Error)
record_result = |state, consumed| {
	text = Str.from_utf8(state.record) ?? ""
	next = reset(state)
	options = if (state.props.format == Array) { ..state.props.json, bom: False } else state.props.json
	match JsonValue.parse_with(text, options) {
		Ok(value) => { state: next, consumed, outcome: Item(value) }
		Err(InvalidJsonValue(error)) => {
			located = { record: state.number, line: state.record_line + error.line - 1, column: error.column + if (error.line == 1) state.record_column - 1 else 0, message: error.message }
			{ state: next, consumed, outcome: if (state.props.format == Ndjson and !limit_error(error.message)) RecordError(located) else Failed(located) }
		}
	}
}

advance : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, JsonValue, JsonStream.Error)
advance = |initial, bytes, end| {
	var $state = initial
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		match utf8($state, byte) {
			Err(error) => return { state: $state, consumed: $index, outcome: Failed(error) }
			Ok(validated) => {
				$state = validated
			}
		}
		if $state.props.format == Ndjson {
			if byte == '\n' {
				$state = move($state, byte)
				$index = $index + 1
				if !$state.record.all(whitespace) {
					return record_result($state, $index)
				}
				$state = { ..$state, record: [], record_line: $state.line, record_column: $state.column }
			} else {
				if $state.record.len() >= $state.props.max_record_bytes {
					return { state: $state, consumed: $index, outcome: Failed(problem($state, "record byte limit exceeded")) }
				}
				$state = move({ ..$state, record: $state.record.append(byte) }, byte)
				$index = $index + 1
			}
		} else {
			collecting = !$state.record.is_empty()
			if $state.phase == Bom {
				if ($state.bom == 1 and byte != 0xBB) or ($state.bom == 2 and byte != 0xBF) {
					return { state: $state, consumed: $index, outcome: Failed(problem($state, "invalid byte order mark")) }
				}
				$state = { ..$state, bom: $state.bom + 1, phase: if ($state.bom == 2) Start else Bom }
			} else if $state.comment != Normal {
				if collecting {
					if $state.record.len() >= $state.props.max_record_bytes {
						return { state: $state, consumed: $index, outcome: Failed(problem($state, "record byte limit exceeded")) }
					}
					$state = { ..$state, record: $state.record.append(byte) }
				}
				match $state.comment {
					Slash => {
						if byte == '/' {
							$state = { ..$state, comment: LineComment }
						}
						else if byte == '*' {
							$state = { ..$state, comment: BlockComment }
						}
						else {
							return { state: $state, consumed: $index, outcome: Failed(problem($state, "expected a JSON comment")) }
						}
					}
					LineComment => {
						if byte == '\r' or byte == '\n' {
							$state = { ..$state, comment: Normal }
						}
					}
					BlockComment => {
						if byte == '*' {
							$state = { ..$state, comment: BlockStar }
						}
					}
					BlockStar => {
						$state = { ..$state, comment: if (byte == '/') Normal else if (byte == '*') BlockStar else BlockComment }
					}
					Normal => {}
				}
			} else if !collecting {
				if $state.line == 1 and $state.column == 1 and byte == 0xEF and $state.props.json.bom {
					$state = { ..$state, phase: Bom, bom: 1 }
				} else if whitespace(byte) {
					{}
				}
				else if byte == '/' and $state.props.json.comments {
					$state = { ..$state, comment: Slash }
				}
				else if $state.phase == Start and byte == '[' {
					$state = { ..$state, phase: First }
				}
				else if ($state.phase == First or $state.phase == Next) and byte == ']' {
					if $state.phase == Next and !$state.props.json.trailing_commas {
						return { state: $state, consumed: $index, outcome: Failed(problem($state, "trailing comma is not allowed")) }
					}
					$state = { ..$state, phase: End }
				} else if $state.phase == Start or $state.phase == End or byte == ',' {
					return { state: $state, consumed: $index, outcome: Failed(problem($state, if ($state.phase == Start) "expected a top-level JSON array" else if ($state.phase == End) "unexpected content after the array" else "expected an array item")) }
				} else {
					if $state.props.max_record_bytes == 0 {
						return { state: $state, consumed: $index, outcome: Failed(problem($state, "record byte limit exceeded")) }
					}
					$state = { ..$state, record_line: $state.line, record_column: $state.column }
					match scan($state, byte) {
						Ok(scanned) => {
							$state = scanned
						}
						Err(error) => return { state: $state, consumed: $index, outcome: Failed(error) }
					}
				}
			} else if !$state.string and $state.stack.is_empty() and (byte == ',' or byte == ']') {
				$state = move({ ..$state, phase: if (byte == ',') Next else End }, byte)
				return record_result($state, $index + 1)
			} else {
				match scan($state, byte) {
					Ok(scanned) => {
						$state = scanned
					}
					Err(error) => return { state: $state, consumed: $index, outcome: Failed(error) }
				}
			}
			$state = move($state, byte)
			$index = $index + 1
		}
	}
	if end == More {
		return { state: $state, consumed: $index, outcome: NeedInput }
	}
	if $state.utf8_remaining != 0 {
		return { state: $state, consumed: $index, outcome: Failed(problem($state, "incomplete UTF-8 character")) }
	}
	if $state.props.format == Ndjson {
		if !$state.record.all(whitespace) {
			return record_result($state, $index)
		}
		return { state: $state, consumed: $index, outcome: Done }
	}
	if $state.phase != End or !$state.record.is_empty() or ($state.comment != Normal and $state.comment != LineComment) {
		return { state: $state, consumed: $index, outcome: Failed(problem($state, "incomplete JSON array")) }
	}
	{ state: $state, consumed: $index, outcome: Done }
}

scan : State, U8 -> Try(State, JsonStream.Error)
scan = |state, byte| {
	if state.record.len() >= state.props.max_record_bytes {
		return Err(problem(state, "record byte limit exceeded"))
	}
	var $next = { ..state, record: state.record.append(byte) }
	if state.string {
		if state.escape {
			$next = { ..$next, escape: False }
		}
		else if byte == '\\' {
			$next = { ..$next, escape: True }
		}
		else if byte == '"' {
			$next = { ..$next, string: False }
		}
	} else if byte == '"' {
		$next = { ..$next, string: True }
	}
	else if byte == '/' and state.props.json.comments {
		$next = { ..$next, comment: Slash }
	}
	else if byte == '[' or byte == '{' {
		if state.stack.len() >= state.props.json.max_depth {
			return Err(problem(state, "nesting limit exceeded"))
		}
		$next = { ..$next, stack: state.stack.append(if (byte == '[') ']' else '}') }
	} else if byte == ']' or byte == '}' {
		if (state.stack.last() ?? 0) != byte {
			return Err(problem(state, "mismatched closing delimiter"))
		}
		$next = { ..$next, stack: state.stack.drop_last(1) }
	}
	Ok($next)
}

Writer : { props : JsonStream.WriteProps, started : Bool }

write : Writer, JsonValue -> Try({ state : Writer, bytes : List(U8) }, JsonStream.WriteError)
write = |state, value| {
	text = JsonValue.to_str_with(value, { sort_keys: state.props.sort_keys, max_bytes: state.props.max_record_bytes })?
	bytes = if state.props.format == Ndjson {
		"${text}\n".to_utf8()
	} else {
		"${if (state.started) "," else "["}${text}".to_utf8()
	}
	Ok({ state: { ..state, started: True }, bytes })
}

finish : Writer -> Try(List(U8), JsonStream.WriteError)
finish = |state| Ok(if (state.props.format == Ndjson) [] else if (state.started) "]".to_utf8() else "[]".to_utf8())

expect Streaming.collect(JsonStream.parse_iter(["1\n{\"x\":".to_utf8(), "true}\n\n\"é\"".to_utf8()].iter(), {})) == Ok([Number("1"), Object([{ key: "x", value: Bool(True) }]), String("é")])
expect Streaming.collect(JsonStream.parse_iter(["[1,{\"x\":".to_utf8(), "[true]},\"é\"]".to_utf8()].iter(), { format: Array })) == Ok([Number("1"), Object([{ key: "x", value: Array([Bool(True)]) }]), String("é")])

# Every two-piece split and every byte boundary must preserve values and errors.
splits_agree : Str, JsonStream.Props -> Bool
splits_agree = |text, props| {
	bytes = text.to_utf8()
	want = List.from_iter(JsonStream.parse_iter([bytes].iter(), props))
	List.from_iter((0.U64).to(bytes.len() + 1)).all(|split| List.from_iter(JsonStream.parse_iter([[], bytes.take_first(split), [], bytes.drop_first(split), []].iter(), props)) == want)
		and List.from_iter(JsonStream.parse_iter(bytes.map(|byte| [byte]).iter(), props)) == want
}

expect {
	text = "\u(FEFF) /* wrapper ],{ */ [ /* item */ {\"é\":\"\\ud83d\\ude00\",\"q\":\"ends\\\\\"}, -1.2e+30, true /* ], */ , ] // final"
	props : JsonStream.Props
	props = { format: Array, json: { bom: True, comments: True, trailing_commas: True } }
	splits_agree(text, props) and Streaming.collect(JsonStream.parse_iter([text.to_utf8()].iter(), props)) == Ok([Object([{ key: "é", value: String("😀") }, { key: "q", value: String("ends\\") }]), Number("-1.2e+30"), Bool(True)])
}
expect ["", "\n\r\n \t\r\n", "null\n42\r\n", "\"é😀\"\n{\"a\":[1,2]}\n", "tru\nfalse\n", "\"unterminated\n1\n"].all(|text| splits_agree(text, {}))
expect ["[]", " [ ] \r\n", "[1,2]", "[1,]", "[,1]", "[1 2]", "[trueX]", "[\"a\\\"b\"]suffix", "[{\"a\":]", "[1", "[", "[] []", "[\"\\uD800\"]"].all(|text| splits_agree(text, { format: Array }))
expect ["[1,]", "[1 2]", "[trueX]", "[1", "[{\"a\":]", "[]x", "null", "", "[/*unfinished]"].all(|text| Streaming.collect(JsonStream.parse_iter([text.to_utf8()].iter(), { format: Array, json: { comments: True } })).is_err())
expect Streaming.collect(JsonStream.parse_iter(["[ 1 , 2 ]".to_utf8()].iter(), { format: Array })) == Ok([Number("1"), Number("2")])
expect {
	props : JsonStream.Props
	props = { errors: { error_policy: ReportInvalidRecords } }
	match Streaming.collect_report(JsonStream.parse_iter(["1\nno\ntrue\n".to_utf8()].iter(), props)) {
		Ok(report) => report.values == [Number("1"), Bool(True)] and report.errors.len() == 1 and report.errors.all(|error| error.record == 2 and error.line == 2 and error.column > 0) and report.report == { emitted_items: 2, rejected_records: 1 }
		Err(_) => False
	}
}
expect {
	items = List.from_iter(JsonStream.parse_iter(["bad\n1\nbad\n".to_utf8()].iter(), { errors: { error_policy: SkipInvalidRecords, max_errors: 1 } }))
	match items {
		[Ok(Value(Number("1"))), Err(ErrorLimitExceeded({ max: 1, .. }))] => True
		_ => False
	}
}
expect {
	items = List.from_iter(JsonStream.parse_iter(["[bad,1]".to_utf8()].iter(), { format: Array, errors: { error_policy: ReportInvalidRecords } }))
	match items {
		[Err(ParseErr(_))] => True
		_ => False
	}
}
expect Streaming.collect(JsonStream.parse_iter(["[123]".to_utf8()].iter(), { format: Array, max_record_bytes: 3 })) == Ok([Number("123")])
expect Streaming.collect(JsonStream.parse_iter(["123\n".to_utf8()].iter(), { max_record_bytes: 3 })) == Ok([Number("123")])
expect Streaming.collect(JsonStream.parse_iter(["[1234]".to_utf8()].iter(), { format: Array, max_record_bytes: 3 })).is_err()
expect Streaming.collect(JsonStream.parse_iter(["1234\n".to_utf8()].iter(), { max_record_bytes: 3, errors: { error_policy: SkipInvalidRecords } })).is_err()
expect Streaming.collect(JsonStream.parse_iter(["[]".to_utf8()].iter(), { format: Array, max_record_bytes: 0 })) == Ok([])
expect Streaming.collect(JsonStream.parse_iter(["[1]".to_utf8()].iter(), { format: Array, max_record_bytes: 0 })).is_err()
expect Streaming.collect(JsonStream.parse_iter(["[[],{}]".to_utf8()].iter(), { format: Array, json: { max_depth: 1, max_items: 0 } })) == Ok([Array([]), Object([])])
expect Streaming.collect(JsonStream.parse_iter(["[1,2]".to_utf8()].iter(), { format: Array, json: { max_depth: 0, max_items: 0 } })) == Ok([Number("1"), Number("2")])
expect Streaming.collect(JsonStream.parse_iter(["[[[]]]".to_utf8()].iter(), { format: Array, json: { max_depth: 1 } })).is_err()
expect {
	options : List(JsonValue.Options)
	options = [{ max_depth: 0 }, { max_items: 0 }, { max_string_bytes: 0 }, { max_number_digits: 0 }, { max_bytes: 0 }]
	inputs = ["[]", "[1]", "\"x\"", "1", "null"]
	List.from_iter(options.iter().with_index()).all(
		|pair| {
			text = inputs.get(pair.0) ?? ""
			items = List.from_iter(JsonStream.parse_iter(["${text}\ntrue\n".to_utf8()].iter(), { json: pair.1, errors: { error_policy: ReportInvalidRecords } }))
			match items {
				[Err(ParseErr(_))] => True
				_ => False
			}
		},
	)
}
expect {
	bad = ["[/*".to_utf8().concat([0xFF]).concat("*/1]".to_utf8()), "[] //".to_utf8().concat([0xED, 0xA0, 0x80]), [0xF0.U8, 0x90], [0xC0.U8, 0x80], [0xF4.U8, 0x90, 0x80, 0x80]]
	bad.all(|bytes| Streaming.collect(JsonStream.parse_iter(bytes.map(|byte| [byte]).iter(), { format: Array, json: { comments: True }, errors: { error_policy: SkipInvalidRecords } })).is_err())
}
expect {
	values = [Number("1"), Object([{ key: "é", value: String("\n") }]), Array([Bool(True), Null])]
	[Ndjson, Array].all(
		|format| {
			parts = List.from_iter(JsonStream.write_iter(values.iter(), { format: format }, 1))
			var $bytes = []
			for part in parts {
				match part {
					Ok(chunk) => {
						$bytes = $bytes.concat(chunk)
					}
					Err(_) => return False
				}
			}
			Streaming.collect(JsonStream.parse_iter($bytes.map(|byte| [byte]).iter(), { format: format })) == Ok(values)
		},
	)
}
expect List.from_iter(JsonStream.write_iter([].iter(), { format: Array }, 1)) == [Ok("[".to_utf8()), Ok("]".to_utf8())]
expect List.from_iter(JsonStream.write_iter([].iter(), {}, 1)) == []
expect List.from_iter(JsonStream.write_iter([Number("NaN")].iter(), { format: Array }, 1)) == [Err(EncodeErr(InvalidNumber("NaN")))]
expect List.from_iter(JsonStream.write_iter([String("x")].iter(), { max_record_bytes: 2 }, 1)) == [Err(EncodeErr(TooLarge(2)))]
expect List.from_iter(JsonStream.write_iter([Number("1")].iter(), {}, 0)) == [Err(InvalidChunkSize)]
expect {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| if index == 0 {
			Ok(("[1,".to_utf8(), 1))
		} else {
			crash "read beyond the first requested value"
		},
	)
	match JsonStream.parse_iter(chunks, { format: Array }).next() {
		One({ item: Ok(Value(Number("1"))), .. }) => True
		_ => False
	}
}

expect Streaming.collect(JsonStream.parse_iter(["[\u(FEFF)1]".to_utf8()].iter(), { format: Array, json: { bom: True } })).is_err()

# The outer array is framing. Its depth and item count are not charged to
# the individual records' JsonValue options.
expect {
	var $equal = True
	for depth in (0.U64).to(4) {
		for items in (0.U64).to(4) {
			options : JsonValue.Options
			options = { max_depth: depth, max_items: items }
			for record in ["null", "1", "[]", "{}", "[1]", "[[1]]", "{\"a\":[1,2]}", "{\"a\":1,\"b\":2}"] {
				actual = Streaming.collect(JsonStream.parse_iter("[${record}]".to_utf8().map(|byte| [byte]).iter(), { format: Array, json: options }))
				match (JsonValue.parse_with(record, options), actual) {
					(Ok(want), Ok([found])) => {
						if want != found {
							$equal = False
						}
					}
					(Err(_), Err(_)) => {}
					_ => {
						$equal = False
					}
				}
			}
		}
	}
	$equal
}

# ReportInvalidRecords must not turn an invalid encoding into a skipped line.
expect {
	bytes = "1\n".to_utf8().concat([0xFF]).concat("\n2\n".to_utf8())
	match List.from_iter(JsonStream.parse_iter(bytes.map(|byte| [byte]).iter(), { errors: { error_policy: ReportInvalidRecords } })) {
		[Ok(Value(Number("1"))), Err(ParseErr({ record: 2, line: 2, column: 1, message: "invalid UTF-8" }))] => True
		_ => False
	}
}
