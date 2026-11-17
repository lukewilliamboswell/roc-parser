import JsonValue
import Utf8
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
	decoder = |props| { props, phase: Start, record: [], workspace: Box.box(JsonValue.Workspace.empty()), number: 1, line: 1, column: 1, record_line: 1, record_column: 1, previous_cr: False, string: False, escape: False, stack: [], comment: Normal, bom: 0, utf8_remaining: 0, utf8_low: 0x80, utf8_high: 0xBF }

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
	workspace : Box(JsonValue.Workspace),
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
Utf8State : { remaining : U8, low : U8, high : U8 }

utf8_next : U8, U8, U8, U8 -> Try(Utf8State, [InvalidUtf8])
utf8_next = |remaining, low, high, byte| {
	if remaining > 0 {
		if byte < low or byte > high {
			return Err(InvalidUtf8)
		}
		return Ok({ remaining: remaining - 1, low: 0x80, high: 0xBF })
	}
	if byte < 0x80 {
		return Ok({ remaining: 0, low: 0x80, high: 0xBF })
	}
	if byte >= 0xC2 and byte <= 0xDF {
		return Ok({ remaining: 1, low: 0x80, high: 0xBF })
	}
	if byte >= 0xE0 and byte <= 0xEF {
		return Ok({ remaining: 2, low: if (byte == 0xE0) 0xA0 else 0x80, high: if (byte == 0xED) 0x9F else 0xBF })
	}
	if byte >= 0xF0 and byte <= 0xF4 {
		return Ok({ remaining: 3, low: if (byte == 0xF0) 0x90 else 0x80, high: if (byte == 0xF4) 0x8F else 0xBF })
	}
	Err(InvalidUtf8)
}

utf8 : State, U8 -> Try(State, JsonStream.Error)
utf8 = |state, byte| match utf8_next(state.utf8_remaining, state.utf8_low, state.utf8_high, byte) {
	Err(_) => Err(problem(state, "invalid UTF-8"))
	Ok(validated) => Ok({ ..state, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high })
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
	# Move the workspace out before parsing; retaining a second reference would
	# force its growing buffers to copy instead of reusing their capacity.
	workspace = state.workspace
	base = { ..state, workspace: Box.box(JsonValue.Workspace.empty()) }
	text = Str.from_utf8(base.record) ?? ""
	options = if (base.props.format == Array) { ..base.props.json, bom: False } else base.props.json
	parsed = JsonValue.parse_reusing(text, options, Box.unbox(workspace))
	next = { ..reset(base), workspace: Box.box(parsed.workspace) }
	match parsed.result {
		Ok(value) => { state: next, consumed, outcome: Item(value) }
		Err(InvalidJsonValue(error)) => {
			located = { record: base.number, line: base.record_line + error.line - 1, column: error.column + if (error.line == 1) base.record_column - 1 else 0, message: error.message }
			{ state: next, consumed, outcome: if (base.props.format == Ndjson and !limit_error(error.message)) RecordError(located) else Failed(located) }
		}
	}
}

# Framing NDJSON does not inspect JSON punctuation. Within an array string,
# only quotes and backslashes change scanner state. Bulk-copy ASCII runs while
# the exceptional-byte path maintains UTF-8 and physical-line diagnostics.
ascii_run_end : List(U8), U64, Bool -> U64
ascii_run_end = |bytes, start, string| {
	cr = U8x16.splat('\r')
	lf = U8x16.splat('\n')
	quote_vector = U8x16.splat('"')
	backslash = U8x16.splat('\\')
	var $pos = start
	while True {
		match U8x16.load(bytes, $pos) {
			Ok(vector) => {
				lines = U8x16.bitwise_or(U8x16.eq_lanes(vector, cr), U8x16.eq_lanes(vector, lf))
				stops = if (string) U8x16.bitwise_or(lines, U8x16.bitwise_or(U8x16.eq_lanes(vector, quote_vector), U8x16.eq_lanes(vector, backslash))) else lines
				mask = U8x16.to_bitmask(U8x16.bitwise_or(vector, stops))
				if mask != 0 {
					return $pos + U16.count_trailing_zero_bits(mask).to_u64()
				}
				$pos = $pos + 16
			}
			Err(_) => {
				while $pos < bytes.len() {
					byte = bytes.get($pos) ?? 0
					if byte >= 0x80 or byte == '\r' or byte == '\n' or (string and (byte == '"' or byte == '\\')) {
						return $pos
					}
					$pos = $pos + 1
				}
				return $pos
			}
		}
	}
}

advance : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, JsonValue, JsonStream.Error)
advance = |state, bytes, end| if (state.props.format == Ndjson) ndjson_step(state, bytes, end) else array_step(state, bytes, end)

# Separate framing loops keep format selection outside the byte hot path.
ndjson_step : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, JsonValue, JsonStream.Error)
ndjson_step = |initial, bytes, end| {
	var $state = initial
	var $index = 0.U64
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if $state.utf8_remaining == 0 and byte < 0x80 and byte != '\r' and byte != '\n' {
			run_end = ascii_run_end(bytes, $index, False)
			len = U64.min(run_end - $index, $state.props.max_record_bytes - U64.min($state.record.len(), $state.props.max_record_bytes))
			if $state.record.is_empty() and len > 0 {
				$state = { ..$state, record: List.with_capacity(U64.min($state.props.max_record_bytes, U64.max(32, len))) }
			}
			$state = { ..$state, record: $state.record.append_sublist(bytes, { start: $index, len }), column: $state.column + len, previous_cr: False }
			$index = $index + len
			if $index < run_end {
				return { state: $state, consumed: $index, outcome: Failed(problem($state, "record byte limit exceeded")) }
			}
		} else {
			if byte >= 0x80 or $state.utf8_remaining > 0 {
				match utf8($state, byte) {
					Err(error) => return { state: $state, consumed: $index, outcome: Failed(error) }
					Ok(validated) => {
						$state = validated
					}
				}
			}
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
		}
	}
	if end == More {
		return { state: $state, consumed: $index, outcome: NeedInput }
	}
	if $state.utf8_remaining != 0 {
		return { state: $state, consumed: $index, outcome: Failed(problem($state, "incomplete UTF-8 character")) }
	}
	if !$state.record.all(whitespace) {
		return record_result($state, $index)
	}
	{ state: $state, consumed: $index, outcome: Done }
}

# Copy each contiguous source span only when yielding or crossing a chunk.
# Owning this small buffer prevents retained values from pinning source chunks.
capture_record : State, List(U8), U64, U64 -> State
capture_record = |state, bytes, start, end| {
	len = end - start
	record = if (state.record.is_empty() and len > 0) List.with_capacity(len) else state.record
	{ ..state, record: record.append_sublist(bytes, { start, len }) }
}

# Outside strings these bytes affect framing or physical source positions.
framing_stop : Utf8.ByteClass
framing_stop = Utf8.ByteClass.from_predicate(|byte| byte >= 0x80 or byte == '"' or byte == '{' or byte == '}' or byte == '[' or byte == ']' or byte == ',' or byte == '/' or byte == '\r' or byte == '\n')

ArrayScan : {
	phase : [Start, First, Next, End, Bom],
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

pack_array : State, ArrayScan, List(U8) -> State
pack_array = |base, scanner, record| { ..base, record, phase: scanner.phase, line: scanner.line, column: scanner.column, record_line: scanner.record_line, record_column: scanner.record_column, previous_cr: scanner.previous_cr, string: scanner.string, escape: scanner.escape, stack: scanner.stack, comment: scanner.comment, bom: scanner.bom, utf8_remaining: scanner.utf8_remaining, utf8_low: scanner.utf8_low, utf8_high: scanner.utf8_high }

array_problem : State, ArrayScan, Str -> JsonStream.Error
array_problem = |base, scanner, message| { record: base.number, line: scanner.line, column: scanner.column, message }

array_step : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, JsonValue, JsonStream.Error)
array_step = |initial, bytes, end| {
	base = { ..initial, record: [], stack: [] }
	props = initial.props
	var $record = initial.record
	var $scan = { phase: initial.phase, line: initial.line, column: initial.column, record_line: initial.record_line, record_column: initial.record_column, previous_cr: initial.previous_cr, string: initial.string, escape: initial.escape, stack: initial.stack, comment: initial.comment, bom: initial.bom, utf8_remaining: initial.utf8_remaining, utf8_low: initial.utf8_low, utf8_high: initial.utf8_high }
	var $index = 0
	var $collecting = !$record.is_empty()
	var $capture_start = 0.U64
	var $record_bytes = $record.len()
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		# Consume an entirely ASCII, unescaped string in one framing step.
		# A miss uses the ordinary split/encoding/budget path unchanged.
		plain_end = if byte == '"' and !$scan.string and $collecting and $scan.comment == Normal and $scan.utf8_remaining == 0 {
			stop = ascii_run_end(bytes, $index + 1, True)
			next_byte = bytes.get(stop + 1) ?? 0
			tail = if (next_byte == ':' or (next_byte == ',' and !$scan.stack.is_empty())) stop + 2 else stop + 1
			if bytes.get(stop) == Ok('"') and tail - $index <= props.max_record_bytes - U64.min($record_bytes, props.max_record_bytes) {
				tail
			} else $index
		} else $index
		if plain_end > $index {
			len = plain_end - $index
			$scan = { ..$scan, column: $scan.column + len, previous_cr: False }
			$index = plain_end
			$record_bytes = $record_bytes + len
		} else if $scan.utf8_remaining == 0 and byte < 0x80 and byte != '\r' and byte != '\n' and $scan.string and !$scan.escape and $scan.comment == Normal and byte != '"' and byte != '\\' {
			run_end = ascii_run_end(bytes, $index, True)
			len = U64.min(run_end - $index, props.max_record_bytes - U64.min($record_bytes, props.max_record_bytes))
			$scan = { ..$scan, column: $scan.column + len, previous_cr: False }
			$index = $index + len
			$record_bytes = $record_bytes + len
			if $index < run_end {
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
			}
		} else if $scan.string and $scan.comment == Normal {
			if byte >= 0x80 or $scan.utf8_remaining > 0 {
				match utf8_next($scan.utf8_remaining, $scan.utf8_low, $scan.utf8_high, byte) {
					Err(_) => return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "invalid UTF-8")) }
					Ok(validated) => {
						$scan = { ..$scan, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high }
					}
				}
			}
			if $record_bytes >= props.max_record_bytes {
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
			}
			if $scan.escape {
				$scan = { ..$scan, escape: False }
			}
			else if byte == '\\' {
				$scan = { ..$scan, escape: True }
			}
			else if byte == '"' {
				$scan = { ..$scan, string: False }
			}
			$record_bytes = $record_bytes + 1
			if byte == '\r' {
				$scan = { ..$scan, line: $scan.line + 1, column: 1, previous_cr: True }
			}
			else if byte == '\n' {
				$scan = { ..$scan, line: $scan.line + if ($scan.previous_cr) 0 else 1, column: 1, previous_cr: False }
			}
			else {
				$scan = { ..$scan, column: $scan.column + 1, previous_cr: False }
			}
			$index = $index + 1
		} else if $collecting and $scan.comment == Normal and $scan.utf8_remaining == 0 and !framing_stop.contains(byte) {
			run_end = Utf8.find_any(bytes, $index, framing_stop)
			len = U64.min(run_end - $index, props.max_record_bytes - U64.min($record_bytes, props.max_record_bytes))
			$scan = { ..$scan, column: $scan.column + len, previous_cr: False }
			$index = $index + len
			$record_bytes = $record_bytes + len
			if $index < run_end {
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
			}
		} else {
			if byte >= 0x80 or $scan.utf8_remaining > 0 {
				match utf8_next($scan.utf8_remaining, $scan.utf8_low, $scan.utf8_high, byte) {
					Err(_) => return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "invalid UTF-8")) }
					Ok(validated) => {
						$scan = { ..$scan, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high }
					}
				}
			}
			var $scan_byte = False
			if $scan.phase == Bom {
				if ($scan.bom == 1 and byte != 0xBB) or ($scan.bom == 2 and byte != 0xBF) {
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "invalid byte order mark")) }
				}
				$scan = { ..$scan, bom: $scan.bom + 1, phase: if ($scan.bom == 2) Start else Bom }
			} else if $scan.comment != Normal {
				if $collecting {
					if $record_bytes >= props.max_record_bytes {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
					}
				}
				match $scan.comment {
					Slash => {
						if byte == '/' {
							$scan = { ..$scan, comment: LineComment }
						}
						else if byte == '*' {
							$scan = { ..$scan, comment: BlockComment }
						}
						else {
							return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "expected a JSON comment")) }
						}
					}
					LineComment => {
						if byte == '\r' or byte == '\n' {
							$scan = { ..$scan, comment: Normal }
						}
					}
					BlockComment => {
						if byte == '*' {
							$scan = { ..$scan, comment: BlockStar }
						}
					}
					BlockStar => {
						$scan = { ..$scan, comment: if (byte == '/') Normal else if (byte == '*') BlockStar else BlockComment }
					}
					Normal => {}
				}
			} else if !$collecting {
				if $scan.line == 1 and $scan.column == 1 and byte == 0xEF and props.json.bom {
					$scan = { ..$scan, phase: Bom, bom: 1 }
				} else if whitespace(byte) {
					{}
				}
				else if byte == '/' and props.json.comments {
					$scan = { ..$scan, comment: Slash }
				}
				else if $scan.phase == Start and byte == '[' {
					$scan = { ..$scan, phase: First }
				}
				else if ($scan.phase == First or $scan.phase == Next) and byte == ']' {
					if $scan.phase == Next and !props.json.trailing_commas {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "trailing comma is not allowed")) }
					}
					$scan = { ..$scan, phase: End }
				} else if $scan.phase == Start or $scan.phase == End or byte == ',' {
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, if ($scan.phase == Start) "expected a top-level JSON array" else if ($scan.phase == End) "unexpected content after the array" else "expected an array item")) }
				} else {
					if props.max_record_bytes == 0 {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
					}
					$scan = { ..$scan, record_line: $scan.line, record_column: $scan.column }
					$collecting = True
					$capture_start = $index
					$scan_byte = True
				}
			} else if !$scan.string and $scan.stack.is_empty() and (byte == ',' or byte == ']') {
				$scan = { ..$scan, phase: if (byte == ',') Next else End, column: $scan.column + 1, previous_cr: False }
				return record_result(capture_record(pack_array(base, $scan, $record), bytes, $capture_start, $index), $index + 1)
			} else {
				$scan_byte = True
			}
			if $scan_byte {
				if $record_bytes >= props.max_record_bytes {
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "record byte limit exceeded")) }
				}
				if byte == '"' {
					$scan = { ..$scan, string: True }
				}
				else if byte == '/' and props.json.comments {
					$scan = { ..$scan, comment: Slash }
				}
				else if byte == '[' or byte == '{' {
					if $scan.stack.len() >= props.json.max_depth {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "nesting limit exceeded")) }
					}
					$scan = { ..$scan, stack: $scan.stack.append(if (byte == '[') ']' else '}') }
				} else if byte == ']' or byte == '}' {
					if ($scan.stack.last() ?? 0) != byte {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "mismatched closing delimiter")) }
					}
					$scan = { ..$scan, stack: $scan.stack.drop_last(1) }
				}
			}
			if $collecting {
				$record_bytes = $record_bytes + 1
			}
			if byte == '\r' {
				$scan = { ..$scan, line: $scan.line + 1, column: 1, previous_cr: True }
			} else if byte == '\n' {
				$scan = { ..$scan, line: $scan.line + if ($scan.previous_cr) 0 else 1, column: 1, previous_cr: False }
			} else {
				$scan = { ..$scan, column: $scan.column + 1, previous_cr: False }
			}
			$index = $index + 1
		}
	}
	if $collecting {
		$record = capture_record(pack_array(base, $scan, $record), bytes, $capture_start, $index).record
	}
	if end == More {
		return { state: pack_array(base, $scan, $record), consumed: $index, outcome: NeedInput }
	}
	if $scan.utf8_remaining != 0 {
		return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "incomplete UTF-8 character")) }
	}
	if $scan.phase != End or !$record.is_empty() or ($scan.comment != Normal and $scan.comment != LineComment) {
		return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, "incomplete JSON array")) }
	}
	{ state: pack_array(base, $scan, $record), consumed: $index, outcome: Done }
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

expect {
	text = "[\"x\u(0)\r\ny\", NaN, {\"x\":1,\"x\":2}, \"\\ud800\"]"
	props : JsonStream.Props
	props = { format: Array, json: { control_characters: True, non_finite: True, duplicates: KeepLast, lone_surrogates: Replace } }
	splits_agree(text, props) and Streaming.collect(JsonStream.parse_iter([text.to_utf8()].iter(), props)) == Ok([String("x\u(0)\r\ny"), Number("NaN"), Object([{ key: "x", value: Number("2") }]), String("�")])
}

# The complete-string shortcut may consume a colon or a nested comma, but must
# preserve framing, source positions, limits, and every split around that tail.
expect ["[{\"name\":\"value\",\"x\":\"\"}]", "[[\"x\",\"y\"],\"z\"]", "[{\"x\":\"value\",\"y\":\"escaped\\n\"}]", "[{\"x\"::1}]", "[{\"x\":\"v\"},]\n", "[{\"x\":\"v\"}]junk"].all(
	|text| {
		var $same = True
		for limit in (0.U64).to(40) {
			$same = $same and splits_agree(text, { format: Array, max_record_bytes: limit })
		}
		$same
	},
)
