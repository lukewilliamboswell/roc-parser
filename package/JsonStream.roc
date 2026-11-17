import Diagnostic
import JsonWorkspace
import JsonConfig
import Parser
import Streaming

## Incremental UTF-8 JSON record streams. Each value owns at most its record's
## bytes. Array framing and invalid encoding are terminal. Independently framed
## NDJSON records may be rejected and skipped.
JsonStream :: [].{

	## NDJSON has one value per physical line. Array emits outer-array items.
	Framing : [Ndjson, Array]

	## JSON options apply to each emitted value, including top_level and limits.
	## Array wrapper comments, a BOM and trailing commas follow those options.
	## max_record_bytes bounds encoded unfinished values before JSON decoding.
	Props := { framing : Framing ?? Ndjson, json : JsonConfig.Props ?? {}, max_record_bytes : U64 ?? 4_194_304, errors : Streaming.RecoveryProps ?? {} }

	## Original one-based line/byte column and one-based record number.
	Error : Diagnostic(JsonConfig.Problem)

	## Record framing failures and the underlying structured JSON problem.
	Problem : JsonConfig.Problem

	## Writers validate a complete value before emitting it. Memory includes one
	## encoded record, bounded by max_record_bytes before rendering.
	WriteProps := { framing : Framing ?? Ndjson, sort_keys : Bool ?? False, max_record_bytes : U64 ?? 4_194_304, chunk_bytes : U64 ?? 65_536 }

	## A value cannot be represented as strict JSON within the writer limits.
	WriteError : [InvalidJsonValue(Error)]

	## Configure the private framing core with a format-owned record parser.
	Decoder(value) : Streaming.Decoder(State(value), value, [InvalidJsonValue(Error)])
	decoder : Props, ParseRecord(value) -> Decoder(value)
	decoder = |props, parse| Streaming.decoder({ initial_state: new_state(props, parse), step: |state, bytes, end| Streaming.map_step_error(advance(state, bytes, end), |error| InvalidJsonValue(error)), errors: props.errors })

	## Configure compact NDJSON or array output with bounded output chunks.
	Encoder(value) : Streaming.Encoder(Writer(value), value, WriteError)
	encoder : WriteProps, Render(value) -> Encoder(value)
	encoder = |props, render| Streaming.encoder({ initial_state: { props, render, started: False }, encode: write, finish, chunk_bytes: props.chunk_bytes })

}

new_state : JsonStream.Props, ParseRecord(value) -> State(value)
new_state = |props, parse| { props, parse, offset: 0, record_offset: 0, phase: Start, record: [], workspace: Box.box(JsonWorkspace.empty()), number: 1, line: 1, column: 1, record_line: 1, record_column: 1, previous_cr: False, string: False, escape: False, stack: [], comment: Normal, bom: 0, utf8_remaining: 0, utf8_low: 0x80, utf8_high: 0xBF }

State(value) : {
	parse : ParseRecord(value),
	offset : U64,
	record_offset : U64,
	props : JsonStream.Props,
	phase : [Start, First, Next, End, Bom],
	record : List(U8),
	workspace : Box(JsonWorkspace(value)),
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

problem : State(value), JsonStream.Problem -> JsonStream.Error
problem = |state, detail| Diagnostic.{ location: Text({ offset: state.record_offset + state.record.len(), line: state.line, column: state.column }), path: [Record(state.number)], problem: detail }

move : State(value), U8 -> State(value)
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

utf8 : State(value), U8 -> Try(State(value), JsonStream.Error)
utf8 = |state, byte| match utf8_next(state.utf8_remaining, state.utf8_low, state.utf8_high, byte) {
	Err(_) => Err(problem(state, InvalidUtf8))
	Ok(validated) => Ok({ ..state, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high })
}

whitespace : U8 -> Bool
whitespace = |byte| byte == ' ' or byte == '\t' or byte == '\r' or byte == '\n'

reset : State(value) -> State(value)
reset = |state| { ..state, record: [], number: state.number + 1, stack: [], string: False, escape: False, comment: Normal, record_line: state.line, record_column: state.column }

# Recovery decisions depend on typed resource failures, never display prose.
limit_error : JsonConfig.Problem -> Bool
limit_error = |detail| match detail {
	LimitExceeded(_) => True
	_ => False
}

record_result : State(value), U64 -> Streaming.Step(State(value), value, JsonStream.Error)
record_result = |state, consumed| {
	# Move the workspace out before parsing; retaining a second reference would
	# force its growing buffers to copy instead of reusing their capacity.
	workspace = state.workspace
	base = { ..state, workspace: Box.box(JsonWorkspace.empty()) }
	text = Str.from_utf8(base.record) ?? ""
	options = if (base.props.framing == Array) { ..base.props.json, bom: False } else base.props.json
	parse = base.parse
	parsed = parse(text, options, Box.unbox(workspace))
	next = { ..reset(base), record_offset: base.offset + consumed, workspace: Box.box(parsed.workspace) }
	match parsed.result {
		Ok(value) => { state: next, consumed, outcome: Item(value) }
		Err(InvalidJsonValue(error)) => {
			position = match error.location {
				Text(at) => at
				_ => crash "JSON record parser must retain its source position"
			}
			located = Diagnostic.{ location: Text({ offset: base.record_offset + position.offset, line: base.record_line + position.line - 1, column: position.column + if (position.line == 1) base.record_column - 1 else 0 }), path: [Record(base.number)].concat(error.path), problem: error.problem }
			{ state: next, consumed, outcome: if (base.props.framing == Ndjson and !limit_error(error.problem)) RecordError(located) else Failed(located) }
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

advance : State(value), List(U8), Streaming.InputEnd -> Streaming.Step(State(value), value, JsonStream.Error)
advance = |state, bytes, end| {
	result = if (state.props.framing == Ndjson) ndjson_step(state, bytes, end) else array_step(state, bytes, end)
	{ ..result, state: { ..result.state, offset: state.offset + result.consumed } }
}

# Separate framing loops keep format selection outside the byte hot path.
ndjson_step : State(value), List(U8), Streaming.InputEnd -> Streaming.Step(State(value), value, JsonStream.Error)
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
				return { state: $state, consumed: $index, outcome: Failed(problem($state, LimitExceeded({ limit: RecordBytes, max: $state.props.max_record_bytes, observed: AtLeast(($state.props.max_record_bytes) + 1) }))) }
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
				$state = { ..$state, record: [], record_offset: initial.offset + $index, record_line: $state.line, record_column: $state.column }
			} else {
				if $state.record.len() >= $state.props.max_record_bytes {
					return { state: $state, consumed: $index, outcome: Failed(problem($state, LimitExceeded({ limit: RecordBytes, max: $state.props.max_record_bytes, observed: AtLeast(($state.props.max_record_bytes) + 1) }))) }
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
		return { state: $state, consumed: $index, outcome: Failed(problem($state, IncompleteUtf8)) }
	}
	if !$state.record.all(whitespace) {
		return record_result($state, $index)
	}
	{ state: $state, consumed: $index, outcome: Done }
}

# Copy each contiguous source span only when yielding or crossing a chunk.
# Owning this small buffer prevents retained values from pinning source chunks.
capture_record : State(value), List(U8), U64, U64 -> State(value)
capture_record = |state, bytes, start, end| {
	len = end - start
	record = if (state.record.is_empty() and len > 0) List.with_capacity(len) else state.record
	{ ..state, record: record.append_sublist(bytes, { start, len }) }
}

# Outside strings these bytes affect framing or physical source positions.
framing_stop : Parser.Utf8.ByteClass
framing_stop = Parser.Utf8.ByteClass.from_predicate(|byte| byte >= 0x80 or byte == '"' or byte == '{' or byte == '}' or byte == '[' or byte == ']' or byte == ',' or byte == '/' or byte == '\r' or byte == '\n')

ArrayScan : {
	record_offset : U64,
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

pack_array : State(value), ArrayScan, List(U8) -> State(value)
pack_array = |base, scanner, record| { ..base, record, phase: scanner.phase, line: scanner.line, column: scanner.column, record_line: scanner.record_line, record_column: scanner.record_column, record_offset: scanner.record_offset, previous_cr: scanner.previous_cr, string: scanner.string, escape: scanner.escape, stack: scanner.stack, comment: scanner.comment, bom: scanner.bom, utf8_remaining: scanner.utf8_remaining, utf8_low: scanner.utf8_low, utf8_high: scanner.utf8_high }

array_problem : State(value), ArrayScan, U64, JsonStream.Problem -> JsonStream.Error
array_problem = |base, scanner, index, detail| Diagnostic.{ location: Text({ offset: base.offset + index, line: scanner.line, column: scanner.column }), path: [Record(base.number)], problem: detail }

array_step : State(value), List(U8), Streaming.InputEnd -> Streaming.Step(State(value), value, JsonStream.Error)
array_step = |initial, bytes, end| {
	base = { ..initial, record: [], stack: [] }
	props = initial.props
	var $record = initial.record
	var $scan = { phase: initial.phase, line: initial.line, column: initial.column, record_offset: initial.record_offset, record_line: initial.record_line, record_column: initial.record_column, previous_cr: initial.previous_cr, string: initial.string, escape: initial.escape, stack: initial.stack, comment: initial.comment, bom: initial.bom, utf8_remaining: initial.utf8_remaining, utf8_low: initial.utf8_low, utf8_high: initial.utf8_high }
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
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
			}
		} else if $scan.string and $scan.comment == Normal {
			if byte >= 0x80 or $scan.utf8_remaining > 0 {
				match utf8_next($scan.utf8_remaining, $scan.utf8_low, $scan.utf8_high, byte) {
					Err(_) => return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, InvalidUtf8)) }
					Ok(validated) => {
						$scan = { ..$scan, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high }
					}
				}
			}
			if $record_bytes >= props.max_record_bytes {
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
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
			run_end = Parser.Utf8.find_any(bytes, $index, framing_stop)
			len = U64.min(run_end - $index, props.max_record_bytes - U64.min($record_bytes, props.max_record_bytes))
			$scan = { ..$scan, column: $scan.column + len, previous_cr: False }
			$index = $index + len
			$record_bytes = $record_bytes + len
			if $index < run_end {
				return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
			}
		} else {
			if byte >= 0x80 or $scan.utf8_remaining > 0 {
				match utf8_next($scan.utf8_remaining, $scan.utf8_low, $scan.utf8_high, byte) {
					Err(_) => return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, InvalidUtf8)) }
					Ok(validated) => {
						$scan = { ..$scan, utf8_remaining: validated.remaining, utf8_low: validated.low, utf8_high: validated.high }
					}
				}
			}
			var $scan_byte = False
			if $scan.phase == Bom {
				if ($scan.bom == 1 and byte != 0xBB) or ($scan.bom == 2 and byte != 0xBF) {
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, InvalidBom)) }
				}
				$scan = { ..$scan, bom: $scan.bom + 1, phase: if ($scan.bom == 2) Start else Bom }
			} else if $scan.comment != Normal {
				if $collecting {
					if $record_bytes >= props.max_record_bytes {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
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
							return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, ExpectedComment)) }
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
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, TrailingComma)) }
					}
					$scan = { ..$scan, phase: End }
				} else if $scan.phase == Start or $scan.phase == End or byte == ',' {
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, if ($scan.phase == Start) ExpectedArray else if ($scan.phase == End) UnexpectedContent else ExpectedArrayItem)) }
				} else {
					if props.max_record_bytes == 0 {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
					}
					$scan = { ..$scan, record_offset: initial.offset + $index, record_line: $scan.line, record_column: $scan.column }
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
					return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: RecordBytes, max: props.max_record_bytes, observed: AtLeast((props.max_record_bytes) + 1) }))) }
				}
				if byte == '"' {
					$scan = { ..$scan, string: True }
				}
				else if byte == '/' and props.json.comments {
					$scan = { ..$scan, comment: Slash }
				}
				else if byte == '[' or byte == '{' {
					if $scan.stack.len() >= props.json.max_depth {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, LimitExceeded({ limit: Nesting, max: props.json.max_depth, observed: AtLeast((props.json.max_depth) + 1) }))) }
					}
					$scan = { ..$scan, stack: $scan.stack.append(if (byte == '[') ']' else '}') }
				} else if byte == ']' or byte == '}' {
					if ($scan.stack.last() ?? 0) != byte {
						return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, MismatchedDelimiter)) }
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
		return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, IncompleteUtf8)) }
	}
	if $scan.phase != End or !$record.is_empty() or ($scan.comment != Normal and $scan.comment != LineComment) {
		return { state: pack_array(base, $scan, $record), consumed: $index, outcome: Failed(array_problem(base, $scan, $index, IncompleteArray)) }
	}
	{ state: pack_array(base, $scan, $record), consumed: $index, outcome: Done }
}

Writer(value) : { props : JsonStream.WriteProps, render : Render(value), started : Bool }

write : Writer(value), value -> Try({ state : Writer(value), bytes : List(U8) }, JsonStream.WriteError)
write = |state, value| {
	render = state.render
	text = render(value, { sort_keys: state.props.sort_keys, max_bytes: state.props.max_record_bytes })?
	bytes = if state.props.framing == Ndjson {
		"${text}\n".to_utf8()
	} else {
		"${if (state.started) "," else "["}${text}".to_utf8()
	}
	Ok({ state: { ..state, started: True }, bytes })
}

finish : Writer(value) -> Try(List(U8), JsonStream.WriteError)
finish = |state| Ok(if (state.props.framing == Ndjson) [] else if (state.started) "]".to_utf8() else "[]".to_utf8())

ParseRecord(value) : Str, JsonConfig.Props, JsonWorkspace(value) -> { result : Try(value, [InvalidJsonValue(JsonStream.Error)]), workspace : JsonWorkspace(value) }

Render(value) : value, JsonConfig.WriteProps -> Try(Str, JsonStream.WriteError)
