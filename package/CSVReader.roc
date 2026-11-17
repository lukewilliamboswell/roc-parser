import Streaming

## Stateful RFC 4180 byte scanning shared by CSV's streaming entrypoints.
CSVReader :: [].{

	## Limits bound the unfinished record, including fields retained for decoding.
	Props := { max_field_bytes : U64 ?? 16_777_216, max_record_bytes : U64 ?? 67_108_864, max_fields : U64 ?? 65_536 }

	Problem : [UnterminatedQuotedField, TextAfterQuote, RecordBytesExceeded({ max : U64 }), FieldBytesExceeded({ max : U64 }), FieldCountExceeded({ max : U64 })]

	## One-based record, field, physical line and byte column.
	Error : { record : U64, field : U64, line : U64, column : U64, problem : Problem }

	## A source position for typed-decoding diagnostics.
	Location : { line : U64, column : U64 }

	## Parser state owns only the unfinished record and the latest positions.
	State : {
		props : Props,
		field : List(U8),
		fields : List(List(U8)),
		starts : List(Location),
		last_starts : List(Location),
		last_end : Location,
		mode : [Start, Unquoted, Quoted, AfterQuote],
		record_started : Bool,
		record_bytes : U64,
		offset : U64,
		line : U64,
		column : U64,
		previous_cr : Bool,
		skip_lf : Bool,
		record : U64,
		field_start : Location,
	}

	## Start scanning; BOM bytes remain part of the first field.
	new : Props -> State
	new = |props| { props, field: [], fields: [], starts: [], last_starts: [], last_end: { line: 1, column: 1 }, mode: Start, record_started: False, record_bytes: 0, offset: 0, line: 1, column: 1, previous_cr: False, skip_lf: False, record: 1, field_start: { line: 1, column: 1 } }

	## Consume at most one complete record. Syntax and limit failures are terminal.
	step : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, List(List(U8)), Error)
	step = |initial, bytes, end| {
		var $state = initial
		var $index = 0
		while $index < bytes.len() {
			byte = bytes.get($index) ?? 0
			if $state.skip_lf and byte == '\n' {
				$state = advance({ ..$state, skip_lf: False }, byte)
				$state = { ..$state, field_start: { line: $state.line, column: $state.column } }
				$index = $index + 1
			} else {
				$state = { ..$state, skip_lf: False }
				if $state.record_bytes >= $state.props.max_record_bytes {
					return failure($state, $index, RecordBytesExceeded({ max: $state.props.max_record_bytes }))
				}
				if $state.mode == Start and $state.fields.len() >= $state.props.max_fields {
					return failure($state, $index, FieldCountExceeded({ max: $state.props.max_fields }))
				}
				$state = { ..$state, record_started: True, record_bytes: $state.record_bytes + 1 }
				is_break = byte == '\r' or byte == '\n'
				if $state.mode != Quoted and (byte == ',' or is_break) {
					$state = finish_field($state)
					at = { line: $state.line, column: $state.column }
					$state = advance($state, byte)
					$index = $index + 1
					if is_break {
						fields = $state.fields
						$state = finish_record($state, at, byte == '\r')
						return { state: $state, consumed: $index, outcome: Item(fields) }
					}
					$state = { ..$state, field_start: { line: $state.line, column: $state.column } }
				} else {
					match $state.mode {
						Start if byte == '"' => {
							$state = { ..$state, mode: Quoted }
						}
						Quoted if byte == '"' => {
							$state = { ..$state, mode: AfterQuote }
						}
						AfterQuote if byte != '"' => return { state: $state, consumed: $index, outcome: Failed({ record: $state.record, field: $state.fields.len() + 1, line: $state.line, column: $state.column, problem: TextAfterQuote }) }
						_ => {
							if $state.field.len() >= $state.props.max_field_bytes {
								return failure($state, $index, FieldBytesExceeded({ max: $state.props.max_field_bytes }))
							}
							$state = { ..$state, field: $state.field.append(byte), mode: if ($state.mode == AfterQuote or $state.mode == Quoted) Quoted else Unquoted }
						}
					}
					$state = advance($state, byte)
					$index = $index + 1
				}
			}
		}
		if end == More {
			return { state: $state, consumed: $index, outcome: NeedInput }
		}
		if $state.mode == Quoted {
			return { state: $state, consumed: $index, outcome: Failed({ record: $state.record, field: $state.fields.len() + 1, line: $state.field_start.line, column: $state.field_start.column, problem: UnterminatedQuotedField }) }
		}
		if $state.record_started {
			if $state.fields.len() >= $state.props.max_fields {
				return failure($state, $index, FieldCountExceeded({ max: $state.props.max_fields }))
			}
			$state = finish_field($state)
			fields = $state.fields
			$state = finish_record($state, { line: $state.line, column: $state.column }, False)
			return { state: $state, consumed: $index, outcome: Item(fields) }
		}
		{ state: $state, consumed: $index, outcome: Done }
	}
}

advance : CSVReader.State, U8 -> CSVReader.State
advance = |state, byte| {
	if byte == '\r' {
		{ ..state, offset: state.offset + 1, line: state.line + 1, column: 1, previous_cr: True }
	}
	else if byte == '\n' {
		{ ..state, offset: state.offset + 1, line: state.line + (if (state.previous_cr) 0 else 1), column: 1, previous_cr: False }
	}
	else {
		{ ..state, offset: state.offset + 1, column: state.column + 1, previous_cr: False }
	}
}

finish_field : CSVReader.State -> CSVReader.State
finish_field = |state| { ..state, fields: state.fields.append(state.field), starts: state.starts.append(state.field_start), field: [], mode: Start }

finish_record : CSVReader.State, CSVReader.Location, Bool -> CSVReader.State
finish_record = |state, at, skip_lf| { ..state, field: [], fields: [], starts: [], last_starts: state.starts, last_end: at, mode: Start, record_started: False, record_bytes: 0, record: state.record + 1, field_start: { line: state.line, column: state.column }, skip_lf }

failure : CSVReader.State, U64, CSVReader.Problem -> Streaming.Step(CSVReader.State, List(List(U8)), CSVReader.Error)
failure = |state, consumed, problem| { state, consumed, outcome: Failed({ record: state.record, field: state.fields.len() + 1, line: state.field_start.line, column: state.field_start.column, problem }) }

expect Streaming.collect(Streaming.iter(["a,\"b".to_utf8(), "\"\"c\"\r".to_utf8(), "\nx,y".to_utf8()].iter(), CSVReader.new({}), CSVReader.step, {})) == Ok([["a".to_utf8(), "b\"c".to_utf8()], ["x".to_utf8(), "y".to_utf8()]])
