import Streaming
import Literal
import Parser
import Utf8

## Incremental Quicken Interchange Format records and bank/cash/card decoding.
## Raw events preserve section changes, field order and repeated unknown fields.
## Typed decoding requires D and T and rejects ambiguous dates by default.
QIF := { events : List(QIF.Event) }.{

	## Exact original-byte location of a record or diagnostic.
	Location : { offset : U64, line : U64, column : U64 }

	## A located syntax, encoding, limit or decoding failure.
	Problem : [IncompleteInput, RecordTooLarge(U64), LineTooLarge(U64), MissingRecordTerminator, UnsupportedSection(Str), DuplicateCoreField, MissingField(U8), InvalidDate({ value : Str, order : DateOrder }), InvalidAmount(Str), InvalidSplitAmount, MissingSplitCategory, InvalidHeader(Str), InvalidField(Field), UnrepresentableCharacter(TextEncoding), InvalidChunkSize, ContextMismatch({ section : Str, account : Str }), NulCharacter, HeaderBeforeTerminator, ContextTooLarge(U64), MissingSection, TooManyFields(U64), InvalidFieldCode(U8), InvalidEncoding({ encoding : TextEncoding, error : Streaming.EncodingError })]
	Error : { offset : U64, line : U64, column : U64, problem : Parser.Problem(Problem) }

	## Supported byte encodings. ASCII is a subset of Utf8.
	TextEncoding : Streaming.TextEncoding

	## Date order is explicit when both first components could be months.
	DateOrder : [RejectAmbiguous, MonthFirst, DayFirst, YearFirst]

	## Limits apply to original bytes before decoding.
	Props := { encoding : TextEncoding ?? Utf8, date_order : DateOrder ?? RejectAmbiguous, max_line_bytes : U64 ?? 1_048_576, max_record_bytes : U64 ?? 16_777_216, max_fields : U64 ?? 65_536, max_context_bytes : U64 ?? 65_536 }

	## A single ordered field, including repeated split/address/extension lines.
	Field : { code : U8, value : Str }

	## A terminated raw record. Section is the current !Type value or Account.
	Record : { section : Str, account : Str, fields : List(Field), at : Location }

	## Header changes and complete records in original order.
	Event : [Header(Str), Record(Record)]

	## Gregorian date, without an invented timezone or time of day.
	Date : { year : U64, month : U64, day : U64 }

	## A split retains its exact amount and optional memo.
	Split : { category : Str, memo : Str, amount : Str }

	## A decoded transaction retains its raw record and exact monetary spelling.
	Transaction : { raw : Record, date : Date, amount : Str, payee : Str, memo : Str, number : Str, category : Str, cleared : Str, splits : List(Split) }

	## Writer layout and encoding. Invalid values fail before a record is emitted.
	WriteProps := { encoding : TextEncoding ?? Utf8, line_ending : [Lf, CrLf, Cr] ?? CrLf, max_record_bytes : U64 ?? 16_777_216 }

	## Quoted/imported source with the same parser options as runtime parsing.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Validate a complete UTF-8 source for quotation or file import.
	validate : ValidationProps -> Try(QIF, [BadQuotedBytes(Str)])
	validate = |props| parse_with(props.source, props.options).map_err(|InvalidQif(error)| Literal.error({ name: props.name, format: "QIF", message: Str.inspect(error.problem), line: error.line, column: error.column }))

	## Convert a checked source quotation.
	from_quote : Str -> Try(QIF, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## Parse all raw QIF events from UTF-8 text.
	parse_str : Str -> Try(QIF, [InvalidQif(Error)])
	parse_str = |source| parse_with(source, {})

	## Text is already decoded; encoding applies only to byte entrypoints.
	parse_with : Str, Props -> Try(QIF, [InvalidQif(Error)])
	parse_with = |source, props| parse_core(
		source.to_utf8(),
		{ ..props, encoding: Utf8 },
		|part, offset| {
			# Str is already valid UTF-8. The shared scanner supplies complete
			# lines/payloads, whose ASCII delimiters establish safe boundaries.
			after = source.drop_first_bytes(offset).map_err(|_| BadEncoding({ index: 0, problem: InvalidUtf8 }))?
			after.drop_last_bytes(after.count_utf8_bytes() - part.len()).map_err(|_| BadEncoding({ index: 0, problem: InvalidUtf8 }))
		},
	)

	## Parse raw bytes with explicit encoding and resource limits.
	parse_bytes : List(U8), Props -> Try(QIF, [InvalidQif(Error)])
	parse_bytes = |bytes, props| {
		encoding = props.encoding
		parse_core(bytes, props, |part, _| Streaming.decode_text(part, encoding))
	}

	## Composable complete-document raw parser.
	parser : Parser(Utf8.Bytes, QIF, Problem)
	parser = Parser.custom(
		|bytes| match parse_bytes(bytes, {}) {
			Ok(value) => Ok({ value, rest: [] })
			Err(InvalidQif(error)) => Err(ParseError({ offset: error.offset, problem: error.problem }))
		},
	)

	## An incremental decoder retains only context and the unfinished record.
	Reader : { props : Props, offset : U64, line_number : U64, column : U64, line_start : U64, line_bytes : List(U8), skip_lf : Bool, fields : List(Field), section : Str, account : Str, record_at : Location, record_bytes : U64, done : Bool }

	## Configure raw events for Streaming.read_iter or Streaming.read_stream.
	decoder : Props -> Streaming.Decoder(Reader, Event, Error)
	decoder = |props| Streaming.decoder(initial_qif(props), qif_step)

	## Typed events retain section and account context.
	TransactionEvent : [Header(Str), Account(Record), Context(Record), Transaction(Transaction)]

	## Configure typed bank, cash, and card transactions.
	transaction_decoder : Props -> Streaming.Decoder(Reader, TransactionEvent, Error)
	transaction_decoder = |props| Streaming.decoder(initial_qif(props), typed_step)

	## Decode a complete document's bank/cash/card records without losing raw fields.
	transactions : QIF, Props -> Try(List(Transaction), [InvalidQif(Error)])
	transactions = |doc, props| {
		var $rows = []
		var $index = 0
		while $index < doc.events.len() {
			event = doc.events.get($index) ?? Header("")
			$index = $index + 1
			match event {
				Header(_) => {}
				Record(record) if record.section == "Account" or record.section == "Cat" or record.section == "Class" => {}
				Record(record) => {
					$rows = $rows.append(decode(record, props)?)
				}
			}
		}
		Ok($rows)
	}

	## Decode one completely framed bank, cash, or credit-card record.
	decode : Record, Props -> Try(Transaction, [InvalidQif(Error)])
	decode = |record, props| {
		bad = |problem| InvalidQif({ offset: record.at.offset, line: record.at.line, column: record.at.column, problem })
		if record.section != "Bank" and record.section != "Cash" and record.section != "CCard" {
			return Err(bad(UnsupportedSection(record.section)))
		}
		# Project first values in one pass, while retaining the original ordered list.
		var $date_text : Try(Str, [Missing])
		var $date_text = Err(Missing)
		var $amount_text : Try(Str, [Missing])
		var $amount_text = Err(Missing)
		var $payee : Try(Str, [Missing])
		var $payee = Err(Missing)
		var $memo : Try(Str, [Missing])
		var $memo = Err(Missing)
		var $number : Try(Str, [Missing])
		var $number = Err(Missing)
		var $category : Try(Str, [Missing])
		var $category = Err(Missing)
		var $cleared : Try(Str, [Missing])
		var $cleared = Err(Missing)
		var $duplicate = False
		var $split_fields = False
		var $index = 0
		while $index < record.fields.len() {
			f = record.fields.get($index) ?? { code: 0, value: "" }
			$index = $index + 1
			match f.code {
				'D' => {
					$duplicate = $duplicate or $date_text.is_ok()
					$date_text = Ok(f.value)
				}
				'T' => {
					$duplicate = $duplicate or $amount_text.is_ok()
					$amount_text = Ok(f.value)
				}
				'P' if $payee.is_err() => {
					$payee = Ok(f.value)
				}
				'M' if $memo.is_err() => {
					$memo = Ok(f.value)
				}
				'N' if $number.is_err() => {
					$number = Ok(f.value)
				}
				'L' if $category.is_err() => {
					$category = Ok(f.value)
				}
				'C' if $cleared.is_err() => {
					$cleared = Ok(f.value)
				}
				'S' | 'E' | '$' => {
					$split_fields = True
				}
				_ => {}
			}
		}
		if $duplicate {
			return Err(bad(DuplicateCoreField))
		}
		date_text = $date_text.map_err(|_| bad(MissingField('D')))?
		date = parse_date(date_text, props.date_order).map_err(|_| bad(InvalidDate({ value: date_text, order: props.date_order })))?
		amount = $amount_text.map_err(|_| bad(MissingField('T')))?
		if !valid_amount(amount) {
			return Err(bad(InvalidAmount(amount)))
		}
		var $splits = []
		var $split = { category: "", memo: "", amount: "" }
		var $has_split = False
		var $split_index = 0
		while $split_fields and $split_index < record.fields.len() {
			f = record.fields.get($split_index) ?? { code: 0, value: "" }
			$split_index = $split_index + 1
			if f.code == 'S' {
				if $has_split {
					if !valid_amount($split.amount) {
						return Err(bad(InvalidSplitAmount))
					}
					$splits = $splits.append($split)
				}
				$split = { category: f.value, memo: "", amount: "" }
				$has_split = True
			} else if f.code == 'E' or f.code == '$' {
				if !$has_split {
					return Err(bad(MissingSplitCategory))
				}
				$split = if (f.code == 'E') { ..$split, memo: f.value } else { ..$split, amount: f.value }
			}
		}
		if $has_split {
			if !valid_amount($split.amount) {
				return Err(bad(InvalidSplitAmount))
			}
			$splits = $splits.append($split)
		}
		Ok({ raw: record, date, amount, payee: $payee ?? "", memo: $memo ?? "", number: $number ?? "", category: $category ?? "", cleared: $cleared ?? "", splits: $splits })
	}

	## First matching field. Raw fields retain all duplicate occurrences.
	field : Record, U8 -> Try(Str, [Missing])
	field = |record, code| record.fields.find_first(|f| f.code == code).map_ok(|f| f.value).map_err(|_| Missing)

	## Convert an exact amount to Roc Dec, failing on unsupported precision/range.
	amount_to_dec : Str -> Try(Dec, [BadNumStr])
	amount_to_dec = |text| {
		if !valid_amount(text) {
			return Err(BadNumStr)
		}
		parts = text.split_on(".")
		if (parts.get(1) ?? "").count_utf8_bytes() > 18 {
			return Err(BadNumStr)
		}
		Dec.from_str(text)
	}

	## Encode a header or complete raw record atomically.
	write_event : Event, WriteProps -> Try(List(U8), [InvalidQif(Error)])
	write_event = |event, props| {
		bad = |problem| InvalidQif({ offset: 0, line: 0, column: 0, problem })
		ending = match props.line_ending {
			Lf => "\n"
			CrLf => "\r\n"
			Cr => "\r"
		}
		text = match event {
			Header(header) => {
				if !header.starts_with("!") or has_break(header) {
					return Err(bad(InvalidHeader(header)))
				}
				"${header}${ending}"
			}
			Record(record) => {
				var $lines = []
				for f in record.fields {
					if f.code < 33 or f.code > 126 or f.code == '!' or f.code == '^' or has_break(f.value) {
						return Err(bad(InvalidField(f)))
					}
					$lines = $lines.append("${Str.from_utf8([f.code]) ?? ""}${f.value}${ending}")
				}
				"${Str.join_with($lines, "")}^${ending}"
			}
		}
		bytes = encode_text(text, props.encoding).map_err(|_| bad(UnrepresentableCharacter(props.encoding)))?
		if bytes.len() > props.max_record_bytes {
			return Err(bad(RecordTooLarge(props.max_record_bytes)))
		}
		Ok(bytes)
	}

	## Validate each complete event atomically before emitting bytes.
	encoder : WriteProps -> Streaming.Encoder({ section : Str, account : Str }, Event, [InvalidQif(Error)])
	encoder = |props| Streaming.encoder({ section: "", account: "" }, |state, event| encode_event(state, event, props), |_| Ok([]))

	## Write a complete document; streaming writers use the same per-record encoder.
	to_bytes : QIF, WriteProps -> Try(List(U8), [InvalidQif(Error)])
	to_bytes = |doc, props| {
		var $bytes = []
		for result in Streaming.write_iter(doc.events.iter(), encoder(props)) {
			match result {
				Ok(bytes) => {
					$bytes = $bytes.concat(bytes)
				}
				Err(EncodeErr(error)) => return Err(error)
				Err(InvalidChunkSize) => return Err(InvalidQif({ offset: 0, line: 0, column: 0, problem: InvalidChunkSize }))
			}
		}
		Ok($bytes)
	}

	## Render canonical UTF-8 text.
	to_str : QIF -> Try(Str, [InvalidQif(Error)])
	to_str = |doc| to_bytes(doc, {}).map_ok(Str.from_utf8_lossy)
}

initial_qif : QIF.Props -> QIF.Reader
initial_qif = |props| { props, offset: 0, line_number: 1, column: 1, line_start: 0, line_bytes: [], skip_lf: False, fields: [], section: "", account: "", record_at: { offset: 0, line: 1, column: 1 }, record_bytes: 0, done: False }

qif_step : QIF.Reader, List(U8), Streaming.InputEnd -> Streaming.Step(QIF.Reader, QIF.Event, QIF.Error)
qif_step = |initial, bytes, end| {
	encoding = initial.props.encoding
	step_with_decode(initial, bytes, end, |part, _| Streaming.decode_text(part, encoding))
}

encode_event : { section : Str, account : Str }, QIF.Event, QIF.WriteProps -> Try({ state : { section : Str, account : Str }, bytes : List(U8) }, [InvalidQif(QIF.Error)])
encode_event = |state, event, props| {
	bytes = QIF.write_event(event, props)?
	next = match event {
		Header(header) => {
			section = if (header == "!Account") "Account" else if (header.starts_with("!Type:")) Str.from_utf8_lossy(header.to_utf8().drop_first(6)) else state.section
			{ ..state, section }
		}
		Record(record) => {
			if state.section == "" or record.section != state.section or record.account != state.account {
				return Err(InvalidQif({ offset: record.at.offset, line: record.at.line, column: record.at.column, problem: ContextMismatch({ section: record.section, account: record.account }) }))
			}
			if record.section == "Account" {
				account = QIF.field(record, 'N') ?? ""
				if account == "" {
					return Err(InvalidQif({ offset: record.at.offset, line: record.at.line, column: record.at.column, problem: MissingField('N') }))
				}
				{ ..state, account }
			} else state
		}
	}
	Ok({ state: next, bytes })
}

typed_step : QIF.Reader, List(U8), Streaming.InputEnd -> Streaming.Step(QIF.Reader, QIF.TransactionEvent, QIF.Error)
typed_step = |state, bytes, end| {
	raw = qif_step(state, bytes, end)
	outcome = match raw.outcome {
		Item(Header(header)) => Item(Header(header))
		Item(Record(record)) if record.section == "Account" => Item(Account(record))
		Item(Record(record)) if record.section == "Cat" or record.section == "Class" => Item(Context(record))
		Item(Record(record)) => match QIF.decode(record, state.props) {
			Ok(value) => Item(Transaction(value))
			Err(InvalidQif(error)) => RecordError(error)
		}
		NeedInput => NeedInput
		Done => Done
		Failed(error) => Failed(error)
		RecordError(error) => RecordError(error)
	}
	{ state: raw.state, consumed: raw.consumed, outcome }
}

# One scanner/collector serves decoded text and byte input. The text callback
# belongs only to a complete parse call; incremental Decoder never stores it.
parse_core : List(U8), QIF.Props, (List(U8), U64 -> Try(Str, [BadEncoding(Streaming.EncodingError)])) -> Try(QIF, [InvalidQif(QIF.Error)])
parse_core = |bytes, props, decode| {
	var $decoder = initial_qif(props)
	var $pos = 0
	var $events = []
	while True {
		result = step_with_decode($decoder, bytes.drop_first($pos), Final, decode)
		$decoder = result.state
		$pos = $pos + result.consumed
		match result.outcome {
			Item(event) => {
				$events = $events.append(event)
			}
			Done => return Ok(QIF.{ events: $events })
			Failed(error) | RecordError(error) => return Err(InvalidQif(error))
			NeedInput => return Err(InvalidQif(located_problem($decoder, IncompleteInput)))
		}
	}
}

# Only field/context data crosses the line-handler boundary. Scalar scan
# coordinates stay local until a public decoder step returns.
LineState : { fields : List(QIF.Field), section : Str, account : Str, record_at : QIF.Location }

decoder_at : QIF.Props, LineState, U64, U64, U64, U64, List(U8), Bool, U64 -> QIF.Reader
decoder_at = |props, state, offset, line_number, column, line_start, line_bytes, skip_lf, record_bytes| {
	{ props, offset, line_number, column, line_start, line_bytes, skip_lf, record_bytes, fields: state.fields, section: state.section, account: state.account, record_at: state.record_at, done: False }
}

step_with_decode : QIF.Reader, List(U8), Streaming.InputEnd, (List(U8), U64 -> Try(Str, [BadEncoding(Streaming.EncodingError)])) -> Streaming.Step(QIF.Reader, QIF.Event, QIF.Error)
step_with_decode = |initial, bytes, end, decode| {
	if initial.done {
		return { state: initial, consumed: 0, outcome: Done }
	}
	props = initial.props
	var $line_state = { fields: initial.fields, section: initial.section, account: initial.account, record_at: initial.record_at }
	var $offset = initial.offset
	var $line_number = initial.line_number
	var $column = initial.column
	var $line_start = initial.line_start
	var $line_bytes = initial.line_bytes
	var $skip_lf = initial.skip_lf
	var $record_bytes = initial.record_bytes
	var $pos = 0
	while $pos < bytes.len() {
		if $skip_lf and bytes.get($pos) == Ok('\n') {
			additional = if ($record_bytes == 0) 0 else 1
			$pos = $pos + 1
			$offset = $offset + 1
			$record_bytes = $record_bytes + additional
			$skip_lf = False
			$line_start = $offset
			if $record_bytes > props.max_record_bytes {
				state = decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)
				return { state: terminal(state), consumed: $pos, outcome: Failed(located_problem(state, RecordTooLarge(props.max_record_bytes))) }
			}
		} else {
			record_room = props.max_record_bytes - $record_bytes
			line_room = props.max_line_bytes - $line_bytes.len()
			room = if (record_room < line_room) record_room else line_room
			remaining = bytes.len() - $pos
			# Inspect at most the budget plus its first violating byte.
			scan_len = if (room < remaining) room + 1 else remaining
			stop = Utf8.find_line_end(bytes.take_first($pos + scan_len), $pos)
			len = stop - $pos
			if len > record_room or len > line_room {
				record_first = record_room <= line_room
				accepted = if (record_first) record_room else line_room
				consumed = accepted + 1
				$offset = $offset + consumed
				$record_bytes = $record_bytes + consumed
				$column = $column + accepted + (if (record_first) 0 else 1)
				state = decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)
				return { state: terminal(state), consumed: $pos + consumed, outcome: Failed(located_problem(state, if (record_first) RecordTooLarge(props.max_record_bytes) else LineTooLarge(props.max_line_bytes))) }
			}
			span = bytes.sublist({ start: $pos, len })
			accumulated = if $line_bytes.is_empty() {
				if end == More {
					# Completed text can outlive this call too. Keep every streaming
					# span independent of the caller's potentially large allocation.
					var $pending = List.with_capacity(len)
					var $copied = 0
					while $copied < len {
						$pending = $pending.append(span.get($copied) ?? 0)
						$copied = $copied + 1
					}
					$pending
				} else span
			} else $line_bytes.concat(span)
			$offset = $offset + len
			$record_bytes = $record_bytes + len
			$column = $column + len
			$skip_lf = False
			$pos = stop
			if $pos == bytes.len() {
				$line_bytes = accumulated
				break
			}
			byte = bytes.get($pos) ?? 0
			$pos = $pos + 1
			additional = if (accumulated == ['^']) 0 else 1
			$offset = $offset + 1
			$record_bytes = $record_bytes + additional
			if $record_bytes > props.max_record_bytes {
				state = decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)
				return { state: terminal(state), consumed: $pos, outcome: Failed(located_problem(state, RecordTooLarge(props.max_record_bytes))) }
			}
			$line_number = $line_number + 1
			$column = 1
			$skip_lf = byte == '\r'
			$line_bytes = []
			origin = { offset: $line_start, line: $line_number - 1, column: 1 }
			processed = process_line($line_state, accumulated, origin, props, decode)
			$line_state = processed.state
			$line_start = $offset
			match processed.result {
				Emitted(event) => {
					$record_bytes = 0
					return { state: decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes), consumed: $pos, outcome: Item(event) }
				}
				Error(error) => return { state: terminal(decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)), consumed: $pos, outcome: Failed(error) }
				Continue => {}
			}
		}
	}
	if end == More {
		return { state: decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes), consumed: $pos, outcome: NeedInput }
	}
	if !$line_bytes.is_empty() {
		line = $line_bytes
		$line_bytes = []
		origin = { offset: $line_start, line: if ($column == 1) $line_number - 1 else $line_number, column: 1 }
		processed = process_line($line_state, line, origin, props, decode)
		$line_state = processed.state
		match processed.result {
			Emitted(event) => {
				$record_bytes = 0
				return { state: decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes), consumed: $pos, outcome: Item(event) }
			}
			Error(error) => return { state: terminal(decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)), consumed: $pos, outcome: Failed(error) }
			Continue => {}
		}
	}
	if !$line_state.fields.is_empty() {
		state = decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)
		return { state: terminal(state), consumed: $pos, outcome: Failed(located_problem(state, MissingRecordTerminator)) }
	}
	{ state: terminal(decoder_at(props, $line_state, $offset, $line_number, $column, $line_start, $line_bytes, $skip_lf, $record_bytes)), consumed: $pos, outcome: Done }
}

terminal : QIF.Reader -> QIF.Reader
terminal = |s| { ..s, line_bytes: [], fields: [], section: "", account: "", done: True }

located_problem : QIF.Reader, Parser.Problem(QIF.Problem) -> QIF.Error
located_problem = |s, problem| { offset: s.offset, line: s.line_number, column: s.column, problem }

process_line : LineState, List(U8), QIF.Location, QIF.Props, (List(U8), U64 -> Try(Str, [BadEncoding(Streaming.EncodingError)])) -> { state : LineState, result : [Continue, Emitted(QIF.Event), Error(QIF.Error)] }
process_line = |s, bytes, origin, props, decode| {
	bom = origin.offset == 0 and props.encoding == Utf8 and bytes.take_first(3) == [0xef, 0xbb, 0xbf]
	content = if (bom) bytes.drop_first(3) else bytes
	prefix = if (bom) 3 else 0
	at = { offset: origin.offset + prefix, line: origin.line, column: origin.column + prefix }
	bad = |problem| { offset: at.offset, line: at.line, column: at.column, problem }
	code = content.first() ?? 0
	if code >= 33 and code <= 126 and code != '!' and code != '^' {
		# An ASCII field indicator needs no conversion. Decode the payload once;
		# malformed payloads still precede framing/context errors as before.
		value = match decode(content.drop_first(1), at.offset + 1) {
			Ok(decoded) => decoded
			Err(BadEncoding(error)) => {
				# Keep the nested diagnostic relative to the full decoded line,
				# even though only the payload needed UTF-8 validation.
				line_error = { ..error, index: error.index + 1 }
				return { state: s, result: Error({ offset: at.offset + line_error.index, line: at.line, column: at.column + line_error.index, problem: InvalidEncoding({ encoding: props.encoding, error: line_error }) }) }
			}
		}
		if contains_nul(content) {
			return { state: s, result: Error(bad(NulCharacter)) }
		}
		if s.section == "" {
			return { state: s, result: Error(bad(MissingSection)) }
		}
		if s.fields.len() >= props.max_fields {
			return { state: s, result: Error(bad(TooManyFields(props.max_fields))) }
		}
		record_at = if (s.fields.is_empty()) at else s.record_at
		# Most records have several fields. Reserve only after the first field
		# validates, and never request more entries than the caller permits.
		pending_fields = if (s.fields.is_empty()) List.with_capacity(U64.min(8, props.max_fields)) else s.fields
		fields = pending_fields.append({ code, value })
		return { state: { ..s, fields, record_at }, result: Continue }
	}
	text = match decode(content, at.offset) {
		Ok(value) => value
		Err(BadEncoding(error)) => return { state: s, result: Error({ offset: at.offset + error.index, line: at.line, column: at.column + error.index, problem: InvalidEncoding({ encoding: props.encoding, error }) }) }
	}
	if contains_nul(bytes) {
		return { state: s, result: Error(bad(NulCharacter)) }
	}
	if content.is_empty() {
		return { state: s, result: Continue }
	}
	if text.starts_with("!") {
		if !s.fields.is_empty() {
			return { state: s, result: Error(bad(HeaderBeforeTerminator)) }
		}
		if bytes.len() > props.max_context_bytes {
			return { state: s, result: Error(bad(ContextTooLarge(props.max_context_bytes))) }
		}
		section = if (text == "!Account") "Account" else if (text.starts_with("!Type:")) Str.from_utf8_lossy(text.to_utf8().drop_first(6)) else s.section
		return { state: { ..s, section }, result: Emitted(Header(text)) }
	}
	if text == "^" {
		if s.section == "" {
			return { state: s, result: Error(bad(MissingSection)) }
		}
		record = { section: s.section, account: s.account, fields: s.fields, at: if (s.fields.is_empty()) at else s.record_at }
		account = if (s.section == "Account") QIF.field(record, 'N') ?? "" else s.account
		if s.section == "Account" and account == "" {
			return { state: s, result: Error(bad(MissingField('N'))) }
		}
		if account.count_utf8_bytes() > props.max_context_bytes {
			return { state: s, result: Error(bad(ContextTooLarge(props.max_context_bytes))) }
		}
		return { state: { ..s, fields: [], account }, result: Emitted(Record(record)) }
	}
	if s.section == "" {
		return { state: s, result: Error(bad(MissingSection)) }
	}
	if s.fields.len() >= props.max_fields {
		return { state: s, result: Error(bad(TooManyFields(props.max_fields))) }
	}
	{ state: s, result: Error(bad(InvalidFieldCode(code))) }
}

has_break : Str -> Bool
has_break = |text| text.to_utf8().any(|b| b == '\r' or b == '\n' or b == 0)

# A fixed byte test avoids creating an iterator closure for every input byte.
contains_nul : List(U8) -> Bool
contains_nul = |bytes| {
	zero = U8x16.splat(0)
	var $index = 0
	while bytes.len() - $index >= 16 {
		block = U8x16.load(bytes, $index) ?? U8x16.splat(1)
		if U8x16.to_bitmask(U8x16.eq_lanes(block, zero)) != 0 {
			return True
		}
		$index = $index + 16
	}
	while $index < bytes.len() {
		if bytes.get($index) == Ok(0) {
			return True
		}
		$index = $index + 1
	}
	False
}

valid_amount : Str -> Bool
valid_amount = |text| {
	bytes = text.to_utf8()
	first = bytes.first() ?? 0
	var $index = if (first == '+' or first == '-') 1 else 0
	var $digits = False
	var $dot = False
	while $index < bytes.len() {
		b = bytes.get($index) ?? 0
		$index = $index + 1
		if b >= '0' and b <= '9' {
			$digits = True
		} else if b == '.' and !$dot {
			$dot = True
		} else {
			return False
		}
	}
	$digits
}

parse_date : Str, QIF.DateOrder -> Try(QIF.Date, [InvalidDate])
parse_date = |text, order| {
	{ a, b, c } = date_components(text)?
	date = match order {
		YearFirst => { year: expand_year(a), month: b, day: c }
		MonthFirst => { year: expand_year(c), month: a, day: b }
		DayFirst => { year: expand_year(c), month: b, day: a }
		RejectAmbiguous => if (a > 31) { year: a, month: b, day: c } else if (a > 12) { year: expand_year(c), month: b, day: a } else if (b > 12) { year: expand_year(c), month: a, day: b } else {
			return Err(InvalidDate)
		}
	}
	if date.year == 0 or date.month == 0 or date.month > 12 or date.day == 0 {
		return Err(InvalidDate)
	}
	leap = date.year % 4 == 0 and (date.year % 100 != 0 or date.year % 400 == 0)
	max_day = if date.month == 2 {
		if (leap) 29 else 28
	} else if (date.month == 4 or date.month == 6 or date.month == 9 or date.month == 11) 30 else 31
	if (date.day > max_day) Err(InvalidDate) else Ok(date)
}

# Decimal components and every supported separator need no temporary strings.
# The fallback retains U64.from_str and Unicode trim semantics for other input.
date_components : Str -> Try({ a : U64, b : U64, c : U64 }, [InvalidDate])
date_components = |text| {
	bytes = text.to_utf8()
	var $index = 0
	var $part = 0
	var $value = 0.U64
	var $digits = False
	var $a = 0.U64
	var $b = 0.U64
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		$index = $index + 1
		if byte >= '0' and byte <= '9' {
			digit = (byte - '0').to_u64()
			if $value > (U64.highest - digit) // 10 {
				return date_components_slow(text)
			}
			$value = $value * 10 + digit
			$digits = True
		} else if (byte == '/' or byte == '-' or byte == '\'') and $digits and $part < 2 {
			if $part == 0 {
				$a = $value
			} else {
				$b = $value
			}
			$part = $part + 1
			$value = 0
			$digits = False
		} else {
			return date_components_slow(text)
		}
	}
	if ($part == 2 and $digits) Ok({ a: $a, b: $b, c: $value }) else date_components_slow(text)
}

date_components_slow : Str -> Try({ a : U64, b : U64, c : U64 }, [InvalidDate])
date_components_slow = |text| {
	parts = Str.replace_each(Str.replace_each(text, "'", "/"), "-", "/").split_on("/")
	if parts.len() != 3 {
		return Err(InvalidDate)
	}
	a = U64.from_str((parts.get(0) ?? "").trim()).map_err(|_| InvalidDate)?
	b = U64.from_str((parts.get(1) ?? "").trim()).map_err(|_| InvalidDate)?
	c = U64.from_str((parts.get(2) ?? "").trim()).map_err(|_| InvalidDate)?
	Ok({ a, b, c })
}

expand_year : U64 -> U64
expand_year = |year| if (year >= 100) year else if (year >= 70) year + 1900 else year + 2000

cp1252 : List(U32)
cp1252 = [0x20ac, 0, 0x201a, 0x0192, 0x201e, 0x2026, 0x2020, 0x2021, 0x02c6, 0x2030, 0x0160, 0x2039, 0x0152, 0, 0x017d, 0, 0, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014, 0x02dc, 0x2122, 0x0161, 0x203a, 0x0153, 0, 0x017e, 0x0178]

decode_text : List(U8), QIF.TextEncoding -> Try(Str, [InvalidEncoding])
decode_text = |bytes, encoding| Streaming.decode_text(bytes, encoding).map_err(|_| InvalidEncoding)

encode_scalar : U32 -> List(U8)
encode_scalar = |cp| {
	if (cp < 128) [cp.to_u8_wrap()] else if (cp < 2048) [((cp / 64) + 192).to_u8_wrap(), ((cp % 64) + 128).to_u8_wrap()] else [((cp / 4096) + 224).to_u8_wrap(), (((cp / 64) % 64) + 128).to_u8_wrap(), ((cp % 64) + 128).to_u8_wrap()]
}

encode_text : Str, QIF.TextEncoding -> Try(List(U8), [InvalidEncoding])
encode_text = |text, encoding| {
	if encoding == Utf8 {
		return Ok(text.to_utf8())
	}
	bytes = text.to_utf8()
	var $pos = 0
	var $out = []
	while $pos < bytes.len() {
		first = bytes.get($pos) ?? 0
		if first < 128 {
			$out = $out.append(first)
			$pos = $pos + 1
		} else {
			var $found = False
			var $candidate = 128.U16
			while $candidate < 256 {
				byte = $candidate.to_u8_wrap()
				cp = if (byte < 160) cp1252.get((byte - 128).to_u64()) ?? 0 else byte.to_u32()
				encoded = encode_scalar(cp)
				if cp != 0 and bytes.sublist({ start: $pos, len: encoded.len() }) == encoded {
					$out = $out.append(byte)
					$pos = $pos + encoded.len()
					$found = True
					break
				}
				$candidate = $candidate + 1
			}
			if !$found {
				return Err(InvalidEncoding)
			}
		}
	}
	Ok($out)
}

expect QIF.parse_str("!Type:Bank\nD27/08/2024\nT-12.3400\nPCafé\n^\n").is_ok()
expect QIF.parse_str("!Type:Bank\nT1\n").is_err()
expect QIF.parse_str("T1\n^\n").is_err()
expect QIF.parse_str("!Type:Bank\nT1\n!Type:Cash\n").is_err()
expect parse_date("29/02/2024", RejectAmbiguous) == Ok({ year: 2024, month: 2, day: 29 })
expect parse_date("29/02/2023", DayFirst).is_err()
expect parse_date("01/02/2024", RejectAmbiguous).is_err()
expect valid_amount("-12345678901234567890.00000000000000000001")
expect !valid_amount("1e3")
expect encode_text("€ café", Windows1252) == Ok([128, 32, 99, 97, 102, 233])
expect decode_text([128, 32, 233], Windows1252) == Ok("€ é")
expect encode_text("😀", Windows1252).is_err()

sample = "!Account\r\nNCurrent\r\nTBank\r\n^\r\n!Type:Bank\r\nD27/08/2024\r\nT-12.3400\r\nPCafé\r\nM😀\r\nSFood\r\nEcoffee\r\n$-12.34\r\nXunknown\r\n^\r\n"

semantic_events : List(QIF.Event) -> List([Header(Str), Record({ section : Str, account : Str, fields : List(QIF.Field) })])
semantic_events = |events| events.map(
	|event| match event {
		Header(header) => Header(header)
		Record(record) => Record({ section: record.section, account: record.account, fields: record.fields })
	},
)

expect {
	doc = QIF.parse_str(sample)?
	written = doc.to_str()?
	again = QIF.parse_str(written)?
	semantic_events(doc.events) == semantic_events(again.events)
}

expect {
	doc = QIF.parse_str(sample)?
	rows = doc.transactions({})?
	row = rows.first()?
	row.raw.account == "Current" and row.amount == "-12.3400" and row.splits == [{ category: "Food", memo: "coffee", amount: "-12.34" }]
}

expect {
	bytes = sample.to_utf8()
	var $valid = True
	for split in (0).to(bytes.len() + 1) {
		chunks = [bytes.take_first(split), [], bytes.drop_first(split)]
		found = Streaming.collect(Streaming.read_iter(chunks.iter(), QIF.decoder({})))?
		expected = QIF.parse_str(sample)?
		$valid = $valid and semantic_events(found) == semantic_events(expected.events)
	}
	$valid
}

expect {
	bad = "!Type:Bank\nD27/08/2024\nTbad\n^\nD27/08/2024\nT1\n^\n".to_utf8()
	all = List.from_iter(Streaming.read_iter([bad].iter(), QIF.transaction_decoder({}).with_errors({ error_policy: ReportInvalidRecords })))
	all.len() == 4 and all.last() == Ok(Ok(Finished({ emitted_items: 2, rejected_records: 1 })))
}

expect {
	bad = "!Type:Bank\nD27/08/2024\nTbad\n^\n".to_utf8()
	all = List.from_iter(Streaming.read_iter([bad].iter(), QIF.transaction_decoder({})))
	match all.last() {
		Ok(Err(ParseErr(_))) => all.len() == 2
		_ => False
	}
}

expect QIF.parse_bytes("!Type:Bank\n".to_utf8().concat([80, 233, 10, 94]), { encoding: Windows1252 }).is_ok()
expect QIF.parse_bytes("!Type:Bank\n".to_utf8().concat([80, 129, 10, 94]), { encoding: Windows1252 }).is_err()
expect QIF.parse_str("!Type:Bank\nP\u(0)\n^\n").is_err()
expect QIF.parse_with("!Type:Bank\nPabc\n^\n", { max_line_bytes: 3 }).is_err()

expect {
	doc = QIF.parse_str(sample)?
	var $bytes = []
	var $valid = True
	for result in Streaming.write_iter(doc.events.iter(), QIF.encoder({}).with_chunk_bytes(1)) {
		chunk = result?
		$valid = $valid and chunk.len() <= 1
		$bytes = $bytes.concat(chunk)
	}
	again = QIF.parse_bytes($bytes, {})?
	$valid and semantic_events(again.events) == semantic_events(doc.events)
}

expect {
	events = [Header("!Type:Bank")]
	List.from_iter(Streaming.write_iter(events.iter(), QIF.encoder({}).with_chunk_bytes(0))) == [Err(InvalidChunkSize)]
}

expect {
	bad = "!Type:Bank\nD27/08/2024\nTbad\n^\n".to_utf8()
	all = List.from_iter(Streaming.read_iter([bad].iter(), QIF.transaction_decoder({}).with_errors({ error_policy: ReportInvalidRecords, max_errors: 0 })))
	match all.last() {
		Ok(Err(ErrorLimitExceeded({ max: 0, .. }))) => True
		_ => False
	}
}

expect QIF.parse_str("!Account\nTBank\n^\n").is_err()
expect QIF.parse_str("!Type:Bank\n^oops\n^\n").is_err()
expect {
	bytes = "!Type:Bank\nP".to_utf8().concat([0x81, '\n', '^', '\n'])
	match QIF.parse_bytes(bytes, { encoding: Windows1252 }) {
		Err(InvalidQif(error)) => error.offset == 12 and error.line == 2 and error.column == 2
		_ => False
	}
}

expect QIF.amount_to_dec("0.0000000000000000001").is_err()
expect {
	bytes = "!Type:Bank\nD27/08/2024\nTbad\n^\nD27/08/2024\nT1\n^\n".to_utf8()
	report = Streaming.collect_report(Streaming.read_iter([bytes].iter(), QIF.transaction_decoder({}).with_errors({ error_policy: ReportInvalidRecords })))?
	report.errors.len() == 1 and report.report == { emitted_items: 2, rejected_records: 1 }
		and match report.values {
			[Header("!Type:Bank"), Transaction(row)] => row.amount == "1"
			_ => False
		}
}
expect {
	doc = QIF.parse_str("!Type:Bank\nD27/08/2024\nTbad\n^\n")?
	match doc.transactions({}) {
		Err(InvalidQif({ problem: InvalidAmount("bad"), .. })) => True
		_ => False
	}
}

expect {
	bytes = [0xef, 0xbb, 0xbf].concat("!Type:Bank\r\nD27/08/2024\r\nT1\r\n^\r\n".to_utf8())
	found = Streaming.collect(Streaming.read_iter(bytes.map(|byte| [byte]).iter(), QIF.decoder({})))?
	expected = QIF.parse_str("!Type:Bank\r\nD27/08/2024\r\nT1\r\n^\r\n")?
	semantic_events(found) == semantic_events(expected.events)
}
expect QIF.parse_bytes([0xef, 0xbb, 0xbf], {}).is_ok()

# Span scanning keeps the original diagnostics at both kinds of byte limit,
# including ties, CRLF boundaries, empty lines and a final unterminated line.
expect {
	var $same = True
	limits : List(QIF.Props)
	limits = [{ max_line_bytes: 10 }, { max_record_bytes: 11 }, { max_line_bytes: 11, max_record_bytes: 11 }, { max_line_bytes: 12, max_record_bytes: 12 }]
	for text in ["!Type:Bank\nP1234567890\n^\n", "!Type:Bank\r\nP1234567890\r\n^\r\n", "!Type:Bank\rP1234567890\r^", "!Type:Bank\n\nP1234567890"] {
		bytes = text.to_utf8()
		for props in limits {
			expected = List.from_iter(Streaming.read_iter([bytes].iter(), QIF.decoder(props)))
			for split in (0.U64).to(bytes.len() + 1) {
				found = List.from_iter(Streaming.read_iter([bytes.take_first(split), [], bytes.drop_first(split)].iter(), QIF.decoder(props)))
				$same = $same and found == expected
			}
		}
	}
	$same
}

expect {
	options : List(QIF.Props)
	options = [{ date_order: RejectAmbiguous }, { date_order: DayFirst }, { date_order: MonthFirst }, { date_order: YearFirst }]
	dates = ["29/02/2024", "29/02/2023", "01/02/2024", "2024-02-29", "8'27'24", " 27 / 08 / 2024 ", "1/1/18446744073709551616"]
	var $same = True
	for date in dates {
		text = "!Type:Bank\r\nD${date}\r\nT-12.340000000000000000001\r\nP日本語 Café € 😀\r\n^\r\n"
		str_doc = QIF.parse_str(text)?
		byte_doc = QIF.parse_bytes(text.to_utf8(), {})?
		for props in options {
			$same = $same and str_doc.transactions(props) == byte_doc.transactions(props)
		}
	}
	$same
}
expect match QIF.parse_with("!Type:Bank\nP1234567890\n^\n", { max_line_bytes: 10 }) {
	Err(InvalidQif(error)) => error == { offset: 22, line: 2, column: 12, problem: LineTooLarge(10) }
	_ => False
}
expect match QIF.parse_with("!Type:Bank\nP1234567890\n^\n", { max_record_bytes: 11 }) {
	Err(InvalidQif(error)) => error == { offset: 23, line: 2, column: 12, problem: RecordTooLarge(11) }
	_ => False
}

expect {
	doc = QIF.parse_str("!Type:Bank\nD27/08/2024\nT-12.3400000000000000000001\nPfirst\nPsecond\nM\nMsecond\nNfirst\nNsecond\nLfirst\nLsecond\nCfirst\nCsecond\nXone\nXtwo\n^\n")?
	row = doc.transactions({})?.first()?
	row.payee == "first" and row.memo == "" and row.number == "first" and row.category == "first" and row.cleared == "first" and row.amount == "-12.3400000000000000000001" and row.raw.fields.len() == 14
}
expect {
	doc = QIF.parse_str("!Type:Bank\nDinvalid\nT1\nD27/08/2024\n^\n")?
	match doc.transactions({}) {
		Err(InvalidQif(error)) => error.problem == DuplicateCoreField
		_ => False
	}
}

expect {
	texts = ["27/08/2024", "2024-02-29", "8'27'24", "27-08'2024", "00027/00008/0000002024", "18446744073709551615/1/1", "18446744073709551616/1/1", "1/1/18446744073709551615", "1/1/18446744073709551616", " 27 / 08 / 2024 ", "\u(a0)27/08/2024\u(2003)", "+27/+8/+2024", "0x1b/0b1000/2_024", "27/8/2.024e3", "27/8/2e3", "", "/1/2", "1//2", "1/2/", "1/2/3/4", "1/2/3x"]
	texts.all(|text| date_components(text) == date_components_slow(text))
}
expect {
	cases = [{ text: "", valid: False }, { text: "+", valid: False }, { text: "-.", valid: False }, { text: ".", valid: False }, { text: "+.1", valid: True }, { text: "-1.", valid: True }, { text: "1..0", valid: False }, { text: "1e2", valid: False }, { text: " 1", valid: False }, { text: "1\u(a0)", valid: False }, { text: "-000000000000000000000000.0000000000000000000001", valid: True }]
	cases.all(|case| valid_amount(case.text) == case.valid)
}
expect {
	doc = QIF.parse_str("!Type:Bank\nDinvalid\nT1\nEorphan\n^\n")?
	match doc.transactions({}) {
		Err(InvalidQif(error)) => error.problem == InvalidDate({ value: "invalid", order: RejectAmbiguous })
		_ => False
	}
}
expect {
	doc = QIF.parse_str("!Type:Bank\nD27/08/2024\nTinvalid\nEorphan\n^\n")?
	match doc.transactions({}) {
		Err(InvalidQif(error)) => error.problem == InvalidAmount("invalid")
		_ => False
	}
}
expect {
	doc = QIF.parse_str("!Type:Bank\nD27/08/2024\nT1\nEorphan\n^\n")?
	match doc.transactions({}) {
		Err(InvalidQif(error)) => error.problem == MissingSplitCategory
		_ => False
	}
}
expect {
	bytes = ['P', 0, 0xff, '\n']
	match QIF.parse_bytes(bytes, {}) {
		Err(InvalidQif(error)) => error == { offset: 2, line: 1, column: 3, problem: InvalidEncoding({ encoding: Utf8, error: { index: 2, problem: InvalidUtf8 } }) }
		_ => False
	}
}
expect {
	bytes = "!Type:Bank\nP".to_utf8().concat([0xff, '\n'])
	match QIF.parse_bytes(bytes, { max_fields: 0 }) {
		Err(InvalidQif(error)) => error == { offset: 12, line: 2, column: 2, problem: InvalidEncoding({ encoding: Utf8, error: { index: 1, problem: InvalidUtf8 } }) }
		_ => False
	}
}

# A large preceding chunk must not become part of retained line/field/context
# storage. All retained text exceeds short-string storage in these examples.
expect {
	section = "LongSectionNameBeyondShortStringStorage"
	value = "A field value beyond short string storage"
	large = List.repeat('\n', 8192).concat("!Type:${section}\nP${value}\n".to_utf8())
	header = QIF.decoder({ max_line_bytes: 128, max_context_bytes: 128 }).step(large, More)
	pending = header.state.step(large.drop_first(header.consumed), More)
	finished = pending.state.step("^\n".to_utf8(), Final)
	match (header.outcome, pending.outcome, finished.outcome) {
		(Item(Header(text)), NeedInput, Item(Record(record))) => text == "!Type:${section}" and pending.state.state().line_bytes.is_empty() and pending.state.state().fields == [{ code: 'P', value }] and record.section == section and record.fields == [{ code: 'P', value }]
		_ => False
	}
}
expect {
	name = "An account name beyond short string storage"
	large = List.repeat('\n', 8192).concat("!Account\nN${name}\n".to_utf8())
	header = QIF.decoder({ max_line_bytes: 128, max_context_bytes: 128 }).step(large, More)
	pending = header.state.step(large.drop_first(header.consumed), More)
	account = pending.state.step("^\n".to_utf8(), More)
	bank = account.state.step("!Type:Bank\n".to_utf8(), More)
	field = bank.state.step("Ppaid\n".to_utf8(), More)
	finished = field.state.step("^\n".to_utf8(), Final)
	match (pending.outcome, account.outcome, bank.outcome, field.outcome, finished.outcome) {
		(NeedInput, Item(Record(_)), Item(Header(_)), NeedInput, Item(Record(record))) => account.state.state().account == name and field.state.state().account == name and record.account == name and record.fields == [{ code: 'P', value: "paid" }]
		_ => False
	}
}
expect {
	var $same = True
	for prefix in (0.U64).to(65) {
		for suffix in (0.U64).to(33) {
			bytes = List.repeat(1.U8, prefix).append(0).concat(List.repeat(255, suffix))
			$same = $same and contains_nul(bytes)
		}
		$same = $same and !contains_nul(List.repeat(255.U8, prefix))
	}
	$same
}

# Complete Str input and validated UTF-8 bytes share framing, locations, limits
# and diagnostics, including BOM, Unicode and non-UTF-8 byte-option overrides.
expect {
	options : List(QIF.Props)
	options = [{}, { encoding: Windows1252 }, { max_line_bytes: 10 }, { max_record_bytes: 11 }, { max_context_bytes: 12 }, { max_fields: 1 }]
	texts = ["", "\n\r\n\r", "\u(feff)", sample, "\u(feff)!Type:Bank\r\nP日本語 Café € 😀\r\nMlong memo beyond short string storage\nXone\nXtwo\n^", "!Type:Bank\nP\u(0)\n^\n", "!Type:Bank\nPvalid\n!Type:Cash\n", "!Type:Bank\rPunterminated", "!Type:Bank\néinvalid indicator\n^\n", "!Type:Bank\n^oops\n^\n", "!Account\rNAn account beyond short string storage\r^\r!Type:Bank\rPpaid\r^\r"]
	var $same = True
	for text in texts {
		for props in options {
			match (QIF.parse_with(text, props), QIF.parse_bytes(text.to_utf8(), { ..props, encoding: Utf8 })) {
				(Ok(a), Ok(b)) => {
					$same = $same and a.events == b.events
				}
				(Err(InvalidQif(a)), Err(InvalidQif(b))) => {
					$same = $same and a == b
				}
				_ => {
					$same = False
				}
			}
		}
	}
	$same
}

expect {
	props : List(QIF.Props)
	props = [{}, { max_line_bytes: 0 }, { max_line_bytes: 10 }, { max_record_bytes: 0 }, { max_record_bytes: 11 }, { max_record_bytes: 12 }, { max_fields: 1 }]
	texts = ["!Type:Bank\r\nPfirst\r\n^\r\n", "!Type:Bank\r\n\r\nPfirst\n^\r!Type:Cash\n^", "!Account\rNCurrent\n^\r\n!Type:Bank\r\nPfirst\n^\r\n", "!Type:Bank\nPunterminated", "!Type:Bank\r\nP1234567890\r\n^\r\n"]
	var $same = True
	for text in texts {
		bytes = text.to_utf8()
		for options in props {
			bulk = List.from_iter(Streaming.read_iter([bytes].iter(), QIF.decoder(options)))
			bytewise = List.from_iter(Streaming.read_iter(bytes.map(|byte| [byte]).iter(), QIF.decoder(options)))
			$same = $same and bulk == bytewise
		}
	}
	$same
}

expect {
	bytes = "!Type:Bank\r\nPone\r\n".to_utf8()
	header = QIF.decoder({}).step(bytes, More)
	body = header.state.step(bytes.drop_first(header.consumed), More)
	record = body.state.step("^".to_utf8(), Final)
	finished = record.state.step([], Final)
	body.outcome == NeedInput and body.state.state().offset == 18 and body.state.state().line_number == 3 and body.state.state().column == 1 and body.state.state().line_start == 18 and body.state.state().record_bytes == 6 and !body.state.state().skip_lf and body.state.state().line_bytes.is_empty() and body.state.state().record_at == { offset: 12, line: 2, column: 1 } and record.state.state().offset == 19 and record.state.state().line_number == 3 and record.state.state().column == 2 and record.state.state().line_start == 18 and record.state.state().record_bytes == 0 and finished.outcome == Done and finished.state.state().done
}
