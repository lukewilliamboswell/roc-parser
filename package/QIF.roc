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
	parse_with = |source, props| parse_bytes(source.to_utf8(), { ..props, encoding: Utf8 })

	## Parse raw bytes with explicit encoding and resource limits.
	parse_bytes : List(U8), Props -> Try(QIF, [InvalidQif(Error)])
	parse_bytes = |bytes, props| {
		var $decoder = initial_qif(props)
		var $pos = 0
		var $events = []
		while True {
			result = qif_step($decoder, bytes.drop_first($pos), Final)
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
		for event in doc.events {
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
		if record.fields.keep_if(|f| f.code == 'D').len() > 1 or record.fields.keep_if(|f| f.code == 'T').len() > 1 {
			return Err(bad(DuplicateCoreField))
		}
		date_text = field(record, 'D').map_err(|_| bad(MissingField('D')))?
		date = parse_date(date_text, props.date_order).map_err(|_| bad(InvalidDate({ value: date_text, order: props.date_order })))?
		amount = field(record, 'T').map_err(|_| bad(MissingField('T')))?
		if !valid_amount(amount) {
			return Err(bad(InvalidAmount(amount)))
		}
		var $splits = []
		var $split = { category: "", memo: "", amount: "" }
		var $has_split = False
		for f in record.fields {
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
		Ok({ raw: record, date, amount, payee: field(record, 'P') ?? "", memo: field(record, 'M') ?? "", number: field(record, 'N') ?? "", category: field(record, 'L') ?? "", cleared: field(record, 'C') ?? "", splits: $splits })
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
		bytes = encode_text(text, props.encoding).map_err(|_| bad(UnrepresentableCharacter(Windows1252)))?
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

qif_step : QIF.Reader, List(U8), Streaming.InputEnd -> Streaming.Step(QIF.Reader, QIF.Event, QIF.Error)
qif_step = |initial, bytes, end| {
	if initial.done {
		return { state: initial, consumed: 0, outcome: Done }
	}
	var $s = initial
	var $pos = 0
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		$pos = $pos + 1
		additional = if (($s.skip_lf and byte == '\n' and $s.record_bytes == 0) or ((byte == '\r' or byte == '\n') and $s.line_bytes == ['^'])) 0 else 1
		$s = { ..$s, offset: $s.offset + 1, record_bytes: $s.record_bytes + additional }
		if $s.record_bytes > $s.props.max_record_bytes {
			return { state: terminal($s), consumed: $pos, outcome: Failed(located_problem($s, RecordTooLarge($s.props.max_record_bytes))) }
		}
		if $s.skip_lf and byte == '\n' {
			$s = { ..$s, skip_lf: False, line_start: $s.offset }
		} else if byte == '\r' or byte == '\n' {
			line = $s.line_bytes
			$s = { ..$s, line_bytes: [], line_number: $s.line_number + 1, column: 1, skip_lf: byte == '\r' }
			processed = process_line($s, line)
			$s = { ..processed.state, line_start: $s.offset }
			match processed.result {
				Emitted(event) => return { state: $s, consumed: $pos, outcome: Item(event) }
				Error(error) => return { state: terminal($s), consumed: $pos, outcome: Failed(error) }
				Continue => {}
			}
		} else {
			$s = { ..$s, skip_lf: False, line_bytes: $s.line_bytes.append(byte), column: $s.column + 1 }
			if $s.line_bytes.len() > $s.props.max_line_bytes {
				return { state: terminal($s), consumed: $pos, outcome: Failed(located_problem($s, LineTooLarge($s.props.max_line_bytes))) }
			}
		}
	}
	if end == More {
		return { state: $s, consumed: $pos, outcome: NeedInput }
	}
	if !$s.line_bytes.is_empty() {
		line = $s.line_bytes
		$s = { ..$s, line_bytes: [] }
		processed = process_line($s, line)
		$s = processed.state
		match processed.result {
			Emitted(event) => return { state: $s, consumed: $pos, outcome: Item(event) }
			Error(error) => return { state: terminal($s), consumed: $pos, outcome: Failed(error) }
			Continue => {}
		}
	}
	if !$s.fields.is_empty() {
		return { state: terminal($s), consumed: $pos, outcome: Failed(located_problem($s, MissingRecordTerminator)) }
	}
	{ state: terminal($s), consumed: $pos, outcome: Done }
}

initial_qif : QIF.Props -> QIF.Reader
initial_qif = |props| { props, offset: 0, line_number: 1, column: 1, line_start: 0, line_bytes: [], skip_lf: False, fields: [], section: "", account: "", record_at: { offset: 0, line: 1, column: 1 }, record_bytes: 0, done: False }

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

terminal : QIF.Reader -> QIF.Reader
terminal = |s| { ..s, line_bytes: [], fields: [], section: "", account: "", done: True }

located_problem : QIF.Reader, Parser.Problem(QIF.Problem) -> QIF.Error
located_problem = |s, problem| { offset: s.offset, line: s.line_number, column: s.column, problem }

process_line : QIF.Reader, List(U8) -> { state : QIF.Reader, result : [Continue, Emitted(QIF.Event), Error(QIF.Error)] }
process_line = |s, bytes| {
	bom = s.line_start == 0 and s.props.encoding == Utf8 and bytes.take_first(3) == [0xef, 0xbb, 0xbf]
	content = if (bom) bytes.drop_first(3) else bytes
	prefix = if (bom) 3 else 0
	at = { offset: s.line_start + prefix, line: if (s.column == 1) s.line_number - 1 else s.line_number, column: prefix + 1 }
	bad = |problem| { state: s, result: Error({ offset: at.offset, line: at.line, column: at.column, problem }) }
	text = match Streaming.decode_text(content, s.props.encoding) {
		Ok(value) => value
		Err(BadEncoding(error)) => return { state: s, result: Error({ offset: at.offset + error.index, line: at.line, column: at.column + error.index, problem: InvalidEncoding({ encoding: s.props.encoding, error }) }) }
	}
	if bytes.contains(0) {
		return bad(NulCharacter)
	}
	if content.is_empty() {
		return { state: s, result: Continue }
	}
	if text.starts_with("!") {
		if !s.fields.is_empty() {
			return bad(HeaderBeforeTerminator)
		}
		if bytes.len() > s.props.max_context_bytes {
			return bad(ContextTooLarge(s.props.max_context_bytes))
		}
		section = if (text == "!Account") "Account" else if (text.starts_with("!Type:")) Str.from_utf8_lossy(text.to_utf8().drop_first(6)) else s.section
		return { state: { ..s, section, record_bytes: 0 }, result: Emitted(Header(text)) }
	}
	if text == "^" {
		if s.section == "" {
			return bad(MissingSection)
		}
		record = { section: s.section, account: s.account, fields: s.fields, at: if (s.fields.is_empty()) at else s.record_at }
		account = if (s.section == "Account") QIF.field(record, 'N') ?? "" else s.account
		if s.section == "Account" and account == "" {
			return bad(MissingField('N'))
		}
		if account.count_utf8_bytes() > s.props.max_context_bytes {
			return bad(ContextTooLarge(s.props.max_context_bytes))
		}
		return { state: { ..s, fields: [], account, record_bytes: 0 }, result: Emitted(Record(record)) }
	}
	if s.section == "" {
		return bad(MissingSection)
	}
	if s.fields.len() >= s.props.max_fields {
		return bad(TooManyFields(s.props.max_fields))
	}
	code = content.first() ?? 0
	if code < 33 or code > 126 or code == '^' {
		return bad(InvalidFieldCode(code))
	}
	value = decode_text(content.drop_first(1), s.props.encoding) ?? ""
	{ state: { ..s, fields: s.fields.append({ code, value }), record_at: if (s.fields.is_empty()) at else s.record_at }, result: Continue }
}

has_break : Str -> Bool
has_break = |text| text.to_utf8().any(|b| b == '\r' or b == '\n' or b == 0)

valid_amount : Str -> Bool
valid_amount = |text| {
	bytes = text.to_utf8()
	unsigned = match bytes {
		['+', .. as rest] | ['-', .. as rest] => rest
		_ => bytes
	}
	var $digits = 0
	var $dots = 0
	for b in unsigned {
		if b >= '0' and b <= '9' {
			$digits = $digits + 1
		} else if b == '.' {
			$dots = $dots + 1
		} else {
			return False
		}
	}
	$digits > 0 and $dots <= 1
}

parse_date : Str, QIF.DateOrder -> Try(QIF.Date, [InvalidDate])
parse_date = |text, order| {
	parts = Str.replace_each(Str.replace_each(text, "'", "/"), "-", "/").split_on("/")
	if parts.len() != 3 {
		return Err(InvalidDate)
	}
	a = U64.from_str((parts.get(0) ?? "").trim()).map_err(|_| InvalidDate)?
	b = U64.from_str((parts.get(1) ?? "").trim()).map_err(|_| InvalidDate)?
	c = U64.from_str((parts.get(2) ?? "").trim()).map_err(|_| InvalidDate)?
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
	} else if ([4.U64, 6, 9, 11].contains(date.month)) 30 else 31
	if (date.day > max_day) Err(InvalidDate) else Ok(date)
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
	source = "!Type:Bank\nD27/08/2024\nTbad\n^\nD27/08/2024\nT1\n^\n"
	report = Streaming.collect_report(Streaming.read_iter([source.to_utf8()].iter(), QIF.transaction_decoder({}).with_errors({ error_policy: ReportInvalidRecords })))?
	report.values.len() == 2 and report.errors.len() == 1 and report.report == { emitted_items: 2, rejected_records: 1 }
}
expect {
	doc = QIF.parse_str("!Type:Bank\nD27/08/2024\nTbad\n^\n")?
	doc.transactions({}).is_err()
}

expect {
	bytes = [0xef, 0xbb, 0xbf].concat("!Type:Bank\r\nD27/08/2024\r\nT1\r\n^\r\n".to_utf8())
	found = Streaming.collect(Streaming.read_iter(bytes.map(|byte| [byte]).iter(), QIF.decoder({})))?
	expected = QIF.parse_str("!Type:Bank\r\nD27/08/2024\r\nT1\r\n^\r\n")?
	semantic_events(found) == semantic_events(expected.events)
}
expect QIF.parse_bytes([0xef, 0xbb, 0xbf], {}).is_ok()
