import Streaming
import Xml
import Parser
import Utf8
import Literal

## Streaming banking and credit-card OFX 1.x SGML and 2.x XML statements.
## Amounts, identifiers, and timestamps remain exact strings. Unknown XML
## transaction elements are preserved in order. Unknown SGML tags are rejected
## because their omitted-close behaviour is ambiguous. DTDs are unsupported.
OFX := { statements : List(OFX.Statement) }.{
	is_eq : _
	to_hash : _

	## Original encoded byte offset and one-based line and byte column.
	Error : { offset : U64, line : U64, column : U64, message : Str }

	## Writer wire syntax; readers detect either family.
	Syntax : [Xml203, Sgml102]

	## Auto honours the header and defaults to UTF-8.
	TextEncoding : [Auto, Utf8, Windows1252]

	## Bounded parser resources; limits are terminal errors.
	Props := { syntax : [Auto, Xml, Sgml] ?? Auto, encoding : TextEncoding ?? Auto, errors : Streaming.ReadProps ?? {}, max_token_bytes : U64 ?? 65_536, max_record_bytes : U64 ?? 4_194_304, max_depth : U64 ?? 64, max_fields : U64 ?? 1024 }

	## XML 2.03 and UTF-8 are the default output.
	WriteProps := { syntax : Syntax ?? Xml203, encoding : Streaming.TextEncoding ?? Utf8, max_record_bytes : U64 ?? 4_194_304 }

	## Account identifiers preserve leading zeroes. Empty optional fields are absent.
	Account : { bank_id : Str, branch_id : Str, account_id : Str, account_type : Str, key : Str }

	## Statement metadata emitted before its first transaction or balance.
	Status : { code : Str, severity : Str, message : Str }

	## Supported response metadata preserved across dialect conversion.
	Header : { old_uid : Str, new_uid : Str, server_date : Str, language : Str, org : Str, fid : Str }

	## Statement wrapper identity and successful response status.
	Context : { kind : [Bank, CreditCard], currency : Str, account : Account, start : Str, end : Str, transaction_id : Str, status : Status, header : Header }

	## Exact financial values with supported extension elements in source order.
	Transaction : { type : Str, posted : Str, amount : Str, id : Str, name : Str, memo : Str, check_number : Str, extensions : List(Xml.Node) }

	## Exact amount and timestamp.
	Balance : { amount : Str, as_of : Str }

	## Bounded logical reader/writer items.
	Event : [StatementStart(Context), Transaction(Transaction), LedgerBalance(Balance), AvailableBalance(Balance), StatementEnd]

	## Whole-document convenience representation.
	Statement : { context : Context, transactions : List(Transaction), ledger_balance : Try(Balance, [Missing]), available_balance : Try(Balance, [Missing]) }

	## Persistent state retaining at most one unfinished record.
	Decoder : State

	## Construct an incremental decoder.
	decoder : Props -> Decoder
	decoder = initial

	## Read at most one logical event and return the consumed byte count.
	step : Decoder, List(U8), Streaming.InputEnd -> Streaming.Step(Decoder, Event, Error)
	step = advance

	## Decode pure byte chunks lazily; all records are checked on consumption.
	parse_iter : Iter(List(U8)), Props -> Iter(Try(Streaming.ReadEvent(Event, Error), Streaming.ReadError(Error)))
	parse_iter = |chunks, props| Streaming.iter(chunks, decoder(props), step, props.errors)

	## Decode effectful chunks, preserving source errors.
	parse_stream : Stream(Try(List(U8), source_error)), Props -> _
	parse_stream = |chunks, props| Streaming.stream(chunks, decoder(props), step, props.errors)

	## Collect events, rejected-record diagnostics, and the successful completion report.
	parse_report : List(U8), Props -> _
	parse_report = |bytes, props| Streaming.collect_report(parse_iter([bytes].iter(), props))

	## Encode logical events lazily, splitting output only after full validation.
	write_iter : Iter(Event), WriteProps, U64 -> _
	write_iter = |events, props, chunk_bytes| Streaming.encode_state_iter(events, writer(props), |state, event| write(state, event).map_ok(|result| { state: result.writer, bytes: result.bytes }), finish, chunk_bytes)

	## Encode a fallible effectful source; source failure does not close the file.
	write_stream : Stream(Try(Event, source_error)), WriteProps, U64 -> _
	write_stream = |events, props, chunk_bytes| Streaming.encode_state_stream(events, writer(props), |state, event| write(state, event).map_ok(|result| { state: result.writer, bytes: result.bytes }), finish, chunk_bytes)

	## Collect a complete encoded document with encounter-order fail-fast errors.
	parse_bytes : List(U8), Props -> Try(OFX, [InvalidOfx(Error)])
	parse_bytes = |bytes, props| {
		if props.errors.error_policy != FailFast {
			return Err(InvalidOfx(write_error("parse_bytes is fail-fast; use parse_iter or parse_report for recovery")))
		}
		var $state = decoder(props)
		var $at = 0
		var $statements = []
		var $current = empty_statement(empty_context)
		while True {
			result = step($state, bytes.drop_first($at), Final)
			$state = result.state
			$at = $at + result.consumed
			match result.outcome {
				Item(StatementStart(context)) => {
					$current = empty_statement(context)
				}
				Item(Transaction(transaction)) => {
					$current = { ..$current, transactions: $current.transactions.append(transaction) }
				}
				Item(LedgerBalance(balance)) => {
					$current = { ..$current, ledger_balance: Ok(balance) }
				}
				Item(AvailableBalance(balance)) => {
					$current = { ..$current, available_balance: Ok(balance) }
				}
				Item(StatementEnd) => {
					$statements = $statements.append($current)
				}
				Failed(error) | RecordError(error) => return Err(InvalidOfx(error))
				Done => return Ok(OFX.{ statements: $statements })
				NeedInput => return Err(InvalidOfx(problem($state, "incomplete OFX document")))
			}
		}
	}

	## Parse already decoded UTF-8; contradictory encoding headers fail.
	parse_str : Str -> Try(OFX, [InvalidOfx(Error)])
	parse_str = |source| parse_bytes(source.to_utf8(), { encoding: Utf8 })

	## Complete OFX document as a byte combinator.
	parser : Parser(Utf8.Bytes, OFX)
	parser = Parser.custom(
		|bytes| match parse_bytes(bytes, {}) {
			Ok(value) => Ok({ value, rest: [] })
			Err(InvalidOfx(error)) => Err(ParseError({ message: error.message, offset: error.offset }))
		},
	)

	## Compilation-time validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Validate imported or quoted OFX.
	validate : ValidationProps -> Try(OFX, [BadQuotedBytes(Str)])
	validate = |props| parse_bytes(props.source.to_utf8(), { ..props.options, encoding: Utf8 }).map_err(|InvalidOfx(error)| Literal.error({ name: props.name, format: "OFX", line: error.line, column: error.column, message: error.message }))

	## Checked literal conversion.
	from_quote : Str -> Try(OFX, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## Stateful writer: validates complete logical items before returning bytes.
	Writer : WriteState

	## Begin a writer. Call write for each item and finish after StatementEnd.
	writer : WriteProps -> Writer
	writer = |props| { props, opened: False, statement: False, list_open: False, kind: Bank, balances: [], header: empty_header }

	## Validate and encode one complete item atomically.
	write : Writer, Event -> Try({ writer : Writer, bytes : List(U8) }, [InvalidOfx(Error)])
	write = write_event

	## Close a successfully completed document.
	finish : Writer -> Try(List(U8), [InvalidOfx(Error)])
	finish = |state| {
		if state.statement {
			return Err(InvalidOfx(write_error("unfinished OFX statement")))
		}
		encode_text(if (state.opened) "</OFX>\n" else "${preamble(state.props, empty_header)}<OFX></OFX>\n", state.props.encoding).map_err(|error| InvalidOfx(error))
	}

	## Collect encoded output; event writing remains bounded by record size.
	to_bytes : OFX, WriteProps -> Try(List(U8), [InvalidOfx(Error)])
	to_bytes = |document, props| {
		var $writer = writer(props)
		var $bytes = []
		for statement in document.statements {
			var $events = [StatementStart(statement.context)].concat(statement.transactions.map(|value| Transaction(value)))
			match statement.ledger_balance {
				Ok(value) => {
					$events = $events.append(LedgerBalance(value))
				}
				Err(_) => {}
			}
			match statement.available_balance {
				Ok(value) => {
					$events = $events.append(AvailableBalance(value))
				}
				Err(_) => {}
			}
			$events = $events.append(StatementEnd)
			for event in $events {
				next = write($writer, event)?
				$writer = next.writer
				$bytes = $bytes.concat(next.bytes)
			}
		}
		Ok($bytes.concat(finish($writer)?))
	}
}

Frame : { name : Str, offset : U64 }

# Statement metadata changes rarely while tokens are consumed. Keep it behind
# shared boxes so moving the lexical state does not copy all its strings.
State : { props : OFX.Props, offset : U64, line : U64, column : U64, last_cr : Bool, token : List(U8), token_mode : Bool, quote : U8, header : List(U8), header_done : Bool, syntax : OFX.Syntax, encoding : [Utf8, Windows1252, Ascii], stack : List(Frame), capture : List(U8), capture_tag : Str, capture_depth : U64, leaf : List(U8), context : Box(OFX.Context), started : Bool, in_statement : Bool, root_seen : Bool, root_closed : Bool, header_info : Box(OFX.Header), wrapper_id : Str, wrapper_status : Box(OFX.Status), status_seen : Bool, context_seen : List(Str), balances_seen : List(Str), terminal : Bool }

WriteState : { props : OFX.WriteProps, opened : Bool, statement : Bool, list_open : Bool, kind : [Bank, CreditCard], balances : List(Str), header : OFX.Header }

empty_account : OFX.Account
empty_account = { bank_id: "", branch_id: "", account_id: "", account_type: "", key: "" }

empty_context : OFX.Context
empty_context = { kind: Bank, currency: "", account: empty_account, start: "", end: "", transaction_id: "", status: { code: "", severity: "", message: "" }, header: empty_header }

empty_header : OFX.Header
empty_header = { old_uid: "NONE", new_uid: "NONE", server_date: "20000101000000", language: "ENG", org: "", fid: "" }

empty_statement : OFX.Context -> OFX.Statement
empty_statement = |context| { context, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }

initial : OFX.Props -> State
initial = |props| { props, offset: 0, line: 1, column: 1, last_cr: False, token: [], token_mode: False, quote: 0, header: [], header_done: False, syntax: Xml203, encoding: if (props.encoding == Windows1252) Windows1252 else Utf8, stack: [], capture: [], capture_tag: "", capture_depth: 0, leaf: [], context: Box.box(empty_context), started: False, in_statement: False, root_seen: False, root_closed: False, header_info: Box.box(empty_header), wrapper_id: "", wrapper_status: Box.box({ code: "", severity: "", message: "" }), status_seen: False, context_seen: [], balances_seen: [], terminal: False }

problem : State, Str -> OFX.Error
problem = |state, message| { offset: state.offset, line: state.line, column: state.column, message }

write_error : Str -> OFX.Error
write_error = |message| { offset: 0, line: 0, column: 0, message }

move : State, U8 -> State
move = |state, byte| {
	if byte == '\r' {
		{ ..state, offset: state.offset + 1, line: state.line + 1, column: 1, last_cr: True }
	}
	else if byte == '\n' {
		{ ..state, offset: state.offset + 1, line: state.line + (if (state.last_cr) 0 else 1), column: 1, last_cr: False }
	}
	else {
		{ ..state, offset: state.offset + 1, column: state.column + 1, last_cr: False }
	}
}

markup_start : Utf8.ByteClass
markup_start = Utf8.ByteClass.from_bytes(['<'])

# Apply source coordinates once per span, visiting only its line endings.
# In particular, CRLF remains one line ending even when split between calls.
move_span : State, List(U8), U64, U64 -> State
move_span = |state, bytes, start, end| {
	var $at = start
	var $line = state.line
	var $column = state.column
	var $last_cr = state.last_cr
	while $at < end {
		next = Utf8.find_line_end(bytes, $at)
		if next >= end {
			break
		}
		if next > $at {
			$last_cr = False
		}
		byte = bytes.get(next) ?? 0
		if byte == '\r' {
			$line = $line + 1
			$last_cr = True
		} else {
			$line = $line + (if ($last_cr) 0 else 1)
			$last_cr = False
		}
		$column = 1
		$at = next + 1
	}
	if $at < end {
		$column = $column + end - $at
		$last_cr = False
	}
	{ ..state, offset: state.offset + end - start, line: $line, column: $column, last_cr: $last_cr }
}

# Include at most the first excess byte so limit errors keep their original
# offset and the unfinished buffer never grows to the rest of a large chunk.
bounded_end : U64, U64, U64, U64 -> U64
bounded_end = |start, end, retained, limit| {
	allowance = if (retained >= limit) 0 else limit - retained
	if end - start > allowance {
		start + allowance + 1
	} else {
		end
	}
}

# Keep the scanner's cursor and quote state in small locals. Materialize one
# token per call rather than updating the full decoder for every byte.
scan_token : List(U8), List(U8), U64, U64, U8 -> { token : List(U8), at : U64, quote : U8, complete : Bool }
scan_token = |prefix, bytes, start, end, quote| {
	joined = |index| if (index < prefix.len()) prefix.get(index) ?? 0 else bytes.get(start + index - prefix.len()) ?? 0
	available = prefix.len() + end - start
	comment = available >= 4 and joined(0) == '<' and joined(1) == '!' and joined(2) == '-' and joined(3) == '-'
	cdata = available >= 9 and joined(0) == '<' and joined(1) == '!' and joined(2) == '[' and joined(3) == 'C' and joined(4) == 'D' and joined(5) == 'A' and joined(6) == 'T' and joined(7) == 'A' and joined(8) == '['
	pi = available >= 2 and joined(0) == '<' and joined(1) == '?'
	var $at = start
	var $quote = quote
	while $at < end {
		byte = bytes.get($at) ?? 0
		$at = $at + 1
		if byte == '"' or byte == '\'' {
			$quote = if ($quote == 0) byte else if ($quote == byte) 0 else $quote
		}
		if byte == '>' {
			len = prefix.len() + $at - start
			complete = if (comment) len >= 3 and joined(len - 2) == '-' and joined(len - 3) == '-' else if (cdata) len >= 3 and joined(len - 2) == ']' and joined(len - 3) == ']' else if (pi) len >= 2 and joined(len - 2) == '?' else $quote == 0
			if complete {
				return { token: prefix.concat(bytes.sublist({ start, len: $at - start })), at: $at, quote: $quote, complete: True }
			}
		}
	}
	{ token: prefix.concat(bytes.sublist({ start, len: $at - start })), at: $at, quote: $quote, complete: False }
}

advance : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, OFX.Event, OFX.Error)
advance = |state, bytes, ending| {
	if state.terminal {
		return { state, consumed: 0, outcome: Done }
	}
	var $s = state
	var $at = 0
	var $borrowed_leaf = False
	while $at < bytes.len() {
		byte = bytes.get($at) ?? 0
		if $s.header_done and $s.token_mode {
			end = bounded_end($at, bytes.len(), $s.token.len(), $s.props.max_token_bytes)
			scanned = scan_token($s.token, bytes, $at, end, $s.quote)
			$s = move_span($s, bytes, $at, scanned.at)
			$at = scanned.at
			$s = { ..$s, token: scanned.token, quote: scanned.quote }
			if $s.token.len() > $s.props.max_token_bytes {
				return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX token exceeds max_token_bytes")) }
			}
			if scanned.complete {
				match consume_token($s) {
					Err(failure) => return { state: { ..failure.state, terminal: True }, consumed: $at, outcome: Failed(failure.error) }
					Ok({ state: next, outcome }) => {
						$s = { ..next, token: [], token_mode: False, quote: 0 }
						match outcome {
							Quiet => {}
							Emitted(event) => return { state: $s, consumed: $at, outcome: Item(event) }
							Rejected(error) => return { state: $s, consumed: $at, outcome: RecordError(error) }
						}
					}
				}
			}
		} else if $s.header_done and byte != '<' {
			end = bounded_end($at, bounded_end($at, Utf8.find_any(bytes, $at, markup_start), $s.leaf.len(), $s.props.max_token_bytes), $s.capture.len() + $s.leaf.len(), $s.props.max_record_bytes)
			$borrowed_leaf = $s.leaf.is_empty()
			$s = move_span($s, bytes, $at, end)
			$s = { ..$s, leaf: $s.leaf.concat(bytes.sublist({ start: $at, len: end - $at })) }
			$at = end
			if $s.leaf.len() > $s.props.max_token_bytes {
				return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX field exceeds max_token_bytes")) }
			}
		} else {
			$s = move($s, byte)
			$at = $at + 1
			if !$s.header_done {
				if byte == '<' {
					match read_header($s) {
						Err(error) => return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(error) }
						Ok(next) => {
							$s = { ..next, token_mode: True, token: ['<'] }
						}
					}
				} else {
					$s = { ..$s, header: $s.header.append(byte) }
					if $s.header.len() > $s.props.max_token_bytes {
						return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX header exceeds max_token_bytes")) }
					}
				}
			} else {
				if !$s.capture_tag.is_empty() {
					$s = { ..$s, capture: $s.capture.concat($s.leaf) }
				}
				else if !$s.leaf.is_empty() {
					match consume_text($s) {
						Err(error) => return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(error) }
						Ok(next) => {
							$s = next
						}
					}
				}
				$s = { ..$s, leaf: [], token_mode: True, token: ['<'] }
				$borrowed_leaf = False
			}
		}
		if $s.capture.len() + $s.leaf.len() > $s.props.max_record_bytes {
			return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX record exceeds max_record_bytes")) }
		}
	}
	if ending == Final {
		if $s.token_mode or !$s.header_done or !$s.stack.is_empty() or !$s.root_closed {
			return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "incomplete OFX document")) }
		}
		if !Str.from_utf8_lossy($s.leaf).trim().is_empty() {
			return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "content after OFX root")) }
		}
		{ state: { ..$s, terminal: True }, consumed: $at, outcome: Done }
	} else {
		if $borrowed_leaf {
			# A complete field may borrow its current chunk, but an unfinished
			# field must not keep that entire allocation alive across pulls.
			var $owned = List.with_capacity($s.leaf.len())
			for byte in $s.leaf {
				$owned = $owned.append(byte)
			}
			$s = { ..$s, leaf: $owned }
		}
		{ state: $s, consumed: $at, outcome: NeedInput }
	}
}

lower : Str -> Str
lower = |text| Str.from_utf8_lossy(text.to_utf8().map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b))

read_header : State -> Try(State, OFX.Error)
read_header = |state| {
	header_bytes = if (state.header.starts_with([239, 187, 191])) state.header.drop_first(3) else state.header
	text = Str.from_utf8(header_bytes).map_err(|_| problem(state, "OFX header must be ASCII"))?
	if text.to_utf8().any(|b| b > 127) {
		return Err(problem(state, "OFX header must be ASCII"))
	}
	if text.trim().is_empty() {
		if state.props.syntax == Sgml {
			return Err(problem(state, "XML source contradicts SGML syntax override"))
		}
		return Ok({ ..state, header_done: True })
	}
	if state.props.syntax == Xml {
		return Err(problem(state, "SGML source contradicts XML syntax override"))
	}
	var $encoding = Utf8
	var $charset = "NONE"
	var $version = ""
	var $seen = []
	var $header_info = Box.unbox(state.header_info)
	for line in text.replace_each("\r", "").split_on("\n").keep_if(|s| !s.trim().is_empty()) {
		parts = line.split_on(":")
		if parts.len() != 2 {
			return Err(problem(state, "malformed OFX header field"))
		}
		key = (parts.first() ?? "").trim()
		value = (parts.last() ?? "").trim()
		if $seen.contains(key) {
			return Err(problem(state, "duplicate OFX header field"))
		}
		$seen = $seen.append(key)
		match key {
			"OFXHEADER" => if value != "100" {
				return Err(problem(state, "unsupported OFX header"))
			}
			"DATA" => if value != "OFXSGML" {
				return Err(problem(state, "unsupported OFX data format"))
			}
			"VERSION" => {
				$version = value
			}
			"ENCODING" => match lower(value) {
				"usascii" => {
					$encoding = Ascii
				}
				"utf-8" | "utf8" | "unicode" => {
					$encoding = Utf8
				}
				_ => return Err(problem(state, "unsupported OFX encoding"))
			}
			"CHARSET" => {
				$charset = value
			}
			"SECURITY" | "COMPRESSION" => if value != "NONE" {
				return Err(problem(state, "encrypted or compressed OFX is unsupported"))
			}
			"OLDFILEUID" => {
				$header_info = { ..$header_info, old_uid: value }
			}
			"NEWFILEUID" => {
				$header_info = { ..$header_info, new_uid: value }
			}
			_ => return Err(problem(state, "unknown OFX header field"))
		}
	}
	if !$seen.contains("OFXHEADER") or !$version.starts_with("1") {
		return Err(problem(state, "missing or inconsistent OFX 1.x header"))
	}
	if $charset == "1252" {
		if $encoding != Ascii {
			return Err(problem(state, "contradictory OFX encoding and charset"))
		}
		$encoding = Windows1252
	} else if $charset != "NONE" and $charset != "0" {
		return Err(problem(state, "unsupported OFX charset"))
	}
	if (state.props.encoding == Utf8 and $encoding == Windows1252) or (state.props.encoding == Windows1252 and $encoding != Windows1252) {
		return Err(problem(state, "encoding override contradicts OFX header"))
	}
	Ok({ ..state, header_done: True, syntax: Sgml102, encoding: $encoding, header: [], header_info: Box.box($header_info) })
}

decode : State, List(U8) -> Try(Str, OFX.Error)
decode = |state, bytes| {
	if state.encoding == Ascii and bytes.any(|b| b > 127) {
		return Err(problem(state, "non-ASCII byte in ASCII OFX"))
	}
	Streaming.decode_text(bytes, if (state.encoding == Windows1252) Windows1252 else Utf8).map_err(|BadEncoding(error)| problem(state, error.message))
}

consume_text : State -> Try(State, OFX.Error)
consume_text = |state| {
	text = decode(state, state.leaf)?
	top = state.stack.last().map_ok(|frame| frame.name) ?? ""
	if !leaf_tag(top) {
		if !text.trim().is_empty() {
			return Err(problem(state, "text outside an OFX field"))
		}
		return Ok(state)
	}
	decoded = Xml.parse_str("<v>${text}</v>").map_err(|_| problem(state, "invalid OFX character data"))?.root.text().trim()
	if ["CURDEF", "DTSTART", "DTEND"].contains(top) {
		if state.started or state.context_seen.contains(top) {
			return Err(problem(state, "duplicate or late OFX context field"))
		}
	}
	context = Box.unbox(state.context)
	if ["DTSERVER", "LANGUAGE", "ORG", "FID"].contains(top) {
		if state.started {
			return Err(problem(state, "late OFX signon metadata"))
		}
		header = Box.unbox(state.header_info)
		next_header = match top {
			"DTSERVER" => { ..header, server_date: decoded }
			"LANGUAGE" => { ..header, language: decoded }
			"ORG" => { ..header, org: decoded }
			_ => { ..header, fid: decoded }
		}
		return Ok({ ..state, header_info: Box.box(next_header) })
	}
	match top {
		"CURDEF" => Ok({ ..state, context_seen: state.context_seen.append(top), context: Box.box({ ..context, currency: decoded }) })
		"DTSTART" => Ok({ ..state, context_seen: state.context_seen.append(top), context: Box.box({ ..context, start: decoded }) })
		"DTEND" => Ok({ ..state, context_seen: state.context_seen.append(top), context: Box.box({ ..context, end: decoded }) })
		"TRNUID" => Ok({ ..state, wrapper_id: decoded })
		"CODE" => {
			if decoded != "0" {
				return Err(problem(state, "non-success OFX responses are unsupported"))
			}
			Ok({ ..state, wrapper_status: Box.box({ ..Box.unbox(state.wrapper_status), code: decoded }) })
		}
		"SEVERITY" => Ok({ ..state, wrapper_status: Box.box({ ..Box.unbox(state.wrapper_status), severity: decoded }) })
		"MESSAGE" => Ok({ ..state, wrapper_status: Box.box({ ..Box.unbox(state.wrapper_status), message: decoded }) })
		_ => Ok(state)
	}
}

Action : [Quiet, Emitted(OFX.Event), Rejected(OFX.Error)]

Consumed : { state : State, outcome : Action }

# Keep the failure state in the result so callers can consume the input state
# without retaining aliases for a cold error arm.
TokenFailure : { state : State, error : OFX.Error }

token_failure : State, Str -> TokenFailure
token_failure = |state, message| { state, error: problem(state, message) }

consume_token : State -> Try(Consumed, TokenFailure)
consume_token = |state| {
	text = decode(state, state.token).map_err(|error| { state, error })?
	if text.starts_with("<!DOCTYPE") or text.starts_with("<!ENTITY") {
		return Err(token_failure(state, "DTDs and external entities are unsupported"))
	}
	if text.starts_with("<!--") or text.starts_with("<![CDATA[") {
		if text.starts_with("<![CDATA[") and text.count_utf8_bytes() < 12 {
			return Err(token_failure(state, "malformed XML comment or CDATA"))
		}
		if text.starts_with("<![CDATA[") and ["NAME", "MEMO", "FITID", "TRNTYPE", "DTPOSTED", "TRNAMT", "CHECKNUM", "BANKID", "BRANCHID", "ACCTID", "ACCTTYPE", "ACCTKEY", "BALAMT", "DTASOF"].contains(state.stack.last().map_ok(|frame| frame.name) ?? "") {
			content = Str.from_utf8_lossy(text.to_utf8().sublist({ start: 9, len: text.count_utf8_bytes() - 12 }))
			if content != content.trim() {
				return Err(token_failure(state, "padded CDATA in typed OFX fields is unsupported"))
			}
		}
		_ = Xml.parse_str("<v>${text}</v>").map_err(|_| token_failure(state, "malformed XML comment or CDATA"))?
		if state.syntax == Sgml102 {
			return Err(token_failure(state, "SGML comments and CDATA are unsupported"))
		}
		if !state.capture_tag.is_empty() {
			return Ok({ state: { ..state, capture: state.capture.concat(state.token) }, outcome: Quiet })
		}
		if text.starts_with("<![CDATA[") {
			return Err(token_failure(state, "CDATA outside captured record is unsupported"))
		}
		return Ok({ state, outcome: Quiet })
	}
	if text.starts_with("<?") {
		if state.root_seen {
			return Err(token_failure(state, "processing instruction inside OFX is unsupported"))
		}
		if state.syntax == Sgml102 {
			return Err(token_failure(state, "XML prologue contradicts SGML header"))
		}
		low = lower(text)
		marker = if (low.starts_with("<?xml ")) "xml" else "ofx"
		if state.context_seen.contains(marker) or (marker == "xml" and state.context_seen.contains("ofx")) {
			return Err(token_failure(state, "duplicate or out-of-order OFX prologue"))
		}
		if marker == "xml" {
			_ = Xml.parse_str("${text}<v/>").map_err(|_| token_failure(state, "malformed XML declaration"))?
			if !state.header.is_empty() and state.header != [239, 187, 191] {
				return Err(token_failure(state, "XML declaration must start the document"))
			}
		}
		var $next = state
		if low.starts_with("<?xml ") {
			declared = xml_encoding(text).map_err(|message| token_failure(state, message))?
			$next = { ..state, encoding: declared }
			if (state.props.encoding == Utf8 and $next.encoding == Windows1252) or (state.props.encoding == Windows1252 and $next.encoding != Windows1252) {
				return Err(token_failure(state, "encoding override contradicts XML declaration"))
			}
		} else if text.starts_with("<?OFX ") {
			if state.token.len() < 8 {
				return Err(token_failure(state, "malformed OFX processing instruction"))
			}
			attrs = Str.from_utf8_lossy(state.token.sublist({ start: 6, len: state.token.len() - 8 }))
			node = Xml.parse_str("<p ${attrs}/>").map_err(|_| token_failure(state, "malformed OFX processing instruction"))?.root
			if node.attribute("OFXHEADER") != Ok("200") or !(node.attribute("VERSION") ?? "").starts_with("2") or node.attribute("SECURITY") != Ok("NONE") {
				return Err(token_failure(state, "unsupported OFX XML header"))
			}
			$next = { ..$next, header_info: Box.box({ ..Box.unbox($next.header_info), old_uid: node.attribute("OLDFILEUID") ?? "NONE", new_uid: node.attribute("NEWFILEUID") ?? "NONE" }) }
		} else {
			return Err(token_failure(state, "unknown OFX processing instruction"))
		}
		return Ok({ state: { ..$next, context_seen: state.context_seen.append(marker) }, outcome: Quiet })
	}
	if text.starts_with("<!") {
		return Err(token_failure(state, "unsupported OFX declaration"))
	}
	closing = text.starts_with("</")
	self_closed = text.ends_with("/>")
	if closing and self_closed {
		return Err(token_failure(state, "OFX closing tags cannot be self-closing"))
	}
	start = if (closing) 2 else 1
	end = text.count_utf8_bytes() - (if (self_closed) 2 else 1)
	if end < start {
		return Err(token_failure(state, "empty OFX tag name"))
	}
	name = Str.from_utf8_lossy(state.token.sublist({ start, len: end - start })).trim()
	if !valid_name(name) {
		return Err(token_failure(state, "OFX tag names must be uppercase without attributes"))
	}
	# Retaining the original record just to access its token would also keep
	# capture/stack aliases alive and force copies on every append below.
	token = state.token
	token_len = token.len()
	var $s = state
	top = $s.stack.last().map_ok(|frame| frame.name) ?? ""
	if $s.syntax == Sgml102 and leaf_tag(top) and !(closing and name == top) {
		if !$s.capture_tag.is_empty() {
			$s = { ..$s, capture: $s.capture.concat("</${top}>".to_utf8()) }
		}
		$s = { ..$s, stack: $s.stack.drop_last(1) }
	}
	if closing {
		return close_tag($s, name, text)
	}
	if $s.root_closed {
		return Err(token_failure($s, "content after OFX root"))
	}
	if !$s.root_seen {
		if name != "OFX" {
			return Err(token_failure($s, "expected OFX root"))
		}
		$s = { ..$s, root_seen: True }
	} else if $s.stack.is_empty() {
		return Err(token_failure($s, "multiple OFX roots"))
	}
	if name == "INVSTMTMSGSRSV1" or name == "INVSTMTRS" {
		return Err(token_failure($s, "investment OFX is unsupported"))
	}
	if $s.syntax == Xml203 and $s.capture_tag.is_empty() and !aggregate_tag(name) and !leaf_tag(name) {
		return Err(token_failure($s, "unsupported OFX element outside transaction extension"))
	}
	if $s.syntax == Sgml102 and !aggregate_tag(name) and !leaf_tag(name) {
		return Err(token_failure($s, "unknown SGML tag: closing behaviour is ambiguous"))
	}
	if $s.stack.len() >= $s.props.max_depth {
		return Err(token_failure($s, "OFX exceeds max_depth"))
	}
	if leaf_tag($s.stack.last().map_ok(|frame| frame.name) ?? "") {
		return Err(token_failure($s, "nested content in OFX scalar field"))
	}
	if name == "STMTTRNRS" or name == "CCSTMTTRNRS" {
		$s = { ..$s, wrapper_id: "", wrapper_status: Box.box({ code: "", severity: "", message: "" }), status_seen: False }
	}
	if name == "STMTRS" or name == "CCSTMTRS" {
		if $s.in_statement {
			return Err(token_failure($s, "nested OFX statements"))
		}
		$s = { ..$s, context: Box.box({ ..empty_context, kind: if (name == "STMTRS") Bank else CreditCard, transaction_id: $s.wrapper_id, status: Box.unbox($s.wrapper_status), header: Box.unbox($s.header_info) }), started: False, in_statement: True, context_seen: [], balances_seen: [] }
	}
	parent = $s.stack.last().map_ok(|frame| frame.name) ?? ""
	if !placement(name, parent) {
		return Err(token_failure($s, "OFX element in invalid parent"))
	}
	if name == "BANKACCTFROM" or name == "CCACCTFROM" {
		if $s.started or $s.context_seen.contains("account") {
			return Err(token_failure($s, "duplicate or late OFX account"))
		}
		$s = { ..$s, context_seen: $s.context_seen.append("account") }
	}
	if name == "LEDGERBAL" or name == "AVAILBAL" {
		if $s.balances_seen.contains(name) or (name == "LEDGERBAL" and $s.balances_seen.contains("AVAILBAL")) {
			return Err(token_failure($s, "duplicate OFX balance"))
		}
		$s = { ..$s, balances_seen: $s.balances_seen.append(name) }
	}
	if capture_tag(name) {
		if !$s.capture_tag.is_empty() {
			return Err(token_failure($s, "nested OFX record aggregate"))
		}
		$s = { ..$s, capture_tag: name, capture_depth: $s.stack.len(), capture: [] }
	}
	if !$s.capture_tag.is_empty() {
		$s = { ..$s, capture: $s.capture.concat(token) }
	}
	$s = { ..$s, stack: $s.stack.append({ name, offset: $s.offset - token_len }) }
	if self_closed {
		return close_tag($s, name, "")
	}
	if name == "STMTTRN" or name == "LEDGERBAL" or name == "AVAILBAL" {
		if !$s.in_statement {
			return Err(token_failure($s, "OFX record outside a statement"))
		}
		if !$s.started {
			validate_context(Box.unbox($s.context)).map_err(|message| token_failure($s, message))?
			$s = { ..$s, started: True }
			return Ok({ state: $s, outcome: Emitted(StatementStart(Box.unbox($s.context))) })
		}
	}
	Ok({ state: $s, outcome: Quiet })
}

close_tag : State, Str, Str -> Try(Consumed, TokenFailure)
close_tag = |state, name, text| {
	if state.stack.last().map_ok(|frame| frame.name) != Ok(name) {
		return Err(token_failure(state, "mismatched OFX closing tag"))
	}
	var $s = { ..state, stack: state.stack.drop_last(1) }
	if !$s.capture_tag.is_empty() {
		$s = { ..$s, capture: $s.capture.concat(text.to_utf8()) }
		if $s.stack.len() == $s.capture_depth {
			if $s.capture.len() > $s.props.max_record_bytes {
				return Err(token_failure($s, "OFX record exceeds max_record_bytes"))
			}
			captured = decode($s, $s.capture).map_err(|error| { state: $s, error })?
			root = Xml.parse_str(captured).map_err(|_| token_failure($s, "malformed OFX record"))?.root
			$s = { ..$s, capture: [], capture_tag: "" }
			if name == "STMTTRN" {
				if children(root).len() > $s.props.max_fields {
					return Err(token_failure($s, "OFX record exceeds max_fields"))
				}
				match transaction(root) {
					Ok(value) => return Ok({ state: $s, outcome: Emitted(Transaction(value)) })
					Err(message) => return Ok({ state: $s, outcome: Rejected(problem($s, message)) })
				}
			} else if name == "BANKACCTFROM" or name == "CCACCTFROM" {
				account = account_from(root).map_err(|message| token_failure($s, message))?
				$s = { ..$s, context: Box.box({ ..Box.unbox($s.context), account }) }
			} else {
				balance = balance_from(root).map_err(|message| token_failure($s, message))?
				return Ok({ state: $s, outcome: Emitted(if (name == "LEDGERBAL") LedgerBalance(balance) else AvailableBalance(balance)) })
			}
		}
	}
	if name == "BANKTRANLIST" and !$s.started {
		validate_context(Box.unbox($s.context)).map_err(|message| token_failure($s, message))?
		return Ok({ state: { ..$s, started: True }, outcome: Emitted(StatementStart(Box.unbox($s.context))) })
	}
	if name == "STMTRS" or name == "CCSTMTRS" {
		if !$s.started {
			return Err(token_failure($s, "OFX statement has no transaction list or balances"))
		}
		return Ok({ state: { ..$s, in_statement: False }, outcome: Emitted(StatementEnd) })
	}
	if name == "STATUS" {
		$s = { ..$s, status_seen: True }
	}
	if name == "OFX" {
		$s = { ..$s, root_closed: True }
	}
	Ok({ state: $s, outcome: Quiet })
}

capture_tag : Str -> Bool
capture_tag = |name| match name {
	"STMTTRN" | "BANKACCTFROM" | "CCACCTFROM" | "LEDGERBAL" | "AVAILBAL" => True
	_ => False
}

aggregate_tag : Str -> Bool
aggregate_tag = |name| match name {
	"OFX" | "SIGNONMSGSRSV1" | "SONRS" | "STATUS" | "FI" | "BANKMSGSRSV1" | "STMTTRNRS" | "STMTRS" | "BANKACCTFROM" | "BANKTRANLIST" | "STMTTRN" | "LEDGERBAL" | "AVAILBAL" | "CREDITCARDMSGSRSV1" | "CCSTMTTRNRS" | "CCSTMTRS" | "CCACCTFROM" | "CURRENCY" | "ORIGCURRENCY" | "PAYEE" | "BANKACCTTO" | "CCACCTTO" => True
	_ => False
}

leaf_tag : Str -> Bool
leaf_tag = |name| match name {
	"CODE" | "SEVERITY" | "MESSAGE" | "DTSERVER" | "LANGUAGE" | "ORG" | "FID" | "TRNUID" | "CLTCOOKIE" | "CURDEF" | "BANKID" | "BRANCHID" | "ACCTID" | "ACCTTYPE" | "ACCTKEY" | "DTSTART" | "DTEND" | "TRNTYPE" | "DTPOSTED" | "DTUSER" | "DTAVAIL" | "TRNAMT" | "FITID" | "CORRECTFITID" | "CORRECTACTION" | "SRVRTID" | "CHECKNUM" | "REFNUM" | "NAME" | "MEMO" | "SIC" | "PAYEEID" | "BALAMT" | "DTASOF" | "CURRENCYRATE" | "CURSYM" | "ADDR1" | "ADDR2" | "ADDR3" | "CITY" | "STATE" | "POSTALCODE" | "COUNTRY" | "PHONE" | "USERKEY" | "INTU.BID" => True
	_ => False
}

children : Xml.Node -> List(Xml.Node)
children = |node| match node {
	Element(element) => element.children.keep_if(|child| child.name().is_ok())
	Text(_) => []
}

field : Xml.Node, Str -> Str
field = |node, name| node.children_named(name).first().map_ok(|child| child.text().trim()) ?? ""

unique : Xml.Node -> Try({}, Str)
unique = |node| {
	var $names = []
	for child in children(node) {
		name = child.name() ?? ""
		if $names.contains(name) {
			return Err("duplicate OFX field ${Str.from_utf8_lossy(name.to_utf8().take_first(64))}")
		}
		$names = $names.append(name)
	}
	Ok({})
}

account_from : Xml.Node -> Try(OFX.Account, Str)
account_from = |node| {
	unique(node)?
	scalar_fields(node, if (node.name() == Ok("CCACCTFROM")) ["ACCTID", "ACCTKEY"] else ["BANKID", "BRANCHID", "ACCTID", "ACCTTYPE", "ACCTKEY"])?
	Ok({ bank_id: field(node, "BANKID"), branch_id: field(node, "BRANCHID"), account_id: field(node, "ACCTID"), account_type: field(node, "ACCTTYPE"), key: field(node, "ACCTKEY") })
}

transaction : Xml.Node -> Try(OFX.Transaction, Str)
transaction = |node| {
	unique(node)?
	if !aggregate_whitespace(node) {
		return Err("unexpected text between transaction fields")
	}
	if children(node).any(|child| leaf_tag(child.name() ?? "") and !children(child).is_empty()) {
		return Err("nested content in scalar transaction field")
	}
	value = { type: field(node, "TRNTYPE"), posted: field(node, "DTPOSTED"), amount: field(node, "TRNAMT"), id: field(node, "FITID"), name: field(node, "NAME"), memo: field(node, "MEMO"), check_number: field(node, "CHECKNUM"), extensions: children(node).keep_if(|child| !["TRNTYPE", "DTPOSTED", "TRNAMT", "FITID", "NAME", "MEMO", "CHECKNUM"].contains(child.name() ?? "")) }
	validate_transaction(value)?
	Ok(value)
}

validate_context : OFX.Context -> Try({}, Str)
validate_context = |context| {
	if !timestamp(context.header.server_date) or context.header.language.count_utf8_bytes() != 3 or !context.header.language.to_utf8().all(|byte| byte >= 'A' and byte <= 'Z') {
		return Err("invalid OFX signon date or language")
	}
	if [context.account.bank_id, context.account.branch_id, context.account.account_id, context.account.account_type, context.account.key, context.transaction_id].any(|text| text != text.trim()) {
		return Err("context identifiers must use normalized whitespace")
	}
	if ![context.currency, context.account.bank_id, context.account.branch_id, context.account.account_id, context.account.account_type, context.account.key, context.transaction_id, context.status.message, context.header.org, context.header.fid].all(valid_text) {
		return Err("invalid XML scalar in context")
	}
	if [context.header.old_uid, context.header.new_uid].any(|text| text.to_utf8().any(|byte| byte < 32 or byte > 126 or byte == '"' or byte == ':' or byte == '<' or byte == '>')) {
		return Err("invalid OFX file UID")
	}
	if context.transaction_id.is_empty() or context.status.code != "0" or context.status.severity != "INFO" {
		return Err("missing or unsupported OFX wrapper identity/status")
	}
	if context.currency.count_utf8_bytes() != 3 or !context.currency.to_utf8().all(|b| b >= 'A' and b <= 'Z') {
		return Err("OFX currency must be three uppercase letters")
	}
	if context.account.account_id.is_empty() {
		return Err("missing OFX account identifier")
	}
	if context.kind == Bank and context.account.bank_id.is_empty() {
		return Err("missing OFX bank identifier")
	}
	if !context.start.is_empty() and !timestamp(context.start) {
		return Err("invalid OFX statement start timestamp")
	}
	if !context.end.is_empty() and !timestamp(context.end) {
		return Err("invalid OFX statement end timestamp")
	}
	Ok({})
}

validate_transaction : OFX.Transaction -> Try({}, Str)
validate_transaction = |value| {
	if [value.id, value.name, value.memo, value.check_number].any(|text| text != text.trim()) {
		return Err("writer values must use normalized OFX whitespace")
	}
	var $extension_names = []
	var $rank = 0
	for node in value.extensions {
		name = node.name().map_err(|_| "transaction extensions must be elements")?
		if ["TRNTYPE", "DTPOSTED", "TRNAMT", "FITID", "NAME", "MEMO", "CHECKNUM"].contains(name) or $extension_names.contains(name) {
			return Err("duplicate transaction extension or core-field collision")
		}
		$extension_names = $extension_names.append(name)
		if field_rank(name) < 999 {
			if field_rank(name) < $rank {
				return Err("OFX extension fields out of order")
			}
			$rank = field_rank(name)
		}
	}
	if ![value.type, value.posted, value.amount, value.id, value.name, value.memo, value.check_number].all(valid_text) {
		return Err("invalid XML scalar in transaction")
	}
	if !["CREDIT", "DEBIT", "INT", "DIV", "FEE", "SRVCHG", "DEP", "ATM", "POS", "XFER", "CHECK", "PAYMENT", "CASH", "DIRECTDEP", "DIRECTDEBIT", "REPEATPMT", "HOLD", "OTHER"].contains(value.type) {
		return Err("invalid OFX transaction type")
	}
	if !timestamp(value.posted) {
		return Err("invalid OFX posted timestamp")
	}
	if !decimal(value.amount) {
		return Err("invalid OFX transaction amount")
	}
	if value.id.is_empty() {
		return Err("missing OFX transaction identifier")
	}
	Ok({})
}

balance_from : Xml.Node -> Try(OFX.Balance, Str)
balance_from = |node| {
	unique(node)?
	scalar_fields(node, ["BALAMT", "DTASOF"])?
	value = { amount: field(node, "BALAMT"), as_of: field(node, "DTASOF") }
	if !decimal(value.amount) or !timestamp(value.as_of) {
		return Err("invalid OFX balance")
	}
	Ok(value)
}

digits : Str -> Bool
digits = |text| !text.is_empty() and text.to_utf8().all(|b| b >= '0' and b <= '9')

decimal : Str -> Bool
decimal = |text| {
	bytes = text.to_utf8()
	if bytes.len() > 128 {
		return False
	}
	var $at = if (bytes.first() == Ok('-') or bytes.first() == Ok('+')) 1 else 0
	var $digits = 0.U64
	var $point = False
	while $at < bytes.len() {
		byte = bytes.get($at) ?? 0
		if byte >= '0' and byte <= '9' {
			$digits = $digits + 1
		} else if byte == '.' and !$point and $digits > 0 {
			$point = True
			$digits = 0
		} else {
			return False
		}
		$at = $at + 1
	}
	$digits > 0
}

timestamp : Str -> Bool
timestamp = |text| {
	parts = text.split_on("[")
	if parts.len() > 2 {
		return False
	}
	base = (parts.first() ?? "").split_on(".")
	date = base.first() ?? ""
	len = date.count_utf8_bytes()
	if ![8, 10, 12, 14].contains(len) or !digits(date) or base.len() > 2 {
		return False
	}
	if base.len() == 2 and (len != 14 or !digits(base.last() ?? "")) {
		return False
	}
	bytes = date.to_utf8()
	num = |at, count| {
		var $value = 0.U64
		var $pos = at
		while $pos < at + count {
			$value = $value * 10 + ((bytes.get($pos) ?? '0') - '0').to_u64()
			$pos = $pos + 1
		}
		$value
	}
	year = num(0, 4)
	month = num(4, 2)
	day = num(6, 2)
	if year == 0 or month == 0 or month > 12 {
		return False
	}
	leap = year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
	maxday = [31, if (leap) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].get(month - 1) ?? 0
	if day == 0 or day > maxday or (len >= 10 and num(8, 2) > 23) or (len >= 12 and num(10, 2) > 59) or (len >= 14 and num(12, 2) > 60) {
		return False
	}
	if parts.len() == 2 {
		zone = parts.last() ?? ""
		if !zone.ends_with("]") {
			return False
		}
		interior = Str.from_utf8_lossy(zone.to_utf8().drop_last(1))
		if interior.contains("[") or interior.contains("]") or interior.split_on(":").len() > 2 {
			return False
		}
		offset = interior.split_on(":").first() ?? ""
		if !decimal(offset) {
			return False
		}
		amount = F64.from_str(offset) ?? 99
		if amount < -12 or amount > 14 {
			return False
		}
	}
	True
}

escape : Str -> Str
escape = |value| value.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;")

scalar : Str, Str, OFX.Syntax -> Str
scalar = |name, value, syntax| if (value.is_empty()) "" else "<${name}>${escape(value)}${if (syntax == Xml203) "</${name}>" else ""}\n"

node_xml : Xml.Node, OFX.Syntax, U64 -> Try(Str, Str)
node_xml = |node, syntax, depth| match node {
	Text(value) => if (valid_text(value)) Ok(escape(value)) else Err("invalid XML text in extension")
	Element(element) => {
		if depth >= 64 {
			return Err("OFX extension exceeds maximum writer depth")
		}
		if !valid_name(element.name) {
			return Err("invalid extension tag name")
		}
		if !element.attributes.is_empty() {
			return Err("OFX extension attributes are unsupported")
		}
		if syntax == Sgml102 and !aggregate_tag(element.name) and !leaf_tag(element.name) {
			return Err("unknown SGML extension cannot be written")
		}
		var $body = ""
		for child in element.children {
			$body = Str.concat($body, node_xml(child, syntax, depth + 1)?)
		}
		Ok("<${element.name}>${$body}${if (syntax == Sgml102 and leaf_tag(element.name)) "\n" else "</${element.name}>"}")
	}
}

preamble : OFX.WriteProps, OFX.Header -> Str
preamble = |props, header| {
	if props.syntax == Xml203 {
		"<?xml version=\"1.0\" encoding=\"${if (props.encoding == Utf8) "UTF-8" else "windows-1252"}\"?>\n<?OFX OFXHEADER=\"200\" VERSION=\"203\" SECURITY=\"NONE\" OLDFILEUID=\"${escape(header.old_uid)}\" NEWFILEUID=\"${escape(header.new_uid)}\"?>\n"
	}
	else {
		"OFXHEADER:100\nDATA:OFXSGML\nVERSION:102\nSECURITY:NONE\nENCODING:${if (props.encoding == Utf8) "UTF-8" else "USASCII"}\nCHARSET:${if (props.encoding == Utf8) "NONE" else "1252"}\nCOMPRESSION:NONE\nOLDFILEUID:${header.old_uid}\nNEWFILEUID:${header.new_uid}\n\n"
	}
}

encode_text : Str, Streaming.TextEncoding -> Try(List(U8), OFX.Error)
encode_text = |text, encoding| {
	if encoding == Utf8 {
		return Ok(text.to_utf8())
	}
	var $out = []
	var $bytes = text.to_utf8()
	while !$bytes.is_empty() {
		first = $bytes.first() ?? 0
		if first < 128 {
			$out = $out.append(first)
			$bytes = $bytes.drop_first(1)
		} else {
			width = if (first < 224) 2 else if (first < 240) 3 else 4
			encoded = $bytes.take_first(width)
			var $found = False
			var $candidate = 128.U64
			while $candidate < 256 {
				match Streaming.decode_text([$candidate.to_u8_wrap()], Windows1252) {
					Ok(value) if value.to_utf8() == encoded => {
						$out = $out.append($candidate.to_u8_wrap())
						$found = True
						break
					}
					_ => {}
				}
				$candidate = $candidate + 1
			}
			if !$found {
				return Err(write_error("character cannot be encoded as Windows-1252"))
			}
			$bytes = $bytes.drop_first(width)
		}
	}
	Ok($out)
}

write_event : OFX.Writer, OFX.Event -> Try({ writer : OFX.Writer, bytes : List(U8) }, [InvalidOfx(OFX.Error)])
write_event = |writer, event| {
	var $next = writer
	var $text = ""
	syntax = writer.props.syntax
	match event {
		StatementStart(context) => {
			if writer.statement {
				return Err(InvalidOfx(write_error("nested writer statement")))
			}
			validate_context(context).map_err(|message| InvalidOfx(write_error(message)))?
			if writer.opened and writer.header != context.header {
				return Err(InvalidOfx(write_error("statement header differs from the open document")))
			}
			if !writer.opened {
				header = context.header
				$text = "${preamble(writer.props, header)}<OFX><SIGNONMSGSRSV1><SONRS><STATUS>${scalar("CODE", "0", syntax)}${scalar("SEVERITY", "INFO", syntax)}</STATUS>${scalar("DTSERVER", header.server_date, syntax)}${scalar("LANGUAGE", header.language, syntax)}${if (header.org.is_empty() and header.fid.is_empty()) "" else "<FI>${scalar("ORG", header.org, syntax)}${scalar("FID", header.fid, syntax)}</FI>"}</SONRS></SIGNONMSGSRSV1>"
			}
			kind = context.kind
			group = if (kind == Bank) "BANKMSGSRSV1" else "CREDITCARDMSGSRSV1"
			wrap = if (kind == Bank) "STMTTRNRS" else "CCSTMTTRNRS"
			stmt = if (kind == Bank) "STMTRS" else "CCSTMTRS"
			acct = if (kind == Bank) "BANKACCTFROM" else "CCACCTFROM"
			account = context.account
			$text = Str.concat($text, "<${group}><${wrap}>${scalar("TRNUID", context.transaction_id, syntax)}<STATUS>${scalar("CODE", context.status.code, syntax)}${scalar("SEVERITY", context.status.severity, syntax)}${scalar("MESSAGE", context.status.message, syntax)}</STATUS><${stmt}>${scalar("CURDEF", context.currency, syntax)}<${acct}>${scalar("BANKID", account.bank_id, syntax)}${scalar("BRANCHID", account.branch_id, syntax)}${scalar("ACCTID", account.account_id, syntax)}${scalar("ACCTTYPE", account.account_type, syntax)}${scalar("ACCTKEY", account.key, syntax)}</${acct}><BANKTRANLIST>${scalar("DTSTART", context.start, syntax)}${scalar("DTEND", context.end, syntax)}")
			$next = { ..writer, opened: True, statement: True, list_open: True, kind, balances: [], header: context.header }
		}
		Transaction(value) => {
			if !writer.statement or !writer.list_open {
				return Err(InvalidOfx(write_error("transaction outside writer transaction list")))
			}
			validate_transaction(value).map_err(|message| InvalidOfx(write_error(message)))?
			$text = Str.concat($text, transaction_text(value, syntax).map_err(|message| InvalidOfx(write_error(message)))?)
		}
		LedgerBalance(balance) | AvailableBalance(balance) => {
			if !writer.statement {
				return Err(InvalidOfx(write_error("balance outside writer statement")))
			}
			if !decimal(balance.amount) or !timestamp(balance.as_of) {
				return Err(InvalidOfx(write_error("invalid writer balance")))
			}
			name = match event {
				LedgerBalance(_) => "LEDGERBAL"
				_ => "AVAILBAL"
			}
			if writer.balances.contains(name) or (name == "LEDGERBAL" and writer.balances.contains("AVAILBAL")) {
				return Err(InvalidOfx(write_error("duplicate or out-of-order writer balance")))
			}
			$text = Str.concat($text, "${if (writer.list_open) "</BANKTRANLIST>" else ""}<${name}>${scalar("BALAMT", balance.amount, syntax)}${scalar("DTASOF", balance.as_of, syntax)}</${name}>\n")
			$next = { ..writer, list_open: False, balances: writer.balances.append(name) }
		}
		StatementEnd => {
			if !writer.statement {
				return Err(InvalidOfx(write_error("writer statement already ended")))
			}
			group = if (writer.kind == Bank) "BANKMSGSRSV1" else "CREDITCARDMSGSRSV1"
			wrap = if (writer.kind == Bank) "STMTTRNRS" else "CCSTMTTRNRS"
			stmt = if (writer.kind == Bank) "STMTRS" else "CCSTMTRS"
			$text = Str.concat($text, "${if (writer.list_open) "</BANKTRANLIST>" else ""}</${stmt}></${wrap}></${group}>\n")
			$next = { ..writer, statement: False, list_open: False }
		}
	}
	bytes = encode_text($text, writer.props.encoding).map_err(|error| InvalidOfx(error))?
	if bytes.len() > writer.props.max_record_bytes {
		return Err(InvalidOfx(write_error("encoded record exceeds max_record_bytes")))
	}
	Ok({ writer: $next, bytes })
}

sample_xml : Str
sample_xml = "<OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>012</BANKID><ACCTID>000123</ACCTID><ACCTTYPE>CHECKING</ACCTTYPE></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND><STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260103120000[-5:EST]</DTPOSTED><TRNAMT>-12.30</TRNAMT><FITID>00001</FITID><NAME>Café &amp; tea</NAME><MEMO>One</MEMO></STMTTRN></BANKTRANLIST><LEDGERBAL><BALAMT>100.00</BALAMT><DTASOF>20260201</DTASOF></LEDGERBAL></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>"

expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	value = statement.transactions.first()?
	statement.context.account.account_id == "000123" and value.amount == "-12.30" and value.id == "00001" and value.name == "Café & tea" and value.posted == "20260103120000[-5:EST]"
}

expect {
	document = OFX.parse_str(sample_xml)?
	xml = OFX.to_bytes(document, {})?
	sgml = OFX.to_bytes(document, { syntax: Sgml102 })?
	OFX.parse_bytes(xml, {}) == Ok(document) and OFX.parse_bytes(sgml, {}) == Ok(document)
}

expect {
	document = OFX.parse_str(sample_xml)?
	encoded = OFX.to_bytes(document, { syntax: Sgml102, encoding: Windows1252 })?
	encoded.contains(233) and OFX.parse_bytes(encoded, {}) == Ok(document) and OFX.parse_bytes(encoded, { encoding: Utf8 }).is_err()
}

expect {
	bytes = sample_xml.to_utf8()
	expected = List.from_iter(OFX.parse_iter([bytes].iter(), {}))
	actual = List.from_iter(OFX.parse_iter(bytes.map(|byte| [byte]).iter(), {}))
	expected == actual and expected.last() == Ok(Ok(Finished({ emitted_items: 4, rejected_records: 0 })))
}

expect {
	invalid = sample_xml.replace_each("-12.30", "bad")
	reported = List.from_iter(OFX.parse_iter([invalid.to_utf8()].iter(), { errors: { error_policy: ReportInvalidRecords } }))
	skipped = List.from_iter(OFX.parse_iter([invalid.to_utf8()].iter(), { errors: { error_policy: SkipInvalidRecords } }))
	reported.len() == 5 and skipped.len() == 4 and reported.last() == Ok(Ok(Finished({ emitted_items: 3, rejected_records: 1 }))) and OFX.parse_str(invalid).is_err()
}

expect OFX.parse_bytes(sample_xml.to_utf8(), { max_depth: 3 }).is_err()
expect OFX.parse_bytes(sample_xml.to_utf8(), { max_token_bytes: 5 }).is_err()
expect OFX.parse_bytes(sample_xml.to_utf8(), { syntax: Sgml }).is_err()
expect OFX.parse_str(sample_xml.replace_each("</OFX>", "")).is_err()
expect OFX.parse_str(sample_xml.replace_each("<OFX>", "<!DOCTYPE OFX><OFX>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("20260103120000[-5:EST]", "20260230000000")).is_err()
expect decimal("-0.000001") and !decimal("NaN") and !decimal("1e3")
expect timestamp("20240229120000.123[5.5:IST]") and !timestamp("20230229")

valid_text : Str -> Bool
valid_text = |value| {
	# Str already guarantees UTF-8. XML 1.0 excludes ASCII controls other than
	# tab/newline/CR, and the two BMP noncharacters U+FFFE and U+FFFF.
	bytes = value.to_utf8()
	var $at = 0
	while $at < bytes.len() {
		byte = bytes.get($at) ?? 0
		if byte < 0x20 and byte != '\t' and byte != '\n' and byte != '\r' {
			return False
		}
		if byte == 0xEF and bytes.get($at + 1) == Ok(0xBF) and (bytes.get($at + 2) == Ok(0xBE) or bytes.get($at + 2) == Ok(0xBF)) {
			return False
		}
		$at = $at + 1
	}
	True
}

placement : Str, Str -> Bool
placement = |name, parent| match name {
	"STMTTRN" => parent == "BANKTRANLIST"
	"CODE" | "SEVERITY" | "MESSAGE" => parent == "STATUS"
	"TRNUID" | "CLTCOOKIE" => parent == "STMTTRNRS" or parent == "CCSTMTTRNRS"
	"STATUS" => parent == "STMTTRNRS" or parent == "CCSTMTTRNRS" or parent == "SONRS"
	"DTSERVER" | "LANGUAGE" | "USERKEY" => parent == "SONRS"
	"FI" => parent == "SONRS"
	"ORG" | "FID" => parent == "FI"
	"TRNTYPE" | "DTPOSTED" | "TRNAMT" | "FITID" | "CHECKNUM" | "MEMO" => parent == "STMTTRN"
	"BANKID" | "BRANCHID" => parent == "BANKACCTFROM" or parent == "BANKACCTTO"
	"ACCTID" | "ACCTTYPE" | "ACCTKEY" => ["BANKACCTFROM", "BANKACCTTO", "CCACCTFROM", "CCACCTTO"].contains(parent)
	"BALAMT" | "DTASOF" => parent == "LEDGERBAL" or parent == "AVAILBAL"
	"BANKTRANLIST" | "LEDGERBAL" | "AVAILBAL" | "CURDEF" => parent == "STMTRS" or parent == "CCSTMTRS"
	"BANKACCTFROM" => parent == "STMTRS"
	"CCACCTFROM" => parent == "CCSTMTRS"
	"DTSTART" | "DTEND" => parent == "BANKTRANLIST"
	"STMTRS" => parent == "STMTTRNRS"
	"CCSTMTRS" => parent == "CCSTMTTRNRS"
	"STMTTRNRS" => parent == "BANKMSGSRSV1"
	"CCSTMTTRNRS" => parent == "CREDITCARDMSGSRSV1"
	"BANKMSGSRSV1" | "CREDITCARDMSGSRSV1" | "SIGNONMSGSRSV1" => parent == "OFX"
	"SONRS" => parent == "SIGNONMSGSRSV1"
	_ => True
}

expect OFX.parse_str(sample_xml.replace_each("</BANKTRANLIST>", "</BANKTRANLIST><CURDEF>USD</CURDEF>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("</LEDGERBAL>", "</LEDGERBAL><LEDGERBAL><BALAMT>999</BALAMT><DTASOF>20260201</DTASOF></LEDGERBAL>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("<BANKTRANLIST>", "").replace_each("</BANKTRANLIST>", "")).is_err()
expect OFX.parse_str(sample_xml.replace_each("<OFX>", "<OFX><!-- bad -- comment -->")).is_err()
expect OFX.parse_bytes("\u(feff)${sample_xml}".to_utf8(), {}).is_ok()
expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	value = statement.transactions.first()?
	opened = OFX.write(OFX.writer({ encoding: Windows1252 }), StatementStart(statement.context))?
	OFX.write(opened.writer, Transaction({ ..value, name: "😀" })).is_err()
}

transaction_text : OFX.Transaction, OFX.Syntax -> Try(Str, Str)
transaction_text = |value, syntax| {
	var $body = "<STMTTRN>"
	var $extensions = value.extensions
	for pair in [("TRNTYPE", value.type), ("DTPOSTED", value.posted), ("TRNAMT", value.amount), ("FITID", value.id), ("CHECKNUM", value.check_number), ("NAME", value.name), ("MEMO", value.memo)] {
		while !$extensions.is_empty() {
			next_rank = $extensions.find_first(|node| field_rank(node.name() ?? "") < 999).map_ok(|node| field_rank(node.name() ?? "")) ?? 999
			if next_rank > field_rank(pair.0) {
				break
			}
			node = $extensions.first() ?? Text("")
			$body = Str.concat($body, node_xml(node, syntax, 0)?)
			$extensions = $extensions.drop_first(1)
		}
		$body = Str.concat($body, scalar(pair.0, pair.1, syntax))
	}
	for node in $extensions {
		$body = Str.concat($body, node_xml(node, syntax, 0)?)
	}
	Ok("${$body}</STMTTRN>\n")
}

field_rank : Str -> U64
field_rank = |name| ["TRNTYPE", "DTPOSTED", "DTUSER", "DTAVAIL", "TRNAMT", "FITID", "CORRECTFITID", "CORRECTACTION", "SRVRTID", "CHECKNUM", "REFNUM", "SIC", "PAYEEID", "NAME", "PAYEE", "EXTDNAME", "BANKACCTTO", "CCACCTTO", "MEMO", "IMAGEDATA", "CURRENCY", "ORIGCURRENCY", "INV401KSOURCE"].find_first_index(|known| known == name) ?? 999

expect OFX.parse_str(sample_xml.replace_each("<BANKMSGSRSV1>", "<BANKMSGSRSV1><CURDEF>USD</CURDEF>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("</OFX>", "<TAXMSGSRSV1></TAXMSGSRSV1></OFX>")).is_err()
expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	value = statement.transactions.first()?
	opened = OFX.write(OFX.writer({}), StatementStart(statement.context))?
	collision = Element({ name: "FITID", attributes: [], children: [Text("duplicate")] })
	OFX.write(opened.writer, Transaction({ ..value, extensions: [collision] })).is_err()
}

expect {
	xml = "<?xml version=\"1.0\" encoding=\"windows-1252\"?>${sample_xml.replace_each("Café &amp; tea", "<![CDATA[Café & tea]]>")}"
	encoded = encode_text(xml, Windows1252)?
	OFX.parse_bytes(encoded, {}).map_ok(|document| document.statements.first().map_ok(|statement| statement.transactions.first().map_ok(|value| value.name))) == Ok(Ok(Ok("Café & tea")))
}
expect {
	body = sample_xml.split_on("<STMTTRN>").last() ?? ""
	record = "<STMTTRN>${body.split_on("</STMTTRN>").first() ?? ""}</STMTTRN>"
	OFX.parse_bytes(sample_xml.to_utf8(), { max_record_bytes: record.count_utf8_bytes() - 1 }).is_err()
}
expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	value = statement.transactions.first()?
	opened = OFX.write(OFX.writer({}), StatementStart(statement.context))?
	nul = Element({ name: "VENDOR", attributes: [], children: [Text("\u(0)")] })
	OFX.write(opened.writer, Transaction({ ..value, extensions: [nul] })).is_err()
}
expect {
	document = OFX.parse_str(sample_xml)?
	context = document.statements.first()?.context
	OFX.write(OFX.writer({ syntax: Sgml102 }), StatementStart({ ..context, header: { ..context.header, old_uid: "a:b" } })).is_err() and OFX.write(OFX.writer({ syntax: Sgml102 }), StatementStart({ ..context, header: { ..context.header, old_uid: "a<b" } })).is_err()
}
expect {
	bytes = "\u(feff)${sample_xml}".to_utf8()
	List.from_iter(OFX.parse_iter([bytes].iter(), {})) == List.from_iter(OFX.parse_iter(bytes.map(|byte| [byte]).iter(), {}))
}

expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	opened = OFX.write(OFX.writer({}), StatementStart(statement.context))?
	balance = statement.ledger_balance?
	once = OFX.write(opened.writer, LedgerBalance(balance))?
	OFX.write(once.writer, LedgerBalance(balance)).is_err()
}
expect !timestamp("20260101[5:EST:extra]") and !timestamp("20260101[5:EST]extra]")

expect OFX.parse_str("<?xml version=\"1.0\"?><?xml version=\"1.0\"?>${sample_xml}").is_err()
expect OFX.parse_str(" <?xml version=\"1.0\"?>${sample_xml}").is_err()

valid_name : Str -> Bool
valid_name = |name| {
	first = name.to_utf8().first() ?? 0
	((first >= 'A' and first <= 'Z') or first == '_' or first == ':') and name.to_utf8().all(|byte| (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '.' or byte == '_' or byte == ':')
}
expect {
	document = OFX.parse_str(sample_xml)?
	statement = document.statements.first()?
	value = statement.transactions.first()?
	opened = OFX.write(OFX.writer({}), StatementStart(statement.context))?
	["1EXT", ".EXT"].all(|name| OFX.write(opened.writer, Transaction({ ..value, extensions: [Element({ name, attributes: [], children: [Text("x")] })] })).is_err())
}
expect OFX.parse_str(sample_xml.replace_each("<OFX>", "<OFX><1EXT></1EXT>")).is_err()

aggregate_whitespace : Xml.Node -> Bool
aggregate_whitespace = |node| match node {
	Text(_) => False
	Element(element) => element.attributes.is_empty() and element.children.all(
		|child| match child {
			Text(value) => value.trim().is_empty()
			Element(_) => True
		},
	)
}

scalar_fields : Xml.Node, List(Str) -> Try({}, Str)
scalar_fields = |node, allowed| {
	if !aggregate_whitespace(node) {
		return Err("unexpected text or attributes in OFX aggregate")
	}
	for child in children(node) {
		if !allowed.contains(child.name() ?? "") or !children(child).is_empty() {
			return Err("unsupported OFX account or balance field")
		}
		match child {
			Element(element) if !element.attributes.is_empty() => return Err("unsupported field attributes")
			_ => {}
		}
	}
	Ok({})
}
expect OFX.parse_str(sample_xml.replace_each("</BANKACCTFROM>", "<EXTRA>x</EXTRA></BANKACCTFROM>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("</LEDGERBAL>", "<EXTRA>x</EXTRA></LEDGERBAL>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("<STMTTRN>", "<STMTTRN>discarded")).is_err()

xml_encoding : Str -> Try([Utf8, Windows1252, Ascii], Str)
xml_encoding = |text| {
	xml = Xml.parse_str("${text}<v/>").map_err(|_| "malformed XML declaration")?
	match xml.declaration {
		Err(_) => Err("missing XML declaration")
		Ok(declaration) => match declaration.encoding {
			Err(_) | Ok(Utf8Encoding) => Ok(Utf8)
			Ok(OtherEncoding(value)) => match lower(value) {
				"windows-1252" => Ok(Windows1252)
				"us-ascii" => Ok(Ascii)
				_ => Err("unsupported XML encoding")
			}
		}
	}
}
expect OFX.parse_bytes("<?xml version=\"1.0\" encoding=\"UTF-8\"?>${sample_xml}".to_utf8(), { encoding: Windows1252 }).is_err()
expect OFX.parse_str("<?xml version=\"1.0\" encoding=\"invalid-utf-8\"?>${sample_xml}").is_err()

expect OFX.parse_str(sample_xml.replace_each("Café &amp; tea", "<![CDATA[  padded  ]]>")).is_err()
expect OFX.parse_str(sample_xml.replace_each("<MEMO>One</MEMO>", "<MEMO><![CDATA[  padded  ]]></MEMO>")).is_err()
expect {
	document = OFX.parse_str(sample_xml.replace_each("Café &amp; tea", "  ordinary  "))?
	document.statements.first()?.transactions.first()?.name == "ordinary"
}

expect ["", "\t\r\n", "plain & < > ]]> text", "Café 日本", "\u(20)\u(D7FF)\u(E000)\u(FFFD)", "\u(10000)\u(10FFFF)", "\u(0)", "\u(8)", "\u(B)", "\u(C)", "\u(1F)", "\u(FFFE)", "\u(FFFF)"].all(|value| valid_text(value) == Xml.parse_str("<v>${escape(value)}</v>").is_ok())
expect ["0", "+0", "-1", "123456789012345678901234567890.0001", "0.00"].all(decimal)
expect ["", "+", "-", ".5", "1.", "1.2.3", "1e2", " 1", "1 ", "1\n"].all(|value| !decimal(value))

expect ["", "plain", "\r", "\n", "\r\n", "a\r\nb\rc\nd", "\n\r\n\r"].all(
	|text| {
		bytes = text.to_utf8()
		start = move(initial({}), '\r')
		bulk = move_span(start, bytes, 0, bytes.len())
		individual = bytes.fold(start, move)
		{ offset: bulk.offset, line: bulk.line, column: bulk.column, last_cr: bulk.last_cr } == { offset: individual.offset, line: individual.line, column: individual.column, last_cr: individual.last_cr }
	},
)

expect {
	bytes = sample_xml.replace_each(">", ">\r\n").replace_each("-12.30", "bad").to_utf8()
	List.from_iter(OFX.parse_iter([bytes].iter(), {})) == List.from_iter(OFX.parse_iter(bytes.map(|byte| [byte]).iter(), {}))
}

expect {
	source = sample_xml.replace_each("Café &amp; tea", "<![CDATA[a>b]]>").replace_each("<MEMO>", "<!-- > ' \" -->\r\n<MEMO>")
	bytes = source.to_utf8()
	List.from_iter(OFX.parse_iter([bytes].iter(), {})) == List.from_iter(OFX.parse_iter(bytes.map(|byte| [byte]).iter(), {})) and OFX.parse_str(source)?.statements.first()?.transactions.first()?.name == "a>b"
}

expect {
	bytes = "\r\n${sample_xml}".to_utf8()
	[0, 1, 3, 7, 8, 16].all(|limit| List.from_iter(OFX.parse_iter([bytes].iter(), { max_token_bytes: limit })) == List.from_iter(OFX.parse_iter(bytes.map(|byte| [byte]).iter(), { max_token_bytes: limit })))
}

expect {
	bytes = sample_xml.to_utf8()
	body = Str.from_utf8(bytes.sublist({ start: 5, len: bytes.len() - 11 }))?
	second = body.replace_each("<TRNUID>0", "<TRNUID>2").replace_each("<CURDEF>AUD", "<CURDEF>USD").replace_each("<ACCTID>000123", "<ACCTID>000456")
	document = OFX.parse_str("<OFX>${body}${second}</OFX>")?
	first = document.statements.first()?.context
	last = document.statements.last()?.context
	document.statements.len() == 2 and first.account.account_id == "000123" and first.currency == "AUD" and first.transaction_id == "0" and last.account.account_id == "000456" and last.currency == "USD" and last.transaction_id == "2"
}

expect {
	failed = OFX.step(OFX.decoder({}), "<OFX></BAD>".to_utf8(), Final)
	match failed.outcome {
		Failed(error) => {
			after = OFX.step(failed.state, "unread".to_utf8(), Final)
			error.offset == 11 and error.line == 1 and error.column == 12 and error.message == "mismatched OFX closing tag" and after.consumed == 0 and after.outcome == Done
		}
		_ => False
	}
}

expect ["</>", "<OFX></>", "<OFX></OFX/>", "<OFX></ />", "<OFX><//>", "<OFX><>", "<?OFX?>", "<?OFX ?>", "<?xml?>", "<?xml ?>", "<![CDATA[>", "<![CDATA[]]>", "<OFX><![CDATA[]]></OFX>"].all(|source| OFX.parse_str(source).is_err())

expect {
	source = sample_xml.replace_each("<MEMO>One</MEMO>", "<MEMO><![CDATA[]]></MEMO>")
	OFX.parse_str(source)?.statements.first()?.transactions.first()?.memo == ""
}
