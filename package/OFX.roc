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
	Context : { kind : [Bank, CreditCard], currency : Str, account : Account, start : Str, end : Str }

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

	## Collect a complete encoded document with encounter-order fail-fast errors.
	parse_bytes : List(U8), Props -> Try(OFX, [InvalidOfx(Error)])
	parse_bytes = |bytes, props| {
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
	validate = |props| parse_bytes(props.source.to_utf8(), props.options).map_err(|InvalidOfx(error)| Literal.error({ name: props.name, format: "OFX", line: error.line, column: error.column, message: error.message }))

	## Checked literal conversion.
	from_quote : Str -> Try(OFX, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## Stateful writer: validates complete logical items before returning bytes.
	Writer : WriteState

	## Begin a writer. Call write for each item and finish after StatementEnd.
	writer : WriteProps -> Writer
	writer = |props| { props, opened: False, statement: False, list_open: False, kind: Bank }

	## Validate and encode one complete item atomically.
	write : Writer, Event -> Try({ writer : Writer, bytes : List(U8) }, [InvalidOfx(Error)])
	write = write_event

	## Close a successfully completed document.
	finish : Writer -> Try(List(U8), [InvalidOfx(Error)])
	finish = |state| {
		if state.statement {
			return Err(InvalidOfx(write_error("unfinished OFX statement")))
		}
		encode_text(if (state.opened) "</OFX>\n" else "${preamble(state.props)}<OFX></OFX>\n", state.props.encoding).map_err(|error| InvalidOfx(error))
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

State : { props : OFX.Props, offset : U64, line : U64, column : U64, last_cr : Bool, token : List(U8), token_mode : Bool, quote : U8, header : List(U8), header_done : Bool, syntax : OFX.Syntax, encoding : [Utf8, Windows1252, Ascii], stack : List(Frame), capture : List(U8), capture_tag : Str, capture_depth : U64, leaf : List(U8), context : OFX.Context, started : Bool, in_statement : Bool, root_seen : Bool, root_closed : Bool, terminal : Bool }

WriteState : { props : OFX.WriteProps, opened : Bool, statement : Bool, list_open : Bool, kind : [Bank, CreditCard] }

empty_account : OFX.Account
empty_account = { bank_id: "", branch_id: "", account_id: "", account_type: "", key: "" }

empty_context : OFX.Context
empty_context = { kind: Bank, currency: "", account: empty_account, start: "", end: "" }

empty_statement : OFX.Context -> OFX.Statement
empty_statement = |context| { context, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }

initial : OFX.Props -> State
initial = |props| { props, offset: 0, line: 1, column: 1, last_cr: False, token: [], token_mode: False, quote: 0, header: [], header_done: False, syntax: Xml203, encoding: Utf8, stack: [], capture: [], capture_tag: "", capture_depth: 0, leaf: [], context: empty_context, started: False, in_statement: False, root_seen: False, root_closed: False, terminal: False }

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

advance : State, List(U8), Streaming.InputEnd -> Streaming.Step(State, OFX.Event, OFX.Error)
advance = |state, bytes, ending| {
	if state.terminal {
		return { state, consumed: 0, outcome: Done }
	}
	var $s = state
	var $at = 0
	while $at < bytes.len() {
		byte = bytes.get($at) ?? 0
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
		} else if $s.token_mode {
			$s = { ..$s, token: $s.token.append(byte) }
			if $s.token.len() > $s.props.max_token_bytes {
				return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX token exceeds max_token_bytes")) }
			}
			if byte == '"' or byte == '\'' {
				$s = { ..$s, quote: if ($s.quote == 0) byte else if ($s.quote == byte) 0 else $s.quote }
			}
			if token_complete($s.token, $s.quote) {
				match consume_token($s) {
					Err(error) => return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(error) }
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
		} else if byte == '<' {
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
		} else {
			$s = { ..$s, leaf: $s.leaf.append(byte) }
			if $s.leaf.len() > $s.props.max_token_bytes {
				return { state: { ..$s, terminal: True }, consumed: $at, outcome: Failed(problem($s, "OFX field exceeds max_token_bytes")) }
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
		{ state: $s, consumed: $at, outcome: NeedInput }
	}
}

token_complete : List(U8), U8 -> Bool
token_complete = |bytes, quote| {
	if bytes.starts_with("<!--".to_utf8()) {
		bytes.ends_with("-->".to_utf8())
	}
	else if bytes.starts_with("<![CDATA[".to_utf8()) {
		bytes.ends_with("]]>".to_utf8())
	}
	else if bytes.starts_with("<?".to_utf8()) {
		bytes.ends_with("?>".to_utf8())
	}
	else {
		bytes.last() == Ok('>') and quote == 0
	}
}

lower : Str -> Str
lower = |text| Str.from_utf8_lossy(text.to_utf8().map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b))

read_header : State -> Try(State, OFX.Error)
read_header = |state| {
	text = Str.from_utf8(state.header).map_err(|_| problem(state, "OFX header must be ASCII"))?
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
			"OLDFILEUID" | "NEWFILEUID" => {}
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
	Ok({ ..state, header_done: True, syntax: Sgml102, encoding: $encoding, header: [] })
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
	context = state.context
	match top {
		"CURDEF" => Ok({ ..state, context: { ..context, currency: decoded } })
		"DTSTART" => Ok({ ..state, context: { ..context, start: decoded } })
		"DTEND" => Ok({ ..state, context: { ..context, end: decoded } })
		"CODE" => if decoded != "0" {
			Err(problem(state, "OFX response status is not success"))
		} else Ok(state)
		_ => Ok(state)
	}
}

Action : [Quiet, Emitted(OFX.Event), Rejected(OFX.Error)]

Consumed : { state : State, outcome : Action }

consume_token : State -> Try(Consumed, OFX.Error)
consume_token = |state| {
	text = Str.from_utf8(state.token).map_err(|_| problem(state, "non-ASCII OFX tag"))?
	if text.starts_with("<!DOCTYPE") or text.starts_with("<!ENTITY") {
		return Err(problem(state, "DTDs and external entities are unsupported"))
	}
	if text.starts_with("<!--") or text.starts_with("<![CDATA[") {
		if state.syntax == Sgml102 {
			return Err(problem(state, "SGML comments and CDATA are unsupported"))
		}
		if !state.capture_tag.is_empty() {
			return Ok({ state: { ..state, capture: state.capture.concat(state.token) }, outcome: Quiet })
		}
		if text.starts_with("<![CDATA[") {
			return Err(problem(state, "CDATA outside captured record is unsupported"))
		}
		return Ok({ state, outcome: Quiet })
	}
	if text.starts_with("<?") {
		if state.root_seen {
			return Err(problem(state, "processing instruction inside OFX is unsupported"))
		}
		low = lower(text)
		var $next = state
		if low.starts_with("<?xml ") {
			if low.contains("windows-1252") {
				$next = { ..state, encoding: Windows1252 }
			}
			else if low.contains("encoding") and !low.contains("utf-8") and !low.contains("us-ascii") {
				return Err(problem(state, "unsupported XML encoding"))
			}
			if low.contains("us-ascii") {
				$next = { ..$next, encoding: Ascii }
			}
			if (state.props.encoding == Utf8 and $next.encoding == Windows1252) or (state.props.encoding == Windows1252 and $next.encoding != Windows1252) {
				return Err(problem(state, "encoding override contradicts XML declaration"))
			}
		} else if !text.starts_with("<?OFX ") {
			return Err(problem(state, "unknown OFX processing instruction"))
		}
		return Ok({ state: $next, outcome: Quiet })
	}
	if text.starts_with("<!") {
		return Err(problem(state, "unsupported OFX declaration"))
	}
	closing = text.starts_with("</")
	self_closed = text.ends_with("/>")
	start = if (closing) 2 else 1
	end = text.count_utf8_bytes() - (if (self_closed) 2 else 1)
	name = Str.from_utf8_lossy(state.token.sublist({ start, len: end - start })).trim()
	if name.is_empty() or !name.to_utf8().all(|b| (b >= 'A' and b <= 'Z') or (b >= '0' and b <= '9') or b == '.' or b == '_' or b == ':') {
		return Err(problem(state, "OFX tag names must be uppercase without attributes"))
	}
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
		return Err(problem($s, "content after OFX root"))
	}
	if !$s.root_seen {
		if name != "OFX" {
			return Err(problem($s, "expected OFX root"))
		}
		$s = { ..$s, root_seen: True }
	} else if $s.stack.is_empty() {
		return Err(problem($s, "multiple OFX roots"))
	}
	if name == "INVSTMTMSGSRSV1" or name == "INVSTMTRS" {
		return Err(problem($s, "investment OFX is unsupported"))
	}
	if $s.syntax == Sgml102 and !aggregate_tag(name) and !leaf_tag(name) {
		return Err(problem($s, "unknown SGML tag: closing behaviour is ambiguous"))
	}
	if $s.stack.len() >= $s.props.max_depth {
		return Err(problem($s, "OFX exceeds max_depth"))
	}
	if leaf_tag($s.stack.last().map_ok(|frame| frame.name) ?? "") {
		return Err(problem($s, "nested content in OFX scalar field"))
	}
	if name == "STMTRS" or name == "CCSTMTRS" {
		if $s.in_statement {
			return Err(problem($s, "nested OFX statements"))
		}
		$s = { ..$s, context: { ..empty_context, kind: if (name == "STMTRS") Bank else CreditCard }, started: False, in_statement: True }
	}
	if capture_tag(name) {
		if !$s.capture_tag.is_empty() {
			return Err(problem($s, "nested OFX record aggregate"))
		}
		$s = { ..$s, capture_tag: name, capture_depth: $s.stack.len(), capture: [] }
	}
	if !$s.capture_tag.is_empty() {
		$s = { ..$s, capture: $s.capture.concat(state.token) }
	}
	$s = { ..$s, stack: $s.stack.append({ name, offset: $s.offset - state.token.len() }) }
	if self_closed {
		return close_tag($s, name, "")
	}
	if name == "STMTTRN" or name == "LEDGERBAL" or name == "AVAILBAL" {
		if !$s.in_statement {
			return Err(problem($s, "OFX record outside a statement"))
		}
		if !$s.started {
			validate_context($s.context).map_err(|message| problem($s, message))?
			$s = { ..$s, started: True }
			return Ok({ state: $s, outcome: Emitted(StatementStart($s.context)) })
		}
	}
	Ok({ state: $s, outcome: Quiet })
}

close_tag : State, Str, Str -> Try(Consumed, OFX.Error)
close_tag = |state, name, text| {
	if state.stack.last().map_ok(|frame| frame.name) != Ok(name) {
		return Err(problem(state, "mismatched OFX closing tag"))
	}
	var $s = { ..state, stack: state.stack.drop_last(1) }
	if !$s.capture_tag.is_empty() {
		$s = { ..$s, capture: $s.capture.concat(text.to_utf8()) }
		if $s.stack.len() == $s.capture_depth {
			captured = decode($s, $s.capture)?
			root = Xml.parse_str(captured).map_err(|_| problem($s, "malformed OFX record"))?.root
			$s = { ..$s, capture: [], capture_tag: "" }
			if name == "STMTTRN" {
				if children(root).len() > $s.props.max_fields {
					return Err(problem($s, "OFX record exceeds max_fields"))
				}
				match transaction(root) {
					Ok(value) => return Ok({ state: $s, outcome: Emitted(Transaction(value)) })
					Err(message) => return Ok({ state: $s, outcome: Rejected(problem($s, message)) })
				}
			} else if name == "BANKACCTFROM" or name == "CCACCTFROM" {
				account = account_from(root).map_err(|message| problem($s, message))?
				$s = { ..$s, context: { ..$s.context, account } }
			} else {
				balance = balance_from(root).map_err(|message| problem($s, message))?
				return Ok({ state: $s, outcome: Emitted(if (name == "LEDGERBAL") LedgerBalance(balance) else AvailableBalance(balance)) })
			}
		}
	}
	if name == "BANKTRANLIST" and !$s.started {
		validate_context($s.context).map_err(|message| problem($s, message))?
		return Ok({ state: { ..$s, started: True }, outcome: Emitted(StatementStart($s.context)) })
	}
	if name == "STMTRS" or name == "CCSTMTRS" {
		if !$s.started {
			return Err(problem($s, "OFX statement has no transaction list or balances"))
		}
		return Ok({ state: { ..$s, in_statement: False }, outcome: Emitted(StatementEnd) })
	}
	if name == "OFX" {
		$s = { ..$s, root_closed: True }
	}
	Ok({ state: $s, outcome: Quiet })
}

capture_tag : Str -> Bool
capture_tag = |name| ["STMTTRN", "BANKACCTFROM", "CCACCTFROM", "LEDGERBAL", "AVAILBAL"].contains(name)

aggregate_tag : Str -> Bool
aggregate_tag = |name| ["OFX", "SIGNONMSGSRSV1", "SONRS", "STATUS", "FI", "BANKMSGSRSV1", "STMTTRNRS", "STMTRS", "BANKACCTFROM", "BANKTRANLIST", "STMTTRN", "LEDGERBAL", "AVAILBAL", "CREDITCARDMSGSRSV1", "CCSTMTTRNRS", "CCSTMTRS", "CCACCTFROM", "CURRENCY", "ORIGCURRENCY", "PAYEE", "BANKACCTTO", "CCACCTTO"].contains(name)

leaf_tag : Str -> Bool
leaf_tag = |name| ["CODE", "SEVERITY", "MESSAGE", "DTSERVER", "LANGUAGE", "ORG", "FID", "TRNUID", "CLTCOOKIE", "CURDEF", "BANKID", "BRANCHID", "ACCTID", "ACCTTYPE", "ACCTKEY", "DTSTART", "DTEND", "TRNTYPE", "DTPOSTED", "DTUSER", "DTAVAIL", "TRNAMT", "FITID", "CORRECTFITID", "CORRECTACTION", "SRVRTID", "CHECKNUM", "REFNUM", "NAME", "MEMO", "SIC", "PAYEEID", "BALAMT", "DTASOF", "CURRENCYRATE", "CURSYM", "ADDR1", "ADDR2", "ADDR3", "CITY", "STATE", "POSTALCODE", "COUNTRY", "PHONE", "USERKEY", "INTU.BID"].contains(name)

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
			return Err("duplicate OFX field ${name}")
		}
		$names = $names.append(name)
	}
	Ok({})
}

account_from : Xml.Node -> Try(OFX.Account, Str)
account_from = |node| {
	unique(node)?
	Ok({ bank_id: field(node, "BANKID"), branch_id: field(node, "BRANCHID"), account_id: field(node, "ACCTID"), account_type: field(node, "ACCTTYPE"), key: field(node, "ACCTKEY") })
}

transaction : Xml.Node -> Try(OFX.Transaction, Str)
transaction = |node| {
	unique(node)?
	value = { type: field(node, "TRNTYPE"), posted: field(node, "DTPOSTED"), amount: field(node, "TRNAMT"), id: field(node, "FITID"), name: field(node, "NAME"), memo: field(node, "MEMO"), check_number: field(node, "CHECKNUM"), extensions: children(node).keep_if(|child| !["TRNTYPE", "DTPOSTED", "TRNAMT", "FITID", "NAME", "MEMO", "CHECKNUM"].contains(child.name() ?? "")) }
	validate_transaction(value)?
	Ok(value)
}

validate_context : OFX.Context -> Try({}, Str)
validate_context = |context| {
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
	if text.count_utf8_bytes() > 128 {
		return False
	}
	unsigned = if (text.starts_with("-") or text.starts_with("+")) Str.from_utf8_lossy(text.to_utf8().drop_first(1)) else text
	parts = unsigned.split_on(".")
	(parts.len() == 1 or parts.len() == 2) and parts.all(digits)
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
	num = |at, count| U64.from_str(Str.from_utf8_lossy(bytes.sublist({ start: at, len: count }))) ?? 0
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
		offset = Str.from_utf8_lossy(zone.to_utf8().drop_last(1)).split_on(":").first() ?? ""
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

node_xml : Xml.Node, OFX.Syntax -> Try(Str, Str)
node_xml = |node, syntax| match node {
	Text(value) => Ok(escape(value))
	Element(element) => {
		if !element.attributes.is_empty() {
			return Err("OFX extension attributes are unsupported")
		}
		if syntax == Sgml102 and !aggregate_tag(element.name) and !leaf_tag(element.name) {
			return Err("unknown SGML extension cannot be written")
		}
		var $body = ""
		for child in element.children {
			$body = Str.concat($body, node_xml(child, syntax)?)
		}
		Ok("<${element.name}>${$body}${if (syntax == Sgml102 and leaf_tag(element.name)) "\n" else "</${element.name}>"}")
	}
}

preamble : OFX.WriteProps -> Str
preamble = |props| {
	if props.syntax == Xml203 {
		"<?xml version=\"1.0\" encoding=\"${if (props.encoding == Utf8) "UTF-8" else "windows-1252"}\"?>\n<?OFX OFXHEADER=\"200\" VERSION=\"203\" SECURITY=\"NONE\" OLDFILEUID=\"NONE\" NEWFILEUID=\"NONE\"?>\n"
	}
	else {
		"OFXHEADER:100\nDATA:OFXSGML\nVERSION:102\nSECURITY:NONE\nENCODING:${if (props.encoding == Utf8) "UTF-8" else "USASCII"}\nCHARSET:${if (props.encoding == Utf8) "NONE" else "1252"}\nCOMPRESSION:NONE\nOLDFILEUID:NONE\nNEWFILEUID:NONE\n\n"
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
	var $text = if (writer.opened) "" else "${preamble(writer.props)}<OFX>\n<SIGNONMSGSRSV1><SONRS><STATUS>${scalar("CODE", "0", writer.props.syntax)}${scalar("SEVERITY", "INFO", writer.props.syntax)}</STATUS>${scalar("DTSERVER", "20000101000000", writer.props.syntax)}${scalar("LANGUAGE", "ENG", writer.props.syntax)}</SONRS></SIGNONMSGSRSV1>\n"
	syntax = writer.props.syntax
	match event {
		StatementStart(context) => {
			if writer.statement {
				return Err(InvalidOfx(write_error("nested writer statement")))
			}
			validate_context(context).map_err(|message| InvalidOfx(write_error(message)))?
			kind = context.kind
			group = if (kind == Bank) "BANKMSGSRSV1" else "CREDITCARDMSGSRSV1"
			wrap = if (kind == Bank) "STMTTRNRS" else "CCSTMTTRNRS"
			stmt = if (kind == Bank) "STMTRS" else "CCSTMTRS"
			acct = if (kind == Bank) "BANKACCTFROM" else "CCACCTFROM"
			account = context.account
			$text = Str.concat($text, "<${group}><${wrap}>${scalar("TRNUID", "0", syntax)}<STATUS>${scalar("CODE", "0", syntax)}${scalar("SEVERITY", "INFO", syntax)}</STATUS><${stmt}>${scalar("CURDEF", context.currency, syntax)}<${acct}>${scalar("BANKID", account.bank_id, syntax)}${scalar("BRANCHID", account.branch_id, syntax)}${scalar("ACCTID", account.account_id, syntax)}${scalar("ACCTTYPE", account.account_type, syntax)}${scalar("ACCTKEY", account.key, syntax)}</${acct}><BANKTRANLIST>${scalar("DTSTART", context.start, syntax)}${scalar("DTEND", context.end, syntax)}")
			$next = { ..writer, opened: True, statement: True, list_open: True, kind }
		}
		Transaction(value) => {
			if !writer.statement or !writer.list_open {
				return Err(InvalidOfx(write_error("transaction outside writer transaction list")))
			}
			validate_transaction(value).map_err(|message| InvalidOfx(write_error(message)))?
			var $extensions = ""
			for node in value.extensions {
				$extensions = Str.concat($extensions, node_xml(node, syntax).map_err(|message| InvalidOfx(write_error(message)))?)
			}
			$text = Str.concat($text, "<STMTTRN>${scalar("TRNTYPE", value.type, syntax)}${scalar("DTPOSTED", value.posted, syntax)}${scalar("TRNAMT", value.amount, syntax)}${scalar("FITID", value.id, syntax)}${scalar("NAME", value.name, syntax)}${scalar("MEMO", value.memo, syntax)}${scalar("CHECKNUM", value.check_number, syntax)}${$extensions}</STMTTRN>\n")
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
			$text = Str.concat($text, "${if (writer.list_open) "</BANKTRANLIST>" else ""}<${name}>${scalar("BALAMT", balance.amount, syntax)}${scalar("DTASOF", balance.as_of, syntax)}</${name}>\n")
			$next = { ..writer, list_open: False }
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
sample_xml = "<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>012</BANKID><ACCTID>000123</ACCTID><ACCTTYPE>CHECKING</ACCTTYPE></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND><STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260103120000[-5:EST]</DTPOSTED><TRNAMT>-12.30</TRNAMT><FITID>00001</FITID><NAME>Café &amp; tea</NAME><MEMO>One</MEMO></STMTTRN></BANKTRANLIST><LEDGERBAL><BALAMT>100.00</BALAMT><DTASOF>20260201</DTASOF></LEDGERBAL></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>"

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
