app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Parser

## Arbitrary UTF-8 must parse or fail cleanly, and every accepted input obeys:
## - every record has at least one field;
## - re-serializing the records with every field quoted parses back to the
##   same records (quoting a field that does not need it is a no-op);
## - re-serializing with minimal quoting and LF, CRLF or CR breaks also
##   parses back to the same records (line break style is irrelevant);
## - appending a line break to input that does not already end in one
##   changes nothing (the final line break is optional);
## - joining two accepted inputs with a line break gives the concatenated
##   records (records are independent);
## - every record also parses alone through CSV.parse_str_to_csv_record.
##
## A three-byte input starting 0xFF instead selects a pathological input (long
## quoted fields full of doubled quotes, many tiny fields, many blank lines,
## an unterminated quote) up to ~75KB long; the campaign's --timeout
## bounds how long the parser may take on it.

Rows : List(List(List(U8)))

all_fields : Parser(CSV.CSVRecord, List(List(U8)))
all_fields = Parser.many(CSV.field(Parser.build_primitive_parser(|bytes| Ok({ val: bytes, input: [] }))))

parse : Str -> Try(Rows, [Rejected])
parse = |text| {
	match CSV.parse_str(all_fields, text) {
		Ok(rows) => Ok(rows)
		Err(SyntaxError(_)) => Err(Rejected)
		Err(ParsingFailure(message)) => crash "parse_str_to_csv reported a ParsingFailure, not a SyntaxError: ${message}\n${Str.inspect(text)}"
		Err(ParsingIncomplete(_)) => crash "all_fields left fields unread\n${Str.inspect(text)}"
	}
}

show_rows : Rows -> Str
show_rows = |rows| Str.inspect(rows.map(|row| row.map(|field| Str.from_utf8(field) ?? "<invalid utf8>")))

quote_bytes : List(U8) -> List(U8)
quote_bytes = |field| field.fold(['"'], |acc, b| if b == '"' acc.concat(['"', '"']) else acc.append(b)).append('"')

needs_quotes : List(U8) -> Bool
needs_quotes = |field| field.contains(',') or field.contains('\r') or field.contains('\n') or field.first() == Ok('"')

## Serialize records. `always` quotes every field; otherwise only those that
## need it, plus a lone empty field (which would otherwise be a blank line
## that a neighbouring break could absorb or a final break could hide).
serialize : Rows, Bool, List(U8) -> Str
serialize = |rows, always, line_break| {
	lines = rows.map(|row| {
		lone_empty = row == [[]]
		cells = row.map(|field| if always or lone_empty or needs_quotes(field) quote_bytes(field) else field)
		join_bytes(cells, [','])
	})
	joined = lines.fold([], |acc, line| acc.concat(line).concat(line_break))
	Str.from_utf8(joined) ?? crash "serialize produced invalid UTF-8"
}

join_bytes : List(List(U8)), List(U8) -> List(U8)
join_bytes = |parts, separator| {
	var $out = []
	var $index = 0
	while $index < parts.len() {
		if $index > 0 {
			$out = $out.concat(separator)
		}
		$out = $out.concat(parts.get($index) ?? [])
		$index = $index + 1
	}
	$out
}

repeat : List(U8), U64 -> List(U8)
repeat = |chunk, count| {
	var $out = List.with_capacity(chunk.len() * count)
	var $index = 0
	while $index < count {
		$out = $out.concat(chunk)
		$index = $index + 1
	}
	$out
}

ends_in_break : Str -> Bool
ends_in_break = |text| {
	last = text.to_utf8().last() ?? 0
	last == '\n' or last == '\r'
}

check_relations : Str, Rows -> {}
check_relations = |input, rows| {
	if rows.any(|row| row.is_empty()) {
		crash "a record with no fields\n${Str.inspect(input)}"
	}
	expect_same = |label, text| {
		match parse(text) {
			Ok(again) if again == rows => {}
			Ok(again) => crash "${label} changed the records\n--- input ---\n${Str.inspect(input)}\n--- records ---\n${show_rows(rows)}\n--- variant ---\n${Str.inspect(text)}\n--- variant records ---\n${show_rows(again)}"
			Err(_) => crash "${label} was rejected\n--- input ---\n${Str.inspect(input)}\n--- records ---\n${show_rows(rows)}\n--- variant ---\n${Str.inspect(text)}"
		}
	}
	expect_same("quoting every field", serialize(rows, Bool.True, ['\r', '\n']))
	expect_same("minimal quoting with LF", serialize(rows, Bool.False, ['\n']))
	expect_same("minimal quoting with CRLF", serialize(rows, Bool.False, ['\r', '\n']))
	expect_same("minimal quoting with CR", serialize(rows, Bool.False, ['\r']))
	if input != "" and !ends_in_break(input) {
		expect_same("appending CRLF", Str.concat(input, "\r\n"))
		expect_same("appending LF", Str.concat(input, "\n"))
	}
	{}
}

check_records_alone : Str, Rows -> {}
check_records_alone = |input, rows| {
	_ = rows.map(|row| {
		lone = serialize([row], Bool.False, [])
		match CSV.parse_str_to_csv_record(lone) {
			Ok(fields) if fields == row => {}
			_ => crash "parse_str_to_csv_record disagrees on one record\n--- input ---\n${Str.inspect(input)}\n--- record ---\n${Str.inspect(lone)}"
		}
	})
	{}
}

pathological : List(U8) -> Str
pathological = |bytes| {
	kind = (bytes.get(1) ?? 0) % 6
	count = 2000 + U8.to_u64(bytes.get(2) ?? 0) * 40
	chunk =
		match kind {
			0 => ['"', '"']
			1 => [',']
			2 => ['\n']
			3 => ['a', '"', 'b']
			4 => ['\r', '\n', '"', 'x', '"', ',']
			_ => "é".to_utf8()
		}
	body = repeat(chunk, count)
	text =
		match kind {
			0 => ['"'].concat(body).append('"')
			5 => ['"'].concat(body)
			_ => body
		}
	Str.from_utf8(text) ?? ""
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	if bytes.len() == 3 and bytes.first() == Ok(0xFF) {
		_ = parse(pathological(bytes))
		Fuzz.keep
	} else {
		match Str.from_utf8(bytes) {
			Err(_) => Fuzz.reject
			Ok(input) => {
				match parse(input) {
					Err(Rejected) => {}
					Ok(rows) => {
						check_relations(input, rows)
						check_records_alone(input, rows)
						split_join(input, rows)
					}
				}
				Fuzz.keep
			}
		}
	}
}

## Split the input at its middle line break and check both halves join back.
split_join : Str, Rows -> {}
split_join = |input, rows| {
	bytes = input.to_utf8()
	match bytes.find_first_index(|b| b == '\n') {
		Ok(at) if at > 0 and bytes.get(at - 1) != Ok('\r') and at + 1 < bytes.len() => {
			before = Str.from_utf8(bytes.sublist({ start: 0, len: at })) ?? ""
			after = Str.from_utf8(bytes.sublist({ start: at + 1, len: bytes.len() - at - 1 })) ?? ""
			match (parse(before), parse(after)) {
				(Ok(first), Ok(second)) if first.concat(second) != rows =>
					crash "records are not independent\n--- input ---\n${Str.inspect(input)}\n--- whole ---\n${show_rows(rows)}\n--- halves ---\n${show_rows(first)}\n${show_rows(second)}"
				_ => {}
			}
		}
		_ => {}
	}
}

target = Fuzz.target_with({
	name: "csv-raw",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| if bytes.len() == 3 and bytes.first() == Ok(0xFF) "pathological ${Str.inspect(bytes.sublist({ start: 0, len: 3 }))}" else Str.inspect(Str.from_utf8(bytes) ?? "<invalid utf8>"),
})
