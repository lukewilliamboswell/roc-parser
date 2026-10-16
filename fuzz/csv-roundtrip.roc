app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Parser
import parser.Utf8

## Round-trip property: fuzzer bytes choose a table of arbitrary UTF-8 fields
## (ragged rows allowed) *and* how to write it: which fields to quote beyond
## those RFC 4180 requires, the line break after each record (CRLF, LF or CR)
## and whether the last record has one. The parser must return exactly that
## table.
##
## The expected rows are the generated table itself, never the parser's
## output. Quoting follows RFC 4180 section 2: a field containing a comma, a
## double quote, CR or LF is enclosed in double quotes with inner quotes
## doubled. Two dialect rules (shared with Python's csv module) decide the
## remaining cases:
## - a quote is only special at the start of a field, so an unquoted field may
##   contain quotes elsewhere;
## - a blank line is one record with one empty field, and a final line break
##   is optional, so a final record that is a lone empty field is quoted when
##   no line break follows it.

Cur : { bytes : List(U8), pos : U64 }

Input : { csv : Str, expected : List(List(Str)) }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

alphabet : List(Str)
alphabet = ["a", "b", "Z", "1", " ", ",", "\"", "\"\"", "\n", "\r", "\r\n", "\t", "'", "\\", "é", "日", "😀", "\u(0)", "\u(7f)", "\u(feff)", "\u(85)", "\u(2028)", "=", "#", ";"]

gen_field : Cur -> { s : Str, cur : Cur }
gen_field = |start| {
	sized = pick(start, 7)
	var $cur = sized.cur
	var $text = ""
	var $index = 0
	while $index < sized.n {
		chosen = pick($cur, alphabet.len())
		$text = Str.concat($text, alphabet.get(chosen.n) ?? "a")
		$cur = chosen.cur
		$index = $index + 1
	}
	{ s: $text, cur: $cur }
}

needs_quotes : Str -> Bool
needs_quotes = |text| {
	bytes = text.to_utf8()
	bytes.contains(',') or bytes.contains('\r') or bytes.contains('\n') or bytes.first() == Ok('"')
}

quote : Str -> Str
quote = |text| "\"${Str.replace_each(text, "\"", "\"\"")}\""

generate : List(U8) -> Input
generate = |bytes| {
	start = { bytes, pos: 0 }
	row_count = pick(start, 7)
	final_break = pick(row_count.cur, 2)
	var $cur = final_break.cur
	var $rows = []
	var $text = ""
	var $previous_break = ""
	var $row_index = 0
	while $row_index < row_count.n {
		field_count = pick($cur, 5)
		$cur = field_count.cur
		var $fields = []
		var $cells = []
		var $field_index = 0
		while $field_index < field_count.n + 1 {
			field = gen_field($cur)
			force = pick(field.cur, 4)
			$cur = force.cur
			is_last_row = $row_index + 1 == row_count.n
			lone_empty_last = is_last_row and final_break.n == 0 and field_count.n == 0 and field.s == ""
			cell = if needs_quotes(field.s) or force.n == 0 or lone_empty_last quote(field.s) else field.s
			$fields = $fields.append(field.s)
			$cells = $cells.append(cell)
			$field_index = $field_index + 1
		}
		row_text = Str.join_with($cells, ",")
		break_choice = pick($cur, 3)
		$cur = break_choice.cur
		# CR then an empty line then LF would read as one CRLF break.
		line_break =
			match break_choice.n {
				0 => "\r\n"
				1 => if $previous_break == "\r" and row_text == "" "\r" else "\n"
				_ => "\r"
			}
		is_last = $row_index + 1 == row_count.n
		$text = Str.concat($text, row_text)
		if !is_last or final_break.n == 1 {
			$text = Str.concat($text, line_break)
		}
		$previous_break = line_break
		$rows = $rows.append($fields)
		$row_index = $row_index + 1
	}
	{ csv: $text, expected: $rows }
}

all_fields : Parser(CSV.Record, List(Str))
all_fields = Parser.many(CSV.field(CSV.string))

show_rows : List(List(Str)) -> Str
show_rows = |rows| Str.inspect(rows)

test : Input -> Fuzz.Outcome
test = |input| {
	match CSV.parse_with(all_fields, input.csv) {
		Ok(rows) if rows == input.expected => {}
		Ok(rows) => crash "round trip mismatch\n--- csv ---\n${Str.inspect(input.csv)}\n--- expected ---\n${show_rows(input.expected)}\n--- actual ---\n${show_rows(rows)}"
		Err(InvalidCsv(problem)) => crash "round trip rejected\n--- csv ---\n${Str.inspect(input.csv)}\n--- expected ---\n${show_rows(input.expected)}\n--- error ---\n${Str.inspect(problem)}"
	}
	match CSV.parse_records(input.csv) {
		Ok(records) if records.map(|fields| fields.map(Str.from_utf8_lossy)) == input.expected => {}
		_ => crash "parse_records disagrees with parse_with\n${Str.inspect(input.csv)}"
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "csv-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "csv: ${Str.inspect(input.csv)}\nexpected: ${show_rows(input.expected)}",
})
