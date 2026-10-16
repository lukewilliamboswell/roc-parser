app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Parser

## Typed decoding properties: fuzzer bytes choose rows of (U64, F64, Str)
## values, how each number is spelled, how each field is quoted, and possibly
## one defect per row (a malformed number, an extra field, a missing field).
##
## - Positional: CSV.parse_with with the record(...).keep(field(u64))...
##   decoder must return exactly those values, or fail on the first defective
##   row with an error naming its record and field.
## - By name (parser_for): the same table written with a header row, its
##   columns in a chosen order and possibly an extra column no field names,
##   must decode through CSV.parse into the `Row` record type to the same
##   values, or fail on the first defective row with its record and field.
##
## The number grammar is Rust's `u64::from_str` / `f64::from_str` (a mature,
## documented reference): U64 is `+?[0-9]+` within range; F64 is an optional
## sign then `inf`, `infinity` or `nan` (any case) or a decimal with optional
## fraction and exponent, rounding out-of-range magnitudes to infinity or
## zero. Roc literal extras (underscores, 0x prefixes, hex floats) and any
## whitespace are rejected.

Cur : { bytes : List(U8), pos : U64 }

Row : { id : U64, score : F64, name : Str }

Defect : [BadId, BadScore, ExtraField, MissingField]

Input : { csv : Str, with_header : Str, order : List(U64), note : Bool, rows : List(Row), first_bad : Try({ index : U64, defect : Defect }, [AllValid]) }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

pick_u64 : Cur -> { n : U64, cur : Cur }
pick_u64 = |start| {
	width = pick(start, 9)
	var $cur = width.cur
	var $value = 0
	var $index = 0
	while $index < width.n {
		chosen = pick($cur, 256)
		$value = $value * 256 + chosen.n
		$cur = chosen.cur
		$index = $index + 1
	}
	{ n: $value, cur: $cur }
}

u64_spellings : U64, Cur -> { text : Str, cur : Cur }
u64_spellings = |value, cur| {
	choice = pick(cur, 4)
	text =
		match choice.n {
			0 => value.to_str()
			1 => Str.concat("+", value.to_str())
			2 => Str.concat("000", value.to_str())
			_ => value.to_str()
		}
	{ text, cur: choice.cur }
}

bad_u64 : List(Str)
bad_u64 = ["", " 1", "1 ", "-1", "-0", "0x10", "1_000", "١", "18446744073709551616", "99999999999999999999", "1.0", "+", "++1", "1e3", "\t1", "1,"]

## Exact F64 values (small integer times a power of two) and their spellings.
pick_f64 : Cur -> { value : F64, text : Str, cur : Cur }
pick_f64 = |start| {
	kind = pick(start, 8)
	match kind.n {
		0 => {
			special = pick(kind.cur, special_f64.len())
			entry = special_f64.get(special.n) ?? { text: "0", value: 0.0 }
			{ value: entry.value, text: entry.text, cur: special.cur }
		}
		_ => {
			mantissa = pick_u64(kind.cur)
			sign = pick(mantissa.cur, 2)
			power = pick(sign.cur, 41)
			magnitude = U64.to_f64(mantissa.n % 9007199254740992)
			var $value = if sign.n == 0 magnitude else -magnitude
			var $step = 0
			while $step < power.n {
				$value = if power.n > 20 $value / 2.0 else $value * 2.0
				$step = $step + 1
			}
			{ value: $value, text: $value.to_str(), cur: power.cur }
		}
	}
}

special_f64 : List({ text : Str, value : F64 })
special_f64 = [
	{ text: "inf", value: F64.infinity },
	{ text: "-Infinity", value: -F64.infinity },
	{ text: "+INF", value: F64.infinity },
	{ text: "NaN", value: F64.nan },
	{ text: "-nan", value: F64.nan },
	{ text: "1e400", value: F64.infinity },
	{ text: "-1e400", value: -F64.infinity },
	{ text: "1e-400", value: 0.0 },
	{ text: ".5", value: 0.5 },
	{ text: "5.", value: 5.0 },
	{ text: "-0", value: -0.0 },
	{ text: "+1.5E3", value: 1500.0 },
	{ text: "007.25e-0", value: 7.25 },
	{ text: "1e+2", value: 100.0 },
]

bad_f64 : List(Str)
bad_f64 = ["", " 1", "1 ", "1_0.5", "0x1p3", "0x10", "1e", "e5", ".", "+-1", "1.2.3", "inf ", "nan1", "١", "infinit", "--1", "1e+", "+", "-"]

alphabet : List(Str)
alphabet = ["a", "Z", "1", " ", ",", "\"", "\n", "\r\n", "é", "😀", "\t", "\u(0)"]

gen_name : Cur -> { s : Str, cur : Cur }
gen_name = |start| {
	sized = pick(start, 6)
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

cell : Str, Cur -> { text : Str, cur : Cur }
cell = |field, cur| {
	force = pick(cur, 3)
	text = if needs_quotes(field) or force.n == 0 "\"${Str.replace_each(field, "\"", "\"\"")}\"" else field
	{ text, cur: force.cur }
}

generate : List(U8) -> Input
generate = |bytes| {
	row_count = pick({ bytes, pos: 0 }, 6)
	order_choice = pick(row_count.cur, orders.len())
	note = pick(order_choice.cur, 2)
	order = orders.get(order_choice.n) ?? [0, 1, 2]
	var $cur = note.cur
	var $rows = []
	var $lines = []
	var $header_lines = [Str.join_with(order.map(|column| column_names.get(column) ?? "").concat(if note.n == 1 ["note"] else []), ",")]
	var $first_bad = Err(AllValid)
	var $index = 0
	while $index < row_count.n + 1 {
		id = pick_u64($cur)
		id_text = u64_spellings(id.n, id.cur)
		score = pick_f64(id_text.cur)
		name = gen_name(score.cur)
		defect = pick(name.cur, 12)
		$cur = defect.cur
		var $base = [id_text.text, score.text, name.s]
		var $kind = Err(NoDefect)
		match defect.n {
			0 => {
				bad = pick($cur, bad_u64.len())
				$cur = bad.cur
				$base = [bad_u64.get(bad.n) ?? "", score.text, name.s]
				$kind = Ok(BadId)
			}
			1 => {
				bad = pick($cur, bad_f64.len())
				$cur = bad.cur
				$base = [id_text.text, bad_f64.get(bad.n) ?? "", name.s]
				$kind = Ok(BadScore)
			}
			2 => {
				$kind = Ok(ExtraField)
			}
			3 => {
				$kind = Ok(MissingField)
			}
			_ => {}
		}
		ordered = order.map(|column| $base.get(column) ?? "").concat(if note.n == 1 ["n"] else [])
		{ positional, named } =
			match $kind {
				Ok(ExtraField) => { positional: $base.append("extra"), named: ordered.append("extra") }
				Ok(MissingField) => { positional: $base.take_first(2), named: ordered.take_first(ordered.len() - 1) }
				_ => { positional: $base, named: ordered }
			}
		match ($kind, $first_bad) {
			(Ok(defect_kind), Err(AllValid)) => {
				$first_bad = Ok({ index: $index, defect: defect_kind })
			}
			_ => {}
		}
		quoted_positional = quote_all(positional, $cur)
		quoted_named = quote_all(named, quoted_positional.cur)
		$cur = quoted_named.cur
		$lines = $lines.append(Str.join_with(quoted_positional.cells, ","))
		$header_lines = $header_lines.append(Str.join_with(quoted_named.cells, ","))
		$rows = $rows.append({ id: id.n, score: score.value, name: name.s })
		$index = $index + 1
	}
	{
		csv: Str.concat(Str.join_with($lines, "\r\n"), "\r\n"),
		with_header: Str.concat(Str.join_with($header_lines, "\n"), "\n"),
		order,
		note: note.n == 1,
		rows: $rows,
		first_bad: $first_bad,
	}
}

column_names : List(Str)
column_names = ["id", "score", "name"]

orders : List(List(U64))
orders = [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]]

quote_all : List(Str), Cur -> { cells : List(Str), cur : Cur }
quote_all = |fields, start| {
	var $cur = start
	var $cells = []
	for field in fields {
		quoted = cell(field, $cur)
		$cur = quoted.cur
		$cells = $cells.append(quoted.text)
	}
	{ cells: $cells, cur: $cur }
}

decoder : Parser(CSV.Record, Row)
decoder =
	CSV.record(|id| |score| |name| { id, score, name })
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(CSV.f64))
		.keep(CSV.field(CSV.string))

same_f64 : F64, F64 -> Bool
same_f64 = |a, b| (a.is_nan() and b.is_nan()) or (a == b and (a != 0.0 or (1.0 / a) == (1.0 / b)))

same_rows : List(Row), List(Row) -> Bool
same_rows = |a, b| a.len() == b.len() and List.map2(a, b, |x, y| x.id == y.id and same_f64(x.score, y.score) and x.name == y.name).all(|same| same)

show_rows : List(Row) -> Str
show_rows = |rows| rows.map(|row| "{ id: ${row.id.to_str()}, score: ${row.score.to_str()}, name: ${Str.inspect(row.name)} }") |> Str.join_with("\n")

## The record and field a defect is reported at.
Expected : { record : U64, field : U64 }

## Position of the first defect when decoding by position.
positional_error : U64, Defect -> Expected
positional_error = |index, defect| {
	field =
		match defect {
			BadId => 1
			BadScore => 2
			ExtraField => 4
			MissingField => 3
		}
	{ record: index + 1, field }
}

## Position of the first defect when decoding by header name: the header is
## record 1, and columns follow the chosen order.
named_error : Input, U64, Defect -> Expected
named_error = |input, index, defect| {
	width = if input.note 4 else 3
	position = |column| (input.order.find_first_index(|c| c == column) ?? 0) + 1
	field =
		match defect {
			BadId => position(0)
			BadScore => position(1)
			ExtraField => width + 1
			MissingField => width
		}
	{ record: index + 2, field }
}

check : Str, Str, Try(List(Row), [InvalidCsv(CSV.Error), MissingRequiredField(Str)]), Try(Expected, [AllValid]), List(Row) -> {}
check = |label, csv, result, expected, rows| {
	context = "${label}\n--- csv ---\n${Str.inspect(csv)}"
	match (expected, result) {
		(Err(AllValid), Ok(actual)) =>
			if !same_rows(actual, rows) {
				crash "decoded rows differ: ${context}\n--- expected ---\n${show_rows(rows)}\n--- actual ---\n${show_rows(actual)}"
			}
		(Err(AllValid), Err(problem)) => crash "valid rows were rejected: ${context}\n--- error ---\n${Str.inspect(problem)}"
		(Ok(_), Ok(actual)) => crash "a defective row decoded: ${context}\n--- actual ---\n${show_rows(actual)}"
		(Ok(wanted), Err(InvalidCsv(problem))) =>
			if problem.record != wanted.record or problem.field != wanted.field or problem.line == 0 or problem.column == 0 {
				crash "wrong error position: ${context}\n--- expected ---\n${Str.inspect(wanted)}\n--- actual ---\n${Str.inspect(problem)}"
			}
		(Ok(_), Err(MissingRequiredField(name))) => crash "every column is present, but ${name} was reported missing: ${context}"
	}
}

test : Input -> Fuzz.Outcome
test = |input| {
	positional : Try(List(Row), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	positional =
		match CSV.parse_with(decoder, input.csv) {
			Ok(rows) => Ok(rows)
			Err(InvalidCsv(problem)) => Err(InvalidCsv(problem))
		}
	check("positional", input.csv, positional, input.first_bad.map_ok(|{ index, defect }| positional_error(index, defect)), input.rows)
	named : Try(List(Row), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	named = CSV.parse(input.with_header)
	check("by header name", input.with_header, named, input.first_bad.map_ok(|{ index, defect }| named_error(input, index, defect)), input.rows)
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "csv-decode",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "csv: ${Str.inspect(input.csv)}\nwith header: ${Str.inspect(input.with_header)}",
})
