app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Parser

## Typed decoding property: fuzzer bytes choose rows of (U64, F64, Str)
## values, how each number is spelled, how each field is quoted, and possibly
## one defect per row (a malformed number, an extra field, a missing field).
## CSV.parse_str with the record(...).keep(field(u64))... decoder must return
## exactly those values, or fail on the first defective row with a message
## naming it.
##
## The number grammar is Rust's `u64::from_str` / `f64::from_str` (a mature,
## documented reference): U64 is `+?[0-9]+` within range; F64 is an optional
## sign then `inf`, `infinity` or `nan` (any case) or a decimal with optional
## fraction and exponent, rounding out-of-range magnitudes to infinity or
## zero. Roc literal extras (underscores, 0x prefixes, hex floats) and any
## whitespace are rejected.

Cur : { bytes : List(U8), pos : U64 }

Row : { id : U64, score : F64, name : Str }

Input : { csv : Str, rows : List(Row), first_bad : Try(U64, [AllValid]) }

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
	var $cur = row_count.cur
	var $rows = []
	var $lines = []
	var $first_bad = Err(AllValid)
	var $index = 0
	while $index < row_count.n + 1 {
		id = pick_u64($cur)
		id_text = u64_spellings(id.n, id.cur)
		score = pick_f64(id_text.cur)
		name = gen_name(score.cur)
		defect = pick(name.cur, 12)
		$cur = defect.cur
		var $fields = [id_text.text, score.text, name.s]
		match defect.n {
			0 => {
				bad = pick($cur, bad_u64.len())
				$cur = bad.cur
				$fields = [bad_u64.get(bad.n) ?? "", score.text, name.s]
			}
			1 => {
				bad = pick($cur, bad_f64.len())
				$cur = bad.cur
				$fields = [id_text.text, bad_f64.get(bad.n) ?? "", name.s]
			}
			2 => {
				$fields = $fields.append("extra")
			}
			3 => {
				$fields = [id_text.text, score.text]
			}
			_ => {}
		}
		if defect.n <= 3 {
			match $first_bad {
				Err(AllValid) => {
					$first_bad = Ok($index)
				}
				Ok(_) => {}
			}
		}
		var $cells = []
		var $field_index = 0
		while $field_index < $fields.len() {
			quoted = cell($fields.get($field_index) ?? "", $cur)
			$cur = quoted.cur
			$cells = $cells.append(quoted.text)
			$field_index = $field_index + 1
		}
		$lines = $lines.append(Str.join_with($cells, ","))
		$rows = $rows.append({ id: id.n, score: score.value, name: name.s })
		$index = $index + 1
	}
	{ csv: Str.concat(Str.join_with($lines, "\r\n"), "\r\n"), rows: $rows, first_bad: $first_bad }
}

decoder : Parser(CSV.CSVRecord, Row)
decoder =
	CSV.record(|id| |score| |name| { id, score, name })
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(CSV.f64))
		.keep(CSV.field(CSV.string))

same_f64 : F64, F64 -> Bool
same_f64 = |a, b| (a.is_nan() and b.is_nan()) or (a == b and (a != 0.0 or (1.0 / a) == (1.0 / b)))

same_row : Row, Row -> Bool
same_row = |a, b| a.id == b.id and same_f64(a.score, b.score) and a.name == b.name

show_rows : List(Row) -> Str
show_rows = |rows| rows.map(|row| "{ id: ${row.id.to_str()}, score: ${row.score.to_str()}, name: ${Str.inspect(row.name)} }") |> Str.join_with("\n")

test : Input -> Fuzz.Outcome
test = |input| {
	result = CSV.parse_str(decoder, input.csv)
	match (input.first_bad, result) {
		(Err(AllValid), Ok(rows)) => {
			matches = rows.len() == input.rows.len() and List.map2(rows, input.rows, same_row).all(|same| same)
			if !matches {
				crash "decoded rows differ\n--- csv ---\n${Str.inspect(input.csv)}\n--- expected ---\n${show_rows(input.rows)}\n--- actual ---\n${show_rows(rows)}"
			}
		}
		(Err(AllValid), Err(ParsingFailure(message))) => crash "valid rows were rejected\n--- csv ---\n${Str.inspect(input.csv)}\n--- message ---\n${message}"
		(Err(AllValid), Err(_)) => crash "valid rows were rejected\n--- csv ---\n${Str.inspect(input.csv)}"
		(Ok(index), Ok(rows)) => crash "row ${(index + 1).to_str()} is defective but decoding succeeded\n--- csv ---\n${Str.inspect(input.csv)}\n--- actual ---\n${show_rows(rows)}"
		(Ok(index), Err(ParsingFailure(message))) => {
			if !Str.contains(message, "record no. ${(index + 1).to_str()}:") {
				crash "failure does not name record ${(index + 1).to_str()}\n--- csv ---\n${Str.inspect(input.csv)}\n--- message ---\n${message}"
			}
		}
		# An extra field is reported as the unread leftover fields.
		(Ok(_), Err(ParsingIncomplete(leftover))) if leftover == ["extra".to_utf8()] => {}
		(Ok(index), Err(ParsingIncomplete(_))) => crash "row ${(index + 1).to_str()} gave an unexpected ParsingIncomplete\n${Str.inspect(input.csv)}"
		(Ok(_), Err(SyntaxError(rest))) => crash "well-formed CSV was a syntax error\n--- csv ---\n${Str.inspect(input.csv)}\n--- rest ---\n${Str.inspect(rest)}"
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "csv-decode",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "csv: ${Str.inspect(input.csv)}",
})
