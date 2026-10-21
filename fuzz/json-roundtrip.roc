app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.JsonValue
import parser.Utf8

## Round-trip property: fuzzer bytes choose a JSON value, a JsonValue.Options
## set, and how to write the value: whitespace, comments, string escapes
## (short escapes, `\u` with lower, upper or mixed case hex, surrogate
## pairs), number lexemes (sign, zero, fraction, `e`/`E` exponents with `+`
## or `-`, integers past U64) and, only where the chosen options permit it, a
## byte order mark, trailing commas, `NaN`/`Infinity`, raw control characters
## and lone surrogates (which read as U+FFFD).
##
## The expected value is built alongside the text, never by calling the
## parser, as are the compact and indented (Python `json.dumps(indent=2)`)
## texts the writer must produce:
## - parse_with and parser_with give exactly the expected value;
## - strict parse_str fails exactly when relaxed syntax was written;
## - to_str and to_str_with give the expected texts, or InvalidNumber when the
##   value holds NaN or Infinity;
## - get, at, get_path, keys and the as_* accessors agree with a direct walk;
## - with limits set to the value's own depth, item count, string length and
##   number length (and the matching top_level), it still parses, and
##   lowering any one of them by one makes it fail.
##
## One input in four instead repeats an object key with an escaped spelling
## and an equal or a different value. Reject, and CollapseIdentical with a
## different value, must fail at the second key's opening quote; KeepFirst
## and KeepLast must keep the first position with the first or last value.

Cur : { bytes : List(U8), pos : U64 }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

Duplicates : [Reject, CollapseIdentical, KeepFirst, KeepLast]

TopLevel : [Any, ObjectOrArray, Object]

Limits : { max_depth : U64, max_items : U64, max_string_bytes : U64, max_number_digits : U64 }

Choice : {
	comments : Bool,
	trailing_commas : Bool,
	bom : Bool,
	non_finite : Bool,
	control_characters : Bool,
	replace_surrogates : Bool,
	duplicates : Duplicates,
	top_level : TopLevel,
	interoperable_numbers : Bool,
	limits : Limits,
}

## What parse_with must return: a value, or an error at a byte offset whose
## message is `message` (or starts with it, when `exact` is False).
Expectation : [Value(JsonValue), Failure({ offset : U64, message : Str, exact : Bool })]

Input : {
	choice : Choice,
	text : Str,
	expected : Expectation,
	canonical : Str,
	pretty : Str,
	relaxed : Bool,
	non_finite : Bool,
	tight : Bool,
}

Emitted : { text : Str, cur : Cur, relaxed : Bool }

no_limits : Limits
no_limits = { max_depth: 128, max_items: U64.highest, max_string_bytes: U64.highest, max_number_digits: U64.highest }

strict : Choice
strict = {
	comments: False,
	trailing_commas: False,
	bom: False,
	non_finite: False,
	control_characters: False,
	replace_surrogates: False,
	duplicates: Reject,
	top_level: Any,
	interoperable_numbers: False,
	limits: no_limits,
}

options_of : Choice -> JsonValue.Options
options_of = |choice| {
	max_depth: choice.limits.max_depth,
	max_items: choice.limits.max_items,
	max_string_bytes: choice.limits.max_string_bytes,
	max_number_digits: choice.limits.max_number_digits,
	comments: choice.comments,
	trailing_commas: choice.trailing_commas,
	bom: choice.bom,
	non_finite: choice.non_finite,
	control_characters: choice.control_characters,
	lone_surrogates: if (choice.replace_surrogates) Replace else Reject,
	duplicates: choice.duplicates,
	top_level: choice.top_level,
	interoperable_numbers: choice.interoperable_numbers,
}

duplicates_from : U64 -> Duplicates
duplicates_from = |n| {
	match n % 4 {
		0 => Reject
		1 => CollapseIdentical
		2 => KeepFirst
		_ => KeepLast
	}
}

## UTF-8

encode_utf8 : U64 -> List(U8)
encode_utf8 = |code| {
	if code < 0x80 {
		[U64.to_u8_wrap(code)]
	} else if code < 0x800 {
		[U64.to_u8_wrap(0xC0 + code // 64), U64.to_u8_wrap(0x80 + code % 64)]
	} else if code < 0x10000 {
		[U64.to_u8_wrap(0xE0 + code // 4096), U64.to_u8_wrap(0x80 + (code // 64) % 64), U64.to_u8_wrap(0x80 + code % 64)]
	} else {
		[U64.to_u8_wrap(0xF0 + code // 262144), U64.to_u8_wrap(0x80 + (code // 4096) % 64), U64.to_u8_wrap(0x80 + (code // 64) % 64), U64.to_u8_wrap(0x80 + code % 64)]
	}
}

## The code points of a string (which Roc guarantees is valid UTF-8).
code_points : Str -> List(U64)
code_points = |text| {
	bytes = text.to_utf8()
	var $codes = []
	var $index = 0
	while $index < bytes.len() {
		lead = U8.to_u64(bytes.get($index) ?? 0)
		cont = |offset| U8.to_u64(bytes.get($index + offset) ?? 0x80) % 64
		if lead < 0x80 {
			$codes = $codes.append(lead)
			$index = $index + 1
		} else if lead < 0xE0 {
			$codes = $codes.append((lead % 32) * 64 + cont(1))
			$index = $index + 2
		} else if lead < 0xF0 {
			$codes = $codes.append((lead % 16) * 4096 + cont(1) * 64 + cont(2))
			$index = $index + 3
		} else {
			$codes = $codes.append((lead % 8) * 262144 + cont(1) * 4096 + cont(2) * 64 + cont(3))
			$index = $index + 4
		}
	}
	$codes
}

## Values (the expected trees)

## Characters strings and keys are made of: ASCII, the characters JSON must
## escape, multi-byte and astral characters, and U+FFFD, which a lone
## surrogate escape also reads as.
alphabet : List(U64)
alphabet = [0x61, 0x5A, 0x31, 0x20, 0x22, 0x5C, 0x2F, 0x08, 0x0C, 0x0A, 0x0D, 0x09, 0x00, 0x1F, 0x7F, 0xE9, 0x65E5, 0x1F600, 0x2028, 0xFEFF, 0x10FFFF, 0xFFFD, 0x7B, 0x3A, 0x2C]

gen_text : Cur, U64 -> { s : Str, cur : Cur }
gen_text = |start, longest| {
	sized = pick(start, longest + 1)
	var $cur = sized.cur
	var $bytes = []
	var $index = 0
	while $index < sized.n {
		chosen = pick($cur, alphabet.len())
		$bytes = $bytes.concat(encode_utf8(alphabet.get(chosen.n) ?? 0x61))
		$cur = chosen.cur
		$index = $index + 1
	}
	{ s: Str.from_utf8($bytes) ?? "", cur: $cur }
}

non_finite_lexemes : List(Str)
non_finite_lexemes = ["NaN", "Infinity", "-Infinity"]

is_non_finite : Str -> Bool
is_non_finite = |lexeme| non_finite_lexemes.contains(lexeme)

integer_parts : List(Str)
integer_parts = ["0", "7", "42", "1234567890", "9007199254740993", "9223372036854775807", "9223372036854775808", "18446744073709551615", "18446744073709551616", "123456789012345678901234567890"]

fraction_parts : List(Str)
fraction_parts = ["", "", ".5", ".0", ".000125", ".1234567890123456789"]

exponent_parts : List(Str)
exponent_parts = ["", "", "", "e1", "E+10", "e-7", "E00", "e+0"]

## A number lexeme following the JSON grammar, or NaN or Infinity when the
## options allow them.
gen_number : Cur, Choice -> { lexeme : Str, cur : Cur }
gen_number = |start, choice| {
	special = pick(start, 8)
	if choice.non_finite and special.n == 0 {
		chosen = pick(special.cur, non_finite_lexemes.len())
		{ lexeme: non_finite_lexemes.get(chosen.n) ?? "NaN", cur: chosen.cur }
	} else {
		sign = pick(special.cur, 3)
		integer = pick(sign.cur, integer_parts.len())
		fraction = pick(integer.cur, fraction_parts.len())
		exponent = pick(fraction.cur, exponent_parts.len())
		lexeme = Str.join_with(
			[
				if (sign.n == 0) "-" else "",
				integer_parts.get(integer.n) ?? "0",
				fraction_parts.get(fraction.n) ?? "",
				exponent_parts.get(exponent.n) ?? "",
			],
			"",
		)
		{ lexeme, cur: exponent.cur }
	}
}

## A value nested at most `depth` deep, with unique keys in every object.
gen_tree : Cur, Choice, U64 -> { value : JsonValue, cur : Cur }
gen_tree = |start, choice, depth| {
	kind = pick(start, 8)
	match kind.n {
		0 => { value: Null, cur: kind.cur }
		1 => {
			flag = pick(kind.cur, 2)
			{ value: Bool(flag.n == 1), cur: flag.cur }
		}
		2 | 3 => {
			number = gen_number(kind.cur, choice)
			{ value: Number(number.lexeme), cur: number.cur }
		}
		6 if depth > 0 => {
			count = pick(kind.cur, 5)
			var $cur = count.cur
			var $items = []
			var $index = 0
			while $index < count.n {
				item = gen_tree($cur, choice, depth - 1)
				$items = $items.append(item.value)
				$cur = item.cur
				$index = $index + 1
			}
			{ value: Array($items), cur: $cur }
		}
		7 if depth > 0 => {
			count = pick(kind.cur, 6)
			var $cur = count.cur
			var $members = []
			var $index = 0
			while $index < count.n {
				key = gen_text($cur, 4)
				member = gen_tree(key.cur, choice, depth - 1)
				$cur = member.cur
				if !$members.any(|existing| existing.key == key.s) {
					$members = $members.append({ key: key.s, value: member.value })
				}
				$index = $index + 1
			}
			{ value: Object($members), cur: $cur }
		}
		_ => {
			text = gen_text(kind.cur, 6)
			{ value: String(text.s), cur: text.cur }
		}
	}
}

## Writing (the text the fuzzer chose)

## Whitespace between tokens, sometimes a comment when the options allow it.
## A line comment without a line break is only written at the end.
emit_ws : Cur, Choice, Bool -> Emitted
emit_ws = |start, choice, at_end| {
	kind = pick(start, 12)
	plain = |text| { text, cur: kind.cur, relaxed: False }
	match kind.n {
		6 => plain(" ")
		7 => plain("\n")
		8 => plain("\r\n")
		9 => plain(" \t")
		10 => plain("\r")
		11 if choice.comments => {
			comment = pick(kind.cur, 7)
			text =
				match comment.n {
					0 => "/* c */"
					1 => "/**/"
					2 => "/***/"
					3 => "/* // \n */"
					4 => " // c\n"
					5 => "//\r\n"
					_ => if (at_end) "// end" else "/*/ */"
				}
			{ text, cur: comment.cur, relaxed: True }
		}
		_ => plain("")
	}
}

hex_digit : U64, Bool -> U8
hex_digit = |digit, upper| {
	if digit < 10 {
		U64.to_u8_wrap(digit) + '0'
	} else if upper {
		U64.to_u8_wrap(digit - 10) + 'A'
	} else {
		U64.to_u8_wrap(digit - 10) + 'a'
	}
}

## `\u` and four hex digits: lower case (0), upper case (1), or mixed (2).
u_escape : U64, U64 -> Str
u_escape = |unit, case| {
	digits = [unit // 4096, (unit // 256) % 16, (unit // 16) % 16, unit % 16]
	var $bytes = ['\\', 'u']
	var $index = 0
	for digit in digits {
		upper = case == 1 or (case == 2 and $index % 2 == 0)
		$bytes = $bytes.append(hex_digit(digit, upper))
		$index = $index + 1
	}
	Str.from_utf8($bytes) ?? ""
}

## A `\u` escape for any character, as a surrogate pair beyond U+FFFF.
code_escape : U64, U64 -> Str
code_escape = |code, case| {
	if code >= 0x10000 {
		offset = code - 0x10000
		Str.concat(u_escape(0xD800 + offset // 1024, case), u_escape(0xDC00 + offset % 1024, (case + 1) % 3))
	} else {
		u_escape(code, case)
	}
}

short_escape : U64 -> Try(Str, [NoShortEscape])
short_escape = |code| {
	match code {
		0x22 => Ok("\\\"")
		0x5C => Ok("\\\\")
		0x2F => Ok("\\/")
		0x08 => Ok("\\b")
		0x0C => Ok("\\f")
		0x0A => Ok("\\n")
		0x0D => Ok("\\r")
		0x09 => Ok("\\t")
		_ => Err(NoShortEscape)
	}
}

## One character of a string: literally, as a short escape, as a `\u`
## escape, or (for U+FFFD, when the options replace lone surrogates) as a
## lone surrogate. A lone high surrogate is only written last, so it can
## never pair with what follows; a lone low surrogate never follows a high
## one. With `escape_all` every character is escaped.
emit_code : Cur, Choice, U64, Bool, Bool -> Emitted
emit_code = |start, choice, code, escape_all, last| {
	style = pick(start, 4)
	case = pick(style.cur, 3)
	cur = case.cur
	literal = Str.from_utf8(encode_utf8(code)) ?? ""
	if style.n == 0 and !escape_all and code >= 0x20 and code != 0x22 and code != 0x5C {
		{ text: literal, cur, relaxed: False }
	} else if style.n == 0 and !escape_all and code < 0x20 and choice.control_characters {
		{ text: literal, cur, relaxed: True }
	} else if style.n == 2 and code == 0xFFFD and choice.replace_surrogates {
		unit = if (last) 0xD83D else [0xDC00, 0xDE00, 0xDFFF].get(case.n) ?? 0xDC00
		{ text: u_escape(unit, case.n), cur, relaxed: True }
	} else {
		match short_escape(code) {
			Ok(escape) if style.n == 1 => { text: escape, cur, relaxed: False }
			_ => { text: code_escape(code, case.n), cur, relaxed: False }
		}
	}
}

emit_string : Cur, Choice, Str, Bool -> Emitted
emit_string = |start, choice, text, escape_all| {
	codes = code_points(text)
	var $cur = start
	var $out = "\""
	var $relaxed = False
	var $index = 0
	for code in codes {
		piece = emit_code($cur, choice, code, escape_all, $index + 1 == codes.len())
		$out = Str.concat($out, piece.text)
		$relaxed = $relaxed or piece.relaxed
		$cur = piece.cur
		$index = $index + 1
	}
	{ text: Str.concat($out, "\""), cur: $cur, relaxed: $relaxed }
}

## A comma after the last item or member, when the options allow it.
emit_trailing_comma : Cur, Choice, Bool -> Emitted
emit_trailing_comma = |start, choice, any_items| {
	chosen = pick(start, 3)
	if choice.trailing_commas and any_items and chosen.n == 0 {
		ws = emit_ws(chosen.cur, choice, False)
		{ text: Str.concat(",", ws.text), cur: ws.cur, relaxed: True }
	} else {
		{ text: "", cur: chosen.cur, relaxed: False }
	}
}

## One member, `ws key ws : ws value ws`.
emit_member : Cur, Choice, { key : Str, value : JsonValue }, Bool -> Emitted
emit_member = |start, choice, member, escape_key| {
	lead = emit_ws(start, choice, False)
	key = emit_string(lead.cur, choice, member.key, escape_key)
	rest = emit_after_key(key.cur, choice, member.value)
	{ text: Str.join_with([lead.text, key.text, rest.text], ""), cur: rest.cur, relaxed: lead.relaxed or key.relaxed or rest.relaxed }
}

## What follows a key: `ws : ws value ws`.
emit_after_key : Cur, Choice, JsonValue -> Emitted
emit_after_key = |start, choice, member_value| {
	before_colon = emit_ws(start, choice, False)
	after_colon = emit_ws(before_colon.cur, choice, False)
	value = emit_value(after_colon.cur, choice, member_value)
	trail = emit_ws(value.cur, choice, False)
	{
		text: Str.join_with([before_colon.text, ":", after_colon.text, value.text, trail.text], ""),
		cur: trail.cur,
		relaxed: before_colon.relaxed or after_colon.relaxed or value.relaxed or trail.relaxed,
	}
}

## One item, `ws value ws`.
emit_item : Cur, Choice, JsonValue -> Emitted
emit_item = |start, choice, item| {
	lead = emit_ws(start, choice, False)
	value = emit_value(lead.cur, choice, item)
	trail = emit_ws(value.cur, choice, False)
	{ text: Str.join_with([lead.text, value.text, trail.text], ""), cur: trail.cur, relaxed: lead.relaxed or value.relaxed or trail.relaxed }
}

emit_value : Cur, Choice, JsonValue -> Emitted
emit_value = |start, choice, value| {
	match value {
		Null => { text: "null", cur: start, relaxed: False }
		Bool(flag) => { text: if (flag) "true" else "false", cur: start, relaxed: False }
		Number(lexeme) => { text: lexeme, cur: start, relaxed: is_non_finite(lexeme) }
		String(text) => emit_string(start, choice, text, False)
		Array(items) => {
			var $cur = start
			var $parts = []
			var $relaxed = False
			for item in items {
				piece = emit_item($cur, choice, item)
				$parts = $parts.append(piece.text)
				$relaxed = $relaxed or piece.relaxed
				$cur = piece.cur
			}
			inner = if ($parts.is_empty()) emit_ws($cur, choice, False) else { text: Str.join_with($parts, ","), cur: $cur, relaxed: False }
			trailing = emit_trailing_comma(inner.cur, choice, !items.is_empty())
			{ text: "[${inner.text}${trailing.text}]", cur: trailing.cur, relaxed: $relaxed or inner.relaxed or trailing.relaxed }
		}
		Object(members) => {
			var $cur = start
			var $parts = []
			var $relaxed = False
			for member in members {
				piece = emit_member($cur, choice, member, False)
				$parts = $parts.append(piece.text)
				$relaxed = $relaxed or piece.relaxed
				$cur = piece.cur
			}
			inner = if ($parts.is_empty()) emit_ws($cur, choice, False) else { text: Str.join_with($parts, ","), cur: $cur, relaxed: False }
			trailing = emit_trailing_comma(inner.cur, choice, !members.is_empty())
			{ text: "{${inner.text}${trailing.text}}", cur: trailing.cur, relaxed: $relaxed or inner.relaxed or trailing.relaxed }
		}
	}
}

## Expected writer output (independent of JsonValue's writer)

escape_json : Str -> Str
escape_json = |text| {
	var $out = []
	for byte in text.to_utf8() {
		$out =
			match byte {
				'"' => $out.concat(['\\', '"'])
				'\\' => $out.concat(['\\', '\\'])
				0x08 => $out.concat(['\\', 'b'])
				0x0C => $out.concat(['\\', 'f'])
				'\n' => $out.concat(['\\', 'n'])
				'\r' => $out.concat(['\\', 'r'])
				'\t' => $out.concat(['\\', 't'])
				_ if byte < 0x20 => $out.concat(['\\', 'u', '0', '0', hex_digit(U8.to_u64(byte // 16), False), hex_digit(U8.to_u64(byte % 16), False)])
				_ => $out.append(byte)
			}
	}
	"\"${Str.from_utf8($out) ?? ""}\""
}

compact : JsonValue -> Str
compact = |value| {
	match value {
		Null => "null"
		Bool(flag) => if (flag) "true" else "false"
		Number(lexeme) => lexeme
		String(text) => escape_json(text)
		Array(items) => "[${Str.join_with(items.map(|item| compact(item)), ",")}]"
		Object(members) => "{${Str.join_with(members.map(|member| "${escape_json(member.key)}:${compact(member.value)}"), ",")}}"
	}
}

## Python's json.dumps(value, indent=2, ensure_ascii=False) layout.
pretty : JsonValue, U64 -> Str
pretty = |value, level| {
	pad = |depth| Str.repeat("  ", depth)
	match value {
		Array([]) => "[]"
		Object([]) => "{}"
		Array(items) => {
			lines = items.map(|item| "${pad(level + 1)}${pretty(item, level + 1)}")
			"[\n${Str.join_with(lines, ",\n")}\n${pad(level)}]"
		}
		Object(members) => {
			lines = members.map(|member| "${pad(level + 1)}${escape_json(member.key)}: ${pretty(member.value, level + 1)}")
			"{\n${Str.join_with(lines, ",\n")}\n${pad(level)}}"
		}
		_ => compact(value)
	}
}

sort_keys : JsonValue -> JsonValue
sort_keys = |value| {
	match value {
		Array(items) => Array(items.map(|item| sort_keys(item)))
		Object(members) =>
			Object(
				members
					.map(|member| { key: member.key, value: sort_keys(member.value) })
					.sort_with(|a, b| compare_bytes(a.key.to_utf8(), b.key.to_utf8())),
			)
		_ => value
	}
}

compare_bytes : List(U8), List(U8) -> [Before, Same, After]
compare_bytes = |a, b| {
	var $index = 0
	var $answer = Same
	var $done = False
	while !$done {
		match (a.get($index), b.get($index)) {
			(Ok(x), Ok(y)) =>
				if x < y {
					$answer = Before
					$done = True
				} else if x > y {
					$answer = After
					$done = True
				} else {
					$index = $index + 1
				}
			(Err(_), Ok(_)) => {
				$answer = Before
				$done = True
			}
			(Ok(_), Err(_)) => {
				$answer = After
				$done = True
			}
			_ => {
				$done = True
			}
		}
	}
	$answer
}

## Sizes the limits measure

Stats : { depth : U64, items : U64, string_bytes : U64, number_chars : U64, non_finite : Bool }

larger : U64, U64 -> U64
larger = |a, b| if (a > b) a else b

merge : Stats, Stats -> Stats
merge = |a, b| {
	depth: larger(a.depth, b.depth),
	items: larger(a.items, b.items),
	string_bytes: larger(a.string_bytes, b.string_bytes),
	number_chars: larger(a.number_chars, b.number_chars),
	non_finite: a.non_finite or b.non_finite,
}

zero_stats : Stats
zero_stats = { depth: 0, items: 0, string_bytes: 0, number_chars: 0, non_finite: False }

stats_of : JsonValue -> Stats
stats_of = |value| {
	match value {
		Number(lexeme) => { ..zero_stats, number_chars: Str.count_utf8_bytes(lexeme), non_finite: is_non_finite(lexeme) }
		String(text) => { ..zero_stats, string_bytes: Str.count_utf8_bytes(text) }
		Array(items) => {
			inner = items.fold(zero_stats, |acc, item| merge(acc, stats_of(item)))
			{ ..inner, depth: inner.depth + 1, items: larger(inner.items, items.len()) }
		}
		Object(members) => {
			inner = members.fold(zero_stats, |acc, member| merge(merge(acc, stats_of(member.value)), { ..zero_stats, string_bytes: Str.count_utf8_bytes(member.key) }))
			{ ..inner, depth: inner.depth + 1, items: larger(inner.items, members.len()) }
		}
		_ => zero_stats
	}
}

## Generation

generate : List(U8) -> Input
generate = |bytes| {
	start = { bytes, pos: 0 }
	mode = pick(start, 4)
	if mode.n == 0 {
		generate_duplicate(mode.cur)
	} else {
		generate_value(mode.cur)
	}
}

generate_value : Cur -> Input
generate_value = |start| {
	relax = pick(start, 2)
	bits = pick(relax.cur, 64)
	on = |mask| relax.n == 1 and (bits.n // mask) % 2 == 1
	duplicates = pick(bits.cur, 4)
	relaxed_choice = {
		..strict,
		comments: on(1),
		trailing_commas: on(2),
		bom: on(4),
		non_finite: on(8),
		control_characters: on(16),
		replace_surrogates: on(32),
		duplicates: duplicates_from(duplicates.n),
	}
	tree = gen_tree(duplicates.cur, relaxed_choice, 4)
	value = tree.value
	stats = stats_of(value)
	tight = pick(tree.cur, 3)
	choice =
		if tight.n == 0 {
			top_level =
				match value {
					Object(_) => Object
					Array(_) => ObjectOrArray
					_ => Any
				}
			limits = {
				max_depth: stats.depth,
				max_items: stats.items,
				max_string_bytes: stats.string_bytes,
				# Whether NaN and Infinity count as digits is not specified.
				max_number_digits: if (stats.non_finite) U64.highest else stats.number_chars,
			}
			{ ..relaxed_choice, top_level, limits }
		} else {
			relaxed_choice
		}

	bom = pick(tight.cur, 3)
	bom_text = if (choice.bom and bom.n == 0) "\u(FEFF)" else ""
	lead = emit_ws(bom.cur, choice, False)
	body = emit_value(lead.cur, choice, value)
	trail = emit_ws(body.cur, choice, True)
	{
		choice,
		text: Str.join_with([bom_text, lead.text, body.text, trail.text], ""),
		expected: Value(value),
		canonical: compact(value),
		pretty: pretty(value, 0),
		relaxed: !bom_text.is_empty() or lead.relaxed or body.relaxed or trail.relaxed,
		non_finite: stats.non_finite,
		tight: tight.n == 0,
	}
}

## Printable ASCII other than `"` and `\`, which an error message can quote
## without escaping.
is_plain_key : Str -> Bool
is_plain_key = |key| key.to_utf8().all(|byte| byte >= 0x20 and byte < 0x7F and byte != '"' and byte != '\\')

generate_duplicate : Cur -> Input
generate_duplicate = |start| {
	mode = pick(start, 4)
	choice = { ..strict, duplicates: duplicates_from(mode.n) }
	count = pick(mode.cur, 24)
	var $cur = count.cur
	var $members = []
	var $index = 0
	while $index <= count.n {
		key = gen_text($cur, 4)
		member = gen_tree(key.cur, choice, 1)
		$cur = member.cur
		if !$members.any(|existing| existing.key == key.s) {
			$members = $members.append({ key: key.s, value: member.value })
		}
		$index = $index + 1
	}
	members = $members
	repeated = pick($cur, members.len())
	original = members.get(repeated.n) ?? { key: "", value: Null }
	# The repeat goes after the original, possibly before later members.
	position = pick(repeated.cur, members.len() - repeated.n)
	split = repeated.n + 1 + position.n
	same = pick(position.cur, 2)
	{ repeat_value, cur: after_value } =
		if same.n == 0 {
			{ repeat_value: original.value, cur: same.cur }
		} else {
			other = gen_tree(same.cur, choice, 1)
			{ repeat_value: if (other.value == original.value) Array([other.value]) else other.value, cur: other.cur }
		}

	# Optionally the last item of an array, after up to two other items.
	wrap = pick(after_value, 4)
	wrapped_in_array = wrap.n > 0
	var $wrap_cur = wrap.cur
	var $before = []
	var $before_texts = []
	var $wrap_index = 1
	while $wrap_index < wrap.n {
		item = gen_tree($wrap_cur, choice, 1)
		emitted = emit_item(item.cur, choice, item.value)
		$before = $before.append(item.value)
		$before_texts = $before_texts.append(emitted.text)
		$wrap_cur = emitted.cur
		$wrap_index = $wrap_index + 1
	}
	lead = emit_ws($wrap_cur, choice, False)
	open = if (wrapped_in_array) "[${Str.join_with($before_texts.append(""), ",")}${lead.text}{" else "${lead.text}{"

	var $emit_cur = lead.cur
	var $parts = []
	for member in members.sublist({ start: 0, len: split }) {
		piece = emit_member($emit_cur, choice, member, False)
		$parts = $parts.append(piece.text)
		$emit_cur = piece.cur
	}
	gap = emit_ws($emit_cur, choice, False)
	prefix = "${open}${Str.join_with($parts, ",")},${gap.text}"
	repeat_key = emit_string(gap.cur, choice, original.key, True)
	repeat_rest = emit_after_key(repeat_key.cur, choice, repeat_value)
	var $later_cur = repeat_rest.cur
	var $later = []
	for member in members.drop_first(split) {
		piece = emit_member($later_cur, choice, member, False)
		$later = $later.append(piece.text)
		$later_cur = piece.cur
	}
	close = emit_ws($later_cur, choice, True)
	later_text = $later.fold("", |acc, piece| "${acc},${piece}")
	text = "${prefix}${repeat_key.text}${repeat_rest.text}${later_text}}${if (wrapped_in_array) "]" else ""}${close.text}"

	first = Object(members)
	last = Object(members.map(|member| if (member.key == original.key) { key: member.key, value: repeat_value } else member))
	wrapped = |object| if (wrapped_in_array) Array($before.append(object)) else object
	offset = Str.count_utf8_bytes(prefix)
	plain = is_plain_key(original.key)
	failure = |suffix| Failure({ offset, message: if (plain) "duplicate key \"${original.key}\"${suffix}" else "duplicate key \"", exact: plain })
	expected =
		match choice.duplicates {
			Reject => failure("")
			CollapseIdentical => if (same.n == 0) Value(wrapped(first)) else failure(" with a different value")
			KeepFirst => Value(wrapped(first))
			KeepLast => Value(wrapped(last))
		}
	{ value: written, canonical, pretty: pretty_text } =
		match expected {
			Value(v) => { value: v, canonical: compact(v), pretty: pretty(v, 0) }
			Failure(_) => { value: Null, canonical: "", pretty: "" }
		}
	{
		choice,
		text,
		expected,
		canonical,
		pretty: pretty_text,
		relaxed: True,
		non_finite: stats_of(written).non_finite,
		tight: False,
	}
}

## Checking

Parsed : Try(JsonValue, [InvalidJsonValue(JsonValue.Error)])

## One-based line and byte column of a byte offset, counting CR LF, CR and
## LF as line ends, independent of the parser's own locator.
line_column : List(U8), U64 -> { line : U64, column : U64 }
line_column = |bytes, offset| {
	var $line = 1
	var $line_start = 0
	var $index = 0
	while $index < offset and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' {
			$line = $line + 1
			$line_start = $index + 1
		} else if byte == '\r' {
			if $index + 1 < offset and bytes.get($index + 1) == Ok('\n') {
				$index = $index + 1
			}
			$line = $line + 1
			$line_start = $index + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: offset - $line_start + 1 }
}

test : Input -> Fuzz.Outcome
test = |input| {
	options = options_of(input.choice)
	actual = JsonValue.parse_with(input.text, options)
	combinator = Utf8.parse_str(JsonValue.parser_with(options), input.text)
	match input.expected {
		Value(expected) => {
			if actual != Ok(expected) {
				crash "round trip mismatch\n${describe(input)}\n--- actual ---\n${show_result(actual)}"
			}
			if combinator != Ok(expected) {
				crash "JsonValue.parser_with disagrees with JsonValue.parse_with: ${Str.inspect(combinator)}\n${describe(input)}"
			}
			check_writer(input, expected)
			check_accessors(expected, expected, [])
			if input.tight {
				check_one_past(input, expected)
			}
		}
		Failure({ offset, message, exact }) => {
			matches = |found| if (exact) found == message else Str.starts_with(found, message)
			location = line_column(input.text.to_utf8(), offset)
			match actual {
				Err(InvalidJsonValue(error)) if error.line == location.line and error.column == location.column and matches(error.message) => {}
				_ => crash "expected an error at ${location.line.to_str()}:${location.column.to_str()}\n${describe(input)}\n--- actual ---\n${show_result(actual)}"
			}
			match combinator {
				Err(ParseError(problem)) if problem.offset == offset and matches(problem.message) => {}
				_ => crash "JsonValue.parser_with must fail at offset ${offset.to_str()}: ${Str.inspect(combinator)}\n${describe(input)}"
			}
		}
	}
	strict_result = JsonValue.parse_str(input.text)
	if strict_result.is_ok() == input.relaxed {
		crash "strict parse_str must fail exactly when relaxed syntax was written (written: ${Str.inspect(input.relaxed)}): ${show_result(strict_result)}\n${describe(input)}"
	}
	Fuzz.keep
}

check_writer : Input, JsonValue -> {}
check_writer = |input, expected| {
	if input.non_finite {
		for written in [JsonValue.to_str(expected), JsonValue.to_str_with(expected, { indent: 2 })] {
			match written {
				Err(InvalidNumber(_)) => {}
				_ => crash "writing NaN or Infinity must fail with InvalidNumber: ${Str.inspect(written)}\n${describe(input)}"
			}
		}
	} else {
		if JsonValue.to_str(expected) != Ok(input.canonical) {
			crash "to_str mismatch\n--- expected ---\n${Str.inspect(input.canonical)}\n--- actual ---\n${Str.inspect(JsonValue.to_str(expected))}"
		}
		if JsonValue.to_str_with(expected, { indent: 2 }) != Ok(input.pretty) {
			crash "to_str_with indent 2 mismatch\n--- expected ---\n${Str.inspect(input.pretty)}\n--- actual ---\n${Str.inspect(JsonValue.to_str_with(expected, { indent: 2 }))}"
		}
		sorted = compact(sort_keys(expected))
		if JsonValue.to_str_with(expected, { sort_keys: True }) != Ok(sorted) {
			crash "to_str_with sort_keys mismatch\n--- expected ---\n${Str.inspect(sorted)}\n--- actual ---\n${Str.inspect(JsonValue.to_str_with(expected, { sort_keys: True }))}"
		}
	}
}

## With limits set to the value's own sizes it parsed; lowering any one of
## them by one, or asking for a stricter top level, must make it fail.
check_one_past : Input, JsonValue -> {}
check_one_past = |input, expected| {
	limits = input.choice.limits
	lowered = |name, changed, message|
		match JsonValue.parse_with(input.text, options_of({ ..input.choice, limits: changed })) {
			Err(InvalidJsonValue(error)) if message.any(|start| Str.starts_with(error.message, start)) => {}
			other => crash "lowering ${name} by one must fail with ${Str.inspect(message)}: ${show_result(other)}\n${describe(input)}"
		}
	if limits.max_depth > 0 {
		less = limits.max_depth - 1
		lowered("max_depth", { ..limits, max_depth: less }, ["nesting exceeds the limit of ${less.to_str()} level"])
	}
	if limits.max_items > 0 {
		less = limits.max_items - 1
		lowered("max_items", { ..limits, max_items: less }, ["array has more than ${less.to_str()} item", "object has more than ${less.to_str()} member"])
	}
	if limits.max_string_bytes > 0 {
		lowered("max_string_bytes", { ..limits, max_string_bytes: limits.max_string_bytes - 1 }, [""])
	}
	if limits.max_number_digits > 0 and limits.max_number_digits < U64.highest {
		lowered("max_number_digits", { ..limits, max_number_digits: limits.max_number_digits - 1 }, [""])
	}
	stricter =
		match expected {
			Object(_) => Err(AlreadyStrictest)
			Array(_) => Ok(Object)
			_ => Ok(ObjectOrArray)
		}
	match stricter {
		Ok(top_level) =>
			if JsonValue.parse_with(input.text, options_of({ ..input.choice, top_level })).is_ok() {
				crash "a stricter top_level must fail\n${describe(input)}"
			}
		Err(AlreadyStrictest) => {}
	}
}

absent_key : Str
absent_key = "no such key in any generated object"

is_json_integer : Str -> Bool
is_json_integer = |lexeme| {
	digits =
		match lexeme.to_utf8() {
			['-', .. as rest] => rest
			bytes => bytes
		}
	match digits {
		['0'] => True
		[first, ..] => first >= '1' and first <= '9' and digits.all(|byte| byte >= '0' and byte <= '9')
		[] => False
	}
}

expected_i64 : Str -> Try(I64, [WrongType, OutOfRange])
expected_i64 = |lexeme| {
	if !is_json_integer(lexeme) {
		Err(WrongType)
	} else if lexeme == "-0" {
		Ok(0)
	} else {
		I64.from_str(lexeme).map_err(|_| OutOfRange)
	}
}

expected_u64 : Str -> Try(U64, [WrongType, OutOfRange])
expected_u64 = |lexeme| {
	if !is_json_integer(lexeme) {
		Err(WrongType)
	} else if lexeme == "-0" {
		Ok(0)
	} else if Str.starts_with(lexeme, "-") {
		Err(OutOfRange)
	} else {
		U64.from_str(lexeme).map_err(|_| OutOfRange)
	}
}

## The Dec for lexemes without an exponent; Err(Unknown) where this target
## does not model the conversion.
expected_dec : Str -> Try(Dec, [OutOfRange, Unknown])
expected_dec = |lexeme| {
	bytes = lexeme.to_utf8()
	if is_non_finite(lexeme) {
		Err(OutOfRange)
	} else if bytes.contains('e') or bytes.contains('E') {
		Err(Unknown)
	} else {
		unsigned = Str.drop_prefix(lexeme, "-")
		parts = Str.split_on(unsigned, ".")
		integer_digits = Str.count_utf8_bytes(parts.get(0) ?? "")
		fraction_digits = Str.count_utf8_bytes(parts.get(1) ?? "")
		if fraction_digits > 18 or integer_digits > 21 {
			Err(OutOfRange)
		} else if integer_digits > 17 {
			Err(Unknown)
		} else {
			Dec.from_str(lexeme).map_err(|_| Unknown)
		}
	}
}

kind_of : JsonValue -> [NullKind, BoolKind, NumberKind, StringKind, ArrayKind, ObjectKind]
kind_of = |value| {
	match value {
		Null => NullKind
		Bool(_) => BoolKind
		Number(_) => NumberKind
		String(_) => StringKind
		Array(_) => ArrayKind
		Object(_) => ObjectKind
	}
}

check_accessors : JsonValue, JsonValue, List(Str) -> {}
check_accessors = |root, node, path| {
	fail = |what| crash "${what} is wrong at path ${Str.inspect(path)} of ${Str.inspect(node)}"
	if root.get_path(path) != Ok(node) {
		fail("get_path")
	}
	kind = kind_of(node)
	is_string = kind == StringKind
	is_bool = kind == BoolKind
	is_number = kind == NumberKind
	is_array = kind == ArrayKind
	is_object = kind == ObjectKind
	if !is_string and node.as_str() != Err(WrongType) {
		fail("as_str")
	}
	if !is_bool and node.as_bool() != Err(WrongType) {
		fail("as_bool")
	}
	if !is_array and node.as_list() != Err(WrongType) {
		fail("as_list")
	}
	if !is_object and node.as_object() != Err(WrongType) {
		fail("as_object")
	}
	if !is_number and (node.as_i64() != Err(WrongType) or node.as_u64() != Err(WrongType) or node.as_dec() != Err(WrongType)) {
		fail("the number accessors")
	}
	if !is_object and (node.get(absent_key) != Err(Missing) or !node.keys().is_empty()) {
		fail("get or keys")
	}
	if !is_array and node.at(0) != Err(Missing) {
		fail("at")
	}
	match node {
		String(text) => {
			if node.as_str() != Ok(text) {
				fail("as_str")
			}
			if root.get_path(path.append("0")) != Err(Missing) {
				fail("get_path below a string")
			}
		}
		Bool(flag) =>
			if node.as_bool() != Ok(flag) {
				fail("as_bool")
			}
		Number(lexeme) => {
			if node.as_i64() != expected_i64(lexeme) {
				fail("as_i64 (${Str.inspect(node.as_i64())})")
			}
			if node.as_u64() != expected_u64(lexeme) {
				fail("as_u64 (${Str.inspect(node.as_u64())})")
			}
			match (expected_dec(lexeme), node.as_dec()) {
				(Ok(dec), Ok(actual)) if dec == actual => {}
				(Err(OutOfRange), Err(OutOfRange)) => {}
				(Err(Unknown), Ok(_)) | (Err(Unknown), Err(OutOfRange)) => {}
				(_, actual) => fail("as_dec (${Str.inspect(actual)})")
			}
		}
		Array(items) => {
			if node.as_list() != Ok(items) {
				fail("as_list")
			}
			if node.at(items.len()) != Err(Missing) or node.get("0") != Err(Missing) {
				fail("at past the end, or get")
			}
			var $index = 0
			for item in items {
				if node.at($index) != Ok(item) {
					fail("at(${$index.to_str()})")
				}
				check_accessors(root, item, path.append($index.to_str()))
				$index = $index + 1
			}
		}
		Object(members) => {
			if node.as_object() != Ok(members) {
				fail("as_object")
			}
			if node.keys() != members.map(|member| member.key) {
				fail("keys")
			}
			if node.get(absent_key) != Err(Missing) or node.at(0) != Err(Missing) {
				fail("get of an absent key, or at")
			}
			for member in members {
				if node.get(member.key) != Ok(member.value) {
					fail("get(${Str.inspect(member.key)})")
				}
				check_accessors(root, member.value, path.append(member.key))
			}
		}
		Null => {}
	}
}

## Display

show_result : Parsed -> Str
show_result = |result| {
	match result {
		Ok(value) => Str.inspect(value)
		Err(InvalidJsonValue(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

duplicates_flag : Duplicates -> Str
duplicates_flag = |duplicates| {
	match duplicates {
		Reject => "reject"
		CollapseIdentical => "collapse-identical"
		KeepFirst => "keep-first"
		KeepLast => "keep-last"
	}
}

## The options as review-probe flags; defaults are left out.
flags : Choice -> Str
flags = |choice| {
	limit = |name, value, default| if (value == default) [] else ["--${name} ${value.to_str()}"]
	switch = |name, on| if (on) ["--${name}"] else []
	top_level =
		match choice.top_level {
			Any => []
			ObjectOrArray => ["--top-level object-or-array"]
			Object => ["--top-level object"]
		}
	duplicates = if (choice.duplicates == Reject) [] else ["--duplicates ${duplicates_flag(choice.duplicates)}"]
	[]
		.concat(limit("max-depth", choice.limits.max_depth, no_limits.max_depth))
		.concat(limit("max-items", choice.limits.max_items, no_limits.max_items))
		.concat(limit("max-string-bytes", choice.limits.max_string_bytes, no_limits.max_string_bytes))
		.concat(limit("max-number-digits", choice.limits.max_number_digits, no_limits.max_number_digits))
		.concat(switch("comments", choice.comments))
		.concat(switch("trailing-commas", choice.trailing_commas))
		.concat(switch("bom", choice.bom))
		.concat(switch("non-finite", choice.non_finite))
		.concat(switch("control-characters", choice.control_characters))
		.concat(if (choice.replace_surrogates) ["--lone-surrogates replace"] else [])
		.concat(duplicates)
		.concat(top_level)
		.concat(switch("interoperable-numbers", choice.interoperable_numbers))
		|> Str.join_with(" ")
}

hex_byte : U8 -> Str
hex_byte = |byte| Str.from_utf8([hex_digit(U8.to_u64(byte // 16), False), hex_digit(U8.to_u64(byte % 16), False)]) ?? "00"

show_expected : Expectation -> Str
show_expected = |expected| {
	match expected {
		Value(value) => Str.inspect(value)
		Failure({ offset, message, exact }) => "error at byte ${offset.to_str()}: ${Str.inspect(message)}${if (exact) "" else "..."}"
	}
}

describe : Input -> Str
describe = |input| "--- text ---\n${Str.inspect(input.text)}\n--- expected ---\n${show_expected(input.expected)}\nJSON-OPTIONS: ${flags(input.choice)}"

## Show an input the way review_json.py's corpus mode reads it.
show : Input -> Str
show = |input| {
	hex = input.text.to_utf8().fold("", |acc, byte| Str.concat(acc, hex_byte(byte)))
	"${describe(input)}\nJSON-HEX: ${hex}"
}

target = Fuzz.target_with({
	name: "json-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show,
})
