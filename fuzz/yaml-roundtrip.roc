app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Yaml

## Round-trip property: fuzzer bytes choose a YAML value *and* how to write it
## (block or flow collections, compact sequence mappings, plain/single/double
## quoted scalars, literal block scalars with every chomping mode, comments,
## blank lines, document markers). The parser must return exactly that value.
##
## The expected value is built alongside the text, never by calling the parser,
## and every emission choice follows the YAML 1.2 core schema rather than the
## parser's own rules.

## Set to Bool.True to also emit constructs the parser is known to get wrong
## (see fuzz/README.md "Known YAML gaps"). Off by default so campaigns can
## explore past them.
known_gaps : Bool
known_gaps = Bool.False

Cur : { bytes : List(U8), pos : U64 }

Out : { value : Yaml, text : Str, cur : Cur, block_scalar : Bool }

Input : { yaml : Str, expected : Yaml }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

spaces : U64 -> Str
spaces = |count| {
	var $text = ""
	var $index = 0
	while $index < count {
		$text = Str.concat($text, " ")
		$index = $index + 1
	}
	$text
}

## Scalars

alphabet : List(Str)
alphabet = ["a", "b", "Z", "x", " ", "1", "0", ".", "-", "e", "+", ":", "#", "'", "\"", "\\", "\t", "\n", "é", "😀", ",", "[", "]", "{", "}", "~", "!", "&", "*", "|", ">", "%", "?", "@", "_", "/"]

gen_string : Cur -> { s : Str, cur : Cur }
gen_string = |start| {
	sized = pick(start, 9)
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

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

all_digits : List(U8) -> Bool
all_digits = |bytes| !bytes.is_empty() and bytes.all(is_digit)

drop_sign : List(U8) -> List(U8)
drop_sign = |bytes| {
	match bytes {
		['+', .. as rest] | ['-', .. as rest] => rest
		_ => bytes
	}
}

## YAML 1.2 core schema float: [-+]? ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? ) ( [eE] [-+]? [0-9]+ )?
is_core_float : List(U8) -> Bool
is_core_float = |bytes| {
	unsigned = drop_sign(bytes)
	{ before_exp, exp } =
		match split_once(unsigned, |b| b == 'e' or b == 'E') {
			Ok(parts) => { before_exp: parts.before, exp: Ok(parts.after) }
			Err(_) => { before_exp: unsigned, exp: Err(NoExponent) }
		}
	mantissa_ok =
		match split_once(before_exp, |b| b == '.') {
			Ok({ before, after }) => (before.is_empty() and all_digits(after)) or (all_digits(before) and (after.is_empty() or all_digits(after)))
			Err(_) => all_digits(before_exp)
		}
	exp_ok =
		match exp {
			Ok(digits) => all_digits(drop_sign(digits))
			Err(_) => Bool.True
		}
	mantissa_ok and exp_ok
}

split_once : List(U8), (U8 -> Bool) -> Try({ before : List(U8), after : List(U8) }, [NotFound])
split_once = |bytes, is_sep| {
	var $index = 0
	var $found = Err(NotFound)
	while $index < bytes.len() {
		match $found {
			Ok(_) => {}
			Err(_) => {
				if is_sep(bytes.get($index) ?? 0) {
					$found = Ok({ before: bytes.sublist({ start: 0, len: $index }), after: bytes.sublist({ start: $index + 1, len: bytes.len() - $index - 1 }) })
				}
			}
		}
		$index = $index + 1
	}
	$found
}

lower : Str -> Str
lower = |text| Str.from_utf8(text.to_utf8().map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)) ?? text

## Would a plain scalar with this text resolve to something other than a string?
resolves_non_string : Str -> Bool
resolves_non_string = |text| {
	bytes = text.to_utf8()
	low = lower(text)
	low == "null" or text == "~" or low == "true" or low == "false" or all_digits(drop_sign(bytes)) or is_core_float(bytes) or (bytes.len() > 0 and text != "~" and ((bytes.get(0) ?? 0) == '.' and (low == ".inf" or low == ".nan")))
}

contains : Str, Str -> Bool
contains = |haystack, needle| Str.contains(haystack, needle)

## Plain scalars the YAML 1.2 spec reads back as the same string.
plain_safe : Str, Bool -> Bool
plain_safe = |text, flow| {
	bytes = text.to_utf8()
	first = bytes.get(0) ?? ' '
	last = bytes.last() ?? ' '
	indicator = ['-', '?', ':', ',', '[', ']', '{', '}', '#', '&', '*', '!', '|', '>', '\'', '"', '%', '@', '`', ' ', '\t']
	flow_bad = flow and (bytes.contains(',') or bytes.contains('[') or bytes.contains(']') or bytes.contains('{') or bytes.contains('}'))
	# Quotes and brackets inside plain scalars confuse the parser's comment and
	# key scanners; float-looking strings that F64.from_str rejects fail.
	gap_bad = !known_gaps and (bytes.contains('\'') or bytes.contains('"') or bytes.contains('[') or bytes.contains(']') or bytes.contains('{') or bytes.contains('}') or float_lookalike(bytes))
	!bytes.is_empty()
	and !indicator.contains(first)
	and last != ' '
	and last != '\t'
	and last != ':'
	and !bytes.contains('\n')
	and !bytes.contains('\t')
	and !contains(text, ": ")
	and !contains(text, ":\t")
	and !contains(text, " #")
	and !contains(text, "\t#")
	and !flow_bad
	and !gap_bad
	and !resolves_non_string(text)
}

## Strings the parser's float heuristic grabs: starts with a digit or '.', only
## float characters, and contains '.', 'e' or 'E'.
float_lookalike : List(U8) -> Bool
float_lookalike = |bytes| {
	body = drop_sign(bytes)
	first = body.get(0) ?? 'x'
	(is_digit(first) or first == '.') and bytes.all(|b| is_digit(b) or b == '.' or b == 'e' or b == 'E' or b == '+' or b == '-') and (bytes.contains('.') or bytes.contains('e') or bytes.contains('E'))
}

escape_utf8 : Str -> Str
escape_utf8 = |text| {
	out = text.to_utf8().fold([], |acc, b| {
		match b {
			'\\' => acc.concat(['\\', '\\'])
			'"' => acc.concat(['\\', '"'])
			'\n' => acc.concat(['\\', 'n'])
			'\r' => acc.concat(['\\', 'r'])
			'\t' => acc.concat(['\\', 't'])
			_ => acc.append(b)
		}
	})
	Str.from_utf8(out) ?? ""
}

single_quote : Str -> Str
single_quote = |text| {
	doubled = text.to_utf8().fold([], |acc, b| if b == '\'' acc.concat(['\'', '\'']) else acc.append(b))
	"'${Str.from_utf8(doubled) ?? ""}'"
}

## Emit a string as a single-line scalar.
emit_string : Cur, Str, Bool -> { text : Str, cur : Cur }
emit_string = |start, text, flow| {
	style = pick(start, 3)
	single_ok = !text.to_utf8().contains('\n') and !text.to_utf8().contains('\r')
	chosen =
		match style.n {
			0 if plain_safe(text, flow) => text
			1 if single_ok => single_quote(text)
			_ => escape_double(text)
		}
	{ text: chosen, cur: style.cur }
}

escape_double : Str -> Str
escape_double = |text| "\"${escape_utf8(text)}\""

int_texts : List({ text : Str, value : I64 })
int_texts = [
	{ text: "0", value: 0 },
	{ text: "-0", value: 0 },
	{ text: "+7", value: 7 },
	{ text: "007", value: 7 },
	{ text: "42", value: 42 },
	{ text: "-17", value: -17 },
	{ text: "9223372036854775807", value: 9223372036854775807 },
	{ text: "-9223372036854775808", value: -9223372036854775808 },
]

float_texts : List({ text : Str, value : F64 })
float_texts = [
	{ text: "1.5", value: 1.5 },
	{ text: "-0.25", value: -0.25 },
	{ text: "1e3", value: 1000.0 },
	{ text: "1E3", value: 1000.0 },
	{ text: "1.0e+3", value: 1000.0 },
	{ text: ".5", value: 0.5 },
	{ text: "5e-1", value: 0.5 },
	{ text: "+1.0", value: 1.0 },
	{ text: "1.", value: 1.0 },
	{ text: "1.25e2", value: 125.0 },
	{ text: "-.5", value: -0.5 },
]

null_texts : List(Str)
null_texts = ["null", "Null", "NULL", "~"]

bool_texts : List({ text : Str, value : Bool })
bool_texts = [
	{ text: "true", value: Bool.True },
	{ text: "True", value: Bool.True },
	{ text: "TRUE", value: Bool.True },
	{ text: "false", value: Bool.False },
	{ text: "False", value: Bool.False },
	{ text: "FALSE", value: Bool.False },
]

## A single-line scalar. `empty_null` allows an empty value to mean null.
gen_scalar : Cur, Bool, Bool -> Out
gen_scalar = |start, flow, empty_null| {
	kind = pick(start, 8)
	choice = pick(kind.cur, 16)
	cur = choice.cur
	match kind.n {
		0 => {
			if empty_null and choice.n % 3 == 0 {
				{ value: Null, text: "", cur, block_scalar: Bool.False }
			} else {
				{ value: Null, text: null_texts.get(choice.n % null_texts.len()) ?? "null", cur, block_scalar: Bool.False }
			}
		}
		1 => {
			entry = bool_texts.get(choice.n % bool_texts.len()) ?? { text: "true", value: Bool.True }
			{ value: Bool(entry.value), text: entry.text, cur, block_scalar: Bool.False }
		}
		2 => {
			entry = int_texts.get(choice.n % int_texts.len()) ?? { text: "0", value: 0 }
			{ value: Int(entry.value), text: entry.text, cur, block_scalar: Bool.False }
		}
		3 => {
			entry = float_texts.get(choice.n % float_texts.len()) ?? { text: "1.5", value: 1.5 }
			{ value: Float(entry.value), text: entry.text, cur, block_scalar: Bool.False }
		}
		_ => {
			generated = gen_string(cur)
			emitted = emit_string(generated.cur, generated.s, flow)
			{ value: String(generated.s), text: emitted.text, cur: emitted.cur, block_scalar: Bool.False }
		}
	}
}

## Literal block scalars

split_newlines : Str -> List(Str)
split_newlines = |text| Str.split_on(text, "\n")

## A literal block scalar for a string containing a newline, or Err when the
## string cannot be written as one under the current settings.
emit_literal : Cur, Str, U64, Bool -> Try({ text : Str, cur : Cur }, [Unsupported])
emit_literal = |start, text, base_indent, explicit_ok| {
	bytes = text.to_utf8()
	parts = split_newlines(text)
	# parts = body lines followed by one "" per trailing newline.
	trailing = count_trailing_empty(parts)
	body = parts.sublist({ start: 0, len: parts.len() - trailing })
	content_lines = body.keep_if(|line| !line.is_empty())
	first_content = content_lines.first() ?? ""
	needs_explicit = Str.starts_with(first_content, " ") or Str.starts_with(first_content, "\t")
	# Tabs in leading whitespace are content, but the parser reads them as indentation.
	tab_led = body.any(leading_tab)
	blank_spaces = body.any(|line| !line.is_empty() and line.to_utf8().all(|b| b == ' ' or b == '\t'))
	if bytes.contains('\r') or !bytes.contains('\n') or body.is_empty() or (needs_explicit and !explicit_ok) or (!known_gaps and (tab_led or blank_spaces)) {
		Err(Unsupported)
	} else {
		indent_choice = pick(start, 3)
		offset = indent_choice.n + 1
		header_choice = pick(indent_choice.cur, 4)
		chomp_choice = pick(header_choice.cur, 3)
		extra_choice = pick(chomp_choice.cur, 3)
		explicit = needs_explicit or (explicit_ok and header_choice.n % 2 == 0)
		indicator = if explicit offset.to_str() else ""
		{ chomp, extra_blank } =
			if trailing == 0 {
				{ chomp: "-", extra_blank: extra_choice.n }
			} else if content_lines.is_empty() {
				# Without content lines only keep chomping preserves the breaks.
				{ chomp: "+", extra_blank: trailing - 1 }
			} else if trailing == 1 and chomp_choice.n != 0 {
				{ chomp: "", extra_blank: extra_choice.n }
			} else {
				{ chomp: "+", extra_blank: trailing - 1 }
			}
		# Indicator order is free: |2- and |-2 are both valid.
		header = if header_choice.n >= 2 "|${chomp}${indicator}" else "|${indicator}${chomp}"
		pad = spaces(base_indent + offset)
		lines = body.map(|line| if line.is_empty() "" else Str.concat(pad, line))
		blanks = List.repeat("", extra_blank)
		Ok({ text: Str.join_with([header].concat(lines).concat(blanks), "\n"), cur: extra_choice.cur })
	}
}

leading_tab : Str -> Bool
leading_tab = |line| {
	var $index = 0
	var $found = Bool.False
	bytes = line.to_utf8()
	while $index < bytes.len() and !$found and ((bytes.get($index) ?? 'x') == ' ' or (bytes.get($index) ?? 'x') == '\t') {
		$found = (bytes.get($index) ?? 'x') == '\t'
		$index = $index + 1
	}
	$found
}

count_trailing_empty : List(Str) -> U64
count_trailing_empty = |parts| {
	var $count = 0
	var $index = parts.len()
	var $done = Bool.False
	while $index > 1 and !$done {
		if (parts.get($index - 1) ?? "x").is_empty() {
			$count = $count + 1
			$index = $index - 1
		} else {
			$done = Bool.True
		}
	}
	$count
}

## Flow collections (single line)

gen_flow : Cur, U64 -> Out
gen_flow = |start, depth| {
	kind = pick(start, 4)
	if depth == 0 or kind.n < 2 {
		gen_scalar(kind.cur, Bool.True, Bool.False)
	} else if kind.n == 2 {
		count = pick(kind.cur, 4)
		var $cur = count.cur
		var $values = []
		var $texts = []
		var $index = 0
		while $index < count.n {
			item = gen_flow($cur, depth - 1)
			$values = $values.append(item.value)
			$texts = $texts.append(item.text)
			$cur = item.cur
			$index = $index + 1
		}
		sep = pick($cur, 2)
		joiner = if sep.n == 0 ", " else ","
		{ value: Sequence($values), text: "[${Str.join_with($texts, joiner)}]", cur: sep.cur, block_scalar: Bool.False }
	} else {
		count = pick(kind.cur, 4)
		var $cur = count.cur
		var $entries = []
		var $texts = []
		var $index = 0
		while $index < count.n {
			key = gen_key($cur, Bool.True)
			item = gen_flow(key.cur, depth - 1)
			$cur = item.cur
			if !$entries.any(|e| e.key == key.key) {
				$entries = $entries.append({ key: key.key, value: item.value })
				$texts = $texts.append("${key.text}: ${item.text}")
			}
			$index = $index + 1
		}
		{ value: Mapping($entries), text: "{${Str.join_with($texts, ", ")}}", cur: $cur, block_scalar: Bool.False }
	}
}

gen_key : Cur, Bool -> { key : Str, text : Str, cur : Cur }
gen_key = |start, flow| {
	generated = gen_string(start)
	style = pick(generated.cur, 3)
	key = generated.s
	single_ok = !key.to_utf8().contains('\n')
	plain_ok = plain_safe(key, flow)
	text =
		match style.n {
			0 if plain_ok => key
			1 if single_ok => single_quote(key)
			_ => escape_double(key)
		}
	{ key, text, cur: style.cur }
}

## Block collections

comment_line : Cur, U64 -> { text : Str, cur : Cur }
comment_line = |start, indent| {
	choice = pick(start, 8)
	text =
		match choice.n {
			0 => "\n${spaces(indent)}# comment: [x, y]"
			1 => "\n"
			_ => ""
		}
	{ text, cur: choice.cur }
}

## Text that follows `key:` or `-` for a value at block indentation `indent`
## (the column of the key or dash).
gen_block_value : Cur, U64, U64, Bool -> Out
gen_block_value = |start, indent, depth, in_sequence| {
	kind = pick(start, 8)
	child = indent + 2
	match kind.n {
		1 | 2 if depth > 0 => {
			mapping = gen_block_mapping(kind.cur, child, depth - 1)
			if mapping.entries_text.is_empty() {
				{ value: Mapping([]), text: " {}", cur: mapping.cur, block_scalar: Bool.False }
			} else if in_sequence and kind.n == 2 {
				# Compact form: "- key: value" with later keys at indent + 2.
				{ value: mapping.value, text: " ${Str.join_with(mapping.entries_text, "\n${spaces(child)}")}", cur: mapping.cur, block_scalar: mapping.block_scalar }
			} else {
				{ value: mapping.value, text: "\n${spaces(child)}${Str.join_with(mapping.entries_text, "\n${spaces(child)}")}", cur: mapping.cur, block_scalar: mapping.block_scalar }
			}
		}
		3 if depth > 0 => {
			sequence = gen_block_sequence(kind.cur, child, depth - 1)
			if sequence.items_text.is_empty() {
				{ value: Sequence([]), text: " []", cur: sequence.cur, block_scalar: Bool.False }
			} else {
				{ value: sequence.value, text: "\n${spaces(child)}${Str.join_with(sequence.items_text, "\n${spaces(child)}")}", cur: sequence.cur, block_scalar: sequence.block_scalar }
			}
		}
		4 if depth > 0 => {
			flow = gen_flow(kind.cur, depth)
			{ value: flow.value, text: " ${flow.text}", cur: flow.cur, block_scalar: Bool.False }
		}
		5 => {
			generated = gen_string(kind.cur)
			# Explicit indentation indicators are relative to the parent node's
			# indentation, which is unambiguous for mapping values only.
			match emit_literal(generated.cur, generated.s, indent, !in_sequence) {
				Ok(literal) => { value: String(generated.s), text: " ${literal.text}", cur: literal.cur, block_scalar: Bool.True }
				Err(_) => {
					quoted = escape_double(generated.s)
					{ value: String(generated.s), text: " ${quoted}", cur: generated.cur, block_scalar: Bool.False }
				}
			}
		}
		_ => {
			scalar = gen_scalar(kind.cur, Bool.False, Bool.True)
			note = pick(scalar.cur, 4)
			trailer = if note.n == 0 and !scalar.text.is_empty() " # note" else ""
			text = if scalar.text.is_empty() trailer else " ${scalar.text}${trailer}"
			{ value: scalar.value, text, cur: note.cur, block_scalar: Bool.False }
		}
	}
}

## Entries without their leading indentation; callers join with "\n" + indent.
gen_block_mapping : Cur, U64, U64 -> { value : Yaml, entries_text : List(Str), cur : Cur, block_scalar : Bool }
gen_block_mapping = |start, indent, depth| {
	count = pick(start, 5)
	var $cur = count.cur
	var $entries = []
	var $texts = []
	var $last_block = Bool.False
	var $index = 0
	while $index < count.n {
		key = gen_key($cur, Bool.False)
		value = gen_block_value(key.cur, indent, depth, Bool.False)
		gap = comment_line(value.cur, indent)
		$cur = gap.cur
		if !$entries.any(|e| e.key == key.key) {
			$entries = $entries.append({ key: key.key, value: value.value })
			# Blank lines after a block scalar would belong to it, so only comments.
			spacer = if value.block_scalar and gap.text == "\n" "" else gap.text
			$texts = $texts.append("${key.text}:${value.text}${spacer}")
			$last_block = value.block_scalar
		}
		$index = $index + 1
	}
	{ value: Mapping($entries), entries_text: $texts, cur: $cur, block_scalar: $last_block }
}

gen_block_sequence : Cur, U64, U64 -> { value : Yaml, items_text : List(Str), cur : Cur, block_scalar : Bool }
gen_block_sequence = |start, indent, depth| {
	count = pick(start, 5)
	var $cur = count.cur
	var $values = []
	var $texts = []
	var $last_block = Bool.False
	var $index = 0
	while $index < count.n {
		value = gen_block_value($cur, indent, depth, Bool.True)
		gap = comment_line(value.cur, indent)
		$cur = gap.cur
		$values = $values.append(value.value)
		spacer = if value.block_scalar and gap.text == "\n" "" else gap.text
		$texts = $texts.append("-${value.text}${spacer}")
		$last_block = value.block_scalar
		$index = $index + 1
	}
	{ value: Sequence($values), items_text: $texts, cur: $cur, block_scalar: $last_block }
}

## Documents

generate : List(U8) -> Input
generate = |bytes| {
	start = { bytes, pos: 0 }
	root = pick(start, 6)
	marker = pick(root.cur, 4)
	depth = 4
	body =
		match root.n {
			0 | 1 => {
				mapping = gen_block_mapping(marker.cur, 0, depth)
				if mapping.entries_text.is_empty() {
					{ value: Mapping([]), text: "{}", block_scalar: Bool.False }
				} else {
					{ value: mapping.value, text: Str.join_with(mapping.entries_text, "\n"), block_scalar: mapping.block_scalar }
				}
			}
			2 | 3 => {
				sequence = gen_block_sequence(marker.cur, 0, depth)
				if sequence.items_text.is_empty() {
					{ value: Sequence([]), text: "[]", block_scalar: Bool.False }
				} else {
					{ value: sequence.value, text: Str.join_with(sequence.items_text, "\n"), block_scalar: sequence.block_scalar }
				}
			}
			4 => {
				flow = gen_flow(marker.cur, depth)
				{ value: flow.value, text: flow.text, block_scalar: Bool.False }
			}
			_ => {
				scalar = gen_scalar(marker.cur, Bool.False, Bool.False)
				{ value: scalar.value, text: scalar.text, block_scalar: Bool.False }
			}
		}
	prefix = if marker.n % 2 == 1 "---\n" else ""
	# Keep-chomped block scalars at end of input gain an extra newline.
	ends_block = body.block_scalar and !known_gaps
	suffix = if marker.n >= 2 or ends_block "\n...\n" else "\n"
	{ yaml: "${prefix}${body.text}${suffix}", expected: body.value }
}

test : Input -> Fuzz.Outcome
test = |input| {
	match Yaml.parse_str(input.yaml) {
		Ok(actual) if actual == input.expected => Fuzz.keep
		actual => crash "round trip mismatch\n--- yaml ---\n${input.yaml}\n--- expected ---\n${Yaml.to_inspect(input.expected)}\n--- actual ---\n${show_result(actual)}"
	}
}

show_result : Try(Yaml, [YamlError(Yaml.Error)]) -> Str
show_result = |result| {
	match result {
		Ok(value) => Yaml.to_inspect(value)
		Err(YamlError(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

target = Fuzz.target_with({
	name: "yaml-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "${input.yaml}\n=> ${Yaml.to_inspect(input.expected)}",
})
