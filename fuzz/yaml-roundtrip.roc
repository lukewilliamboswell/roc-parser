app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
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

Cur : { bytes : List(U8), pos : U64 }

Out : { value : Yaml, text : Str, cur : Cur, block_scalar : Bool }

Input : { yaml : Str, expected : Yaml, record_yaml : Str, record : Rec }

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
alphabet = ["a", "b", "Z", "x", " ", "1", "0", ".", "-", "e", "+", ":", "#", "'", "\"", "\\", "\t", "\n", "é", "😀", ",", "[", "]", "{", "}", "~", "!", "&", "*", "|", ">", "%", "?", "@", "_", "/", "\u(0)", "\u(1b)", "\u(85)", "\u(a0)", "\u(2028)"]

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
			Err(_) => True
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

## Would a plain scalar with this text resolve to something other than a string?
resolves_non_string : Str -> Bool
resolves_non_string = |text| {
	bytes = text.to_utf8()
	core_words = ["null", "Null", "NULL", "~", "true", "True", "TRUE", "false", "False", "FALSE", ".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF", "-.inf", "-.Inf", "-.INF", ".nan", ".NaN", ".NAN"]
	radix = |prefix, ok| Str.starts_with(text, prefix) and bytes.len() > 2 and bytes.drop_first(2).all(ok)
	is_hex = |b| is_digit(b) or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')
	core_words.contains(text) or all_digits(drop_sign(bytes)) or is_core_float(bytes) or radix("0o", |b| b >= '0' and b <= '7') or radix("0x", is_hex)
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
	and !resolves_non_string(text)
	# At the start of a line these are document markers.
	and !(Str.starts_with(text, "---") or Str.starts_with(text, "..."))
}

escape_utf8 : Str -> Str
escape_utf8 = |text| {
	out = text.to_utf8().fold(
		[],
		|acc, b| {
			match b {
				'\\' => acc.concat(['\\', '\\'])
				'"' => acc.concat(['\\', '"'])
				'\n' => acc.concat(['\\', 'n'])
				'\r' => acc.concat(['\\', 'r'])
				'\t' => acc.concat(['\\', 't'])
				_ => acc.append(b)
			}
		},
	)
	# Characters that are not printable in YAML must use escapes; others vary.
	(Str.from_utf8(out) ?? "")
		|> Str.replace_each("\u(0)", "\\0")
		|> Str.replace_each("\u(1b)", "\\e")
		|> Str.replace_each("\u(85)", "\\N")
		|> Str.replace_each("\u(a0)", "\\_")
		|> Str.replace_each("\u(2028)", "\\L")
		|> Str.replace_each("😀", "\\U0001F600")
		|> Str.replace_each("é", "\\u00E9")
		|> Str.replace_each("/", "\\/")
}

## Strings with characters YAML only allows escaped, or line separators.
needs_escapes : Str -> Bool
needs_escapes = |text| ["\u(0)", "\u(1b)", "\u(85)", "\u(2028)"].any(|special| Str.contains(text, special))

single_quote : Str -> Str
single_quote = |text| {
	doubled = text.to_utf8().fold([], |acc, b| if b == '\'' acc.concat(['\'', '\'']) else acc.append(b))
	"'${Str.from_utf8(doubled) ?? ""}'"
}

## Emit a string as a single-line scalar.
emit_string : Cur, Str, Bool -> { text : Str, cur : Cur }
emit_string = |start, text, flow| {
	style = pick(start, 3)
	single_ok = !text.to_utf8().contains('\n') and !text.to_utf8().contains('\r') and !needs_escapes(text)
	chosen =
		match style.n {
			0 if plain_safe(text, flow) and !needs_escapes(text) => text
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
	{ text: "0x1F", value: 31 },
	{ text: "0xffffFFFF", value: 4294967295 },
	{ text: "0o17", value: 15 },
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
	{ text: ".inf", value: F64.infinity },
	{ text: "-.Inf", value: -F64.infinity },
	{ text: "1e400", value: F64.infinity },
]

null_texts : List(Str)
null_texts = ["null", "Null", "NULL", "~"]

bool_texts : List({ text : Str, value : Bool })
bool_texts = [
	{ text: "true", value: True },
	{ text: "True", value: True },
	{ text: "TRUE", value: True },
	{ text: "false", value: False },
	{ text: "False", value: False },
	{ text: "FALSE", value: False },
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
				{ value: Null, text: "", cur, block_scalar: False }
			} else {
				{ value: Null, text: null_texts.get(choice.n % null_texts.len()) ?? "null", cur, block_scalar: False }
			}
		}
		1 => {
			entry = bool_texts.get(choice.n % bool_texts.len()) ?? { text: "true", value: True }
			{ value: Bool(entry.value), text: entry.text, cur, block_scalar: False }
		}
		2 => {
			entry = int_texts.get(choice.n % int_texts.len()) ?? { text: "0", value: 0 }
			{ value: Int(entry.value), text: entry.text, cur, block_scalar: False }
		}
		3 => {
			entry = float_texts.get(choice.n % float_texts.len()) ?? { text: "1.5", value: 1.5 }
			{ value: Float(entry.value), text: entry.text, cur, block_scalar: False }
		}
		_ => {
			generated = gen_string(cur)
			emitted = emit_string(generated.cur, generated.s, flow)
			{ value: Text(generated.s), text: emitted.text, cur: emitted.cur, block_scalar: False }
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
	if needs_escapes(text) or bytes.contains('\r') or !bytes.contains('\n') or body.is_empty() or (needs_explicit and !explicit_ok) {
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

count_trailing_empty : List(Str) -> U64
count_trailing_empty = |parts| {
	var $count = 0
	var $index = parts.len()
	var $done = False
	while $index > 1 and !$done {
		if (parts.get($index - 1) ?? "x").is_empty() {
			$count = $count + 1
			$index = $index - 1
		} else {
			$done = True
		}
	}
	$count
}

## Flow collections (single line)

gen_flow : Cur, U64 -> Out
gen_flow = |start, depth| {
	kind = pick(start, 4)
	if depth == 0 or kind.n < 2 {
		gen_scalar(kind.cur, True, False)
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
		{ value: Sequence($values), text: "[${Str.join_with($texts, joiner)}]", cur: sep.cur, block_scalar: False }
	} else {
		count = pick(kind.cur, 4)
		var $cur = count.cur
		var $entries = []
		var $texts = []
		var $index = 0
		while $index < count.n {
			key = gen_key($cur, True)
			item = gen_flow(key.cur, depth - 1)
			$cur = item.cur
			if !$entries.any(|e| e.key == key.key) {
				$entries = $entries.append({ key: key.key, value: item.value })
				$texts = $texts.append("${key.text}: ${item.text}")
			}
			$index = $index + 1
		}
		{ value: Mapping($entries), text: "{${Str.join_with($texts, ", ")}}", cur: $cur, block_scalar: False }
	}
}

gen_key : Cur, Bool -> { key : Str, text : Str, cur : Cur }
gen_key = |start, flow| {
	generated = gen_string(start)
	style = pick(generated.cur, 3)
	key = generated.s
	single_ok = !key.to_utf8().contains('\n') and !needs_escapes(key)
	plain_ok = plain_safe(key, flow) and !needs_escapes(key)
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
				{ value: Mapping([]), text: " {}", cur: mapping.cur, block_scalar: False }
			} else if in_sequence and kind.n == 2 {
				# Compact form: "- key: value" with later keys at indent + 2.
				{ value: mapping.value, text: " ${Str.join_with(mapping.entries_text, "\n${spaces(child)}")}", cur: mapping.cur, block_scalar: mapping.block_scalar }
			} else {
				{ value: mapping.value, text: "\n${spaces(child)}${Str.join_with(mapping.entries_text, "\n${spaces(child)}")}", cur: mapping.cur, block_scalar: mapping.block_scalar }
			}
		}
		3 if depth > 0 => {
			form = pick(kind.cur, 3)
			# Mapping values may hold an "indentless" sequence at the key's column.
			indentless = !in_sequence and form.n == 1
			column = if indentless indent else child
			sequence = gen_block_sequence(form.cur, column, depth - 1)
			if sequence.items_text.is_empty() {
				{ value: Sequence([]), text: " []", cur: sequence.cur, block_scalar: False }
			} else if in_sequence and form.n == 2 {
				# Compact form: "- - item" with later items at indent + 2.
				{ value: sequence.value, text: " ${Str.join_with(sequence.items_text, "\n${spaces(child)}")}", cur: sequence.cur, block_scalar: sequence.block_scalar }
			} else {
				{ value: sequence.value, text: "\n${spaces(column)}${Str.join_with(sequence.items_text, "\n${spaces(column)}")}", cur: sequence.cur, block_scalar: sequence.block_scalar }
			}
		}
		4 if depth > 0 => {
			flow = gen_flow(kind.cur, depth)
			{ value: flow.value, text: " ${flow.text}", cur: flow.cur, block_scalar: False }
		}
		5 => {
			generated = gen_string(kind.cur)
			# Explicit indentation indicators are relative to the parent node's
			# indentation, which is unambiguous for mapping values only.
			match emit_literal(generated.cur, generated.s, indent, !in_sequence) {
				Ok(literal) => { value: Text(generated.s), text: " ${literal.text}", cur: literal.cur, block_scalar: True }
				Err(_) => {
					quoted = escape_double(generated.s)
					{ value: Text(generated.s), text: " ${quoted}", cur: generated.cur, block_scalar: False }
				}
			}
		}
		_ => {
			scalar = gen_scalar(kind.cur, False, True)
			note = pick(scalar.cur, 4)
			trailer = if note.n == 0 and !scalar.text.is_empty() " # note" else ""
			text = if scalar.text.is_empty() trailer else " ${scalar.text}${trailer}"
			{ value: scalar.value, text, cur: note.cur, block_scalar: False }
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
	var $last_block = False
	var $index = 0
	while $index < count.n {
		key = gen_key($cur, False)
		value = gen_block_value(key.cur, indent, depth, False)
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
	var $last_block = False
	var $index = 0
	while $index < count.n {
		value = gen_block_value($cur, indent, depth, True)
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
					{ value: Mapping([]), text: "{}", block_scalar: False }
				} else {
					{ value: mapping.value, text: Str.join_with(mapping.entries_text, "\n"), block_scalar: mapping.block_scalar }
				}
			}
			2 | 3 => {
				sequence = gen_block_sequence(marker.cur, 0, depth)
				if sequence.items_text.is_empty() {
					{ value: Sequence([]), text: "[]", block_scalar: False }
				} else {
					{ value: sequence.value, text: Str.join_with(sequence.items_text, "\n"), block_scalar: sequence.block_scalar }
				}
			}
			4 => {
				flow = gen_flow(marker.cur, depth)
				{ value: flow.value, text: flow.text, block_scalar: False }
			}
			_ => {
				scalar = gen_scalar(marker.cur, False, False)
				{ value: scalar.value, text: scalar.text, block_scalar: False }
			}
		}
	prefix = if marker.n % 2 == 1 "---\n" else ""
	suffix = if marker.n >= 2 "\n...\n" else "\n"
	typed = gen_record({ bytes, pos: 0 })
	{ yaml: "${prefix}${body.text}${suffix}", expected: body.value, record_yaml: typed.text, record: typed.value }
}

## Typed decoding

## A record type with every shape `Yaml.decode` maps: text, numbers,
## booleans, a list, a nested record and absent optional fields.
Rec : {
	name : Str,
	count : I64,
	ratio : F64,
	flag : Bool,
	tags : List(Str),
	inner : { level : U8, note : Try(Str, [Missing]) },
	gone : Try(Str, [Missing]),
}

## Decode property: fuzzer bytes choose a `Rec` value and how to write it
## (entry order, scalar styles, flow or block lists, unknown keys). Decoding
## the text into `Rec` must give back exactly that value.
gen_record : Cur -> { text : Str, value : Rec }
gen_record = |start| {
	name = gen_string(start)
	name_text = emit_string(name.cur, name.s, False)
	count = pick(name_text.cur, int_texts.len())
	count_entry = int_texts.get(count.n) ?? { text: "0", value: 0 }
	finite_floats = float_texts.keep_if(|entry| entry.value.is_finite())
	ratio = pick(count.cur, finite_floats.len())
	ratio_entry = finite_floats.get(ratio.n) ?? { text: "1.5", value: 1.5 }
	flag = pick(ratio.cur, bool_texts.len())
	flag_entry = bool_texts.get(flag.n) ?? { text: "true", value: True }
	tag_count = pick(flag.cur, 4)
	block_tags = pick(tag_count.cur, 2)

	var $cur = block_tags.cur
	var $tags = []
	var $tag_texts = []
	var $index = 0
	while $index < tag_count.n {
		tag = gen_string($cur)
		emitted = emit_string(tag.cur, tag.s, block_tags.n == 0)
		$tags = $tags.append(tag.s)
		$tag_texts = $tag_texts.append(emitted.text)
		$cur = emitted.cur
		$index = $index + 1
	}

	tags_text =
		if $tags.is_empty() {
			"tags: []"
		} else if block_tags.n == 0 {
			"tags: [${Str.join_with($tag_texts, ", ")}]"
		} else {
			"tags:\n${Str.join_with($tag_texts.map(|t| "  - ${t}"), "\n")}"
		}

	level = pick($cur, 256)
	has_note = pick(level.cur, 2)
	note = gen_string(has_note.cur)
	note_text = emit_string(note.cur, note.s, False)
	inner_text =
		if has_note.n == 0 {
			"inner:\n  level: ${level.n.to_str()}"
		} else {
			"inner:\n  note: ${note_text.text}\n  level: ${level.n.to_str()}"
		}
	extra = pick(note_text.cur, 3)
	extra_text =
		match extra.n {
			0 => []
			1 => ["unknown: [1, {a: b}]"]
			_ => ["unknown:\n  - x\n  - y: z"]
		}
	entries = [
		"name: ${name_text.text}",
		"count: ${count_entry.text}",
		"ratio: ${ratio_entry.text}",
		"flag: ${flag_entry.text}",
		tags_text,
		inner_text,
	].concat(extra_text)
	rotation = pick(extra.cur, entries.len())
	ordered = entries.drop_first(rotation.n).concat(entries.take_first(rotation.n))
	value = {
		name: name.s,
		count: count_entry.value,
		ratio: ratio_entry.value,
		flag: flag_entry.value,
		tags: $tags,
		inner: { level: U64.to_u8_wrap(level.n), note: if has_note.n == 0 Err(Missing) else Ok(note.s) },
		gone: Err(Missing),
	}

	{ text: "${Str.join_with(ordered, "\n")}\n", value }
}

test : Input -> Fuzz.Outcome
test = |input| {
	match Yaml.parse_str(input.yaml) {
		Ok(actual) if actual == input.expected => {}
		actual => crash "round trip mismatch\n--- yaml ---\n${input.yaml}\n--- expected ---\n${Yaml.to_inspect(input.expected)}\n--- actual ---\n${show_result(actual)}"
	}

	decoded : Try(Rec, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	decoded = Yaml.decode(input.record_yaml)

	if decoded == Ok(input.record) {
		Fuzz.keep
	} else {
		crash "decode mismatch\n--- yaml ---\n${input.record_yaml}\n--- expected ---\n${Str.inspect(input.record)}\n--- actual ---\n${Str.inspect(decoded)}"
	}
}

show_result : Try(Yaml, [InvalidYaml(Yaml.Error)]) -> Str
show_result = |result| {
	match result {
		Ok(value) => Yaml.to_inspect(value)
		Err(InvalidYaml(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

target = Fuzz.target_with({
	name: "yaml-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "${input.yaml}\n=> ${Yaml.to_inspect(input.expected)}",
})
