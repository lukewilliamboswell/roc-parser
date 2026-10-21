app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.JsonValue
import parser.Utf8

## Arbitrary bytes under the default, strict RFC 8259 options.
##
## Bytes that are not UTF-8 go to JsonValue.parser, which must not crash and
## must stay inside the input. For UTF-8 input:
## - errors point at a real one-based line and byte column with a message;
## - JsonValue.parser agrees with JsonValue.parse_str: content after the value
##   is the parser's unconsumed rest, and other errors match exactly;
## - an accepted value writes back out (compact, indented and sorted) to text
##   that reads back as the same value, or its key-sorted copy, and writing
##   sorted text again changes nothing;
## - a leading newline gives the same value, or the same error one line lower;
## - wrapping the input as `[x]` or `{"k":x}` gives the wrapped value, or a
##   nesting error when the value is already at the depth limit.
##
## Inputs starting with byte 0xFF instead build a deterministic shape whose
## parameters come from the next bytes: depth and item counts at and one past
## a limit, many keys with a duplicate at the end, many members holding `[]`,
## an escape-heavy string, and unclosed nesting. Each must give exactly the
## expected value, or the expected error at the expected column.

Parsed : Try(JsonValue, [InvalidJsonValue(JsonValue.Error)])

default_depth : U64
default_depth = 128

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match bytes {
		[0xFF, .. as rest] => shapes(rest)
		_ =>
			match Str.from_utf8(bytes) {
				Err(_) => {
					check_bytes(bytes)
					Fuzz.keep
				}
				Ok(input) => {
					result = JsonValue.parse_str(input)
					check_location(bytes, result)
					check_combinator(bytes, result)
					if !Str.starts_with(input, "\u(FEFF)") {
						check_leading_newline(input, result)
					}
					match result {
						Ok(value) => {
							check_writer(value)
							check_wrapping(input, value)
						}
						Err(_) => {}
					}
					Fuzz.keep
				}
			}
	}
}

## Positions

## Line lengths in bytes, where CR LF, CR, and LF each end a line.
line_lengths : List(U8) -> List(U64)
line_lengths = |bytes| {
	var $lengths = []
	var $current = 0
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' or byte == '\r' {
			$lengths = $lengths.append($current)
			$current = 0
			$index = if (byte == '\r' and bytes.get($index + 1) == Ok('\n')) $index + 2 else $index + 1
		} else {
			$current = $current + 1
			$index = $index + 1
		}
	}
	$lengths.append($current)
}

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

check_location : List(U8), Parsed -> {}
check_location = |bytes, result| {
	match result {
		Ok(_) => {}
		Err(InvalidJsonValue(error)) => {
			if error.line == 0 or error.column == 0 {
				crash "JSON error locations must be one-based: ${show_result(result)}"
			}
			if error.message.is_empty() {
				crash "JSON errors must explain the problem"
			}
			lengths = line_lengths(bytes)
			match lengths.get(error.line - 1) {
				Err(_) => crash "error line ${error.line.to_str()} is past the last line (${lengths.len().to_str()}): ${show_result(result)}"
				Ok(length) =>
					if error.column > length + 1 {
						crash "error column ${error.column.to_str()} is past the end of line ${error.line.to_str()} (${length.to_str()} bytes): ${show_result(result)}"
					}
			}
		}
	}
}

## Combinator agreement

## Bytes that are not UTF-8: the parser's contract is only that it does not
## crash (like Xml.parser, it may read strings in such input lossily), so
## this checks that it stays inside the input.
check_bytes : List(U8) -> {}
check_bytes = |bytes| {
	match Utf8.parse_bytes_partial(JsonValue.parser, bytes) {
		Ok({ value: _, rest }) =>
			if rest.len() > bytes.len() {
				crash "JsonValue.parser returned more input than it was given"
			}
		Err(ParseError({ message, offset })) =>
			if offset > bytes.len() or message.is_empty() {
				crash "JsonValue.parser failed outside the input (offset ${offset.to_str()} of ${bytes.len().to_str()}): ${message}"
			}
	}
}

unexpected_content : Str
unexpected_content = "unexpected content after the JSON value"

check_combinator : List(U8), Parsed -> {}
check_combinator = |bytes, result| {
	combinator = Utf8.parse_bytes_partial(JsonValue.parser, bytes)
	consistent =
		match (result, combinator) {
			(Ok(value), Ok({ value: other, rest })) => rest.is_empty() and value == other
			(Err(InvalidJsonValue(error)), Ok({ value: other, rest })) => {
				consumed = bytes.len() - rest.len()
				location = line_column(bytes, consumed)
				same_place = location.line == error.line and location.column == error.column
				prefix = Str.from_utf8(bytes.sublist({ start: 0, len: consumed })) ?? ""
				!rest.is_empty() and same_place and error.message == unexpected_content and JsonValue.parse_str(prefix) == Ok(other)
			}
			(Err(InvalidJsonValue(error)), Err(ParseError({ message, offset }))) => {
				location = line_column(bytes, offset)
				location.line == error.line and location.column == error.column and message == error.message
			}
			_ => False
		}
	if !consistent {
		crash "JsonValue.parser disagrees with JsonValue.parse_str\nparse_str: ${show_result(result)}\nparser: ${Str.inspect(combinator)}"
	}
}

## Metamorphic relations

check_leading_newline : Str, Parsed -> {}
check_leading_newline = |input, result| {
	shifted = JsonValue.parse_str("\n${input}")
	consistent =
		match (result, shifted) {
			(Ok(plain), Ok(with_newline)) => plain == with_newline
			(Err(InvalidJsonValue(plain)), Err(InvalidJsonValue(with_newline))) =>
				plain.line + 1 == with_newline.line and plain.column == with_newline.column and plain.message == with_newline.message
			_ => False
		}
	if !consistent {
		crash "a leading newline changed the result\nplain:   ${show_result(result)}\nshifted: ${show_result(shifted)}"
	}
}

check_wrapping : Str, JsonValue -> {}
check_wrapping = |input, value| {
	array = JsonValue.parse_str("[${input}]")
	object = JsonValue.parse_str("{\"k\":${input}}")
	if depth_of(value) < default_depth {
		if array != Ok(Array([value])) {
			crash "wrapping in [x] gave ${show_result(array)}"
		}
		if object != Ok(Object([{ key: "k", value }])) {
			crash "wrapping in {\"k\":x} gave ${show_result(object)}"
		}
	} else {
		if !is_nesting_error(array, default_depth) or !is_nesting_error(object, default_depth) {
			crash "wrapping a value at the depth limit must fail\narray:  ${show_result(array)}\nobject: ${show_result(object)}"
		}
	}
}

is_nesting_error : Parsed, U64 -> Bool
is_nesting_error = |result, limit| {
	match result {
		Err(InvalidJsonValue(error)) => Str.starts_with(error.message, "nesting exceeds the limit of ${limit.to_str()} level")
		Ok(_) => False
	}
}

## Arrays and objects in the deepest chain, counted the way max_depth counts.
depth_of : JsonValue -> U64
depth_of = |value| {
	match value {
		Array(items) => 1 + items.fold(0, |deepest, item| larger(deepest, depth_of(item)))
		Object(members) => 1 + members.fold(0, |deepest, member| larger(deepest, depth_of(member.value)))
		_ => 0
	}
}

larger : U64, U64 -> U64
larger = |a, b| if (a > b) a else b

## Writer

check_writer : JsonValue -> {}
check_writer = |value| {
	compact =
		match JsonValue.to_str(value) {
			Ok(text) => text
			Err(problem) => crash "to_str refused a parsed value: ${Str.inspect(problem)}\n${Str.inspect(value)}"
		}
	if JsonValue.parse_str(compact) != Ok(value) {
		crash "to_str output does not read back as the same value: ${Str.inspect(compact)}"
	}
	if strip_insignificant(compact) != compact {
		crash "to_str output has whitespace outside strings: ${Str.inspect(compact)}"
	}

	match JsonValue.to_str_with(value, { indent: 2 }) {
		Ok(pretty) => {
			if JsonValue.parse_str(pretty) != Ok(value) {
				crash "indented output does not read back as the same value: ${Str.inspect(pretty)}"
			}
			if strip_insignificant(pretty) != compact {
				crash "indented output differs from to_str beyond whitespace\ncompact: ${Str.inspect(compact)}\npretty:  ${Str.inspect(pretty)}"
			}
		}
		Err(problem) => crash "to_str_with indent refused a parsed value: ${Str.inspect(problem)}"
	}

	sorted = sort_keys(value)
	match JsonValue.to_str_with(value, { sort_keys: True }) {
		Ok(text) => {
			if JsonValue.parse_str(text) != Ok(sorted) {
				crash "sorted output does not read back as the key-sorted value: ${Str.inspect(text)}"
			}
			if JsonValue.to_str(sorted) != Ok(text) or JsonValue.to_str_with(sorted, { sort_keys: True }) != Ok(text) {
				crash "writing sorted output again changed it: ${Str.inspect(text)}"
			}
			match JsonValue.to_str_with(value, { sort_keys: True, indent: 2 }) {
				Ok(sorted_pretty) =>
					if strip_insignificant(sorted_pretty) != text {
						crash "sorted indented output differs from sorted compact output\ncompact: ${Str.inspect(text)}\npretty:  ${Str.inspect(sorted_pretty)}"
					}
				Err(problem) => crash "to_str_with sort_keys and indent refused a parsed value: ${Str.inspect(problem)}"
			}
		}
		Err(problem) => crash "to_str_with sort_keys refused a parsed value: ${Str.inspect(problem)}"
	}
}

## Remove JSON whitespace outside strings.
strip_insignificant : Str -> Str
strip_insignificant = |text| {
	bytes = text.to_utf8()
	var $out = []
	var $in_string = False
	var $escaped = False
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if $in_string {
			$out = $out.append(byte)
			if $escaped {
				$escaped = False
			} else if byte == '\\' {
				$escaped = True
			} else if byte == '"' {
				$in_string = False
			}
		} else if byte != ' ' and byte != '\n' and byte != '\r' and byte != '\t' {
			$out = $out.append(byte)
			if byte == '"' {
				$in_string = True
			}
		}
		$index = $index + 1
	}
	Str.from_utf8($out) ?? ""
}

## Object members sorted by the UTF-8 bytes of their keys, recursively.
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

## Deterministic shapes

size_from : U64, U64 -> U64
size_from = |high, low| high * 64 + low + 1

## 0, 1, ... count - 1.
indexes_below : U64 -> List(U64)
indexes_below = |count| {
	var $indexes = []
	for index in 0..<count {
		$indexes = $indexes.append(index)
	}
	$indexes
}

shapes : List(U8) -> Fuzz.Outcome
shapes = |bytes| {
	a = U8.to_u64(bytes.get(1) ?? 0)
	b = U8.to_u64(bytes.get(2) ?? 0)
	match (bytes.get(0) ?? 0) % 7 {
		0 => depth_boundary(a % 64, b % 2 == 0)
		1 => depth_boundary(default_depth, b % 2 == 0)
		2 => items_boundary(a % 64, b)
		3 => duplicate_after_keys(a % 48 + 1, b)
		4 => many_empty_members(size_from(a, b))
		5 => escape_heavy(size_from(a, b))
		_ => unclosed(a % 200, b % 2 == 0)
	}
	Fuzz.keep
}

## `count` copies of an array, or of an object with key "a", around 0.
nested : U64, Bool -> { text : Str, value : JsonValue, open_width : U64 }
nested = |count, arrays| {
	open = if (arrays) "[" else "{\"a\":"
	close = if (arrays) "]" else "}"
	var $value = Number("0")
	var $index = 0
	while $index < count {
		$value = if (arrays) Array([$value]) else Object([{ key: "a", value: $value }])
		$index = $index + 1
	}
	{ text: "${Str.repeat(open, count)}0${Str.repeat(close, count)}", value: $value, open_width: Str.count_utf8_bytes(open) }
}

## Nesting exactly at a limit parses; one more level fails at its own bracket.
depth_boundary : U64, Bool -> {}
depth_boundary = |limit, arrays| {
	at = nested(limit, arrays)
	past = nested(limit + 1, arrays)
	{ at_limit, past_limit } =
		if limit == default_depth {
			{ at_limit: JsonValue.parse_str(at.text), past_limit: JsonValue.parse_str(past.text) }
		} else {
			{ at_limit: JsonValue.parse_with(at.text, { max_depth: limit }), past_limit: JsonValue.parse_with(past.text, { max_depth: limit }) }
		}
	if at_limit != Ok(at.value) {
		crash "nesting of ${limit.to_str()} with max_depth ${limit.to_str()} gave ${show_result(at_limit)}"
	}
	column = limit * past.open_width + 1
	match past_limit {
		Err(InvalidJsonValue(error)) if error.line == 1 and error.column == column and is_nesting_error(past_limit, limit) => {}
		_ => crash "nesting of ${(limit + 1).to_str()} with max_depth ${limit.to_str()} must fail at 1:${column.to_str()}, but gave ${show_result(past_limit)}"
	}
}

## Exactly `limit` items or members parse; one more fails at the first excess one.
items_boundary : U64, U64 -> {}
items_boundary = |limit, choice| {
	arrays = choice % 2 == 0
	separator = if ((choice // 2) % 2 == 0) "," else ", "
	item = |index| if (arrays) index.to_str() else "\"k${index.to_str()}\":${index.to_str()}"
	value_of = |count| {
		numbers = indexes_below(count)
		if arrays {
			Array(numbers.map(|index| Number(index.to_str())))
		} else {
			Object(numbers.map(|index| { key: "k${index.to_str()}", value: Number(index.to_str()) }))
		}
	}
	open = if (arrays) "[" else "{"
	close = if (arrays) "]" else "}"
	text_of = |count| {
		pieces = indexes_below(count).map(item)
		"${open}${Str.join_with(pieces, separator)}${close}"
	}

	at_limit = JsonValue.parse_with(text_of(limit), { max_items: limit })
	if at_limit != Ok(value_of(limit)) {
		crash "${limit.to_str()} items with max_items ${limit.to_str()} gave ${show_result(at_limit)}"
	}

	# The first excess item starts after the first `limit` items and their separators.
	before_excess = if (limit == 0) open else "${open}${Str.join_with(indexes_below(limit).map(item), separator)}${separator}"
	column = Str.count_utf8_bytes(before_excess) + 1
	message = if (arrays) "array has more than ${limit.to_str()} item" else "object has more than ${limit.to_str()} member"
	past_limit = JsonValue.parse_with(text_of(limit + 1), { max_items: limit })
	match past_limit {
		Err(InvalidJsonValue(error)) if error.line == 1 and error.column == column and Str.starts_with(error.message, message) => {}
		_ => crash "${(limit + 1).to_str()} items with max_items ${limit.to_str()} must fail at 1:${column.to_str()} with \"${message}...\", but gave ${show_result(past_limit)}"
	}
}

## `count` distinct keys and then a repeat of one of them, possibly spelled
## with an escape and with an equal or a different value, under every
## duplicate mode. More than 16 keys reaches the Dict path.
duplicate_after_keys : U64, U64 -> {}
duplicate_after_keys = |count, choice| {
	repeated = choice % count
	escaped = (choice // 64) % 2 == 1
	same_value = (choice // 128) % 2 == 1
	indexes = indexes_below(count)
	members = indexes.map(|index| { key: "k${index.to_str()}", value: Number(index.to_str()) })
	original = "{${Str.join_with(indexes.map(|index| "\"k${index.to_str()}\":${index.to_str()}"), ",")},"
	spelling = if (escaped) "\\u006b${repeated.to_str()}" else "k${repeated.to_str()}"
	{ repeat_value, repeat_text } = if (same_value) { repeat_value: Number(repeated.to_str()), repeat_text: repeated.to_str() } else { repeat_value: String("x"), repeat_text: "\"x\"" }
	text = "${original}\"${spelling}\":${repeat_text}}"
	column = Str.count_utf8_bytes(original) + 1
	key = "k${repeated.to_str()}"
	first = Object(members)
	last = Object(members.map(|member| if (member.key == key) { key, value: repeat_value } else member))

	expect_error = |result, message, mode| {
		match result {
			Err(InvalidJsonValue(error)) if error.line == 1 and error.column == column and error.message == message => {}
			_ => crash "a repeated key with ${mode} must fail at 1:${column.to_str()} with ${Str.inspect(message)}, but gave ${show_result(result)}\n${Str.inspect(text)}"
		}
	}
	expect_value = |result, expected, mode| {
		if result != Ok(expected) {
			crash "a repeated key with ${mode} gave ${show_result(result)}\n${Str.inspect(text)}"
		}
	}

	expect_error(JsonValue.parse_str(text), "duplicate key \"${key}\"", "the default options")
	collapsed = JsonValue.parse_with(text, { duplicates: CollapseIdentical })
	if same_value {
		expect_value(collapsed, first, "CollapseIdentical and an equal value")
	} else {
		expect_error(collapsed, "duplicate key \"${key}\" with a different value", "CollapseIdentical")
	}
	expect_value(JsonValue.parse_with(text, { duplicates: KeepFirst }), first, "KeepFirst")
	expect_value(JsonValue.parse_with(text, { duplicates: KeepLast }), last, "KeepLast")
}

## Many members, and many items, each holding `[]`; to_str writes the same
## compact text back.
many_empty_members : U64 -> {}
many_empty_members = |count| {
	indexes = indexes_below(count)
	object_text = "{${Str.join_with(indexes.map(|index| "\"m${index.to_str()}\":[]"), ",")}}"
	object = Object(indexes.map(|index| { key: "m${index.to_str()}", value: Array([]) }))
	array_text = "[${Str.join_with(List.repeat("[]", count), ",")}]"
	array = Array(List.repeat(Array([]), count))
	for { text, expected } in [{ text: object_text, expected: object }, { text: array_text, expected: array }] {
		parsed = JsonValue.parse_str(text)
		if parsed != Ok(expected) {
			crash "${count.to_str()} members holding [] parsed wrongly: ${show_result(parsed)}"
		}
		if JsonValue.to_str(expected) != Ok(text) {
			crash "${count.to_str()} members holding [] were written back differently"
		}
	}
}

## A long string made of every kind of escape, including a surrogate pair and
## both cases of hex digits.
escape_heavy : U64 -> {}
escape_heavy = |count| {
	escaped = "\\n\\u0041\\\"\\\\\\/\\ud83d\\uDE00\\u00E9\\t\\u001f"
	decoded = "\nA\"\\/😀é\t\u(1f)"
	written = "\\nA\\\"\\\\/😀é\\t\\u001f"
	text = "\"${Str.repeat(escaped, count)}\""
	expected = String(Str.repeat(decoded, count))
	parsed = JsonValue.parse_str(text)
	if parsed != Ok(expected) {
		crash "${count.to_str()} escape groups parsed wrongly"
	}
	if JsonValue.to_str(expected) != Ok("\"${Str.repeat(written, count)}\"") {
		crash "${count.to_str()} escape groups were written back wrongly"
	}
}

## Unclosed nesting fails where the next value should start, or at the
## bracket that passes the default depth limit.
unclosed : U64, Bool -> {}
unclosed = |count, arrays| {
	open = if (arrays) "[" else "{\"a\":"
	width = Str.count_utf8_bytes(open)
	result = JsonValue.parse_str(Str.repeat(open, count))
	if count <= default_depth {
		column = count * width + 1
		match result {
			Err(InvalidJsonValue(error)) if error.line == 1 and error.column == column and Str.starts_with(error.message, "expected a JSON value") => {}
			_ => crash "${count.to_str()} unclosed ${open} must fail at 1:${column.to_str()} with \"expected a JSON value\", but gave ${show_result(result)}"
		}
	} else {
		column = default_depth * width + 1
		match result {
			Err(InvalidJsonValue(error)) if error.line == 1 and error.column == column and is_nesting_error(result, default_depth) => {}
			_ => crash "${count.to_str()} unclosed ${open} must fail at the depth limit 1:${column.to_str()}, but gave ${show_result(result)}"
		}
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

hex_byte : U8 -> Str
hex_byte = |byte| {
	digits = "0123456789abcdef".to_utf8()
	Str.from_utf8([digits.get(U8.to_u64(byte // 16)) ?? '0', digits.get(U8.to_u64(byte % 16)) ?? '0']) ?? "00"
}

## Show an input the way review_json.py's corpus mode reads it. Shape inputs
## (first byte 0xFF) are not documents, so they carry no marker.
show : List(U8) -> Str
show = |bytes| {
	match bytes {
		[0xFF, ..] => "shape parameters ${Str.inspect(bytes)}"
		_ => {
			hex = bytes.fold("", |acc, byte| Str.concat(acc, hex_byte(byte)))
			"${Str.inspect(Str.from_utf8(bytes))}\nJSON-HEX: ${hex}"
		}
	}
}

target = Fuzz.target_with({
	name: "json-raw",
	generator: Fuzz.raw_bytes,
	test,
	show,
})
