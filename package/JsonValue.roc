import Parser
import Utf8
import Literal as Quotation

## A dynamic JSON value and parser based on RFC 8259.
##
## Use this module when a record type cannot describe a document: keys that
## are not Roc identifiers (such as JSON-LD's `@context`), arrays whose
## objects differ in shape, JSON Schema files, or data that must pass through
## untouched. To decode JSON straight into your own record type, use the
## builtin `Json` instead.
##
## A [JsonValue] is one of:
##
## - `Null`, `Bool` and `String`;
## - `Number`, holding the exact number text from the input, so integers of any
##   size and decimals of any precision survive unchanged. Convert it with
##   [JsonValue.as_i64], [JsonValue.as_u64] or [JsonValue.as_dec];
## - `Array`, and `Object`, whose members keep their source order.
##
## [JsonValue.parse_str] is strict: it accepts exactly the JSON of RFC 8259
## and rejects duplicate keys. [JsonValue.parse_with] takes [JsonValue.Options]
## to accept common relaxations (comments, trailing commas, a byte order
## mark, `NaN` and `Infinity`, lenient strings), to add checks (I-JSON
## numbers, the top-level shape, size guards), and to choose what a repeated
## key means. Nesting is limited to 128 levels by default.
##
## [JsonValue.to_str] and [JsonValue.to_str_with] write a value back out as
## compact or indented JSON, refusing values that would not read back the same.
##
## Strings and numbers read from the input share its bytes instead of being
## copied, so a value keeps the whole input alive while any of them is.
##
## ```roc
## expect {
##     doc = JsonValue.parse_str("{\"@context\": \"https://spdx.org/rdf/3.0.1/spdx-context.jsonld\", \"@graph\": []}")?
##     doc.keys() == ["@context", "@graph"]
## }
## ```
JsonValue := [
	Null,
	Bool(Bool),
	Number(Str),
	String(Str),
	Array(List(JsonValue)),
	Object(List({ key : Str, value : JsonValue })),
].{

	## Location and explanation of invalid JSON input. Lines and columns are
	## one-based; a column counts bytes from the start of its line.
	Error : { line : U64, column : U64, message : Str }

	## Compare two JSON values structurally. Numbers compare by their text, so
	## `Number("1.0")` and `Number("1")` differ.
	is_eq : _

	## Hash a JSON value, so values can be `Dict` keys and `Set` members.
	to_hash : _

	## How [JsonValue.parse_with] reads a document. Omitted fields use the
	## defaults shown, which together are strict RFC 8259.
	##
	## Limits:
	##
	## - `max_depth`: the most arrays and objects that may be nested;
	## - `max_items`: the most items in one array or members in one object;
	## - `max_string_bytes`: the most UTF-8 bytes in one decoded string or key;
	## - `max_number_digits`: the most characters in one number.
	##
	## Relaxations, each off by default:
	##
	## - `comments`: `//` line comments and `/* */` block comments wherever
	##   whitespace may appear;
	## - `trailing_commas`: a comma after the last item or member;
	## - `bom`: skip a leading byte order mark (U+FEFF);
	## - `non_finite`: `NaN`, `Infinity` and `-Infinity`, kept as `Number` text;
	## - `control_characters`: unescaped U+0000 to U+001F inside strings;
	## - `lone_surrogates`: `Replace` turns an unpaired `\u` surrogate into
	##   U+FFFD instead of rejecting it.
	##
	## Checks:
	##
	## - `duplicates`: what a key repeated in one object means. Keys are
	##   compared after unescaping. `Reject` refuses it. `CollapseIdentical`
	##   keeps one copy of a key repeated with an equal value and refuses one
	##   repeated with a different value. `KeepFirst` ignores later copies.
	##   `KeepLast` keeps the first position with the last value, as
	##   JavaScript and Python do;
	## - `top_level`: require the document to be an `Object`, or an
	##   `ObjectOrArray`;
	## - `interoperable_numbers`: refuse numbers that an IEEE 754 double cannot
	##   hold, as I-JSON (RFC 7493) requires: integers beyond ±(2^53−1), more
	##   than 17 significant digits, and values that overflow or underflow.
	##
	## ```roc
	## expect JsonValue.parse_with("[1, 2,] // done", { comments: True, trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
	## ```
	Options := {
		max_depth : U64 ?? 128,
		max_items : U64 ?? U64.highest,
		max_string_bytes : U64 ?? U64.highest,
		max_number_digits : U64 ?? U64.highest,
		comments : Bool ?? False,
		trailing_commas : Bool ?? False,
		bom : Bool ?? False,
		non_finite : Bool ?? False,
		control_characters : Bool ?? False,
		lone_surrogates : [Reject, Replace] ?? Reject,
		duplicates : [Reject, CollapseIdentical, KeepFirst, KeepLast] ?? Reject,
		top_level : [Any, ObjectOrArray, Object] ?? Any,
		interoperable_numbers : Bool ?? False,
	}

	## Checked source, its name for errors, and the [JsonValue.Options] to read it with.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Options ?? {} }

	## Parse a complete document during compilation, reporting its source name
	## and position. It accepts exactly what [JsonValue.parse_with] accepts with
	## the same options.
	validate : ValidationProps -> Try(JsonValue, [BadQuotedBytes(Str)])
	validate = |props| {
		JsonValue.parse_with(props.source, props.options)
			.map_err(|InvalidJson(error)| Quotation.error({ name: props.name, format: "JSON", message: error.message, line: error.line, column: error.column }))
	}

	## Use a strict JSON document as a quoted value.
	from_quote : Str -> Try(JsonValue, [BadQuotedBytes(Str)])
	from_quote = |source| JsonValue.validate({ source: source })

	## Parse one complete, strict RFC 8259 document. Whitespace may surround the
	## value; anything else after it is an error.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": [true, null]}") == Ok(Object([{ key: "a", value: Array([Bool(True), Null]) }]))
	##
	## expect JsonValue.parse_str("[1,]") == Err(InvalidJson({ line: 1, column: 3, message: "trailing comma is not allowed" }))
	## ```
	parse_str : Str -> Try(JsonValue, [InvalidJson(Error)])
	parse_str = |input| JsonValue.parse_with(input, {})

	## Parse one complete document with the given [JsonValue.Options].
	##
	## ```roc
	## expect JsonValue.parse_with("{\"a\": 1, \"a\": 2}", { duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("2") }]))
	## ```
	parse_with : Str, Options -> Try(JsonValue, [InvalidJson(Error)])
	parse_with = |input, options| {
		match scan(input, input.to_utf8(), options) {
			Ok({ value, pos }) =>
				if pos == input.count_utf8_bytes() {
					Ok(value)
				} else {
					Err(InvalidJson(locate(input, pos, "unexpected content after the JSON value")))
				}

			Err(JsonFail(failure)) => Err(InvalidJson(locate(input, failure.offset, failure.message)))
		}
	}

	## A [Parser] that reads one strict JSON value and the whitespace around
	## it, and leaves the rest of the input for the next parser. A failure is
	## `ParseError({ message, offset })` with the byte offset of the problem.
	##
	## ```roc
	## expect Utf8.parse_str_partial(JsonValue.parser, "[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
	##
	## expect Utf8.parse_str(JsonValue.parser, "[1 2]") == Err(ParseError({ message: "expected ',' or ']' after an array item", offset: 3 }))
	## ```
	parser : Parser(Utf8.Bytes, JsonValue)
	parser = JsonValue.parser_with({})

	## Like [JsonValue.parser], with the given [JsonValue.Options].
	##
	## ```roc
	## expect Utf8.parse_str(JsonValue.parser_with({ comments: True }), "1 // one") == Ok(Number("1"))
	## ```
	parser_with : Options -> Parser(Utf8.Bytes, JsonValue)
	parser_with = |options|
		Parser.custom(
			|input| {
				# Validate once, so strings can be cut from the input without
				# validating each one again.
				match scan(Str.from_utf8(input) ?? "", input, options) {
					Ok({ value, pos }) => Ok({ value, rest: input.drop_first(pos) })
					Err(JsonFail(failure)) => Err(ParseError({ message: failure.message, offset: failure.offset }))
				}
			},
		)

	## The value of the first member named `key` in an object. Anything else,
	## or an object without that key, is `Err(Missing)`.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": 1}").map_ok(|doc| doc.get("a")) == Ok(Ok(Number("1")))
	## ```
	get : JsonValue, Str -> Try(JsonValue, [Missing])
	get = |value, key| {
		match value {
			Object(members) =>
				match members.find_first(|member| member.key == key) {
					Ok(member) => Ok(member.value)
					Err(_) => Err(Missing)
				}

			_ => Err(Missing)
		}
	}

	## The item at a zero-based `index` of an array. Anything else, or an index
	## past the end, is `Err(Missing)`.
	##
	## ```roc
	## expect JsonValue.parse_str("[10, 20]").map_ok(|doc| doc.at(1)) == Ok(Ok(Number("20")))
	## ```
	at : JsonValue, U64 -> Try(JsonValue, [Missing])
	at = |value, index| {
		match value {
			Array(items) =>
				match items.get(index) {
					Ok(item) => Ok(item)
					Err(_) => Err(Missing)
				}

			_ => Err(Missing)
		}
	}

	## Follow a path of object keys and array indexes. A segment applied to an
	## array must be a decimal index such as `"0"`, without a sign or leading
	## zeros, as in a JSON Pointer (RFC 6901).
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": [{\"b\": true}]}").map_ok(|doc| doc.get_path(["a", "0", "b"])) == Ok(Ok(Bool(True)))
	## ```
	get_path : JsonValue, List(Str) -> Try(JsonValue, [Missing])
	get_path = |value, path| {
		var $current = value
		for segment in path {
			$current =
				match $current {
					Object(_) => JsonValue.get($current, segment)?
					Array(_) => JsonValue.at($current, array_index(segment)?)?
					_ => return Err(Missing)
				}
		}
		Ok($current)
	}

	## The keys of an object, in order; empty for anything else.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"b\": 1, \"a\": 2}").map_ok(|doc| doc.keys()) == Ok(["b", "a"])
	## ```
	keys : JsonValue -> List(Str)
	keys = |value| {
		match value {
			Object(members) => members.map(|member| member.key)
			_ => []
		}
	}

	## The text of a `String` value, or `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("\"caf\\u00e9\"").map_ok(|doc| doc.as_str()) == Ok(Ok("café"))
	## ```
	as_str : JsonValue -> Try(Str, [WrongType])
	as_str = |value| {
		match value {
			String(text) => Ok(text)
			_ => Err(WrongType)
		}
	}

	## The boolean of a `Bool` value, or `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("false").map_ok(|doc| doc.as_bool()) == Ok(Ok(False))
	## ```
	as_bool : JsonValue -> Try(Bool, [WrongType])
	as_bool = |value| {
		match value {
			Bool(flag) => Ok(flag)
			_ => Err(WrongType)
		}
	}

	## The items of an `Array` value, or `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("[null]").map_ok(|doc| doc.as_list()) == Ok(Ok([Null]))
	## ```
	as_list : JsonValue -> Try(List(JsonValue), [WrongType])
	as_list = |value| {
		match value {
			Array(items) => Ok(items)
			_ => Err(WrongType)
		}
	}

	## The members of an `Object` value in order, or `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": null}").map_ok(|doc| doc.as_object()) == Ok(Ok([{ key: "a", value: Null }]))
	## ```
	as_object : JsonValue -> Try(List({ key : Str, value : JsonValue }), [WrongType])
	as_object = |value| {
		match value {
			Object(members) => Ok(members)
			_ => Err(WrongType)
		}
	}

	## The integer of a `Number` written as a JSON integer, with no fraction or
	## exponent. Other values, including `1.0` and `1e3`, are `Err(WrongType)`;
	## an integer outside the `I64` range is `Err(OutOfRange)`.
	##
	## ```roc
	## expect JsonValue.parse_str("-42").map_ok(|doc| doc.as_i64()) == Ok(Ok(-42))
	##
	## expect JsonValue.parse_str("1e3").map_ok(|doc| doc.as_i64()) == Ok(Err(WrongType))
	## ```
	as_i64 : JsonValue -> Try(I64, [WrongType, OutOfRange])
	as_i64 = |_value| crash "JsonValue.as_i64 is not implemented yet"

	## Like [JsonValue.as_i64], for `U64`. A negative integer other than `-0`
	## is `Err(OutOfRange)`.
	##
	## ```roc
	## expect JsonValue.parse_str("18446744073709551615").map_ok(|doc| doc.as_u64()) == Ok(Ok(18446744073709551615))
	##
	## expect JsonValue.parse_str("-1").map_ok(|doc| doc.as_u64()) == Ok(Err(OutOfRange))
	## ```
	as_u64 : JsonValue -> Try(U64, [WrongType, OutOfRange])
	as_u64 = |_value| crash "JsonValue.as_u64 is not implemented yet"

	## The value of a `Number` as a `Dec`. A number outside the `Dec` range,
	## with more than 18 decimal places, or `NaN` or `Infinity`, is
	## `Err(OutOfRange)`; anything else is `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("-1.25e+2").map_ok(|doc| doc.as_dec()) == Ok(Ok(-125.0))
	## ```
	as_dec : JsonValue -> Try(Dec, [WrongType, OutOfRange])
	as_dec = |_value| crash "JsonValue.as_dec is not implemented yet"

	## How [JsonValue.to_str_with] writes a value. `sort_keys` writes object
	## members in byte order of their keys, recursively. `indent` greater than
	## zero writes each item and member on its own line, indented by that many
	## spaces per level.
	##
	## ```roc
	## expect JsonValue.to_str_with(Object([{ key: "b", value: Null }, { key: "a", value: Array([]) }]), { sort_keys: True, indent: 2 }) == Ok("{\n  \"a\": [],\n  \"b\": null\n}")
	## ```
	WriteOptions := { sort_keys : Bool ?? False, indent : U64 ?? 0 }

	## Write a value as compact JSON, keeping member order.
	##
	## A value built by hand can be one that JSON cannot express: a `Number`
	## whose text is not a JSON number (including `NaN` and `Infinity`) is
	## `InvalidNumber`, a key repeated in one object is `DuplicateKey`, and
	## more than 128 levels of nesting is `TooDeep`.
	##
	## ```roc
	## expect JsonValue.to_str(Object([{ key: "a", value: Number("1") }, { key: "b", value: Array([Bool(True), Null]) }])) == Ok("{\"a\":1,\"b\":[true,null]}")
	##
	## expect JsonValue.to_str(Number("NaN")) == Err(InvalidNumber("NaN"))
	## ```
	to_str : JsonValue -> Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep])
	to_str = |value| JsonValue.to_str_with(value, {})

	## Write a value with the given [JsonValue.WriteOptions].
	##
	## ```roc
	## expect JsonValue.to_str_with(Array([Number("1"), String("two")]), { indent: 4 }) == Ok("[\n    1,\n    \"two\"\n]")
	## ```
	to_str_with : JsonValue, WriteOptions -> Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep])
	to_str_with = |_value, _options| crash "JsonValue.to_str_with is not implemented yet"
}

# ------------------------------------------------------------------ reading

Failure : { offset : U64, message : Str }

fail : U64, Str -> Try(_, [JsonFail(Failure)])
fail = |offset, message| Err(JsonFail({ offset, message }))

locate : Str, U64, Str -> JsonValue.Error
locate = |input, offset, message| {
	position = Quotation.position(input, offset)
	{ line: position.line, column: position.column, message }
}

## An array or object whose closing bracket has not been read yet, saved
## while a nested one is open: what kind it is, where its children start in
## the shared `items` or `members` list, and its pending member key.
Frame : {
	container : Container,
	first : U64,
	key : Str,
	key_at : U64,
}

## Where a completed value goes: it is the document, an array item, or the
## value of the pending object member.
Container : [Top, InArray, InObject]

Member : { key : Str, value : JsonValue }

# Scan one JSON value and the whitespace (and comments) after it. `src` is the
# input as a `Str` when it is valid UTF-8, or "" when it is not; strings are
# then cut from it instead of being validated one by one.
#
# Arrays and objects are scanned with an explicit stack, so deep nesting
# cannot exhaust the call stack. The items of every open array live in one
# flat `$items` list and the members of every open object in one flat
# `$members` list (with their key offsets in `$key_ats`); each frame remembers
# where its children start. Both lists are plain variables that nothing else
# refers to, so appending updates them in place; a container's children are
# copied out once, when it closes.
scan : Str, List(U8), JsonValue.Options -> Try({ value : JsonValue, pos : U64 }, [JsonFail(Failure)])
scan = |src, bytes, options| {
	check = src.is_empty()
	comments = options.comments
	max_depth = options.max_depth
	max_items = options.max_items
	non_finite = options.non_finite
	start = skip_bom(bytes, options.bom)?
	var $pos = skip_ws(bytes, start, comments, check)?
	check_top_level(bytes, $pos, options)?
	var $open : List(Frame)
	var $open = []
	var $items : List(JsonValue)
	var $items = []
	var $members : List(Member)
	var $members = []
	var $key_ats : List(U64)
	var $key_ats = []
	var $container : Container
	var $container = Top
	var $first = 0
	var $key = ""
	var $key_at = 0
	var $value = Null
	# Whether `$value` holds a value just completed, which goes to the open
	# container next; otherwise a value starts at `$pos`.
	var $have = False
	while True {
		if $have {
			match $container {
				Top => {
					end = skip_ws(bytes, $pos, comments, check)?
					return Ok({ value: $value, pos: end })
				}

				InArray => {
					$items = $items.append($value)
					after = skip_ws(bytes, $pos, comments, check)?
					byte = bytes.get(after) ?? 0
					var $close = after
					if byte == ',' {
						next = skip_ws(bytes, after + 1, comments, check)?
						if bytes.get(next) == Ok(']') {
							if !options.trailing_commas {
								return fail(after, "trailing comma is not allowed")
							}
							$close = next
						} else if $items.len() - $first >= max_items {
							return fail(next, "array has more than ${max_items.to_str()} items")
						} else {
							$have = False
							$pos = next
						}
					} else if byte != ']' {
						return fail(after, "expected ',' or ']' after an array item")
					}
					if $have {
						$value = Array(copy_from($items, $first))
						$items = truncate($items, $first)
						$pos = $close + 1
						# Pop the frame after the match on it: popping inside the
						# match left the stack shared, so the next push copied it.
						match $open.last() {
							Ok(parent) => {
								$container = parent.container
								$first = parent.first
								$key = parent.key
								$key_at = parent.key_at
							}

							Err(_) => {}
						}
						$open = $open.drop_last(1)
					}
				}

				InObject => {
					$members = $members.append({ key: $key, value: $value })
					$key_ats = $key_ats.append($key_at)
					after = skip_ws(bytes, $pos, comments, check)?
					byte = bytes.get(after) ?? 0
					var $close = after
					if byte == ',' {
						next = skip_ws(bytes, after + 1, comments, check)?
						if bytes.get(next) == Ok('}') {
							if !options.trailing_commas {
								return fail(after, "trailing comma is not allowed")
							}
							$close = next
						} else if $members.len() - $first >= max_items {
							return fail(next, "object has more than ${max_items.to_str()} members")
						} else {
							member = scan_key(src, bytes, next, options)?
							$key = member.key
							$key_at = next
							$have = False
							$pos = member.pos
						}
					} else if byte != '}' {
						return fail(after, "expected ',' or '}' after an object member")
					}
					if $have {
						$value = Object(close_members($members, $key_ats, $first, options.duplicates)?)
						$members = truncate($members, $first)
						$key_ats = truncate($key_ats, $first)
						$pos = $close + 1
						match $open.last() {
							Ok(parent) => {
								$container = parent.container
								$first = parent.first
								$key = parent.key
								$key_at = parent.key_at
							}

							Err(_) => {}
						}
						$open = $open.drop_last(1)
					}
				}
			}
		} else {
			byte = bytes.get($pos) ?? 0
			if byte == '"' {
				string = scan_string(src, bytes, $pos, options)?
				$value = String(string.str)
				$pos = string.end
				$have = True
			} else if byte == '[' {
				if $open.len() >= max_depth {
					return fail($pos, "nesting exceeds the limit of ${max_depth.to_str()} levels")
				}
				next = skip_ws(bytes, $pos + 1, comments, check)?
				if bytes.get(next) == Ok(']') {
					$value = Array([])
					$pos = next + 1
					$have = True
				} else if max_items == 0 {
					return fail(next, "array has more than 0 items")
				} else {
					$open = $open.append({ container: $container, first: $first, key: $key, key_at: $key_at })
					$container = InArray
					$first = $items.len()
					$pos = next
				}
			} else if byte == '{' {
				if $open.len() >= max_depth {
					return fail($pos, "nesting exceeds the limit of ${max_depth.to_str()} levels")
				}
				next = skip_ws(bytes, $pos + 1, comments, check)?
				if bytes.get(next) == Ok('}') {
					$value = Object([])
					$pos = next + 1
					$have = True
				} else if max_items == 0 {
					return fail(next, "object has more than 0 members")
				} else {
					member = scan_key(src, bytes, next, options)?
					$open = $open.append({ container: $container, first: $first, key: $key, key_at: $key_at })
					$container = InObject
					$first = $members.len()
					$key = member.key
					$key_at = next
					$pos = member.pos
				}
			} else if byte == 't' {
				if at3(bytes, $pos + 1, 'r', 'u', 'e') {
					$value = Bool(True)
					$pos = $pos + 4
					$have = True
				} else {
					return fail($pos, "invalid literal: expected true, false or null")
				}
			} else if byte == 'f' {
				if at3(bytes, $pos + 1, 'a', 'l', 's') and bytes.get($pos + 4) == Ok('e') {
					$value = Bool(False)
					$pos = $pos + 5
					$have = True
				} else {
					return fail($pos, "invalid literal: expected true, false or null")
				}
			} else if byte == 'n' {
				if at3(bytes, $pos + 1, 'u', 'l', 'l') {
					$value = Null
					$pos = $pos + 4
					$have = True
				} else {
					return fail($pos, "invalid literal: expected true, false or null")
				}
			} else if byte == '-' or (byte >= '0' and byte <= '9') or (non_finite and (byte == 'N' or byte == 'I')) {
				number = scan_number(src, bytes, $pos, options)?
				$value = Number(number.str)
				$pos = number.end
				$have = True
			} else {
				return fail($pos, "expected a JSON value")
			}
		}
	}
}

## Whether the three bytes from `pos` are `a`, `b` and `c`.
at3 : List(U8), U64, U8, U8, U8 -> Bool
at3 = |bytes, pos, a, b, c| {
	bytes.get(pos) == Ok(a) and bytes.get(pos + 1) == Ok(b) and bytes.get(pos + 2) == Ok(c)
}

## The position after a leading byte order mark, if one is allowed.
skip_bom : List(U8), Bool -> Try(U64, [JsonFail(Failure)])
skip_bom = |bytes, allowed| {
	if at3(bytes, 0, 0xEF, 0xBB, 0xBF) {
		if allowed {
			Ok(3)
		} else {
			fail(0, "byte order mark is not allowed")
		}
	} else {
		Ok(0)
	}
}

## Refuse a document whose value at `pos` has the wrong shape for `top_level`.
## Bytes that start no value are left for the scanner to report.
check_top_level : List(U8), U64, JsonValue.Options -> Try({}, [JsonFail(Failure)])
check_top_level = |bytes, pos, options| {
	byte = bytes.get(pos) ?? 0
	scalar =
		byte == '"'
			or byte == '-'
			or (byte >= '0' and byte <= '9')
			or byte == 't'
			or byte == 'f'
			or byte == 'n'
			or (options.non_finite and (byte == 'N' or byte == 'I'))
	match options.top_level {
		Any => Ok({})
		ObjectOrArray =>
			if scalar {
				fail(pos, "top-level value must be an object or an array")
			} else {
				Ok({})
			}

		Object =>
			if scalar or byte == '[' {
				fail(pos, "top-level value must be an object")
			} else {
				Ok({})
			}
	}
}

## The key of an object member starting at `start`, and the position after
## the `:` that follows it and any whitespace after that.
scan_key : Str, List(U8), U64, JsonValue.Options -> Try({ key : Str, pos : U64 }, [JsonFail(Failure)])
scan_key = |src, bytes, start, options| {
	if bytes.get(start) != Ok('"') {
		return fail(start, "expected a string key")
	}
	string = scan_string(src, bytes, start, options)?
	colon = skip_ws(bytes, string.end, options.comments, src.is_empty())?
	if bytes.get(colon) != Ok(':') {
		return fail(colon, "expected ':' after an object key")
	}
	Ok({ key: string.str, pos: skip_ws(bytes, colon + 1, options.comments, src.is_empty())? })
}

## The members of the object whose members start at `first`, with repeated
## keys handled as `duplicates` says. Most objects are small, so their keys
## are compared one by one; larger objects index their keys in a `Dict`.
close_members : List(Member), List(U64), U64, [Reject, CollapseIdentical, KeepFirst, KeepLast] -> Try(List(Member), [JsonFail(Failure)])
close_members = |members, key_ats, first, duplicates| {
	count = members.len() - first
	var $out = List.with_capacity(count)
	var $index = first
	if count <= small_object {
		while $index < members.len() {
			match members.get($index) {
				Ok(member) => {
					match $out.find_first_index(|kept| kept.key == member.key) {
						Err(_) => {
							$out = $out.append(member)
						}

						Ok(earlier) => {
							match duplicate_action($out, earlier, member, duplicates) {
								Ok(Skip) => {}
								Ok(Replace) => {
									# `earlier` indexes `$out`, so the update is always in bounds.
									$out = $out.set(earlier, { key: member.key, value: member.value }) ?? []
								}

								Err(message) => return fail(key_ats.get($index) ?? 0, message)
							}
						}
					}
				}

				Err(_) => {}
			}
			$index = $index + 1
		}
	} else {
		var $seen = Dict.with_capacity(count)
		while $index < members.len() {
			match members.get($index) {
				Ok(member) => {
					match $seen.get(member.key) {
						Err(_) => {
							$seen = $seen.insert(member.key, $out.len())
							$out = $out.append(member)
						}

						Ok(earlier) => {
							match duplicate_action($out, earlier, member, duplicates) {
								Ok(Skip) => {}
								Ok(Replace) => {
									# `earlier` indexes `$out`, so the update is always in bounds.
									$out = $out.set(earlier, { key: member.key, value: member.value }) ?? []
								}

								Err(message) => return fail(key_ats.get($index) ?? 0, message)
							}
						}
					}
				}

				Err(_) => {}
			}
			$index = $index + 1
		}
	}
	Ok($out)
}

## The most members an object may have for its keys to be compared one by one.
small_object : U64
small_object = 16

## What to do with `member` when `out[earlier]` has the same key.
duplicate_action : List(Member), U64, Member, [Reject, CollapseIdentical, KeepFirst, KeepLast] -> Try([Skip, Replace], Str)
duplicate_action = |out, earlier, member, duplicates| {
	match duplicates {
		Reject => Err("duplicate key ${quote(member.key)}")
		KeepFirst => Ok(Skip)
		KeepLast => Ok(Replace)
		CollapseIdentical =>
			match out.get(earlier) {
				Ok(kept) if kept.value == member.value => Ok(Skip)
				_ => Err("duplicate key ${quote(member.key)} with a different value")
			}
	}
}

## A fresh copy of the items from `start` on, so the shared list stays
## uniquely referenced.
copy_from : List(a), U64 -> List(a)
copy_from = |items, start| {
	var $out = List.with_capacity(items.len() - start)
	var $index = start
	while $index < items.len() {
		match items.get($index) {
			Ok(item) => {
				$out = $out.append(item)
			}

			Err(_) => {}
		}
		$index = $index + 1
	}
	$out
}

## Drop the items from `len` on, one at a time from the end so the list
## keeps its allocation.
truncate : List(a), U64 -> List(a)
truncate = |items, len| {
	var $out = items
	while $out.len() > len {
		$out = $out.drop_last(1)
	}
	$out
}

## Skip whitespace, and comments when they are allowed, from `start`. Most
## calls find a token straight away, so one byte is checked before the scan.
skip_ws : List(U8), U64, Bool, Bool -> Try(U64, [JsonFail(Failure)])
skip_ws = |bytes, start, comments, check| {
	byte = bytes.get(start) ?? 'x'
	if byte > ' ' and byte != '/' {
		Ok(start)
	} else {
		skip_ws_and_comments(bytes, start, comments, check)
	}
}

skip_ws_and_comments : List(U8), U64, Bool, Bool -> Try(U64, [JsonFail(Failure)])
skip_ws_and_comments = |bytes, start, comments, check| {
	var $pos = start
	while True {
		byte = bytes.get($pos) ?? 'x'
		if byte == ' ' or byte == '\n' or byte == '\r' or byte == '\t' {
			$pos = Utf8.skip_class(bytes, $pos + 1, ws_class)
		} else if byte == '/' {
			if !comments {
				return fail($pos, "comments are not allowed")
			}
			next = bytes.get($pos + 1) ?? 0
			if next == '/' {
				end = Utf8.find_line_end(bytes, $pos + 2)
				if check {
					check_utf8(bytes, $pos + 2, end)?
				}
				$pos = end
			} else if next == '*' {
				close = block_comment_end(bytes, $pos + 2)
				if close >= bytes.len() {
					return fail($pos, "unterminated block comment")
				}
				if check {
					check_utf8(bytes, $pos + 2, close)?
				}
				$pos = close + 2
			} else {
				return Ok($pos)
			}
		} else {
			return Ok($pos)
		}
	}
}

## The position of the `*/` that ends a block comment, searching from
## `start`, or the input length when there is none.
block_comment_end : List(U8), U64 -> U64
block_comment_end = |bytes, start| {
	var $pos = Utf8.find_any(bytes, start, star_class)
	while $pos < bytes.len() and bytes.get($pos + 1) != Ok('/') {
		$pos = Utf8.find_any(bytes, $pos + 1, star_class)
	}
	if $pos < bytes.len() {
		$pos
	} else {
		bytes.len()
	}
}

## Fail at the first byte of [start, end) that is not valid UTF-8.
check_utf8 : List(U8), U64, U64 -> Try({}, [JsonFail(Failure)])
check_utf8 = |bytes, start, end| {
	match Str.from_utf8(bytes.sublist({ start, len: end - start })) {
		Ok(_) => Ok({})
		Err(BadUtf8({ index, .. })) => fail(start + index, "invalid UTF-8")
	}
}

## The input bytes [start, end) as a `Str`. With the validated input string
## this is a slice of it, checked only at its two ends; without it (invalid
## UTF-8 elsewhere in the input) the bytes are validated.
slice_checked : Str, List(U8), U64, U64 -> Try(Str, [JsonFail(Failure)])
slice_checked = |src, bytes, start, end| {
	if src.is_empty() {
		match Str.from_utf8(bytes.sublist({ start, len: end - start })) {
			Ok(str) => Ok(str)
			Err(BadUtf8({ index, .. })) => fail(start + index, "invalid UTF-8")
		}
	} else {
		match src.drop_first_bytes(start) {
			Ok(tail) => Ok(tail.drop_last_bytes(bytes.len() - end) ?? "")
			Err(_) => Ok("")
		}
	}
}

## The string whose opening quote is at `start`, decoded, and the position
## after its closing quote. A string without escapes is cut from the input.
scan_string : Str, List(U8), U64, JsonValue.Options -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_string = |src, bytes, start, options| {
	run_end = Utf8.skip_class(bytes, start + 1, plain_string_class)
	if bytes.get(run_end) == Ok('"') {
		if run_end - start - 1 > options.max_string_bytes {
			return fail(start, "string exceeds the limit of ${options.max_string_bytes.to_str()} bytes")
		}
		Ok({ str: slice_checked(src, bytes, start + 1, run_end)?, end: run_end + 1 })
	} else {
		scan_escaped_string(bytes, start, run_end, options)
	}
}

## The rest of a string that needs decoding, from `run_end`, where the plain
## run that starts it ends.
scan_escaped_string : List(U8), U64, U64, JsonValue.Options -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_escaped_string = |bytes, start, run_end, options| {
	var $out = List.with_capacity(run_end - start + 16)
	$out = $out.append_sublist(bytes, { start: start + 1, len: run_end - start - 1 })
	var $pos = run_end
	while True {
		byte =
			match bytes.get($pos) {
				Ok(b) => b
				Err(_) => return fail(start, "unterminated string")
			}
		if byte == '"' {
			if $out.len() > options.max_string_bytes {
				return fail(start, "string exceeds the limit of ${options.max_string_bytes.to_str()} bytes")
			}
			return match Str.from_utf8($out) {
				Ok(str) => Ok({ str, end: $pos + 1 })
				Err(_) => {
					# Escapes decode to valid UTF-8, so the bad bytes are in the input.
					check_utf8(bytes, start + 1, $pos)?
					fail(start, "invalid UTF-8")
				}
			}
		} else if byte == '\\' {
			escape =
				match bytes.get($pos + 1) {
					Ok(b) => b
					Err(_) => return fail(start, "unterminated string")
				}
			if escape == 'u' {
				code =
					match hex4(bytes, $pos + 2) {
						Ok(c) => c
						Err(_) => return fail($pos, "invalid \\u escape: expected four hexadecimal digits")
					}
				if code >= 0xD800 and code <= 0xDBFF {
					low =
						if bytes.get($pos + 6) == Ok('\\') and bytes.get($pos + 7) == Ok('u') {
							hex4(bytes, $pos + 8) ?? 0
						} else {
							0
						}
					if low >= 0xDC00 and low <= 0xDFFF {
						$out = append_utf8($out, 0x10000 + (code - 0xD800) * 0x400 + (low - 0xDC00))
						$pos = $pos + 12
					} else {
						match options.lone_surrogates {
							Replace => {
								$out = append_utf8($out, 0xFFFD)
								$pos = $pos + 6
							}

							Reject => return fail($pos, "unpaired surrogate in \\u escape")
						}
					}
				} else if code >= 0xDC00 and code <= 0xDFFF {
					match options.lone_surrogates {
						Replace => {
							$out = append_utf8($out, 0xFFFD)
							$pos = $pos + 6
						}

						Reject => return fail($pos, "unpaired surrogate in \\u escape")
					}
				} else {
					$out = append_utf8($out, code)
					$pos = $pos + 6
				}
			} else {
				decoded =
					if escape == '"' or escape == '\\' or escape == '/' {
						escape
					} else if escape == 'n' {
						'\n'
					} else if escape == 't' {
						'\t'
					} else if escape == 'r' {
						'\r'
					} else if escape == 'b' {
						0x08
					} else if escape == 'f' {
						0x0C
					} else {
						return fail($pos, "invalid escape sequence")
					}
				$out = $out.append(decoded)
				$pos = $pos + 2
			}
		} else if byte < 0x20 {
			if !options.control_characters {
				return fail($pos, "control character U+${hex_upper(byte.to_u32())} must be escaped")
			}
			$out = $out.append(byte)
			$pos = $pos + 1
		} else {
			run = Utf8.skip_class(bytes, $pos + 1, plain_string_class)
			$out = $out.append_sublist(bytes, { start: $pos, len: run - $pos })
			$pos = run
		}
	}
}

## The code unit written as four hexadecimal digits from `pos`.
hex4 : List(U8), U64 -> Try(U32, [InvalidHex])
hex4 = |bytes, pos| {
	var $code = 0.U32
	var $index = pos
	while $index < pos + 4 {
		byte = bytes.get($index) ?? 0
		digit =
			if byte >= '0' and byte <= '9' {
				byte - '0'
			} else if byte >= 'a' and byte <= 'f' {
				byte - 'a' + 10
			} else if byte >= 'A' and byte <= 'F' {
				byte - 'A' + 10
			} else {
				return Err(InvalidHex)
			}
		$code = $code * 16 + digit.to_u32()
		$index = $index + 1
	}
	Ok($code)
}

## `code` as at least four uppercase hexadecimal digits.
hex_upper : U32 -> Str
hex_upper = |code| {
	digits = "0123456789ABCDEF".to_utf8()
	var $out = []
	var $rest = code
	while $rest > 0 or $out.len() < 4 {
		$out = $out.prepend(digits.get(($rest % 16).to_u64()) ?? '0')
		$rest = $rest // 16
	}
	Str.from_utf8($out) ?? ""
}

## `out` with the UTF-8 encoding of `code` appended.
append_utf8 : List(U8), U32 -> List(U8)
append_utf8 = |out, code| {
	if code < 0x80 {
		out.append(code.to_u8_wrap())
	} else if code < 0x800 {
		out.append((0xC0 + code // 64).to_u8_wrap()).append((0x80 + code % 64).to_u8_wrap())
	} else if code < 0x10000 {
		out.append((0xE0 + code // 4096).to_u8_wrap()).append((0x80 + (code // 64) % 64).to_u8_wrap()).append((0x80 + code % 64).to_u8_wrap())
	} else {
		out.append((0xF0 + code // 262144).to_u8_wrap()).append((0x80 + (code // 4096) % 64).to_u8_wrap()).append((0x80 + (code // 64) % 64).to_u8_wrap()).append((0x80 + code % 64).to_u8_wrap())
	}
}

## The number starting at `start`, checked against the options, and the
## position after it.
scan_number : Str, List(U8), U64, JsonValue.Options -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_number = |src, bytes, start, options| {
	end =
		match non_finite_end(bytes, start, options.non_finite) {
			Ok(after) => after
			Err(_) => number_end(bytes, start)?
		}
	if end - start > options.max_number_digits {
		return fail(start, "number exceeds the limit of ${options.max_number_digits.to_str()} characters")
	}
	if options.interoperable_numbers {
		match interoperable(bytes, start, end) {
			Ok({}) => {}
			Err(reason) => return fail(start, "number ${excerpt(bytes, start, end)} is not interoperable: ${reason}")
		}
	}
	Ok({ str: slice_checked(src, bytes, start, end)?, end })
}

## The end of `NaN`, `Infinity` or `-Infinity` at `start`, when allowed.
non_finite_end : List(U8), U64, Bool -> Try(U64, [NotNonFinite])
non_finite_end = |bytes, start, allowed| {
	if !allowed {
		Err(NotNonFinite)
	} else {
		sign = if (bytes.get(start) == Ok('-')) 1 else 0
		if bytes.sublist({ start: start + sign, len: 8 }) == infinity_bytes {
			Ok(start + sign + 8)
		} else if sign == 0 and at3(bytes, start, 'N', 'a', 'N') {
			Ok(start + 3)
		} else {
			Err(NotNonFinite)
		}
	}
}

infinity_bytes : List(U8)
infinity_bytes = "Infinity".to_utf8()

## The end of the number that starts at `start`, following the grammar of
## RFC 8259 section 6: `-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?`.
number_end : List(U8), U64 -> Try(U64, [JsonFail(Failure)])
number_end = |bytes, start| {
	var $pos = start
	if bytes.get($pos) == Ok('-') {
		$pos = $pos + 1
	}
	first = bytes.get($pos) ?? 0
	if first == '0' {
		$pos = $pos + 1
		if is_digit(bytes.get($pos) ?? 0) {
			return fail(start, "leading zeros are not allowed in numbers")
		}
	} else if first >= '1' and first <= '9' {
		$pos = digits_end(bytes, $pos + 1)
	} else if $pos > start {
		return fail(start, "expected a digit after '-'")
	} else {
		return fail(start, "expected a JSON value")
	}
	if bytes.get($pos) == Ok('.') {
		if !is_digit(bytes.get($pos + 1) ?? 0) {
			return fail(start, "expected a digit after the decimal point")
		}
		$pos = digits_end(bytes, $pos + 2)
	}
	exponent = bytes.get($pos) ?? 0
	if exponent == 'e' or exponent == 'E' {
		$pos = $pos + 1
		sign = bytes.get($pos) ?? 0
		if sign == '+' or sign == '-' {
			$pos = $pos + 1
		}
		if !is_digit(bytes.get($pos) ?? 0) {
			return fail(start, "expected a digit in the exponent")
		}
		$pos = digits_end(bytes, $pos + 1)
	}
	Ok($pos)
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

digits_end : List(U8), U64 -> U64
digits_end = |bytes, start| {
	var $pos = start
	while is_digit(bytes.get($pos) ?? 0) {
		$pos = $pos + 1
	}
	$pos
}

## Whether the whole of `bytes` is a JSON integer: `-?(0|[1-9][0-9]*)`.
is_integer_lexeme : List(U8) -> Bool
is_integer_lexeme = |bytes| {
	start = if (bytes.first() == Ok('-')) 1 else 0
	match bytes.get(start) {
		Ok('0') => bytes.len() == start + 1
		Ok(first) if first >= '1' and first <= '9' => digits_end(bytes, start + 1) == bytes.len()
		_ => False
	}
}

## Why the number [start, end) cannot be held by an IEEE 754 double as
## I-JSON (RFC 7493 section 2.2) requires, if it cannot.
interoperable : List(U8), U64, U64 -> Try({}, Str)
interoperable = |bytes, start, end| {
	sign = if (bytes.get(start) == Ok('-')) 1 else 0
	first = bytes.get(start + sign) ?? 0
	if first == 'N' or first == 'I' {
		return Err("it is not a finite number")
	}
	var $pos = start + sign
	var $integer = True
	var $digits = 0
	var $value = 0.U64
	var $first_nonzero = 0
	var $last_nonzero = 0
	var $nonzero = False
	while $pos < end {
		byte = bytes.get($pos) ?? 0
		if is_digit(byte) {
			if byte != '0' {
				if !$nonzero {
					$first_nonzero = $digits
					$nonzero = True
				}
				$last_nonzero = $digits
			}
			if $digits < 17 {
				$value = $value * 10 + (byte - '0').to_u64()
			}
			$digits = $digits + 1
			$pos = $pos + 1
		} else if byte == '.' {
			$integer = False
			$pos = $pos + 1
		} else {
			$integer = False
			$pos = end
		}
	}
	if $integer {
		if $digits > 16 or $value > max_safe_integer {
			Err("it is an integer beyond 2^53-1 in magnitude")
		} else {
			Ok({})
		}
	} else if $nonzero and $last_nonzero - $first_nonzero + 1 > 17 {
		Err("it has more than 17 significant digits")
	} else {
		match F64.from_str(Str.from_utf8(bytes.sublist({ start, len: end - start })) ?? "") {
			Ok(double) =>
				if !double.is_finite() {
					Err("it is too large for a double")
				} else if $nonzero and double == 0.0 {
					Err("it underflows to zero as a double")
				} else {
					Ok({})
				}

			Err(_) => Err("it is too large for a double")
		}
	}
}

## 2^53 - 1, the largest integer every IEEE 754 double can hold exactly.
max_safe_integer : U64
max_safe_integer = 9007199254740991

## The bytes [start, end) for an error message, shortened when long.
excerpt : List(U8), U64, U64 -> Str
excerpt = |bytes, start, end| {
	if end - start > 40 {
		"${Str.from_utf8(bytes.sublist({ start, len: 40 })) ?? ""}..."
	} else {
		Str.from_utf8(bytes.sublist({ start, len: end - start })) ?? ""
	}
}

## Array indexes in a path: decimal digits without a leading zero.
array_index : Str -> Try(U64, [Missing])
array_index = |segment| {
	if is_integer_lexeme(segment.to_utf8()) and !segment.starts_with("-") {
		match U64.from_str(segment) {
			Ok(index) => Ok(index)
			Err(_) => Err(Missing)
		}
	} else {
		Err(Missing)
	}
}

# String bytes that need no decoding or escaping: anything but control
# characters, the quote and the backslash. Scanned 16 bytes at a time.
plain_string_class : Utf8.ByteClass
plain_string_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b != '"' and b != '\\')

ws_class : Utf8.ByteClass
ws_class = Utf8.ByteClass.from_bytes([' ', '\t', '\n', '\r'])

star_class : Utf8.ByteClass
star_class = Utf8.ByteClass.from_bytes(['*'])

# ------------------------------------------------------------------ writing

## `buf` with `text` written after it as a JSON string. Only the quote, the
## backslash and control characters are escaped, as Python's
## `json.dumps(ensure_ascii=False)` does.
write_string : List(U8), Str -> List(U8)
write_string = |buf, text| {
	bytes = text.to_utf8()
	len = bytes.len()
	var $out = buf.append('"')
	var $pos = 0
	while $pos < len {
		run = Utf8.skip_class(bytes, $pos, plain_string_class)
		$out = $out.append_sublist(bytes, { start: $pos, len: run - $pos })
		if run < len {
			$out = append_escape($out, bytes.get(run) ?? 0)
		}
		$pos = run + 1
	}
	$out.append('"')
}

## `out` with the escape for a quote, backslash or control byte appended.
append_escape : List(U8), U8 -> List(U8)
append_escape = |out, byte| {
	escaped = out.append('\\')
	if byte == '"' or byte == '\\' {
		escaped.append(byte)
	} else if byte == '\n' {
		escaped.append('n')
	} else if byte == '\r' {
		escaped.append('r')
	} else if byte == '\t' {
		escaped.append('t')
	} else if byte == 0x08 {
		escaped.append('b')
	} else if byte == 0x0C {
		escaped.append('f')
	} else {
		escaped.append('u').append('0').append('0').append(hex_lower(byte // 16)).append(hex_lower(byte % 16))
	}
}

hex_lower : U8 -> U8
hex_lower = |nibble| if (nibble < 10) '0' + nibble else 'a' + nibble - 10

## `text` written as a JSON string, for error messages.
quote : Str -> Str
quote = |text| Str.from_utf8(write_string([], text)) ?? ""

# ------------------------------------------------------------------ tests

# The error `parse_with` reports for `input`, or `Err(Parsed)`.
error_of : Str, JsonValue.Options -> Try(JsonValue.Error, [Parsed])
error_of = |input, options| {
	match JsonValue.parse_with(input, options) {
		Ok(_) => Err(Parsed)
		Err(InvalidJson(error)) => Ok(error)
	}
}

# The message `parse_with` reports for `input`, or "" when it parses.
message_of : Str, JsonValue.Options -> Str
message_of = |input, options| {
	match error_of(input, options) {
		Ok(error) => error.message
		Err(_) => ""
	}
}

# The failure `parser_with` reports for raw bytes, or `Err(Parsed)`.
parser_failure : List(U8), JsonValue.Options -> Try({ message : Str, offset : U64 }, [Parsed])
parser_failure = |bytes, options| {
	match Parser.run(JsonValue.parser_with(options), bytes) {
		Ok(_) => Err(Parsed)
		Err(ParseError(failure)) => Ok(failure)
	}
}

# An object of `count` members named k0, k1, ..., each holding `[]`.
many_keys : U64 -> Str
many_keys = |count| {
	var $out = "{"
	var $index = 0
	while $index < count {
		if $index > 0 {
			$out = Str.concat($out, ",")
		}
		$out = Str.concat($out, "\"k${$index.to_str()}\":[]")
		$index = $index + 1
	}
	Str.concat($out, "}")
}

# The names k0, k1, ... of the members of `many_keys(count)`.

# `depth` arrays nested around `inner`.
nested : U64, Str -> Str
nested = |depth, inner| Str.concat(Str.repeat("[", depth), Str.concat(inner, Str.repeat("]", depth)))

# `depth` arrays nested around `Null`, built as a value.

# --- Grammar.

# Whitespace may surround every token.
expect JsonValue.parse_str(" \t\n\r[ 1 , 2 ]\r\n ") == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.parse_str("{ \"a\" : { } , \"b\" : [ ] }") == Ok(Object([{ key: "a", value: Object([]) }, { key: "b", value: Array([]) }]))

# Scalars at the top level.
expect JsonValue.parse_str("null") == Ok(Null) and JsonValue.parse_str("true") == Ok(Bool(True)) and JsonValue.parse_str("false") == Ok(Bool(False))
expect JsonValue.parse_str("\"\"") == Ok(String("")) and JsonValue.parse_str("[]") == Ok(Array([])) and JsonValue.parse_str("{}") == Ok(Object([]))

# Numbers keep their exact text.
expect ["0", "-0", "10", "1.0", "0.0", "0e0", "1E3", "-0.0e-0", "1e+2", "123456789012345678901234567890"].all(|text| JsonValue.parse_str(text) == Ok(Number(text)))

# Number grammar errors point at the start of the number.
expect error_of("01", {}) == Ok({ line: 1, column: 1, message: "leading zeros are not allowed in numbers" })
expect error_of("[-01]", {}) == Ok({ line: 1, column: 2, message: "leading zeros are not allowed in numbers" })
expect error_of("00", {}) == Ok({ line: 1, column: 1, message: "leading zeros are not allowed in numbers" })
expect error_of("-", {}) == Ok({ line: 1, column: 1, message: "expected a digit after '-'" })
expect error_of("[1, -a]", {}) == Ok({ line: 1, column: 5, message: "expected a digit after '-'" })
expect error_of("1.", {}) == Ok({ line: 1, column: 1, message: "expected a digit after the decimal point" })
expect error_of("[1.e3]", {}) == Ok({ line: 1, column: 2, message: "expected a digit after the decimal point" })
expect ["1e", "1e+", "1E-", "1.5ex"].all(|text| message_of(text, {}) == "expected a digit in the exponent")
expect error_of(".5", {}) == Ok({ line: 1, column: 1, message: "expected a JSON value" })
expect error_of("+1", {}) == Ok({ line: 1, column: 1, message: "expected a JSON value" })

# Literals are exact.
expect error_of("tru", {}) == Ok({ line: 1, column: 1, message: "invalid literal: expected true, false or null" })
expect error_of("[nul]", {}) == Ok({ line: 1, column: 2, message: "invalid literal: expected true, false or null" })
expect error_of("fals", {}) == Ok({ line: 1, column: 1, message: "invalid literal: expected true, false or null" })
expect error_of("True", {}) == Ok({ line: 1, column: 1, message: "expected a JSON value" })
expect error_of("truex", {}) == Ok({ line: 1, column: 5, message: "unexpected content after the JSON value" })

# Structural errors point at the offending byte.
expect error_of("", {}) == Ok({ line: 1, column: 1, message: "expected a JSON value" })
expect error_of("  ", {}) == Ok({ line: 1, column: 3, message: "expected a JSON value" })
expect error_of("[", {}) == Ok({ line: 1, column: 2, message: "expected a JSON value" })
expect error_of("[1", {}) == Ok({ line: 1, column: 3, message: "expected ',' or ']' after an array item" })
expect error_of("[1 2]", {}) == Ok({ line: 1, column: 4, message: "expected ',' or ']' after an array item" })
expect error_of("[1,,2]", {}) == Ok({ line: 1, column: 4, message: "expected a JSON value" })
expect error_of("{", {}) == Ok({ line: 1, column: 2, message: "expected a string key" })
expect error_of("{1:2}", {}) == Ok({ line: 1, column: 2, message: "expected a string key" })
expect error_of("{\"a\" 1}", {}) == Ok({ line: 1, column: 6, message: "expected ':' after an object key" })
expect error_of("{\"a\"", {}) == Ok({ line: 1, column: 5, message: "expected ':' after an object key" })
expect error_of("{\"a\":}", {}) == Ok({ line: 1, column: 6, message: "expected a JSON value" })
expect error_of("{\"a\":1", {}) == Ok({ line: 1, column: 7, message: "expected ',' or '}' after an object member" })
expect error_of("{\"a\":1 \"b\":2}", {}) == Ok({ line: 1, column: 8, message: "expected ',' or '}' after an object member" })
expect error_of("{\"a\":1,\"b\"}", {}) == Ok({ line: 1, column: 11, message: "expected ':' after an object key" })
expect error_of("[1] x", {}) == Ok({ line: 1, column: 5, message: "unexpected content after the JSON value" })
expect error_of("[1]]", {}) == Ok({ line: 1, column: 4, message: "unexpected content after the JSON value" })
expect error_of("[1}", {}) == Ok({ line: 1, column: 3, message: "expected ',' or ']' after an array item" })

# A trailing comma is refused at the comma.
expect error_of("[1,]", {}) == Ok({ line: 1, column: 3, message: "trailing comma is not allowed" })
expect error_of("{\"a\":1,}", {}) == Ok({ line: 1, column: 7, message: "trailing comma is not allowed" })

# Lines count LF, CR and CRLF; columns count bytes.
expect error_of("[\n1,\n]", {}) == Ok({ line: 2, column: 2, message: "trailing comma is not allowed" })
expect error_of("{\r\n\"a\": tru\r\n}", {}) == Ok({ line: 2, column: 6, message: "invalid literal: expected true, false or null" })
expect error_of("[\"é\",\r\"ü\" x]", {}) == Ok({ line: 2, column: 6, message: "expected ',' or ']' after an array item" })

# --- Strings.

expect JsonValue.parse_str("\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"") == Ok(String("\"\\/\u(8)\u(C)\n\r\t"))
expect JsonValue.parse_str("\"\\u00e9\\u00E9\\u0041\\u0000\"") == Ok(String("ééA\u(0)"))
expect JsonValue.parse_str("\"héllo 😀\"") == Ok(String("héllo 😀"))
expect JsonValue.parse_str("\"é\\n😀\"") == Ok(String("é\n😀"))
expect JsonValue.parse_str("{\"a\\nb\":1}") == Ok(Object([{ key: "a\nb", value: Number("1") }]))

# Surrogate pairs combine, in either case of hexadecimal digits.
expect JsonValue.parse_str("\"\\uD83D\\uDE00\"") == Ok(String("😀"))
expect JsonValue.parse_str("\"\\ud800\\udc00\\udbff\\udfff\"") == Ok(String("\u(10000)\u(10FFFF)"))

# A lone surrogate is refused at its escape.
expect error_of("\"\\ud800\"", {}) == Ok({ line: 1, column: 2, message: "unpaired surrogate in \\u escape" })
expect error_of("\"a\\udc00\"", {}) == Ok({ line: 1, column: 3, message: "unpaired surrogate in \\u escape" })
expect error_of("\"\\ud800\\u0041\"", {}) == Ok({ line: 1, column: 2, message: "unpaired surrogate in \\u escape" })
expect error_of("\"\\udc00\\ud800\"", {}) == Ok({ line: 1, column: 2, message: "unpaired surrogate in \\u escape" })

# Escapes must be known, and `\u` needs four hexadecimal digits.
expect error_of("\"\\x\"", {}) == Ok({ line: 1, column: 2, message: "invalid escape sequence" })
expect error_of("\"ab\\'\"", {}) == Ok({ line: 1, column: 4, message: "invalid escape sequence" })
expect error_of("\"\\U0041\"", {}) == Ok({ line: 1, column: 2, message: "invalid escape sequence" })
expect error_of("\"\\u12\"", {}) == Ok({ line: 1, column: 2, message: "invalid \\u escape: expected four hexadecimal digits" })
expect error_of("[\"\\u12G4\"]", {}) == Ok({ line: 1, column: 3, message: "invalid \\u escape: expected four hexadecimal digits" })

# An unterminated string is reported at its opening quote.
expect error_of("\"abc", {}) == Ok({ line: 1, column: 1, message: "unterminated string" })
expect error_of("\"abc\\", {}) == Ok({ line: 1, column: 1, message: "unterminated string" })
expect error_of("[\"a", {}) == Ok({ line: 1, column: 2, message: "unterminated string" })
expect error_of("\"a\\\"", {}) == Ok({ line: 1, column: 1, message: "unterminated string" })

# Raw control characters must be escaped; DEL and space need not be.
expect error_of("\"a\nb\"", {}) == Ok({ line: 1, column: 3, message: "control character U+000A must be escaped" })
expect error_of("\"\u(0)\"", {}) == Ok({ line: 1, column: 2, message: "control character U+0000 must be escaped" })
expect error_of("\"\u(1F)\"", {}) == Ok({ line: 1, column: 2, message: "control character U+001F must be escaped" })
expect error_of("{\"a\tb\":1}", {}) == Ok({ line: 1, column: 4, message: "control character U+0009 must be escaped" })
expect JsonValue.parse_str("\"\u(7F) \"") == Ok(String("\u(7F) "))

# --- The parser.

expect Utf8.parse_str_partial(JsonValue.parser, "[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
expect Utf8.parse_str_partial(JsonValue.parser, "12abc") == Ok({ value: Number("12"), rest: "abc" })
expect Utf8.parse_str_partial(JsonValue.parser_with({ comments: True }), "{} // c\nx") == Ok({ value: Object([]), rest: "x" })
expect Utf8.parse_str(JsonValue.parser, "[1 2]") == Err(ParseError({ message: "expected ',' or ']' after an array item", offset: 3 }))
expect parser_failure([], {}) == Ok({ message: "expected a JSON value", offset: 0 })

# Input that is not UTF-8 is refused where the bad bytes are, in plain and
# escaped strings, keys and comments; the rest of the input may hold anything.
expect parser_failure([0xFF], {}) == Ok({ message: "expected a JSON value", offset: 0 })
expect parser_failure(['"', 0xFF, '"'], {}) == Ok({ message: "invalid UTF-8", offset: 1 })
expect parser_failure(['"', '\\', 'n', 'a', 0xC3, '"'], {}) == Ok({ message: "invalid UTF-8", offset: 4 })
expect parser_failure(['[', '"', 'a', 0xE2, 0x82, '"', ']'], {}) == Ok({ message: "invalid UTF-8", offset: 3 })
expect parser_failure(['{', '"', 0xED, 0xA0, 0x80, '"', ':', '1', '}'], {}) == Ok({ message: "invalid UTF-8", offset: 2 })
expect parser_failure(['1', ' ', '/', '/', 0xFF], { comments: True }) == Ok({ message: "invalid UTF-8", offset: 4 })
expect parser_failure(['1', '/', '*', 0xC0, '*', '/'], { comments: True }) == Ok({ message: "invalid UTF-8", offset: 3 })
expect Parser.run(JsonValue.parser, ['"', 'a', '"', ' ', 0xFF]) == Ok({ value: String("a"), rest: [0xFF] })
expect Parser.run(JsonValue.parser, ['[', '"', 0xC3, 0xA9, '\\', 't', '"', ']', 0xFF]) == Ok({ value: Array([String("é\t")]), rest: [0xFF] })

# --- Accessors.

expect JsonValue.get(Object([{ key: "a", value: Number("1") }, { key: "a", value: Number("2") }]), "a") == Ok(Number("1"))
expect JsonValue.get(Object([]), "a") == Err(Missing) and JsonValue.get(Array([Null]), "0") == Err(Missing)
expect JsonValue.at(Array([Null]), 1) == Err(Missing) and JsonValue.at(Object([{ key: "0", value: Null }]), 0) == Err(Missing)
expect {
	match JsonValue.parse_str("{\"a\":[10,{\"b\":[true]}],\"01\":null}") {
		Ok(doc) =>
			JsonValue.get_path(doc, []) == Ok(doc)
				and JsonValue.get_path(doc, ["a", "1", "b", "0"]) == Ok(Bool(True))
				and JsonValue.get_path(doc, ["01"]) == Ok(Null)
				and ["01", "-0", "+1", " 1", "1.0", "18446744073709551616", "2"].all(|index| JsonValue.get_path(doc, ["a", index]) == Err(Missing))
				and JsonValue.get_path(doc, ["a", "0", "x"]) == Err(Missing)
				and JsonValue.get_path(doc, ["missing"]) == Err(Missing)

		Err(_) => False
	}
}
expect JsonValue.keys(Array([Null])) == [] and JsonValue.keys(Object([{ key: "b", value: Null }, { key: "a", value: Null }])) == ["b", "a"]
expect JsonValue.as_str(Number("1")) == Err(WrongType) and JsonValue.as_bool(Null) == Err(WrongType) and JsonValue.as_list(Object([])) == Err(WrongType) and JsonValue.as_object(Array([])) == Err(WrongType)
expect JsonValue.as_str(String("s")) == Ok("s") and JsonValue.as_bool(Bool(True)) == Ok(True) and JsonValue.as_list(Array([Null])) == Ok([Null]) and JsonValue.as_object(Object([])) == Ok([])

# --- Scaling: each append updates the shared lists in place, so these are linear.

expect JsonValue.parse_str(many_keys(20000)).map_ok(|value| JsonValue.keys(value).len()) == Ok(20000)
expect JsonValue.parse_str("[${Str.join_with(List.repeat("1", 100000), ",")}]").map_ok(|value| JsonValue.as_list(value).map_ok(List.len)) == Ok(Ok(100000))
expect JsonValue.parse_with(nested(10000, "[]"), { max_depth: 10001 }).is_ok()
expect JsonValue.parse_str("\"${Str.repeat("\\n", 50000)}\"").map_ok(|value| JsonValue.as_str(value).map_ok(Str.count_utf8_bytes)) == Ok(Ok(50000))

# --- Doc examples: reading.

expect {
	match JsonValue.parse_str("{\"@context\": \"https://spdx.org/rdf/3.0.1/spdx-context.jsonld\", \"@graph\": []}") {
		Ok(doc) => doc.keys() == ["@context", "@graph"]
		Err(_) => False
	}
}
expect JsonValue.parse_with("[1, 2,] // done", { comments: True, trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.parse_str("{\"a\": [true, null]}") == Ok(Object([{ key: "a", value: Array([Bool(True), Null]) }]))
expect JsonValue.parse_str("[1,]") == Err(InvalidJson({ line: 1, column: 3, message: "trailing comma is not allowed" }))
expect JsonValue.parse_with("{\"a\": 1, \"a\": 2}", { duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("2") }]))
expect Utf8.parse_str_partial(JsonValue.parser, "[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
expect Utf8.parse_str(JsonValue.parser, "[1 2]") == Err(ParseError({ message: "expected ',' or ']' after an array item", offset: 3 }))
expect Utf8.parse_str(JsonValue.parser_with({ comments: True }), "1 // one") == Ok(Number("1"))
expect JsonValue.parse_str("{\"a\": 1}").map_ok(|doc| doc.get("a")) == Ok(Ok(Number("1")))
expect JsonValue.parse_str("[10, 20]").map_ok(|doc| doc.at(1)) == Ok(Ok(Number("20")))
expect JsonValue.parse_str("{\"a\": [{\"b\": true}]}").map_ok(|doc| doc.get_path(["a", "0", "b"])) == Ok(Ok(Bool(True)))
expect JsonValue.parse_str("{\"b\": 1, \"a\": 2}").map_ok(|doc| doc.keys()) == Ok(["b", "a"])
expect JsonValue.parse_str("\"caf\\u00e9\"").map_ok(|doc| doc.as_str()) == Ok(Ok("café"))
expect JsonValue.parse_str("false").map_ok(|doc| doc.as_bool()) == Ok(Ok(False))
expect JsonValue.parse_str("[null]").map_ok(|doc| doc.as_list()) == Ok(Ok([Null]))
expect JsonValue.parse_str("{\"a\": null}").map_ok(|doc| doc.as_object()) == Ok(Ok([{ key: "a", value: Null }]))
