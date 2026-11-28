import DiagnosticOwnership
import Streaming
import JsonConfig
import Diagnostic
import JsonWorkspace
import JsonStream
import Parser

## A dynamic JSON value and parser based on RFC 8259.
##
## Use this module when a record type cannot describe a document: keys that
## are not Roc identifiers (such as JSON-LD's `@context`), arrays whose
## objects differ in shape, JSON Schema files, or data that must pass through
## untouched. To decode JSON straight into your own record type, use the
## builtin `Json` instead.
##
## A `JsonValue` is one of:
##
## - `Null`, `Bool` and `String`;
## - `Number`, holding the exact number text from the input, so integers of any
##   size and decimals of any precision survive unchanged. Convert it with
##   [JsonValue.as_i64], [JsonValue.as_u64] or [JsonValue.as_dec];
## - `Array`, and `Object`, whose members keep their source order.
##
## [JsonValue.parse_str] is strict: it accepts exactly the JSON of RFC 8259
## and rejects duplicate keys. [JsonValue.parse_str] takes [JsonValue.Props]
## to accept common relaxations (comments, trailing commas, a byte order
## mark, `NaN` and `Infinity`, lenient strings), to add checks (I-JSON
## numbers, the top-level shape, size guards), and to choose what a repeated
## key means. Nesting is limited to 128 levels by default.
##
## [JsonValue.encode] writes a value back out as UTF-8 bytes containing
## compact or indented JSON, refusing values that would not read back the same.
##
## Strings and numbers read from the input share its bytes instead of being
## copied, so a value keeps the whole input alive while any of them is.
##
## ```roc
## expect {
##     doc = JsonValue.parse_str("{\"@context\": \"https://spdx.org/rdf/3.0.1/spdx-context.jsonld\", \"@graph\": []}", {})?
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
	Error : Diagnostic(Problem)

	## A parser resource bound, preserving its configured maximum.
	Resource : JsonConfig.Resource

	## Why an exact numeric lexeme cannot be represented interoperably.
	NumberProblem : JsonConfig.NumberProblem

	## Structured JSON syntax, option, resource, and numeric failures.
	Problem : JsonConfig.Problem

	## Compare two JSON values structurally. Numbers compare by their text, so
	## `Number("1.0")` and `Number("1")` differ.
	is_eq : _

	## Hash a JSON value, so values can be `Dict` keys and `Set` members.
	to_hash : _

	## How [JsonValue.parse_str] reads a document. Omitted fields use the
	## defaults shown, which together are strict RFC 8259.
	##
	## Limits:
	##
	## - `max_bytes`: bytes in the supplied input, including whitespace and any
	##   BOM, checked before constructing a value; unlimited by default;
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
	## expect JsonValue.parse_str("[1, 2,] // done", { comments: True, trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
	## ```
	Props : JsonConfig.Props

	## Checked source, its name for errors, and the [JsonValue.Props] to read it with.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : JsonConfig.Props ?? {} }

	## Validate a complete document, retaining its source name
	## and structured position. It accepts exactly what [JsonValue.parse_str] accepts with
	## the same options.
	##
	## ```roc
	## expect JsonValue.validate({ source: "{\"a\": [1,]}", name: "list.json" }) == Err(InvalidJsonValue(Parser.Diagnostic.{ problem: TrailingComma, location: Text({ offset: 8, line: 1, column: 9 }), path: [Source("list.json")] }))
	##
	## expect JsonValue.validate({ source: "{\"a\": [1,]}", options: { trailing_commas: True } }).map_ok(|doc| doc.get_path(["a", "0"])) == Ok(Ok(Number("1")))
	## ```
	validate : ValidationProps -> Try(JsonValue, [InvalidJsonValue(Error)])
	validate = |props| JsonValue.parse_str(props.source, props.options)
		.map_err(|InvalidJsonValue(error)| InvalidJsonValue({ ..error, path: [Source(props.name)].concat(error.path) }))

	## Use a strict JSON document as a quoted value.
	##
	## ```roc
	## expect {
	##     settings : JsonValue
	##     settings = "{\"port\": 8080}"
	##     settings.get("port") == Ok(Number("8080"))
	## }
	##
	## expect JsonValue.from_quote("{\"a\": 1, \"a\": 1}") == Err(BadQuotedBytes("<literal>:1:10: JSON: DuplicateKey(\"a\")"))
	## ```
	from_quote : Str -> Try(JsonValue, [BadQuotedBytes(Str)])
	from_quote = |source| JsonValue.validate({ source: source }).map_err(
		|InvalidJsonValue(error)| {
			position = Diagnostic.line_column(error.location) ?? { line: 1, column: 1 }
			Parser.Quotation.error({ name: "<literal>", format: "JSON", message: Str.inspect(error.problem), line: position.line, column: position.column })
		},
	)

	## Parse one complete, strict RFC 8259 document. Whitespace may surround the
	## value; anything else after it is an error.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": [true, null]}", {}) == Ok(Object([{ key: "a", value: Array([Bool(True), Null]) }]))
	##
	## expect JsonValue.parse_str("[1,]", {}) == Err(InvalidJsonValue(Parser.Diagnostic.{ location: Text({ offset: 3 - 1, line: 1, column: 3 }), path: [], problem: TrailingComma }))
	## ```
	## Parse one complete document with the given [JsonValue.Props].
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": 1, \"a\": 2}", { duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("2") }]))
	## ```
	parse_str : Str, Props -> Try(JsonValue, [InvalidJsonValue(Error)])
	parse_str = |input, options| {
		match scan(input, input.to_utf8(), options) {
			Ok({ value, pos }) =>
				if pos == input.count_utf8_bytes() {
					Ok(value)
				} else {
					Err(InvalidJsonValue(locate(input, pos, UnexpectedContent)))
				}

			Err(JsonFail(failure)) => Err(InvalidJsonValue(locate(input, failure.offset, failure.problem)))
		}
	}

	## Framing and recovery configuration for JSON record sources.
	RecordProps : JsonStream.Props
	RecordWriteProps : JsonStream.WriteProps

	## Read NDJSON records or top-level array items through the shared decoder.
	record_decoder : RecordProps -> JsonStream.Decoder(JsonValue)
	record_decoder = |props| JsonStream.decoder(props, parse_reusing)

	## Write framed JSON records using the same bounded encoder contract.
	record_encoder : RecordWriteProps -> JsonStream.Encoder(JsonValue)
	record_encoder = |props| JsonStream.encoder(props, |value, options| write_text(value, options).map_err(encoding_error))

	## A [Parser] that reads one strict JSON value and the whitespace around
	## it, and leaves the rest of the input for the next parser. A failure is
	## `ParseError(Parser.Diagnostic.{ problem: problem, location: InputOffset(offset), path: [] })` with the byte offset of the problem.
	##
	## ```roc
	## expect (JsonValue.parser({})).run_str("[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
	##
	## expect (JsonValue.parser({})).parse_str("[1 2]") == Err(ParseError(Parser.Diagnostic.{ problem: ExpectedArrayDelimiter, location: Text({ offset: 3, line: 1, column: 4 }), path: [] }))
	## ```
	## Configure the grammar with [JsonValue.Props].
	## `max_bytes` bounds the supplied buffer, including any unconsumed suffix.
	##
	## ```roc
	## expect (JsonValue.parser({ comments: True })).parse_str("1 // one") == Ok(Number("1"))
	## ```
	parser : Props -> Parser(Parser.Utf8.Bytes, JsonValue, Problem)
	parser = |options|
		Parser.custom(
			|input| {
				# Validate once, so strings can be cut from the input without
				# validating each one again.
				match scan(Str.from_utf8(input) ?? "", input, options) {
					Ok({ value, pos }) => Ok({ value, rest: input.drop_first(pos) })
					Err(JsonFail(failure)) => Err(ParseError(Diagnostic.{ problem: failure.problem, location: InputOffset(failure.offset), path: [] }))
				}
			},
		)

	## The value of the first member named `key` in an object. Anything else,
	## or an object without that key, is `Err(Missing)`.
	##
	## ```roc
	## expect JsonValue.parse_str("{\"a\": 1}", {}).map_ok(|doc| doc.get("a")) == Ok(Ok(Number("1")))
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
	## expect JsonValue.parse_str("[10, 20]", {}).map_ok(|doc| doc.at(1)) == Ok(Ok(Number("20")))
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
	## expect JsonValue.parse_str("{\"a\": [{\"b\": true}]}", {}).map_ok(|doc| doc.get_path(["a", "0", "b"])) == Ok(Ok(Bool(True)))
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
	## expect JsonValue.parse_str("{\"b\": 1, \"a\": 2}", {}).map_ok(|doc| doc.keys()) == Ok(["b", "a"])
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
	## expect JsonValue.parse_str("\"caf\\u00e9\"", {}).map_ok(|doc| doc.as_str()) == Ok(Ok("café"))
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
	## expect JsonValue.parse_str("false", {}).map_ok(|doc| doc.as_bool()) == Ok(Ok(False))
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
	## expect JsonValue.parse_str("[null]", {}).map_ok(|doc| doc.as_list()) == Ok(Ok([Null]))
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
	## expect JsonValue.parse_str("{\"a\": null}", {}).map_ok(|doc| doc.as_object()) == Ok(Ok([{ key: "a", value: Null }]))
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
	## expect JsonValue.parse_str("-42", {}).map_ok(|doc| doc.as_i64()) == Ok(Ok(-42))
	##
	## expect JsonValue.parse_str("1e3", {}).map_ok(|doc| doc.as_i64()) == Ok(Err(WrongType))
	## ```
	as_i64 : JsonValue -> Try(I64, [WrongType, OutOfRange])
	as_i64 = |value| {
		match value {
			Number(lexeme) =>
				if is_integer_lexeme(lexeme.to_utf8()) {
					match I64.from_str(lexeme) {
						Ok(number) => Ok(number)
						Err(_) => Err(OutOfRange)
					}
				} else {
					Err(WrongType)
				}

			_ => Err(WrongType)
		}
	}

	## Like [JsonValue.as_i64], for `U64`. A negative integer other than `-0`
	## is `Err(OutOfRange)`.
	##
	## ```roc
	## expect JsonValue.parse_str("18446744073709551615", {}).map_ok(|doc| doc.as_u64()) == Ok(Ok(18446744073709551615))
	##
	## expect JsonValue.parse_str("-1", {}).map_ok(|doc| doc.as_u64()) == Ok(Err(OutOfRange))
	## ```
	as_u64 : JsonValue -> Try(U64, [WrongType, OutOfRange])
	as_u64 = |value| {
		match value {
			Number(lexeme) =>
				if is_integer_lexeme(lexeme.to_utf8()) {
					match U64.from_str(lexeme) {
						Ok(number) => Ok(number)
						Err(_) => Err(OutOfRange)
					}
				} else {
					Err(WrongType)
				}

			_ => Err(WrongType)
		}
	}

	## The value of a `Number` as a `Dec`. A number outside the `Dec` range,
	## one that needs more than 18 decimal places (trailing zeros don't count,
	## so `1.50000000000000000000` is `1.5`), or `NaN` or `Infinity`, is
	## `Err(OutOfRange)`; anything else is `Err(WrongType)`.
	##
	## ```roc
	## expect JsonValue.parse_str("-1.25e+2", {}).map_ok(|doc| doc.as_dec()) == Ok(Ok(-125.0))
	## ```
	as_dec : JsonValue -> Try(Dec, [WrongType, OutOfRange])
	as_dec = |value| {
		match value {
			Number(lexeme) => {
				bytes = lexeme.to_utf8()
				if is_number_lexeme(bytes) {
					match Dec.from_str(lexeme) {
						Ok(number) => Ok(number)
						Err(_) => Err(OutOfRange)
					}
				} else if is_non_finite_lexeme(bytes) {
					Err(OutOfRange)
				} else {
					Err(WrongType)
				}
			}

			_ => Err(WrongType)
		}
	}

	## Output ordering, indentation and the maximum number of UTF-8 bytes.
	WriteProps : JsonConfig.WriteProps

	## Encode a complete JSON value as UTF-8 bytes. Values constructed in code
	## are validated before output; invalid numbers, duplicate keys and resource
	## limits retain structured diagnostics with a generated-value location.
	##
	## ```roc
	## expect {
	##     value : JsonValue
	##     value = Number("1")
	##     value.encode({}) == Ok("1".to_utf8())
	## }
	## ```
	encode : JsonValue, WriteProps -> Try(List(U8), [InvalidJsonValue(Error)])
	encode = |value, props| write_text(value, props).map_ok(Str.to_utf8).map_err(encoding_error)

}

# ------------------------------------------------------------------ reading

Failure : { offset : U64, problem : JsonValue.Problem }

fail : U64, JsonValue.Problem -> Try(_, [JsonFail(Failure)])
fail = |offset, problem| Err(JsonFail({ offset, problem }))

locate : Str, U64, JsonValue.Problem -> JsonValue.Error
locate = |input, offset, problem| {
	position = Parser.Quotation.position(input, offset)
	Diagnostic.{ location: Text({ offset, line: position.line, column: position.column }), path: [], problem: own_json_problem(problem) }
}

## An array or object whose closing bracket has not been read yet, saved
## while a nested one is open: what kind it is, where its children start in
## the shared `items` or `members` list, and its pending member key.
Frame : JsonWorkspace.Frame

Container : JsonWorkspace.Container

Member : JsonWorkspace.Member(JsonValue)

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
scan : Str, List(U8), JsonValue.Props -> Try({ value : JsonValue, pos : U64 }, [JsonFail(Failure)])
scan = |src, bytes, options| scan_reusing(src, bytes, options, JsonWorkspace.empty()).map_ok(|parsed| { value: parsed.value, pos: parsed.pos })

scan_reusing : Str, List(U8), JsonValue.Props, JsonWorkspace(JsonValue) -> Try({ value : JsonValue, pos : U64, workspace : JsonWorkspace(JsonValue) }, [JsonFail(Failure)])
scan_reusing = |src, bytes, options, workspace| {
	if bytes.len() > options.max_bytes {
		return fail(options.max_bytes, LimitExceeded({ resource: DocumentBytes, max: options.max_bytes, observed: AtLeast((options.max_bytes) + 1) }))
	}
	check = src.is_empty()
	comments = options.comments
	max_depth = options.max_depth
	max_items = options.max_items
	non_finite = options.non_finite
	start = skip_bom(bytes, options.bom)?
	var $pos = skip_ws(bytes, start, comments, check)?
	check_top_level(bytes, $pos, options)?
	var $open : List(Frame)
	var $open = workspace.open
	var $items : List(JsonValue)
	var $items = workspace.items
	var $members : List(Member)
	var $members = workspace.members
	var $key_ats : List(U64)
	var $key_ats = workspace.key_ats
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
					return Ok({ value: $value, pos: end, workspace: JsonWorkspace.{ open: $open, items: $items, members: $members, key_ats: $key_ats } })
				}

				InArray => {
					$items = $items.append($value)
					# Transfer the completed subtree without retaining a second local
					# reference while the scanner starts the next value.
					$value = Null
					after = skip_ws(bytes, $pos, comments, check)?
					byte = bytes.get(after) ?? 0
					var $close = after
					if byte == ',' {
						next = skip_ws(bytes, after + 1, comments, check)?
						if bytes.get(next) == Ok(']') {
							if !options.trailing_commas {
								return fail(after, TrailingComma)
							}
							$close = next
						} else if $items.len() - $first >= max_items {
							return fail(next, LimitExceeded({ resource: ArrayItems, max: max_items, observed: AtLeast((max_items) + 1) }))
						} else {
							$have = False
							$pos = next
						}
					} else if byte != ']' {
						return fail(after, ExpectedArrayDelimiter)
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
					$value = Null
					$key_ats = $key_ats.append($key_at)
					after = skip_ws(bytes, $pos, comments, check)?
					byte = bytes.get(after) ?? 0
					var $close = after
					if byte == ',' {
						next = skip_ws(bytes, after + 1, comments, check)?
						if bytes.get(next) == Ok('}') {
							if !options.trailing_commas {
								return fail(after, TrailingComma)
							}
							$close = next
						} else if $members.len() - $first >= max_items {
							return fail(next, LimitExceeded({ resource: ObjectMembers, max: max_items, observed: AtLeast((max_items) + 1) }))
						} else {
							member = scan_key(src, bytes, next, options)?
							$key = member.key
							$key_at = next
							$have = False
							$pos = member.pos
						}
					} else if byte != '}' {
						return fail(after, ExpectedObjectDelimiter)
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
					return fail($pos, LimitExceeded({ resource: Nesting, max: max_depth, observed: AtLeast((max_depth) + 1) }))
				}
				next = skip_ws(bytes, $pos + 1, comments, check)?
				if bytes.get(next) == Ok(']') {
					$value = Array([])
					$pos = next + 1
					$have = True
				} else if max_items == 0 {
					return fail(next, LimitExceeded({ resource: ArrayItems, max: 0, observed: AtLeast((0) + 1) }))
				} else {
					$open = $open.append({ container: $container, first: $first, key: $key, key_at: $key_at })
					$container = InArray
					$first = $items.len()
					$pos = next
				}
			} else if byte == '{' {
				if $open.len() >= max_depth {
					return fail($pos, LimitExceeded({ resource: Nesting, max: max_depth, observed: AtLeast((max_depth) + 1) }))
				}
				next = skip_ws(bytes, $pos + 1, comments, check)?
				if bytes.get(next) == Ok('}') {
					$value = Object([])
					$pos = next + 1
					$have = True
				} else if max_items == 0 {
					return fail(next, LimitExceeded({ resource: ObjectMembers, max: 0, observed: AtLeast((0) + 1) }))
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
					return fail($pos, InvalidLiteral)
				}
			} else if byte == 'f' {
				if at3(bytes, $pos + 1, 'a', 'l', 's') and bytes.get($pos + 4) == Ok('e') {
					$value = Bool(False)
					$pos = $pos + 5
					$have = True
				} else {
					return fail($pos, InvalidLiteral)
				}
			} else if byte == 'n' {
				if at3(bytes, $pos + 1, 'u', 'l', 'l') {
					$value = Null
					$pos = $pos + 4
					$have = True
				} else {
					return fail($pos, InvalidLiteral)
				}
			} else if byte == '-' or (byte >= '0' and byte <= '9') or (non_finite and (byte == 'N' or byte == 'I')) {
				number = scan_number(src, bytes, $pos, options)?
				$value = Number(number.str)
				$pos = number.end
				$have = True
			} else {
				return fail($pos, ExpectedValue)
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
			fail(0, BomNotAllowed)
		}
	} else {
		Ok(0)
	}
}

## Refuse a document whose value at `pos` has the wrong shape for `top_level`.
## Bytes that start no value are left for the scanner to report.
check_top_level : List(U8), U64, JsonValue.Props -> Try({}, [JsonFail(Failure)])
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
				fail(pos, TopLevelRequired(ObjectOrArray))
			} else {
				Ok({})
			}

		Object =>
			if scalar or byte == '[' {
				fail(pos, TopLevelRequired(Object))
			} else {
				Ok({})
			}
	}
}

## The key of an object member starting at `start`, and the position after
## the `:` that follows it and any whitespace after that.
scan_key : Str, List(U8), U64, JsonValue.Props -> Try({ key : Str, pos : U64 }, [JsonFail(Failure)])
scan_key = |src, bytes, start, options| {
	if bytes.get(start) != Ok('"') {
		return fail(start, ExpectedStringKey)
	}
	string = scan_string(src, bytes, start, options)?
	colon = skip_ws(bytes, string.end, options.comments, src.is_empty())?
	if bytes.get(colon) != Ok(':') {
		return fail(colon, ExpectedKeyColon)
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
					member_key = member.key
					match member_key_index($out, member_key) {
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

								Err(problem) => return fail(key_ats.get($index) ?? 0, problem)
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

								Err(problem) => return fail(key_ats.get($index) ?? 0, problem)
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

# Search only key strings; the compared members' nested values are unrelated
# to duplicate detection and must not be retained for each comparison.
member_key_index : List(Member), Str -> Try(U64, [NotFound])
member_key_index = |members, key| {
	var $index = 0.U64
	while $index < members.len() {
		found_key = match members.get($index) {
			Ok({ key: present, .. }) => present
			_ => ""
		}
		if found_key == key {
			return Ok($index)
		}
		$index = $index + 1
	}
	Err(NotFound)
}

## The most members an object may have for its keys to be compared one by one.
small_object : U64
small_object = 16

## What to do with `member` when `out[earlier]` has the same key.
duplicate_action : List(Member), U64, Member, [Reject, CollapseIdentical, KeepFirst, KeepLast] -> Try([Skip, Replace], JsonValue.Problem)
duplicate_action = |out, earlier, member, duplicates| {
	match duplicates {
		Reject => Err(DuplicateKey(member.key))
		KeepFirst => Ok(Skip)
		KeepLast => Ok(Replace)
		CollapseIdentical =>
			match out.get(earlier) {
				Ok(kept) if kept.value == member.value => Ok(Skip)
				_ => Err(ConflictingDuplicateKey(member.key))
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
			$pos = Parser.Utf8.skip_class(bytes, $pos + 1, ws_class)
		} else if byte == '/' {
			if !comments {
				return fail($pos, CommentsNotAllowed)
			}
			next = bytes.get($pos + 1) ?? 0
			if next == '/' {
				end = Parser.Utf8.find_line_end(bytes, $pos + 2)
				if check {
					check_utf8(bytes, $pos + 2, end)?
				}
				$pos = end
			} else if next == '*' {
				close = block_comment_end(bytes, $pos + 2)
				if close >= bytes.len() {
					return fail($pos, UnterminatedComment)
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
	var $pos = Parser.Utf8.find_any(bytes, start, star_class)
	while $pos < bytes.len() and bytes.get($pos + 1) != Ok('/') {
		$pos = Parser.Utf8.find_any(bytes, $pos + 1, star_class)
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
		Err(BadUtf8({ index, .. })) => fail(start + index, InvalidUtf8)
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
			Err(BadUtf8({ index, .. })) => fail(start + index, InvalidUtf8)
		}
	} else {
		match src.drop_first_bytes(start) {
			Ok(tail) => Ok(tail.drop_last_bytes(bytes.len() - end) ?? "")
			Err(_) => Ok("")
		}
	}
}

# JSON strings have only three stop conditions. Direct vector comparisons
# avoid the generic byte-class lookup tables on this frequent short-token path.
plain_string_end : List(U8), U64 -> U64
plain_string_end = |bytes, start| {
	quote_vector = U8x16.splat('"')
	backslash = U8x16.splat('\\')
	high = U8x16.splat(0xE0)
	zero = U8x16.splat(0)
	var $pos = start
	while True {
		match U8x16.load(bytes, $pos) {
			Ok(vector) => {
				control = U8x16.eq_lanes(U8x16.bitwise_and(vector, high), zero)
				mask = U8x16.to_bitmask(U8x16.bitwise_or(control, U8x16.bitwise_or(U8x16.eq_lanes(vector, quote_vector), U8x16.eq_lanes(vector, backslash))))
				if mask != 0 {
					return $pos + U16.count_trailing_zero_bits(mask).to_u64()
				}
				$pos = $pos + 16
			}
			Err(_) => {
				while $pos < bytes.len() {
					byte = bytes.get($pos) ?? 0
					if byte < 0x20 or byte == '"' or byte == '\\' {
						return $pos
					}
					$pos = $pos + 1
				}
				return $pos
			}
		}
	}
}

## The string whose opening quote is at `start`, decoded, and the position
## after its closing quote. A string without escapes is cut from the input.
scan_string : Str, List(U8), U64, JsonValue.Props -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_string = |src, bytes, start, options| {
	run_end = plain_string_end(bytes, start + 1)
	if bytes.get(run_end) == Ok('"') {
		if run_end - start - 1 > options.max_string_bytes {
			return fail(start, LimitExceeded({ resource: StringBytes, max: options.max_string_bytes, observed: AtLeast((options.max_string_bytes) + 1) }))
		}
		Ok({ str: slice_checked(src, bytes, start + 1, run_end)?, end: run_end + 1 })
	} else {
		scan_escaped_string(bytes, start, run_end, options)
	}
}

## The rest of a string that needs decoding, from `run_end`, where the plain
## run that starts it ends.
scan_escaped_string : List(U8), U64, U64, JsonValue.Props -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_escaped_string = |bytes, start, run_end, options| {
	var $out = List.with_capacity(run_end - start + 16)
	$out = $out.append_sublist(bytes, { start: start + 1, len: run_end - start - 1 })
	var $pos = run_end
	while True {
		byte =
			match bytes.get($pos) {
				Ok(b) => b
				Err(_) => return fail(start, UnterminatedString)
			}
		if byte == '"' {
			if $out.len() > options.max_string_bytes {
				return fail(start, LimitExceeded({ resource: StringBytes, max: options.max_string_bytes, observed: AtLeast((options.max_string_bytes) + 1) }))
			}
			return match Str.from_utf8($out) {
				Ok(str) => Ok({ str, end: $pos + 1 })
				Err(_) => {
					# Escapes decode to valid UTF-8, so the bad bytes are in the input.
					check_utf8(bytes, start + 1, $pos)?
					fail(start, InvalidUtf8)
				}
			}
		} else if byte == '\\' {
			escape =
				match bytes.get($pos + 1) {
					Ok(b) => b
					Err(_) => return fail(start, UnterminatedString)
				}
			if escape == 'u' {
				code =
					match hex4(bytes, $pos + 2) {
						Ok(c) => c
						Err(_) => return fail($pos, InvalidUnicodeEscape)
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

							Reject => return fail($pos, UnpairedSurrogate)
						}
					}
				} else if code >= 0xDC00 and code <= 0xDFFF {
					match options.lone_surrogates {
						Replace => {
							$out = append_utf8($out, 0xFFFD)
							$pos = $pos + 6
						}

						Reject => return fail($pos, UnpairedSurrogate)
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
						return fail($pos, InvalidEscape)
					}
				$out = $out.append(decoded)
				$pos = $pos + 2
			}
		} else if byte < 0x20 {
			if !options.control_characters {
				return fail($pos, ControlCharacter(byte.to_u32()))
			}
			$out = $out.append(byte)
			$pos = $pos + 1
		} else {
			run = plain_string_end(bytes, $pos + 1)
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
scan_number : Str, List(U8), U64, JsonValue.Props -> Try({ str : Str, end : U64 }, [JsonFail(Failure)])
scan_number = |src, bytes, start, options| {
	end =
		match non_finite_end(bytes, start, options.non_finite) {
			Ok(after) => after
			Err(_) => number_end(bytes, start)?
		}
	if end - start > options.max_number_digits {
		return fail(start, LimitExceeded({ resource: NumberCharacters, max: options.max_number_digits, observed: AtLeast((options.max_number_digits) + 1) }))
	}
	if options.interoperable_numbers {
		match interoperable(bytes, start, end) {
			Ok({}) => {}
			Err(reason) => return fail(start, NonInteroperableNumber({ text: slice_checked(src, bytes, start, end)?, reason }))
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
			return fail(start, LeadingZero)
		}
	} else if first >= '1' and first <= '9' {
		$pos = digits_end(bytes, $pos + 1)
	} else if $pos > start {
		return fail(start, ExpectedMinusDigit)
	} else {
		return fail(start, ExpectedValue)
	}
	if bytes.get($pos) == Ok('.') {
		if !is_digit(bytes.get($pos + 1) ?? 0) {
			return fail(start, ExpectedFractionDigit)
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
			return fail(start, ExpectedExponentDigit)
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

## Whether the whole of `bytes` is a JSON number.
is_number_lexeme : List(U8) -> Bool
is_number_lexeme = |bytes| {
	match number_end(bytes, 0) {
		Ok(end) => end == bytes.len()
		Err(_) => False
	}
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

## Whether `bytes` is `NaN`, `Infinity` or `-Infinity`.
is_non_finite_lexeme : List(U8) -> Bool
is_non_finite_lexeme = |bytes| non_finite_end(bytes, 0, True) == Ok(bytes.len())

## Why the number [start, end) cannot be held by an IEEE 754 double as
## I-JSON (RFC 7493 section 2.2) requires, if it cannot.
interoperable : List(U8), U64, U64 -> Try({}, JsonValue.NumberProblem)
interoperable = |bytes, start, end| {
	sign = if (bytes.get(start) == Ok('-')) 1 else 0
	first = bytes.get(start + sign) ?? 0
	if first == 'N' or first == 'I' {
		return Err(NotFinite)
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
			Err(UnsafeInteger)
		} else {
			Ok({})
		}
	} else if $nonzero and $last_nonzero - $first_nonzero + 1 > 17 {
		Err(SignificantDigits)
	} else {
		match F64.from_str(Str.from_utf8(bytes.sublist({ start, len: end - start })) ?? "") {
			Ok(double) =>
				if !double.is_finite() {
					Err(DoubleOverflow)
				} else if $nonzero and double == 0.0 {
					Err(DoubleUnderflow)
				} else {
					Ok({})
				}

			Err(_) => Err(DoubleOverflow)
		}
	}
}

## 2^53 - 1, the largest integer every IEEE 754 double can hold exactly.
max_safe_integer : U64
max_safe_integer = 9007199254740991

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
plain_string_class : Parser.Utf8.ByteClass
plain_string_class = Parser.Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b != '"' and b != '\\')

ws_class : Parser.Utf8.ByteClass
ws_class = Parser.Utf8.ByteClass.from_bytes([' ', '\t', '\n', '\r'])

star_class : Parser.Utf8.ByteClass
star_class = Parser.Utf8.ByteClass.from_bytes(['*'])

# ------------------------------------------------------------------ writing

WriteError : [InvalidNumber(Str), DuplicateKey(Str), TooDeep, TooLarge(U64)]

# Keep all arithmetic within the caller's limit, including U64.highest.
measure_add : U64, U64, U64 -> Try(U64, [TooLarge(U64)])
measure_add = |used, count, limit| {
	if used > limit or count > limit - used {
		return Err(TooLarge(limit))
	}
	Ok(used + count)
}

measure_line : U64, U64, U64, U64 -> Try(U64, [TooLarge(U64)])
measure_line = |used, indent, depth, limit| {
	if indent == 0 {
		return Ok(used)
	}
	next = measure_add(used, 1, limit)?
	if depth > 0 and indent > (limit - next) // depth {
		return Err(TooLarge(limit))
	}
	measure_add(next, indent * depth, limit)
}

measure_string : Str, U64, U64 -> Try(U64, [TooLarge(U64)])
measure_string = |text, used, limit| {
	start = measure_add(used, 2, limit)?
	_ = measure_add(start, text.count_utf8_bytes(), limit)?
	var $used = start
	for byte in text.to_utf8() {
		count = if (byte == '"' or byte == '\\' or [8, 9, 10, 12, 13].contains(byte)) 2 else if (byte < 32) 6 else 1
		$used = measure_add($used, count, limit)?
	}
	Ok($used)
}

# Measure exactly the same representation as write_value. Key sorting changes
# order, not length. No serialized buffer is allocated during this traversal.
measure_value : JsonValue, U64, U64, U64, U64 -> Try(U64, WriteError)
measure_value = |value, indent, depth, used, limit| {
	match value {
		Null => measure_add(used, 4, limit)
		Bool(flag) => measure_add(used, if (flag) 4 else 5, limit)
		String(text) => measure_string(text, used, limit)
		Number(text) => {
			next = measure_add(used, text.count_utf8_bytes(), limit)?
			if !is_number_lexeme(text.to_utf8()) {
				return Err(InvalidNumber(text))
			}
			Ok(next)
		}
		Array(items) => {
			if depth >= max_write_depth {
				return Err(TooDeep)
			}
			if items.is_empty() {
				return measure_add(used, 2, limit)
			}
			var $used = measure_add(used, 1, limit)?
			var $later = False
			for item in items {
				if $later {
					$used = measure_add($used, 1, limit)?
				}
				$used = measure_line($used, indent, depth + 1, limit)?
				$used = measure_value(item, indent, depth + 1, $used, limit)?
				$later = True
			}
			measure_add(measure_line($used, indent, depth, limit)?, 1, limit)
		}
		Object(members) => {
			if depth >= max_write_depth {
				return Err(TooDeep)
			}
			if members.is_empty() {
				return measure_add(used, 2, limit)
			}
			var $used = measure_add(used, 1, limit)?
			check_unique_keys(members)?
			var $later = False
			for member in members {
				if $later {
					$used = measure_add($used, 1, limit)?
				}
				$used = measure_line($used, indent, depth + 1, limit)?
				$used = measure_string(member.key, $used, limit)?
				$used = measure_add($used, if (indent == 0) 1 else 2, limit)?
				$used = measure_value(member.value, indent, depth + 1, $used, limit)?
				$later = True
			}
			measure_add(measure_line($used, indent, depth, limit)?, 1, limit)
		}
	}
}

## The deepest nesting the writer accepts, the parser's default limit.
max_write_depth : U64
max_write_depth = 128

## `buf` with `value` written after it. `depth` counts the arrays and objects
## around `value`.
write_value : List(U8), JsonValue, Bool, U64, U64 -> Try(List(U8), WriteError)
write_value = |buf, value, sort_keys, indent, depth| {
	match value {
		Null => Ok(buf.append('n').append('u').append('l').append('l'))
		Bool(flag) =>
			if flag {
				Ok(buf.append('t').append('r').append('u').append('e'))
			} else {
				Ok(buf.append('f').append('a').append('l').append('s').append('e'))
			}

		Number(lexeme) => {
			bytes = lexeme.to_utf8()
			if is_number_lexeme(bytes) {
				Ok(buf.append_sublist(bytes, { start: 0, len: bytes.len() }))
			} else {
				Err(InvalidNumber(lexeme))
			}
		}

		String(text) => Ok(write_string(buf, text))
		Array(items) => {
			if depth >= max_write_depth {
				return Err(TooDeep)
			}
			if items.is_empty() {
				return Ok(buf.append('[').append(']'))
			}
			var $out = buf.append('[')
			var $later = False
			for item in items {
				if $later {
					$out = $out.append(',')
				}
				$out = write_value(new_line($out, indent, depth + 1), item, sort_keys, indent, depth + 1)?
				$later = True
			}
			Ok(new_line($out, indent, depth).append(']'))
		}

		Object(members) => {
			if depth >= max_write_depth {
				return Err(TooDeep)
			}
			if members.is_empty() {
				return Ok(buf.append('{').append('}'))
			}
			check_unique_keys(members)?
			ordered = if (sort_keys) members.sort_with(compare_keys) else members
			var $out = buf.append('{')
			var $later = False
			for member in ordered {
				if $later {
					$out = $out.append(',')
				}
				$out = write_string(new_line($out, indent, depth + 1), member.key).append(':')
				if indent > 0 {
					$out = $out.append(' ')
				}
				$out = write_value($out, member.value, sort_keys, indent, depth + 1)?
				$later = True
			}
			Ok(new_line($out, indent, depth).append('}'))
		}
	}
}

## `buf` with a line break and the indentation for `depth` levels, when
## writing indented output.
new_line : List(U8), U64, U64 -> List(U8)
new_line = |buf, indent, depth| {
	if indent == 0 {
		buf
	} else {
		var $out = buf.append('\n')
		var $level = 0
		while $level < depth {
			var $spaces = 0
			while $spaces < indent {
				$out = $out.append(' ')
				$spaces = $spaces + 1
			}
			$level = $level + 1
		}
		$out
	}
}

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
		run = plain_string_end(bytes, $pos)
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

## `Err(DuplicateKey(key))` for the first key repeated among `members`.
check_unique_keys : List(Member) -> Try({}, WriteError)
check_unique_keys = |members| {
	if members.len() <= small_object {
		var $index = 1
		while $index < members.len() {
			match members.get($index) {
				Ok(member) =>
					if members.take_first($index).any(|earlier| earlier.key == member.key) {
						return Err(DuplicateKey(member.key))
					}

				Err(_) => {}
			}
			$index = $index + 1
		}
	} else {
		var $seen = Set.with_capacity(members.len())
		for member in members {
			if $seen.contains(member.key) {
				return Err(DuplicateKey(member.key))
			}
			$seen = $seen.insert(member.key)
		}
	}
	Ok({})
}

## Order members by the UTF-8 bytes of their keys, which is code point order.
compare_keys : Member, Member -> [Before, Same, After]
compare_keys = |left, right| {
	a = left.key.to_utf8()
	b = right.key.to_utf8()
	var $index = 0
	while $index < a.len() and $index < b.len() {
		x = a.get($index) ?? 0
		y = b.get($index) ?? 0
		if x < y {
			return Before
		}
		if x > y {
			return After
		}
		$index = $index + 1
	}
	if a.len() < b.len() {
		Before
	} else if a.len() > b.len() {
		After
	} else {
		Same
	}
}

# ------------------------------------------------------------------ tests

# The error `parse_str` reports for `input`, or `Err(Parsed)`.
error_of : Str, JsonValue.Props -> Try({ line : U64, column : U64, problem : JsonValue.Problem }, [Parsed])
error_of = |input, options| {
	match JsonValue.parse_str(input, options) {
		Ok(_) => Err(Parsed)
		Err(InvalidJsonValue(error)) => {
			position = Diagnostic.line_column(error.location) ?? crash "JSON source diagnostic must retain text coordinates"
			Ok({ line: position.line, column: position.column, problem: error.problem })
		}
	}
}

# The typed problem `parse_str` reports, or `Err(Parsed)`.
problem_of : Str, JsonValue.Props -> Try(JsonValue.Problem, [Parsed])
problem_of = |input, options| error_of(input, options).map_ok(|error| error.problem)

# The failure `parser` reports for raw bytes, or `Err(Parsed)`.
parser_failure : List(U8), JsonValue.Props -> Try({ problem : Parser.Problem(JsonValue.Problem), offset : U64 }, [Parsed])
parser_failure = |bytes, options| {
	match Parser.run(JsonValue.parser(options), bytes) {
		Ok(_) => Err(Parsed)
		Err(ParseError(failure)) => match failure.location {
			InputOffset(offset) => Ok({ offset, problem: failure.problem })
			_ => crash "JSON byte parser must retain relative input coordinates"
		}
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
key_names : U64 -> List(Str)
key_names = |count| {
	var $out = []
	var $index = 0
	while $index < count {
		$out = $out.append("k${$index.to_str()}")
		$index = $index + 1
	}
	$out
}

# `depth` arrays nested around `inner`.
nested : U64, Str -> Str
nested = |depth, inner| Str.concat(Str.repeat("[", depth), Str.concat(inner, Str.repeat("]", depth)))

# `depth` arrays nested around `Null`, built as a value.
nested_value : U64 -> JsonValue
nested_value = |depth| {
	var $value = Null
	var $index = 0
	while $index < depth {
		$value = Array([$value])
		$index = $index + 1
	}
	$value
}

# --- Ported from roc-blueprint's JsonData (62bedf4).

# Unicode surrogate pairs and escaped controls survive exact JSON round trips.
expect {
	text = "a\n\t\"\\é😀"
	write_text(String(text), {}).map_ok(|rendered| JsonValue.parse_str(rendered, {})) == Ok(Ok(String(text)))
		and JsonValue.parse_str("\"\\ud83d\\ude00\"", {}) == Ok(String("😀"))
}

# Malformed documents are refused.
expect
	[
		"",
		"{}{}",
		"{\"a\":1,\"a\":2}",
		"[1,]",
		"01",
		"1e",
		"\"\\ud800\"",
		"\"\\udc00\"",
		"\"\\x00\"",
		"{\"a\" 1}",
		"true false",
	]
		.all(|rendered| JsonValue.parse_str(rendered, {}).is_err())

# Booleans, numbers, arrays and null remain typed data through a round trip.
expect {
	match JsonValue.parse_str("{\"a\":[true,false,null,-1.25e+2,0]}", {}) {
		Ok(value) => write_text(value, {}).map_ok(|rendered| JsonValue.parse_str(rendered, {})) == Ok(Ok(value))
		Err(_) => False
	}
}

# Depth and collection bounds reject oversized structures.
expect {
	items = Str.join_with(List.repeat("null", 4097), ",")
	JsonValue.parse_str(nested(130, "null"), {}).is_err()
		and JsonValue.parse_str("[${items}]", { max_items: 4096 }).is_err()
}

# Sorted output orders the members of every object.
expect {
	result = JsonValue.parse_str("{\"b\":[{\"z\":1,\"a\":2}],\"a\":{\"d\":\"x\",\"c\":true}}", {}).map_ok(|value| write_text(value, { sort_keys: True }))
	result == Ok(Ok("{\"a\":{\"c\":true,\"d\":\"x\"},\"b\":[{\"a\":2,\"z\":1}]}"))
}

# Constructed numbers cannot inject syntax.
expect ["", "null", "01", "1,2", "1e", "NaN", "1}\n{\"injected\":true"].all(|text| write_text(Number(text), {}) == Err(InvalidNumber(text)))
expect write_text(Object([{ key: "a", value: Null }, { key: "a", value: Bool(True) }]), {}) == Err(DuplicateKey("a"))
expect write_text(Number("340282366920938463463374607431768211455"), {}) == Ok("340282366920938463463374607431768211455")
expect JsonValue.parse_str("{\"a\":1,\"\\u0061\":2}", {}).is_err()

# Writing more than 128 levels of nesting is refused.
expect write_text(nested_value(130), {}) == Err(TooDeep)

# A string limit counts decoded bytes.
expect {
	text = Str.repeat("a", 65536)
	JsonValue.parse_str("\"${text}\"", { max_string_bytes: 65536 }) == Ok(String(text))
		and JsonValue.parse_str("\"${text}a\"", { max_string_bytes: 65536 }).is_err()
}

# Larger documents may hold more items when the limit allows.
expect {
	items = Str.join_with(List.repeat("0", 5000), ",")
	JsonValue.parse_str("[${items}]", { max_items: 4096 }).is_err() and JsonValue.parse_str("[${items}]", {}).is_ok()
}

# A key repeated with an equal value is one key only where the options say so,
# and a conflicting one never is.
expect JsonValue.parse_str("{\"a\":[1],\"a\":[1]}", {}).is_err()
expect JsonValue.parse_str("{\"a\":[1],\"b\":2,\"a\":[1]}", { duplicates: CollapseIdentical }) == Ok(Object([{ key: "a", value: Array([Number("1")]) }, { key: "b", value: Number("2") }]))
expect JsonValue.parse_str("{\"a\":1,\"a\":2}", { duplicates: CollapseIdentical }).is_err()
expect JsonValue.keys(Object([{ key: "x", value: Null }])) == ["x"]
expect
	[("0", Ok(0)), ("12", Ok(12)), ("3.00", Err(WrongType)), ("01", Err(WrongType)), ("1.5", Err(WrongType)), ("-1", Ok(-1)), ("1e3", Err(WrongType))]
		.all(|(lexeme, expected)| JsonValue.as_i64(Number(lexeme)) == expected)

# --- Grammar.

# Whitespace may surround every token.
expect JsonValue.parse_str(" \t\n\r[ 1 , 2 ]\r\n ", {}) == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.parse_str("{ \"a\" : { } , \"b\" : [ ] }", {}) == Ok(Object([{ key: "a", value: Object([]) }, { key: "b", value: Array([]) }]))

# Scalars at the top level.
expect JsonValue.parse_str("null", {}) == Ok(Null) and JsonValue.parse_str("true", {}) == Ok(Bool(True)) and JsonValue.parse_str("false", {}) == Ok(Bool(False))
expect JsonValue.parse_str("\"\"", {}) == Ok(String("")) and JsonValue.parse_str("[]", {}) == Ok(Array([])) and JsonValue.parse_str("{}", {}) == Ok(Object([]))

# Numbers keep their exact text.
expect ["0", "-0", "10", "1.0", "0.0", "0e0", "1E3", "-0.0e-0", "1e+2", "123456789012345678901234567890"].all(|rendered| JsonValue.parse_str(rendered, {}) == Ok(Number(rendered)))

# Number grammar errors point at the start of the number.
expect error_of("01", {}) == Ok({ line: 1, column: 1, problem: LeadingZero })
expect error_of("[-01]", {}) == Ok({ line: 1, column: 2, problem: LeadingZero })
expect error_of("00", {}) == Ok({ line: 1, column: 1, problem: LeadingZero })
expect error_of("-", {}) == Ok({ line: 1, column: 1, problem: ExpectedMinusDigit })
expect error_of("[1, -a]", {}) == Ok({ line: 1, column: 5, problem: ExpectedMinusDigit })
expect error_of("1.", {}) == Ok({ line: 1, column: 1, problem: ExpectedFractionDigit })
expect error_of("[1.e3]", {}) == Ok({ line: 1, column: 2, problem: ExpectedFractionDigit })
expect ["1e", "1e+", "1E-", "1.5ex"].all(|text| problem_of(text, {}) == Ok(ExpectedExponentDigit))
expect error_of(".5", {}) == Ok({ line: 1, column: 1, problem: ExpectedValue })
expect error_of("+1", {}) == Ok({ line: 1, column: 1, problem: ExpectedValue })

# Literals are exact.
expect error_of("tru", {}) == Ok({ line: 1, column: 1, problem: InvalidLiteral })
expect error_of("[nul]", {}) == Ok({ line: 1, column: 2, problem: InvalidLiteral })
expect error_of("fals", {}) == Ok({ line: 1, column: 1, problem: InvalidLiteral })
expect error_of("True", {}) == Ok({ line: 1, column: 1, problem: ExpectedValue })
expect error_of("truex", {}) == Ok({ line: 1, column: 5, problem: UnexpectedContent })

# Structural errors point at the offending byte.
expect error_of("", {}) == Ok({ line: 1, column: 1, problem: ExpectedValue })
expect error_of("  ", {}) == Ok({ line: 1, column: 3, problem: ExpectedValue })
expect error_of("[", {}) == Ok({ line: 1, column: 2, problem: ExpectedValue })
expect error_of("[1", {}) == Ok({ line: 1, column: 3, problem: ExpectedArrayDelimiter })
expect error_of("[1 2]", {}) == Ok({ line: 1, column: 4, problem: ExpectedArrayDelimiter })
expect error_of("[1,,2]", {}) == Ok({ line: 1, column: 4, problem: ExpectedValue })
expect error_of("{", {}) == Ok({ line: 1, column: 2, problem: ExpectedStringKey })
expect error_of("{1:2}", {}) == Ok({ line: 1, column: 2, problem: ExpectedStringKey })
expect error_of("{\"a\" 1}", {}) == Ok({ line: 1, column: 6, problem: ExpectedKeyColon })
expect error_of("{\"a\"", {}) == Ok({ line: 1, column: 5, problem: ExpectedKeyColon })
expect error_of("{\"a\":}", {}) == Ok({ line: 1, column: 6, problem: ExpectedValue })
expect error_of("{\"a\":1", {}) == Ok({ line: 1, column: 7, problem: ExpectedObjectDelimiter })
expect error_of("{\"a\":1 \"b\":2}", {}) == Ok({ line: 1, column: 8, problem: ExpectedObjectDelimiter })
expect error_of("{\"a\":1,\"b\"}", {}) == Ok({ line: 1, column: 11, problem: ExpectedKeyColon })
expect error_of("[1] x", {}) == Ok({ line: 1, column: 5, problem: UnexpectedContent })
expect error_of("[1]]", {}) == Ok({ line: 1, column: 4, problem: UnexpectedContent })
expect error_of("[1}", {}) == Ok({ line: 1, column: 3, problem: ExpectedArrayDelimiter })

# A trailing comma is refused at the comma.
expect error_of("[1,]", {}) == Ok({ line: 1, column: 3, problem: TrailingComma })
expect error_of("{\"a\":1,}", {}) == Ok({ line: 1, column: 7, problem: TrailingComma })

# Lines count LF, CR and CRLF; columns count bytes.
expect error_of("[\n1,\n]", {}) == Ok({ line: 2, column: 2, problem: TrailingComma })
expect error_of("{\r\n\"a\": tru\r\n}", {}) == Ok({ line: 2, column: 6, problem: InvalidLiteral })
expect error_of("[\"é\",\r\"ü\" x]", {}) == Ok({ line: 2, column: 6, problem: ExpectedArrayDelimiter })

# --- Strings.

expect JsonValue.parse_str("\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"", {}) == Ok(String("\"\\/\u(8)\u(C)\n\r\t"))
expect JsonValue.parse_str("\"\\u00e9\\u00E9\\u0041\\u0000\"", {}) == Ok(String("ééA\u(0)"))
expect JsonValue.parse_str("\"héllo 😀\"", {}) == Ok(String("héllo 😀"))
expect JsonValue.parse_str("\"é\\n😀\"", {}) == Ok(String("é\n😀"))
expect JsonValue.parse_str("{\"a\\nb\":1}", {}) == Ok(Object([{ key: "a\nb", value: Number("1") }]))

# Surrogate pairs combine, in either case of hexadecimal digits.
expect JsonValue.parse_str("\"\\uD83D\\uDE00\"", {}) == Ok(String("😀"))
expect JsonValue.parse_str("\"\\ud800\\udc00\\udbff\\udfff\"", {}) == Ok(String("\u(10000)\u(10FFFF)"))

# A lone surrogate is refused at its escape.
expect error_of("\"\\ud800\"", {}) == Ok({ line: 1, column: 2, problem: UnpairedSurrogate })
expect error_of("\"a\\udc00\"", {}) == Ok({ line: 1, column: 3, problem: UnpairedSurrogate })
expect error_of("\"\\ud800\\u0041\"", {}) == Ok({ line: 1, column: 2, problem: UnpairedSurrogate })
expect error_of("\"\\udc00\\ud800\"", {}) == Ok({ line: 1, column: 2, problem: UnpairedSurrogate })

# Escapes must be known, and `\u` needs four hexadecimal digits.
expect error_of("\"\\x\"", {}) == Ok({ line: 1, column: 2, problem: InvalidEscape })
expect error_of("\"ab\\'\"", {}) == Ok({ line: 1, column: 4, problem: InvalidEscape })
expect error_of("\"\\U0041\"", {}) == Ok({ line: 1, column: 2, problem: InvalidEscape })
expect error_of("\"\\u12\"", {}) == Ok({ line: 1, column: 2, problem: InvalidUnicodeEscape })
expect error_of("[\"\\u12G4\"]", {}) == Ok({ line: 1, column: 3, problem: InvalidUnicodeEscape })

# An unterminated string is reported at its opening quote.
expect error_of("\"abc", {}) == Ok({ line: 1, column: 1, problem: UnterminatedString })
expect error_of("\"abc\\", {}) == Ok({ line: 1, column: 1, problem: UnterminatedString })
expect error_of("[\"a", {}) == Ok({ line: 1, column: 2, problem: UnterminatedString })
expect error_of("\"a\\\"", {}) == Ok({ line: 1, column: 1, problem: UnterminatedString })

# Raw control characters must be escaped; DEL and space need not be.
expect error_of("\"a\nb\"", {}) == Ok({ line: 1, column: 3, problem: ControlCharacter(0x000A) })
expect error_of("\"\u(0)\"", {}) == Ok({ line: 1, column: 2, problem: ControlCharacter(0x0000) })
expect error_of("\"\u(1F)\"", {}) == Ok({ line: 1, column: 2, problem: ControlCharacter(0x001F) })
expect error_of("{\"a\tb\":1}", {}) == Ok({ line: 1, column: 4, problem: ControlCharacter(0x0009) })
expect JsonValue.parse_str("\"\u(7F) \"", {}) == Ok(String("\u(7F) "))

# --- The parser.

expect (JsonValue.parser({})).run_str("[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
expect (JsonValue.parser({})).run_str("12abc") == Ok({ value: Number("12"), rest: "abc" })
expect (JsonValue.parser({ comments: True })).run_str("{} // c\nx") == Ok({ value: Object([]), rest: "x" })
expect (JsonValue.parser({})).parse_str("[1 2]") == Err(ParseError(Diagnostic.{ problem: ExpectedArrayDelimiter, location: Text({ offset: 3, line: 1, column: 4 }), path: [] }))
expect parser_failure([], {}) == Ok({ problem: ExpectedValue, offset: 0 })

# Input that is not UTF-8 is refused where the bad bytes are, in plain and
# escaped strings, keys and comments; the rest of the input may hold anything.
expect parser_failure([0xFF], {}) == Ok({ problem: ExpectedValue, offset: 0 })
expect parser_failure(['"', 0xFF, '"'], {}) == Ok({ problem: InvalidUtf8, offset: 1 })
expect parser_failure(['"', '\\', 'n', 'a', 0xC3, '"'], {}) == Ok({ problem: InvalidUtf8, offset: 4 })
expect parser_failure(['[', '"', 'a', 0xE2, 0x82, '"', ']'], {}) == Ok({ problem: InvalidUtf8, offset: 3 })
expect parser_failure(['{', '"', 0xED, 0xA0, 0x80, '"', ':', '1', '}'], {}) == Ok({ problem: InvalidUtf8, offset: 2 })
expect parser_failure(['1', ' ', '/', '/', 0xFF], { comments: True }) == Ok({ problem: InvalidUtf8, offset: 4 })
expect parser_failure(['1', '/', '*', 0xC0, '*', '/'], { comments: True }) == Ok({ problem: InvalidUtf8, offset: 3 })
expect Parser.run(JsonValue.parser({}), ['"', 'a', '"', ' ', 0xFF]) == Ok({ value: String("a"), rest: [0xFF] })
expect Parser.run(JsonValue.parser({}), ['[', '"', 0xC3, 0xA9, '\\', 't', '"', ']', 0xFF]) == Ok({ value: Array([String("é\t")]), rest: [0xFF] })

# The builtin `Json` fails with `InvalidJson(Str)`. A tag of a different name
# lets one function pass errors from both on with `?`.
TypedAndDynamic : { port : U64, extra : JsonValue }

read_both : Str, Str -> Try(TypedAndDynamic, [InvalidJson(Str), InvalidJsonValue(JsonValue.Error), MissingRequiredField(Str)])
read_both = |typed, dynamic| {
	settings : { port : U64 }
	settings = Json.parse(typed)?
	extra = JsonValue.parse_str(dynamic, {})?
	Ok({ port: settings.port, extra })
}

expect read_both("{\"port\": 8080}", "{\"@context\": 1}") == Ok({ port: 8080, extra: Object([{ key: "@context", value: Number("1") }]) })
expect read_both("{\"port\": 8080}", "[1,]") == Err(InvalidJsonValue(Diagnostic.{ location: Text({ offset: 3 - 1, line: 1, column: 3 }), path: [], problem: TrailingComma }))
expect read_both("{\"port\": \"x\"}", "{}").is_err()

# --- Limits.

# 128 levels of nesting are allowed by default, and the 129th bracket is not.
expect JsonValue.parse_str(nested(128, "1"), {}).is_ok()
expect error_of(nested(129, "1"), {}) == Ok({ line: 1, column: 129, problem: LimitExceeded({ resource: Nesting, max: 128, observed: AtLeast((128) + 1) }) })
expect error_of(nested(128, "{}"), {}) == Ok({ line: 1, column: 129, problem: LimitExceeded({ resource: Nesting, max: 128, observed: AtLeast((128) + 1) }) })
expect error_of("{\"a\":{\"b\":[]}}", { max_depth: 2 }) == Ok({ line: 1, column: 11, problem: LimitExceeded({ resource: Nesting, max: 2, observed: AtLeast((2) + 1) }) })
expect JsonValue.parse_str("{\"a\":{\"b\":1}}", { max_depth: 2 }).is_ok()

# With no nesting allowed, only scalars are accepted.
expect ["1", "\"s\"", "null"].all(|text| JsonValue.parse_str(text, { max_depth: 0 }).is_ok())
expect error_of("[]", { max_depth: 0 }) == Ok({ line: 1, column: 1, problem: LimitExceeded({ resource: Nesting, max: 0, observed: AtLeast((0) + 1) }) })
expect error_of(" {}", { max_depth: 0 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: Nesting, max: 0, observed: AtLeast((0) + 1) }) })
expect JsonValue.parse_str("[1]", { max_depth: 1 }).is_ok() and error_of("[[]]", { max_depth: 1 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: Nesting, max: 1, observed: AtLeast((1) + 1) }) })

# A raised limit is checked the same way, and deep nesting does not exhaust
# the call stack.
expect JsonValue.parse_str(nested(200, ""), { max_depth: 200 }).is_ok() and JsonValue.parse_str(nested(201, ""), { max_depth: 200 }).is_err()
expect JsonValue.parse_str(nested(20000, "1"), { max_depth: 20000 }).map_ok(|value| JsonValue.get_path(value, ["0", "0", "0"]).is_ok()) == Ok(True)

# `max_items` limits each array and object, at the first item past it.
expect JsonValue.parse_str("[1,2]", { max_items: 2 }).is_ok() and JsonValue.parse_str("[[1,2],[3,4]]", { max_items: 2 }).is_ok()
expect error_of("[1,2,3]", { max_items: 2 }) == Ok({ line: 1, column: 6, problem: LimitExceeded({ resource: ArrayItems, max: 2, observed: AtLeast((2) + 1) }) })
expect error_of("{\"a\":1,\"b\":2,\"c\":3}", { max_items: 2 }) == Ok({ line: 1, column: 14, problem: LimitExceeded({ resource: ObjectMembers, max: 2, observed: AtLeast((2) + 1) }) })
expect JsonValue.parse_str("[[],{}]", { max_items: 2 }).is_ok() and JsonValue.parse_str("[]", { max_items: 0 }).is_ok() and JsonValue.parse_str("{}", { max_items: 0 }).is_ok()
expect error_of("[1]", { max_items: 0 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: ArrayItems, max: 0, observed: AtLeast((0) + 1) }) })
expect error_of("{\"a\":1}", { max_items: 0 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: ObjectMembers, max: 0, observed: AtLeast((0) + 1) }) })
expect JsonValue.parse_str("[1,2,]", { max_items: 2, trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
expect parser_failure("[ 1]".to_utf8(), { max_items: 0 }) == Ok({ problem: LimitExceeded({ resource: ArrayItems, max: 0, observed: AtLeast((0) + 1) }), offset: 2 })

# Members are counted as written, before repeated keys are removed.
expect error_of("{\"a\":1,\"a\":1}", { max_items: 1, duplicates: CollapseIdentical }) == Ok({ line: 1, column: 8, problem: LimitExceeded({ resource: ObjectMembers, max: 1, observed: AtLeast((1) + 1) }) })
expect error_of("{\"a\":1,\"a\":1}", { max_items: 1 }) == Ok({ line: 1, column: 8, problem: LimitExceeded({ resource: ObjectMembers, max: 1, observed: AtLeast((1) + 1) }) })
expect error_of("{\"a\":1,\"a\":2}", { max_items: 1, duplicates: KeepFirst }) == Ok({ line: 1, column: 8, problem: LimitExceeded({ resource: ObjectMembers, max: 1, observed: AtLeast((1) + 1) }) })
expect error_of("{\"a\":1,\"a\":2}", { max_items: 1, duplicates: KeepLast }) == Ok({ line: 1, column: 8, problem: LimitExceeded({ resource: ObjectMembers, max: 1, observed: AtLeast((1) + 1) }) })
expect JsonValue.parse_str("{\"a\":1,\"a\":2}", { max_items: 2, duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("2") }]))

# `max_string_bytes` counts decoded UTF-8 bytes, keys included.
expect ["\"abc\"", "\"\\u00e9a\"", "\"é\"", "{\"abc\":1}"].all(|text| JsonValue.parse_str(text, { max_string_bytes: 3 }).is_ok())
expect error_of("\"abcd\"", { max_string_bytes: 3 }) == Ok({ line: 1, column: 1, problem: LimitExceeded({ resource: StringBytes, max: 3, observed: AtLeast((3) + 1) }) })
expect error_of("[\"\\u00e9ab\"]", { max_string_bytes: 3 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: StringBytes, max: 3, observed: AtLeast((3) + 1) }) })
expect error_of("{\"abcd\":1}", { max_string_bytes: 3 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: StringBytes, max: 3, observed: AtLeast((3) + 1) }) })
expect problem_of("\"éé\"", { max_string_bytes: 3 }) == Ok(LimitExceeded({ resource: StringBytes, max: 3, observed: AtLeast((3) + 1) }))

# `max_number_digits` counts every character of a number.
expect ["123", "-12", "1.5", "0"].all(|text| JsonValue.parse_str(text, { max_number_digits: 3 }).is_ok())
expect error_of("1234", { max_number_digits: 3 }) == Ok({ line: 1, column: 1, problem: LimitExceeded({ resource: NumberCharacters, max: 3, observed: AtLeast((3) + 1) }) })
expect error_of("[1,-123]", { max_number_digits: 3 }) == Ok({ line: 1, column: 4, problem: LimitExceeded({ resource: NumberCharacters, max: 3, observed: AtLeast((3) + 1) }) })

# --- Duplicate keys.

# Keys are compared after unescaping, and the error points at the second key.
expect error_of("{\"a\":1,\"a\":2}", {}) == Ok({ line: 1, column: 8, problem: DuplicateKey("a") })
expect error_of("{\"a\":1,\"\\u0061\":2}", {}) == Ok({ line: 1, column: 8, problem: DuplicateKey("a") })
expect problem_of("{\"a\\\"b\":1,\"a\\\"b\":2}", {}) == Ok(DuplicateKey("a\"b"))
expect JsonValue.parse_str("{\"a\":{\"a\":1},\"b\":{\"a\":2}}", {}).is_ok()
expect error_of("[{\"a\":1},\n{\"b\":[],\"b\":{}}]", {}) == Ok({ line: 2, column: 9, problem: DuplicateKey("b") })

# CollapseIdentical keeps one copy of an equal repeat and refuses a different one.
expect JsonValue.parse_str("{\"a\":{\"x\":[1]},\"a\":{\"x\":[1]},\"a\":{\"x\":[1]}}", { duplicates: CollapseIdentical }) == Ok(Object([{ key: "a", value: Object([{ key: "x", value: Array([Number("1")]) }]) }]))
expect error_of("{\"a\":1,\"a\":2}", { duplicates: CollapseIdentical }) == Ok({ line: 1, column: 8, problem: ConflictingDuplicateKey("a") })
expect problem_of("{\"a\":1,\"a\":1.0}", { duplicates: CollapseIdentical }) == Ok(ConflictingDuplicateKey("a"))
expect problem_of("{\"a\":{\"x\":1,\"y\":2},\"a\":{\"y\":2,\"x\":1}}", { duplicates: CollapseIdentical }) == Ok(ConflictingDuplicateKey("a"))

# KeepFirst drops later members; KeepLast keeps the first position with the last value.
expect JsonValue.parse_str("{\"a\":1,\"b\":2,\"a\":3}", { duplicates: KeepFirst }) == Ok(Object([{ key: "a", value: Number("1") }, { key: "b", value: Number("2") }]))
expect JsonValue.parse_str("{\"a\":1,\"b\":2,\"a\":3,\"a\":4}", { duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("4") }, { key: "b", value: Number("2") }]))

# Objects with more than 16 members index their keys; both paths agree.
expect {
	members = "\"k0\":[],\"k1\":[],\"k2\":[],\"k3\":[],\"k4\":[],\"k5\":[],\"k6\":[],\"k7\":[],\"k8\":[],\"k9\":[],\"k10\":[],\"k11\":[],\"k12\":[],\"k13\":[],\"k14\":[],\"k15\":[]"
	column = Str.count_utf8_bytes("{${members},") + 1
	error_of("{${members},\"k3\":1}", {}) == Ok({ line: 1, column, problem: DuplicateKey("k3") })
		and error_of("{${members},\"k15\":1}", {}) == Ok({ line: 1, column, problem: DuplicateKey("k15") })
		and error_of("{${members},\"k16\":1,\"k3\":1}", {}) == Ok({ line: 1, column: column + 8, problem: DuplicateKey("k3") })
}
expect {
	members = Str.drop_last_bytes(Str.drop_first_bytes(many_keys(20), 1) ?? "", 1) ?? ""
	column = Str.count_utf8_bytes("{${members},") + 1
	text = "{${members},\"k3\":1}"
	keys = key_names(20)
	error_of(text, {}) == Ok({ line: 1, column, problem: DuplicateKey("k3") })
		and error_of(text, { duplicates: CollapseIdentical }) == Ok({ line: 1, column, problem: ConflictingDuplicateKey("k3") })
		and JsonValue.parse_str("{${members},\"k3\":[]}", { duplicates: CollapseIdentical }).map_ok(JsonValue.keys) == Ok(keys)
		and JsonValue.parse_str(text, { duplicates: KeepFirst }).map_ok(|value| (JsonValue.keys(value), JsonValue.get(value, "k3"))) == Ok((keys, Ok(Array([]))))
		and JsonValue.parse_str(text, { duplicates: KeepLast }).map_ok(|value| (JsonValue.keys(value), JsonValue.get(value, "k3"))) == Ok((keys, Ok(Number("1"))))
		and JsonValue.parse_str(many_keys(20), {}).map_ok(JsonValue.keys) == Ok(keys)
}

# --- Relaxations.

# Comments may appear wherever whitespace may.
expect JsonValue.parse_str("// c\n[1, /* x */ 2] // end", { comments: True }) == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.parse_str("{\"a\" /* c */ : /* d */ 1 /**/, \"b\"//\r:2}", { comments: True }) == Ok(Object([{ key: "a", value: Number("1") }, { key: "b", value: Number("2") }]))
expect ["/**/1", "/* a * b **/1", "1 // no newline at the end", "1\r\n// one\r\n"].all(|text| JsonValue.parse_str(text, { comments: True }) == Ok(Number("1")))
expect JsonValue.parse_str("[1] // end", { comments: True }) == Ok(Array([Number("1")])) and (JsonValue.parser({ comments: True })).parse_str("[1] // end") == Ok(Array([Number("1")]))
expect error_of("/* only */", { comments: True }) == Ok({ line: 1, column: 11, problem: ExpectedValue })
expect error_of("[1 / 2]", { comments: True }) == Ok({ line: 1, column: 4, problem: ExpectedArrayDelimiter })

# Without the option, a comment is refused at its slash.
expect error_of("// c\n1", {}) == Ok({ line: 1, column: 1, problem: CommentsNotAllowed })
expect error_of("[1 /* x */]", {}) == Ok({ line: 1, column: 4, problem: CommentsNotAllowed })
expect error_of("1 /", {}) == Ok({ line: 1, column: 3, problem: CommentsNotAllowed })

# An unterminated block comment is reported where it opens.
expect error_of("[1 /* x", { comments: True }) == Ok({ line: 1, column: 4, problem: UnterminatedComment })
expect error_of("/* */ /*/", { comments: True }) == Ok({ line: 1, column: 7, problem: UnterminatedComment })

# Trailing commas.
expect JsonValue.parse_str("[1,2,]", { trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.parse_str("{\"a\":[1,],}", { trailing_commas: True }) == Ok(Object([{ key: "a", value: Array([Number("1")]) }]))
expect error_of("[,]", { trailing_commas: True }) == Ok({ line: 1, column: 2, problem: ExpectedValue })
expect error_of("[1,,]", { trailing_commas: True }) == Ok({ line: 1, column: 4, problem: ExpectedValue })
expect error_of("{,}", { trailing_commas: True }) == Ok({ line: 1, column: 2, problem: ExpectedStringKey })
expect error_of("1,", { trailing_commas: True }) == Ok({ line: 1, column: 2, problem: UnexpectedContent })
expect JsonValue.parse_str("[1, // one\n/* end */]", { comments: True, trailing_commas: True }) == Ok(Array([Number("1")]))

# A byte order mark is skipped only at the start, when allowed.
expect JsonValue.parse_str("\u(FEFF)[1]", { bom: True }) == Ok(Array([Number("1")]))
expect error_of("\u(FEFF)[1]", {}) == Ok({ line: 1, column: 1, problem: BomNotAllowed })
expect error_of("[\u(FEFF)1]", { bom: True }) == Ok({ line: 1, column: 2, problem: ExpectedValue })
expect error_of("\u(FEFF)", { bom: True }) == Ok({ line: 1, column: 4, problem: ExpectedValue })
expect JsonValue.parse_str("\u(FEFF){}", { bom: True, top_level: Object }) == Ok(Object([]))
expect error_of("\u(FEFF)[]", { bom: True, top_level: Object }) == Ok({ line: 1, column: 4, problem: TopLevelRequired(Object) })
expect (JsonValue.parser({ bom: True })).parse_str("\u(FEFF) [1]") == Ok(Array([Number("1")]))
expect (JsonValue.parser({})).parse_str("\u(FEFF)[1]") == Err(ParseError(Diagnostic.{ problem: BomNotAllowed, location: Text({ offset: 0, line: 1, column: 1 }), path: [] }))

# NaN and Infinity, as number text.
expect JsonValue.parse_str("[NaN, Infinity, -Infinity]", { non_finite: True }) == Ok(Array([Number("NaN"), Number("Infinity"), Number("-Infinity")]))
expect error_of("NaN", {}) == Ok({ line: 1, column: 1, problem: ExpectedValue })
expect error_of("[Infinity]", {}) == Ok({ line: 1, column: 2, problem: ExpectedValue })
expect error_of("-Infinity", {}) == Ok({ line: 1, column: 1, problem: ExpectedMinusDigit })
expect error_of("Nan", { non_finite: True }) == Ok({ line: 1, column: 1, problem: ExpectedValue })
expect error_of("-NaN", { non_finite: True }) == Ok({ line: 1, column: 1, problem: ExpectedMinusDigit })
expect error_of("[-Inf]", { non_finite: True }) == Ok({ line: 1, column: 2, problem: ExpectedMinusDigit })
expect error_of("Infinityx", { non_finite: True }) == Ok({ line: 1, column: 9, problem: UnexpectedContent })
expect JsonValue.parse_str("Infinity", { non_finite: True, max_number_digits: 8 }).is_ok() and problem_of("-Infinity", { non_finite: True, max_number_digits: 8 }) == Ok(LimitExceeded({ resource: NumberCharacters, max: 8, observed: AtLeast((8) + 1) }))
expect JsonValue.parse_str("NaN", { non_finite: True, max_number_digits: 3 }).is_ok() and error_of("[NaN]", { non_finite: True, max_number_digits: 2 }) == Ok({ line: 1, column: 2, problem: LimitExceeded({ resource: NumberCharacters, max: 2, observed: AtLeast((2) + 1) }) })
expect problem_of("Infinity", { non_finite: True, max_number_digits: 7 }) == Ok(LimitExceeded({ resource: NumberCharacters, max: 7, observed: AtLeast((7) + 1) }))

# Raw control characters inside strings and keys.
expect JsonValue.parse_str("{\"a\tb\":\"\u(0)\n\u(1F)\"}", { control_characters: True }) == Ok(Object([{ key: "a\tb", value: String("\u(0)\n\u(1F)") }]))

# Lone surrogates may become U+FFFD; pairs still combine.
expect JsonValue.parse_str("[\"\\ud800\", \"\\ud800\\u0041\", \"\\udc00x\", \"\\udc00\\ud800\", \"\\ud83d\\ude00\"]", { lone_surrogates: Replace }) == Ok(Array([String("\u(FFFD)"), String("\u(FFFD)A"), String("\u(FFFD)x"), String("\u(FFFD)\u(FFFD)"), String("😀")]))

# Strict input reads the same under every set of relaxations.
expect {
	documents = [
		"{\"a\":[1,-2.5e3,true,null],\"b\":{\"c\":\"\\u00e9\\n\\ud83d\\ude00\"}}",
		" [ ] ",
		"0",
		"\"x\"",
		"[{\"a\":{}}, [[]], \"\"]",
	]
	relaxed : List(JsonValue.Props)
	relaxed = [
		{ comments: True },
		{ trailing_commas: True },
		{ bom: True },
		{ non_finite: True },
		{ control_characters: True },
		{ lone_surrogates: Replace },
		{ comments: True, trailing_commas: True, bom: True, non_finite: True, control_characters: True, lone_surrogates: Replace },
	]
	documents.all(|text| relaxed.all(|options| JsonValue.parse_str(text, options) == JsonValue.parse_str(text, {}) and JsonValue.parse_str(text, {}).is_ok()))
}

# --- Checks.

# The top-level shape.
expect JsonValue.parse_str("{}", { top_level: Object }).is_ok() and JsonValue.parse_str("[]", { top_level: ObjectOrArray }).is_ok() and JsonValue.parse_str("{}", { top_level: ObjectOrArray }).is_ok()
expect error_of("[]", { top_level: Object }) == Ok({ line: 1, column: 1, problem: TopLevelRequired(Object) })
expect error_of(" null", { top_level: Object }) == Ok({ line: 1, column: 2, problem: TopLevelRequired(Object) })
expect error_of("\"s\"", { top_level: ObjectOrArray }) == Ok({ line: 1, column: 1, problem: TopLevelRequired(ObjectOrArray) })
expect error_of("NaN", { top_level: ObjectOrArray, non_finite: True }) == Ok({ line: 1, column: 1, problem: TopLevelRequired(ObjectOrArray) })
expect (JsonValue.parser({ top_level: Object })).parse_str(" [1]") == Err(ParseError(Diagnostic.{ problem: TopLevelRequired(Object), location: Text({ offset: 1, line: 1, column: 2 }), path: [] }))
expect (JsonValue.parser({ top_level: ObjectOrArray })).parse_str("1") == Err(ParseError(Diagnostic.{ problem: TopLevelRequired(ObjectOrArray), location: Text({ offset: 0, line: 1, column: 1 }), path: [] }))
expect (JsonValue.parser({ top_level: Object, bom: True })).parse_str("\u(FEFF){}") == Ok(Object([]))
expect error_of("@", { top_level: Object }) == Ok({ line: 1, column: 1, problem: ExpectedValue })

# I-JSON numbers: integers within ±(2^53−1), at most 17 significant digits,
# and no overflow or underflow.
expect
	["9007199254740991", "-9007199254740991", "0", "-0", "0.1", "1.2345678901234567", "0.000123456789012345670000", "1e308", "5e-324", "0e-400", "1E2", "-0.0"]
		.all(|text| JsonValue.parse_str(text, { interoperable_numbers: True }) == Ok(Number(text)))
expect
	["9007199254740992", "-9007199254740992", Str.repeat("9", 400), "1.23456789012345678", "1e400", "-1e400", "1e-400", "1.7976931348623159e308"]
		.all(
			|text| match problem_of(text, { interoperable_numbers: True }) {
				Ok(NonInteroperableNumber(detail)) => detail.text == text
				_ => False
			},
		)
expect error_of("[9007199254740992]", { interoperable_numbers: True }) == Ok({ line: 1, column: 2, problem: NonInteroperableNumber({ text: "9007199254740992", reason: UnsafeInteger }) })
expect problem_of("1.23456789012345678", { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: "1.23456789012345678", reason: SignificantDigits }))
expect problem_of("1e400", { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: "1e400", reason: DoubleOverflow }))
expect problem_of("1e-400", { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: "1e-400", reason: DoubleUnderflow }))
expect problem_of("-Infinity", { interoperable_numbers: True, non_finite: True }) == Ok(NonInteroperableNumber({ text: "-Infinity", reason: NotFinite }))
expect JsonValue.parse_str("[NaN]", { non_finite: True }).is_ok() and problem_of("[NaN]", { non_finite: True, interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: "NaN", reason: NotFinite }))

# Number diagnostics preserve the complete lexeme and a matchable reason.
expect
	[
		{ text: "9007199254740992", reason: UnsafeInteger },
		{ text: "1.23456789012345678", reason: SignificantDigits },
		{ text: "1e400", reason: DoubleOverflow },
		{ text: "1e-400", reason: DoubleUnderflow },
		{ text: "-Infinity", reason: NotFinite },
	].all(|detail| error_of(detail.text, { interoperable_numbers: True, non_finite: True }) == Ok({ line: 1, column: 1, problem: NonInteroperableNumber(detail) }))

# --- Accessors.

expect JsonValue.get(Object([{ key: "a", value: Number("1") }, { key: "a", value: Number("2") }]), "a") == Ok(Number("1"))
expect JsonValue.get(Object([]), "a") == Err(Missing) and JsonValue.get(Array([Null]), "0") == Err(Missing)
expect JsonValue.at(Array([Null]), 1) == Err(Missing) and JsonValue.at(Object([{ key: "0", value: Null }]), 0) == Err(Missing)
expect {
	match JsonValue.parse_str("{\"a\":[10,{\"b\":[true]}],\"01\":null}", {}) {
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

expect JsonValue.parse_str(many_keys(20000), {}).map_ok(|value| JsonValue.keys(value).len()) == Ok(20000)
expect JsonValue.parse_str("[${Str.join_with(List.repeat("1", 100000), ",")} ]", {}).map_ok(|value| JsonValue.as_list(value).map_ok(List.len)) == Ok(Ok(100000))
expect JsonValue.parse_str(nested(10000, "[]"), { max_depth: 10001 }).is_ok()
expect JsonValue.parse_str("\"${Str.repeat("\\n", 50000)}\"", {}).map_ok(|value| JsonValue.as_str(value).map_ok(Str.count_utf8_bytes)) == Ok(Ok(50000))

# --- Number accessors.

# Only JSON integers convert to I64 and U64; `-0` is zero.
expect JsonValue.as_i64(Number("9223372036854775807")) == Ok(9223372036854775807) and JsonValue.as_i64(Number("-9223372036854775808")) == Ok(-9223372036854775808)
expect JsonValue.as_i64(Number("9223372036854775808")) == Err(OutOfRange) and JsonValue.as_i64(Number("-9223372036854775809")) == Err(OutOfRange)
expect JsonValue.as_i64(Number("-0")) == Ok(0) and JsonValue.as_u64(Number("-0")) == Ok(0)
expect ["1.0", "1e3", "0x10", "+1", "1_0", "", " 1", "01", "-", "NaN", "Infinity"].all(|text| JsonValue.as_i64(Number(text)) == Err(WrongType) and JsonValue.as_u64(Number(text)) == Err(WrongType))
expect JsonValue.as_i64(String("1")) == Err(WrongType) and JsonValue.as_u64(Null) == Err(WrongType)
expect JsonValue.as_u64(Number("18446744073709551615")) == Ok(18446744073709551615) and JsonValue.as_u64(Number("18446744073709551616")) == Err(OutOfRange)
expect JsonValue.as_u64(Number("-1")) == Err(OutOfRange) and JsonValue.as_u64(Number("0")) == Ok(0)

# Any JSON number converts to Dec when it fits; NaN and Infinity never do.
expect JsonValue.as_dec(Number("1e3")) == Ok(1000.0) and JsonValue.as_dec(Number("1E3")) == Ok(1000.0) and JsonValue.as_dec(Number("-1.25e+2")) == Ok(-125.0)
expect JsonValue.as_dec(Number("0.1")) == Ok(0.1) and JsonValue.as_dec(Number("-0")) == Ok(0.0) and JsonValue.as_dec(Number("12")) == Ok(12.0)
expect ["NaN", "Infinity", "-Infinity", "1e400", "0.0000000000000000001", "1000000000000000000000"].all(|text| JsonValue.as_dec(Number(text)) == Err(OutOfRange))
expect ["0x10", "+1", "1_0", "", ".5", "1.", "nan"].all(|text| JsonValue.as_dec(Number(text)) == Err(WrongType))
expect JsonValue.as_dec(Number("1.50000000000000000000")) == Ok(1.5) and JsonValue.as_dec(Number("0.000000000000000001")) == Ok(0.000000000000000001)
expect JsonValue.as_dec(String("1")) == Err(WrongType)

# Huge lexemes are handled without overflow, by the accessors and the I-JSON check.
expect {
	digits = Str.repeat("7", 100000)
	tiny = "0.${Str.repeat("0", 100000)}1"
	JsonValue.as_i64(Number(digits)) == Err(OutOfRange)
		and JsonValue.as_u64(Number("-${digits}")) == Err(OutOfRange)
		and JsonValue.as_dec(Number(digits)) == Err(OutOfRange)
		and JsonValue.as_dec(Number(tiny)) == Err(OutOfRange)
		and JsonValue.as_dec(Number("1e999999999999999999999")) == Err(OutOfRange)
		and problem_of(digits, { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: digits, reason: UnsafeInteger }))
		and problem_of(tiny, { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: tiny, reason: DoubleUnderflow }))
		and problem_of("1e999999999999999999999", { interoperable_numbers: True }) == Ok(NonInteroperableNumber({ text: "1e999999999999999999999", reason: DoubleOverflow }))
		and JsonValue.parse_str(digits, {}) == Ok(Number(digits))
}

# --- Writing.

expect write_text(Object([{ key: "a", value: Number("1") }, { key: "b", value: Array([Bool(True), Null]) }]), {}) == Ok("{\"a\":1,\"b\":[true,null]}")
expect write_text(Array([Bool(False), String(""), Object([]), Array([])]), {}) == Ok("[false,\"\",{},[]]")

# Indented output matches Python's `json.dumps(v, indent=2, ensure_ascii=False)`.
expect {
	value = Object([{ key: "a", value: Array([]) }, { key: "b", value: Object([{ key: "c", value: Array([Number("1"), Number("2")]) }]) }, { key: "d", value: Object([]) }])
	write_text(value, { indent: 2 }) == Ok("{\n  \"a\": [],\n  \"b\": {\n    \"c\": [\n      1,\n      2\n    ]\n  },\n  \"d\": {}\n}")
		and write_text(Number("1"), { indent: 2 }) == Ok("1")
		and write_text(Array([Array([Null])]), { indent: 1 }) == Ok("[\n [\n  null\n ]\n]")
}

# Sorted keys follow UTF-8 byte order, which is code point order.
expect {
	value = Object([{ key: "B", value: Number("1") }, { key: "a", value: Number("2") }, { key: "aa", value: Number("3") }, { key: "b", value: Number("4") }, { key: "é", value: Number("5") }, { key: "ab", value: Number("6") }])
	write_text(value, { sort_keys: True }) == Ok("{\"B\":1,\"a\":2,\"aa\":3,\"ab\":6,\"b\":4,\"é\":5}")
}

# Only the quote, the backslash and control characters are escaped.
expect write_text(String("\"\\\u(8)\u(C)\n\r\t\u(1)\u(1F)\u(7F)é😀/"), {}) == Ok("\"\\\"\\\\\\b\\f\\n\\r\\t\\u0001\\u001f\u(7F)é😀/\"")
expect write_text(Object([{ key: "a\"\n", value: Null }]), {}) == Ok("{\"a\\\"\\n\":null}")

# Number text is written as it is when it is a JSON number, and refused otherwise.
expect ["-0", "1E+2", "0.000", "-1.5e-7", "340282366920938463463374607431768211455"].all(|text| write_text(Number(text), {}) == Ok(text))
expect ["Infinity", "-Infinity", "+1", "0x10", "1_0", " 1", "1 ", "-", ".5"].all(|text| write_text(Array([Number(text)]), {}) == Err(InvalidNumber(text)))

# A repeated key is refused in small and large objects, at any depth.
expect write_text(Array([Object([{ key: "a", value: Null }, { key: "b", value: Null }, { key: "a", value: Null }])]), {}) == Err(DuplicateKey("a"))
expect {
	members = key_names(20).map(|key| { key, value: Null })
	write_text(Object(members.append({ key: "k3", value: Null })), {}) == Err(DuplicateKey("k3"))
		and write_text(Object(members.append({ key: "k3", value: Null })), { sort_keys: True }) == Err(DuplicateKey("k3"))
		and write_text(Object(members), {}).is_ok()
}

# 128 levels of nesting are written, and 129 are refused.
expect write_text(nested_value(128), {}) == Ok(nested(128, "null")) and write_text(nested_value(129), {}) == Err(TooDeep)
expect write_text(Object([{ key: "a", value: nested_value(128) }]), {}) == Err(TooDeep)

# Writing a long array and a large object is linear too.
expect {
	text = "[${Str.join_with(List.repeat("1", 100000), ",")}]"
	JsonValue.parse_str(text, {}).map_ok(|value| write_text(value, {})) == Ok(Ok(text))
		and JsonValue.parse_str(many_keys(20000), {}).map_ok(|value| write_text(value, { sort_keys: True }).is_ok()) == Ok(True)
}

# What is written reads back as the same value, compact or indented.
expect {
	text = "{\"a\":[1,-0,1.5E+3,\"\\u0000\\u001f\\\"\\\\é😀\",true,false,null,{}],\"b\":{\"c\":[[],[{}]],\"\":\"\"}}"
	match JsonValue.parse_str(text, {}) {
		Ok(value) => {
			pretty = write_text(value, { indent: 3, sort_keys: True })
			write_text(value, {}).map_ok(|rendered| JsonValue.parse_str(rendered, {})) == Ok(Ok(value))
				and write_text(value, { indent: 2 }).map_ok(|rendered| JsonValue.parse_str(rendered, {})) == Ok(Ok(value))
				and pretty.map_ok(|out| JsonValue.parse_str(out, {}).map_ok(|again| write_text(again, { indent: 3, sort_keys: True }))) == Ok(Ok(pretty))
		}

		Err(_) => False
	}
}

# --- Doc examples: reading.

expect {
	match JsonValue.parse_str("{\"@context\": \"https://spdx.org/rdf/3.0.1/spdx-context.jsonld\", \"@graph\": []}", {}) {
		Ok(doc) => doc.keys() == ["@context", "@graph"]
		Err(_) => False
	}
}
expect JsonValue.parse_str("[1, 2,] // done", { comments: True, trailing_commas: True }) == Ok(Array([Number("1"), Number("2")]))
expect JsonValue.validate({ source: "{\"a\": [1,]}", name: "list.json" }) == Err(InvalidJsonValue(Diagnostic.{ problem: TrailingComma, location: Text({ offset: 8, line: 1, column: 9 }), path: [Source("list.json")] }))
expect JsonValue.validate({ source: "{\"a\": [1,]}", options: { trailing_commas: True } }).map_ok(|doc| doc.get_path(["a", "0"])) == Ok(Ok(Number("1")))
expect {
	settings : JsonValue
	settings = "{\"port\": 8080}"
	settings.get("port") == Ok(Number("8080"))
}
expect JsonValue.from_quote("{\"a\": 1, \"a\": 1}") == Err(BadQuotedBytes("<literal>:1:10: JSON: DuplicateKey(\"a\")"))
expect JsonValue.parse_str("{\"a\": [true, null]}", {}) == Ok(Object([{ key: "a", value: Array([Bool(True), Null]) }]))
expect JsonValue.parse_str("[1,]", {}) == Err(InvalidJsonValue(Diagnostic.{ location: Text({ offset: 3 - 1, line: 1, column: 3 }), path: [], problem: TrailingComma }))
expect JsonValue.parse_str("{\"a\": 1, \"a\": 2}", { duplicates: KeepLast }) == Ok(Object([{ key: "a", value: Number("2") }]))
expect (JsonValue.parser({})).run_str("[1] rest").map_ok(|{ rest, .. }| rest) == Ok("rest")
expect (JsonValue.parser({})).parse_str("[1 2]") == Err(ParseError(Diagnostic.{ problem: ExpectedArrayDelimiter, location: Text({ offset: 3, line: 1, column: 4 }), path: [] }))
expect (JsonValue.parser({ comments: True })).parse_str("1 // one") == Ok(Number("1"))
expect JsonValue.parse_str("{\"a\": 1}", {}).map_ok(|doc| doc.get("a")) == Ok(Ok(Number("1")))
expect JsonValue.parse_str("[10, 20]", {}).map_ok(|doc| doc.at(1)) == Ok(Ok(Number("20")))
expect JsonValue.parse_str("{\"a\": [{\"b\": true}]}", {}).map_ok(|doc| doc.get_path(["a", "0", "b"])) == Ok(Ok(Bool(True)))
expect JsonValue.parse_str("{\"b\": 1, \"a\": 2}", {}).map_ok(|doc| doc.keys()) == Ok(["b", "a"])
expect JsonValue.parse_str("\"caf\\u00e9\"", {}).map_ok(|doc| doc.as_str()) == Ok(Ok("café"))
expect JsonValue.parse_str("false", {}).map_ok(|doc| doc.as_bool()) == Ok(Ok(False))
expect JsonValue.parse_str("[null]", {}).map_ok(|doc| doc.as_list()) == Ok(Ok([Null]))
expect JsonValue.parse_str("{\"a\": null}", {}).map_ok(|doc| doc.as_object()) == Ok(Ok([{ key: "a", value: Null }]))

# --- Doc examples: writing and numbers.

expect JsonValue.parse_str("-42", {}).map_ok(|doc| doc.as_i64()) == Ok(Ok(-42))
expect JsonValue.parse_str("1e3", {}).map_ok(|doc| doc.as_i64()) == Ok(Err(WrongType))
expect JsonValue.parse_str("18446744073709551615", {}).map_ok(|doc| doc.as_u64()) == Ok(Ok(18446744073709551615))
expect JsonValue.parse_str("-1", {}).map_ok(|doc| doc.as_u64()) == Ok(Err(OutOfRange))
expect JsonValue.parse_str("-1.25e+2", {}).map_ok(|doc| doc.as_dec()) == Ok(Ok(-125.0))
expect write_text(Object([{ key: "b", value: Null }, { key: "a", value: Array([]) }]), { sort_keys: True, indent: 2 }) == Ok("{\n  \"a\": [],\n  \"b\": null\n}")
expect write_text(Object([{ key: "a", value: Number("1") }, { key: "b", value: Array([Bool(True), Null]) }]), {}) == Ok("{\"a\":1,\"b\":[true,null]}")
expect write_text(Number("NaN"), {}) == Err(InvalidNumber("NaN"))
expect write_text(Array([Number("1"), String("two")]), { indent: 4 }) == Ok("[\n    1,\n    \"two\"\n]")

# Bounded documents retain strict grammar, caller rules and exact number text.
expect JsonValue.parse_str("null", { max_bytes: 4 }) == Ok(Null)
expect JsonValue.parse_str("null ", { max_bytes: 4 }) == Err(InvalidJsonValue(Diagnostic.{ location: Text({ offset: 5 - 1, line: 1, column: 5 }), path: [], problem: LimitExceeded({ resource: DocumentBytes, max: 4, observed: AtLeast((4) + 1) }) }))
expect JsonValue.parse_str("\"é\"", { max_bytes: 4 }) == Ok(String("é"))
expect JsonValue.parse_str("\"é\"", { max_bytes: 3 }).is_err()
expect JsonValue.parse_str("\u(FEFF)null", { bom: True, max_bytes: 7 }) == Ok(Null)
expect JsonValue.parse_str("\u(FEFF)null", { bom: True, max_bytes: 6 }).is_err()
expect JsonValue.parse_str("{\"x\":0,\"x\":0}", { duplicates: CollapseIdentical, max_bytes: 13 }) == Ok(Object([{ key: "x", value: Number("0") }]))
expect JsonValue.parse_str("{\"x\":0,\"x\":1}", { duplicates: CollapseIdentical, max_bytes: 100 }).is_err()
expect JsonValue.parse_str("[0,0]", { max_items: 1, max_bytes: 100 }).is_err()
expect JsonValue.parse_str("[[0]]", { max_depth: 1, max_bytes: 100 }).is_err()
expect JsonValue.parse_str("\"é\"", { max_string_bytes: 1, max_bytes: 100 }).is_err()
expect JsonValue.parse_str("22", { max_number_digits: 1, max_bytes: 100 }).is_err()
expect JsonValue.parse_str("null true", { max_bytes: 100 }).is_err()
expect JsonValue.parse_str("null", { max_bytes: 0 }).is_err()
expect JsonValue.parse_str("", { max_bytes: 0 }).is_err()
expect JsonValue.parse_str("9007199254740993123456789", { max_bytes: 25 }) == Ok(Number("9007199254740993123456789"))

# Exact output size includes escaping and formatting, with no final newline.
expect write_text(String("\n"), { max_bytes: 4 }) == Ok("\"\\n\"")
expect write_text(String("\n"), { max_bytes: 3 }) == Err(TooLarge(3))
expect write_text(String("\u(01)"), { max_bytes: 8 }) == Ok("\"\\u0001\"")
expect write_text(String("\u(01)"), { max_bytes: 7 }) == Err(TooLarge(7))
expect write_text(Object([{ key: "\u(01)", value: Null }]), { max_bytes: 15 }) == Ok("{\"\\u0001\":null}")
expect write_text(Object([{ key: "\u(01)", value: Null }]), { max_bytes: 14 }) == Err(TooLarge(14))
expect write_text(Array([]), { indent: U64.highest, max_bytes: 2 }) == Ok("[]")
expect write_text(Null, { indent: U64.highest, max_bytes: 4 }) == Ok("null")
expect write_text(Array([Null]), { indent: U64.highest, max_bytes: U64.highest }) == Err(TooLarge(U64.highest))
expect write_text(Object([{ key: "x", value: Null }]), { indent: U64.highest, max_bytes: U64.highest }) == Err(TooLarge(U64.highest))
expect write_text(Null, { max_bytes: 0 }) == Err(TooLarge(0))
expect write_text(Number("NaN"), { max_bytes: 100 }) == Err(InvalidNumber("NaN"))
expect write_text(Object([{ key: "a", value: Null }, { key: "a", value: Null }]), { max_bytes: 100 }) == Err(DuplicateKey("a"))
expect measure_add(U64.highest, 0, U64.highest) == Ok(U64.highest)
expect measure_add(U64.highest, 1, U64.highest) == Err(TooLarge(U64.highest))
expect measure_line(0, U64.highest, 0, U64.highest) == Ok(1)
expect {
	var $tree = Null
	for _ in List.repeat({}, 128) {
		$tree = Array([$tree])
	}
	exact = write_text($tree, { max_bytes: 260 })
	exact.map_ok(|text| text.count_utf8_bytes()) == Ok(260)
		and write_text(Array([$tree]), { max_bytes: 1000 }) == Err(TooDeep)
}
expect {
	values = [Null, Bool(False), Bool(True), Number("-0.001e+123"), String("café\n\t\\\"\u(00)"), Array([]), Object([]), Array([Null, Bool(True)]), Object([{ key: "é\n", value: Array([Number("9007199254740993123456789")]) }])]
	formats : List(JsonValue.WriteProps)
	formats = [{}, { indent: 1 }, { indent: 3, sort_keys: True }]
	values.all(
		|value| {
			formats.all(
				|options| {
					match write_text(value, options) {
						Ok(text) => write_text(value, { ..options, max_bytes: text.count_utf8_bytes() }) == Ok(text)
							and write_text(value, { ..options, max_bytes: text.count_utf8_bytes() - 1 }).is_err()
						Err(_) => False
					}
				},
			)
		},
	)
}

# The same input byte limit applies to combinators and compile-time validation.
expect (JsonValue.parser({ max_bytes: 4 })).parse_str("null") == Ok(Null)
expect (JsonValue.parser({ max_bytes: 4 })).run_str("null next") == Err(ParseError(Diagnostic.{ problem: LimitExceeded({ resource: DocumentBytes, max: 4, observed: AtLeast((4) + 1) }), location: Text({ offset: 4, line: 1, column: 5 }), path: [] }))
expect (JsonValue.parser({ max_bytes: 9 })).run_str("null next").map_ok(|{ rest, .. }| rest) == Ok("next")
expect JsonValue.validate({ source: "null", options: { max_bytes: 4 } }) == Ok(Null)
expect JsonValue.validate({ source: "null ", options: { max_bytes: 4 } }) == Err(InvalidJsonValue(Diagnostic.{ problem: LimitExceeded({ resource: DocumentBytes, max: 4, observed: AtLeast(5) }), location: Text({ offset: 4, line: 1, column: 5 }), path: [Source("<literal>")] }))
expect write_text(Array([Null]), { indent: U64.highest }) == Err(TooLarge(U64.highest))

# The specialized vector stop mask agrees with the generic JSON byte class at
# every byte value, including controls, escapes and high UTF-8 bytes, across
# vector loads and scalar tails.
expect {
	var $same = True
	for prefix in (0.U64).to(32) {
		for stop in (0.U16).to(255) {
			bytes = List.repeat('a', prefix).append(stop.to_u8_wrap()).concat(List.repeat('b', 32)).append('"')
			$same = $same and plain_string_end(bytes, 0) == Parser.Utf8.skip_class(bytes, 0, plain_string_class)
			$same = $same and plain_string_end(bytes, U64.min(3, bytes.len())) == Parser.Utf8.skip_class(bytes, U64.min(3, bytes.len()), plain_string_class)
		}
	}
	$same
}

# Reused buffers must start every independent document logically empty, and
# changing options or recovering after failure must not expose previous values.
expect {
	options : List(JsonValue.Props)
	options = [{}, { comments: True, trailing_commas: True, bom: True, non_finite: True, lone_surrogates: Replace }, { max_depth: 0 }, { max_items: 1 }, { max_string_bytes: 2 }, { max_number_digits: 2 }, { max_bytes: 8 }, { duplicates: KeepFirst }, { duplicates: KeepLast }, { duplicates: CollapseIdentical }]
	var $same = True
	for props in options {
		var $workspace = JsonWorkspace.empty()
		for input in ["{\"name\":\"longer source\",\"items\":[1,{\"x\":true},3]}", "[1,2,", "{\"x\":1,\"x\":2}", "{\"x\":1,\"x\":1}", "{\"x\":\"é😀\"}", "/*c*/[1,]", "\u(FEFF)null", "NaN", "\"\\ud800\"", "1 extra", "", "{}", "false", "[4]"] {
			parsed = parse_reusing(input, props, $workspace)
			$workspace = parsed.workspace
			$same = $same and parsed.result == JsonValue.parse_str(input, props)
			$same = $same and $workspace.open.is_empty() and $workspace.items.is_empty() and $workspace.members.is_empty() and $workspace.key_ats.is_empty()
		}
	}
	$same
}

expect {
	first = parse_reusing("{\"old\":[1,2,{\"name\":\"previous\"}]}", {}, JsonWorkspace.empty())
	left = parse_reusing("{\"left\":[3,4]}", {}, first.workspace)
	right = parse_reusing("{\"right\":[5,{\"name\":\"next\"}]}", {}, first.workspace)
	first.result == JsonValue.parse_str("{\"old\":[1,2,{\"name\":\"previous\"}]}", {}) and left.result == JsonValue.parse_str("{\"left\":[3,4]}", {}) and right.result == JsonValue.parse_str("{\"right\":[5,{\"name\":\"next\"}]}", {})
}

parse_reusing : Str, JsonValue.Props, JsonWorkspace(JsonValue) -> { result : Try(JsonValue, [InvalidJsonValue(JsonValue.Error)]), workspace : JsonWorkspace(JsonValue) }
parse_reusing = |input, options, workspace| match scan_reusing(input, input.to_utf8(), options, workspace) {
	Ok(parsed) => {
		result = if (parsed.pos == input.count_utf8_bytes()) Ok(parsed.value) else Err(InvalidJsonValue(locate(input, parsed.pos, UnexpectedContent)))
		{ result, workspace: parsed.workspace }
	}
	Err(JsonFail(failure)) => { result: Err(InvalidJsonValue(locate(input, failure.offset, failure.problem))), workspace: JsonWorkspace.empty() }
}

encoding_error : JsonConfig.WriteError -> [InvalidJsonValue(JsonValue.Error)]
encoding_error = |problem| {
	detail = match problem {
		InvalidNumber(text) => InvalidNumber(Diagnostic.excerpt(text.to_utf8()))
		DuplicateKey(key) => DuplicateKey(DiagnosticOwnership.own_text(key))
		TooDeep => LimitExceeded({ resource: Nesting, max: 128, observed: AtLeast((128) + 1) })
		TooLarge(max) => LimitExceeded({ resource: DocumentBytes, max: max, observed: AtLeast((max) + 1) })
	}
	InvalidJsonValue(Diagnostic.{ problem: detail, location: Unavailable(GeneratedValue), path: [] })
}

# Framed JSON records exercise the same public format-owned API.
expect (JsonValue.record_decoder({})).collect(["1\n{\"x\":".to_utf8(), "true}\n\n\"é\"".to_utf8()].iter()) == Ok([Number("1"), Object([{ key: "x", value: Bool(True) }]), String("é")])
expect (JsonValue.record_decoder({ framing: Array })).collect(["[1,{\"x\":".to_utf8(), "[true]},\"é\"]".to_utf8()].iter()) == Ok([Number("1"), Object([{ key: "x", value: Array([Bool(True)]) }]), String("é")])

# Every two-piece split and every byte boundary must preserve values and errors.
splits_agree : Str, JsonStream.Props -> Bool
splits_agree = |text, props| {
	bytes = text.to_utf8()
	want = List.from_iter((JsonValue.record_decoder(props)).read_iter([bytes].iter()))
	List.from_iter((0.U64).to(bytes.len() + 1)).all(|split| List.from_iter((JsonValue.record_decoder(props)).read_iter([[], bytes.take_first(split), [], bytes.drop_first(split), []].iter())) == want)
		and List.from_iter((JsonValue.record_decoder(props)).read_iter(bytes.map(|byte| [byte]).iter())) == want
}

expect {
	text = "\u(FEFF) /* wrapper ],{ */ [ /* item */ {\"é\":\"\\ud83d\\ude00\",\"q\":\"ends\\\\\"}, -1.2e+30, true /* ], */ , ] // final"
	props : JsonStream.Props
	props = { framing: Array, json: { bom: True, comments: True, trailing_commas: True } }
	splits_agree(text, props) and (JsonValue.record_decoder(props)).collect([text.to_utf8()].iter()) == Ok([Object([{ key: "é", value: String("😀") }, { key: "q", value: String("ends\\") }]), Number("-1.2e+30"), Bool(True)])
}
expect ["", "\n\r\n \t\r\n", "null\n42\r\n", "\"é😀\"\n{\"a\":[1,2]}\n", "tru\nfalse\n", "\"unterminated\n1\n"].all(|text| splits_agree(text, {}))
expect ["[]", " [ ] \r\n", "[1,2]", "[1,]", "[,1]", "[1 2]", "[trueX]", "[\"a\\\"b\"]suffix", "[{\"a\":]", "[1", "[", "[] []", "[\"\\uD800\"]"].all(|text| splits_agree(text, { framing: Array }))
expect ["[1,]", "[1 2]", "[trueX]", "[1", "[{\"a\":]", "[]x", "null", "", "[/*unfinished]"].all(|text| (JsonValue.record_decoder({ framing: Array, json: { comments: True } })).collect([text.to_utf8()].iter()).is_err())
expect (JsonValue.record_decoder({ framing: Array })).collect(["[ 1 , 2 ]".to_utf8()].iter()) == Ok([Number("1"), Number("2")])
expect {
	props : Streaming.RecoveryProps
	props = { policy: ReportInvalidRecords }
	match (JsonValue.record_decoder({ errors: props })).collect_report(["1\nno\ntrue\n".to_utf8()].iter()) {
		Ok(report) => report.values == [Number("1"), Bool(True)] and report.errors.len() == 1 and report.errors.all(
			|issue| match issue {
				InvalidJsonValue(error) => error.path == [Record(2)] and match error.location {
					Text(at) => at.line == 2 and at.column > 0 and at.offset >= 2
					_ => False
				}
			},
		) and report.report == { emitted_items: 2, rejected_records: 1 }
		Err(_) => False
	}
}
expect {
	items = List.from_iter((JsonValue.record_decoder({ errors: { policy: SkipInvalidRecords, max_errors: 1 } })).read_iter(["bad\n1\nbad\n".to_utf8()].iter()))
	match items {
		[Ok(Value(Number("1"))), Err(RecoveryLimitExceeded(Diagnostic.{ problem: RejectedRecords({ max: 1, .. }), .. }))] => True
		_ => False
	}
}
expect {
	items = List.from_iter((JsonValue.record_decoder({ framing: Array, errors: { policy: ReportInvalidRecords } })).read_iter(["[bad,1]".to_utf8()].iter()))
	match items {
		[Err(InvalidJsonValue(_))] => True
		_ => False
	}
}
expect (JsonValue.record_decoder({ framing: Array, max_record_bytes: 3 })).collect(["[123]".to_utf8()].iter()) == Ok([Number("123")])
expect (JsonValue.record_decoder({ max_record_bytes: 3 })).collect(["123\n".to_utf8()].iter()) == Ok([Number("123")])
expect (JsonValue.record_decoder({ framing: Array, max_record_bytes: 3 })).collect(["[1234]".to_utf8()].iter()).is_err()
expect (JsonValue.record_decoder({ max_record_bytes: 3, errors: { policy: SkipInvalidRecords } })).collect(["1234\n".to_utf8()].iter()).is_err()
expect (JsonValue.record_decoder({ framing: Array, max_record_bytes: 0 })).collect(["[]".to_utf8()].iter()) == Ok([])
expect (JsonValue.record_decoder({ framing: Array, max_record_bytes: 0 })).collect(["[1]".to_utf8()].iter()).is_err()
expect (JsonValue.record_decoder({ framing: Array, json: { max_depth: 1, max_items: 0 } })).collect(["[[],{}]".to_utf8()].iter()) == Ok([Array([]), Object([])])
expect (JsonValue.record_decoder({ framing: Array, json: { max_depth: 0, max_items: 0 } })).collect(["[1,2]".to_utf8()].iter()) == Ok([Number("1"), Number("2")])
expect (JsonValue.record_decoder({ framing: Array, json: { max_depth: 1 } })).collect(["[[[]]]".to_utf8()].iter()).is_err()
expect {
	options : List(JsonValue.Props)
	options = [{ max_depth: 0 }, { max_items: 0 }, { max_string_bytes: 0 }, { max_number_digits: 0 }, { max_bytes: 0 }]
	inputs = ["[]", "[1]", "\"x\"", "1", "null"]
	List.from_iter(options.iter().with_index()).all(
		|pair| {
			text = inputs.get(pair.0) ?? ""
			items = List.from_iter((JsonValue.record_decoder({ json: pair.1, errors: { policy: ReportInvalidRecords } })).read_iter(["${text}\ntrue\n".to_utf8()].iter()))
			match items {
				[Err(InvalidJsonValue(_))] => True
				_ => False
			}
		},
	)
}
expect {
	bad = ["[/*".to_utf8().concat([0xFF]).concat("*/1]".to_utf8()), "[] //".to_utf8().concat([0xED, 0xA0, 0x80]), [0xF0.U8, 0x90], [0xC0.U8, 0x80], [0xF4.U8, 0x90, 0x80, 0x80]]
	bad.all(|bytes| (JsonValue.record_decoder({ framing: Array, json: { comments: True }, errors: { policy: SkipInvalidRecords } })).collect(bytes.map(|byte| [byte]).iter()).is_err())
}
expect {
	values = [Number("1"), Object([{ key: "é", value: String("\n") }]), Array([Bool(True), Null])]
	[Ndjson, Array].all(
		|format| {
			parts = List.from_iter((JsonValue.record_encoder({ framing: format, chunk_bytes: 1 })).write_iter(values.iter()))
			var $bytes = []
			for part in parts {
				match part {
					Ok(chunk) => {
						$bytes = $bytes.concat(chunk)
					}
					Err(_) => return False
				}
			}
			(JsonValue.record_decoder({ framing: format })).collect($bytes.map(|byte| [byte]).iter()) == Ok(values)
		},
	)
}
expect List.from_iter((JsonValue.record_encoder({ framing: Array, chunk_bytes: 1 })).write_iter([].iter())) == [Ok("[".to_utf8()), Ok("]".to_utf8())]
expect List.from_iter((JsonValue.record_encoder({ chunk_bytes: 1 })).write_iter([].iter())) == []
expect List.from_iter((JsonValue.record_encoder({ framing: Array, chunk_bytes: 1 })).write_iter([Number("NaN")].iter())) == [Err(InvalidJsonValue(Diagnostic.{ problem: InvalidNumber(Diagnostic.excerpt("NaN".to_utf8())), location: Unavailable(GeneratedValue), path: [] }))]
expect List.from_iter((JsonValue.record_encoder({ max_record_bytes: 2, chunk_bytes: 1 })).write_iter([String("x")].iter())) == [Err(InvalidJsonValue(Diagnostic.{ problem: LimitExceeded({ resource: DocumentBytes, max: 2, observed: AtLeast(3) }), location: Unavailable(GeneratedValue), path: [] }))]
expect List.from_iter((JsonValue.record_encoder({ chunk_bytes: 0 })).write_iter([Number("1")].iter())) == [Err(InvalidChunkSize(Diagnostic.{ problem: BelowMinimum({ minimum: 1, actual: 0 }), location: Unavailable(NotApplicable), path: [Key("chunk_bytes")] }))]
expect {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| if index == 0 {
			Ok(("[1,".to_utf8(), 1))
		} else {
			crash "read beyond the first requested value"
		},
	)
	match (JsonValue.record_decoder({ framing: Array })).read_iter(chunks).next() {
		One({ item: Ok(Value(Number("1"))), .. }) => True
		_ => False
	}
}

expect (JsonValue.record_decoder({ framing: Array, json: { bom: True } })).collect(["[\u(FEFF)1]".to_utf8()].iter()).is_err()

# The outer array is framing. Its depth and item count are not charged to
# the individual records' JsonValue options.
expect {
	var $equal = True
	for depth in (0.U64).to(4) {
		for items in (0.U64).to(4) {
			options : JsonValue.Props
			options = { max_depth: depth, max_items: items }
			for record in ["null", "1", "[]", "{}", "[1]", "[[1]]", "{\"a\":[1,2]}", "{\"a\":1,\"b\":2}"] {
				actual = (JsonValue.record_decoder({ framing: Array, json: options })).collect("[${record}]".to_utf8().map(|byte| [byte]).iter())
				match (JsonValue.parse_str(record, options), actual) {
					(Ok(want), Ok([found])) => {
						if want != found {
							$equal = False
						}
					}
					(Err(_), Err(_)) => {}
					_ => {
						$equal = False
					}
				}
			}
		}
	}
	$equal
}

# ReportInvalidRecords must not turn an invalid encoding into a skipped line.
expect {
	bytes = "1\n".to_utf8().concat([0xFF]).concat("\n2\n".to_utf8())
	match List.from_iter((JsonValue.record_decoder({ errors: { policy: ReportInvalidRecords } })).read_iter(bytes.map(|byte| [byte]).iter())) {
		[Ok(Value(Number("1"))), Err(InvalidJsonValue(Diagnostic.{ location: Text({ offset: 2, line: 2, column: 1 }), path: [Record(2)], problem: InvalidUtf8 }))] => True
		_ => False
	}
}

expect {
	text = "[\"x\u(0)\r\ny\", NaN, {\"x\":1,\"x\":2}, \"\\ud800\"]"
	props : JsonStream.Props
	props = { framing: Array, json: { control_characters: True, non_finite: True, duplicates: KeepLast, lone_surrogates: Replace } }
	splits_agree(text, props) and (JsonValue.record_decoder(props)).collect([text.to_utf8()].iter()) == Ok([String("x\u(0)\r\ny"), Number("NaN"), Object([{ key: "x", value: Number("2") }]), String("�")])
}

# The complete-string shortcut may consume a colon or a nested comma, but must
# preserve framing, source positions, limits, and every split around that tail.
expect ["[{\"name\":\"value\",\"x\":\"\"}]", "[[\"x\",\"y\"],\"z\"]", "[{\"x\":\"value\",\"y\":\"escaped\\n\"}]", "[{\"x\"::1}]", "[{\"x\":\"v\"},]\n", "[{\"x\":\"v\"}]junk"].all(
	|text| {
		var $same = True
		for limit in (0.U64).to(40) {
			$same = $same and splits_agree(text, { framing: Array, max_record_bytes: limit })
		}
		$same
	},
)

write_text : JsonValue, JsonValue.WriteProps -> Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep, TooLarge(U64)])
write_text = |value, options| {
	_ = measure_value(value, options.indent, 0, 0, options.max_bytes)?
	bytes = write_value(List.with_capacity(256), value, options.sort_keys, options.indent, 0)?
	Ok(Str.from_utf8(bytes) ?? "")
}

own_json_problem : JsonValue.Problem -> JsonValue.Problem
own_json_problem = |problem| match problem {
	DuplicateKey(key) => DuplicateKey(DiagnosticOwnership.own_text(key))
	ConflictingDuplicateKey(key) => ConflictingDuplicateKey(DiagnosticOwnership.own_text(key))
	NonInteroperableNumber(detail) => NonInteroperableNumber({ text: DiagnosticOwnership.own_text(detail.text), reason: detail.reason })
	other => other
}
