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
	parse_str : Str -> Try(JsonValue, [InvalidJson(Error)])
	parse_str = |input| JsonValue.parse_with(input, {})

	## Parse one complete document with the given [JsonValue.Options].
	parse_with : Str, Options -> Try(JsonValue, [InvalidJson(Error)])
	parse_with = |_input, _options| crash "JsonValue.parse_with is not implemented yet"

	## A [Parser] that reads one strict JSON value and the whitespace around
	## it, and leaves the rest of the input for the next parser. A failure is
	## `ParseError({ message, offset })` with the byte offset of the problem.
	parser : Parser(Utf8.Bytes, JsonValue)
	parser = JsonValue.parser_with({})

	## Like [JsonValue.parser], with the given [JsonValue.Options].
	parser_with : Options -> Parser(Utf8.Bytes, JsonValue)
	parser_with = |_options| Parser.custom(|_input| crash "JsonValue.parser_with is not implemented yet")

	## The value of the first member named `key` in an object. Anything else,
	## or an object without that key, is `Err(Missing)`.
	get : JsonValue, Str -> Try(JsonValue, [Missing])
	get = |_value, _key| crash "JsonValue.get is not implemented yet"

	## The item at a zero-based `index` of an array. Anything else, or an index
	## past the end, is `Err(Missing)`.
	at : JsonValue, U64 -> Try(JsonValue, [Missing])
	at = |_value, _index| crash "JsonValue.at is not implemented yet"

	## Follow a path of object keys and array indexes. A segment applied to an
	## array must be a decimal index such as `"0"`.
	get_path : JsonValue, List(Str) -> Try(JsonValue, [Missing])
	get_path = |_value, _path| crash "JsonValue.get_path is not implemented yet"

	## The keys of an object, in order; empty for anything else.
	keys : JsonValue -> List(Str)
	keys = |_value| crash "JsonValue.keys is not implemented yet"

	## The text of a `String` value, or `Err(WrongType)`.
	as_str : JsonValue -> Try(Str, [WrongType])
	as_str = |_value| crash "JsonValue.as_str is not implemented yet"

	## The boolean of a `Bool` value, or `Err(WrongType)`.
	as_bool : JsonValue -> Try(Bool, [WrongType])
	as_bool = |_value| crash "JsonValue.as_bool is not implemented yet"

	## The items of an `Array` value, or `Err(WrongType)`.
	as_list : JsonValue -> Try(List(JsonValue), [WrongType])
	as_list = |_value| crash "JsonValue.as_list is not implemented yet"

	## The members of an `Object` value in order, or `Err(WrongType)`.
	as_object : JsonValue -> Try(List({ key : Str, value : JsonValue }), [WrongType])
	as_object = |_value| crash "JsonValue.as_object is not implemented yet"

	## The integer of a `Number` written as a JSON integer, with no fraction or
	## exponent. Other values, including `1.0` and `1e3`, are `Err(WrongType)`;
	## an integer outside the `I64` range is `Err(OutOfRange)`.
	as_i64 : JsonValue -> Try(I64, [WrongType, OutOfRange])
	as_i64 = |_value| crash "JsonValue.as_i64 is not implemented yet"

	## Like [JsonValue.as_i64], for `U64`. A negative integer other than `-0`
	## is `Err(OutOfRange)`.
	as_u64 : JsonValue -> Try(U64, [WrongType, OutOfRange])
	as_u64 = |_value| crash "JsonValue.as_u64 is not implemented yet"

	## The value of a `Number` as a `Dec`. A number outside the `Dec` range,
	## with more than 18 decimal places, or `NaN` or `Infinity`, is
	## `Err(OutOfRange)`; anything else is `Err(WrongType)`.
	as_dec : JsonValue -> Try(Dec, [WrongType, OutOfRange])
	as_dec = |_value| crash "JsonValue.as_dec is not implemented yet"

	## How [JsonValue.to_str_with] writes a value. `sort_keys` writes object
	## members in byte order of their keys, recursively. `indent` greater than
	## zero writes each item and member on its own line, indented by that many
	## spaces per level.
	WriteOptions := { sort_keys : Bool ?? False, indent : U64 ?? 0 }

	## Write a value as compact JSON, keeping member order.
	##
	## A value built by hand can be one that JSON cannot express: a `Number`
	## whose text is not a JSON number (including `NaN` and `Infinity`) is
	## `InvalidNumber`, a key repeated in one object is `DuplicateKey`, and
	## more than 128 levels of nesting is `TooDeep`.
	to_str : JsonValue -> Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep])
	to_str = |value| JsonValue.to_str_with(value, {})

	## Write a value with the given [JsonValue.WriteOptions].
	to_str_with : JsonValue, WriteOptions -> Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep])
	to_str_with = |_value, _options| crash "JsonValue.to_str_with is not implemented yet"
}
