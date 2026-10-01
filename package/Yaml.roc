import Parser exposing [Parser]
import Utf8

## A practical YAML configuration parser.
##
## This module implements a YAML 1.2 subset aimed at configuration files and
## Markdown frontmatter. It supports a single document (optionally between
## `---` and `...` markers), block mappings and sequences (including compact and
## indentless sequences), single-line flow collections, comments, literal and
## folded block scalars, single-line quoted scalars with all YAML escapes, and
## plain scalars resolved with the YAML 1.2 core schema (null, booleans, decimal,
## octal and hexadecimal integers, floats, `.inf` and `.nan`).
##
## Mapping keys are their source text, so `1` and `01` are different keys.
## Nesting is limited to 100 levels. Anchors, aliases, tags, directives,
## complex keys, multi-line flow collections, multi-line quoted or plain
## scalars, and multi-document streams are rejected with a parse error rather
## than misread.
##
## There are two ways to read a document:
##
## - [Yaml.decode] reads it straight into your own record, list, dict or tuple
##   type, which the compiler infers from how the result is used;
## - [Yaml.parse_str] returns a [Yaml] tree, which [Yaml.get], [Yaml.at],
##   [Yaml.get_path] and the `as_` helpers make easy to explore.
##
## A [Yaml] tree value is one of:
##
## - `Null` for `null`, `~`, an empty value or an empty document;
## - `Bool`, `Int` (an `I64`; larger integers are an error) and `Float` for
##   plain scalars the core schema resolves;
## - `Text` for quoted scalars, block scalars and any other plain scalar;
## - `Sequence` and `Mapping` for collections. Mapping entries keep their source
##   order, and a duplicate key is an error.
Yaml := [
	Null,
	Bool(Bool),
	Int(I64),
	Float(F64),
	Text(Str),
	Sequence(List(Yaml)),
	Mapping(List({ key : Str, value : Yaml })),
].{

	## Location and explanation of invalid YAML input. Lines and columns are
	## one-based; a column counts bytes from the start of its line.
	Error : { line : U64, column : U64, message : Str }

	## Compare two parsed YAML values structurally.
	is_eq : _

	## Hash a parsed YAML value, so trees can be `Dict` keys or `Set` members.
	to_hash : _

	## Parse one YAML configuration document from a string.
	##
	## Empty input produces `Null`. Invalid input returns `InvalidYaml` with a
	## one-based line and column. See the module description for the supported subset.
	##
	## ```roc
	## expect Yaml.parse_str("draft: false") == Ok(Mapping([{ key: "draft", value: Bool(False) }]))
	## ```
	parse_str : Str -> Try(Yaml, [InvalidYaml(Error)])
	parse_str = |input| {
		node = parse_tree(input.to_utf8())?
		resolve(node)
	}

	## A [Parser] that reads the rest of its input as one YAML document, for
	## composing YAML with other parsers.
	##
	## It always consumes all of its input. A failure is a `ParseError` whose
	## `offset` is the byte offset of the reported line and column.
	##
	## ```roc
	## expect Utf8.parse_str(Yaml.parser, "a: 1") == Ok(Mapping([{ key: "a", value: Int(1) }]))
	## ```
	parser : Parser(Utf8.Bytes, Yaml)
	parser = Parser.custom(
		|bytes| {
			parsed = match parse_tree(bytes) {
				Ok(node) => resolve(node)
				Err(problem) => Err(problem)
			}

			match parsed {
				Ok(value) => Ok({ value, rest: [] })
				Err(InvalidYaml(problem)) => Err(ParseError({ message: problem.message, offset: offset_of(bytes, problem.line, problem.column) }))
			}
		},
	)

	## The value stored under `key` in a mapping. Anything else, or a mapping
	## without that key, is `Err(Missing)`.
	##
	## ```roc
	## expect Yaml.parse_str("title: Post").map_ok(|doc| doc.get("title")) == Ok(Ok(Text("Post")))
	## ```
	get : Yaml, Str -> Try(Yaml, [Missing])
	get = |value, key| {
		match value {
			Mapping(entries) => {
				var $found = Err(Missing)
				for entry in entries {
					if entry.key == key {
						$found = Ok(entry.value)
					}
				}
				$found
			}

			_ => Err(Missing)
		}
	}

	## The item at a zero-based `index` of a sequence. Anything else, or an
	## index past the end, is `Err(Missing)`.
	##
	## ```roc
	## expect Yaml.parse_str("[a, b]").map_ok(|doc| doc.at(1)) == Ok(Ok(Text("b")))
	## ```
	at : Yaml, U64 -> Try(Yaml, [Missing])
	at = |value, index| {
		match value {
			Sequence(items) => items.get(index).map_err(|_| Missing)
			_ => Err(Missing)
		}
	}

	## Follow a path of mapping keys and sequence indexes. A segment applied
	## to a sequence must be a decimal index such as `"0"`.
	##
	## ```roc
	## expect {
	##     doc = Yaml.parse_str("jobs:\n  build:\n    steps: [checkout, test]\n")?
	##     doc.get_path(["jobs", "build", "steps", "1"]) == Ok(Text("test"))
	## }
	## ```
	get_path : Yaml, List(Str) -> Try(Yaml, [Missing])
	get_path = |value, path| {
		var $current = Ok(value)
		for segment in path {
			$current =
				match $current {
					Ok(Sequence(items)) =>
						match U64.from_str(segment) {
							Ok(index) => items.get(index).map_err(|_| Missing)
							Err(_) => Err(Missing)
						}

					Ok(node) => node.get(segment)
					Err(_) => Err(Missing)
				}
		}
		$current
	}

	## The text of a `Text` value. Other values, including numbers and
	## booleans, are `Err(WrongType)`; use [Yaml.decode] to read a plain
	## scalar such as `1.10` as text.
	as_str : Yaml -> Try(Str, [WrongType])
	as_str = |value| {
		match value {
			Text(text) => Ok(text)
			_ => Err(WrongType)
		}
	}

	## The integer of an `Int` value, or `Err(WrongType)`.
	as_i64 : Yaml -> Try(I64, [WrongType])
	as_i64 = |value| {
		match value {
			Int(integer) => Ok(integer)
			_ => Err(WrongType)
		}
	}

	## The boolean of a `Bool` value, or `Err(WrongType)`.
	as_bool : Yaml -> Try(Bool, [WrongType])
	as_bool = |value| {
		match value {
			Bool(boolean) => Ok(boolean)
			_ => Err(WrongType)
		}
	}

	## The items of a `Sequence` value, or `Err(WrongType)`.
	##
	## ```roc
	## expect Yaml.parse_str("[1, 2]").map_ok(|doc| doc.as_list()) == Ok(Ok([Int(1), Int(2)]))
	## ```
	as_list : Yaml -> Try(List(Yaml), [WrongType])
	as_list = |value| {
		match value {
			Sequence(items) => Ok(items)
			_ => Err(WrongType)
		}
	}

	## Render a parsed YAML value in Roc source-like notation for inspection.
	##
	## The output is meant for debugging and test messages, not for writing
	## YAML. Control characters in text and keys are escaped, so the output is
	## always printable on one line.
	##
	## ```roc
	## expect Yaml.to_inspect(Sequence([Int(1), Text("a")])) == "Sequence([Int(1), Text(\"a\")])"
	## ```
	to_inspect : Yaml -> Str
	to_inspect = |value| inspect_yaml(value)

	## Decode one YAML document straight into a Roc type, chosen by type
	## inference.
	##
	## - A mapping decodes into a record (by field name), or a `Dict` with
	##   `Str` or integer keys. Unknown keys are skipped; use [Yaml.decoder]
	##   with `unknown_keys: Reject` to make them an error.
	## - A sequence decodes into a `List` or, when it has exactly the right
	##   number of items, a tuple.
	## - A scalar is resolved for the type that asks for it, using the YAML 1.2
	##   core schema: `Str` takes any quoted or block scalar and any plain
	##   scalar except a null, so `version: 1.10` stays `"1.10"`; `Bool`, the
	##   integer types, `F32`, `F64` and `Dec` take the matching core-schema
	##   forms; a tag union without payloads takes the tag's name.
	## - A null (`null`, `~` or an empty value) decodes as an empty list,
	##   dict or record, and fills a `Try(_, [Null])` field with `Err(Null)`.
	## - A `Try(_, [Missing])` field is `Err(Missing)` when its key is absent.
	##   Any other absent field fails with `MissingRequiredField(name)`, a tag
	##   the compiler adds to the error type.
	##
	## Invalid YAML and values that do not fit the type both fail with
	## `InvalidYaml`, at the line and column of the offending value.
	##
	## ```roc
	## Config : { name : Str, version : Str, port : U16, debug : Try(Bool, [Missing]) }
	##
	## expect {
	##     config : Try(Config, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	##     config = Yaml.decode("name: app\nversion: 1.10\nport: 8080\n")
	##     config == Ok({ name: "app", version: "1.10", port: 8080, debug: Err(Missing) })
	## }
	## ```
	decode : Str -> Try(a, [InvalidYaml(Error), ..errs])
		where [a.Parseable([InvalidYaml(Error), ..errs])]
	decode = |input| {
		A : a
		parse_value = A.parser_for(Yaml.Format.{ keys: SnakeCase, unknown_keys: Skip })
		decode_with(parse_value, input)
	}

	## Build a decoder like [Yaml.decode] with other conventions.
	##
	## - `keys` says how YAML keys spell Roc's `snake_case` field names:
	##   `SnakeCase` as written, `KebabCase` with dashes (`user-id`) or
	##   `CamelCase` (`userId`).
	## - `unknown_keys` is `Skip` to ignore keys the record does not have, or
	##   `Reject` to fail with `InvalidYaml` at the first one.
	##
	## Build the decoder once, for example as a top-level constant, and call it
	## for each document.
	##
	## ```roc
	## decode_strict = Yaml.decoder({ keys: KebabCase, unknown_keys: Reject })
	##
	## expect {
	##     result : Try({ user_id : U64 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	##     result = decode_strict("user-id: 7\n")
	##     result == Ok({ user_id: 7 })
	## }
	## ```
	decoder : DecodeOptions -> (Str -> Try(a, [InvalidYaml(Error), ..errs]))
		where [a.Parseable([InvalidYaml(Error), ..errs])]
	decoder = |options| {
		A : a
		parse_value = A.parser_for(Yaml.Format.{ keys: options.keys, unknown_keys: options.unknown_keys })
		|input| decode_with(parse_value, input)
	}

	## Options for [Yaml.decoder].
	DecodeOptions : { keys : [SnakeCase, KebabCase, CamelCase], unknown_keys : [Skip, Reject] }

	## Names the requirement that a type can be decoded from YAML, so a
	## generic function can say "YAML-decodable" without naming the format and
	## cursor types:
	##
	## ```roc
	## load : Str -> Try(a, [InvalidYaml(Yaml.Error), ..errs]) where [a.Yaml.Parseable([InvalidYaml(Yaml.Error), ..errs])]
	## load = |text| Yaml.decode(text)
	## ```
	a.Parseable(errs) :
		where [
			a.parser_for : Yaml.Format -> (Yaml.Cursor -> Try({ value : a, rest : Yaml.Cursor }, errs)),
		]

	## The decoding format that [Yaml.decode] passes to a type's `parser_for`.
	## It implements the builtin parsing protocol; you do not call its methods
	## yourself.
	Format :: { keys : [SnakeCase, KebabCase, CamelCase], unknown_keys : [Skip, Reject] }.{

		## Spell a Roc field name as a YAML key, following the decoder's `keys` option.
		rename_field : Format, Str -> Str
		rename_field = |format, name| {
			match format.keys {
				SnakeCase => name
				KebabCase => Str.join_with(Str.split_on(name, "_"), "-")
				CamelCase => camel_case(name)
			}
		}

		## Read a string: any quoted or block scalar, or a plain scalar other than null.
		parse_str : Format, Cursor -> Try({ value : Str, rest : Cursor }, [InvalidYaml(Error)])
		parse_str = |_, cursor| {
			scalar = take_scalar(cursor, "a string")?
			if scalar.plain and is_null_text(scalar.text) {
				fail(scalar.line, scalar.column, "expected a string, found null")
			} else {
				Ok({ value: scalar.text, rest: advance_cursor(cursor, 1) })
			}
		}

		## Read a core-schema boolean such as `true` or `FALSE`.
		parse_bool : Format, Cursor -> Try({ value : Bool, rest : Cursor }, [InvalidYaml(Error)])
		parse_bool = |_, cursor| {
			scalar = take_scalar(cursor, "a boolean")?
			value =
				if !scalar.plain {
					return scalar_mismatch(scalar, "a boolean")
				} else if ["true", "True", "TRUE"].contains(scalar.text) {
					True
				} else if ["false", "False", "FALSE"].contains(scalar.text) {
					False
				} else {
					return scalar_mismatch(scalar, "a boolean")
				}
			Ok({ value, rest: advance_cursor(cursor, 1) })
		}

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_u8 : Format, Cursor -> Try({ value : U8, rest : Cursor }, [InvalidYaml(Error)])
		parse_u8 = |_, cursor| decode_integer(cursor, "U8", U8.from_str, U128.to_u8_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_i8 : Format, Cursor -> Try({ value : I8, rest : Cursor }, [InvalidYaml(Error)])
		parse_i8 = |_, cursor| decode_integer(cursor, "I8", I8.from_str, U128.to_i8_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_u16 : Format, Cursor -> Try({ value : U16, rest : Cursor }, [InvalidYaml(Error)])
		parse_u16 = |_, cursor| decode_integer(cursor, "U16", U16.from_str, U128.to_u16_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_i16 : Format, Cursor -> Try({ value : I16, rest : Cursor }, [InvalidYaml(Error)])
		parse_i16 = |_, cursor| decode_integer(cursor, "I16", I16.from_str, U128.to_i16_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_u32 : Format, Cursor -> Try({ value : U32, rest : Cursor }, [InvalidYaml(Error)])
		parse_u32 = |_, cursor| decode_integer(cursor, "U32", U32.from_str, U128.to_u32_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_i32 : Format, Cursor -> Try({ value : I32, rest : Cursor }, [InvalidYaml(Error)])
		parse_i32 = |_, cursor| decode_integer(cursor, "I32", I32.from_str, U128.to_i32_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_u64 : Format, Cursor -> Try({ value : U64, rest : Cursor }, [InvalidYaml(Error)])
		parse_u64 = |_, cursor| decode_integer(cursor, "U64", U64.from_str, U128.to_u64_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_i64 : Format, Cursor -> Try({ value : I64, rest : Cursor }, [InvalidYaml(Error)])
		parse_i64 = |_, cursor| decode_integer(cursor, "I64", I64.from_str, U128.to_i64_try)

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_u128 : Format, Cursor -> Try({ value : U128, rest : Cursor }, [InvalidYaml(Error)])
		parse_u128 = |_, cursor| decode_integer(cursor, "U128", U128.from_str, |n| Ok(n))

		## Read a core-schema integer (decimal, `0o` octal or `0x` hexadecimal) that fits the type.
		parse_i128 : Format, Cursor -> Try({ value : I128, rest : Cursor }, [InvalidYaml(Error)])
		parse_i128 = |_, cursor| decode_integer(cursor, "I128", I128.from_str, U128.to_i128_try)

		## Read a core-schema integer or float; `.inf` and `.nan` only where the type has them.
		parse_dec : Format, Cursor -> Try({ value : Dec, rest : Cursor }, [InvalidYaml(Error)])
		parse_dec = |_, cursor| decode_float(cursor, "Dec", Dec.from_str, |_| Err(NotFinite))

		## Read a core-schema integer or float; `.inf` and `.nan` only where the type has them.
		parse_f32 : Format, Cursor -> Try({ value : F32, rest : Cursor }, [InvalidYaml(Error)])
		parse_f32 = |_, cursor| decode_float(cursor, "F32", F32.from_str, special_f32)

		## Read a core-schema integer or float; `.inf` and `.nan` only where the type has them.
		parse_f64 : Format, Cursor -> Try({ value : F64, rest : Cursor }, [InvalidYaml(Error)])
		parse_f64 = |_, cursor| decode_float(cursor, "F64", F64.from_str, special_f64)

		## Succeeds only on a null, so a `Try(_, [Null])` field falls back to
		## its value parser otherwise.
		parse_null : Format, Cursor -> Try(Cursor, [InvalidYaml(Error)])
		parse_null = |_, cursor| {
			event = current_event(cursor)?
			match event.kind {
				Plain(text) if is_null_text(text) => Ok(advance_cursor(cursor, 1))
				_ => fail(event.line, event.column, "expected null")
			}
		}

		## Read a tag without payloads from a scalar holding its name.
		parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), Cursor -> Try({ value : a, rest : Cursor }, [InvalidYaml(Error)])
		parse_tag_union = |format, spec, cursor| {
			scalar = take_scalar(cursor, "a tag name")?
			Encoding.ParseTagUnionSpec.parse(
				spec,
				{
					tag: scalar.text,
					encoding: format,
					state: advance_cursor(cursor, 1),
					start_payloads: |state, count|
						if count == 0 {
							Ok(state)
						} else {
							fail(scalar.line, scalar.column, "tags with payloads cannot be decoded from YAML")
						},
					next_payload: |state, _, _| Ok(state),
					finish_payloads: |state, _| Ok(state),
					missing: InvalidYaml({ line: scalar.line, column: scalar.column, message: "unexpected tag `${scalar.text}`" }),
				},
			)
		}

		## Start a list at a sequence (or a null, as an empty list); sequences are always counted.
		parse_list_start : Format, Cursor -> Try([Counted({ len : U64, rest : Cursor }), Uncounted(Cursor)], [InvalidYaml(Error)])
		parse_list_start = |_, cursor| {
			event = current_event(cursor)?
			match event.kind {
				SeqStart({ len, size: _ }) => Ok(Counted({ len, rest: advance_cursor(cursor, 1) }))
				Plain(text) if is_null_text(text) => Ok(Counted({ len: 0, rest: advance_cursor(cursor, 1) }))
				_ => mismatch(event, "a sequence")
			}
		}

		## Sequences are always counted, so the driver never asks for the next item.
		parse_list_next : Format, Cursor -> Try([Item(Cursor), Done(Cursor)], [InvalidYaml(Error)])
		parse_list_next = |_, cursor| Ok(Done(cursor))

		## Protocol step that counted YAML collections never need; it does nothing.
		parse_list_after_item : Format, Cursor -> Try([Continue(Cursor), Done(Cursor)], [InvalidYaml(Error)])
		parse_list_after_item = |_, cursor| Ok(Done(cursor))

		## Tuple protocol step; a tuple needs a sequence of exactly its length.
		parse_tuple_start : Format, Cursor, U64 -> Try(Cursor, [InvalidYaml(Error)])
		parse_tuple_start = |_, cursor, expected| {
			event = current_event(cursor)?
			match event.kind {
				SeqStart({ len, size: _ }) if len == expected => Ok(advance_cursor(cursor, 1))
				SeqStart({ len, size: _ }) => fail(event.line, event.column, "expected a sequence of ${expected.to_str()} items, found ${len.to_str()}")
				_ => mismatch(event, "a sequence of ${expected.to_str()} items")
			}
		}

		## Tuple protocol step; a tuple needs a sequence of exactly its length.
		parse_tuple_next : Format, Cursor, U64, U64 -> Try(Cursor, [InvalidYaml(Error)])
		parse_tuple_next = |_, cursor, _, _| Ok(cursor)

		## Tuple protocol step; a tuple needs a sequence of exactly its length.
		parse_tuple_end : Format, Cursor, U64 -> Try(Cursor, [InvalidYaml(Error)])
		parse_tuple_end = |_, cursor, _| Ok(cursor)

		## Start a record or dict at a mapping (or a null, as an empty one); mappings are always counted.
		parse_record_start : Format, Cursor -> Try([Counted({ len : U64, rest : Cursor }), Uncounted(Cursor)], [InvalidYaml(Error)])
		parse_record_start = |_, cursor| mapping_start(cursor)

		## Read the next mapping key for the generated record parser to match.
		parse_record_field : Format,
		Encoding.FieldName.FieldNames(_shape),
		Cursor -> Try(
			[
				Field({ field : Encoding.FieldName(_shape), rest : Cursor }),
				TryField({ name : Str, rest : Cursor }),
				TryFieldCaseless({ name : Str, rest : Cursor }),
				Continue(Cursor),
				Done(Cursor),
			],
			[InvalidYaml(Error)],
		)
		parse_record_field = |_, _, cursor| {
			event = current_event(cursor)?
			match event.kind {
				Key(name) => Ok(TryField({ name, rest: advance_cursor(cursor, 1) }))
				_ => mismatch(event, "a mapping key")
			}
		}

		## Mappings are always counted, so the driver never asks whether another
		## field follows.
		parse_record_after_field : Format, Cursor -> Try([Continue(Cursor), Done(Cursor)], [InvalidYaml(Error)])
		parse_record_after_field = |_, cursor| Ok(Done(cursor))

		## Skip the value of a key the record lacks, or reject it when `unknown_keys` is `Reject`.
		skip_record_field : Format, Cursor -> Try(Cursor, [InvalidYaml(Error)])
		skip_record_field = |format, cursor| {
			match format.unknown_keys {
				Skip => Ok(skip_value(cursor))
				Reject => {
					key = cursor.events.get(cursor.pos - 1) ?? { line: 1, column: 1, kind: Key("") }
					name =
						match key.kind {
							Key(text) => text
							_ => ""
						}
					fail(key.line, key.column, "unknown key `${name}`")
				}
			}
		}

		## Start a record or dict at a mapping (or a null, as an empty one); mappings are always counted.
		parse_dict_start : Format, Cursor -> Try([Counted({ len : U64, rest : Cursor }), Uncounted(Cursor)], [InvalidYaml(Error)])
		parse_dict_start = |_, cursor| mapping_start(cursor)

		## Protocol step that counted YAML collections never need; it does nothing.
		parse_dict_next : Format, Cursor -> Try([Entry(Cursor), Done(Cursor)], [InvalidYaml(Error)])
		parse_dict_next = |_, cursor| Ok(Done(cursor))

		## Protocol step that counted YAML collections never need; it does nothing.
		parse_dict_after_key : Format, Cursor -> Try(Cursor, [InvalidYaml(Error)])
		parse_dict_after_key = |_, cursor| Ok(cursor)

		## Protocol step that counted YAML collections never need; it does nothing.
		parse_dict_after_entry : Format, Cursor -> Try([Continue(Cursor), Done(Cursor)], [InvalidYaml(Error)])
		parse_dict_after_entry = |_, cursor| Ok(Done(cursor))

		## The error for a value the generated parser cannot use.
		invalid_value : Format, Cursor -> [InvalidYaml(Error)]
		invalid_value = |_, cursor| {
			event = cursor.events.get(cursor.pos) ?? { line: 1, column: 1, kind: Plain("") }
			InvalidYaml({ line: event.line, column: event.column, message: "this value cannot be decoded into the requested type" })
		}

		## Read a mapping key as a `Dict` key.
		parse_key_str : Format, Cursor -> Try({ value : Str, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_str = |_, cursor| {
			key = take_key(cursor)?
			Ok({ value: key.text, rest: advance_cursor(cursor, 1) })
		}

		## Read a mapping key as a `Dict` key.
		parse_key_u8 : Format, Cursor -> Try({ value : U8, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_u8 = |_, cursor| decode_integer(cursor, "U8", U8.from_str, U128.to_u8_try)

		## Read a mapping key as a `Dict` key.
		parse_key_i8 : Format, Cursor -> Try({ value : I8, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_i8 = |_, cursor| decode_integer(cursor, "I8", I8.from_str, U128.to_i8_try)

		## Read a mapping key as a `Dict` key.
		parse_key_u16 : Format, Cursor -> Try({ value : U16, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_u16 = |_, cursor| decode_integer(cursor, "U16", U16.from_str, U128.to_u16_try)

		## Read a mapping key as a `Dict` key.
		parse_key_i16 : Format, Cursor -> Try({ value : I16, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_i16 = |_, cursor| decode_integer(cursor, "I16", I16.from_str, U128.to_i16_try)

		## Read a mapping key as a `Dict` key.
		parse_key_u32 : Format, Cursor -> Try({ value : U32, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_u32 = |_, cursor| decode_integer(cursor, "U32", U32.from_str, U128.to_u32_try)

		## Read a mapping key as a `Dict` key.
		parse_key_i32 : Format, Cursor -> Try({ value : I32, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_i32 = |_, cursor| decode_integer(cursor, "I32", I32.from_str, U128.to_i32_try)

		## Read a mapping key as a `Dict` key.
		parse_key_u64 : Format, Cursor -> Try({ value : U64, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_u64 = |_, cursor| decode_integer(cursor, "U64", U64.from_str, U128.to_u64_try)

		## Read a mapping key as a `Dict` key.
		parse_key_i64 : Format, Cursor -> Try({ value : I64, rest : Cursor }, [InvalidYaml(Error)])
		parse_key_i64 = |_, cursor| decode_integer(cursor, "I64", I64.from_str, U128.to_i64_try)
	}

	## The read position that [Yaml.decode] threads through a type's parser:
	## the document as a flat list of events and an index into it.
	Cursor :: { events : List(Event), pos : U64 }
}

## One step of a flattened document. A collection start records its item
## count and how many events its contents span, so skipping it is O(1).
Event : { line : U64, column : U64, kind : [Plain(Str), Quoted(Str), Key(Str), SeqStart({ len : U64, size : U64 }), MapStart({ len : U64, size : U64 })] }

## A parsed document before plain scalars are resolved. Keeping them as text
## lets [Yaml.decode] resolve each one for the type that reads it.
Node := { line : U64, column : U64, kind : [Plain(Str), Quoted(Str), Seq(List(Node)), Map(List(NodeEntry))] }

NodeEntry : { key : Str, line : U64, column : U64, value : Node }

null_node : U64, U64 -> Node
null_node = |line, column| Node.{ line, column, kind: Plain("") }

## Parse a document into a tree of unresolved nodes.
parse_tree : Utf8.Bytes -> Try(Node, [InvalidYaml(Yaml.Error)])
parse_tree = |input| {
	bytes = check_printable(drop_byte_order_mark(input))?
	raw_lines = split_lines(bytes, 1)
	lines = prepare_lines(raw_lines)?
	start = { lines, at: 0, head: Err(NoHead) }

	match peek(start) {
		Err(_) => Ok(null_node(1, 1))

		Ok(first) if first.tab =>
			fail(first.number, first.indent + 1, "tabs may not be used for YAML indentation")

		Ok(first) if first.indent != 0 =>
			fail(first.number, 1, "the document root must not be indented")

		Ok(first) => {
			parsed = parse_node(start, first.indent, 0, raw_lines)?

			match peek(parsed.input) {
				Err(_) => Ok(parsed.node)

				Ok(leftover) =>
					fail(leftover.number, leftover.indent + 1, "unexpected content after the document root")
			}
		}
	}
}

## Resolve every plain scalar with the YAML 1.2 core schema.
resolve : Node -> Try(Yaml, [InvalidYaml(Yaml.Error)])
resolve = |node| {
	match node.kind {
		Plain(text) => resolve_plain(text.to_utf8(), node.line, node.column)
		Quoted(text) => Ok(Text(text))

		Seq(items) => {
			var $values = List.with_capacity(items.len())
			for item in items {
				$values = $values.append(resolve(item)?)
			}
			Ok(Sequence($values))
		}

		Map(entries) => {
			var $resolved = List.with_capacity(entries.len())
			for entry in entries {
				$resolved = $resolved.append({ key: entry.key, value: resolve(entry.value)? })
			}
			Ok(Mapping($resolved))
		}
	}
}

## Flatten a tree into decoding events, in document order.
flatten : Node, List(Event) -> List(Event)
flatten = |node, events| {
	match node.kind {
		Plain(text) => events.append({ line: node.line, column: node.column, kind: Plain(text) })
		Quoted(text) => events.append({ line: node.line, column: node.column, kind: Quoted(text) })

		Seq(items) => {
			start = events.len()
			var $events = events.append({ line: node.line, column: node.column, kind: SeqStart({ len: items.len(), size: 0 }) })
			for item in items {
				$events = flatten(item, $events)
			}
			size = $events.len() - start - 1
			$events.set(start, { line: node.line, column: node.column, kind: SeqStart({ len: items.len(), size }) }) ?? $events
		}

		Map(entries) => {
			start = events.len()
			var $events = events.append({ line: node.line, column: node.column, kind: MapStart({ len: entries.len(), size: 0 }) })
			for entry in entries {
				$events = flatten(entry.value, $events.append({ line: entry.line, column: entry.column, kind: Key(entry.key) }))
			}
			size = $events.len() - start - 1
			$events.set(start, { line: node.line, column: node.column, kind: MapStart({ len: entries.len(), size }) }) ?? $events
		}
	}
}

decode_with : (Yaml.Cursor -> Try({ value : a, rest : Yaml.Cursor }, [InvalidYaml(Yaml.Error), ..errs])), Str -> Try(a, [InvalidYaml(Yaml.Error), ..errs])
decode_with = |parse_value, input| {
	node = parse_tree(input.to_utf8())?
	events = flatten(node, [])
	parsed = parse_value(Yaml.Cursor.{ events, pos: 0 })?
	Ok(parsed.value)
}

advance_cursor : Yaml.Cursor, U64 -> Yaml.Cursor
advance_cursor = |cursor, count| Yaml.Cursor.{ events: cursor.events, pos: cursor.pos + count }

current_event : Yaml.Cursor -> Try(Event, [InvalidYaml(Yaml.Error)])
current_event = |cursor| {
	match cursor.events.get(cursor.pos) {
		Ok(event) => Ok(event)
		Err(_) => fail(1, 1, "unexpected end of the YAML document")
	}
}

## Skip the value at the cursor, including everything inside a collection.
skip_value : Yaml.Cursor -> Yaml.Cursor
skip_value = |cursor| {
	match cursor.events.get(cursor.pos) {
		Ok({ kind: SeqStart({ len: _, size }), .. }) | Ok({ kind: MapStart({ len: _, size }), .. }) => advance_cursor(cursor, size + 1)
		_ => advance_cursor(cursor, 1)
	}
}

mapping_start : Yaml.Cursor -> Try([Counted({ len : U64, rest : Yaml.Cursor }), Uncounted(Yaml.Cursor)], [InvalidYaml(Yaml.Error)])
mapping_start = |cursor| {
	event = current_event(cursor)?
	match event.kind {
		MapStart({ len, size: _ }) => Ok(Counted({ len, rest: advance_cursor(cursor, 1) }))
		Plain(text) if is_null_text(text) => Ok(Counted({ len: 0, rest: advance_cursor(cursor, 1) }))
		_ => mismatch(event, "a mapping")
	}
}

Scalar : { text : Str, plain : Bool, line : U64, column : U64 }

take_scalar : Yaml.Cursor, Str -> Try(Scalar, [InvalidYaml(Yaml.Error)])
take_scalar = |cursor, expected| {
	event = current_event(cursor)?
	match event.kind {
		Plain(text) => Ok({ text, plain: True, line: event.line, column: event.column })
		Quoted(text) => Ok({ text, plain: False, line: event.line, column: event.column })
		_ => mismatch(event, expected)
	}
}

## A mapping key, which resolves like a plain scalar for integer dict keys.
take_key : Yaml.Cursor -> Try(Scalar, [InvalidYaml(Yaml.Error)])
take_key = |cursor| {
	event = current_event(cursor)?
	match event.kind {
		Key(text) => Ok({ text, plain: True, line: event.line, column: event.column })
		_ => mismatch(event, "a mapping key")
	}
}

mismatch : Event, Str -> Try(_, [InvalidYaml(Yaml.Error)])
mismatch = |event, expected| {
	found =
		match event.kind {
			SeqStart(_) => "a sequence"
			MapStart(_) => "a mapping"
			Key(text) => "the key `${text}`"
			Quoted(text) => "the string ${Str.inspect(text)}"
			Plain(text) if is_null_text(text) => "null"
			Plain(text) => "`${text}`"
		}
	fail(event.line, event.column, "expected ${expected}, found ${found}")
}

scalar_mismatch : Scalar, Str -> Try(_, [InvalidYaml(Yaml.Error)])
scalar_mismatch = |scalar, expected| {
	kind = if scalar.plain Plain(scalar.text) else Quoted(scalar.text)
	mismatch({ line: scalar.line, column: scalar.column, kind }, expected)
}

is_null_text : Str -> Bool
is_null_text = |text| ["", "null", "Null", "NULL", "~"].contains(text)

## Read an integer scalar (or integer mapping key) with the core schema's
## decimal, `0o` octal and `0x` hexadecimal forms.
decode_integer : Yaml.Cursor, Str, (Str -> Try(n, _from_str_err)), (U128 -> Try(n, _range_err)) -> Try({ value : n, rest : Yaml.Cursor }, [InvalidYaml(Yaml.Error)])
decode_integer = |cursor, type_name, from_str, from_u128| {
	event = current_event(cursor)?
	scalar =
		match event.kind {
			Key(text) | Plain(text) => { text, plain: True, line: event.line, column: event.column }
			Quoted(text) => { text, plain: False, line: event.line, column: event.column }
			_ => return mismatch(event, "an integer")
		}
	bytes = scalar.text.to_utf8()
	parsed =
		if !scalar.plain {
			Err(NotInteger)
		} else if is_decimal_integer(bytes) {
			unsigned = match bytes {
				['+', .. as rest] => Str.from_utf8_lossy(rest)
				_ => scalar.text
			}
			from_str(unsigned).map_err(|_| OutOfRange)
		} else {
			match bytes {
				['0', 'o', .. as digits] if !digits.is_empty() and digits.all(|b| b >= '0' and b <= '7') =>
					match radix_u128(digits, 8) {
						Ok(n) => from_u128(n).map_err(|_| OutOfRange)
						Err(_) => Err(OutOfRange)
					}

				['0', 'x', .. as digits] if !digits.is_empty() and digits.all(is_hex_digit) =>
					match radix_u128(digits, 16) {
						Ok(n) => from_u128(n).map_err(|_| OutOfRange)
						Err(_) => Err(OutOfRange)
					}

				_ => Err(NotInteger)
			}
		}

	match parsed {
		Ok(value) => Ok({ value, rest: advance_cursor(cursor, 1) })
		Err(OutOfRange) => fail(scalar.line, scalar.column, "integer `${scalar.text}` is outside the range of ${type_name}")
		Err(NotInteger) => scalar_mismatch(scalar, "an integer")
	}
}

radix_u128 : Utf8.Bytes, U128 -> Try(U128, [OutOfRange])
radix_u128 = |digits, radix| {
	var $total = 0
	for byte in digits {
		digit =
			if is_digit(byte) {
				U8.to_u128(byte - '0')
			} else if byte >= 'a' {
				U8.to_u128(byte - 'a' + 10)
			} else {
				U8.to_u128(byte - 'A' + 10)
			}
		if $total > (U128.highest - digit) // radix {
			return Err(OutOfRange)
		}
		$total = $total * radix + digit
	}
	Ok($total)
}

## Read a float scalar: any core-schema integer or float, plus `.inf` and
## `.nan` where the target type has them.
decode_float : Yaml.Cursor, Str, (Str -> Try(n, _from_str_err)), ([Infinity, NegativeInfinity, NaN] -> Try(n, _special_err)) -> Try({ value : n, rest : Yaml.Cursor }, [InvalidYaml(Yaml.Error)])
decode_float = |cursor, type_name, from_str, special| {
	scalar = take_scalar(cursor, "a number")?
	bytes = scalar.text.to_utf8()
	text = scalar.text
	parsed =
		if !scalar.plain {
			Err(NotNumber)
		} else if [".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF"].contains(text) {
			special(Infinity).map_err(|_| OutOfRange)
		} else if ["-.inf", "-.Inf", "-.INF"].contains(text) {
			special(NegativeInfinity).map_err(|_| OutOfRange)
		} else if [".nan", ".NaN", ".NAN"].contains(text) {
			special(NaN).map_err(|_| OutOfRange)
		} else if is_decimal_integer(bytes) {
			from_str(Str.drop_prefix(text, "+")).map_err(|_| OutOfRange)
		} else {
			match core_float_text(bytes) {
				Ok(canonical) => from_str(canonical).map_err(|_| OutOfRange)
				Err(_) => Err(NotNumber)
			}
		}

	match parsed {
		Ok(value) => Ok({ value, rest: advance_cursor(cursor, 1) })
		Err(OutOfRange) => fail(scalar.line, scalar.column, "number `${text}` cannot be represented as ${type_name}")
		Err(NotNumber) => scalar_mismatch(scalar, "a number")
	}
}

special_f64 : [Infinity, NegativeInfinity, NaN] -> Try(F64, [NotFinite])
special_f64 = |special| {
	match special {
		Infinity => Ok(F64.infinity)
		NegativeInfinity => Ok(-F64.infinity)
		NaN => Ok(F64.nan)
	}
}

special_f32 : [Infinity, NegativeInfinity, NaN] -> Try(F32, [NotFinite])
special_f32 = |special| {
	match special {
		Infinity => Ok(F32.infinity)
		NegativeInfinity => Ok(-F32.infinity)
		NaN => Ok(F32.nan)
	}
}

camel_case : Str -> Str
camel_case = |name| {
	var $out = []
	var $upper = False
	for byte in name.to_utf8() {
		if byte == '_' {
			$upper = !$out.is_empty()
		} else if $upper and byte >= 'a' and byte <= 'z' {
			$out = $out.append(byte - 32)
			$upper = False
		} else {
			$out = $out.append(byte)
			$upper = False
		}
	}
	Str.from_utf8_lossy($out)
}

## The byte offset of a one-based line and (byte) column in `bytes`. A byte
## order mark comes before the first line.
offset_of : Utf8.Bytes, U64, U64 -> U64
offset_of = |bytes, line, column| {
	var $index =
		match bytes {
			[0xEF, 0xBB, 0xBF, ..] => 3
			_ => 0
		}
	var $line = 1

	while $index < bytes.len() and $line < line {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0

		if byte == '\n' or (byte == '\r' and next != '\n') {
			$line = $line + 1
		}

		$index = $index + 1
	}

	offset = $index + column - 1
	if offset > bytes.len() bytes.len() else offset
}

drop_byte_order_mark : Utf8.Bytes -> Utf8.Bytes
drop_byte_order_mark = |bytes| {
	match bytes {
		[0xEF, 0xBB, 0xBF, .. as rest] => rest
		_ => bytes
	}
}

## Reject characters outside YAML 1.2's printable set (5.1): C0 controls other
## than tab and line breaks, DEL, C1 controls other than NEL, and U+FFFE/U+FFFF.
check_printable : Utf8.Bytes -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
check_printable = |bytes| {
	var $line = 1
	var $column = 1
	var $index = 0
	var $problem = Err(NoProblem)

	while $index < bytes.len() and $problem == Err(NoProblem) {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0
		after = bytes.get($index + 2) ?? 0
		control = (byte < 0x20 and byte != '\t' and byte != '\n' and byte != '\r') or byte == 0x7F
		c1 = byte == 0xC2 and next >= 0x80 and next <= 0x9F and next != 0x85
		non_character = byte == 0xEF and next == 0xBF and (after == 0xBE or after == 0xBF)

		if control or c1 or non_character {
			$problem = Ok({ line: $line, column: $column })
		} else if byte == '\n' or (byte == '\r' and next != '\n') {
			$line = $line + 1
			$column = 1
		} else {
			# Columns count bytes, like every other error location.
			$column = $column + 1
		}

		$index = $index + 1
	}

	match $problem {
		Ok(location) => fail(location.line, location.column, "YAML does not allow this control or non-character code point")
		Err(_) => Ok(bytes)
	}
}

## `terminated` is false only for a final line without a line break. `tab`
## marks a tab right after the indentation, which is an error only where the
## line is block structure rather than block scalar content.
Line : { content : Utf8.Bytes, indent : U64, number : U64, terminated : Bool, tab : Bool }

## The structural lines still to parse: an index into the prepared lines,
## plus an optional `head` line that stands in front of them. A compact
## collection after "- " becomes such a head, so no list is ever copied or
## rescanned and parsing stays linear in the number of lines.
Input : { lines : List(Line), at : U64, head : Try(Line, [NoHead]) }

NodeResult : { node : Node, input : Input }

peek : Input -> Try(Line, [End])
peek = |input| {
	match input.head {
		Ok(line) => Ok(line)
		Err(_) => input.lines.get(input.at).map_err(|_| End)
	}
}

advance : Input -> Input
advance = |input| {
	match input.head {
		Ok(_) => { lines: input.lines, at: input.at, head: Err(NoHead) }
		Err(_) => { lines: input.lines, at: input.at + 1, head: Err(NoHead) }
	}
}

## Replace the current line with `line`.
replace_head : Input, Line -> Input
replace_head = |input, line| {
	rest = advance(input)
	{ lines: rest.lines, at: rest.at, head: Ok(line) }
}

Quote : [NoQuote, SingleQuote, DoubleQuote]

BlockStyle : [LiteralBlock, FoldedBlock]

BlockChomp : [ClipChomp, StripChomp, KeepChomp]

BlockIndent : [AutoIndent, ExplicitIndent(U64)]

ContentIndent : [PendingIndent, FixedIndent(U64)]

BlockHeader : { style : BlockStyle, chomp : BlockChomp, indent : BlockIndent }

nesting_message : Str
nesting_message = "YAML nesting exceeds the supported limit of 100 levels"

tab_message : Str
tab_message = "tabs may not be used for YAML indentation"

parse_node : Input, U64, U64, List(Line) -> Try(NodeResult, [InvalidYaml(Yaml.Error)])
parse_node = |input, indent, depth, raw_lines| {
	match peek(input) {
		Err(_) if depth >= 100 => fail(1, 1, nesting_message)
		Ok(line) if depth >= 100 => fail(line.number, line.indent + 1, nesting_message)
		Err(_) => fail(1, 1, "expected a YAML value")
		Ok(first) if first.tab => fail(first.number, first.indent + 1, tab_message)
		Ok(first) if first.indent != indent => fail(first.number, first.indent + 1, "unexpected indentation")
		Ok(first) if is_sequence_line(first.content) => parse_sequence(input, indent, depth, raw_lines)

		Ok(first) => {
			match split_mapping_entry(first.content) {
				Ok(_) => parse_mapping(input, indent, depth, raw_lines)
				Err(_) => {
					if starts_block_scalar(first.content) {
						# At the document root (indentation -1) content may start in column 0.
						min_indent = if depth == 0 and first.indent == 0 0 else first.indent + 1
						parse_block_scalar(advance(input), raw_lines, first.content, first.number, first.indent + 1, min_indent)
					} else {
						node = parse_inline_value(first.content, first.number, first.indent + 1, depth + 1)?
						Ok({ node, input: advance(input) })
					}
				}
			}
		}
	}
}

parse_mapping : Input, U64, U64, List(Line) -> Try(NodeResult, [InvalidYaml(Yaml.Error)])
parse_mapping = |start, indent, depth, raw_lines| {
	location = node_location(start)
	var $input = start
	var $entries = []
	var $seen = Set.empty()
	var $done = False

	while !$done {
		match mapping_line($input, indent)? {
			Stop => {
				$done = True
			}

			Entry({ line, parts }) => {
				key_column = line.indent + 1
				key = parse_key(parts.key, line.number, key_column)?

				if $seen.contains(key) {
					return fail(line.number, key_column, "duplicate mapping key `${key}`")
				}

				rest = advance($input)
				value_column = line.indent + parts.value_column
				parsed =
					if parts.value.is_empty() {
						match peek(rest) {
							Ok(next) if next.indent > indent => parse_node(rest, next.indent, depth + 1, raw_lines)?

							# A sequence may sit at its key's indentation (YAML 1.2 8.2.1).
							Ok(next) if next.indent == indent and is_sequence_line(next.content) => parse_sequence(rest, indent, depth + 1, raw_lines)?

							_ => { node: null_node(line.number, value_column), input: rest }
						}
					} else if starts_block_scalar(parts.value) {
						parse_block_scalar(rest, raw_lines, parts.value, line.number, value_column, line.indent + 1)?
					} else {
						node = parse_inline_value(parts.value, line.number, value_column, depth + 1)?
						{ node, input: rest }
					}

				$seen = $seen.insert(key)
				$entries = $entries.append({ key, line: line.number, column: key_column, value: parsed.node })
				$input = parsed.input
			}
		}
	}

	Ok({ node: Node.{ line: location.line, column: location.column, kind: Map($entries) }, input: $input })
}

MappingParts : { key : Utf8.Bytes, value : Utf8.Bytes, value_column : U64 }

## The next entry of a block mapping at `indent`, or `Stop` where the mapping ends.
mapping_line : Input, U64 -> Try([Stop, Entry({ line : Line, parts : MappingParts })], [InvalidYaml(Yaml.Error)])
mapping_line = |input, indent| {
	match peek(input) {
		Err(_) => Ok(Stop)
		Ok(line) if line.tab => fail(line.number, line.indent + 1, tab_message)
		Ok(line) if line.indent < indent => Ok(Stop)
		Ok(line) if line.indent > indent =>
			fail(line.number, line.indent + 1, "unexpected indentation after a mapping value; multi-line plain scalars are not supported, so quote the value or use a block scalar (|)")

		Ok(line) if is_sequence_line(line.content) => Ok(Stop)
		Ok(line) =>
			match split_mapping_entry(line.content) {
				Ok(parts) => Ok(Entry({ line, parts }))
				Err(_) => Ok(Stop)
			}
	}
}

node_location : Input -> { line : U64, column : U64 }
node_location = |input| {
	match peek(input) {
		Ok(first) => { line: first.number, column: first.indent + 1 }
		Err(_) => { line: 1, column: 1 }
	}
}

parse_sequence : Input, U64, U64, List(Line) -> Try(NodeResult, [InvalidYaml(Yaml.Error)])
parse_sequence = |start, indent, depth, raw_lines| {
	location = node_location(start)
	var $input = start
	var $items = []
	var $done = False

	while !$done {
		match sequence_line($input, indent)? {
			Stop => {
				$done = True
			}

			Entry(line) => {
				rest = advance($input)
				payload = sequence_payload(line.content)
				# The entry's content starts after "-" and its separating spaces.
				payload_indent = line.indent + 1 + count_spaces(line.content.drop_first(1), 0)
				compact = !payload.is_empty() and (is_sequence_line(payload) or split_mapping_entry(payload).is_ok())

				# A tab before a compact collection would be part of its indentation.
				if compact and line.content.drop_first(1).take_first(payload_indent - line.indent).contains('\t') {
					return fail(line.number, line.indent + 2, tab_message)
				}

				parsed =
					if payload.is_empty() {
						match peek(rest) {
							Ok(next) if next.indent > indent => parse_node(rest, next.indent, depth + 1, raw_lines)?
							_ => { node: null_node(line.number, line.indent + 2), input: rest }
						}
					} else if compact {
						virtual = { content: payload, indent: payload_indent, number: line.number, terminated: line.terminated, tab: False }
						parse_node(replace_head($input, virtual), payload_indent, depth + 1, raw_lines)?
					} else if starts_block_scalar(payload) {
						parse_block_scalar(rest, raw_lines, payload, line.number, payload_indent + 1, line.indent + 1)?
					} else {
						node = parse_inline_value(payload, line.number, payload_indent + 1, depth + 1)?
						{ node, input: rest }
					}

				$items = $items.append(parsed.node)
				$input = parsed.input
			}
		}
	}

	Ok({ node: Node.{ line: location.line, column: location.column, kind: Seq($items) }, input: $input })
}

## The next entry of a block sequence at `indent`, or `Stop` where the sequence ends.
sequence_line : Input, U64 -> Try([Stop, Entry(Line)], [InvalidYaml(Yaml.Error)])
sequence_line = |input, indent| {
	match peek(input) {
		Err(_) => Ok(Stop)
		Ok(line) if line.tab => fail(line.number, line.indent + 1, tab_message)
		Ok(line) if line.indent < indent => Ok(Stop)
		Ok(line) if line.indent > indent => fail(line.number, line.indent + 1, "unexpected indentation after a sequence value")
		Ok(line) if !is_sequence_line(line.content) => Ok(Stop)
		Ok(line) => Ok(Entry(line))
	}
}

starts_block_scalar : Utf8.Bytes -> Bool
starts_block_scalar = |bytes| {
	match trim_spaces(bytes) {
		['|', ..] | ['>', ..] => True
		_ => False
	}
}

## Parse a block scalar whose header is on line `line`. Its content comes
## from the raw lines, which are numbered from one, so the line after the
## header is at index `line`.
parse_block_scalar : Input, List(Line), Utf8.Bytes, U64, U64, U64 -> Try(NodeResult, [InvalidYaml(Yaml.Error)])
parse_block_scalar = |rest, raw_lines, header_bytes, line, column, min_indent| {
	header = parse_block_header(header_bytes, line, column)?
	collected = collect_block_lines(raw_lines, line, min_indent, header.indent, line)?
	body = render_block_scalar(collected.lines, collected.terminated, header.style, header.chomp)

	var $input = rest
	while peek($input).map_ok(|next| next.number <= collected.consumed_through) == Ok(True) {
		$input = advance($input)
	}

	Ok({ node: Node.{ line, column, kind: Quoted(Str.from_utf8_lossy(body)) }, input: $input })
}

parse_block_header : Utf8.Bytes, U64, U64 -> Try(BlockHeader, [InvalidYaml(Yaml.Error)])
parse_block_header = |raw, line, column| {
	bytes = trim_spaces(raw)

	match bytes {
		['|', .. as rest] =>
			parse_block_header_options(rest, { style: LiteralBlock, chomp: ClipChomp, indent: AutoIndent }, line, column)

		['>', .. as rest] =>
			parse_block_header_options(rest, { style: FoldedBlock, chomp: ClipChomp, indent: AutoIndent }, line, column)

		_ => fail(line, column, "invalid block scalar header")
	}
}

parse_block_header_options : Utf8.Bytes, BlockHeader, U64, U64 -> Try(BlockHeader, [InvalidYaml(Yaml.Error)])
parse_block_header_options = |bytes, header, line, column| {
	match bytes {
		[] => Ok(header)

		['-', .. as rest] => {
			match header.chomp {
				ClipChomp =>
					parse_block_header_options(
						rest,
						{ style: header.style, chomp: StripChomp, indent: header.indent },
						line,
						column,
					)

				_ => fail(line, column, "duplicate block scalar chomping indicator")
			}
		}

		['+', .. as rest] => {
			match header.chomp {
				ClipChomp =>
					parse_block_header_options(
						rest,
						{ style: header.style, chomp: KeepChomp, indent: header.indent },
						line,
						column,
					)

				_ => fail(line, column, "duplicate block scalar chomping indicator")
			}
		}

		[digit, .. as rest] if digit >= '1' and digit <= '9' => {
			match header.indent {
				AutoIndent => {
					indent = block_indent_from_digit(digit)
					parse_block_header_options(rest, { style: header.style, chomp: header.chomp, indent: ExplicitIndent(indent) }, line, column)
				}

				ExplicitIndent(_) => fail(line, column, "duplicate block scalar indentation indicator")
			}
		}

		_ => fail(line, column, "unsupported block scalar header")
	}
}

block_indent_from_digit : U8 -> U64
block_indent_from_digit = |digit| {
	match digit {
		'1' => 1
		'2' => 2
		'3' => 3
		'4' => 4
		'5' => 5
		'6' => 6
		'7' => 7
		'8' => 8
		'9' => 9
		_ => 1
	}
}

## Collect a block scalar's lines without their content indentation (YAML 1.2
## 8.1.1), following the yaml-test-suite reference behaviour. Lines of spaces
## are empty (`[]`), keeping any spaces beyond the content indentation as
## text. Content must be indented at least `min_indent` spaces: one more than
## the parent node, or zero for a scalar at the document root. Lines are read
## from index `start` of `raw_lines` on. `terminated`
## says whether the last content line ended with a line break.
collect_block_lines : List(Line), U64, U64, BlockIndent, U64 -> Try({ lines : List(Utf8.Bytes), consumed_through : U64, terminated : Bool }, [InvalidYaml(Yaml.Error)])
collect_block_lines = |raw_lines, start, min_indent, block_indent, header_line| {
	var $content_indent =
		match block_indent {
			# Like libyaml and ruamel, a root indicator counts from column 0.
			ExplicitIndent(offset) => FixedIndent((if min_indent == 0 1 else min_indent) + offset - 1)
			AutoIndent => PendingIndent
		}
	var $lines = []
	var $consumed_through = header_line
	var $terminated = True
	var $leading_spaces = 0
	var $leading_line = header_line
	var $done = False
	var $index = start

	while !$done {
		match raw_lines.get($index) {
			Err(_) => {
				$done = True
			}

			Ok(line) => {
				spaces = count_spaces(line.content, 0)
				after_spaces = line.content.drop_first(spaces)
				white_only = trim_spaces(after_spaces).is_empty()

				if after_spaces.is_empty() {
					# An empty line, even a final one without a line break.
					text =
						match $content_indent {
							FixedIndent(width) if spaces > width => line.content.drop_first(width)
							_ => []
						}

					# Before the indentation is detected, remember the deepest empty line.
					if $content_indent == PendingIndent and spaces > $leading_spaces {
						$leading_spaces = spaces
						$leading_line = line.number
					}

					$lines = $lines.append(text)
					$consumed_through = line.number
					$terminated = True
					$index = $index + 1
				} else if spaces < min_indent or is_document_marker(line.content) {
					if white_only {
						# Only a tab can follow; it would be indentation.
						return fail(line.number, spaces + 1, "tabs may not be used for YAML indentation")
					}

					$done = True
				} else {
					required =
						match $content_indent {
							FixedIndent(width) => width
							PendingIndent => spaces
						}

					if spaces < required and white_only {
						$lines = $lines.append([])
						$consumed_through = line.number
						$terminated = True
						$index = $index + 1
					} else if spaces < required {
						$done = True
					} else {
						$content_indent = FixedIndent(required)
						$lines = $lines.append(line.content.drop_first(required))
						$consumed_through = line.number
						# Trailing white space at the end of input still ends its line.
						$terminated = line.terminated or white_only
						$index = $index + 1
					}
				}
			}
		}
	}

	match $content_indent {
		FixedIndent(width) if $leading_spaces > width and block_indent == AutoIndent =>
			fail($leading_line, width + 1, "leading empty lines of a block scalar must not be indented more than its content")

		_ => Ok({ lines: $lines, consumed_through: $consumed_through, terminated: $terminated })
	}
}

## "---" or "..." at the start of a line, alone or followed by white space.
is_document_marker : Utf8.Bytes -> Bool
is_document_marker = |bytes| {
	match bytes {
		['-', '-', '-'] | ['.', '.', '.'] => True
		['-', '-', '-', ' ', ..] | ['-', '-', '-', '\t', ..] | ['.', '.', '.', ' ', ..] | ['.', '.', '.', '\t', ..] => True
		_ => False
	}
}

count_spaces : Utf8.Bytes, U64 -> U64
count_spaces = |bytes, count| {
	match bytes {
		[' ', .. as rest] => count_spaces(rest, count + 1)
		_ => count
	}
}

## Apply the block style and chomping indicator (YAML 1.2 8.1.1.2). Content
## runs through the last non-empty line; later empty lines are trailing.
render_block_scalar : List(Utf8.Bytes), Bool, BlockStyle, BlockChomp -> Utf8.Bytes
render_block_scalar = |lines, terminated, style, chomp| {
	content_len = last_content_index(lines, 0, 0)
	content = lines.sublist({ start: 0, len: content_len })
	trailing = lines.len() - content_len

	if content_len == 0 {
		match chomp {
			KeepChomp => List.repeat('\n', trailing)
			_ => []
		}
	} else {
		body =
			match style {
				LiteralBlock => join_block_lines(content)
				FoldedBlock => fold_block_lines(content)
			}

		# The last content line has a break unless it ended the input.
		final_break = trailing > 0 or terminated

		match chomp {
			StripChomp => body
			ClipChomp => if final_break body.append('\n') else body
			KeepChomp => append_bytes(if final_break body.append('\n') else body, List.repeat('\n', trailing))
		}
	}
}

last_content_index : List(Utf8.Bytes), U64, U64 -> U64
last_content_index = |lines, index, last| {
	match lines.get(index) {
		Err(_) => last
		Ok(line) if line.is_empty() => last_content_index(lines, index + 1, last)
		Ok(_) => last_content_index(lines, index + 1, index + 1)
	}
}

join_block_lines : List(Utf8.Bytes) -> Utf8.Bytes
join_block_lines = |lines| {
	match lines {
		[] => []
		[first, .. as rest] => join_block_lines_help(rest, first)
	}
}

join_block_lines_help : List(Utf8.Bytes), Utf8.Bytes -> Utf8.Bytes
join_block_lines_help = |lines, out| {
	match lines {
		[] => out
		[line, .. as rest] => join_block_lines_help(rest, append_bytes(out.append('\n'), line))
	}
}

## Fold lines (YAML 1.2 8.1.3, 6.5): a break between two text lines that do not
## start with white space becomes a space, or is dropped when empty lines
## follow it; breaks next to more-indented lines are kept.
fold_block_lines : List(Utf8.Bytes) -> Utf8.Bytes
fold_block_lines = |lines| {
	var $out = []
	var $previous = Err(NoLine)
	var $empties = 0

	var $index = 0

	while $index < lines.len() {
		line = lines.get($index) ?? []
		$index = $index + 1

		if line.is_empty() {
			$empties = $empties + 1
		} else {
			separator =
				match $previous {
					Err(_) => List.repeat('\n', $empties)
					Ok(before) if !starts_with_space(before) and !starts_with_space(line) =>
						if $empties == 0 [' '] else List.repeat('\n', $empties)

					Ok(_) => List.repeat('\n', $empties + 1)
				}

			$out = append_bytes(append_bytes($out, separator), line)
			$previous = Ok(line)
			$empties = 0
		}
	}

	$out
}

parse_inline_value : Utf8.Bytes, U64, U64, U64 -> Try(Node, [InvalidYaml(Yaml.Error)])
parse_inline_value = |raw, line, column, depth| {
	bytes = trim_spaces(raw)

	match bytes {
		[] => Ok(null_node(line, column))

		['[', ..] | ['{', ..] if depth >= 100 => fail(line, column, nesting_message)

		['[', ..] => parse_flow_sequence(bytes, line, column, depth)

		['{', ..] => parse_flow_mapping(bytes, line, column, depth)

		['|', ..] | ['>', ..] =>
			fail(line, column, "block scalars are only supported as standalone mapping or sequence values")

		['&', ..] | ['*', ..] | ['!', ..] =>
			fail(line, column, "anchors, aliases, and tags are not supported by this YAML subset")

		['%', ..] =>
			fail(line, column, "YAML directives are not supported by this YAML subset")

		['?'] | ['?', ' ', ..] | ['?', '\t', ..] =>
			fail(line, column, "complex mapping keys are not supported by this YAML subset")

		[first, ..] if first == ']' or first == '}' or first == ',' or first == '@' or first == '`' =>
			fail(line, column, "a plain scalar cannot start with `${Str.from_utf8_lossy([first])}`")

		['-'] | ['-', ' ', ..] | ['-', '\t', ..] | [':'] | [':', ' ', ..] | [':', '\t', ..] =>
			fail(line, column, "a plain scalar cannot start with an indicator followed by white space")

		['"', ..] =>
			parse_double_quoted(bytes, line, column).map_ok(|text| Node.{ line, column, kind: Quoted(text) })

		['\'', ..] =>
			parse_single_quoted(bytes, line, column).map_ok(|text| Node.{ line, column, kind: Quoted(text) })

		_ if contains_mapping_indicator(bytes) =>
			fail(line, column, "a mapping value is not allowed here; quote the scalar if it contains \": \"")

		_ => Ok(Node.{ line, column, kind: Plain(Str.from_utf8_lossy(bytes)) })
	}
}

## ": " or a final ":" inside a plain scalar would start a mapping value.
contains_mapping_indicator : Utf8.Bytes -> Bool
contains_mapping_indicator = |bytes| {
	match bytes {
		[] => False
		[':'] => True
		[':', ' ', ..] | [':', '\t', ..] => True
		[_, .. as rest] => contains_mapping_indicator(rest)
	}
}

## Resolve a plain scalar with the YAML 1.2 core schema (10.3.2). Anything
## that matches none of its forms is a string.
resolve_plain : Utf8.Bytes, U64, U64 -> Try(Yaml, [InvalidYaml(Yaml.Error)])
resolve_plain = |bytes, line, column| {
	text = Str.from_utf8_lossy(bytes)

	if ["", "null", "Null", "NULL", "~"].contains(text) {
		Ok(Null)
	} else if ["true", "True", "TRUE"].contains(text) {
		Ok(Bool(True))
	} else if ["false", "False", "FALSE"].contains(text) {
		Ok(Bool(False))
	} else if is_decimal_integer(bytes) {
		match I64.from_str(text) {
			Ok(value) => Ok(Int(value))
			Err(_) => fail(line, column, "integer `${text}` is outside the supported I64 range")
		}
	} else {
		match bytes {
			['0', 'o', .. as digits] if !digits.is_empty() and digits.all(|b| b >= '0' and b <= '7') =>
				radix_integer(digits, 8, text, line, column)

			['0', 'x', .. as digits] if !digits.is_empty() and digits.all(is_hex_digit) =>
				radix_integer(digits, 16, text, line, column)

			_ =>
				match special_float(text) {
					Ok(value) => Ok(Float(value))
					Err(_) =>
						match core_float_text(bytes) {
							Ok(canonical) => Ok(Float(F64.from_str(canonical) ?? (if bytes.first() == Ok('-') -F64.infinity else F64.infinity)))
							Err(_) => Ok(Text(text))
						}
				}
		}
	}
}

special_float : Str -> Try(F64, [NotSpecial])
special_float = |text| {
	if [".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF"].contains(text) {
		Ok(F64.infinity)
	} else if ["-.inf", "-.Inf", "-.INF"].contains(text) {
		Ok(-F64.infinity)
	} else if [".nan", ".NaN", ".NAN"].contains(text) {
		Ok(F64.nan)
	} else {
		Err(NotSpecial)
	}
}

is_hex_digit : U8 -> Bool
is_hex_digit = |byte| is_digit(byte) or (byte >= 'a' and byte <= 'f') or (byte >= 'A' and byte <= 'F')

radix_integer : Utf8.Bytes, U64, Str, U64, U64 -> Try(Yaml, [InvalidYaml(Yaml.Error)])
radix_integer = |digits, radix, text, line, column| {
	value = digits.fold(
		Ok(0),
		|acc, byte| {
			digit =
				if is_digit(byte) {
					U8.to_u64(byte - '0')
				} else if byte >= 'a' {
					U8.to_u64(byte - 'a' + 10)
				} else {
					U8.to_u64(byte - 'A' + 10)
				}

			match acc {
				Ok(total) if total <= (9223372036854775807 - digit) // radix => Ok(total * radix + digit)
				_ => Err(Overflow)
			}
		},
	)

	match value {
		Ok(total) => Ok(Int(U64.to_i64_wrap(total)))
		Err(_) => fail(line, column, "integer `${text}` is outside the supported I64 range")
	}
}

## Match the core schema float form [-+]? ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? ) ( [eE] [-+]? [0-9]+ )?
## and spell it as sign, digits, ".", digits, exponent for F64.from_str.
core_float_text : Utf8.Bytes -> Try(Str, [NotFloat])
core_float_text = |bytes| {
	{ sign, unsigned } =
		match bytes {
			['-', .. as rest] => { sign: "-", unsigned: rest }
			['+', .. as rest] => { sign: "", unsigned: rest }
			_ => { sign: "", unsigned: bytes }
		}
	mantissa = unsigned.take_first(count_while(unsigned, |b| b != 'e' and b != 'E'))
	exponent = unsigned.drop_first(mantissa.len())
	whole = mantissa.take_first(count_while(mantissa, is_digit))
	after_whole = mantissa.drop_first(whole.len())
	{ has_point, fraction } =
		match after_whole {
			['.', .. as rest] => { has_point: True, fraction: rest }
			_ => { has_point: False, fraction: after_whole }
		}
	exponent_digits =
		match exponent {
			['e', '+', .. as rest] | ['e', '-', .. as rest] | ['E', '+', .. as rest] | ['E', '-', .. as rest] | ['e', .. as rest] | ['E', .. as rest] => rest
			_ => []
		}
	mantissa_ok = fraction.all(is_digit) and (!whole.is_empty() or (has_point and !fraction.is_empty()))
	exponent_ok = exponent.is_empty() or (!exponent_digits.is_empty() and exponent_digits.all(is_digit))
	# A plain integer is not a float; "1." and "1e3" are.
	is_float = has_point or !exponent.is_empty()

	if mantissa_ok and exponent_ok and is_float {
		whole_text = if whole.is_empty() "0" else Str.from_utf8_lossy(whole)
		fraction_text = if fraction.is_empty() "0" else Str.from_utf8_lossy(fraction)
		exponent_text = if exponent.is_empty() "" else Str.from_utf8_lossy(exponent)
		Ok("${sign}${whole_text}.${fraction_text}${exponent_text}")
	} else {
		Err(NotFloat)
	}
}

count_while : Utf8.Bytes, (U8 -> Bool) -> U64
count_while = |bytes, keep| {
	var $count = 0
	while $count < bytes.len() and keep(bytes.get($count) ?? 0) {
		$count = $count + 1
	}
	$count
}

parse_flow_sequence : Utf8.Bytes, U64, U64, U64 -> Try(Node, [InvalidYaml(Yaml.Error)])
parse_flow_sequence = |bytes, line, column, depth| {
	inner = unwrap_flow(bytes, '[', ']', line, column)?
	parts = if trim_spaces(inner).is_empty() [] else split_flow_items(inner, line, column)?
	var $items = List.with_capacity(parts.len())

	for part in parts {
		$items = $items.append(parse_inline_value(part, line, column, depth + 1)?)
	}

	Ok(Node.{ line, column, kind: Seq($items) })
}

parse_flow_mapping : Utf8.Bytes, U64, U64, U64 -> Try(Node, [InvalidYaml(Yaml.Error)])
parse_flow_mapping = |bytes, line, column, depth| {
	inner = unwrap_flow(bytes, '{', '}', line, column)?
	parts = if trim_spaces(inner).is_empty() [] else split_flow_items(inner, line, column)?
	var $entries = List.with_capacity(parts.len())
	var $seen = Set.empty()

	for part in parts {
		split =
			match split_mapping_entry(trim_spaces(part)) {
				Ok(found) => found
				Err(_) => return fail(line, column, "expected a key and value in flow mapping")
			}
		key = parse_key(split.key, line, column)?

		if $seen.contains(key) {
			return fail(line, column, "duplicate mapping key `${key}`")
		}

		value = parse_inline_value(split.value, line, column, depth + 1)?
		$seen = $seen.insert(key)
		$entries = $entries.append({ key, line, column, value })
	}

	Ok(Node.{ line, column, kind: Map($entries) })
}

parse_key : Utf8.Bytes, U64, U64 -> Try(Str, [InvalidYaml(Yaml.Error)])
parse_key = |raw, line, column| {
	bytes = trim_spaces(raw)

	match bytes {
		[] => fail(line, column, "mapping keys must not be empty")
		['"', ..] => parse_double_quoted(bytes, line, column)
		['\'', ..] => parse_single_quoted(bytes, line, column)
		['[', ..] | ['{', ..] | ['?'] | ['?', ' ', ..] | ['?', '\t', ..] => fail(line, column, "complex mapping keys are not supported by this YAML subset")
		['&', ..] | ['*', ..] | ['!', ..] => fail(line, column, "anchors, aliases, and tags are not supported by this YAML subset")
		# Like a value, and like a first line before any `---`, `%` reads as a directive.
		['%', ..] => fail(line, column, "YAML directives are not supported by this YAML subset")
		[first, ..] if first == '|' or first == '>' or first == '@' or first == '`' or first == ']' or first == '}' or first == ',' =>
			fail(line, column, "a plain mapping key cannot start with `${Str.from_utf8_lossy([first])}`")
		_ => Ok(Str.from_utf8_lossy(bytes))
	}
}

parse_single_quoted : Utf8.Bytes, U64, U64 -> Try(Str, [InvalidYaml(Yaml.Error)])
parse_single_quoted = |bytes, line, column| {
	inner = quoted_inner(bytes, '\'', line, column)?
	unescape_single(inner, [], line, column).map_ok(Str.from_utf8_lossy)
}

## The text between a scalar's opening quote and its real closing quote, which
## must end the scalar. In single quotes '' is an escaped quote; in double
## quotes a backslash escapes the next byte.
quoted_inner : Utf8.Bytes, U8, U64, U64 -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
quoted_inner = |bytes, quote, line, column| {
	var $index = 1
	var $close = Err(Unterminated)

	while $index < bytes.len() and $close == Err(Unterminated) {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0

		if quote == '"' and byte == '\\' {
			$index = $index + 2
		} else if quote == '\'' and byte == '\'' and next == '\'' {
			$index = $index + 2
		} else if byte == quote {
			$close = Ok($index)
		} else {
			$index = $index + 1
		}
	}

	kind = if quote == '"' "double" else "single"

	match $close {
		Err(_) => fail(line, column, "unterminated ${kind}-quoted string")
		Ok(close) if close + 1 != bytes.len() => fail(line, column, "unexpected text after the closing quote of a ${kind}-quoted string")
		Ok(close) => Ok(bytes.sublist({ start: 1, len: close - 1 }))
	}
}

unescape_single : Utf8.Bytes, Utf8.Bytes, U64, U64 -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
unescape_single = |bytes, out, line, column| {
	match bytes {
		[] => Ok(out)
		['\'', '\'', .. as rest] => unescape_single(rest, out.append('\''), line, column)
		['\'', ..] => fail(line, column, "a single quote inside a quoted string must be doubled")
		[first, .. as rest] => unescape_single(rest, out.append(first), line, column)
	}
}

parse_double_quoted : Utf8.Bytes, U64, U64 -> Try(Str, [InvalidYaml(Yaml.Error)])
parse_double_quoted = |bytes, line, column| {
	inner = quoted_inner(bytes, '"', line, column)?
	unescape_double(inner, [], line, column).map_ok(Str.from_utf8_lossy)
}

unescape_double : Utf8.Bytes, Utf8.Bytes, U64, U64 -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
unescape_double = |bytes, out, line, column| {
	match bytes {
		[] => Ok(out)
		['\\', escaped, .. as rest] => {
			match simple_escape(escaped) {
				Ok(decoded) => unescape_double(rest, append_bytes(out, decoded), line, column)
				Err(_) => {
					digits =
						match escaped {
							'x' => 2
							'u' => 4
							'U' => 8
							_ => 0
						}

					if digits == 0 {
						fail(line, column, "unsupported escape sequence `\\${escaped_character(bytes.drop_first(1))}`")
					} else {
						encoded = encode_code_point(rest.sublist({ start: 0, len: digits }), digits, line, column)?
						unescape_double(rest.drop_first(digits), append_bytes(out, encoded), line, column)
					}
				}
			}
		}

		['\\'] => fail(line, column, "unterminated escape sequence")
		[first, .. as rest] => unescape_double(rest, out.append(first), line, column)
	}
}

## YAML 1.2 single-character escapes (5.7), as UTF-8.
simple_escape : U8 -> Try(Utf8.Bytes, [NotSimple])
simple_escape = |escaped| {
	match escaped {
		'0' => Ok([0])
		'a' => Ok([7])
		'b' => Ok([8])
		't' | '\t' => Ok(['\t'])
		'n' => Ok(['\n'])
		'v' => Ok([11])
		'f' => Ok([12])
		'r' => Ok(['\r'])
		'e' => Ok([27])
		' ' => Ok([' '])
		'"' => Ok(['"'])
		'/' => Ok(['/'])
		'\\' => Ok(['\\'])
		'N' => Ok([0xC2, 0x85])
		'_' => Ok([0xC2, 0xA0])
		'L' => Ok([0xE2, 0x80, 0xA8])
		'P' => Ok([0xE2, 0x80, 0xA9])
		_ => Err(NotSimple)
	}
}

## The whole (possibly multi-byte) character at the start of `bytes`.
escaped_character : Utf8.Bytes -> Str
escaped_character = |bytes| {
	width =
		match bytes {
			[first, ..] if first >= 0xF0 => 4
			[first, ..] if first >= 0xE0 => 3
			[first, ..] if first >= 0xC0 => 2
			_ => 1
		}

	Str.from_utf8(bytes.sublist({ start: 0, len: width })) ?? "?"
}

encode_code_point : Utf8.Bytes, U64, U64, U64 -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
encode_code_point = |hex, digits, line, column| {
	if hex.len() != digits {
		fail(line, column, "escape sequence needs ${digits.to_str()} hexadecimal digits")
	} else {
		match parse_hex(hex, 0) {
			Err(_) => fail(line, column, "escape sequence needs ${digits.to_str()} hexadecimal digits")
			Ok(code) if code >= 0xD800 and code <= 0xDFFF => fail(line, column, "escape sequence is a UTF-16 surrogate, not a character")
			Ok(code) if code > 0x10FFFF => fail(line, column, "escape sequence is beyond the last Unicode character")
			Ok(code) => Ok(utf8_encode(code))
		}
	}
}

parse_hex : Utf8.Bytes, U32 -> Try(U32, [InvalidHex])
parse_hex = |bytes, value| {
	match bytes {
		[] => Ok(value)
		[first, .. as rest] => {
			digit =
				if first >= '0' and first <= '9' {
					Ok(first - '0')
				} else if first >= 'a' and first <= 'f' {
					Ok(first - 'a' + 10)
				} else if first >= 'A' and first <= 'F' {
					Ok(first - 'A' + 10)
				} else {
					Err(InvalidHex)
				}

			parse_hex(rest, value * 16 + U8.to_u32(digit?))
		}
	}
}

utf8_encode : U32 -> Utf8.Bytes
utf8_encode = |code| {
	if code < 0x80 {
		[U32.to_u8_wrap(code)]
	} else if code < 0x800 {
		[U32.to_u8_wrap(0xC0 + code // 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else if code < 0x10000 {
		[U32.to_u8_wrap(0xE0 + code // 4096), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else {
		[U32.to_u8_wrap(0xF0 + code // 262144), U32.to_u8_wrap(0x80 + (code // 4096) % 64), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
	}
}

prepare_lines : List(Line) -> Try(List(Line), [InvalidYaml(Yaml.Error)])
prepare_lines = |raw_lines| {
	clean = clean_lines(raw_lines, [])?

	without_start =
		match clean {
			[first, ..] if first.indent == 0 and first.content.first() == Ok('%') =>
				return fail(first.number, 1, "YAML directives are not supported by this YAML subset")

			[first, .. as rest] if first.indent == 0 and first.content == "---".to_utf8() => rest
			[first, .. as rest] if first.indent == 0 and is_document_marker(first.content) and Str.starts_with(Str.from_utf8_lossy(first.content), "---") => {
				# "--- node": the root node starts on the marker line (YAML 1.2 9.1.4).
				node = trim_spaces(first.content.drop_first(3))
				column = first.content.len() - node.len() + 1
				if is_sequence_line(node) or split_mapping_entry(node).is_ok() {
					return fail(first.number, column, "a block collection cannot start on the document start line")
				}
				List.prepend(rest, { content: node, indent: 0, number: first.number, terminated: first.terminated, tab: False })
			}

			_ => clean
		}

	# A "%" line right after "---" cannot be content either; report it the
	# same way, before any later document markers.
	match without_start {
		[first, ..] if first.indent == 0 and first.content.first() == Ok('%') =>
			return fail(first.number, 1, "YAML directives are not supported by this YAML subset")

		_ => {}
	}

	remove_document_end(without_start, [])
}

clean_lines : List(Line), List(Line) -> Try(List(Line), [InvalidYaml(Yaml.Error)])
clean_lines = |lines, out| {
	match lines {
		[] => Ok(out)

		[line, .. as rest] => {
			indent = count_spaces(line.content, 0)
			after_indent = line.content.drop_first(indent)
			content = trim_end_spaces(strip_comment(after_indent))
			tab = after_indent.first() == Ok('\t')

			if content.is_empty() {
				clean_lines(rest, out)
			} else {
				clean_lines(rest, out.append({ content, indent, number: line.number, terminated: line.terminated, tab }))
			}
		}
	}
}

remove_document_end : List(Line), List(Line) -> Try(List(Line), [InvalidYaml(Yaml.Error)])
remove_document_end = |lines, out| {
	match lines {
		[] => Ok(out)

		[line, .. as rest] if line.indent == 0 and line.content == "...".to_utf8() => {
			match rest {
				[] => Ok(out)
				[next, ..] => fail(next.number, next.indent + 1, "multiple YAML documents are not supported")
			}
		}

		[line, ..] if line.indent == 0 and line.content == "---".to_utf8() =>
			fail(line.number, 1, "multiple YAML documents are not supported")

		[line, .. as rest] => remove_document_end(rest, out.append(line))
	}
}

## The lines of `input`, each a slice of it rather than a copy.
split_lines : Utf8.Bytes, U64 -> List(Line)
split_lines = |input, first_number| {
	var $lines = []
	var $number = first_number
	var $start = 0
	var $index = 0
	while $index < input.len() {
		byte = input.get($index) ?? 0
		if byte == '\n' or byte == '\r' {
			$lines = $lines.append({ content: input.sublist({ start: $start, len: $index - $start }), indent: 0, number: $number, terminated: True, tab: False })
			$number = $number + 1
			$index = if byte == '\r' and input.get($index + 1) == Ok('\n') $index + 2 else $index + 1
			$start = $index
		} else {
			$index = $index + 1
		}
	}
	# A line break ends a line; it does not start an empty final one.
	if $start == input.len() and !$lines.is_empty() {
		$lines
	} else {
		$lines.append({ content: input.sublist({ start: $start, len: input.len() - $start }), indent: 0, number: $number, terminated: False, tab: False })
	}
}

## `bytes` up to the comment that ends it, if any, as a slice of `bytes`.
## Quotes and flow brackets are tracked so a `#` inside them is kept, and a
## `#` only starts a comment at the start or after white space.
strip_comment : Utf8.Bytes -> Utf8.Bytes
strip_comment = |bytes| {
	var $quote = NoQuote
	var $escaped = False
	var $separated = True
	var $depth = 0
	var $index = 0
	var $end = bytes.len()
	while $index < $end {
		first = bytes.get($index) ?? 0
		was_escaped = $escaped
		was_separated = $separated
		$escaped = False
		$separated = False
		if first == '#' and $quote == NoQuote and was_separated {
			$end = $index
		} else if first == '\\' and $quote == DoubleQuote and !was_escaped {
			$escaped = True
			$index = $index + 1
		} else if first == '"' and $quote == NoQuote and scalar_can_start(bytes.sublist({ start: 0, len: $index }), $depth > 0) {
			$quote = DoubleQuote
			$index = $index + 1
		} else if first == '"' and $quote == DoubleQuote and !was_escaped {
			$quote = NoQuote
			$index = $index + 1
		} else if first == '\'' and $quote == NoQuote and scalar_can_start(bytes.sublist({ start: 0, len: $index }), $depth > 0) {
			$quote = SingleQuote
			$index = $index + 1
		} else if first == '\'' and $quote == SingleQuote and bytes.get($index + 1) == Ok('\'') {
			$index = $index + 2
		} else if first == '\'' and $quote == SingleQuote {
			$quote = NoQuote
			$index = $index + 1
		} else if (first == '[' or first == '{') and $quote == NoQuote and ($depth > 0 or scalar_can_start(bytes.sublist({ start: 0, len: $index }), False)) {
			$depth = $depth + 1
			$index = $index + 1
		} else if (first == ']' or first == '}') and $quote == NoQuote and $depth > 0 {
			$depth = $depth - 1
			$index = $index + 1
		} else {
			$separated = first == ' ' or first == '\t'
			$index = $index + 1
		}
	}
	bytes.sublist({ start: 0, len: $end })
}

## Whether a scalar or flow collection may start after `prefix`: only white
## space and standalone "-" or "?" indicators may separate it from the start of
## the line, a ": " value indicator, or (in a flow collection) "[", "{" or ",".
## Anywhere else, quotes and brackets are ordinary plain scalar text.
scalar_can_start : Utf8.Bytes, Bool -> Bool
scalar_can_start = |prefix, in_flow| {
	var $index = prefix.len()
	var $answer = Err(Undecided)

	while $answer == Err(Undecided) {
		end = $index
		while $index > 0 and is_white(prefix.get($index - 1) ?? 'x') {
			$index = $index - 1
		}
		separated = $index < end

		if $index == 0 {
			$answer = Ok(True)
		} else {
			previous = prefix.get($index - 1) ?? 'x'
			standalone = $index == 1 or is_white(prefix.get($index - 2) ?? 'x')

			if in_flow and (previous == '[' or previous == '{' or previous == ',') {
				$answer = Ok(True)
			} else if previous == ':' and separated {
				$answer = Ok(True)
			} else if (previous == '-' or previous == '?') and separated and standalone {
				$index = $index - 1
			} else {
				$answer = Ok(False)
			}
		}
	}

	$answer ?? False
}

is_white : U8 -> Bool
is_white = |byte| byte == ' ' or byte == '\t'

split_mapping_entry : Utf8.Bytes -> Try({ key : Utf8.Bytes, value : Utf8.Bytes, value_column : U64 }, [NotFound])
split_mapping_entry = |bytes| {
	find_mapping_colon(bytes, bytes, NoQuote, False, 0, 0, 0)
}

find_mapping_colon : Utf8.Bytes, Utf8.Bytes, Quote, Bool, U64, U64, U64 -> Try({ key : Utf8.Bytes, value : Utf8.Bytes, value_column : U64 }, [NotFound])
find_mapping_colon = |all, bytes, quote, escaped, square_depth, curly_depth, index| {
	in_flow = square_depth > 0 or curly_depth > 0
	can_start = |_| scalar_can_start(all.sublist({ start: 0, len: index }), in_flow)

	match bytes {
		[] => Err(NotFound)

		[':', .. as rest] if quote == NoQuote and !in_flow and (rest.is_empty() or starts_with_space(rest)) =>
			Ok({ key: all.sublist({ start: 0, len: index }), value: trim_start_spaces(rest), value_column: index + 2 + rest.len() - trim_start_spaces(rest).len() })

		['\\', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, quote, True, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == NoQuote and can_start({}) =>
			find_mapping_colon(all, rest, DoubleQuote, False, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, NoQuote, False, square_depth, curly_depth, index + 1)

		['\'', .. as rest] if quote == NoQuote and can_start({}) =>
			find_mapping_colon(all, rest, SingleQuote, False, square_depth, curly_depth, index + 1)

		['\'', '\'', .. as rest] if quote == SingleQuote =>
			find_mapping_colon(all, rest, quote, False, square_depth, curly_depth, index + 2)

		['\'', .. as rest] if quote == SingleQuote =>
			find_mapping_colon(all, rest, NoQuote, False, square_depth, curly_depth, index + 1)

		['[', .. as rest] if quote == NoQuote and (in_flow or can_start({})) => find_mapping_colon(all, rest, quote, False, square_depth + 1, curly_depth, index + 1)
		[']', .. as rest] if quote == NoQuote and square_depth > 0 => find_mapping_colon(all, rest, quote, False, square_depth - 1, curly_depth, index + 1)
		['{', .. as rest] if quote == NoQuote and (in_flow or can_start({})) => find_mapping_colon(all, rest, quote, False, square_depth, curly_depth + 1, index + 1)
		['}', .. as rest] if quote == NoQuote and curly_depth > 0 => find_mapping_colon(all, rest, quote, False, square_depth, curly_depth - 1, index + 1)

		[_, .. as rest] => find_mapping_colon(all, rest, quote, False, square_depth, curly_depth, index + 1)
	}
}

split_flow_items : Utf8.Bytes, U64, U64 -> Try(List(Utf8.Bytes), [InvalidYaml(Yaml.Error)])
split_flow_items = |bytes, line, column| {
	split_flow_items_help(bytes, [], [], NoQuote, False, 0, 0, line, column)
}

split_flow_items_help : Utf8.Bytes, Utf8.Bytes, List(Utf8.Bytes), Quote, Bool, U64, U64, U64, U64 -> Try(List(Utf8.Bytes), [InvalidYaml(Yaml.Error)])
split_flow_items_help = |bytes, current, items, quote, escaped, square_depth, curly_depth, line, column| {
	match bytes {
		[] if quote != NoQuote => fail(line, column, "unterminated quoted string in flow collection")
		[] if square_depth != 0 or curly_depth != 0 => fail(line, column, "unterminated nested flow collection")
		[] if trim_spaces(current).is_empty() => fail(line, column, "flow collections may not contain an empty item")
		[] => Ok(items.append(trim_spaces(current)))

		[',', .. as rest] if quote == NoQuote and square_depth == 0 and curly_depth == 0 => {
			if trim_spaces(current).is_empty() {
				fail(line, column, "flow collections may not contain an empty item")
			} else {
				split_flow_items_help(rest, [], items.append(trim_spaces(current)), quote, False, square_depth, curly_depth, line, column)
			}
		}

		['\\', .. as rest] if quote == DoubleQuote and !escaped => split_flow_items_help(rest, current.append('\\'), items, quote, True, square_depth, curly_depth, line, column)
		['"', .. as rest] if quote == NoQuote and scalar_can_start(current, True) => split_flow_items_help(rest, current.append('"'), items, DoubleQuote, False, square_depth, curly_depth, line, column)
		['"', .. as rest] if quote == DoubleQuote and !escaped => split_flow_items_help(rest, current.append('"'), items, NoQuote, False, square_depth, curly_depth, line, column)
		['\'', .. as rest] if quote == NoQuote and scalar_can_start(current, True) => split_flow_items_help(rest, current.append('\''), items, SingleQuote, False, square_depth, curly_depth, line, column)
		['\'', '\'', .. as rest] if quote == SingleQuote => split_flow_items_help(rest, current.concat(['\'', '\'']), items, quote, False, square_depth, curly_depth, line, column)
		['\'', .. as rest] if quote == SingleQuote => split_flow_items_help(rest, current.append('\''), items, NoQuote, False, square_depth, curly_depth, line, column)
		['[', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('['), items, quote, False, square_depth + 1, curly_depth, line, column)
		[']', ..] if quote == NoQuote and square_depth == 0 => fail(line, column, "unexpected closing bracket in flow collection")
		[']', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append(']'), items, quote, False, square_depth - 1, curly_depth, line, column)
		['{', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('{'), items, quote, False, square_depth, curly_depth + 1, line, column)
		['}', ..] if quote == NoQuote and curly_depth == 0 => fail(line, column, "unexpected closing brace in flow collection")
		['}', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('}'), items, quote, False, square_depth, curly_depth - 1, line, column)
		[first, .. as rest] => split_flow_items_help(rest, current.append(first), items, quote, False, square_depth, curly_depth, line, column)
	}
}

unwrap_flow : Utf8.Bytes, U8, U8, U64, U64 -> Try(Utf8.Bytes, [InvalidYaml(Yaml.Error)])
unwrap_flow = |bytes, open, close, line, column| {
	if bytes.len() < 2 or bytes.get(0) != Ok(open) or bytes.get(bytes.len() - 1) != Ok(close) {
		fail(line, column, "unterminated flow collection")
	} else {
		Ok(bytes.sublist({ start: 1, len: bytes.len() - 2 }))
	}
}

is_sequence_line : Utf8.Bytes -> Bool
is_sequence_line = |bytes| {
	match bytes {
		['-'] => True
		['-', ' ', ..] | ['-', '\t', ..] => True
		_ => False
	}
}

sequence_payload : Utf8.Bytes -> Utf8.Bytes
sequence_payload = |bytes| {
	match bytes {
		['-'] => []
		['-', ' ', .. as rest] | ['-', '\t', .. as rest] => trim_spaces(rest)
		_ => bytes
	}
}

is_decimal_integer : Utf8.Bytes -> Bool
is_decimal_integer = |bytes| {
	match bytes {
		['+', .. as rest] | ['-', .. as rest] => !rest.is_empty() and all_digits(rest)
		_ => !bytes.is_empty() and all_digits(bytes)
	}
}

all_digits : Utf8.Bytes -> Bool
all_digits = |bytes| {
	match bytes {
		[] => True
		[first, .. as rest] => first >= '0' and first <= '9' and all_digits(rest)
	}
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

starts_with_space : Utf8.Bytes -> Bool
starts_with_space = |bytes| {
	match bytes {
		[' ', ..] | ['\t', ..] => True
		_ => False
	}
}

trim_spaces : Utf8.Bytes -> Utf8.Bytes
trim_spaces = |bytes| trim_end_spaces(trim_start_spaces(bytes))

trim_start_spaces : Utf8.Bytes -> Utf8.Bytes
trim_start_spaces = |bytes| {
	match bytes {
		[' ', .. as rest] | ['\t', .. as rest] => trim_start_spaces(rest)
		_ => bytes
	}
}

trim_end_spaces : Utf8.Bytes -> Utf8.Bytes
trim_end_spaces = |bytes| {
	var $len = bytes.len()
	while $len > 0 and is_white(bytes.get($len - 1) ?? 'x') {
		$len = $len - 1
	}
	bytes.take_first($len)
}

append_bytes : Utf8.Bytes, Utf8.Bytes -> Utf8.Bytes
append_bytes = |left, right| left.concat(right)

fail : U64, U64, Str -> Try(_, [InvalidYaml(Yaml.Error)])
fail = |line, column, message| Err(InvalidYaml({ line, column, message }))

when_error : Try(Yaml, [InvalidYaml(Yaml.Error)]) -> Str
when_error = |result| {
	match result {
		Ok(_) => ""
		Err(InvalidYaml(error)) => error.message
	}
}

inspect_yaml : Yaml -> Str
inspect_yaml = |value| {
	match value {
		Null => "Null"
		Bool(boolean) => if boolean "Bool(True)" else "Bool(False)"
		Int(integer) => "Int(${integer.to_str()})"
		Float(float) => "Float(${float.to_str()})"
		Text(text) => "Text(${inspect_text(text)})"
		Sequence(values) => "Sequence([${Str.join_with(values.map(inspect_yaml), ", ")}])"
		Mapping(entries) => "Mapping([${Str.join_with(entries.map(inspect_entry), ", ")}])"
	}
}

inspect_entry : { key : Str, value : Yaml } -> Str
inspect_entry = |entry| "{ key: ${inspect_text(entry.key)}, value: ${inspect_yaml(entry.value)} }"

## A Roc string literal for `text`, escaping quotes, backslashes, `$` and
## every C0, DEL and C1 control character, so the result is one printable line.
inspect_text : Str -> Str
inspect_text = |text| {
	bytes = text.to_utf8()
	var $out = List.with_capacity(bytes.len() + 2).append('"')
	var $index = 0

	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0

		if byte == 0xC2 and next >= 0x80 and next <= 0x9F {
			$out = $out.concat(code_point_escape(U8.to_u32(next)))
			$index = $index + 2
		} else {
			escaped =
				match byte {
					'"' => ['\\', '"']
					'\\' => ['\\', '\\']
					'$' => ['\\', '$']
					'\n' => ['\\', 'n']
					'\r' => ['\\', 'r']
					'\t' => ['\\', 't']
					_ if byte < 0x20 or byte == 0x7F => code_point_escape(U8.to_u32(byte))
					_ => [byte]
				}
			$out = $out.concat(escaped)
			$index = $index + 1
		}
	}

	Str.from_utf8_lossy($out.append('"'))
}

code_point_escape : U32 -> Utf8.Bytes
code_point_escape = |code| {
	hex = |digit| if digit < 10 U32.to_u8_wrap(digit) + '0' else U32.to_u8_wrap(digit) - 10 + 'a'
	['\\', 'u', '(', hex(code // 16), hex(code % 16), ')']
}

# Empty documents parse as null.
expect Yaml.parse_str("") == Ok(Null)

# Common frontmatter scalars resolve to useful values.
expect {
	actual =
		Yaml.parse_str(
			\\title: A small article
			\\draft: false
			\\count: 3
			\\rating: 4.5
			\\description: null
			,
		)?

	actual
		== Mapping([
			{ key: "title", value: Text("A small article") },
			{ key: "draft", value: Bool(False) },
			{ key: "count", value: Int(3) },
			{ key: "rating", value: Float(4.5) },
			{ key: "description", value: Null },
		])
}

# Nested mappings and sequences parse by indentation.
expect {
	actual =
		Yaml.parse_str(
			\\site:
			\\  title: Roc
			\\  tags:
			\\    - parser
			\\    - yaml
			,
		)?

	actual
		== Mapping([
			{
				key: "site",
				value: Mapping([
					{ key: "title", value: Text("Roc") },
					{ key: "tags", value: Sequence([Text("parser"), Text("yaml")]) },
				]),
			},
		])
}

# Sequence items may be compact mappings.
expect {
	actual =
		Yaml.parse_str(
			\\people:
			\\  - name: Ada
			\\    active: true
			\\  - name: Grace
			,
		)?

	actual
		== Mapping([
			{
				key: "people",
				value: Sequence([
					Mapping([{ key: "name", value: Text("Ada") }, { key: "active", value: Bool(True) }]),
					Mapping([{ key: "name", value: Text("Grace") }]),
				]),
			},
		])
}

# Flow collections support concise config values.
expect {
	actual = Yaml.parse_str("ports: [80, 443]\nlabels: { tier: web, public: true }")?

	actual
		== Mapping([
			{ key: "ports", value: Sequence([Int(80), Int(443)]) },
			{ key: "labels", value: Mapping([{ key: "tier", value: Text("web") }, { key: "public", value: Bool(True) }]) },
		])
}

# Quotes preserve scalar strings and comment markers.
expect {
	actual = Yaml.parse_str("enabled: \"true\"\nmessage: 'it''s # text' # comment")?
	actual == Mapping([{ key: "enabled", value: Text("true") }, { key: "message", value: Text("it's # text") }])
}

# Optional document markers work for Markdown frontmatter bodies.
expect {
	actual = Yaml.parse_str("---\ntitle: Post\n...")?
	actual == Mapping([{ key: "title", value: Text("Post") }])
}

# Duplicate mapping keys fail.
expect Yaml.parse_str("name: first\nname: second").is_err()

# Tabs used for indentation fail.
expect Yaml.parse_str("root:\n\tchild: value").is_err()

# Advanced YAML features fail explicitly.
expect Yaml.parse_str("value: &anchor text").is_err()

# Multiple documents are outside the supported subset.
expect Yaml.parse_str("one: 1\n---\ntwo: 2").is_err()

# Plain strings containing dots or the letter e are not mistaken for floats.
expect {
	actual = Yaml.parse_str("file: .git\nname: release")?
	actual == Mapping([{ key: "file", value: Text(".git") }, { key: "name", value: Text("release") }])
}

# Plain scalars resolve with the YAML 1.2 core schema; everything else is a string.
expect {
	actual = Yaml.parse_str("[1.2.3, 1e, nUlL, tRUE, 0x1F, 0o17, 1., .5, -1.5e2, .inf, -.Inf, 1_000, 0X1]")?
	actual
		== Sequence([
			Text("1.2.3"),
			Text("1e"),
			Text("nUlL"),
			Text("tRUE"),
			Int(31),
			Int(15),
			Float(1.0),
			Float(0.5),
			Float(-150.0),
			Float(F64.infinity),
			Float(-F64.infinity),
			Text("1_000"),
			Text("0X1"),
		])
}

# Not-a-number resolves to a float.
expect {
	match Yaml.parse_str(".nan") {
		Ok(Float(value)) => F64.is_nan(value)
		_ => False
	}
}

# Sequence entries may be compact nested sequences.
expect {
	actual = Yaml.parse_str("- - a\n  - b\n- - - c\n")?
	actual == Sequence([Sequence([Text("a"), Text("b")]), Sequence([Sequence([Text("c")])])])
}

# Mapping values may be sequences at the key's own indentation.
expect {
	actual = Yaml.parse_str("steps:\n- run: a\n  name: x\n- b\nnext: 1\n")?
	actual
		== Mapping([
			{ key: "steps", value: Sequence([Mapping([{ key: "run", value: Text("a") }, { key: "name", value: Text("x") }]), Text("b")]) },
			{ key: "next", value: Int(1) },
		])
}

# A byte order mark is not content; control characters are rejected.
expect {
	actual = Yaml.parse_str("\u(FEFF)value: one\n")?
	actual == Mapping([{ key: "value", value: Text("one") }]) and Yaml.parse_str("value: a\u(0)b").is_err() and Yaml.parse_str("v: \u(85)").is_ok()
}

# The root node may start on the document start line, but not a block collection.
expect {
	actual = Yaml.parse_str("--- |1-\n x\n")?
	plain = Yaml.parse_str("---\tscalar\n")?
	actual == Text("x") and plain == Text("scalar") and Yaml.parse_str("--- a: b\n").is_err()
}

# Indicators cannot start plain scalars, and plain scalars cannot hold ": ".
expect {
	invalid = ["value: ]", "[-]", "[-, -]", "- [ : empty key ]", "a: b: c: d", "&a: key", "a: -"]
	valid = Yaml.parse_str("a: ?x\n?y: -z\n")?
	invalid.all(|text| Yaml.parse_str(text).is_err()) and valid == Mapping([{ key: "a", value: Text("?x") }, { key: "?y", value: Text("-z") }])
}

# A tab may separate a sequence dash from a scalar, but not from a nested collection.
expect {
	actual = Yaml.parse_str("-\t-1\n")?
	actual == Sequence([Int(-1)]) and Yaml.parse_str("-\t-\n").is_err() and Yaml.parse_str("- \t-\n").is_err()
}

# Flow collections count toward the nesting limit.
expect {
	deep = Str.concat(Str.repeat("[", 150), Str.repeat("]", 150))
	shallow = Str.concat(Str.repeat("[", 50), Str.repeat("]", 50))
	Yaml.parse_str(deep).is_err() and Yaml.parse_str(shallow).is_ok() and Yaml.parse_str(Str.concat("a:\n  b: ", deep)).is_err()
}

# A quoted scalar ends at its real closing quote; anything after it is an error.
expect {
	stray = Yaml.parse_str("v: \"a\\\\\"\"")
	trailing = Yaml.parse_str("v: \"a\" x")
	single = Yaml.parse_str("v: 'it''s'")?
	message = |result| when_error(result)
	stray.is_err() and message(trailing) == "unexpected text after the closing quote of a double-quoted string" and single == Mapping([{ key: "v", value: Text("it's") }])
}

# Errors name the unsupported feature rather than a symptom.
expect {
	directive = when_error(Yaml.parse_str("%YAML 1.2\n---\na: 1\n"))
	continued = when_error(Yaml.parse_str("a: one\n  two\n"))
	directive == "YAML directives are not supported by this YAML subset" and Str.contains(continued, "multi-line plain scalars")
}

# Syntax errors report their source location.
expect {
	match Yaml.parse_str("root:\n\tchild: value") {
		Err(InvalidYaml(problem)) => problem.line == 2 and problem.column == 1
		Ok(_) => False
	}
}

# Root sequences and empty entries are supported.
expect {
	actual = Yaml.parse_str("- first\n-\n- third")?
	actual == Sequence([Text("first"), Null, Text("third")])
}

# CRLF input and comment-only lines are ignored correctly.
expect {
	actual = Yaml.parse_str("# config\r\nname: roc-parser\r\n")?
	actual == Mapping([{ key: "name", value: Text("roc-parser") }])
}

# Nested flow collections keep quoted commas inside strings.
expect {
	actual = Yaml.parse_str("value: [{ name: 'one,two' }, [1, 2]]")?
	actual == Mapping([{ key: "value", value: Sequence([Mapping([{ key: "name", value: Text("one,two") }]), Sequence([Int(1), Int(2)])]) }])
}

# Double-quoted strings support common escapes.
expect {
	actual = Yaml.parse_str("message: \"first\\nsecond\"")?
	actual == Mapping([{ key: "message", value: Text("first\nsecond") }])
}

# Double-quoted strings support every YAML 1.2 escape, including Unicode.
expect {
	actual = Yaml.parse_str("v: \"\\x41\\u00e9\\U0001F600\\/\\_\\0\"")?
	actual == Mapping([{ key: "v", value: Text("Aé😀/\u(a0)\u(0)") }])
}

# Unsupported escapes before a multi-byte character fail instead of crashing.
expect {
	match Yaml.parse_str("v: \"\\é\"") {
		Err(InvalidYaml({ message, .. })) => message == "unsupported escape sequence `\\é`"
		_ => False
	}
}

# Surrogate and short Unicode escapes are rejected.
expect Yaml.parse_str("v: \"\\uD800\"").is_err() and Yaml.parse_str("v: \"\\u12\"").is_err()

# Block scalars parse as multiline strings, including folded style.
expect {
	actual =
		Yaml.parse_str(
			\\description: |
			\\  first line
			\\  second line
			\\summary: >
			\\  one
			\\  two
			,
		)?

	actual
		== Mapping([
			{ key: "description", value: Text("first line\nsecond line\n") },
			{ key: "summary", value: Text("one two") },
		])
}

# A block scalar's final line without a line break gets no newline, even when kept.
expect {
	clip = Yaml.parse_str("a: |\n  x")?
	keep = Yaml.parse_str("a: |+\n  x")?
	clip == Mapping([{ key: "a", value: Text("x") }]) and keep == clip
}

# Keep chomping at the end of input keeps exactly the trailing line breaks.
expect {
	actual = Yaml.parse_str("a: |+\n  x\n\n")?
	actual == Mapping([{ key: "a", value: Text("x\n\n") }])
}

# Block scalars without content lines are empty unless kept.
expect {
	clip = Yaml.parse_str("a: |\n\n")?
	keep = Yaml.parse_str("a: |+\n\n\n")?
	clip == Mapping([{ key: "a", value: Text("") }]) and keep == Mapping([{ key: "a", value: Text("\n\n") }])
}

# Tabs after the indentation are block scalar content, not indentation.
expect {
	actual = Yaml.parse_str("a: |\n  x\n  \ty\n")?
	actual == Mapping([{ key: "a", value: Text("x\n\ty\n") }])
}

# Whitespace beyond the content indentation on an otherwise empty line is content.
expect {
	actual = Yaml.parse_str("a: |\n  x\n   \t\n  y\n")?
	actual == Mapping([{ key: "a", value: Text("x\n \t\ny\n") }])
}

# Folding drops the line break before empty lines between text lines.
expect {
	actual = Yaml.parse_str("a: >\n  a\n\n\n  b\n")?
	actual == Mapping([{ key: "a", value: Text("a\n\nb\n") }])
}

# Final whitespace without a line break still ends its line (yaml-test-suite JEF9, L24T).
expect {
	keep = Yaml.parse_str("- |+\n   ")?
	clip = Yaml.parse_str("foo: |\n  x\n   ")?
	keep == Sequence([Text("\n")]) and clip == Mapping([{ key: "foo", value: Text("x\n \n") }])
}

# Spaces and a tab form a content line that sets the indentation (R4YG, Y79Y).
expect {
	actual = Yaml.parse_str("foo: |\n \t\nbar: 1\n")?
	actual == Mapping([{ key: "foo", value: Text("\t\n") }, { key: "bar", value: Int(1) }])
}

# A tab-only line cannot end a block scalar (Y79Y).
expect Yaml.parse_str("foo: |\n\t\nbar: 1\n").is_err()

# Block scalars at the document root may start in column 0, until a document marker.
expect {
	actual = Yaml.parse_str("|\na\n...\n")?
	actual == Text("a\n")
}

# Lone carriage returns are line breaks.
expect {
	actual = Yaml.parse_str("value: |\r  one\r  two\r")?
	actual == Mapping([{ key: "value", value: Text("one\ntwo\n") }])
}

# Compact mappings in sequence entries are indented by the spaces after "-"
# (YAML 1.2 8.2.1: here the mapping, and so the indicator, is relative to column 4).
expect {
	actual = Yaml.parse_str("-   value: |2\n      a\n    next: done\n")?
	actual == Sequence([Mapping([{ key: "value", value: Text("a\n") }, { key: "next", value: Text("done") }])])
}

# Quotes and brackets inside plain scalars are ordinary text.
expect {
	actual = Yaml.parse_str("k: it's # comment\nit's: \"q\" # c\na[b: x]{\nlist: [a, 'b''c', \"d, e\"]\n")?
	actual
		== Mapping([
			{ key: "k", value: Text("it's") },
			{ key: "it's", value: Text("q") },
			{ key: "a[b", value: Text("x]{") },
			{ key: "list", value: Sequence([Text("a"), Text("b'c"), Text("d, e")]) },
		])
}

# Indicator-like text inside a plain scalar does not start a quoted scalar.
expect {
	actual = Yaml.parse_str("{_? -  ': 1, b: 2}")?
	actual == Mapping([{ key: "_? -  '", value: Int(1) }, { key: "b", value: Int(2) }])
}

# A dash inside plain text is not an indicator that can start a quoted scalar.
expect {
	actual = Yaml.parse_str("b- \"q: x\n")?
	actual == Mapping([{ key: "b- \"q", value: Text("x") }])
}

# A tab before a document marker is not a document marker.
expect Yaml.parse_str("\t---\na: 1").is_err()

# Leading empty lines may not be indented more than detected block content.
expect Yaml.parse_str("a: |\n    \n  x\n").is_err()

# Chomping indicators are supported for block scalars.
expect {
	actual = Yaml.parse_str("note: |-\n  hello")?
	actual == Mapping([{ key: "note", value: Text("hello") }])
}

# Block scalar content keeps blank lines and # characters literally.
expect {
	actual = Yaml.parse_str("text: |\n  # not a comment\n\n  after\n")?
	actual == Mapping([{ key: "text", value: Text("# not a comment\n\nafter\n") }])
}

# Explicit block indentation indicators are supported.
expect {
	actual = Yaml.parse_str("script: |2-\n  echo one\n  echo two\nnext: done")?

	actual
		== Mapping([
			{ key: "script", value: Text("echo one\necho two") },
			{ key: "next", value: Text("done") },
		])
}

# Folded blocks preserve line breaks around indented continuation lines.
expect {
	actual = Yaml.parse_str("text: >\n  intro\n    code\n  outro\n")?
	actual == Mapping([{ key: "text", value: Text("intro\n  code\noutro\n") }])
}

# Malformed flow collections still fail.
expect Yaml.parse_str("values: [one, two").is_err()

# Doc examples.
expect Yaml.parse_str("draft: false") == Ok(Mapping([{ key: "draft", value: Bool(False) }]))
expect Yaml.to_inspect(Sequence([Int(1), Text("a")])) == "Sequence([Int(1), Text(\"a\")])"

# Null spellings and an empty value all parse as Null.
expect Yaml.parse_str("a: null\nb: ~\nc:\n") == Ok(Mapping([{ key: "a", value: Null }, { key: "b", value: Null }, { key: "c", value: Null }]))

# Doc examples for the tree helpers and the parser.
expect Yaml.parse_str("title: Post").map_ok(|doc| doc.get("title")) == Ok(Ok(Text("Post")))
expect Yaml.parse_str("[a, b]").map_ok(|doc| doc.at(1)) == Ok(Ok(Text("b")))
expect Yaml.parse_str("[1, 2]").map_ok(|doc| doc.as_list()) == Ok(Ok([Int(1), Int(2)]))
expect Utf8.parse_str(Yaml.parser, "a: 1") == Ok(Mapping([{ key: "a", value: Int(1) }]))

expect {
	doc = Yaml.parse_str("jobs:\n  build:\n    steps: [checkout, test]\n")?
	doc.get_path(["jobs", "build", "steps", "1"]) == Ok(Text("test"))
}

# Helpers report a missing key, index or path segment, and a wrong type.
expect {
	doc = Yaml.parse_str("a: 1\nb: [true]\nc: text\n")?
	checks = [
		doc.get("z") == Err(Missing),
		doc.at(0) == Err(Missing),
		doc.get_path(["b", "x"]) == Err(Missing),
		doc.get_path(["b", "1"]) == Err(Missing),
		doc.get_path([]) == Ok(doc),
		doc.get("a").map_ok(|v| v.as_i64()) == Ok(Ok(1)),
		doc.get("a").map_ok(|v| v.as_str()) == Ok(Err(WrongType)),
		doc.get_path(["b", "0"]).map_ok(|v| v.as_bool()) == Ok(Ok(True)),
		doc.get("c").map_ok(|v| v.as_list()) == Ok(Err(WrongType)),
	]
	checks.all(|ok| ok)
}

# Parser failures carry the byte offset of the reported location.
expect {
	match Utf8.parse_str(Yaml.parser, "a: 1\nb: [x\n") {
		Err(ParseError({ offset, .. })) => offset == 8
		Ok(_) => False
	}
}

expect {
	match Utf8.parse_str(Yaml.parser, "\u(FEFF)é: [x\n") {
		Err(ParseError({ offset, .. })) => offset == 7
		Ok(_) => False
	}
}

# Inspection escapes control characters, quotes and interpolation.
expect Yaml.to_inspect(Text("a\u(0)\n\u(1b)\"\\\u(85)é")) == "Text(\"a\\u(00)\\n\\u(1b)\\\"\\\\\\u(85)é\")"
expect Yaml.to_inspect(Mapping([{ key: "k\t", value: Bool(True) }])) == "Mapping([{ key: \"k\\t\", value: Bool(True) }])"

# Trees can be hashed.
expect {
	doc = Yaml.parse_str("a: [1, x]")?
	set = Set.empty().insert(doc).insert(doc)
	set.len() == 1
}

# Decoding: the doc example.
DocConfig : { name : Str, version : Str, port : U16, debug : Try(Bool, [Missing]) }

expect {
	config : Try(DocConfig, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	config = Yaml.decode("name: app\nversion: 1.10\nport: 8080\n")
	config == Ok({ name: "app", version: "1.10", port: 8080, debug: Err(Missing) })
}

decode_strict = Yaml.decoder({ keys: KebabCase, unknown_keys: Reject })

expect {
	result : Try({ user_id : U64 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = decode_strict("user-id: 7\n")
	result == Ok({ user_id: 7 })
}

# Decoding nested records, lists, tuples, dicts and optional fields.
Service : {
	image : Str,
	ports : List(U16),
	env : Dict(Str, Str),
	limits : { cpu : F64, memory : Str },
	pair : (Str, I64),
	replicas : Try(U8, [Missing]),
	command : Try(Str, [Null]),
}

expect {
	text = "image: \"nginx:1.25\"\nports: [80, 0x1BB]\nenv:\n  MODE: production\n  LEVEL: 3\nlimits: { cpu: .5, memory: 512Mi }\npair:\n  - x\n  - -4\ncommand: ~\nextra: ignored\n"
	result : Try(Service, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode(text)
	expected_env = Dict.empty().insert("MODE", "production").insert("LEVEL", "3")
	result == Ok({ image: "nginx:1.25", ports: [80, 443], env: expected_env, limits: { cpu: 0.5, memory: "512Mi" }, pair: ("x", -4), replicas: Err(Missing), command: Err(Null) })
}

# Plain scalars resolve per target: text keeps its spelling, numbers and booleans follow the core schema.
expect {
	result : Try({ a : Str, b : Str, c : F64, d : Bool, e : I8, f : Dec, g : U128 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("a: 1.10\nb: true\nc: 7\nd: FALSE\ne: +12\nf: 1.25\ng: 99999999999999999999\n")
	result == Ok({ a: "1.10", b: "true", c: 7.0, d: False, e: 12, f: 1.25, g: 99999999999999999999 })
}

expect {
	result : Try({ e : I8 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("e: -0o0\n")
	result == Err(InvalidYaml({ line: 1, column: 4, message: "expected an integer, found `-0o0`" }))
}

# Type mismatches report the value's location.
expect {
	result : Try({ port : U16 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("port: \"80\"\n")
	result == Err(InvalidYaml({ line: 1, column: 7, message: "expected an integer, found the string \"80\"" }))
}

expect {
	result : Try({ port : U8 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("port: 300\n")
	result == Err(InvalidYaml({ line: 1, column: 7, message: "integer `300` is outside the range of U8" }))
}

expect {
	result : Try({ name : Str }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("name:\n")
	result == Err(InvalidYaml({ line: 1, column: 6, message: "expected a string, found null" }))
}

expect {
	result : Try({ tags : List(Str) }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("tags:\n  a: b\n")
	result == Err(InvalidYaml({ line: 2, column: 3, message: "expected a sequence, found a mapping" }))
}

# A missing required field comes from the compiler-generated parser.
expect {
	result : Try({ name : Str, port : U16 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("name: app\n")
	result == Err(MissingRequiredField("port"))
}

# Null decodes as an empty collection, and an empty document as an empty record.
expect {
	result : Try({ tags : List(Str), labels : Dict(Str, Str), extra : Try(Str, [Missing]) }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("tags:\nlabels: ~\n")
	empty : Try({ extra : Try(Str, [Missing]) }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	empty = Yaml.decode("")
	result == Ok({ tags: [], labels: Dict.empty(), extra: Err(Missing) }) and empty == Ok({ extra: Err(Missing) })
}

# Unknown keys are skipped, including whole nested collections, unless rejected.
expect {
	result : Try({ b : Str }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("a:\n  x: [1, {y: 2}]\n  z:\n    - 3\nb: kept\n")
	strict : Try({ b : Str }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	strict = Yaml.decoder({ keys: SnakeCase, unknown_keys: Reject })("b: kept\nc: 1\n")
	result == Ok({ b: "kept" }) and strict == Err(InvalidYaml({ line: 2, column: 1, message: "unknown key `c`" }))
}

# Keys may be camelCase.
expect {
	result : Try({ user_id : U64, display_name : Str }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decoder({ keys: CamelCase, unknown_keys: Skip })("userId: 1\ndisplayName: Ada\n")
	result == Ok({ user_id: 1, display_name: "Ada" })
}

# Tuples need exactly their length; tags decode from their names; dicts may have integer keys.
expect {
	short : Try({ p : (I64, I64) }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	short = Yaml.decode("p: [1]\n")
	level : Try({ level : [Debug, Info] }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	level = Yaml.decode("level: Info\n")
	bad : Try({ level : [Debug, Info] }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	bad = Yaml.decode("level: Loud\n")
	codes : Try(Dict(U16, Str), [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	codes = Yaml.decode("200: ok\n404: missing\n")
	checks = [
		short == Err(InvalidYaml({ line: 1, column: 4, message: "expected a sequence of 2 items, found 1" })),
		level == Ok({ level: Info }),
		bad == Err(InvalidYaml({ line: 1, column: 8, message: "unexpected tag `Loud`" })),
		codes == Ok(Dict.empty().insert(200, "ok").insert(404, "missing")),
	]
	checks.all(|ok| ok)
}

# Lists of records decode from block sequences of compact mappings.
expect {
	result : Try(List({ name : Str, tags : List(Str) }), [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("- name: a\n  tags: [x]\n- name: b\n  tags: []\n")
	result == Ok([{ name: "a", tags: ["x"] }, { name: "b", tags: [] }])
}

# Invalid YAML fails the same way when decoding.
expect {
	result : Try({ a : Str }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("a: [x\n")
	result.is_err()
}

# Floats accept the special values where the type has them.
expect {
	result : Try({ a : F64, b : F32 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	result = Yaml.decode("a: -.inf\nb: 1e3\n")
	dec : Try({ a : Dec }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
	dec = Yaml.decode("a: .inf\n")
	(result == Ok({ a: -F64.infinity, b: 1000.0 })) and (dec == Err(InvalidYaml({ line: 1, column: 4, message: "number `.inf` cannot be represented as Dec" })))
}

# A key starting with % is reported as a directive, with or without a leading --- (fuzz: yaml-raw).
expect when_error(Yaml.parse_str("%:")) == when_error(Yaml.parse_str("---\n%:"))
expect when_error(Yaml.parse_str("%-- x\n...\nx")) == when_error(Yaml.parse_str("---\n%-- x\n...\nx"))
