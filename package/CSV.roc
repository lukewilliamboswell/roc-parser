import Parser
import Utf8

## RFC 4180-style CSV parsing and typed record decoding.
##
## The dialect is RFC 4180 with the common relaxations of Python's `csv`
## module (strict mode) and Rust's `csv` crate:
##
## - Fields are separated by `,` and records by CRLF, LF or a lone CR. The line
##   break after the last record is optional, and empty input has no records.
## - A field is any run of bytes other than `,`, CR and LF, so fields may hold
##   arbitrary UTF-8, tabs, spaces (kept verbatim) and control characters.
## - A field that starts with `"` is quoted: it may contain `,`, CR and LF, a
##   doubled `""` stands for one `"`, and the closing quote must be followed by
##   `,`, a line break or the end of input. An unterminated quoted field is an
##   error.
## - A `"` anywhere else in an unquoted field is an ordinary character.
## - A blank line is a record with one empty field, as RFC 4180's grammar says
##   (Python's `csv.reader` returns an empty list for it instead).
## - Rows may have different numbers of fields. A byte order mark is not
##   removed; it is part of the first field.
##
## There are three ways to read a file:
##
## - [CSV.parse] decodes rows into records whose field names match the header
##   row, with the row type chosen by type inference;
## - [CSV.parse_str] runs a hand-built record parser (`record`, `field` and
##   `Parser.keep`) over every row, matching columns by position;
## - [CSV.parse_records] returns the raw fields.
##
## ```roc
## Person : { name : Str, age : U64 }
##
## people : Try(List(Person), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
## people = CSV.parse("name,age\nAda,36\nAlan,41\n")
##
## expect people == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
## ```
CSV :: [].{

	## One CSV record: its fields as raw bytes, in column order.
	##
	## Fields parsed from a `Str` are valid UTF-8; records built by hand may
	## hold any bytes.
	Record : List(List(U8))

	## Where and why CSV text could not be read or decoded.
	##
	## `record` and `field` are one-based and count every record of the input,
	## including a header row and blank lines. `line` and `column` are the
	## one-based physical line and byte column of the field (or of the record,
	## when the field does not exist); they are `0` when the source text is not
	## known, as in [CSV.decode]. `message` is bounded in length and quotes
	## field text lossily, so it is safe to show for any input.
	Error : { record : U64, field : U64, line : U64, column : U64, message : Str }

	## Parse CSV text into raw records without decoding them.
	##
	## ```roc
	## expect CSV.parse_records("a,\"b,c\"\n") == Ok([["a".to_utf8(), "b,c".to_utf8()]])
	## ```
	parse_records : Str -> Try(List(Record), [InvalidCsv(Error)])
	parse_records = |text| {
		bytes = text.to_utf8()
		match scan_table(bytes) {
			Ok(records) => Ok(records)
			Err(bad) => Err(InvalidCsv(syntax_error(bytes, bad)))
		}
	}

	## A parser for a whole CSV file, for use inside larger parsers.
	##
	## It reads records until its input ends, so it consumes all of it or fails
	## with the offset of the malformed field. It accepts any bytes.
	parser : Parser(Utf8.Bytes, List(Record))
	parser = Parser.custom(
		|bytes| {
			match scan_table(bytes) {
				Ok(records) => Ok({ value: records, rest: [] })
				Err(bad) => Err(ParseError({ message: bad_field_message(bad.problem), offset: bad.at }))
			}
		},
	)

	## Split off the first record as column names.
	##
	## Names are decoded lossily and kept verbatim. Empty input gives no names
	## and no rows.
	##
	## ```roc
	## expect {
	##     records = CSV.parse_records("name,age\nAda,36\n")?
	##     CSV.split_header(records) == { header: ["name", "age"], rows: [["Ada".to_utf8(), "36".to_utf8()]] }
	## }
	## ```
	split_header : List(Record) -> { header : List(Str), rows : List(Record) }
	split_header = |records| {
		match records.first() {
			Ok(first) => { header: first.map(Str.from_utf8_lossy), rows: records.drop_first(1) }
			Err(_) => { header: [], rows: [] }
		}
	}

	## Parse CSV text and decode every record with a hand-built record parser.
	##
	## Fails with `InvalidCsv(error)` at the first problem: text that is not
	## valid CSV, a field that does not decode, a record with too few fields,
	## or a record with fields the parser did not read.
	##
	## ```roc
	## user : Parser(CSV.Record, { name : Str, age : U64 })
	## user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.u64))
	##
	## expect CSV.parse_str(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
	## ```
	parse_str : Parser(Record, a), Str -> Try(List(a), [InvalidCsv(Error)])
	parse_str = |record_parser, text| {
		bytes = text.to_utf8()
		match scan_table(bytes) {
			Err(bad) => Err(InvalidCsv(syntax_error(bytes, bad)))
			Ok(records) => {
				match decode(record_parser, records) {
					Ok(values) => Ok(values)
					Err(InvalidCsv(problem)) => Err(InvalidCsv(locate(bytes, problem)))
				}
			}
		}
	}

	## Decode already parsed records with a hand-built record parser.
	##
	## Errors are as for [CSV.parse_str], numbered from the first record given,
	## with `line` and `column` set to `0`.
	decode : Parser(Record, a), List(Record) -> Try(List(a), [InvalidCsv(Error)])
	decode = |record_parser, records| {
		var $values = List.with_capacity(records.len())
		var $index = 0
		while $index < records.len() {
			fields = records.get($index) ?? []
			match record_parser.run(fields) {
				Ok({ value, rest }) if rest.is_empty() => {
					$values = $values.append(value)
				}
				Ok({ value: _, rest }) => {
					return Err(InvalidCsv(leftover_error(record_parser, fields, rest.len(), $index + 1)))
				}
				Err(ParseError({ message, offset })) => {
					return Err(InvalidCsv({ record: $index + 1, field: offset + 1, line: 0, column: 0, message }))
				}
			}
			$index = $index + 1
		}
		Ok($values)
	}

	## Start a record parser with a curried constructor for the desired value.
	##
	## Add one `.keep(CSV.field(...))` per column, in column order:
	##
	## ```roc
	## Name : { first : Str, last : Str }
	##
	## name : Parser(CSV.Record, Name)
	## name = CSV.record(|first| |last| { first, last }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.string))
	##
	## expect CSV.parse_str(name, "Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
	## ```
	record : a -> Parser(Record, a)
	record = |f| {
		Parser.const(f)
	}

	## Consume the next field of a [CSV.Record] using a parser for its bytes.
	##
	## The field parser must consume the whole field. Fails when the record has
	## no fields left.
	field : Parser(Utf8.Bytes, a) -> Parser(Record, a)
	field = |field_parser| {
		Parser.custom(
			|fields| {
				match fields.first() {
					Err(_) =>
						Err(ParseError({ message: "expected another field, but the record has no more", offset: 0 }))

					Ok(bytes) => {
						match field_parser.run(bytes) {
							Ok({ value, rest }) if rest.is_empty() => Ok({ value, rest: fields.drop_first(1) })
							Ok(_) => Err(ParseError({ message: "the field parser did not read all of `${excerpt(bytes)}`", offset: 0 }))
							Err(ParseError({ message, offset: _ })) => Err(ParseError({ message, offset: 0 }))
						}
					}
				}
			},
		)
	}

	## Parse one field as a valid UTF-8 string, kept verbatim (no trimming).
	string : Parser(Utf8.Bytes, Str)
	string = Parser.custom(
		|bytes| {
			match Str.from_utf8(bytes) {
				Ok(text) => Ok({ value: text, rest: [] })
				Err(_) => Err(ParseError({ message: "expected UTF-8 text, found `${excerpt(bytes)}`", offset: 0 }))
			}
		},
	)

	## Parse one field as an unsigned 64-bit integer.
	##
	## The field must be ASCII decimal digits with an optional leading `+`, and
	## fit in a U64 (the grammar of Rust's `u64::from_str`). Whitespace,
	## underscores, signs other than `+` and `0x`-style prefixes are rejected.
	u64 : Parser(Utf8.Bytes, U64)
	u64 = Parser.custom(
		|bytes| {
			match int_from_bytes(bytes, U64.from_str, False) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError({ message: expected_message("a U64", bytes), offset: 0 }))
			}
		},
	)

	## Parse one field as a 64-bit floating-point number.
	##
	## The field is an optional sign followed by `inf`, `infinity` or `nan` in
	## any case, or by a decimal number with an optional fraction and exponent
	## (`12`, `1.5`, `.5`, `5.`, `1e-3`, `2.5E+10`); this is the grammar of
	## Rust's `f64::from_str`. Magnitudes too large for an F64 become infinity
	## and too small ones become zero. Whitespace, underscores and hexadecimal
	## forms are rejected.
	f64 : Parser(Utf8.Bytes, F64)
	f64 = Parser.custom(
		|bytes| {
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError({ message: expected_message("an F64", bytes), offset: 0 }))
			}
		},
	)

	## Names the requirement that a row type can be decoded from CSV, so a
	## signature can say "CSV-parseable" without naming the internal format
	## and state types.
	row.Parseable(errs) :
		where [
			row.parser_for : Format -> (State -> Try({ value : row, rest : State }, errs)),
		]

	## Decode CSV text whose first record is a header row into rows of an
	## inferred type, usually a record.
	##
	## Each record field reads the column whose header is exactly its name;
	## columns no field names are ignored, and when two columns share a name
	## the last one wins. A field of type `Try(a, [Missing])` is
	## `Err(Missing)` when its column is absent, and a field of type
	## `Try(a, [Null])` is `Err(Null)` when its cell is empty. A required field
	## with no column fails with `MissingRequiredField(name)`, which the
	## compiler adds to the error type.
	##
	## Cells decode as `Str` (verbatim), `Bool` (`true` or `false` in any
	## letter case), integers (ASCII digits with an optional sign, `-` only for
	## signed types), `Dec` (an optional sign, digits and an optional fraction),
	## `F32` and `F64` (as [CSV.f64]), and tags without payloads (the cell is
	## the tag name). A field that is a record, a tuple or a tag with a payload
	## fails when it is decoded; a field that is a list or a dictionary does not
	## compile.
	##
	## Blank lines are skipped. Every other record must have exactly as many
	## fields as the header; a shorter or longer record is an error naming it.
	##
	## ```roc
	## Item : { sku : Str, count : U64, note : Try(Str, [Missing]), price : Try(Dec, [Null]) }
	##
	## items : Try(List(Item), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	## items = CSV.parse("price,sku,count\n1.50,A1,3\n,B2,0\n")
	##
	## expect items == Ok([
	##     { sku: "A1", count: 3, note: Err(Missing), price: Ok(1.50) },
	##     { sku: "B2", count: 0, note: Err(Missing), price: Err(Null) },
	## ])
	## ```
	parse : Str -> Try(List(row), [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	parse = |text| {
		Row : row
		decode_table(text, Row.parser_for(Format.Default), HeaderExact)
	}

	## Like [CSV.parse], but header names are normalized before they are
	## matched: surrounding spaces are removed, ASCII letters are lowercased,
	## and runs of spaces, `-` and `_` become one `_`. A column headed
	## `First Name`, `first-name` or `FIRST_NAME` fills a field `first_name`.
	##
	## ```roc
	## names : Try(List({ first_name : Str }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	## names = CSV.parse_normalized(" First Name \nAda\n")
	##
	## expect names == Ok([{ first_name: "Ada" }])
	## ```
	parse_normalized : Str -> Try(List(row), [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	parse_normalized = |text| {
		Row : row
		decode_table(text, Row.parser_for(Format.Default), HeaderNormalized)
	}

	## Decode CSV text without a header row into tuples, one element per
	## column.
	##
	## Every record (blank lines excepted) must have exactly as many fields as
	## the tuple has elements. Cells decode as in [CSV.parse].
	##
	## ```roc
	## pairs : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	## pairs = CSV.parse_headerless("Ada,36\nAlan,41\n")
	##
	## expect pairs == Ok([("Ada", 36), ("Alan", 41)])
	## ```
	parse_headerless : Str -> Try(List(row), [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	parse_headerless = |text| {
		Row : row
		decode_table(text, Row.parser_for(Format.Default), Headerless)
	}

	## The CSV format for type-directed decoding. Used through [CSV.parse];
	## you do not need to name it.
	Format :: [Default].{

		## Field names are matched as written; [CSV.parse_normalized] normalizes the header instead.
		rename_field : Format, Str -> Str
		rename_field = |_, name| name

		## Read the cell as UTF-8 text, verbatim.
		parse_str : Format, State -> Try({ value : Str, rest : State }, [InvalidCsv(Error)])
		parse_str = |_, state| {
			bytes = current_cell(state)?
			match Str.from_utf8(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_message("UTF-8 text", bytes)))
			}
		}

		## Read `true` or `false` in any letter case.
		parse_bool : Format, State -> Try({ value : Bool, rest : State }, [InvalidCsv(Error)])
		parse_bool = |_, state| {
			bytes = current_cell(state)?
			word = lowercase_ascii(bytes)
			if word == "true".to_utf8() {
				Ok({ value: True, rest: state })
			} else if word == "false".to_utf8() {
				Ok({ value: False, rest: state })
			} else {
				Err(cell_error(state, expected_message("true or false", bytes)))
			}
		}

		## Read ASCII digits with an optional `+` as a U8.
		parse_u8 : Format, State -> Try({ value : U8, rest : State }, [InvalidCsv(Error)])
		parse_u8 = |_, state| cell_int(state, U8.from_str, False, "a U8")

		## Read ASCII digits with an optional sign as a I8.
		parse_i8 : Format, State -> Try({ value : I8, rest : State }, [InvalidCsv(Error)])
		parse_i8 = |_, state| cell_int(state, I8.from_str, True, "an I8")

		## Read ASCII digits with an optional `+` as a U16.
		parse_u16 : Format, State -> Try({ value : U16, rest : State }, [InvalidCsv(Error)])
		parse_u16 = |_, state| cell_int(state, U16.from_str, False, "a U16")

		## Read ASCII digits with an optional sign as a I16.
		parse_i16 : Format, State -> Try({ value : I16, rest : State }, [InvalidCsv(Error)])
		parse_i16 = |_, state| cell_int(state, I16.from_str, True, "an I16")

		## Read ASCII digits with an optional `+` as a U32.
		parse_u32 : Format, State -> Try({ value : U32, rest : State }, [InvalidCsv(Error)])
		parse_u32 = |_, state| cell_int(state, U32.from_str, False, "a U32")

		## Read ASCII digits with an optional sign as a I32.
		parse_i32 : Format, State -> Try({ value : I32, rest : State }, [InvalidCsv(Error)])
		parse_i32 = |_, state| cell_int(state, I32.from_str, True, "an I32")

		## Read ASCII digits with an optional `+` as a U64.
		parse_u64 : Format, State -> Try({ value : U64, rest : State }, [InvalidCsv(Error)])
		parse_u64 = |_, state| cell_int(state, U64.from_str, False, "a U64")

		## Read ASCII digits with an optional sign as a I64.
		parse_i64 : Format, State -> Try({ value : I64, rest : State }, [InvalidCsv(Error)])
		parse_i64 = |_, state| cell_int(state, I64.from_str, True, "an I64")

		## Read ASCII digits with an optional `+` as a U128.
		parse_u128 : Format, State -> Try({ value : U128, rest : State }, [InvalidCsv(Error)])
		parse_u128 = |_, state| cell_int(state, U128.from_str, False, "a U128")

		## Read ASCII digits with an optional sign as a I128.
		parse_i128 : Format, State -> Try({ value : I128, rest : State }, [InvalidCsv(Error)])
		parse_i128 = |_, state| cell_int(state, I128.from_str, True, "an I128")

		## Read an optional sign, digits and an optional fraction.
		parse_dec : Format, State -> Try({ value : Dec, rest : State }, [InvalidCsv(Error)])
		parse_dec = |_, state| {
			bytes = current_cell(state)?
			match dec_from_bytes(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_message("a Dec", bytes)))
			}
		}

		## Read a float with the grammar of [CSV.f64].
		parse_f32 : Format, State -> Try({ value : F32, rest : State }, [InvalidCsv(Error)])
		parse_f32 = |_, state| {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value: value.to_f32_wrap(), rest: state })
				Err(_) => Err(cell_error(state, expected_message("an F32", bytes)))
			}
		}

		## Read a float with the grammar of [CSV.f64].
		parse_f64 : Format, State -> Try({ value : F64, rest : State }, [InvalidCsv(Error)])
		parse_f64 = |_, state| {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_message("an F64", bytes)))
			}
		}

		## An empty cell is null, so a `Try(a, [Null])` field is `Err(Null)`.
		parse_null : Format, State -> Try(State, [InvalidCsv(Error)])
		parse_null = |_, state| {
			bytes = current_cell(state)?
			if bytes.is_empty() Ok(state) else Err(cell_error(state, "expected an empty cell"))
		}

		## A row is one record; a record inside a row is rejected.
		parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [InvalidCsv(Error)])
		parse_record_start = |_, state| {
			if state.nested {
				Err(cell_error(state, "a CSV cell cannot hold a record"))
			} else {
				Ok(Uncounted(State.{ row: state.row, header: state.header, column: 0, cell: 0, record: state.record, nested: True }))
			}
		}

		## Offer the next header name as the field to fill.
		parse_record_field : Format,
		Encoding.FieldName.FieldNames(_shape),
		State -> Try(
			[
				Field({ field : Encoding.FieldName(_shape), rest : State }),
				TryField({ name : Str, rest : State }),
				TryFieldCaseless({ name : Str, rest : State }),
				Continue(State),
				Done(State),
			],
			[InvalidCsv(Error)],
		)
		parse_record_field = |_, _, state| {
			match state.header.get(state.column) {
				Err(_) => Ok(Done(state))
				Ok(name) => Ok(TryField({ name, rest: State.{ row: state.row, header: state.header, column: state.column + 1, cell: state.column, record: state.record, nested: True } }))
			}
		}

		## The record ends after the last header column.
		parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [InvalidCsv(Error)])
		parse_record_after_field = |_, state| {
			if state.column >= state.header.len() Ok(Done(state)) else Ok(Continue(state))
		}

		## The cell to skip is chosen by the column, so nothing is consumed.
		skip_record_field : Format, State -> Try(State, [InvalidCsv(Error)])
		skip_record_field = |_, state| Ok(state)

		## A headerless row is a tuple with one element per field.
		parse_tuple_start : Format, State, U64 -> Try(State, [InvalidCsv(Error)])
		parse_tuple_start = |_, state, len| {
			if state.nested {
				Err(cell_error(state, "a CSV cell cannot hold a tuple"))
			} else if state.row.len() != len {
				Err(InvalidCsv({ record: state.record, field: smaller(state.row.len(), len) + 1, line: 0, column: 0, message: width_message(state.row.len(), len) }))
			} else {
				Ok(State.{ row: state.row, header: state.header, column: 0, cell: 0, record: state.record, nested: True })
			}
		}

		## Move to the cell of the next tuple element.
		parse_tuple_next : Format, State, U64, U64 -> Try(State, [InvalidCsv(Error)])
		parse_tuple_next = |_, state, index, _| Ok(State.{ row: state.row, header: state.header, column: index, cell: index, record: state.record, nested: True })

		## The error for a cell that holds no valid value.
		invalid_value : Format, State -> [InvalidCsv(Error)]
		invalid_value = |_, state| cell_error(state, "the cell does not hold a valid value")

		## Finish a tuple row.
		parse_tuple_end : Format, State, U64 -> Try(State, [InvalidCsv(Error)])
		parse_tuple_end = |_, state, _| Ok(state)

		## A tag without a payload is read from its name.
		parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [InvalidCsv(Error)])
		parse_tag_union = |format, spec, state| {
			bytes = current_cell(state)?
			Encoding.ParseTagUnionSpec.parse(
				spec,
				{
					tag: Str.from_utf8_lossy(bytes),
					encoding: format,
					state,
					start_payloads: |s, count| if count == 0 Ok(s) else Err(cell_error(s, "a CSV cell cannot hold a tag with a payload")),
					next_payload: |s, _, _| Ok(s),
					finish_payloads: |s, _| Ok(s),
					missing: cell_error(state, "unknown tag `${excerpt(bytes)}`"),
				},
			)
		}
	}

	## The cursor for type-directed decoding: one record, the header names,
	## and indices into them. Used through [CSV.parse]; you do not need to name
	## it.
	State :: { row : Record, header : List(Str), column : U64, cell : U64, record : U64, nested : Bool }
}

## Decode every data record of `text` with a row parser built by `parser_for`.
decode_table : Str, (CSV.State -> Try({ value : row, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errs])), [HeaderExact, HeaderNormalized, Headerless] -> Try(List(row), [InvalidCsv(CSV.Error), ..errs])
decode_table = |text, parse_row, mode| {
	bytes = text.to_utf8()
	records =
		match scan_table(bytes) {
			Ok(found) => found
			Err(bad) => return Err(InvalidCsv(syntax_error(bytes, bad)))
		}
	{ header, first } =
		match mode {
			Headerless => { header: [], first: 0 }
			HeaderExact => { header: (records.first() ?? []).map(Str.from_utf8_lossy), first: 1 }
			HeaderNormalized => { header: (records.first() ?? []).map(normalize_name), first: 1 }
		}
	width = header.len()
	var $values = List.with_capacity(records.len())
	var $index = first
	while $index < records.len() {
		row = records.get($index) ?? []
		if row != [[]] {
			if mode != Headerless and row.len() != width {
				return Err(InvalidCsv(locate(bytes, { record: $index + 1, field: smaller(row.len(), width) + 1, line: 0, column: 0, message: width_message(row.len(), width) })))
			}
			match parse_row(CSV.State.{ row, header, column: 0, cell: 0, record: $index + 1, nested: False }) {
				Ok({ value, rest: _ }) => {
					$values = $values.append(value)
				}
				Err(InvalidCsv(problem)) => return Err(InvalidCsv(locate(bytes, problem)))
				Err(other) => return Err(other)
			}
		}
		$index = $index + 1
	}
	Ok($values)
}

smaller : U64, U64 -> U64
smaller = |a, b| if a < b a else b

width_message : U64, U64 -> Str
width_message = |found, wanted| {
	if found < wanted {
		"the record has ${found.to_str()} fields, fewer than the ${wanted.to_str()} columns"
	} else {
		"the record has ${found.to_str()} fields, more than the ${wanted.to_str()} columns"
	}
}

## Lowercase ASCII, trim spaces, and turn runs of spaces, `-` and `_` into one `_`.
normalize_name : List(U8) -> Str
normalize_name = |bytes| {
	var $out = []
	var $pending = False
	for byte in bytes {
		if byte == ' ' or byte == '-' or byte == '_' {
			$pending = !$out.is_empty()
		} else {
			if $pending {
				$out = $out.append('_')
				$pending = False
			}
			$out = $out.append(if byte >= 'A' and byte <= 'Z' byte + 32 else byte)
		}
	}
	Str.from_utf8_lossy($out)
}

current_cell : CSV.State -> Try(List(U8), [InvalidCsv(CSV.Error)])
current_cell = |state| {
	match state.row.get(state.cell) {
		Ok(bytes) => Ok(bytes)
		Err(_) => Err(cell_error(state, "the record has no field here"))
	}
}

cell_error : CSV.State, Str -> [InvalidCsv(CSV.Error)]
cell_error = |state, message| InvalidCsv({ record: state.record, field: state.cell + 1, line: 0, column: 0, message })

cell_int : CSV.State, (Str -> Try(n, [BadNumStr])), Bool, Str -> Try({ value : n, rest : CSV.State }, [InvalidCsv(CSV.Error)])
cell_int = |state, from_str, signed, name| {
	bytes = current_cell(state)?
	match int_from_bytes(bytes, from_str, signed) {
		Ok(value) => Ok({ value, rest: state })
		Err(_) => Err(cell_error(state, expected_message(name, bytes)))
	}
}

## Read `+?[0-9]+` (or `[+-]?[0-9]+` when `signed`) with `from_str`.
int_from_bytes : List(U8), (Str -> Try(n, [BadNumStr])), Bool -> Try(n, [NotAnInteger])
int_from_bytes = |bytes, from_str, signed| {
	{ negative, digits } =
		match bytes {
			['+', .. as rest] => { negative: False, digits: rest }
			['-', .. as rest] if signed => { negative: True, digits: rest }
			_ => { negative: False, digits: bytes }
		}
	if all_digits(digits) {
		text = Str.from_utf8_lossy(if negative ['-'].concat(digits) else digits)
		match from_str(text) {
			Ok(value) => Ok(value)
			Err(_) => Err(NotAnInteger)
		}
	} else {
		Err(NotAnInteger)
	}
}

## Read `[+-]?[0-9]*(.[0-9]*)?` with at least one digit as a `Dec`.
dec_from_bytes : List(U8) -> Try(Dec, [NotADec])
dec_from_bytes = |bytes| {
	unsigned =
		match bytes {
			['+', .. as rest] | ['-', .. as rest] => rest
			_ => bytes
		}
	{ whole, fraction } =
		match unsigned.find_first_index(|b| b == '.') {
			Ok(at) => { whole: unsigned.sublist({ start: 0, len: at }), fraction: unsigned.drop_first(at + 1) }
			Err(_) => { whole: unsigned, fraction: [] }
		}
	ok = (all_digits(whole) or whole.is_empty()) and (all_digits(fraction) or fraction.is_empty()) and !(whole.is_empty() and fraction.is_empty())
	if ok {
		match Dec.from_str(Str.from_utf8_lossy(bytes)) {
			Ok(value) => Ok(value)
			Err(_) => Err(NotADec)
		}
	} else {
		Err(NotADec)
	}
}

## Bytes of field text quoted in error messages.
excerpt_len : U64
excerpt_len = 40

## Render the start of a field for an error message: bounded, and lossy so
## that bytes which are not UTF-8 cannot crash.
excerpt : List(U8) -> Str
excerpt = |bytes| {
	if bytes.len() <= excerpt_len {
		Str.from_utf8_lossy(bytes)
	} else {
		Str.concat(Str.from_utf8_lossy(bytes.sublist({ start: 0, len: excerpt_len })), "…")
	}
}

expected_message : Str, List(U8) -> Str
expected_message = |what, bytes| "expected ${what}, found `${excerpt(bytes)}`"

## Explain why a record parser left fields unread, preferring the furthest
## failure (such as a field that stopped a `Parser.many`).
leftover_error : Parser(CSV.Record, a), CSV.Record, U64, U64 -> CSV.Error
leftover_error = |record_parser, fields, left, record| {
	used = fields.len() - left
	extra = { record, field: used + 1, line: 0, column: 0, message: "the record has ${fields.len().to_str()} fields, but the parser read only ${used.to_str()}" }
	match record_parser.parse(fields) {
		Err(ParseError({ message, offset })) if message != "unexpected input" => { record, field: offset + 1, line: 0, column: 0, message }
		_ => extra
	}
}

## A malformed quoted field: which record and field it is, where it starts
## (`at`) and what is wrong.
BadField : { record : U64, field : U64, at : U64, problem : [Unterminated, TextAfterQuote] }

bad_field_message : [Unterminated, TextAfterQuote] -> Str
bad_field_message = |problem| {
	match problem {
		Unterminated => "unterminated quoted field"
		TextAfterQuote => "a closing quote must be followed by `,`, a line break or the end of input"
	}
}

syntax_error : List(U8), BadField -> CSV.Error
syntax_error = |bytes, bad| {
	{ line, column } = line_column(bytes, bad.at)
	{ record: bad.record + 1, field: bad.field + 1, line, column, message: bad_field_message(bad.problem) }
}

## Fill in the line and column of a decoding error from the source bytes.
## Only runs on the error path, so it rescans the input.
locate : List(U8), CSV.Error -> CSV.Error
locate = |bytes, problem| {
	{ line, column } = line_column(bytes, field_offset(bytes, problem.record - 1, problem.field - 1))
	{ record: problem.record, field: problem.field, line, column, message: problem.message }
}

## Byte offset where field `field` of record `record` (both zero-based)
## starts, or where the record starts when it has no such field.
field_offset : List(U8), U64, U64 -> U64
field_offset = |bytes, record, field| {
	var $pos = 0
	var $index = 0
	while $index < record {
		match scan_record(bytes, $pos) {
			Ok({ fields: _, next }) => {
				$pos = after_break(bytes, next)
			}
			Err(_) => {
				return $pos
			}
		}
		$index = $index + 1
	}
	record_start = $pos
	var $field = 0
	while $field < field {
		next =
			if bytes.get($pos) == Ok('"') {
				match scan_quoted(bytes, $pos) {
					Ok(found) => found.next
					Err(_) => return record_start
				}
			} else {
				scan_unquoted(bytes, $pos)
			}
		if bytes.get(next) == Ok(',') {
			$pos = next + 1
		} else {
			return record_start
		}
		$field = $field + 1
	}
	$pos
}

## One-based line and byte column of `offset`; CRLF, LF and a lone CR each
## end a line.
line_column : List(U8), U64 -> { line : U64, column : U64 }
line_column = |bytes, offset| {
	var $line = 1
	var $line_start = 0
	var $pos = 0
	while $pos < offset and $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == '\n' or (byte == '\r' and bytes.get($pos + 1) != Ok('\n')) {
			$line = $line + 1
			$line_start = $pos + 1
		}
		$pos = $pos + 1
	}
	{ line: $line, column: offset - $line_start + 1 }
}

## Offset after the line break (if any) at `at`.
after_break : List(U8), U64 -> U64
after_break = |bytes, at| {
	match bytes.get(at) {
		Ok('\r') => if bytes.get(at + 1) == Ok('\n') at + 2 else at + 1
		Ok('\n') => at + 1
		_ => at
	}
}

## Scan records until the input ends.
scan_table : List(U8) -> Try(List(CSV.Record), BadField)
scan_table = |bytes| {
	len = bytes.len()
	var $records = []
	var $pos = 0
	while $pos < len {
		match scan_record(bytes, $pos) {
			Err({ field, at, problem }) => {
				return Err({ record: $records.len(), field, at, problem })
			}
			Ok({ fields, next }) => {
				$records = $records.append(fields)
				$pos = after_break(bytes, next)
			}
		}
	}
	Ok($records)
}

## Scan one record starting at `start`, stopping before its line break (or at
## the end of input). Fails with the index, offset and problem of a malformed
## quoted field.
scan_record : List(U8), U64 -> Try({ fields : CSV.Record, next : U64 }, { field : U64, at : U64, problem : [Unterminated, TextAfterQuote] })
scan_record = |bytes, start| {
	len = bytes.len()
	var $fields = []
	var $pos = start
	while True {
		scanned =
			if bytes.get($pos) == Ok('"') {
				scan_quoted(bytes, $pos)
			} else {
				end = scan_unquoted(bytes, $pos)
				Ok({ field: bytes.sublist({ start: $pos, len: end - $pos }), next: end })
			}
		match scanned {
			Err({ at, problem }) => {
				return Err({ field: $fields.len(), at, problem })
			}
			Ok({ field, next }) => {
				$fields = $fields.append(field)
				if next < len and bytes.get(next) == Ok(',') {
					$pos = next + 1
				} else {
					return Ok({ fields: $fields, next })
				}
			}
		}
	}
	Ok({ fields: $fields, next: $pos })
}

## Offset of the first `,`, CR or LF at or after `start`, or the input length.
scan_unquoted : List(U8), U64 -> U64
scan_unquoted = |bytes, start| {
	len = bytes.len()
	var $pos = start
	while $pos < len and !is_delimiter(bytes.get($pos) ?? 0) {
		$pos = $pos + 1
	}
	$pos
}

is_delimiter : U8 -> Bool
is_delimiter = |byte| byte == ',' or byte == '\r' or byte == '\n'

## Scan a quoted field whose opening `"` is at `start`.
scan_quoted : List(U8), U64 -> Try({ field : List(U8), next : U64 }, { at : U64, problem : [Unterminated, TextAfterQuote] })
scan_quoted = |bytes, start| {
	len = bytes.len()
	var $field = []
	var $chunk_start = start + 1
	var $pos = start + 1
	while $pos < len {
		if bytes.get($pos) == Ok('"') {
			$field = $field.concat(bytes.sublist({ start: $chunk_start, len: $pos - $chunk_start }))
			if bytes.get($pos + 1) == Ok('"') {
				$field = $field.append('"')
				$pos = $pos + 2
				$chunk_start = $pos
			} else {
				after = $pos + 1
				if after >= len or is_delimiter(bytes.get(after) ?? 0) {
					return Ok({ field: $field, next: after })
				}
				return Err({ at: after, problem: TextAfterQuote })
			}
		} else {
			$pos = $pos + 1
		}
	}
	Err({ at: start, problem: Unterminated })
}

all_digits : List(U8) -> Bool
all_digits = |bytes| !bytes.is_empty() and bytes.all(|b| b >= '0' and b <= '9')

lowercase_ascii : List(U8) -> List(U8)
lowercase_ascii = |bytes| bytes.map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)

## Parse a float using the grammar documented on `CSV.f64`.
decimal_f64 : List(U8) -> Try(F64, [NotAFloat])
decimal_f64 = |bytes| {
	{ negative, unsigned } =
		match bytes {
			['-', .. as rest] => { negative: True, unsigned: rest }
			['+', .. as rest] => { negative: False, unsigned: rest }
			_ => { negative: False, unsigned: bytes }
		}
	signed = |value| if negative -value else value
	word = lowercase_ascii(unsigned)
	if word == "inf".to_utf8() or word == "infinity".to_utf8() {
		Ok(signed(F64.infinity))
	} else if word == "nan".to_utf8() {
		Ok(F64.nan)
	} else {
		{ mantissa, exponent } =
			match unsigned.find_first_index(|b| b == 'e' or b == 'E') {
				Ok(at) => { mantissa: unsigned.sublist({ start: 0, len: at }), exponent: Ok(unsigned.drop_first(at + 1)) }
				Err(_) => { mantissa: unsigned, exponent: Err(NoExponent) }
			}
		{ whole, fraction } =
			match mantissa.find_first_index(|b| b == '.') {
				Ok(at) => { whole: mantissa.sublist({ start: 0, len: at }), fraction: mantissa.drop_first(at + 1) }
				Err(_) => { whole: mantissa, fraction: [] }
			}
		mantissa_ok = (all_digits(whole) or whole.is_empty()) and (all_digits(fraction) or fraction.is_empty()) and !(whole.is_empty() and fraction.is_empty())
		exponent_ok =
			match exponent {
				Ok(['+', .. as digits]) | Ok(['-', .. as digits]) => all_digits(digits)
				Ok(digits) => all_digits(digits)
				Err(NoExponent) => True
			}
		if mantissa_ok and exponent_ok {
			match F64.from_str(Str.from_utf8_lossy(bytes)) {
				Ok(value) => Ok(value)
				# F64.from_str rounds tiny magnitudes to zero but rejects huge ones.
				Err(_) =>
					if whole.concat(fraction).all(|b| b == '0') {
						Ok(signed(0.0))
					} else {
						Ok(signed(F64.infinity))
					}
			}
		} else {
			Err(NotAFloat)
		}
	}
}

parses_u64 : Str, Try(U64, {}) -> Bool
parses_u64 = |text, expected| {
	actual = Utf8.parse_bytes(CSV.u64, text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted
		(Err(_), Err({})) => True
		_ => False
	}
}

parses_f64 : Str, Try(F64, {}) -> Bool
parses_f64 = |text, expected| {
	actual = Utf8.parse_bytes(CSV.f64, text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted or (value.is_nan() and wanted.is_nan())
		(Err(_), Err({})) => True
		_ => False
	}
}

records : Str -> Try(List(List(Str)), [InvalidCsv(CSV.Error)])
records = |text| CSV.parse_records(text).map_ok(|found| found.map(|fields| fields.map(Str.from_utf8_lossy)))

error_of : Try(a, [InvalidCsv(CSV.Error), ..others]) -> Try(CSV.Error, [NoError])
error_of = |result| {
	match result {
		Err(InvalidCsv(problem)) => Ok(problem)
		_ => Err(NoError)
	}
}

# U64 fields are `+?[0-9]+`; Roc literal syntax is not accepted.
expect parses_u64("0", Ok(0)) and parses_u64("+5", Ok(5)) and parses_u64("007", Ok(7))
expect parses_u64("18446744073709551615", Ok(18446744073709551615)) and parses_u64("18446744073709551616", Err({}))
expect ["", "+", "-0", "-1", " 1", "1 ", "1_000", "0x10", "0b1", "1e3", "1.0"].all(|text| parses_u64(text, Err({})))

# F64 fields follow Rust's f64::from_str grammar, saturating out-of-range values.
expect parses_f64("1.5", Ok(1.5)) and parses_f64(".5", Ok(0.5)) and parses_f64("5.", Ok(5.0)) and parses_f64("-2.5E+1", Ok(-25.0))
expect parses_f64("inf", Ok(F64.infinity)) and parses_f64("-Infinity", Ok(-F64.infinity)) and parses_f64("NaN", Ok(F64.nan))
expect parses_f64("1e400", Ok(F64.infinity)) and parses_f64("-1e400", Ok(-F64.infinity)) and parses_f64("1e-400", Ok(0.0))
expect parses_f64("0e99999999999999999999", Ok(0.0)) and parses_f64("1.7976931348623159e308", Ok(F64.infinity))
expect ["", "+", "-", ".", "e5", "1e", "1e+", "1_0.5", "0x10", "0x1p3", " 1", "1 ", "1.2.3", "--1", "infinit", "nan1"].all(|text| parses_f64(text, Err({})))

# Empty input has no records; a blank line is one record with one empty field.
expect records("") == Ok([])
expect records("\n") == Ok([[""]])
expect records("a\n\nb") == Ok([["a"], [""], ["b"]])

# The final line break is optional, whichever style it is.
expect records("a,b\r\nc,d\r\n") == Ok([["a", "b"], ["c", "d"]])
expect records("a\n") == Ok([["a"]])
expect records("a,\n") == Ok([["a", ""]])

# A lone CR separates records; CR LF is a single break.
expect records("a\rb\r") == Ok([["a"], ["b"]])
expect records("a\r\rb") == Ok([["a"], [""], ["b"]])
expect records("a\r\n\nb") == Ok([["a"], [""], ["b"]])

# Fields hold any UTF-8 and control characters, and bare quotes are literal.
expect records("é,😀\t\u(0)\u(7f)") == Ok([["é", "😀\t\u(0)\u(7f)"]])
expect records("a\"b, \"c\" ") == Ok([["a\"b", " \"c\" "]])
expect records("\u(feff)a") == Ok([["\u(feff)a"]])

# Quoted fields hold delimiters and doubled quotes.
expect records("\"a,\r\nb\"\"\",\"\"") == Ok([["a,\r\nb\"", ""]])

# Text after a closing quote is an error located at that text.
expect error_of(records("x\n\"a\"b,c")) == Ok({ record: 2, field: 1, line: 2, column: 4, message: "a closing quote must be followed by `,`, a line break or the end of input" })

# An unterminated quoted field is an error located at its opening quote.
expect error_of(records("a,\"b")) == Ok({ record: 1, field: 2, line: 1, column: 3, message: "unterminated quoted field" })

# The composable parser accepts any bytes and reports malformed fields by offset.
expect Utf8.parse_bytes(CSV.parser, [0xFF, ',', 0xC3]) == Ok([[[0xFF], [0xC3]]])
expect Utf8.parse_bytes(CSV.parser, "a\n\"b".to_utf8()) == Err(ParseError({ message: "unterminated quoted field", offset: 2 }))

# The header helper splits off the column names.
expect CSV.split_header([]) == { header: [], rows: [] }
expect CSV.split_header([["a".to_utf8()], ["1".to_utf8()]]) == { header: ["a"], rows: [["1".to_utf8()]] }

# A hand-built record parser decodes every row, as in the module docs.
user : Parser(CSV.Record, { name : Str, age : U64 })
user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.u64))

expect CSV.parse_str(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])

# A field that does not decode names its record, field and position.
expect error_of(CSV.parse_str(user, "Ada,36\nAlan,forty\n")) == Ok({ record: 2, field: 2, line: 2, column: 6, message: "expected a U64, found `forty`" })

# Extra fields are an error carrying the record number.
expect error_of(CSV.parse_str(user, "Ada,36\n\"Al\nan\",41,x\n")) == Ok({ record: 2, field: 3, line: 3, column: 8, message: "the record has 3 fields, but the parser read only 2" })

# Too few fields is an error too.
expect error_of(CSV.parse_str(user, "Ada")) == Ok({ record: 1, field: 2, line: 1, column: 1, message: "expected another field, but the record has no more" })

# Regression: decoding fields that are not UTF-8 cannot crash, and messages
# quote a bounded, lossy excerpt.
expect {
	bad = [0xFF, 0xC3, 'x']
	error_of(CSV.decode(user, [[bad, "1".to_utf8()]])) == Ok({ record: 1, field: 1, line: 0, column: 0, message: "expected UTF-8 text, found `\u(fffd)\u(fffd)x`" })
}
expect {
	long = List.repeat('9', 5000)
	match CSV.decode(user, [["a".to_utf8(), long]]) {
		Err(InvalidCsv(problem)) => problem.message.count_utf8_bytes() < 100
		Ok(_) => False
	}
}
expect {
	all_fields = Parser.many(CSV.field(CSV.u64))
	error_of(CSV.decode(all_fields, [["1".to_utf8(), [0xFF, 0xFE]]])) == Ok({ record: 1, field: 2, line: 0, column: 0, message: "expected a U64, found `\u(fffd)`" })
}

# Type-directed decoding: header names choose fields, unknown columns are
# skipped, and absent optional columns are `Err(Missing)`.
Person : { name : Str, age : U64, admin : Bool, email : Try(Str, [Missing]) }

PersonResult : Try(List(Person), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])

expect {
	people : PersonResult
	people = CSV.parse("age,extra,name,admin\n36,zz,Ada,true\r\n7,,\"Bob, Jr\",FALSE\n")
	people == Ok([{ name: "Ada", age: 36, admin: True, email: Err(Missing) }, { name: "Bob, Jr", age: 7, admin: False, email: Err(Missing) }])
}

expect {
	people : PersonResult
	people = CSV.parse("email,name,age,admin\na@b.c,Ada,36,true\n")
	people == Ok([{ name: "Ada", age: 36, admin: True, email: Ok("a@b.c") }])
}

# A required column that is absent is reported by the compiler-added error.
expect {
	people : PersonResult
	people = CSV.parse("name,admin\nAda,true\n")
	people == Err(MissingRequiredField("age"))
}

# A cell that does not decode is located by record, field, line and column.
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\nAda,36,true\nBob,old,true\n")
	error_of(people) == Ok({ record: 3, field: 2, line: 3, column: 5, message: "expected a U64, found `old`" })
}

# Short and long rows are errors naming the record; blank lines are skipped.
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\n\nAda,36\n")
	error_of(people) == Ok({ record: 3, field: 3, line: 3, column: 1, message: "the record has 2 fields, fewer than the 3 columns" })
}
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\nAda,36,true,x\n\n")
	error_of(people) == Ok({ record: 2, field: 4, line: 2, column: 13, message: "the record has 4 fields, more than the 3 columns" })
}

# Empty cells are `Err(Null)` for `Try(_, [Null])` fields and `""` for strings.
expect {
	rows : Try(List({ a : Try(U64, [Null]), b : Str }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a,b\n,\n5,y\n")
	rows == Ok([{ a: Err(Null), b: "" }, { a: Ok(5), b: "y" }])
}

# Tags without payloads decode from their names.
expect {
	rows : Try(List({ status : [Active, Inactive] }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("status\nActive\nInactive\n")
	rows == Ok([{ status: Active }, { status: Inactive }])
}
expect {
	rows : Try(List({ status : [Active, Inactive] }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("status\nNope\n")
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, message: "unknown tag `Nope`" })
}

# Every number type uses the data grammar; signs only where they fit.
expect {
	rows : Try(List({ a : I8, b : U16, c : I32, d : U128, e : I128, f : Dec, g : F32, h : U8, i : I16, j : U32, k : I64 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a,b,c,d,e,f,g,h,i,j,k\n-128,+65535,-7,340282366920938463463374607431768211455,-1,-2.50,1.5,255,-32768,0,-9\n")
	rows == Ok([{ a: -128, b: 65535, c: -7, d: 340282366920938463463374607431768211455, e: -1, f: -2.5, g: 1.5, h: 255, i: -32768, j: 0, k: -9 }])
}
expect {
	rows : Try(List({ a : U8 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\n256\n")
	rows.is_err()
}
expect {
	rows : Try(List({ a : U8 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\n-1\n")
	rows.is_err()
}
expect {
	rows : Try(List({ a : Dec }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\n1e3\n")
	rows.is_err()
}

# Tags with payloads are rejected.
expect {
	rows : Try(List({ a : [Tagged(Str), Plain] }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\nTagged\n")
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, message: "a CSV cell cannot hold a tag with a payload" })
}

# Nested records are rejected.
expect {
	rows : Try(List({ a : { b : Str } }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\nx\n")
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, message: "a CSV cell cannot hold a record" })
}

# Normalized header names match snake_case fields.
expect {
	rows : Try(List({ first_name : Str, last_name : Str }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse_normalized(" First Name ,LAST--name\nAda,Lovelace\n")
	rows == Ok([{ first_name: "Ada", last_name: "Lovelace" }])
}

# Headerless input decodes into tuples, and the width must match.
expect {
	rows : Try(List((Str, U64, Bool)), [InvalidCsv(CSV.Error)])
	rows = CSV.parse_headerless("Ada,36,true\nAlan,41,false\n")
	rows == Ok([("Ada", 36, True), ("Alan", 41, False)])
}
expect {
	rows : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	rows = CSV.parse_headerless("Ada,36\nAlan\n")
	error_of(rows) == Ok({ record: 2, field: 2, line: 2, column: 1, message: "the record has 1 fields, fewer than the 2 columns" })
}

# Malformed CSV is reported before any decoding.
expect {
	people : PersonResult
	people = CSV.parse("name\n\"Ada")
	error_of(people) == Ok({ record: 2, field: 1, line: 2, column: 1, message: "unterminated quoted field" })
}

# A header with no data rows decodes to no rows, as does empty input.
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\n")
	people == Ok([])
}
expect {
	people : PersonResult
	people = CSV.parse("")
	people == Ok([])
}

# The examples in the documentation comments.
expect {
	people : Try(List({ name : Str, age : U64 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	people = CSV.parse("name,age\nAda,36\nAlan,41\n")
	people == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
}

expect CSV.parse_records("a,\"b,c\"\n") == Ok([["a".to_utf8(), "b,c".to_utf8()]])

expect {
	found = CSV.parse_records("name,age\nAda,36\n")?
	CSV.split_header(found) == { header: ["name", "age"], rows: [["Ada".to_utf8(), "36".to_utf8()]] }
}

expect {
	name : Parser(CSV.Record, { first : Str, last : Str })
	name = CSV.record(|first| |last| { first, last }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.string))
	CSV.parse_str(name, "Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
}

expect {
	items : Try(List({ sku : Str, count : U64, note : Try(Str, [Missing]), price : Try(Dec, [Null]) }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	items = CSV.parse("price,sku,count\n1.50,A1,3\n,B2,0\n")
	items == Ok([
		{ sku: "A1", count: 3, note: Err(Missing), price: Ok(1.50) },
		{ sku: "B2", count: 0, note: Err(Missing), price: Err(Null) },
	])
}

expect {
	names : Try(List({ first_name : Str }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	names = CSV.parse_normalized(" First Name \nAda\n")
	names == Ok([{ first_name: "Ada" }])
}

expect {
	pairs : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	pairs = CSV.parse_headerless("Ada,36\nAlan,41\n")
	pairs == Ok([("Ada", 36), ("Alan", 41)])
}
