import Parser
import Utf8
import Literal as Quotation
import Streaming
import CSVReader

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
## - [CSV.parse_with] runs a hand-built record parser (`record`, `field` and
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
CSV := { records : List(CSV.Record) }.{

	## Compare CSV documents by their raw records.
	is_eq : _

	## Hash a CSV document by its raw records.
	to_hash : _

	## One CSV record: its fields as raw bytes, in column order.
	##
	## Fields parsed from a `Str` are valid UTF-8; records built by hand may
	## hold any bytes.
	Record : List(List(U8))

	## Native CSV failures. Payload bytes are bounded excerpts of source data.
	Problem(p) : [Syntax(CSVReader.Problem), ExpectedValue({ expected : ValueType, bytes : List(U8) }), MissingField, WidthMismatch({ found : U64, expected : U64 }), EmptyHeader, DuplicateHeader(Str), UnsupportedCell([Record, Tuple, TagPayload]), InvalidValue, UnknownTag(List(U8)), ..p]

	## The scalar type expected by a cell decoder.
	ValueType : [Text, Boolean, Empty, U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, Dec, F32, F64]

	## One-based record, field, physical line and byte column; unknown positions are zero.
	Located(problem) : { record : U64, field : U64, line : U64, column : U64, problem : problem }

	## A CSV syntax or type-directed decoding failure.
	Error : Located(Parser.Problem(Problem([])))

	## A custom positional parser failure, preserving its own tagged problem.
	RowError(problem) : Located([ParserFailure(Parser.Problem(problem)), UnreadFields({ found : U64, read : U64 })])

	## Whether the first record is a header, and how its names are matched.
	## Raw validation keeps every record unchanged in all modes.
	HeaderMode : [Absent, Exact, Normalized]

	## Header checks. They only apply when a header mode is selected.
	Lints := { duplicate_headers : Bool ?? True, empty_headers : Bool ?? True }

	## Checked raw parsing; headers are absent unless explicitly selected.
	ValidationProps := { source : Str, name : Str ?? "<literal>", header : HeaderMode ?? Absent, lints : Lints ?? {} }

	## Checked decoding; header names match exactly unless overridden.
	DecodeValidationProps := { source : Str, name : Str ?? "<literal>", header : HeaderMode ?? Exact, lints : Lints ?? {} }

	## Check quoted CSV during compilation. Access raw records with `.records`.
	## The first record is data unless configured validation selects a header.
	from_quote : Str -> Try(CSV, [BadQuotedBytes(Str)])
	from_quote = |source| CSV.validate({ source: source }).map_ok(|records| CSV.{ records })

	## A quotation of typed CSV rows. Access the rows with `.value`.
	Decoded(row) : CsvDecoded(row)

	## Validate raw CSV records, rejecting selected header checks as well as
	## malformed CSV. Header checks compare normalized names in `Normalized`
	## mode; `Absent` does not interpret any record as column names.
	validate : ValidationProps -> Try(List(Record), [BadQuotedBytes(Str)])
	validate = |props| checked_records(props.source, props.name, props.header, props.lints)

	## Validate and decode rows into the inferred Roc type. `Exact` and
	## `Normalized` use a header; `Absent` decodes headerless tuples.
	validate_as : DecodeValidationProps -> Try(List(row), [BadQuotedBytes(Str)])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	validate_as = |props| {
		records = checked_records(props.source, props.name, props.header, props.lints)?
		Row : row
		match decode_records(props.source.to_utf8(), records, Row.parser_for(Format.Default), decoding_mode(props.header)) {
			Ok(values) => Ok(values)
			Err(InvalidCsv(problem)) => Err(checked_error(props.name, problem))
			Err(other) => Err(Quotation.error({ name: props.name, format: "CSV", message: "decoding failed: ${Str.inspect(other)}" }))
		}
	}

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
	parser : {} -> Parser(Utf8.Bytes, List(Record), Problem(p))
	parser = |_| Parser.custom(
		|bytes| {
			match scan_table(bytes) {
				Ok(records) => Ok({ value: records, rest: [] })
				Err(bad) => Err(ParseError({ problem: Syntax(scanner_problem(bad.problem)), offset: bad.at }))
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
	## user : Parser(CSV.Record, { name : Str, age : U64 }, _)
	## user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.u64({})))
	##
	## expect CSV.parse_with(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
	## ```
	parse_with : Parser(Record, a, p), Str -> Try(List(a), [InvalidCsv(Error), InvalidRow(RowError(p))])
	parse_with = |record_parser, text| {
		bytes = text.to_utf8()
		var $values = []
		var $pos = 0.U64
		var $index = 0.U64
		while $pos < bytes.len() {
			match scan_record(bytes, $pos) {
				Err(bad) => return Err(InvalidCsv(syntax_error(bytes, { field: bad.field, at: bad.at, problem: bad.problem, record: $index })))
				Ok({ fields, next }) => {
					match record_parser.run(fields) {
						Ok({ value, rest }) if rest.is_empty() => {
							$values = $values.append(value)
						}
						Ok({ rest, .. }) => return Err(InvalidRow(locate(bytes, leftover_error(record_parser, fields, rest.len(), $index + 1))))
						Err(ParseError({ problem, offset })) => return Err(InvalidRow(locate(bytes, { record: $index + 1, field: offset + 1, line: 0, column: 0, problem: ParserFailure(problem) })))
					}
					$pos = after_break(bytes, next)
					$index = $index + 1
				}
			}
		}
		Ok($values)
	}

	## Decode already parsed records with a hand-built record parser.
	##
	## Errors are as for [CSV.parse_with], numbered from the first record given,
	## with `line` and `column` set to `0`.
	decode : Parser(Record, a, p), List(Record) -> Try(List(a), [InvalidCsv(Error), InvalidRow(RowError(p))])
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
					return Err(InvalidRow(leftover_error(record_parser, fields, rest.len(), $index + 1)))
				}
				Err(ParseError({ problem, offset })) => {
					return Err(InvalidRow({ record: $index + 1, field: offset + 1, line: 0, column: 0, problem: ParserFailure(problem) }))
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
	## name : Parser(CSV.Record, Name, _)
	## name = CSV.record(|first| |last| { first, last }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.string({})))
	##
	## expect CSV.parse_with(name, "Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
	## ```
	record : a -> Parser(Record, a, p)
	record = |f| {
		Parser.const(f)
	}

	## Consume the next field of a [CSV.Record] using a parser for its bytes.
	##
	## The field parser must consume the whole field. Fails when the record has
	## no fields left.
	field : Parser(Utf8.Bytes, a, p) -> Parser(Record, a, [MissingField, UnconsumedField(U64), FieldProblem({ offset : U64, problem : Parser.Problem(p) })])
	field = |field_parser| {
		Parser.custom(
			|fields| {
				match fields.first() {
					Err(_) =>
						Err(ParseError({ problem: MissingField, offset: 0 }))

					Ok(bytes) => {
						match field_parser.run(bytes) {
							Ok({ value, rest }) if rest.is_empty() => Ok({ value, rest: fields.drop_first(1) })
							Ok({ rest, .. }) => Err(ParseError({ problem: UnconsumedField(rest.len()), offset: 0 }))
							Err(ParseError({ problem, offset })) => Err(ParseError({ problem: FieldProblem({ offset, problem }), offset: 0 }))
						}
					}
				}
			},
		)
	}

	## Parse one field as a valid UTF-8 string, kept verbatim (no trimming).
	string : {} -> Parser(Utf8.Bytes, Str, Problem(p))
	string = |_| Parser.custom(
		|bytes| {
			match Str.from_utf8(bytes) {
				Ok(text) => Ok({ value: text, rest: [] })
				Err(_) => Err(ParseError({ problem: expected_problem(Text, bytes), offset: 0 }))
			}
		},
	)

	## Parse one field as an unsigned 64-bit integer.
	##
	## The field must be ASCII decimal digits with an optional leading `+`, and
	## fit in a U64 (the grammar of Rust's `u64::from_str`). Whitespace,
	## underscores, signs other than `+` and `0x`-style prefixes are rejected.
	u64 : {} -> Parser(Utf8.Bytes, U64, Problem(p))
	u64 = |_| Parser.custom(
		|bytes| {
			match int_from_bytes(bytes, U64.from_str, False) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError({ problem: expected_problem(U64, bytes), offset: 0 }))
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
	f64 : {} -> Parser(Utf8.Bytes, F64, Problem(p))
	f64 = |_| Parser.custom(
		|bytes| {
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError({ problem: expected_problem(F64, bytes), offset: 0 }))
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
	## pairs : Try(List((Str, U64)), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
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
		parse_str : Format, State -> Try({ value : Str, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_str = |_, state| {
			bytes = current_cell(state)?
			match Str.from_utf8(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(Text, bytes)))
			}
		}

		## Read `true` or `false` in any letter case.
		parse_bool : Format, State -> Try({ value : Bool, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_bool = |_, state| {
			bytes = current_cell(state)?
			word = lowercase_ascii(bytes)
			if word == "true".to_utf8() {
				Ok({ value: True, rest: state })
			} else if word == "false".to_utf8() {
				Ok({ value: False, rest: state })
			} else {
				Err(cell_error(state, expected_problem(Boolean, bytes)))
			}
		}

		## Read ASCII digits with an optional `+` as a U8.
		parse_u8 : Format, State -> Try({ value : U8, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_u8 = |_, state| cell_int(state, U8.from_str, False, U8)

		## Read ASCII digits with an optional sign as a I8.
		parse_i8 : Format, State -> Try({ value : I8, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_i8 = |_, state| cell_int(state, I8.from_str, True, I8)

		## Read ASCII digits with an optional `+` as a U16.
		parse_u16 : Format, State -> Try({ value : U16, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_u16 = |_, state| cell_int(state, U16.from_str, False, U16)

		## Read ASCII digits with an optional sign as a I16.
		parse_i16 : Format, State -> Try({ value : I16, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_i16 = |_, state| cell_int(state, I16.from_str, True, I16)

		## Read ASCII digits with an optional `+` as a U32.
		parse_u32 : Format, State -> Try({ value : U32, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_u32 = |_, state| cell_int(state, U32.from_str, False, U32)

		## Read ASCII digits with an optional sign as a I32.
		parse_i32 : Format, State -> Try({ value : I32, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_i32 = |_, state| cell_int(state, I32.from_str, True, I32)

		## Read ASCII digits with an optional `+` as a U64.
		parse_u64 : Format, State -> Try({ value : U64, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_u64 = |_, state| cell_int(state, U64.from_str, False, U64)

		## Read ASCII digits with an optional sign as a I64.
		parse_i64 : Format, State -> Try({ value : I64, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_i64 = |_, state| cell_int(state, I64.from_str, True, I64)

		## Read ASCII digits with an optional `+` as a U128.
		parse_u128 : Format, State -> Try({ value : U128, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_u128 = |_, state| cell_int(state, U128.from_str, False, U128)

		## Read ASCII digits with an optional sign as a I128.
		parse_i128 : Format, State -> Try({ value : I128, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_i128 = |_, state| cell_int(state, I128.from_str, True, I128)

		## Read an optional sign, digits and an optional fraction.
		parse_dec : Format, State -> Try({ value : Dec, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_dec = |_, state| {
			bytes = current_cell(state)?
			match dec_from_bytes(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(Dec, bytes)))
			}
		}

		## Read a float with the grammar of [CSV.f64].
		parse_f32 : Format, State -> Try({ value : F32, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_f32 = |_, state| {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value: value.to_f32_wrap(), rest: state })
				Err(_) => Err(cell_error(state, expected_problem(F32, bytes)))
			}
		}

		## Read a float with the grammar of [CSV.f64].
		parse_f64 : Format, State -> Try({ value : F64, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_f64 = |_, state| {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(F64, bytes)))
			}
		}

		## An empty cell is null, so a `Try(a, [Null])` field is `Err(Null)`.
		parse_null : Format, State -> Try(State, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_null = |_, state| {
			bytes = current_cell(state)?
			if (bytes.is_empty()) Ok(state) else Err(cell_error(state, expected_problem(Empty, bytes)))
		}

		## A row is one record; a record inside a row is rejected.
		parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_record_start = |_, state| {
			if state.nested {
				Err(cell_error(state, UnsupportedCell(Record)))
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
			[InvalidCsv(Error), MissingRequiredField(Str)],
		)
		parse_record_field = |_, _, state| {
			match state.header.get(state.column) {
				Err(_) => Ok(Done(state))
				Ok(name) => Ok(TryField({ name, rest: State.{ row: state.row, header: state.header, column: state.column + 1, cell: state.column, record: state.record, nested: True } }))
			}
		}

		## The record ends after the last header column.
		parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_record_after_field = |_, state| {
			if (state.column >= state.header.len()) Ok(Done(state)) else Ok(Continue(state))
		}

		## The cell to skip is chosen by the column, so nothing is consumed.
		skip_record_field : Format, State -> Try(State, [InvalidCsv(Error), MissingRequiredField(Str)])
		skip_record_field = |_, state| Ok(state)

		## A headerless row is a tuple with one element per field.
		parse_tuple_start : Format, State, U64 -> Try(State, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_tuple_start = |_, state, len| {
			if state.nested {
				Err(cell_error(state, UnsupportedCell(Tuple)))
			} else if state.row.len() != len {
				Err(InvalidCsv({ record: state.record, field: smaller(state.row.len(), len) + 1, line: 0, column: 0, problem: width_problem(state.row.len(), len) }))
			} else {
				Ok(State.{ row: state.row, header: state.header, column: 0, cell: 0, record: state.record, nested: True })
			}
		}

		## Move to the cell of the next tuple element.
		parse_tuple_next : Format, State, U64, U64 -> Try(State, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_tuple_next = |_, state, index, _| Ok(State.{ row: state.row, header: state.header, column: index, cell: index, record: state.record, nested: True })

		## The error for a cell that holds no valid value.
		invalid_value : Format, State -> [InvalidCsv(Error), MissingRequiredField(Str)]
		invalid_value = |_, state| cell_error(state, InvalidValue)

		## Finish a tuple row.
		parse_tuple_end : Format, State, U64 -> Try(State, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_tuple_end = |_, state, _| Ok(state)

		## Report a required field absent from the input.
		missing_field : Format, Str -> [InvalidCsv(Error), MissingRequiredField(Str)]
		missing_field = |_, name| MissingRequiredField(name)

		## A tag without a payload is read from its name.
		parse_tag_union : Format, Encoding.ParseTagUnionSpec(a, Format, State, [InvalidCsv(Error), MissingRequiredField(Str)]), State -> Try({ value : a, rest : State }, [InvalidCsv(Error), MissingRequiredField(Str)])
		parse_tag_union = |format, spec, state| {
			bytes = current_cell(state)?
			Encoding.ParseTagUnionSpec.parse(
				spec,
				{
					tag: Str.from_utf8_lossy(bytes),
					encoding: format,
					state,
					start_payloads: |s, count| if (count == 0) Ok(s) else Err(cell_error(s, UnsupportedCell(TagPayload))),
					next_payload: |s, _, _| Ok(s),
					finish_payloads: |s, _| Ok(s),
					missing: cell_error(state, UnknownTag(error_excerpt(bytes))),
				},
			)
		}
	}

	## Limits for one unfinished record; completed rows leave the decoder.
	ReaderProps : CSVReader.Props

	## Decode raw byte records with the shared streaming API.
	decoder : ReaderProps -> Streaming.Decoder(CSVReader.State, Record, Error)
	decoder = |props| Streaming.decoder(CSVReader.new(props), raw_step)

	## Decode safely framed rows with a custom positional parser.
	positional_decoder : Parser(Record, row, p), ReaderProps -> Streaming.Decoder(CSVReader.State, row, [InvalidCsv(Error), InvalidRow(RowError(p))])
	positional_decoder = |parse_row, props| Streaming.decoder(CSVReader.new(props), |state, bytes, end| positional_step(parse_row, state, bytes, end))

	## Decode rows into the inferred type using the chosen header mode.
	typed_decoder : HeaderMode, ReaderProps -> Streaming.Decoder(TypedState, row, [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	typed_decoder = |header, props| {
		Row : row
		parse_row = Row.parser_for(Format.Default)
		Streaming.decoder(typed_state(header, props), |state, bytes, end| typed_step(parse_row, state, bytes, end))
	}

	## Writer options. Each emitted chunk is bounded; records are validated first.
	LineEnding : [CrLf, Lf, Cr]

	## Whether to quote only special fields or every field.
	Quoting : [Necessary, Always]

	## Writer byte limits and output dialect.
	WriteProps := { line_ending : LineEnding ?? CrLf, quoting : Quoting ?? Necessary, max_field_bytes : U64 ?? 16_777_216, max_record_bytes : U64 ?? 67_108_864, max_fields : U64 ?? 65_536 }

	## Invalid writer input or configured record bounds.
	WriteProblem : [EmptyRecord, FieldCountExceeded({ max : U64 }), FieldBytesExceeded({ max : U64 }), RecordBytesExceeded({ max : U64 })]

	## Validate and encode one byte record using necessary quoting and CRLF.
	encode_record : Record, WriteProps -> Try(List(U8), WriteProblem)
	encode_record = |row_fields, props| {
		if row_fields.is_empty() {
			return Err(EmptyRecord)
		}
		if row_fields.len() > props.max_fields {
			return Err(FieldCountExceeded({ max: props.max_fields }))
		}
		terminator = match props.line_ending {
			CrLf => ['\r', '\n']
			Lf => ['\n']
			Cr => ['\r']
		}
		var $size = terminator.len() + row_fields.len() - 1
		for cell_bytes in row_fields {
			if cell_bytes.len() > props.max_field_bytes {
				return Err(FieldBytesExceeded({ max: props.max_field_bytes }))
			}
			quoted = props.quoting == Always or cell_bytes.contains(',') or cell_bytes.contains('"') or cell_bytes.contains('\r') or cell_bytes.contains('\n') or row_fields == [[]]
			size = cell_bytes.len() + (if (quoted) 2 + cell_bytes.count_if(|byte| byte == '"') else 0)
			if size > props.max_record_bytes or $size > props.max_record_bytes - size {
				return Err(RecordBytesExceeded({ max: props.max_record_bytes }))
			}
			$size = $size + size
		}
		var $out = List.with_capacity($size)
		var $index = 0.U64
		for cell_bytes in row_fields {
			if $index > 0 {
				$out = $out.append(',')
			}
			quoted = props.quoting == Always or cell_bytes.contains(',') or cell_bytes.contains('"') or cell_bytes.contains('\r') or cell_bytes.contains('\n') or row_fields == [[]]
			if quoted {
				$out = $out.append('"')
				for byte in cell_bytes {
					$out = $out.append(byte)
					if byte == '"' {
						$out = $out.append('"')
					}
				}
				$out = $out.append('"')
			} else {
				$out = $out.concat(cell_bytes)
			}
			$index = $index + 1
		}
		Ok($out.concat(terminator))
	}

	## Encode validated records through the shared streaming writer.
	encoder : WriteProps -> Streaming.Encoder({}, Record, WriteProblem)
	encoder = |props| Streaming.encoder({}, |state, fields| encode_record(fields, props).map_ok(|bytes| { state, bytes }), |_| Ok([]))

	## The cursor for type-directed decoding: one record, the header names,
	## and indices into them. Used through [CSV.parse]; you do not need to name
	## it.
	State :: { row : Record, header : List(Str), column : U64, cell : U64, record : U64, nested : Bool }
}

TypedState : { reader : CSVReader.State, header : List(Str), header_read : Bool, mode : CSV.HeaderMode }

typed_state : CSV.HeaderMode, CSV.ReaderProps -> TypedState
typed_state = |mode, props| { reader: CSVReader.new(props), header: [], header_read: mode == Absent, mode }

stream_location : CSVReader.State, CSV.Located(p) -> CSV.Located(p)
stream_location = |reader, problem| {
	at = reader.last_starts.get(if (problem.field == 0) 0 else problem.field - 1) ?? (reader.last_starts.first() ?? reader.last_end)
	{ ..problem, line: at.line, column: at.column }
}

raw_step : CSVReader.State, List(U8), Streaming.InputEnd -> Streaming.Step(CSVReader.State, CSV.Record, CSV.Error)
raw_step = |state, bytes, end| {
	next = CSVReader.step(state, bytes, end)
	outcome = match next.outcome {
		Item(fields) => Item(fields)
		RecordError(error) => RecordError({ record: error.record, field: error.field, line: error.line, column: error.column, problem: Syntax(error.problem) })
		Failed(error) => Failed({ record: error.record, field: error.field, line: error.line, column: error.column, problem: Syntax(error.problem) })
		NeedInput => NeedInput
		Done => Done
	}
	{ state: next.state, consumed: next.consumed, outcome }
}

positional_step : Parser(CSV.Record, row, p), CSVReader.State, List(U8), Streaming.InputEnd -> Streaming.Step(CSVReader.State, row, [InvalidCsv(CSV.Error), InvalidRow(CSV.RowError(p))])
positional_step = |parse_row, reader, bytes, end| {
	next = raw_step(reader, bytes, end)
	outcome = match next.outcome {
		Item(fields) => match parse_row.run(fields) {
			Ok({ value, rest }) if rest.is_empty() => Item(value)
			Ok({ rest, .. }) => RecordError(InvalidRow(stream_location(next.state, leftover_error(parse_row, fields, rest.len(), next.state.record - 1))))
			Err(ParseError({ problem, offset })) => RecordError(InvalidRow(stream_location(next.state, { record: next.state.record - 1, field: offset + 1, line: 0, column: 0, problem: ParserFailure(problem) })))
		}
		RecordError(error) => RecordError(InvalidCsv(error))
		Failed(error) => Failed(InvalidCsv(error))
		NeedInput => NeedInput
		Done => Done
	}
	{ state: next.state, consumed: next.consumed, outcome }
}

typed_step : (CSV.State -> Try({ value : row, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errs])), TypedState, List(U8), Streaming.InputEnd -> Streaming.Step(TypedState, row, [InvalidCsv(CSV.Error), ..errs])
typed_step = |parse_row, initial, bytes, end| {
	var $state = initial
	var $consumed = 0.U64
	while True {
		next = raw_step($state.reader, bytes.drop_first($consumed), end)
		$consumed = $consumed + next.consumed
		$state = { ..$state, reader: next.state }
		match next.outcome {
			Item(fields) => {
				if !$state.header_read {
					$state = { ..$state, header: fields.map(|cell| if ($state.mode == Normalized) normalize_name(cell) else Str.from_utf8_lossy(cell)), header_read: True }
				} else if fields != [[]] {
					if $state.mode != Absent and fields.len() != $state.header.len() {
						error = stream_location(next.state, { record: next.state.record - 1, field: smaller(fields.len(), $state.header.len()) + 1, line: 0, column: 0, problem: width_problem(fields.len(), $state.header.len()) })
						return { state: $state, consumed: $consumed, outcome: RecordError(InvalidCsv(error)) }
					}
					parsed = parse_row(CSV.State.{ row: fields, header: $state.header, column: 0, cell: 0, record: next.state.record - 1, nested: False })
					outcome = match parsed {
						Ok({ value, .. }) => Item(value)
						Err(InvalidCsv(error)) => RecordError(InvalidCsv(stream_location(next.state, error)))
						Err(other) => RecordError(other)
					}
					return { state: $state, consumed: $consumed, outcome }
				}
			}
			NeedInput => return { state: $state, consumed: $consumed, outcome: NeedInput }
			Done => return { state: $state, consumed: $consumed, outcome: Done }
			Failed(error) => return { state: $state, consumed: $consumed, outcome: Failed(InvalidCsv(error)) }
			RecordError(error) => return { state: $state, consumed: $consumed, outcome: RecordError(InvalidCsv(error)) }
		}
	}
}

CsvDecoded(row) := { value : List(row) }.{

	## Decode quoted CSV using exact header names and enabled header checks.
	from_quote : Str -> Try(CSV.Decoded(row), [BadQuotedBytes(Str)])
		where [row.CSV.Parseable([InvalidCsv(CSV.Error), ..errs])]
	from_quote = |source| CSV.validate_as({ source: source }).map_ok(|value| CSV.Decoded.{ value })
}

## Decode every data record of `text` with a row parser built by `parser_for`.
decode_table : Str, (CSV.State -> Try({ value : row, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errs])), [HeaderExact, HeaderNormalized, Headerless] -> Try(List(row), [InvalidCsv(CSV.Error), ..errs])
decode_table = |text, parse_row, mode| {
	bytes = text.to_utf8()
	var $header = []
	var $index = 0.U64
	var $pos = 0.U64
	var $values = []
	while $pos < bytes.len() {
		match scan_record(bytes, $pos) {
			Err(bad) => return Err(InvalidCsv(syntax_error(bytes, { field: bad.field, at: bad.at, problem: bad.problem, record: $index })))
			Ok({ fields: row, next }) => {
				if $index == 0 and mode != Headerless {
					$header = row.map(|field| if (mode == HeaderNormalized) normalize_name(field) else Str.from_utf8_lossy(field))
				} else if row != [[]] {
					if mode != Headerless and row.len() != $header.len() {
						return Err(InvalidCsv(locate(bytes, { record: $index + 1, field: smaller(row.len(), $header.len()) + 1, line: 0, column: 0, problem: width_problem(row.len(), $header.len()) })))
					}
					match parse_row(CSV.State.{ row, header: $header, column: 0, cell: 0, record: $index + 1, nested: False }) {
						Ok({ value, rest: _ }) => {
							$values = $values.append(value)
						}
						Err(InvalidCsv(problem)) => return Err(InvalidCsv(locate(bytes, problem)))
						Err(other) => return Err(other)
					}
				}
				$pos = after_break(bytes, next)
				$index = $index + 1
			}
		}
	}
	Ok($values)
}

decode_records : List(U8), List(CSV.Record), (CSV.State -> Try({ value : row, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errs])), [HeaderExact, HeaderNormalized, Headerless] -> Try(List(row), [InvalidCsv(CSV.Error), ..errs])
decode_records = |bytes, records, parse_row, mode| {
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
				return Err(InvalidCsv(locate(bytes, { record: $index + 1, field: smaller(row.len(), width) + 1, line: 0, column: 0, problem: width_problem(row.len(), width) })))
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

decoding_mode : CSV.HeaderMode -> [HeaderExact, HeaderNormalized, Headerless]
decoding_mode = |header| match header {
	Absent => Headerless
	Exact => HeaderExact
	Normalized => HeaderNormalized
}

checked_error : Str, CSV.Error -> [BadQuotedBytes(Str)]
checked_error = |name, problem| Quotation.error({ name, format: "CSV", message: "record ${problem.record.to_str()}, field ${problem.field.to_str()}: ${Str.inspect(problem.problem)}", line: problem.line, column: problem.column })

checked_records : Str, Str, CSV.HeaderMode, CSV.Lints -> Try(List(CSV.Record), [BadQuotedBytes(Str)])
checked_records = |source, name, header, lints| {
	bytes = source.to_utf8()
	records = scan_table(bytes).map_err(|bad| checked_error(name, syntax_error(bytes, bad)))?
	if header != Absent {
		names = (records.first() ?? []).map(|field| if (header == Normalized) normalize_name(field) else Str.from_utf8_lossy(field))
		var $seen = Set.empty()
		var $index = 0
		for field in names {
			if lints.empty_headers and field == "" {
				return Err(checked_error(name, locate(bytes, { record: 1, field: $index + 1, line: 0, column: 0, problem: EmptyHeader })))
			}
			if lints.duplicate_headers and $seen.contains(field) {
				return Err(checked_error(name, locate(bytes, { record: 1, field: $index + 1, line: 0, column: 0, problem: DuplicateHeader(field) })))
			}
			$seen = $seen.insert(field)
			$index = $index + 1
		}
	}
	Ok(records)
}

smaller : U64, U64 -> U64
smaller = |a, b| if (a < b) a else b

width_problem : U64, U64 -> Parser.Problem(CSV.Problem([]))
width_problem = |found, expected| WidthMismatch({ found, expected })

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
			$out = $out.append(if (byte >= 'A' and byte <= 'Z') byte + 32 else byte)
		}
	}
	Str.from_utf8_lossy($out)
}

current_cell : CSV.State -> Try(List(U8), [InvalidCsv(CSV.Error)])
current_cell = |state| {
	match state.row.get(state.cell) {
		Ok(bytes) => Ok(bytes)
		Err(_) => Err(cell_error(state, MissingField))
	}
}

cell_error : CSV.State, Parser.Problem(CSV.Problem([])) -> [InvalidCsv(CSV.Error)]
cell_error = |state, problem| InvalidCsv({ record: state.record, field: state.cell + 1, line: 0, column: 0, problem })

cell_int : CSV.State, (Str -> Try(n, [BadNumStr])), Bool, CSV.ValueType -> Try({ value : n, rest : CSV.State }, [InvalidCsv(CSV.Error)])
cell_int = |state, from_str, signed, name| {
	bytes = current_cell(state)?
	match int_from_bytes(bytes, from_str, signed) {
		Ok(value) => Ok({ value, rest: state })
		Err(_) => Err(cell_error(state, expected_problem(name, bytes)))
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
		text = Str.from_utf8_lossy(if (negative) ['-'].concat(digits) else digits)
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

## Maximum source bytes retained in a failed cell payload.
excerpt_len : U64
excerpt_len = 40

## Detach diagnostic bytes from a potentially much larger field or source.
error_excerpt : List(U8) -> List(U8)
error_excerpt = |bytes| {
	var $out = List.with_capacity(U64.min(bytes.len(), excerpt_len))
	for byte in bytes.take_first(excerpt_len) {
		$out = $out.append(byte)
	}
	$out
}

expected_problem : CSV.ValueType, List(U8) -> Parser.Problem(CSV.Problem(p))
expected_problem = |expected, bytes| ExpectedValue({ expected, bytes: error_excerpt(bytes) })

## Explain why a record parser left fields unread, preferring the furthest
## failure (such as a field that stopped a `Parser.many`).
leftover_error : Parser(CSV.Record, a, p), CSV.Record, U64, U64 -> CSV.RowError(p)
leftover_error = |record_parser, fields, left, record| {
	used = fields.len() - left
	extra = { record, field: used + 1, line: 0, column: 0, problem: UnreadFields({ found: fields.len(), read: used }) }
	match record_parser.parse(fields) {
		Err(ParseError({ problem: UnexpectedInput, .. })) => extra
		Err(ParseError({ problem, offset })) => { record, field: offset + 1, line: 0, column: 0, problem: ParserFailure(problem) }
		_ => extra
	}
}

## A malformed quoted field: which record and field it is, where it starts
## (`at`) and what is wrong.
BadField : { record : U64, field : U64, at : U64, problem : [Unterminated, TextAfterQuote] }

scanner_problem : [Unterminated, TextAfterQuote] -> CSVReader.Problem
scanner_problem = |problem| match problem {
	Unterminated => UnterminatedQuotedField
	TextAfterQuote => TextAfterQuote
}

syntax_error : List(U8), BadField -> CSV.Error
syntax_error = |bytes, bad| {
	{ line, column } = line_column(bytes, bad.at)
	{ record: bad.record + 1, field: bad.field + 1, line, column, problem: Syntax(scanner_problem(bad.problem)) }
}

## Fill in the line and column of a decoding error from the source bytes.
## Only runs on the error path, so it rescans the input.
locate : List(U8), CSV.Located(p) -> CSV.Located(p)
locate = |bytes, problem| {
	{ line, column } = line_column(bytes, field_offset(bytes, problem.record - 1, problem.field - 1))
	{ record: problem.record, field: problem.field, line, column, problem: problem.problem }
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
		Ok('\r') => if (bytes.get(at + 1) == Ok('\n')) at + 2 else at + 1
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
}

## Offset of the first `,`, CR or LF at or after `start`, or the input length.
scan_unquoted : List(U8), U64 -> U64
scan_unquoted = |bytes, start| Utf8.find_any(bytes, start, delimiters)

# Bytes that end an unquoted field, found 16 at a time.
delimiters : Utf8.ByteClass
delimiters = Utf8.ByteClass.from_bytes([',', '\r', '\n'])

quote : Utf8.ByteClass
quote = Utf8.ByteClass.from_bytes(['"'])

is_delimiter : U8 -> Bool
is_delimiter = |byte| byte == ',' or byte == '\r' or byte == '\n'

## Scan a quoted field whose opening `"` is at `start`.
scan_quoted : List(U8), U64 -> Try({ field : List(U8), next : U64 }, { at : U64, problem : [Unterminated, TextAfterQuote] })
scan_quoted = |bytes, start| {
	len = bytes.len()
	var $field = []
	var $chunk_start = start + 1
	var $pos = Utf8.find_any(bytes, start + 1, quote)
	while $pos < len {
		if bytes.get($pos) == Ok('"') {
			chunk = bytes.sublist({ start: $chunk_start, len: $pos - $chunk_start })
			# A field with no doubled quotes stays a slice of the input.
			$field = if ($field.is_empty()) chunk else $field.concat(chunk)
			if bytes.get($pos + 1) == Ok('"') {
				$field = $field.append('"')
				$chunk_start = $pos + 2
				$pos = Utf8.find_any(bytes, $pos + 2, quote)
			} else {
				after = $pos + 1
				if after >= len or is_delimiter(bytes.get(after) ?? 0) {
					return Ok({ field: $field, next: after })
				}
				return Err({ at: after, problem: TextAfterQuote })
			}
		} else {
			$pos = Utf8.find_any(bytes, $pos + 1, quote)
		}
	}
	Err({ at: start, problem: Unterminated })
}

all_digits : List(U8) -> Bool
all_digits = |bytes| !bytes.is_empty() and bytes.all(|b| b >= '0' and b <= '9')

lowercase_ascii : List(U8) -> List(U8)
lowercase_ascii = |bytes| bytes.map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b)

## Parse a float using the grammar documented on `CSV.f64({})`.
decimal_f64 : List(U8) -> Try(F64, [NotAFloat])
decimal_f64 = |bytes| {
	{ negative, unsigned } =
		match bytes {
			['-', .. as rest] => { negative: True, unsigned: rest }
			['+', .. as rest] => { negative: False, unsigned: rest }
			_ => { negative: False, unsigned: bytes }
		}
	signed = |value| if (negative) -value else value
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
	actual = Utf8.parse_bytes(CSV.u64({}), text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted
		(Err(_), Err({})) => True
		_ => False
	}
}

parses_f64 : Str, Try(F64, {}) -> Bool
parses_f64 = |text, expected| {
	actual = Utf8.parse_bytes(CSV.f64({}), text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted or (value.is_nan() and wanted.is_nan())
		(Err(_), Err({})) => True
		_ => False
	}
}

records : Str -> Try(List(List(Str)), [InvalidCsv(CSV.Error)])
records = |text| CSV.parse_records(text).map_ok(|found| found.map(|fields| fields.map(Str.from_utf8_lossy)))

row_error_of : Try(a, [InvalidRow(CSV.RowError(p)), ..others]) -> Try(CSV.RowError(p), [NoError])
row_error_of = |result| match result {
	Err(InvalidRow(error)) => Ok(error)
	_ => Err(NoError)
}

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
expect error_of(records("x\n\"a\"b,c")) == Ok({ record: 2, field: 1, line: 2, column: 4, problem: Syntax(TextAfterQuote) })

# An unterminated quoted field is an error located at its opening quote.
expect error_of(records("a,\"b")) == Ok({ record: 1, field: 2, line: 1, column: 3, problem: Syntax(UnterminatedQuotedField) })

# The composable parser accepts any bytes and reports malformed fields by offset.
expect Utf8.parse_bytes(CSV.parser({}), [0xFF, ',', 0xC3]) == Ok([[[0xFF], [0xC3]]])
expect Utf8.parse_bytes(CSV.parser({}), "a\n\"b".to_utf8()) == Err(ParseError({ problem: Syntax(UnterminatedQuotedField), offset: 2 }))

# The header helper splits off the column names.
expect CSV.split_header([]) == { header: [], rows: [] }
expect CSV.split_header([["a".to_utf8()], ["1".to_utf8()]]) == { header: ["a"], rows: [["1".to_utf8()]] }

# A hand-built record parser decodes every row, as in the module docs.
user : Parser(CSV.Record, { name : Str, age : U64 }, _)
user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.u64({})))

expect CSV.parse_with(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])

# A field that does not decode names its record, field and position.
expect row_error_of(CSV.parse_with(user, "Ada,36\nAlan,forty\n")) == Ok({ record: 2, field: 2, line: 2, column: 6, problem: ParserFailure(FieldProblem({ offset: 0, problem: ExpectedValue({ expected: U64, bytes: "forty".to_utf8() }) })) })

# Extra fields are an error carrying the record number.
expect row_error_of(CSV.parse_with(user, "Ada,36\n\"Al\nan\",41,x\n")) == Ok({ record: 2, field: 3, line: 3, column: 8, problem: UnreadFields({ found: 3, read: 2 }) })

# Too few fields is an error too.
expect row_error_of(CSV.parse_with(user, "Ada")) == Ok({ record: 1, field: 2, line: 1, column: 1, problem: ParserFailure(MissingField) })

# Invalid UTF-8 remains raw source bytes in bounded structured payloads.
expect {
	bad = [0xFF, 0xC3, 'x']
	row_error_of(CSV.decode(user, [[bad, "1".to_utf8()]])) == Ok({ record: 1, field: 1, line: 0, column: 0, problem: ParserFailure(FieldProblem({ offset: 0, problem: ExpectedValue({ expected: Text, bytes: bad }) })) })
}
expect {
	long = List.repeat('9', 5000)
	match CSV.decode(user, [["a".to_utf8(), long]]) {
		Err(InvalidRow({ problem: ParserFailure(FieldProblem({ problem: ExpectedValue({ bytes, .. }), .. })), .. })) => bytes.len() == 40
		_ => False
	}
}
expect {
	all_fields = Parser.many(CSV.field(CSV.u64({})))
	row_error_of(CSV.decode(all_fields, [["1".to_utf8(), [0xFF, 0xFE]]])) == Ok({ record: 1, field: 2, line: 0, column: 0, problem: ParserFailure(FieldProblem({ offset: 0, problem: ExpectedValue({ expected: U64, bytes: [0xFF, 0xFE] }) })) })
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
	error_of(people) == Ok({ record: 3, field: 2, line: 3, column: 5, problem: ExpectedValue({ expected: U64, bytes: "old".to_utf8() }) })
}

# Short and long rows are errors naming the record; blank lines are skipped.
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\n\nAda,36\n")
	error_of(people) == Ok({ record: 3, field: 3, line: 3, column: 1, problem: WidthMismatch({ found: 2, expected: 3 }) })
}
expect {
	people : PersonResult
	people = CSV.parse("name,age,admin\nAda,36,true,x\n\n")
	error_of(people) == Ok({ record: 2, field: 4, line: 2, column: 13, problem: WidthMismatch({ found: 4, expected: 3 }) })
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
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, problem: UnknownTag("Nope".to_utf8()) })
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
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, problem: UnsupportedCell(TagPayload) })
}

# Nested records are rejected.
expect {
	rows : Try(List({ a : { b : Str } }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse("a\nx\n")
	error_of(rows) == Ok({ record: 2, field: 1, line: 2, column: 1, problem: UnsupportedCell(Record) })
}

# Normalized header names match snake_case fields.
expect {
	rows : Try(List({ first_name : Str, last_name : Str }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse_normalized(" First Name ,LAST--name\nAda,Lovelace\n")
	rows == Ok([{ first_name: "Ada", last_name: "Lovelace" }])
}

# Headerless input decodes into tuples, and the width must match.
expect {
	rows : Try(List((Str, U64, Bool)), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse_headerless("Ada,36,true\nAlan,41,false\n")
	rows == Ok([("Ada", 36, True), ("Alan", 41, False)])
}
expect {
	rows : Try(List((Str, U64)), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	rows = CSV.parse_headerless("Ada,36\nAlan\n")
	error_of(rows) == Ok({ record: 2, field: 2, line: 2, column: 1, problem: WidthMismatch({ found: 1, expected: 2 }) })
}

# Malformed CSV is reported before any decoding.
expect {
	people : PersonResult
	people = CSV.parse("name\n\"Ada")
	error_of(people) == Ok({ record: 2, field: 1, line: 2, column: 1, problem: Syntax(UnterminatedQuotedField) })
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
	name : Parser(CSV.Record, { first : Str, last : Str }, _)
	name = CSV.record(|first| |last| { first, last }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.string({})))
	CSV.parse_with(name, "Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
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
	pairs : Try(List((Str, U64)), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
	pairs = CSV.parse_headerless("Ada,36\nAlan,41\n")
	pairs == Ok([("Ada", 36), ("Alan", 41)])
}

expect {
	table : CSV
	table =
		\\name,age
		\\Ada,36
	table.records == [["name".to_utf8(), "age".to_utf8()], ["Ada".to_utf8(), "36".to_utf8()]]
}

expect {
	people : CSV.Decoded({ name : Str, age : U64 })
	people = "name,age\nAda,36\n"
	people.value == [{ name: "Ada", age: 36 }]
}

# Raw CSV never guesses that a first record contains header names.
expect CSV.validate({ source: "name,name,\n" }) == Ok([["name".to_utf8(), "name".to_utf8(), []]])

expect CSV.validate({ source: "name,name\n", header: Exact, name: "names.csv" }) == Err(BadQuotedBytes("names.csv:1:6: CSV: record 1, field 2: DuplicateHeader(\"name\")"))
expect CSV.validate({ source: "name,\n", header: Exact }) == Err(BadQuotedBytes("<literal>:1:6: CSV: record 1, field 2: EmptyHeader"))
expect CSV.validate({ source: "", header: Exact }) == Ok([])

expect {
	source = "First-Name,FIRST_NAME\nAda,Alan\n"
	exact = CSV.validate({ source, header: Exact }).is_ok()
	normalized = match CSV.validate({ source, header: Normalized }) {
		Err(BadQuotedBytes(message)) => message.contains("DuplicateHeader") and message.contains("first_name")
		Ok(_) => False
	}
	exact and normalized
}

expect {
	source = "name,name,\nAda,Alan,\n"
	CSV.validate({ source, header: Exact, lints: { duplicate_headers: False, empty_headers: False } }) == CSV.parse_records(source).map_err(|_| BadQuotedBytes("unexpected parse error"))
}

expect {
	match CSV.validate({ source: "name,name\n\"unterminated", header: Exact, name: "bad.csv" }) {
		Err(BadQuotedBytes(message)) => message.starts_with("bad.csv:2:1:") and message.contains("UnterminatedQuotedField") and !message.contains("DuplicateHeader")
		Ok(_) => False
	}
}

expect {
	result : Try(List({ first_name : Str }), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: " First Name \nAda\n", header: Normalized })
	result == Ok([{ first_name: "Ada" }])
}

expect {
	result : Try(List({ name : Str }), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: "name,name\nAda,Alan", lints: { duplicate_headers: False } })
	result == Ok([{ name: "Alan" }])
}

expect {
	result : Try(List((Str, U64)), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: "Ada,36\nAlan,41", header: Absent })
	result == Ok([("Ada", 36), ("Alan", 41)])
}

expect {
	result : Try(List({ name : Str, age : U64 }), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: "name,age\nAda,nope", name: "people.csv" })
	match result {
		Err(BadQuotedBytes(message)) => message.starts_with("people.csv:2:5:") and message.contains("record 2, field 2")
		Ok(_) => False
	}
}

expect {
	result : Try(List({ required : Str }), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: "other\nvalue", name: "missing.csv" })
	match result {
		Err(BadQuotedBytes(message)) => message.starts_with("missing.csv: CSV:") and message.contains("required")
		Ok(_) => False
	}
}

CheckedCsvFailure := [Never].{
	parser_for : CSV.Format -> (CSV.State -> Try({ value : CheckedCsvFailure, rest : CSV.State }, [InvalidCsv(CSV.Error), CustomCsvFailure(Str)]))
	parser_for = |_| |_| Err(CustomCsvFailure("custom detail"))
}

expect {
	result : Try(List(CheckedCsvFailure), [BadQuotedBytes(Str)])
	result = CSV.validate_as({ source: "header\nvalue" })
	match result {
		Err(BadQuotedBytes(message)) => message.contains("CustomCsvFailure") and message.contains("custom detail")
		Ok(_) => False
	}
}

# The streaming scanner and complete parser agree at every byte boundary,
# including UTF-8, doubled quotes, CRLF, and malformed quoted fields.
stream_records_for_test : List(List(U8)) -> Try(List(CSV.Record), [InvalidCsv(CSV.Error)])
stream_records_for_test = |chunks| {
	var $rows = []
	for event in Streaming.read_iter(chunks.iter(), CSV.decoder({})) {
		match event {
			Ok(Value(row)) => {
				$rows = $rows.append(row)
			}
			Ok(Finished(_)) => return Ok($rows)
			Ok(Rejected(error)) | Err(ParseErr(error)) => return Err(InvalidCsv(error))
			Err(_) => crash "streaming protocol failure"
		}
	}
	crash "missing finished event"
}
expect ["", "\n", "a,b\r\nc,d", "a\r\n\nb", "é,😀", "\u(feff)a", "a\"b, \"c\" ", "\"a\r\nb\",\"x\"\"y\"\n", "a,\"b", "x\n\"a\"b,c"].all(
	|text| {
		bytes = text.to_utf8()
		(0..=bytes.len()).iter().fold(True, |good, split| good and stream_records_for_test([bytes.take_first(split), [], bytes.drop_first(split)]) == CSV.parse_records(text))
	},
)
expect CSV.encode_record(["a,b".to_utf8(), "x\"y".to_utf8()], {}) == Ok("\"a,b\",\"x\"\"y\"\r\n".to_utf8())
expect CSV.encode_record([[]], {}) == Ok("\"\"\r\n".to_utf8())
expect CSV.encode_record([], {}).is_err()
expect CSV.encode_record(["a".to_utf8()], { max_record_bytes: 3 }) == Ok("a\r\n".to_utf8())
expect CSV.encode_record(["a".to_utf8()], { max_record_bytes: 2 }).is_err()
expect {
	encoded = List.from_iter(Streaming.write_iter([["a,b".to_utf8(), "é".to_utf8()], [[]]].iter(), CSV.encoder({}).with_chunk_bytes(1)))
	encoded.all(|result| result.map_ok(|bytes| bytes.len() <= 1) == Ok(True)) and stream_records_for_test(encoded.map(|chunk| chunk ?? [])) == Ok([["a,b".to_utf8(), "é".to_utf8()], [[]]])
}
expect {
	decode = Parser.many(CSV.field(CSV.u64({})))
	events = Streaming.read_iter(["1\nwrong\n2\n".to_utf8()].iter(), CSV.positional_decoder(decode, {}).with_errors({ error_policy: ReportInvalidRecords }))
	match Streaming.collect_report(events) {
		Ok({ values, errors, report }) => values == [[1], [2]] and errors.len() == 1 and errors.all(
			|error| match error {
				InvalidRow(at) => at.line == 2
				_ => False
			},
		) and report == { emitted_items: 2, rejected_records: 1 }
		Err(_) => False
	}
}
expect {
	result : Try(List({ name : Str, age : U64 }), _)
	result = Streaming.collect(Streaming.read_iter(["name,age\r".to_utf8(), "\nAda,36\nAlan,41".to_utf8()].iter(), CSV.typed_decoder(Exact, {})))
	result == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
}
expect {
	result : Try(List({ id : U64 }), _)
	result = CSV.parse("id\nwrong\n\"unterminated")
	match result {
		Err(InvalidCsv(error)) => error.record == 2 and error.problem == ExpectedValue({ expected: U64, bytes: "wrong".to_utf8() })
		_ => False
	}
}
expect {
	decode = Parser.many(CSV.field(CSV.u64({})))
	match CSV.parse_with(decode, "bad\n\"unterminated") {
		Err(InvalidRow(error)) => error.record == 1 and error.problem == ParserFailure(FieldProblem({ offset: 0, problem: ExpectedValue({ expected: U64, bytes: "bad".to_utf8() }) }))
		_ => False
	}
}

expect CSV.encode_record(["a".to_utf8(), "b".to_utf8()], { quoting: Always, line_ending: Lf }) == Ok("\"a\",\"b\"\n".to_utf8())
expect CSV.encode_record(["a".to_utf8()], { line_ending: Cr }) == Ok("a\r".to_utf8())

expect Streaming.collect(Streaming.read_iter(["\"a\"\"\"\n".to_utf8()].iter(), CSV.decoder({ max_field_bytes: 2 }))) == Ok([["a\"".to_utf8()]])
expect Streaming.collect(Streaming.read_iter(["\"a\"\"\"\n".to_utf8()].iter(), CSV.decoder({ max_field_bytes: 1 }))).is_err()
expect Streaming.collect(Streaming.read_iter(["a,b".to_utf8()].iter(), CSV.decoder({ max_fields: 2 }))) == Ok([["a".to_utf8(), "b".to_utf8()]])
expect Streaming.collect(Streaming.read_iter(["a,b,c".to_utf8()].iter(), CSV.decoder({ max_fields: 2 }))).is_err()
expect Streaming.collect(Streaming.read_iter(["a,b".to_utf8()].iter(), CSV.decoder({ max_record_bytes: 3 }))) == Ok([["a".to_utf8(), "b".to_utf8()]])
expect Streaming.collect(Streaming.read_iter(["a,b".to_utf8()].iter(), CSV.decoder({ max_record_bytes: 2 }))).is_err()

# User parser tags and byte offsets survive positional row decoding unchanged.
expect {
	custom = CSV.field(Parser.custom(|_| Err(ParseError({ offset: 2, problem: OutsideRange({ max: 10.U64 }) }))))
	match CSV.parse_with(custom, "123") {
		Err(InvalidRow({ record: 1, field: 1, line: 1, column: 1, problem: ParserFailure(FieldProblem({ offset: 2, problem: OutsideRange({ max: 10 }) })) })) => True
		_ => False
	}
}

# Native CSV fields compose with UTF-8 and caller-specific problems.
expect {
	row = CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string({})))
		.keep(CSV.field(Utf8.digits({})))
	CSV.parse_with(row, "apples,42\n") == Ok([{ name: "apples", count: 42 }])
}
expect {
	even = Utf8.digits({}).map(
		|value| if value % 2 == 0 {
			Ok(value)
		} else {
			Err(OddCount(value))
		},
	).flatten()
	row = CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string({})))
		.keep(CSV.field(even))
	match CSV.parse_with(row, "apples,3\n") {
		Err(InvalidRow(error)) => error.record == 1 and error.field == 2 and error.line == 1 and error.column == 8 and error.problem == ParserFailure(FieldProblem({ offset: 0, problem: OddCount(3) }))
		_ => False
	}
}
