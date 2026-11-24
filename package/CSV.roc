import DiagnosticOwnership
import Diagnostic
import Parser
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
## Read a file as typed rows, text fields, or raw bytes:
##
## - [CSV.decode_str] decodes rows into records whose field names match the header
##   row, with the row type chosen by type inference;
## - [CSV.rows_parser] runs a hand-built record parser (`record`, `field` and
##   `Parser.keep`) over every row, matching columns by position;
## - [CSV.parse_string_records] returns every field as a `Str`;
## - [CSV.parse_str] returns the raw fields.
##
## ```roc
## Person : { name : Str, age : U64 }
##
## people : Try(List(Person), [InvalidCsv(CSV.Error)])
## people = CSV.decode_str("name,age\nAda,36\nAlan,41\n", {})
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
	Problem(p) : [Syntax(CSVReader.Problem), ExpectedValue({ expected : ValueType, source : Diagnostic.Excerpt }), MissingField, WidthMismatch({ found : U64, expected : U64 }), EmptyHeader, DuplicateHeader(Str), MissingRequiredField(Str), EmptyRecord, LimitExceeded({ resource : Resource, max : U64, observed : Diagnostic.Observed }), UnsupportedCell([Record, Tuple, TagPayload]), InvalidValue, UnknownTag(Diagnostic.Excerpt), ..p]

	## The scalar type expected by a cell decoder.
	ValueType : [Text, Boolean, Empty, U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, Dec, F32, F64]

	## A CSV syntax or type-directed decoding failure.
	Error : Diagnostic(Problem([]))

	## A custom positional parser failure, preserving its own tagged problem.
	RowError(problem) : Diagnostic([ParserFailure(Parser.Problem(problem)), UnreadFields({ found : U64, read : U64 })])

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
	from_quote = |source| CSV.validate({ source: source }).map_ok(|records| CSV.{ records }).map_err(|InvalidCsv(error)| checked_error("<literal>", error))

	## A quotation of typed CSV rows. Access the rows with `.value`.
	Decoded(row) : CsvDecoded(row)

	## Validate raw CSV records, rejecting selected header checks as well as
	## malformed CSV. Header checks compare normalized names in `Normalized`
	## mode; `Absent` does not interpret any record as column names.
	validate : ValidationProps -> Try(List(Record), [InvalidCsv(Error)])
	validate = |props| checked_records(props.source, props.header, props.lints).map_err(|InvalidCsv(error)| InvalidCsv({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) }))

	## Validate and decode rows into the inferred Roc type. `Exact` and
	## `Normalized` use a header; `Absent` decodes headerless tuples.
	validate_as : DecodeValidationProps -> Try(List(row), [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	validate_as = |props| {
		records = CSV.validate({ source: props.source, header: props.header, lints: props.lints, name: props.name })?
		Row : row
		decode_records(props.source.to_utf8(), records, Row.parser_for(CsvFormat.Default), decoding_mode(props.header)).map_err(
			|failure| match failure {
				InvalidCsv(error) => InvalidCsv({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) })
				other => other
			},
		)
	}

	## Parse CSV text into raw records without decoding them.
	##
	## ```roc
	## expect CSV.parse_str("a,\"b,c\"\n", {}).map_ok(|document| document.records) == Ok([["a".to_utf8(), "b,c".to_utf8()]])
	## ```

	## Parse every field as text, including the header as an ordinary record.
	##
	## Fields are kept verbatim, with CSV quoting removed and doubled quotes
	## decoded. Empty input has no records; a blank line has one empty field.
	## Uses the same grammar and syntax diagnostics as [CSV.parse_str].
	##
	## ```roc
	## expect CSV.parse_string_records("name,note\nAda,\"said \"\"hello\"\"\"\n", {}) == Ok([["name", "note"], ["Ada", "said \"hello\""]])
	## ```
	parse_string_records : Str, Props -> Try(List(List(Str)), [InvalidCsv(Error)])
	parse_string_records = |text, props| {
		match props.validation {
			SyntaxOnly => {}
			Checked(lints) => {
				_ = checked_records(text, props.header, lints)?
			}
		}
		bytes = text.to_utf8()
		len = bytes.len()
		match scan_table_as(bytes, |field_bytes, start, next, quoted| string_field(text, len, field_bytes, start, next, quoted)) {
			Ok(records) => Ok(records)
			Err(bad) => Err(InvalidCsv(syntax_error(bytes, bad)))
		}
	}

	## A parser for a whole CSV file, for use inside larger parsers.
	##
	## It reads records until its input ends, so it consumes all of it or fails
	## with the offset of the malformed field. It accepts any bytes.
	parser : Props -> Parser(Parser.Utf8.Bytes, CSV, [InvalidCsv(Error)])
	parser = |props| Parser.custom(
		|bytes| {
			match CSV.parse_bytes(bytes, props) {
				Ok(value) => Ok({ value, rest: [] })
				Err(InvalidCsv(error)) => Err(ParseError(Diagnostic.{ problem: InvalidCsv(error), location: error.location, path: error.path }))
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
	##     records = CSV.parse_str("name,age\nAda,36\n", {}).map_ok(|document| document.records)?
	##     CSV.{records:records}.split_header() == { header: ["name", "age"], rows: [["Ada".to_utf8(), "36".to_utf8()]] }
	## }
	## ```
	split_header : CSV -> { header : List(Str), rows : List(Record) }
	split_header = |document| {
		records = document.records
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
	## expect CSV.rows_parser({row:user}).parse_str("Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
	## ```

	## Decode already parsed records with a hand-built record parser.
	##
	## Errors are as for [CSV.rows_parser], numbered from the first record given,
	## with an explicitly unavailable source location.

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
	## expect CSV.rows_parser({row:name}).parse_str("Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
	## ```
	record : a -> Parser(Record, a, p)
	record = |f| {
		Parser.const(f)
	}

	## Consume the next field of a [CSV.Record] using a parser for its bytes.
	##
	## The field parser must consume the whole field. Fails when the record has
	## no fields left.
	field : Parser(Parser.Utf8.Bytes, a, p) -> Parser(Record, a, [MissingField, UnconsumedField(U64), FieldProblem(Diagnostic(Parser.Problem(p)))])
	field = |field_parser| {
		Parser.custom(
			|fields| {
				match fields.first() {
					Err(_) =>
						Err(ParseError(Diagnostic.{ problem: MissingField, location: InputOffset(0), path: [] }))

					Ok(bytes) => {
						match field_parser.run(bytes) {
							Ok({ value, rest }) if rest.is_empty() => Ok({ value, rest: fields.drop_first(1) })
							Ok({ rest, .. }) => Err(ParseError(Diagnostic.{ problem: UnconsumedField(rest.len()), location: InputOffset(0), path: [] }))
							Err(ParseError(error)) => Err(ParseError(Diagnostic.{ problem: FieldProblem(decoded_error(error)), location: InputOffset(0), path: error.path }))
						}
					}
				}
			},
		)
	}

	## Parse one field as a valid UTF-8 string, kept verbatim (no trimming).
	string : {} -> Parser(Parser.Utf8.Bytes, Str, Problem(p))
	string = |_| Parser.custom(
		|bytes| {
			match Str.from_utf8(bytes) {
				Ok(text) => Ok({ value: text, rest: [] })
				Err(_) => Err(ParseError(Diagnostic.{ problem: expected_problem(Text, bytes), location: InputOffset(0), path: [] }))
			}
		},
	)

	## Parse one field as an unsigned 64-bit integer.
	##
	## The field must be ASCII decimal digits with an optional leading `+`, and
	## fit in a U64 (the grammar of Rust's `u64::from_str`). Whitespace,
	## underscores, signs other than `+` and `0x`-style prefixes are rejected.
	u64 : {} -> Parser(Parser.Utf8.Bytes, U64, Problem(p))
	u64 = |_| Parser.custom(
		|bytes| {
			match int_from_bytes(bytes, U64.from_str, False) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError(Diagnostic.{ problem: expected_problem(U64, bytes), location: InputOffset(0), path: [] }))
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
	f64 : {} -> Parser(Parser.Utf8.Bytes, F64, Problem(p))
	f64 = |_| Parser.custom(
		|bytes| {
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: [] })
				Err(_) => Err(ParseError(Diagnostic.{ problem: expected_problem(F64, bytes), location: InputOffset(0), path: [] }))
			}
		},
	)

	## Names the requirement that a row type can be decoded from CSV, so a
	## signature can say "CSV-parseable" without naming the internal format
	## and state types.
	row.Parseable(errs) :
		where [
			row.parser_for : CsvFormat(errs) -> (State -> Try({ value : row, rest : State }, errs)),
		]

	## Typed rows use exact headers unless another mode is explicitly selected.
	DecodeProps := { header : HeaderMode ?? Exact }

	## Parse a complete CSV document and retain its raw records.
	Props := { header : HeaderMode ?? Absent, validation : [SyntaxOnly, Checked(Lints)] ?? SyntaxOnly }
	parse_str : Str, Props -> Try(CSV, [InvalidCsv(Error)])
	parse_str = |source, props| CSV.parse_bytes(source.to_utf8(), props)

	## Parse raw CSV bytes without imposing an encoding on field payloads.
	parse_bytes : List(U8), Props -> Try(CSV, [InvalidCsv(Error)])
	parse_bytes = |bytes, props| {
		records = scan_table(bytes).map_err(|bad| InvalidCsv(syntax_error(bytes, bad)))?
		match props.validation {
			SyntaxOnly => {}
			Checked(lints) => check_headers(bytes, records, props.header, lints)?
		}
		Ok(CSV.{ records })
	}

	## Decode typed rows directly from source using the existing whole-input scanner.
	decode_str : Str, DecodeProps -> Try(List(row), [InvalidCsv(Error), ..errs]) where [row.Parseable([InvalidCsv(Error), ..errs])]
	decode_str = |source, props| {
		Row : row
		decode_table(source, Row.parser_for(CsvFormat.Default), decoding_mode(props.header))
	}

	## Decode an already parsed document without inventing physical coordinates.
	decode : CSV, DecodeProps -> Try(List(row), [InvalidCsv(Error), ..errs]) where [row.Parseable([InvalidCsv(Error), ..errs])]
	decode = |document, props| {
		Row : row
		decode_records([], document.records, Row.parser_for(CsvFormat.Default), decoding_mode(props.header))
	}

	## Decode this document with a positional row grammar.
	decode_rows : CSV, RowsProps(row, problem) -> Try(List(row), [InvalidCsv(Error), InvalidRow(RowError(problem))])
	decode_rows = |document, props| decode_rows_core(props.row, document.records)

	## A configured whole-document parser for hand-built positional row grammars.
	RowsProps(row, problem) := { row : Parser(Record, row, problem) }
	rows_parser : RowsProps(row, problem) -> Parser(Parser.Utf8.Bytes, List(row), [InvalidCsv(Error), InvalidRow(RowError(problem))])
	rows_parser = |props| Parser.custom(
		|bytes| {
			match Str.from_utf8(bytes) {
				Ok(source) => match parse_rows_core(props.row, source) {
					Ok(value) => Ok({ value, rest: [] })
					Err(InvalidCsv(error)) => Err(ParseError(Diagnostic.{ problem: InvalidCsv(error), location: error.location, path: error.path }))
					Err(InvalidRow(error)) => Err(ParseError(Diagnostic.{ problem: InvalidRow(error), location: error.location, path: error.path }))
				}
				Err(BadUtf8(problem)) => Err(ParseError(Diagnostic.{ problem: InvalidCsv(Diagnostic.{ problem: ExpectedValue({ expected: Text, source: Diagnostic.excerpt(bytes.drop_first(problem.index)) }), location: ByteOffset(problem.index), path: [] }), location: InputOffset(problem.index), path: [] }))
			}
		},
	)

	## The format protocol for custom parser_for implementations.
	## Its parameter is the complete error row shared by all parsing operations.
	Format(errors) : CsvFormat(errors)

	## Limits for one unfinished record; completed rows leave the decoder.
	Resource : CSVReader.Resource
	ReaderLimits : CSVReader.Props
	ReaderProps := { max_field_bytes : U64 ?? CSVReader.default_field_bytes, max_record_bytes : U64 ?? CSVReader.default_record_bytes, max_fields : U64 ?? CSVReader.default_fields, errors : Streaming.RecoveryProps ?? {} }

	## Decode raw byte records with the shared streaming API.
	decoder : ReaderProps -> Streaming.Decoder(CSVReader.State, Record, [InvalidCsv(Error)])
	decoder = |props| Streaming.decoder({ initial_state: CSVReader.new(reader_limits(props)), step: |state, bytes, end| Streaming.map_step_error(raw_step(state, bytes, end), |error| InvalidCsv(error)), errors: props.errors })

	## Decode safely framed rows with a custom positional parser.
	PositionalProps(row, p) := { row : Parser(Record, row, p), reader : CSVReader.Props ?? {}, errors : Streaming.RecoveryProps ?? {} }
	positional_decoder : PositionalProps(row, p) -> Streaming.Decoder(CSVReader.State, row, [InvalidCsv(Error), InvalidRow(RowError(p))])
	positional_decoder = |props| Streaming.decoder({ initial_state: CSVReader.new(props.reader), step: |state, bytes, end| positional_step(props.row, state, bytes, end), errors: props.errors })

	## Decode rows into the inferred type using the chosen header mode.
	TypedProps := { header : HeaderMode ?? Exact, reader : CSVReader.Props ?? {}, errors : Streaming.RecoveryProps ?? {} }
	typed_decoder : TypedProps -> Streaming.Decoder(TypedState, row, [InvalidCsv(Error), ..errs])
		where [row.Parseable([InvalidCsv(Error), ..errs])]
	typed_decoder = |props| {
		Row : row
		parse_row = Row.parser_for(CsvFormat.Default)
		Streaming.decoder({ initial_state: typed_state(props.header, props.reader), step: |state, bytes, end| typed_step(parse_row, state, bytes, end), errors: props.errors })
	}

	## Writer options. Each emitted chunk is bounded; records are validated first.
	LineEnding : [CrLf, Lf, Cr]

	## Whether to quote only special fields or every field.
	Quoting : [Necessary, Always]

	## Writer byte limits and output dialect.
	WriteProps := { line_ending : LineEnding ?? CrLf, quoting : Quoting ?? Necessary, max_field_bytes : U64 ?? 16_777_216, max_record_bytes : U64 ?? 67_108_864, max_fields : U64 ?? 65_536, chunk_bytes : U64 ?? 65_536 }

	## Invalid writer input or configured record bounds.

	## Validate and encode one byte record using necessary quoting and CRLF.
	encode_record : Record, WriteProps -> Try(List(U8), [InvalidCsv(Error)])
	encode_record = |row_fields, props| {
		if row_fields.is_empty() {
			return Err(write_error(EmptyRecord, []))
		}
		if row_fields.len() > props.max_fields {
			return Err(write_error(LimitExceeded({ resource: FieldCount, max: props.max_fields, observed: Exact(row_fields.len()) }), []))
		}
		terminator = match props.line_ending {
			CrLf => ['\r', '\n']
			Lf => ['\n']
			Cr => ['\r']
		}
		var $size = terminator.len() + row_fields.len() - 1
		var $field_index = 0.U64
		for cell_bytes in row_fields {
			$field_index = $field_index + 1
			if cell_bytes.len() > props.max_field_bytes {
				return Err(write_error(LimitExceeded({ resource: FieldBytes, max: props.max_field_bytes, observed: Exact(cell_bytes.len()) }), [Field($field_index)]))
			}
			quoted = props.quoting == Always or cell_bytes.contains(',') or cell_bytes.contains('"') or cell_bytes.contains('\r') or cell_bytes.contains('\n') or row_fields == [[]]
			size = cell_bytes.len() + (if (quoted) 2 + cell_bytes.count_if(|byte| byte == '"') else 0)
			if size > props.max_record_bytes or $size > props.max_record_bytes - size {
				return Err(write_error(LimitExceeded({ resource: RecordBytes, max: props.max_record_bytes, observed: AtLeast($size + size) }), []))
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
	encoder : WriteProps -> Streaming.Encoder(U64, Record, [InvalidCsv(Error)])
	encoder = |props| Streaming.encoder({
		initial_state: 0.U64,
		encode: |state, fields| {
			bytes = encode_record(fields, props).map_err(|InvalidCsv(error)| InvalidCsv({ ..error, path: [Record(state + 1)].concat(error.path) }))?
			Ok({ state: state + 1, bytes })
		},
		finish: |_| Ok([]),
		chunk_bytes: props.chunk_bytes,
	})

	## Encode this document with the configured writer and completion checks.
	encode : CSV, WriteProps -> Try(List(U8), [InvalidCsv(Error), InvalidChunkSize(Parser.Diagnostic(Parser.ChunkSizeProblem))])
	encode = |document, props| {
		var $bytes = []
		for chunk in CSV.encoder(props).write_iter(document.records.iter()) {
			$bytes = $bytes.concat(chunk?)
		}
		Ok($bytes)
	}

	## The cursor for type-directed decoding: one record, the header names,
	## and indices into them. Used through [CSV.decode_str]; you do not need to name
	## it.
	State :: { row : Record, header : List(Str), column : U64, cell : U64, record : U64, nested : Bool }
}

CsvFormat(errors) :: [Default].{

	## Field names are matched as written; [CSV.decode_str] normalizes the header instead.
	rename_field : CsvFormat([InvalidCsv(CSV.Error), ..errors]), Str -> Str
	rename_field = |_, name| name

	## Read the cell as UTF-8 text, verbatim.
	parse_str : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : Str, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_str = |_, state| {
		widened = {
			bytes = current_cell(state)?
			match Str.from_utf8(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(Text, bytes)))
			}
		}?
		Ok(widened)
	}

	## Read `true` or `false` in any letter case.
	parse_bool : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : Bool, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_bool = |_, state| {
		widened = {
			bytes = current_cell(state)?
			word = lowercase_ascii(bytes)
			if word == "true".to_utf8() {
				Ok({ value: True, rest: state })
			} else if word == "false".to_utf8() {
				Ok({ value: False, rest: state })
			} else {
				Err(cell_error(state, expected_problem(Boolean, bytes)))
			}
		}?
		Ok(widened)
	}

	## Read ASCII digits with an optional `+` as a U8.
	parse_u8 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : U8, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_u8 = |_, state| {
		widened = cell_int(state, U8.from_str, False, U8)?
		Ok(widened)
	}

	## Read ASCII digits with an optional sign as a I8.
	parse_i8 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : I8, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_i8 = |_, state| {
		widened = cell_int(state, I8.from_str, True, I8)?
		Ok(widened)
	}

	## Read ASCII digits with an optional `+` as a U16.
	parse_u16 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : U16, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_u16 = |_, state| {
		widened = cell_int(state, U16.from_str, False, U16)?
		Ok(widened)
	}

	## Read ASCII digits with an optional sign as a I16.
	parse_i16 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : I16, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_i16 = |_, state| {
		widened = cell_int(state, I16.from_str, True, I16)?
		Ok(widened)
	}

	## Read ASCII digits with an optional `+` as a U32.
	parse_u32 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : U32, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_u32 = |_, state| {
		widened = cell_int(state, U32.from_str, False, U32)?
		Ok(widened)
	}

	## Read ASCII digits with an optional sign as a I32.
	parse_i32 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : I32, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_i32 = |_, state| {
		widened = cell_int(state, I32.from_str, True, I32)?
		Ok(widened)
	}

	## Read ASCII digits with an optional `+` as a U64.
	parse_u64 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : U64, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_u64 = |_, state| {
		widened = cell_int(state, U64.from_str, False, U64)?
		Ok(widened)
	}

	## Read ASCII digits with an optional sign as a I64.
	parse_i64 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : I64, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_i64 = |_, state| {
		widened = cell_int(state, I64.from_str, True, I64)?
		Ok(widened)
	}

	## Read ASCII digits with an optional `+` as a U128.
	parse_u128 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : U128, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_u128 = |_, state| {
		widened = cell_int(state, U128.from_str, False, U128)?
		Ok(widened)
	}

	## Read ASCII digits with an optional sign as a I128.
	parse_i128 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : I128, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_i128 = |_, state| {
		widened = cell_int(state, I128.from_str, True, I128)?
		Ok(widened)
	}

	## Read an optional sign, digits and an optional fraction.
	parse_dec : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : Dec, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_dec = |_, state| {
		widened = {
			bytes = current_cell(state)?
			match dec_from_bytes(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(Dec, bytes)))
			}
		}?
		Ok(widened)
	}

	## Read a float with the grammar of [CSV.f64].
	parse_f32 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : F32, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_f32 = |_, state| {
		widened = {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value: value.to_f32_wrap(), rest: state })
				Err(_) => Err(cell_error(state, expected_problem(F32, bytes)))
			}
		}?
		Ok(widened)
	}

	## Read a float with the grammar of [CSV.f64].
	parse_f64 : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : F64, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
	parse_f64 = |_, state| {
		widened = {
			bytes = current_cell(state)?
			match decimal_f64(bytes) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(cell_error(state, expected_problem(F64, bytes)))
			}
		}?
		Ok(widened)
	}

	## An empty cell is null, so a `Try(a, [Null])` field is `Err(Null)`.
	parse_null : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try(CSV.State, [InvalidCsv(CSV.Error), ..errors])
	parse_null = |_, state| {
		widened = {
			bytes = current_cell(state)?
			if (bytes.is_empty()) Ok(state) else Err(cell_error(state, expected_problem(Empty, bytes)))
		}?
		Ok(widened)
	}

	## A row is one record; a record inside a row is rejected.
	parse_record_start : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try([Counted({ len : U64, rest : CSV.State }), Uncounted(CSV.State)], [InvalidCsv(CSV.Error), ..errors])
	parse_record_start = |_, state| {
		widened = {
			if state.nested {
				Err(cell_error(state, UnsupportedCell(Record)))
			} else {
				Ok(Uncounted(CSV.State.{ row: state.row, header: state.header, column: 0, cell: 0, record: state.record, nested: True }))
			}
		}?
		Ok(widened)
	}

	## Offer the next header name as the field to fill.
	parse_record_field : CsvFormat([InvalidCsv(CSV.Error), ..errors]),
	Encoding.FieldName.FieldNames(_shape),
	CSV.State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : CSV.State }),
			TryField({ name : Str, rest : CSV.State }),
			TryFieldCaseless({ name : Str, rest : CSV.State }),
			Continue(CSV.State),
			Done(CSV.State),
		],
		[InvalidCsv(CSV.Error), ..errors],
	)
	parse_record_field = |_, _, state| {
		widened = {
			match state.header.get(state.column) {
				Err(_) => Ok(Done(state))
				Ok(name) => Ok(TryField({ name, rest: CSV.State.{ row: state.row, header: state.header, column: state.column + 1, cell: state.column, record: state.record, nested: True } }))
			}
		}?
		Ok(widened)
	}

	## The record ends after the last header column.
	parse_record_after_field : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try([Continue(CSV.State), Done(CSV.State)], [InvalidCsv(CSV.Error), ..errors])
	parse_record_after_field = |_, state| {
		widened = {
			if (state.column >= state.header.len()) Ok(Done(state)) else Ok(Continue(state))
		}?
		Ok(widened)
	}

	## The cell to skip is chosen by the column, so nothing is consumed.
	skip_record_field : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try(CSV.State, [InvalidCsv(CSV.Error), ..errors])
	skip_record_field = |_, state| {
		widened = {
			Ok(state)
		}?
		Ok(widened)
	}

	## A headerless row is a tuple with one element per field.
	parse_tuple_start : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State, U64 -> Try(CSV.State, [InvalidCsv(CSV.Error), ..errors])
	parse_tuple_start = |_, state, len| {
		widened = {
			if state.nested {
				Err(cell_error(state, UnsupportedCell(Tuple)))
			} else if state.row.len() != len {
				Err(InvalidCsv(csv_error({ record: state.record, field: smaller(state.row.len(), len) + 1, line: 0, column: 0, problem: width_problem(state.row.len(), len) })))
			} else {
				Ok(CSV.State.{ row: state.row, header: state.header, column: 0, cell: 0, record: state.record, nested: True })
			}
		}?
		Ok(widened)
	}

	## Move to the cell of the next tuple element.
	parse_tuple_next : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State, U64, U64 -> Try(CSV.State, [InvalidCsv(CSV.Error), ..errors])
	parse_tuple_next = |_, state, index, _| {
		widened = {
			Ok(CSV.State.{ row: state.row, header: state.header, column: index, cell: index, record: state.record, nested: True })
		}?
		Ok(widened)
	}

	## The error for a cell that holds no valid value.
	invalid_value : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State -> [InvalidCsv(CSV.Error), ..errors]
	invalid_value = |_, state| cell_error(state, InvalidValue)

	## Finish a tuple row.
	parse_tuple_end : CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State, U64 -> Try(CSV.State, [InvalidCsv(CSV.Error), ..errors])
	parse_tuple_end = |_, state, _| {
		widened = {
			Ok(state)
		}?
		Ok(widened)
	}

	## Report a required field absent from the input.
	missing_field : CsvFormat([InvalidCsv(CSV.Error), ..errors]), Str -> [InvalidCsv(CSV.Error), ..errors]
	missing_field = |_, name| InvalidCsv(Diagnostic.{ problem: MissingRequiredField(name), location: Unavailable(NotRetained), path: [Key(name)] })

	## A tag without a payload is read from its name.
	parse_tag_union : CsvFormat([InvalidCsv(CSV.Error), ..errors]), Encoding.ParseTagUnionSpec(a, CsvFormat([InvalidCsv(CSV.Error), ..errors]), CSV.State, [InvalidCsv(CSV.Error), ..errors]), CSV.State -> Try({ value : a, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errors])
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
				missing: cell_error(state, UnknownTag(Diagnostic.excerpt(bytes))),
			},
		)
	}

}

parse_records_core : Str -> Try(List(CSV.Record), [InvalidCsv(CSV.Error)])
parse_records_core = |text| {
	bytes = text.to_utf8()
	match scan_table(bytes) {
		Ok(records) => Ok(records)
		Err(bad) => Err(InvalidCsv(syntax_error(bytes, bad)))
	}
}

parse_rows_core : Parser(CSV.Record, a, p), Str -> Try(List(a), [InvalidCsv(CSV.Error), InvalidRow(CSV.RowError(p))])
parse_rows_core = |record_parser, text| {
	bytes = text.to_utf8()
	var $values = []
	var $pos = 0.U64
	var $index = 0.U64
	var $field_capacity = 0.U64
	while $pos < bytes.len() {
		match scan_record_sized(bytes, $pos, $field_capacity) {
			Err(bad) => return Err(InvalidCsv(syntax_error(bytes, { field: bad.field, at: bad.at, problem: bad.problem, record: $index })))
			Ok({ fields, next }) => {
				$field_capacity = smaller(fields.len(), 64)
				match record_parser.run(fields) {
					Ok({ value, rest }) if rest.is_empty() => {
						$values = $values.append(value)
					}
					Ok({ rest, .. }) => {
						# Rebuild the row only for this error, so successful decoding
						# can consume its fields without keeping another list alias.
						match scan_record(bytes, $pos) {
							Ok({ fields: again, .. }) => return Err(InvalidRow(locate(bytes, leftover_error(record_parser, again, rest.len(), $index + 1))))
							Err(bad) => return Err(InvalidCsv(syntax_error(bytes, { field: bad.field, at: bad.at, problem: bad.problem, record: $index })))
						}
					}
					Err(ParseError(failure)) => return Err(InvalidRow(locate(bytes, row_error($index + 1, failure))))
				}
				$pos = after_break(bytes, next)
				$index = $index + 1
			}
		}
	}
	Ok($values)
}

decode_rows_core : Parser(CSV.Record, a, p), List(CSV.Record) -> Try(List(a), [InvalidCsv(CSV.Error), InvalidRow(CSV.RowError(p))])
decode_rows_core = |record_parser, records| {
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
			Err(ParseError(failure)) => {
				return Err(InvalidRow(row_error($index + 1, failure)))
			}
		}
		$index = $index + 1
	}
	Ok($values)
}

write_error : CSV.Problem([]), List(Diagnostic.PathSegment) -> [InvalidCsv(CSV.Error)]
write_error = |problem, path| InvalidCsv(Diagnostic.{ problem, location: Unavailable(GeneratedValue), path })

TypedState : { reader : CSVReader.State, header : List(Str), header_read : Bool, mode : CSV.HeaderMode }

typed_state : CSV.HeaderMode, CSV.ReaderLimits -> TypedState
typed_state = |mode, props| { reader: CSVReader.new(props), header: [], header_read: mode == Absent, mode }

stream_location : CSVReader.State, Diagnostic(p) -> Diagnostic(p)
stream_location = |reader, problem| {
	if csv_coordinates(problem).field == 0 {
		return problem
	}
	at = reader.last_starts.get(if (csv_coordinates(problem).field == 0) 0 else csv_coordinates(problem).field - 1) ?? (reader.last_starts.first() ?? reader.last_end)
	{ ..problem, location: Text(at) }
}

reader_limits : CSV.ReaderProps -> CSVReader.Props
reader_limits = |props| { max_field_bytes: props.max_field_bytes, max_record_bytes: props.max_record_bytes, max_fields: props.max_fields }

reader_problem : CSVReader.Problem -> CSV.Problem([])
reader_problem = |problem| match problem {
	LimitExceeded(detail) => LimitExceeded(detail)
	_ => Syntax(problem)
}

raw_step : CSVReader.State, List(U8), Streaming.InputEnd -> Streaming.Step(CSVReader.State, CSV.Record, CSV.Error)
raw_step = |state, bytes, end| {
	next = CSVReader.step(state, bytes, end)
	outcome = match next.outcome {
		Item(fields) => Item(fields)
		RecordError(error) => RecordError(Diagnostic.{ problem: reader_problem(error.problem), location: Text({ offset: error.offset, line: error.line, column: error.column }), path: [Record(error.record), Field(error.field)] })
		Failed(error) => Failed(Diagnostic.{ problem: reader_problem(error.problem), location: Text({ offset: error.offset, line: error.line, column: error.column }), path: [Record(error.record), Field(error.field)] })
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
			Err(ParseError(failure)) => RecordError(InvalidRow(stream_location(next.state, row_error(next.state.record - 1, failure))))
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
						error = stream_location(next.state, csv_error({ record: next.state.record - 1, field: smaller(fields.len(), $state.header.len()) + 1, line: 0, column: 0, problem: width_problem(fields.len(), $state.header.len()) }))
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
	from_quote = |source| CSV.validate_as({ source: source }).map_ok(|value| CSV.Decoded.{ value }).map_err(|failure| Parser.Quotation.error({ format: "CSV", message: Str.inspect(failure) }))
}

## Decode every data record of `text` with a row parser built by `parser_for`.
decode_table : Str, (CSV.State -> Try({ value : row, rest : CSV.State }, [InvalidCsv(CSV.Error), ..errs])), [HeaderExact, HeaderNormalized, Headerless] -> Try(List(row), [InvalidCsv(CSV.Error), ..errs])
decode_table = |text, parse_row, mode| {
	bytes = text.to_utf8()
	var $header = []
	var $index = 0.U64
	var $pos = 0.U64
	var $values = []
	var $field_capacity = 0.U64
	while $pos < bytes.len() {
		match scan_record_sized(bytes, $pos, $field_capacity) {
			Err(bad) => return Err(InvalidCsv(syntax_error(bytes, { field: bad.field, at: bad.at, problem: bad.problem, record: $index })))
			Ok({ fields: row, next }) => {
				$field_capacity = smaller(row.len(), 64)
				if $index == 0 and mode != Headerless {
					$header = row.map(|field| if (mode == HeaderNormalized) normalize_name(field) else Str.from_utf8_lossy(field))
				} else if row != [[]] {
					if mode != Headerless and row.len() != $header.len() {
						return Err(InvalidCsv(locate(bytes, csv_error({ record: $index + 1, field: smaller(row.len(), $header.len()) + 1, line: 0, column: 0, problem: width_problem(row.len(), $header.len()) }))))
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
				return Err(InvalidCsv(locate(bytes, csv_error({ record: $index + 1, field: smaller(row.len(), width) + 1, line: 0, column: 0, problem: width_problem(row.len(), width) }))))
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
checked_error = |name, problem| Parser.Quotation.error({ name, format: "CSV", message: "record ${csv_coordinates(problem).record.to_str()}, field ${csv_coordinates(problem).field.to_str()}: ${Str.inspect(problem.problem)}", line: csv_coordinates(problem).line, column: csv_coordinates(problem).column })

checked_records : Str, CSV.HeaderMode, CSV.Lints -> Try(List(CSV.Record), [InvalidCsv(CSV.Error)])
checked_records = |source, header, lints| CSV.parse_bytes(source.to_utf8(), { header, validation: Checked(lints) }).map_ok(|document| document.records)

check_headers : List(U8), List(CSV.Record), CSV.HeaderMode, CSV.Lints -> Try({}, [InvalidCsv(CSV.Error)])
check_headers = |bytes, records, header, lints| {
	if header != Absent {
		names = (records.first() ?? []).map(|field| if (header == Normalized) normalize_name(field) else Str.from_utf8_lossy(field))
		var $seen = Set.empty()
		var $index = 0
		for field in names {
			if lints.empty_headers and field == "" {
				return Err(InvalidCsv(locate(bytes, csv_error({ record: 1, field: $index + 1, line: 0, column: 0, problem: EmptyHeader }))))
			}
			if lints.duplicate_headers and $seen.contains(field) {
				return Err(InvalidCsv(locate(bytes, csv_error({ record: 1, field: $index + 1, line: 0, column: 0, problem: DuplicateHeader(DiagnosticOwnership.own_text(field)) }))))
			}
			$seen = $seen.insert(field)
			$index = $index + 1
		}
	}
	Ok({})
}

smaller : U64, U64 -> U64
smaller = |a, b| if (a < b) a else b

width_problem : U64, U64 -> CSV.Problem([])
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

cell_error : CSV.State, CSV.Problem([]) -> [InvalidCsv(CSV.Error)]
cell_error = |state, problem| InvalidCsv(csv_error({ record: state.record, field: state.cell + 1, line: 0, column: 0, problem }))

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

expected_problem : CSV.ValueType, List(U8) -> CSV.Problem(p)
expected_problem = |expected, bytes| ExpectedValue({ expected, source: Diagnostic.excerpt(bytes) })

## Explain why a record parser left fields unread, preferring the furthest
## failure (such as a field that stopped a `Parser.many`).
leftover_error : Parser(CSV.Record, a, p), CSV.Record, U64, U64 -> CSV.RowError(p)
leftover_error = |record_parser, fields, left, record| {
	used = fields.len() - left
	extra = csv_error({ record, field: used + 1, line: 0, column: 0, problem: UnreadFields({ found: fields.len(), read: used }) })
	match record_parser.parse(fields) {
		Err(ParseError({ problem: UnexpectedInput, .. })) => extra
		Err(ParseError(failure)) => row_error(record, failure)
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
	Diagnostic.{ problem: Syntax(scanner_problem(bad.problem)), location: Text({ offset: bad.at, line, column }), path: [Record(bad.record + 1), Field(bad.field + 1)] }
}

## Fill in the line and column of a decoding error from the source bytes.
## Only runs on the error path, so it rescans the input.
locate : List(U8), Diagnostic(p) -> Diagnostic(p)
locate = |bytes, problem| {
	if bytes.is_empty() {
		return problem
	}
	coordinates = csv_coordinates(problem)
	if coordinates.record == 0 or coordinates.field == 0 {
		return problem
	}
	offset = field_offset(bytes, coordinates.record - 1, coordinates.field - 1)
	{ line, column } = line_column(bytes, offset)
	{ ..problem, location: Text({ offset, line, column }) }
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
scan_table = |bytes| scan_table_as(bytes, |field, _, _, _| field)

scan_table_as : List(U8), (List(U8), U64, U64, Bool -> a) -> Try(List(List(a)), BadField)
scan_table_as = |bytes, convert| {
	len = bytes.len()
	var $records = []
	var $pos = 0
	var $field_capacity = 0.U64
	while $pos < len {
		match scan_record_as(bytes, $pos, $field_capacity, convert) {
			Err({ field, at, problem }) => {
				return Err({ record: $records.len(), field, at, problem })
			}
			Ok({ fields, next }) => {
				$field_capacity = smaller(fields.len(), 64)
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
scan_record = |bytes, start| scan_record_sized(bytes, start, 0)

# The previous row's width avoids repeated growth for regular tables. The
# callers cap the hint so an unusually wide row does not force a large
# allocation for the following row. Width remains unconstrained.
scan_record_sized : List(U8), U64, U64 -> Try({ fields : CSV.Record, next : U64 }, { field : U64, at : U64, problem : [Unterminated, TextAfterQuote] })
scan_record_sized = |bytes, start, field_capacity| scan_record_as(bytes, start, field_capacity, |field, _, _, _| field)

# Convert fields as they are scanned, without materializing an intermediate
# byte-field row when the caller needs another representation.
scan_record_as : List(U8), U64, U64, (List(U8), U64, U64, Bool -> a) -> Try({ fields : List(a), next : U64 }, { field : U64, at : U64, problem : [Unterminated, TextAfterQuote] })
scan_record_as = |bytes, start, field_capacity, convert| {
	len = bytes.len()
	var $fields = List.with_capacity(field_capacity)
	var $pos = start
	while True {
		quoted = bytes.get($pos) == Ok('"')
		scanned =
			if quoted {
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
				$fields = $fields.append(convert(field, $pos, next, quoted))
				if next < len and bytes.get(next) == Ok(',') {
					$pos = next + 1
				} else {
					return Ok({ fields: $fields, next })
				}
			}
		}
	}
}

# Delimiters and quotes are ASCII, so the scanner's field boundaries cannot
# split a UTF-8 code point in a Str input. Unescaped fields can be sliced
# directly; doubled quotes require the scanner's decoded bytes instead.
string_field : Str, U64, List(U8), U64, U64, Bool -> Str
string_field = |text, len, field, start, next, quoted| {
	content_start = if (quoted) start + 1 else start
	content_end = if (quoted) next - 1 else next
	if quoted and field.len() != content_end - content_start {
		return Str.from_utf8_lossy(field)
	}
	match text.drop_first_bytes(content_start) {
		Ok(suffix) => suffix.drop_last_bytes(len - content_end) ?? Str.from_utf8_lossy(field)
		Err(_) => Str.from_utf8_lossy(field)
	}
}

## Offset of the first `,`, CR or LF at or after `start`, or the input length.
scan_unquoted : List(U8), U64 -> U64
scan_unquoted = |bytes, start| {
	comma = U8x16.splat(',')
	cr = U8x16.splat('\r')
	lf = U8x16.splat('\n')
	var $pos = start
	while True {
		match U8x16.load(bytes, $pos) {
			Ok(lanes) => {
				mask = lanes.eq_lanes(comma).bitwise_or(lanes.eq_lanes(cr)).bitwise_or(lanes.eq_lanes(lf)).to_bitmask()
				if mask != 0 {
					return $pos + mask.count_trailing_zero_bits().to_u64()
				}
				$pos = $pos + 16
			}
			Err(_) => break
		}
	}
	while $pos < bytes.len() {
		if is_delimiter(bytes.get($pos) ?? 0) {
			break
		}
		$pos = $pos + 1
	}
	$pos
}

find_quote : List(U8), U64 -> U64
find_quote = |bytes, start| {
	quote = U8x16.splat('"')
	var $pos = start
	while True {
		match U8x16.load(bytes, $pos) {
			Ok(lanes) => {
				mask = lanes.eq_lanes(quote).to_bitmask()
				if mask != 0 {
					return $pos + mask.count_trailing_zero_bits().to_u64()
				}
				$pos = $pos + 16
			}
			Err(_) => break
		}
	}
	while $pos < bytes.len() {
		if bytes.get($pos) == Ok('"') {
			break
		}
		$pos = $pos + 1
	}
	$pos
}

# Exercise every vector lane, the scalar tail, and non-ASCII bytes without
# depending on UTF-8 decoding or on the streaming scanner's implementation.
expect [0, 'x', 0x80, 0xFF].all(
	|fill| [',', '\r', '\n'].all(
		|delimiter| (0..=48).iter().fold(
			True,
			|good, at| {
				bytes = List.repeat(fill, at).append(delimiter).concat(List.repeat(fill, 19))
				good and scan_unquoted(bytes, 0) == at and scan_unquoted(bytes, at + 1) == bytes.len()
			},
		),
	),
)
expect [0, 'x', 0x80, 0xFF].all(
	|fill| (0..=48).iter().fold(
		True,
		|good, at| {
			bytes = List.repeat(fill, at).append('"').concat(List.repeat(fill, 19))
			good and find_quote(bytes, 0) == at and find_quote(bytes, at + 1) == bytes.len()
		},
	),
)

is_delimiter : U8 -> Bool
is_delimiter = |byte| byte == ',' or byte == '\r' or byte == '\n'

## Scan a quoted field whose opening `"` is at `start`.
scan_quoted : List(U8), U64 -> Try({ field : List(U8), next : U64 }, { at : U64, problem : [Unterminated, TextAfterQuote] })
scan_quoted = |bytes, start| {
	len = bytes.len()
	var $field = []
	var $chunk_start = start + 1
	var $pos = find_quote(bytes, start + 1)
	while $pos < len {
		if bytes.get($pos) == Ok('"') {
			chunk = bytes.sublist({ start: $chunk_start, len: $pos - $chunk_start })
			# A field with no doubled quotes stays a slice of the input.
			$field = if ($field.is_empty()) chunk else $field.concat(chunk)
			if bytes.get($pos + 1) == Ok('"') {
				$field = $field.append('"')
				$chunk_start = $pos + 2
				$pos = find_quote(bytes, $pos + 2)
			} else {
				after = $pos + 1
				if after >= len or is_delimiter(bytes.get(after) ?? 0) {
					return Ok({ field: $field, next: after })
				}
				return Err({ at: after, problem: TextAfterQuote })
			}
		} else {
			$pos = find_quote(bytes, $pos + 1)
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
	actual = (CSV.u64({})).parse(text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted
		(Err(_), Err({})) => True
		_ => False
	}
}

parses_f64 : Str, Try(F64, {}) -> Bool
parses_f64 = |text, expected| {
	actual = (CSV.f64({})).parse(text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted or (value.is_nan() and wanted.is_nan())
		(Err(_), Err({})) => True
		_ => False
	}
}

records : Str -> Try(List(List(Str)), [InvalidCsv(CSV.Error)])
records = |text| CSV.parse_str(text, {}).map_ok(|document| document.records).map_ok(|found| found.map(|fields| fields.map(Str.from_utf8_lossy)))

physical_error_for_test : Diagnostic(p) -> Diagnostic(p)
physical_error_for_test = |error| match error.location {
	Text(at) => { ..error, location: LineColumn({ line: at.line, column: at.column }) }
	_ => error
}

row_error_of : Try(a, [InvalidRow(CSV.RowError(p)), ParseError(Diagnostic(Parser.Problem([InvalidCsv(CSV.Error), InvalidRow(CSV.RowError(p))]))), ..others]) -> Try(CSV.RowError(p), [NoError])
row_error_of = |result| match result {
	Err(InvalidRow(error)) => Ok(physical_error_for_test(error))
	Err(ParseError({ problem: InvalidRow(error), .. })) => Ok(physical_error_for_test(error))
	_ => Err(NoError)
}
# Text collection has the same rows and exact syntax diagnostics as raw and
# generic string decoding, including the header, BOM, and relaxed quote rules.
expect CSV.parse_string_records("name,note\nAda,\"said \"\"hello\"\"\"\n", {}) == Ok([["name", "note"], ["Ada", "said \"hello\""]])
expect ["", "\n", "\r\n", ",", "\"\"", "\"\",", "a,b\r\nc,d", "a\r\n\nb", "é,😀", "\u(feff)a", "a\"b, \"c\" ", "\"a\r\nb\",\"x\"\"y\"\n", "a,\"b", "x\n\"a\"b,c", "a,b,c,d\nx\n,,\n\"a,b\",c\n"].all(
	|text| {
		expected = records(text)
		generic_agrees = match (CSV.rows_parser({ row: Parser.many(CSV.field(CSV.string({}))) }).parse_str(text), expected) {
			(Ok(actual), Ok(wanted)) => actual == wanted
			(Err(ParseError({ problem: InvalidCsv(actual), .. })), Err(InvalidCsv(wanted))) => actual == wanted
			_ => False
		}
		CSV.parse_string_records(text, {}) == expected and generic_agrees
	},
)

# Quoted and doubled-quote UTF-8 fields at every vector lane keep complete
# code points and the same decoded text, including adjacent empty fields.
expect (0..=48).iter().fold(
	True,
	|good, width| {
		prefix = Str.from_utf8_lossy(List.repeat('x', width))
		plain = "${prefix},\"é,😀\",\r\n"
		escaped = "${prefix},\"é,😀\"\"q\",\r\n"
		good and CSV.parse_string_records(plain, {}) == Ok([[prefix, "é,😀", ""]]) and CSV.parse_string_records(escaped, {}) == Ok([[prefix, "é,😀\"q", ""]])
	},
)

error_of : Try(a, [InvalidCsv(CSV.Error), ..others]) -> Try(CSV.Error, [NoError])
error_of = |result| {
	match result {
		Err(InvalidCsv(problem)) => Ok(physical_error_for_test(problem))
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
expect error_of(records("x\n\"a\"b,c")) == Ok(csv_error({ record: 2, field: 1, line: 2, column: 4, problem: Syntax(TextAfterQuote) }))

# An unterminated quoted field is an error located at its opening quote.
expect error_of(records("a,\"b")) == Ok(csv_error({ record: 1, field: 2, line: 1, column: 3, problem: Syntax(UnterminatedQuotedField) }))

# The composable parser accepts any bytes and reports malformed fields by offset.
expect (CSV.parser({})).parse([0xFF, ',', 0xC3]).map_ok(|document| document.records) == Ok([[[0xFF], [0xC3]]])
expect match (CSV.parser({})).parse("a\n\"b".to_utf8()) {
	Err(ParseError({ problem: InvalidCsv(error), .. })) => error.problem == Syntax(UnterminatedQuotedField) and error.location == Text({ offset: 2, line: 2, column: 1 })
	_ => False
}

# The header helper splits off the column names.
expect CSV.{ records: [] }.split_header() == { header: [], rows: [] }
expect CSV.{ records: [["a".to_utf8()], ["1".to_utf8()]] }.split_header() == { header: ["a"], rows: [["1".to_utf8()]] }

# A hand-built record parser decodes every row, as in the module docs.
user : Parser(CSV.Record, { name : Str, age : U64 }, _)
user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.u64({})))

expect CSV.rows_parser({ row: user }).parse_str("Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])

# Row widths can grow or shrink between records, including empty fields.
expect {
	text = "a,b,c,d\nx\n,,\n\"a,b\",c\n"
	expected = [["a", "b", "c", "d"], ["x"], ["", "", ""], ["a,b", "c"]]
	records(text) == Ok(expected) and CSV.rows_parser({ row: Parser.many(CSV.field(CSV.string({}))) }).parse_str(text) == Ok(expected)
}

# A field that does not decode names its record, field and position.
expect row_error_of(CSV.rows_parser({ row: user }).parse_str("Ada,36\nAlan,forty\n")) == Ok(csv_error({ record: 2, field: 2, line: 2, column: 6, problem: ParserFailure(FieldProblem(Diagnostic.{ problem: ExpectedValue({ expected: U64, source: Diagnostic.excerpt("forty".to_utf8()) }), location: ByteOffset(0), path: [] })) }))

# Extra fields are an error carrying the record number.
expect row_error_of(CSV.rows_parser({ row: user }).parse_str("Ada,36\n\"Al\nan\",41,x\n")) == Ok(csv_error({ record: 2, field: 3, line: 3, column: 8, problem: UnreadFields({ found: 3, read: 2 }) }))

# A repetition's rejected field keeps its problem and quoted-field position
# when the row is rebuilt for the leftover diagnostic. Later syntax loses to it.
expect row_error_of(CSV.rows_parser({ row: Parser.many(CSV.field(CSV.u64({}))) }).parse_str("1,2\r\n3,\"4\",nope\r\n\"unterminated")) == Ok(csv_error({ record: 2, field: 3, line: 2, column: 7, problem: ParserFailure(FieldProblem(Diagnostic.{ problem: ExpectedValue({ expected: U64, source: Diagnostic.excerpt("nope".to_utf8()) }), location: ByteOffset(0), path: [] })) }))

# Too few fields is an error too.
expect row_error_of(CSV.rows_parser({ row: user }).parse_str("Ada")) == Ok(csv_error({ record: 1, field: 2, line: 1, column: 1, problem: ParserFailure(MissingField) }))

# Invalid UTF-8 remains raw source bytes in bounded structured payloads.
expect {
	bad = [0xFF, 0xC3, 'x']
	row_error_of(CSV.{ records: [[bad, "1".to_utf8()]] }.decode_rows({ row: user })) == Ok(csv_error({ record: 1, field: 1, line: 0, column: 0, problem: ParserFailure(FieldProblem(Diagnostic.{ problem: ExpectedValue({ expected: Text, source: Diagnostic.excerpt(bad) }), location: ByteOffset(0), path: [] })) }))
}
expect {
	long = List.repeat('9', 5000)
	match CSV.{ records: [["a".to_utf8(), long]] }.decode_rows({ row: user }) {
		Err(InvalidRow({ problem: ParserFailure(FieldProblem({ problem: ExpectedValue({ source, .. }), .. })), .. })) => source.bytes.len() == 40 and source.total_bytes == long.len()
		_ => False
	}
}
expect {
	all_fields = Parser.many(CSV.field(CSV.u64({})))
	row_error_of(CSV.{ records: [["1".to_utf8(), [0xFF, 0xFE]]] }.decode_rows({ row: all_fields })) == Ok(csv_error({ record: 1, field: 2, line: 0, column: 0, problem: ParserFailure(FieldProblem(Diagnostic.{ problem: ExpectedValue({ expected: U64, source: Diagnostic.excerpt([0xFF, 0xFE]) }), location: ByteOffset(0), path: [] })) }))
}

# Type-directed decoding: header names choose fields, unknown columns are
# skipped, and absent optional columns are `Err(Missing)`.
Person : { name : Str, age : U64, admin : Bool, email : Try(Str, [Missing]) }

PersonResult : Try(List(Person), [InvalidCsv(CSV.Error)])

expect {
	people : PersonResult
	people = CSV.decode_str("age,extra,name,admin\n36,zz,Ada,true\r\n7,,\"Bob, Jr\",FALSE\n", {})
	people == Ok([{ name: "Ada", age: 36, admin: True, email: Err(Missing) }, { name: "Bob, Jr", age: 7, admin: False, email: Err(Missing) }])
}

expect {
	people : PersonResult
	people = CSV.decode_str("email,name,age,admin\na@b.c,Ada,36,true\n", {})
	people == Ok([{ name: "Ada", age: 36, admin: True, email: Ok("a@b.c") }])
}

# A required column that is absent is reported by the compiler-added error.
expect {
	people : PersonResult
	people = CSV.decode_str("name,admin\nAda,true\n", {})
	people == Err(InvalidCsv(Diagnostic.{ problem: MissingRequiredField("age"), location: Unavailable(NotRetained), path: [Key("age")] }))
}

# A cell that does not decode is located by record, field, line and column.
expect {
	people : PersonResult
	people = CSV.decode_str("name,age,admin\nAda,36,true\nBob,old,true\n", {})
	error_of(people) == Ok(csv_error({ record: 3, field: 2, line: 3, column: 5, problem: ExpectedValue({ expected: U64, source: Diagnostic.excerpt("old".to_utf8()) }) }))
}

# Short and long rows are errors naming the record; blank lines are skipped.
expect {
	people : PersonResult
	people = CSV.decode_str("name,age,admin\n\nAda,36\n", {})
	error_of(people) == Ok(csv_error({ record: 3, field: 3, line: 3, column: 1, problem: WidthMismatch({ found: 2, expected: 3 }) }))
}
expect {
	people : PersonResult
	people = CSV.decode_str("name,age,admin\nAda,36,true,x\n\n", {})
	error_of(people) == Ok(csv_error({ record: 2, field: 4, line: 2, column: 13, problem: WidthMismatch({ found: 4, expected: 3 }) }))
}

# Empty cells are `Err(Null)` for `Try(_, [Null])` fields and `""` for strings.
expect {
	rows : Try(List({ a : Try(U64, [Null]), b : Str }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a,b\n,\n5,y\n", {})
	rows == Ok([{ a: Err(Null), b: "" }, { a: Ok(5), b: "y" }])
}

# Tags without payloads decode from their names.
expect {
	rows : Try(List({ status : [Active, Inactive] }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("status\nActive\nInactive\n", {})
	rows == Ok([{ status: Active }, { status: Inactive }])
}
expect {
	rows : Try(List({ status : [Active, Inactive] }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("status\nNope\n", {})
	error_of(rows) == Ok(csv_error({ record: 2, field: 1, line: 2, column: 1, problem: UnknownTag(Diagnostic.excerpt("Nope".to_utf8())) }))
}

# Every number type uses the data grammar; signs only where they fit.
expect {
	rows : Try(List({ a : I8, b : U16, c : I32, d : U128, e : I128, f : Dec, g : F32, h : U8, i : I16, j : U32, k : I64 }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a,b,c,d,e,f,g,h,i,j,k\n-128,+65535,-7,340282366920938463463374607431768211455,-1,-2.50,1.5,255,-32768,0,-9\n", {})
	rows == Ok([{ a: -128, b: 65535, c: -7, d: 340282366920938463463374607431768211455, e: -1, f: -2.5, g: 1.5, h: 255, i: -32768, j: 0, k: -9 }])
}
expect {
	rows : Try(List({ a : U8 }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a\n256\n", {})
	rows.is_err()
}
expect {
	rows : Try(List({ a : U8 }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a\n-1\n", {})
	rows.is_err()
}
expect {
	rows : Try(List({ a : Dec }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a\n1e3\n", {})
	rows.is_err()
}

# Tags with payloads are rejected.
expect {
	rows : Try(List({ a : [Tagged(Str), Plain] }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a\nTagged\n", {})
	error_of(rows) == Ok(csv_error({ record: 2, field: 1, line: 2, column: 1, problem: UnsupportedCell(TagPayload) }))
}

# Nested records are rejected.
expect {
	rows : Try(List({ a : { b : Str } }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("a\nx\n", {})
	error_of(rows) == Ok(csv_error({ record: 2, field: 1, line: 2, column: 1, problem: UnsupportedCell(Record) }))
}

# Normalized header names match snake_case fields.
expect {
	rows : Try(List({ first_name : Str, last_name : Str }), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str(" First Name ,LAST--name\nAda,Lovelace\n", { header: Normalized })
	rows == Ok([{ first_name: "Ada", last_name: "Lovelace" }])
}

# Headerless input decodes into tuples, and the width must match.
expect {
	rows : Try(List((Str, U64, Bool)), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("Ada,36,true\nAlan,41,false\n", { header: Absent })
	rows == Ok([("Ada", 36, True), ("Alan", 41, False)])
}
expect {
	rows : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	rows = CSV.decode_str("Ada,36\nAlan\n", { header: Absent })
	error_of(rows) == Ok(csv_error({ record: 2, field: 2, line: 2, column: 1, problem: WidthMismatch({ found: 1, expected: 2 }) }))
}

# Malformed CSV is reported before any decoding.
expect {
	people : PersonResult
	people = CSV.decode_str("name\n\"Ada", {})
	error_of(people) == Ok(csv_error({ record: 2, field: 1, line: 2, column: 1, problem: Syntax(UnterminatedQuotedField) }))
}

# A header with no data rows decodes to no rows, as does empty input.
expect {
	people : PersonResult
	people = CSV.decode_str("name,age,admin\n", {})
	people == Ok([])
}
expect {
	people : PersonResult
	people = CSV.decode_str("", {})
	people == Ok([])
}

# The examples in the documentation comments.
expect {
	people : Try(List({ name : Str, age : U64 }), [InvalidCsv(CSV.Error)])
	people = CSV.decode_str("name,age\nAda,36\nAlan,41\n", {})
	people == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
}

expect CSV.parse_str("a,\"b,c\"\n", {}).map_ok(|document| document.records) == Ok([["a".to_utf8(), "b,c".to_utf8()]])

expect {
	found = CSV.parse_str("name,age\nAda,36\n", {}).map_ok(|document| document.records)?
	CSV.{ records: found }.split_header() == { header: ["name", "age"], rows: [["Ada".to_utf8(), "36".to_utf8()]] }
}

expect {
	name : Parser(CSV.Record, { first : Str, last : Str }, _)
	name = CSV.record(|first| |last| { first, last }).keep(CSV.field(CSV.string({}))).keep(CSV.field(CSV.string({})))
	CSV.rows_parser({ row: name }).parse_str("Ada,Lovelace") == Ok([{ first: "Ada", last: "Lovelace" }])
}

expect {
	items : Try(List({ sku : Str, count : U64, note : Try(Str, [Missing]), price : Try(Dec, [Null]) }), [InvalidCsv(CSV.Error)])
	items = CSV.decode_str("price,sku,count\n1.50,A1,3\n,B2,0\n", {})
	items == Ok([
		{ sku: "A1", count: 3, note: Err(Missing), price: Ok(1.50) },
		{ sku: "B2", count: 0, note: Err(Missing), price: Err(Null) },
	])
}

expect {
	names : Try(List({ first_name : Str }), [InvalidCsv(CSV.Error)])
	names = CSV.decode_str(" First Name \nAda\n", { header: Normalized })
	names == Ok([{ first_name: "Ada" }])
}

expect {
	pairs : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	pairs = CSV.decode_str("Ada,36\nAlan,41\n", { header: Absent })
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

## Raw validation never guesses that the first record is a header.
expect CSV.validate({ source: "name,name,\n" }) == Ok([["name".to_utf8(), "name".to_utf8(), []]])

## Duplicate and empty header failures retain typed reasons and original offsets.
expect CSV.validate({ source: "name,name\n", name: "headers.csv", header: Exact }) == Err(InvalidCsv(Diagnostic.{ problem: DuplicateHeader("name"), location: Text({ offset: 5, line: 1, column: 6 }), path: [Record(1), Field(2), Source("headers.csv")] }))
expect CSV.validate({ source: "name,\n", header: Exact }) == Err(InvalidCsv(Diagnostic.{ problem: EmptyHeader, location: Text({ offset: 5, line: 1, column: 6 }), path: [Record(1), Field(2), Source("<literal>")] }))
expect CSV.validate({ source: "", header: Exact }) == Ok([])

## Normalized checks detect collisions while exact checks keep distinct names.
expect {
	source = "First-Name,FIRST_NAME\nAda,Alan\n"
	exact = CSV.validate({ source, header: Exact })?
	match CSV.validate({ source, header: Normalized }) {
		Err(InvalidCsv(error)) => error.problem == DuplicateHeader("first_name") and exact.len() == 3 - 1
		Ok(_) => False
	}
}

## Disabling header lints preserves syntax parsing and does not silently check names.
expect {
	source = "name,name,\nAda,Alan,\n"
	CSV.validate({ source, header: Exact, lints: { duplicate_headers: False, empty_headers: False } }) == CSV.parse_str(source, {}).map_ok(|document| document.records)
}

## Syntax errors take precedence over header lint checks.
expect match CSV.validate({ source: "name,name\n\"unterminated", header: Exact }) {
	Err(InvalidCsv(error)) => error.problem == Syntax(UnterminatedQuotedField) and error.location == Text({ offset: 10, line: 2, column: 1 })
	Ok(_) => False
}

## Checked typed decoding preserves normalized headers and headerless tuples.
expect {
	result : Try(List({ first_name : Str }), [InvalidCsv(CSV.Error)])
	result = CSV.validate_as({ source: " First Name \nAda\n", header: Normalized })
	result == Ok([{ first_name: "Ada" }])
}
expect {
	result : Try(List((Str, U64)), [InvalidCsv(CSV.Error)])
	result = CSV.validate_as({ source: "Ada,36\nAlan,41", header: Absent })
	result == Ok([("Ada", 36), ("Alan", 41)])
}

## A missing schema field has no fabricated physical cell position.
expect {
	result : Try(List({ required : Str }), [InvalidCsv(CSV.Error)])
	result = CSV.validate_as({ source: "other\nvalue" })
	match result {
		Err(InvalidCsv(error)) => error.problem == MissingRequiredField("required") and error.location == Unavailable(NotRetained) and error.path == [Key("required"), Source("<literal>")]
		Ok(_) => False
	}
}

CheckedCsvFailure := [Never].{
	is_eq : _
	parser_for : CSV.Format(errors) -> (CSV.State -> Try({ value : CheckedCsvFailure, rest : CSV.State }, [InvalidCsv(CSV.Error), CustomCsvFailure(Str), ..errors]))
	parser_for = |_| |_| Err(CustomCsvFailure("custom detail"))
}

## Custom type-directed failures remain tagged values during runtime validation.
expect {
	result : Try(List(CheckedCsvFailure), [InvalidCsv(CSV.Error), CustomCsvFailure(Str)])
	result = CSV.validate_as({ source: "header\nvalue" })
	result == Err(CustomCsvFailure("custom detail"))
}

# The streaming scanner and complete parser agree at every byte boundary,
# including UTF-8, doubled quotes, CRLF, and malformed quoted fields.
stream_records_for_test : List(List(U8)) -> Try(List(CSV.Record), [InvalidCsv(CSV.Error)])
stream_records_for_test = |chunks| {
	var $rows = []
	for event in (CSV.decoder({})).read_iter(chunks.iter()) {
		match event {
			Ok(Value(row)) => {
				$rows = $rows.append(row)
			}
			Ok(Finished(_)) => return Ok($rows)
			Ok(Rejected(InvalidCsv(error))) | Err(InvalidCsv(error)) => return Err(InvalidCsv(error))
			Err(_) => crash "streaming protocol failure"

		}
	}
	crash "missing finished event"
}
expect ["", "\n", "a,b\r\nc,d", "a\r\n\nb", "é,😀", "\u(feff)a", "a\"b, \"c\" ", "\"a\r\nb\",\"x\"\"y\"\n", "a,\"b", "x\n\"a\"b,c"].all(
	|text| {
		bytes = text.to_utf8()
		(0..=bytes.len()).iter().fold(True, |good, split| good and stream_records_for_test([bytes.take_first(split), [], bytes.drop_first(split)]) == CSV.parse_str(text, {}).map_ok(|document| document.records))
	},
)
expect CSV.encode_record(["a,b".to_utf8(), "x\"y".to_utf8()], {}) == Ok("\"a,b\",\"x\"\"y\"\r\n".to_utf8())
expect CSV.encode_record([[]], {}) == Ok("\"\"\r\n".to_utf8())
expect CSV.encode_record([], {}).is_err()
expect CSV.encode_record(["a".to_utf8()], { max_record_bytes: 3 }) == Ok("a\r\n".to_utf8())
expect CSV.encode_record(["a".to_utf8()], { max_record_bytes: 2 }).is_err()
expect {
	encoded = List.from_iter((CSV.encoder({ chunk_bytes: 1 })).write_iter([["a,b".to_utf8(), "é".to_utf8()], [[]]].iter()))
	encoded.all(|result| result.map_ok(|bytes| bytes.len() <= 1) == Ok(True)) and stream_records_for_test(encoded.map(|chunk| chunk ?? [])) == Ok([["a,b".to_utf8(), "é".to_utf8()], [[]]])
}
expect {
	decode = Parser.many(CSV.field(CSV.u64({})))
	decoder = CSV.positional_decoder({ row: decode, errors: { policy: ReportInvalidRecords } })
	match decoder.collect_report(["1\nwrong\n2\n".to_utf8()].iter()) {
		Ok({ values, errors, report }) => values == [[1], [2]] and errors.len() == 1 and errors.all(
			|error| match error {
				InvalidRow(at) => csv_coordinates(at).line == 2
				_ => False
			},
		) and report == { emitted_items: 2, rejected_records: 1 }
		Err(_) => False
	}
}
expect {
	result : Try(List({ name : Str, age : U64 }), _)
	result = (CSV.typed_decoder({ header: Exact, reader: {} })).collect(["name,age\r".to_utf8(), "\nAda,36\nAlan,41".to_utf8()].iter())
	result == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
}
expect {
	result : Try(List({ id : U64 }), _)
	result = CSV.decode_str("id\nwrong\n\"unterminated", {})
	match result {
		Err(InvalidCsv(error)) => csv_coordinates(error).record == 2 and error.problem == ExpectedValue({ expected: U64, source: Diagnostic.excerpt("wrong".to_utf8()) })
		_ => False
	}
}
expect {
	decode = Parser.many(CSV.field(CSV.u64({})))
	match CSV.rows_parser({ row: decode }).parse_str("bad\n\"unterminated") {
		Err(ParseError({ problem: InvalidRow(error), .. })) => csv_coordinates(error).record == 1 and error.problem == ParserFailure(FieldProblem(Diagnostic.{ problem: ExpectedValue({ expected: U64, source: Diagnostic.excerpt("bad".to_utf8()) }), location: ByteOffset(0), path: [] }))
		_ => False
	}
}

expect CSV.encode_record(["a".to_utf8(), "b".to_utf8()], { quoting: Always, line_ending: Lf }) == Ok("\"a\",\"b\"\n".to_utf8())
expect CSV.encode_record(["a".to_utf8()], { line_ending: Cr }) == Ok("a\r".to_utf8())

expect (CSV.decoder({ max_field_bytes: 2 })).collect(["\"a\"\"\"\n".to_utf8()].iter()) == Ok([["a\"".to_utf8()]])
expect (CSV.decoder({ max_field_bytes: 1 })).collect(["\"a\"\"\"\n".to_utf8()].iter()).is_err()
expect (CSV.decoder({ max_fields: 2 })).collect(["a,b".to_utf8()].iter()) == Ok([["a".to_utf8(), "b".to_utf8()]])
expect (CSV.decoder({ max_fields: 2 })).collect(["a,b,c".to_utf8()].iter()).is_err()
expect (CSV.decoder({ max_record_bytes: 3 })).collect(["a,b".to_utf8()].iter()) == Ok([["a".to_utf8(), "b".to_utf8()]])
expect (CSV.decoder({ max_record_bytes: 2 })).collect(["a,b".to_utf8()].iter()).is_err()

# User parser tags and byte offsets survive positional row decoding unchanged.
expect {
	custom = CSV.field(Parser.custom(|_| Err(ParseError(Diagnostic.{ problem: OutsideRange({ max: 10.U64 }), location: InputOffset(2), path: [] }))))
	match CSV.rows_parser({ row: custom }).parse_str("123") {
		Err(ParseError({ problem: InvalidRow(error), .. })) => error.problem == ParserFailure(FieldProblem(Diagnostic.{ problem: OutsideRange({ max: 10 }), location: ByteOffset(2), path: [] })) and error.path == [Record(1), Field(1)]
		_ => False
	}
}

# Native CSV fields compose with UTF-8 and caller-specific problems.
expect {
	row = CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string({})))
		.keep(CSV.field(Parser.Utf8.digits({})))
	CSV.rows_parser({ row: row }).parse_str("apples,42\n") == Ok([{ name: "apples", count: 42 }])
}
expect {
	even = Parser.Utf8.digits({}).map(
		|value| if value % 2 == 0 {
			Ok(value)
		} else {
			Err(OddCount(value))
		},
	).flatten()
	row = CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string({})))
		.keep(CSV.field(even))
	match CSV.rows_parser({ row: row }).parse_str("apples,3\n") {
		Err(ParseError({ problem: InvalidRow(error), .. })) => csv_coordinates(error).record == 1 and csv_coordinates(error).field == 2 and csv_coordinates(error).line == 1 and csv_coordinates(error).column == 8 and error.problem == ParserFailure(FieldProblem(Diagnostic.{ problem: OddCount(3), location: ByteOffset(0), path: [] }))
		_ => False
	}
}

decoded_error : Diagnostic(p) -> Diagnostic(p)
decoded_error = |error| {
	location = match error.location {
		InputOffset(offset) => ByteOffset(offset)
		_ => error.location
	}
	{ ..error, location }
}

row_error : U64, Diagnostic(Parser.Problem(p)) -> CSV.RowError(p)
row_error = |record, error| {
	fields = match error.location {
		InputOffset(index) => [Field(index + 1)]
		_ => []
	}
	location = match error.location {
		InputOffset(_) => Unavailable(NotRetained)
		_ => error.location
	}
	Diagnostic.{ problem: ParserFailure(error.problem), location, path: [Record(record)].concat(fields).concat(error.path) }
}

# The sole conversion from logical CSV coordinates into shared diagnostics.
csv_error : { record : U64, field : U64, line : U64, column : U64, problem : p } -> Diagnostic(p)
csv_error = |error| {
	path = (if (error.record > 0) [Record(error.record)] else [])
		.concat(if (error.field > 0) [Field(error.field)] else [])
	location = if (error.line > 0 and error.column > 0) LineColumn({ line: error.line, column: error.column }) else Unavailable(NotRetained)
	Diagnostic.{ problem: error.problem, location, path }
}

csv_coordinates : Diagnostic(p) -> { record : U64, field : U64, line : U64, column : U64 }
csv_coordinates = |error| {
	var $record = 0.U64
	var $field = 0.U64
	for part in error.path {
		match part {
			Record(index) => {
				$record = index
			}
			Field(index) => {
				$field = index
			}
			_ => {}
		}
	}
	position = Diagnostic.line_column(error.location) ?? { line: 0, column: 0 }
	{ record: $record, field: $field, line: position.line, column: position.column }
}

## Parsed documents support receiver decoding and encoding without another format import.
expect {
	document = CSV.parse_str("name,age\r\nAda,36\r\n", {})?
	rows : List({ name : Str, age : U64 })
	rows = document.decode({})?
	actual = document.encode({})?
	rows == [{ name: "Ada", age: 36 }] and actual == "name,age\r\nAda,36\r\n".to_utf8()
}

## Detached document failures keep logical record and field paths without source coordinates.
expect {
	document = CSV.{ records: [["age".to_utf8()], ["bad".to_utf8()]] }
	result : Try(List({ age : U64 }), [InvalidCsv(CSV.Error)])
	result = document.decode({})
	match result {
		Err(InvalidCsv(error)) => error.location == Unavailable(NotRetained) and error.path == [Record(2), Field(1)]
		Ok(_) => False
	}
}

## Reader limits retain their resource, configured maximum and first violating count.
expect match CSV.decoder({ max_field_bytes: 1 }).collect(["ab\n".to_utf8()].iter()) {
	Err(InvalidCsv(error)) => error.problem == LimitExceeded({ resource: FieldBytes, max: 1, observed: AtLeast(2) }) and error.location == Text({ offset: 0, line: 1, column: 1 }) and error.path == [Record(1), Field(1)]
	_ => False
}

## Writer failures identify generated values and preserve exact field counts.
expect CSV.encode_record(["a".to_utf8(), "b".to_utf8()], { max_fields: 1 }) == Err(InvalidCsv(Diagnostic.{ problem: LimitExceeded({ resource: FieldCount, max: 1, observed: Exact(2) }), location: Unavailable(GeneratedValue), path: [] }))

## Streaming writer failures add the logical record without fabricating text positions.
expect {
	results = List.from_iter(CSV.encoder({}).write_iter([["good".to_utf8()], []].iter()))
	match results.last()? {
		Err(InvalidCsv(error)) => error.problem == EmptyRecord and error.location == Unavailable(GeneratedValue) and error.path == [Record(2)]
		_ => False
	}
}

## Typed and positional readers share recovery configuration at the outer props level.
expect {
	row = Parser.many(CSV.field(CSV.u64({})))
	report = CSV.positional_decoder({ row, reader: { max_field_bytes: 16 }, errors: { policy: ReportInvalidRecords } }).collect_report(["1\nbad\n2\n".to_utf8()].iter())?
	report.values == [[1], [2]] and report.errors.len() == 1 and report.report.rejected_records == 1
}

## Derived parsing shares the caller's error row without losing custom failures.
expect {
	decode : Str -> Try(List({ name : Str, age : U64 }), [CallerFailure, InvalidCsv(CSV.Error)])
	decode = |source| {
		rows = CSV.decode_str(source, {})?
		Ok(rows)
	}
	decode("name,age\nAda,36\n") == Ok([{ name: "Ada", age: 36 }])
}
expect {
	decode : Str -> Try(List({ status : CheckedCsvFailure }), [CallerFailure, InvalidCsv(CSV.Error), CustomCsvFailure(Str)])
	decode = |source| {
		rows = CSV.decode_str(source, {})?
		Ok(rows)
	}
	decode("status\nvalue\n") == Err(CustomCsvFailure("custom detail"))
}
