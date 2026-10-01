import Parser
import String

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
## The first row is treated as data rather than as headings.
CSV :: { records : List(List(String.Utf8)) }.{

	## Compare two decoded CSV values structurally.
	is_eq : _

	## One CSV row represented as a list of raw UTF-8 fields.
	CSVRecord : List(CSVField)

	## One raw UTF-8 field from a CSV row.
	CSVField : String.Utf8

	## Parse CSV text and decode every record with the supplied record parser.
	parse_str : Parser(CSVRecord, a), Str -> Try(List(a), [ParsingFailure(Str), SyntaxError(Str), ParsingIncomplete(CSVRecord)])
	parse_str = |csv_parser, input| {
		match parse_str_to_csv(input) {
			Err(ParsingIncomplete(rest)) => {
				rest_str = String.str_from_utf8(rest)

				Err(SyntaxError(rest_str))
			}

			Err(ParsingFailure(str)) => {
				Err(ParsingFailure(str))
			}

			Ok(csv_data) => {
				match parse_csv(csv_parser, csv_data) {
					Err(ParsingFailure(str)) => {
						Err(ParsingFailure(str))
					}

					Err(ParsingIncomplete(problem)) => {
						Err(ParsingIncomplete(problem))
					}

					Ok(vals) => {
						Ok(vals)
					}
				}
			}
		}
	}

	## Decode every record in an already parsed `CSV` value.
	parse_csv : Parser(CSVRecord, a), CSV -> Try(List(a), [ParsingFailure(Str), ParsingIncomplete(CSVRecord)])
	parse_csv = |csv_parser, { records: csv_data }| {
		csv_data
			.map_with_index(
				|record_fields_list, index| {
					{ record: record_fields_list, index: index }
				},
			)
			.fold_until(
				Try.Ok([]),
				|state, { record: record_fields_list, index: index }| {
					match parse_csv_record(csv_parser, record_fields_list) {
						Err(ParsingFailure(problem)) => {
							index_str = (index + 1).to_str()
							record_str =
								record_fields_list
									.map(String.str_from_utf8)
									.map(
										|val| {
											"\"${val}\""
										},
									)
									|> Str.join_with(", ")
							problem_str = "${problem}\nWhile parsing record no. ${index_str}: `${record_str}`"

							Break(Err(ParsingFailure(problem_str)))
						}

						Err(ParsingIncomplete(problem)) => {
							Break(Err(ParsingIncomplete(problem)))
						}

						Ok(val) => {
							state
								.map_ok(
									|vals| {
										vals.append(val)
									},
								)
								|> Continue
						}
					}
				},
			)
	}

	## Decode one `CSVRecord` with the supplied record parser.
	##
	## Parsing succeeds only when the record parser consumes every field.
	parse_csv_record : Parser(CSVRecord, a), CSVRecord -> Try(a, [ParsingFailure(Str), ParsingIncomplete(CSVRecord)])
	parse_csv_record = |csv_parser, record_fields_list| {
		Parser.parse(
			csv_parser,
			record_fields_list,
			|leftover| {
				leftover == []
			},
		)
	}

	## Start a record parser with a curried constructor for the desired value.
	##
	## ```roc
	## record(|first_name| |last_name| |age| User({ first_name, last_name, age }))
	## .field(string)
	## .field(string)
	## .field(u64)
	## ```
	record : a -> Parser(CSVRecord, a)
	record = |f| {
		Parser.const(f)
	}

	## Consume the next field of a `CSVRecord` using a UTF-8 field parser.
	field : Parser(String.Utf8, a) -> Parser(CSVRecord, a)
	field = |field_parser| {
		Parser.build_primitive_parser(
			|fields_list| {
				match fields_list.get(0) {
					Err(OutOfBounds) =>
						Err(ParsingFailure("expected another CSV field but there are no more fields in this record"))

					Ok(raw_str) => {
						match String.parse_utf8(field_parser, raw_str) {
							Ok(val) => {
								Ok({ val: val, input: fields_list.drop_first(1) })
							}

							Err(ParsingFailure(reason)) => {
								field_str = raw_str |> String.str_from_utf8

								Err(ParsingFailure("Field `${field_str}` could not be parsed. ${reason}"))
							}

							Err(ParsingIncomplete(reason)) => {
								reason_str = String.str_from_utf8(reason)
								fields_str =
									fields_list
										.map(String.str_from_utf8)
										|> Str.join_with(", ")

								Err(ParsingFailure("The field parser was unable to read the whole field: `${reason_str}` while parsing the first field of leftover ${fields_str})"))
							}
						}
					}
				}
			},
		)
	}

	## Parse one CSV field as a valid UTF-8 string.
	string : Parser(CSVField, Str)
	string = String.any_string

	## Parse one CSV field as an unsigned 64-bit integer.
	u64 : Parser(CSVField, U64)
	u64 =
		string
			.map(
				|val| {
					match U64.from_str(val) {
						Ok(num) => Ok(num)
						Err(_) => Err("${val} is not a U64.")
					}
				},
			)
			.flatten()

	## Parse one CSV field as a 64-bit floating-point number.
	f64 : Parser(CSVField, F64)
	f64 =
		string
			.map(
				|val| {
					match F64.from_str(val) {
						Ok(num) => Ok(num)
						Err(_) => Err("${val} is not a F64.")
					}
				},
			)
			.flatten()

	## Parse CSV text into raw records and UTF-8 fields.
	parse_str_to_csv : Str -> Try(CSV, [ParsingFailure(Str), ParsingIncomplete(String.Utf8)])
	parse_str_to_csv = |input| {
		Parser.parse(
			file,
			input.to_utf8(),
			|leftover| {
				leftover == []
			},
		)
	}

	## Parse one CSV row into raw UTF-8 fields.
	parse_str_to_csv_record : Str -> Try(CSVRecord, [ParsingFailure(Str), ParsingIncomplete(String.Utf8)])
	parse_str_to_csv_record = |input| {
		Parser.parse(
			csv_record,
			input.to_utf8(),
			|leftover| {
				leftover == []
			},
		)
	}

	## Parse a complete RFC 4180-style CSV file into raw records and fields.
	file : Parser(String.Utf8, CSV)
	file =
		csv_records
			.map(
				|records| {
					{ records }
				},
			)
}

csv_record : Parser(String.Utf8, CSV.CSVRecord)
csv_record = Parser.build_primitive_parser(
	|bytes| {
		match scan_record(bytes, 0) {
			Ok({ fields, next }) => Ok({ val: fields, input: bytes.drop_first(next) })
			Err(BadField(at)) => Err(ParsingFailure(bad_field_message(bytes, at)))
		}
	},
)

## Parse records until the input ends; on a malformed field, stop and leave the
## input from the start of that field unconsumed.
csv_records : Parser(String.Utf8, List(CSV.CSVRecord))
csv_records = Parser.build_primitive_parser(
	|bytes| {
		len = bytes.len()
		var $records = []
		var $pos = 0
		var $rest = Err(Done)
		var $more = len > 0
		while $more {
			match scan_record(bytes, $pos) {
				Err(BadField(at)) => {
					$rest = Ok(at)
					$more = Bool.False
				}
				Ok({ fields, next }) => {
					$records = $records.append(fields)
					after_break =
						match bytes.get(next) {
							Ok('\r') => if bytes.get(next + 1) == Ok('\n') next + 2 else next + 1
							Ok('\n') => next + 1
							_ => next
						}
					$pos = after_break
					$more = after_break < len
				}
			}
		}
		match $rest {
			Ok(at) => Ok({ val: $records, input: bytes.drop_first(at) })
			Err(Done) => Ok({ val: $records, input: [] })
		}
	},
)

bad_field_message : String.Utf8, U64 -> Str
bad_field_message = |bytes, at| {
	"malformed quoted CSV field at byte ${at.to_str()}: `${String.str_from_utf8(bytes.drop_first(at))}`"
}

## Scan one record starting at `start`, stopping before its line break (or at
## the end of input). Fails with the offset of a malformed quoted field.
scan_record : String.Utf8, U64 -> Try({ fields : CSV.CSVRecord, next : U64 }, [BadField(U64)])
scan_record = |bytes, start| {
	len = bytes.len()
	var $fields = []
	var $pos = start
	var $result = Err(BadField(start))
	var $more = Bool.True
	while $more {
		scanned =
			if bytes.get($pos) == Ok('"') {
				scan_quoted(bytes, $pos)
			} else {
				end = scan_unquoted(bytes, $pos)
				Ok({ field: bytes.sublist({ start: $pos, len: end - $pos }), next: end })
			}
		match scanned {
			Err(bad) => {
				$result = Err(bad)
				$more = Bool.False
			}
			Ok({ field, next }) => {
				$fields = $fields.append(field)
				if next < len and bytes.get(next) == Ok(',') {
					$pos = next + 1
				} else {
					$result = Ok({ fields: $fields, next })
					$more = Bool.False
				}
			}
		}
	}
	$result
}

## Offset of the first `,`, CR or LF at or after `start`, or the input length.
scan_unquoted : String.Utf8, U64 -> U64
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
scan_quoted : String.Utf8, U64 -> Try({ field : CSV.CSVField, next : U64 }, [BadField(U64)])
scan_quoted = |bytes, start| {
	len = bytes.len()
	var $field = []
	var $chunk_start = start + 1
	var $pos = start + 1
	var $result = Err(BadField(start))
	var $more = Bool.True
	while $more {
		if $pos >= len {
			# Unterminated quoted field.
			$more = Bool.False
		} else if bytes.get($pos) == Ok('"') {
			$field = $field.concat(bytes.sublist({ start: $chunk_start, len: $pos - $chunk_start }))
			if bytes.get($pos + 1) == Ok('"') {
				$field = $field.append('"')
				$pos = $pos + 2
				$chunk_start = $pos
			} else {
				after = $pos + 1
				if after >= len or is_delimiter(bytes.get(after) ?? 0) {
					$result = Ok({ field: $field, next: after })
				}
				$more = Bool.False
			}
		} else {
			$pos = $pos + 1
		}
	}
	$result
}

## Empty input has no records; a blank line is one record with one empty field.
expect CSV.parse_str_to_csv("") == Ok({ records: [] })
expect CSV.parse_str_to_csv("\n") == Ok({ records: [[[]]] })
expect CSV.parse_str_to_csv("a\n\nb") == Ok({ records: [["a".to_utf8()], [[]], ["b".to_utf8()]] })

## The final line break is optional, whichever style it is.
expect CSV.parse_str_to_csv("a,b\r\nc,d\r\n") == Ok({ records: [["a".to_utf8(), "b".to_utf8()], ["c".to_utf8(), "d".to_utf8()]] })
expect CSV.parse_str_to_csv("a\n") == Ok({ records: [["a".to_utf8()]] })
expect CSV.parse_str_to_csv("a,\n") == Ok({ records: [["a".to_utf8(), []]] })

## A lone CR separates records; CR LF is a single break.
expect CSV.parse_str_to_csv("a\rb\r") == Ok({ records: [["a".to_utf8()], ["b".to_utf8()]] })
expect CSV.parse_str_to_csv("a\r\rb") == Ok({ records: [["a".to_utf8()], [[]], ["b".to_utf8()]] })
expect CSV.parse_str_to_csv("a\r\n\nb") == Ok({ records: [["a".to_utf8()], [[]], ["b".to_utf8()]] })

## Fields hold any UTF-8 and control characters, and bare quotes are literal.
expect CSV.parse_str_to_csv("é,😀\t\u(0)\u(7f)") == Ok({ records: [["é".to_utf8(), "😀\t\u(0)\u(7f)".to_utf8()]] })
expect CSV.parse_str_to_csv("a\"b, \"c\" ") == Ok({ records: [["a\"b".to_utf8(), " \"c\" ".to_utf8()]] })
expect CSV.parse_str_to_csv("\u(feff)a") == Ok({ records: [["\u(feff)a".to_utf8()]] })

## Quoted fields hold delimiters and doubled quotes.
expect CSV.parse_str_to_csv("\"a,\r\nb\"\"\",\"\"") == Ok({ records: [["a,\r\nb\"".to_utf8(), []]] })

## Text after a closing quote, or a missing closing quote, is an error.
expect {
	match CSV.parse_str_to_csv("x\n\"a\"b,c") {
		Err(ParsingIncomplete(rest)) => rest == "\"a\"b,c".to_utf8()
		_ => Bool.False
	}
}
expect {
	match CSV.parse_str_to_csv("a,\"b") {
		Err(ParsingIncomplete(rest)) => rest == "\"b".to_utf8()
		_ => Bool.False
	}
}

## One record parses alone; a line break after it is left over.
expect CSV.parse_str_to_csv_record("a,\"b\nc\"") == Ok(["a".to_utf8(), "b\nc".to_utf8()])
expect CSV.parse_str_to_csv_record("") == Ok([[]])
expect {
	match CSV.parse_str_to_csv_record("a\n") {
		Err(ParsingIncomplete(rest)) => rest == ['\n']
		_ => Bool.False
	}
}
