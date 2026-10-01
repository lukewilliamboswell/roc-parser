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
## The first row is treated as data rather than as headings.
##
## Decode typed values by building a record parser with `record`, `field` and
## `Parser.keep`, then running it with `parse_str`:
##
## ```roc
## user : Parser(CSV.CSVRecord, { name : Str, age : U64 })
## user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.u64))
##
## expect CSV.parse_str(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
## ```
CSV :: { records : List(List(Utf8.Bytes)) }.{

	## Compare two decoded CSV values structurally.
	is_eq : _

	## One CSV row represented as a list of raw UTF-8 fields.
	CSVRecord : List(CSVField)

	## One raw UTF-8 field from a CSV row.
	CSVField : Utf8.Bytes

	## Parse CSV text and decode every record with the supplied record parser.
	##
	## Returns `SyntaxError(rest)` when the text is not valid CSV (for example an
	## unterminated quoted field), `ParsingFailure(msg)` naming the record number
	## when a record does not decode, and `ParsingIncomplete(fields)` when the
	## record parser leaves fields unused.
	parse_str : Parser(CSVRecord, a), Str -> Try(List(a), [ParsingFailure(Str), SyntaxError(Str), ParsingIncomplete(CSVRecord)])
	parse_str = |csv_parser, input| {
		match parse_str_to_csv(input) {
			Err(ParsingIncomplete(rest)) => {
				rest_str = Utf8.str_from_utf8(rest)

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
	##
	## Stops at the first record that fails; the failure message includes the
	## 1-based record number and the record's fields.
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
									.map(Utf8.str_from_utf8)
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
	## Add one `.keep(CSV.field(...))` per column, in column order:
	##
	## ```roc
	## CSV.record(|first_name| |last_name| |age| User({ first_name, last_name, age }))
	##     .keep(CSV.field(CSV.string))
	##     .keep(CSV.field(CSV.string))
	##     .keep(CSV.field(CSV.u64))
	## ```
	record : a -> Parser(CSVRecord, a)
	record = |f| {
		Parser.const(f)
	}

	## Consume the next field of a `CSVRecord` using a UTF-8 field parser.
	##
	## The field parser must consume the whole field. Fails when the record has
	## no fields left.
	field : Parser(Utf8.Bytes, a) -> Parser(CSVRecord, a)
	field = |field_parser| {
		Parser.build_primitive_parser(
			|fields_list| {
				match fields_list.get(0) {
					Err(OutOfBounds) =>
						Err(ParsingFailure("expected another CSV field but there are no more fields in this record"))

					Ok(raw_str) => {
						match Utf8.parse_utf8(field_parser, raw_str) {
							Ok(val) => {
								Ok({ value: val, rest: fields_list.drop_first(1) })
							}

							Err(ParsingFailure(reason)) => {
								field_str = raw_str |> Utf8.str_from_utf8

								Err(ParsingFailure("Field `${field_str}` could not be parsed. ${reason}"))
							}

							Err(ParsingIncomplete(reason)) => {
								reason_str = Utf8.str_from_utf8(reason)
								fields_str =
									fields_list
										.map(Utf8.str_from_utf8)
										|> Str.join_with(", ")

								Err(ParsingFailure("The field parser was unable to read the whole field: `${reason_str}` while parsing the first field of leftover ${fields_str})"))
							}
						}
					}
				}
			},
		)
	}

	## Parse one CSV field as a valid UTF-8 string, kept verbatim (no trimming).
	string : Parser(CSVField, Str)
	string = Utf8.any_string

	## Parse one CSV field as an unsigned 64-bit integer.
	##
	## The field must be ASCII decimal digits with an optional leading `+`, and
	## fit in a U64 (the grammar of Rust's `u64::from_str`). Whitespace,
	## underscores, signs other than `+` and `0x`-style prefixes are rejected.
	u64 : Parser(CSVField, U64)
	u64 =
		string
			.map(
				|val| {
					digits =
						match val.to_utf8() {
							['+', .. as rest] => rest
							bytes => bytes
						}
					if all_digits(digits) {
						match U64.from_str(val) {
							Ok(num) => Ok(num)
							Err(_) => Err("${val} is not a U64.")
						}
					} else {
						Err("${val} is not a U64.")
					}
				},
			)
			.flatten()

	## Parse one CSV field as a 64-bit floating-point number.
	##
	## The field is an optional sign followed by `inf`, `infinity` or `nan` in
	## any case, or by a decimal number with an optional fraction and exponent
	## (`12`, `1.5`, `.5`, `5.`, `1e-3`, `2.5E+10`); this is the grammar of
	## Rust's `f64::from_str`. Magnitudes too large for an F64 become infinity
	## and too small ones become zero. Whitespace, underscores and hexadecimal
	## forms are rejected.
	f64 : Parser(CSVField, F64)
	f64 =
		string
			.map(
				|val| {
					match decimal_f64(val) {
						Ok(num) => Ok(num)
						Err(_) => Err("${val} is not a F64.")
					}
				},
			)
			.flatten()

	## Parse CSV text into raw records and UTF-8 fields without decoding them.
	##
	## Use this to inspect rows of varying shape, or to decode later with `parse_csv`.
	parse_str_to_csv : Str -> Try(CSV, [ParsingFailure(Str), ParsingIncomplete(Utf8.Bytes)])
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
	##
	## A line break after the row is not consumed, so it is reported as
	## `ParsingIncomplete`.
	parse_str_to_csv_record : Str -> Try(CSVRecord, [ParsingFailure(Str), ParsingIncomplete(Utf8.Bytes)])
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
	##
	## This is the parser behind `parse_str_to_csv`, for use inside larger parsers.
	file : Parser(Utf8.Bytes, CSV)
	file =
		csv_records
			.map(
				|records| {
					{ records }
				},
			)
}

csv_record : Parser(Utf8.Bytes, CSV.CSVRecord)
csv_record = Parser.build_primitive_parser(
	|bytes| {
		match scan_record(bytes, 0) {
			Ok({ fields, next }) => Ok({ value: fields, rest: bytes.drop_first(next) })
			Err(BadField(at)) => Err(ParsingFailure(bad_field_message(bytes, at)))
		}
	},
)

## Parse records until the input ends; on a malformed field, stop and leave the
## input from the start of that field unconsumed.
csv_records : Parser(Utf8.Bytes, List(CSV.CSVRecord))
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
			Ok(at) => Ok({ value: $records, rest: bytes.drop_first(at) })
			Err(Done) => Ok({ value: $records, rest: [] })
		}
	},
)

bad_field_message : Utf8.Bytes, U64 -> Str
bad_field_message = |bytes, at| {
	"malformed quoted CSV field at byte ${at.to_str()}: `${Utf8.str_from_utf8(bytes.drop_first(at))}`"
}

## Scan one record starting at `start`, stopping before its line break (or at
## the end of input). Fails with the offset of a malformed quoted field.
scan_record : Utf8.Bytes, U64 -> Try({ fields : CSV.CSVRecord, next : U64 }, [BadField(U64)])
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
scan_unquoted : Utf8.Bytes, U64 -> U64
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
scan_quoted : Utf8.Bytes, U64 -> Try({ field : CSV.CSVField, next : U64 }, [BadField(U64)])
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

all_digits : List(U8) -> Bool
all_digits = |bytes| !bytes.is_empty() and bytes.all(|b| b >= '0' and b <= '9')

lowercase_ascii : List(U8) -> List(U8)
lowercase_ascii = |bytes| bytes.map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)

## Parse a float using the grammar documented on `CSV.f64`.
decimal_f64 : Str -> Try(F64, [NotAFloat])
decimal_f64 = |text| {
	bytes = text.to_utf8()
	{ negative, unsigned } =
		match bytes {
			['-', .. as rest] => { negative: Bool.True, unsigned: rest }
			['+', .. as rest] => { negative: Bool.False, unsigned: rest }
			_ => { negative: Bool.False, unsigned: bytes }
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
				Err(NoExponent) => Bool.True
			}
		if mantissa_ok and exponent_ok {
			match F64.from_str(text) {
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
	actual = Utf8.parse_utf8(CSV.u64, text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted
		(Err(_), Err({})) => Bool.True
		_ => Bool.False
	}
}

parses_f64 : Str, Try(F64, {}) -> Bool
parses_f64 = |text, expected| {
	actual = Utf8.parse_utf8(CSV.f64, text.to_utf8())
	match (actual, expected) {
		(Ok(value), Ok(wanted)) => value == wanted or (value.is_nan() and wanted.is_nan())
		(Err(_), Err({})) => Bool.True
		_ => Bool.False
	}
}

## U64 fields are `+?[0-9]+`; Roc literal syntax is not accepted.
expect parses_u64("0", Ok(0)) and parses_u64("+5", Ok(5)) and parses_u64("007", Ok(7))
expect parses_u64("18446744073709551615", Ok(18446744073709551615)) and parses_u64("18446744073709551616", Err({}))
expect ["", "+", "-0", "-1", " 1", "1 ", "1_000", "0x10", "0b1", "1e3", "1.0"].all(|text| parses_u64(text, Err({})))

## F64 fields follow Rust's f64::from_str grammar, saturating out-of-range values.
expect parses_f64("1.5", Ok(1.5)) and parses_f64(".5", Ok(0.5)) and parses_f64("5.", Ok(5.0)) and parses_f64("-2.5E+1", Ok(-25.0))
expect parses_f64("inf", Ok(F64.infinity)) and parses_f64("-Infinity", Ok(-F64.infinity)) and parses_f64("NaN", Ok(F64.nan))
expect parses_f64("1e400", Ok(F64.infinity)) and parses_f64("-1e400", Ok(-F64.infinity)) and parses_f64("1e-400", Ok(0.0))
expect parses_f64("0e99999999999999999999", Ok(0.0)) and parses_f64("1.7976931348623159e308", Ok(F64.infinity))
expect ["", "+", "-", ".", "e5", "1e", "1e+", "1_0.5", "0x10", "0x1p3", " 1", "1 ", "1.2.3", "--1", "infinit", "nan1"].all(|text| parses_f64(text, Err({})))

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

## Typed decoding runs a record parser over every row, as in the module docs.
expect {
	user : Parser(CSV.CSVRecord, { name : Str, age : U64 })
	user = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.u64))
	CSV.parse_str(user, "Ada,36\nAlan,41\n") == Ok([{ name: "Ada", age: 36 }, { name: "Alan", age: 41 }])
}
