import Parser

## Parsers and helpers specialized to UTF-8 byte lists and Roc `Str` values.
##
## Use these with the combinators in [Parser] whenever the input is text.
## Parsers work on bytes (`Bytes`), so `codeunit` and `digit` match single
## bytes, and `parse_str` converts a `Str` to bytes and back for you.
Utf8 :: [].{

	## UTF-8 input represented as a list of bytes.
	Bytes : List(U8)

	## Parse a whole `Str` using a [Parser].
	##
	## Fails with `ParseError({ message, offset })`, where `offset` is the byte
	## offset of the furthest failure. Input left over after the parser
	## succeeds is a failure too (see [Parser.parse]).
	##
	## ```roc
	## color : Parser(Utf8.Bytes, [Red, Green, Blue])
	## color =
	##     Parser.one_of([
	##         Parser.const(Red).skip(Utf8.string("red")),
	##         Parser.const(Green).skip(Utf8.string("green")),
	##         Parser.const(Blue).skip(Utf8.string("blue")),
	##     ])
	##
	## expect Utf8.parse_str(color, "green") == Ok(Green)
	## ```
	parse_str : Parser(Bytes, a), Str -> Try(a, [ParseError({ message : Str, offset : U64 })])
	parse_str = |parser, input| {
		parser.parse(input.to_utf8())
	}

	## Runs a parser against the start of a string, allowing the parser to consume it only partially.
	##
	## - If the parser succeeds, returns the resulting `value` and the `rest` of the string.
	##   A `rest` that starts inside a multi-byte character is rendered with
	##   U+FFFD replacement characters rather than crashing.
	## - If the parser fails, returns `Err(ParseError({ message, offset }))`.
	##
	## ```roc
	## at_sign : Parser(Utf8.Bytes, [AtSign])
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str_partial(at_sign, "@").map_ok(|r| r.value) == Ok(AtSign)
	## expect Utf8.parse_str_partial(at_sign, "$").is_err()
	## ```
	parse_str_partial : Parser(Bytes, a), Str -> Try({ value : a, rest : Str }, [ParseError({ message : Str, offset : U64 })])
	parse_str_partial = |parser, input| {
		parser
			.run(input.to_utf8())
			.map_ok(|{ value, rest: remaining }| { value, rest: str_from_utf8(remaining) })
	}

	## Runs a parser against UTF-8 bytes, requiring the parser to consume them fully.
	##
	## Fails with `ParseError({ message, offset })` like [Utf8.parse_str].
	parse_bytes : Parser(Bytes, a), Bytes -> Try(a, [ParseError({ message : Str, offset : U64 })])
	parse_bytes = |parser, input| {
		parser.parse(input)
	}

	## Runs a parser against the start of UTF-8 bytes, allowing the parser to consume them only partially.
	##
	## Returns the parsed `value` and the `rest` of the bytes, or
	## `Err(ParseError({ message, offset }))`.
	parse_bytes_partial : Parser(Bytes, a), Bytes -> Try({ value : a, rest : Bytes }, [ParseError({ message : Str, offset : U64 })])
	parse_bytes_partial = |parser, input| {
		parser.run(input)
	}

	## Match one UTF-8 code unit when it satisfies the given predicate.
	##
	## Fails on empty input or when the predicate returns `False`.
	##
	## ```roc
	## is_digit : U8 -> Bool
	## is_digit = |b| b >= '0' and b <= '9'
	##
	## expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "0") == Ok('0')
	## expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "*").is_err()
	## ```
	codeunit_satisfies : (U8 -> Bool) -> Parser(Bytes, U8)
	codeunit_satisfies = |check| {
		Parser.custom(
			|input| {
				{ before: start, others: input_rest } = input.split_at(1)

				match start.get(0) {
					Err(OutOfBounds) =>
						Err(ParseError({ message: "expected a codeunit satisfying a condition, but input was empty.", offset: 0 }))

					Ok(start_codeunit) => {
						if check(start_codeunit) {
							Ok({ value: start_codeunit, rest: input_rest })
						} else {
							other_char = str_from_codeunit(start_codeunit)
							input_str = excerpt(input)

							Err(ParseError({ message: "expected a codeunit satisfying a condition but found `${other_char}`.\n While reading: `${input_str}`", offset: 0 }))
						}
					}
				}
			},
		)
	}

	## Match one exact UTF-8 code unit.
	##
	## For a character outside ASCII, which is several code units, use `string`.
	##
	## ```roc
	## at_sign : Parser(Utf8.Bytes, [AtSign])
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str(at_sign, "@") == Ok(AtSign)
	## expect Utf8.parse_str_partial(at_sign, "$").is_err()
	## ```
	codeunit : U8 -> Parser(Bytes, U8)
	codeunit = |expected_code_unit| {
		Parser.custom(
			|input| {
				match input {
					[] =>
						Err(ParseError({ message: "expected char `${str_from_codeunit(expected_code_unit)}` but input was empty.", offset: 0 }))

					[first, .. as others] if first == expected_code_unit =>
						Ok({ value: expected_code_unit, rest: others })

					[first, ..] =>
						Err(ParseError({ message: "expected char `${str_from_codeunit(expected_code_unit)}` but found `${str_from_codeunit(first)}`.\n While reading: `${excerpt(input)}`", offset: 0 }))
				}
			},
		)
	}

	## Match an exact sequence of UTF-8 bytes and return them.
	utf8 : List(U8) -> Parser(Bytes, List(U8))
	utf8 = |expected_string| {
		# Implemented manually instead of a sequence of codeunits
		# because of efficiency and better error messages
		Parser.custom(
			|input| {
				{ before: start, others: input_rest } = input.split_at(expected_string.len())

				if start == expected_string {
					Ok({ value: expected_string, rest: input_rest })
				} else {
					error_string = str_from_utf8(expected_string)
					other_string = str_from_utf8(start)
					input_string = excerpt(input)

					Err(ParseError({ message: "expected string `${error_string}` but found `${other_string}`.\nWhile reading: ${input_string}", offset: 0 }))
				}
			},
		)
	}

	## Match the given `Str` exactly (case-sensitive) and return it.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.string("Foo"), "Foo") == Ok("Foo")
	## expect Utf8.parse_str(Utf8.string("Foo"), "Bar").is_err()
	## ```
	string : Str -> Parser(Bytes, Str)
	string = |expected_string| {
		utf8(expected_string.to_utf8())
			.map(
				|_val| {
					expected_string
				},
			)
	}

	## Match any single `U8` code unit; fails only on empty input.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.any_codeunit, "a") == Ok('a')
	## expect Utf8.parse_str(Utf8.any_codeunit, "$") == Ok('$')
	## ```
	any_codeunit : Parser(Bytes, U8)
	any_codeunit = codeunit_satisfies(|_| True)

	## Consume the rest of the input and return it as bytes; never fails.
	##
	## ```roc
	## expect {
	##     bytes = "consumes all the input".to_utf8()
	##     Utf8.rest.parse(bytes) == Ok(bytes)
	## }
	## ```
	rest : Parser(Bytes, Bytes)
	rest = Parser.custom(
		|input| {
			Ok({ value: input, rest: [] })
		},
	)

	## Consume the rest of the input as a `Str`, failing if the bytes are not valid UTF-8.
	rest_str : Parser(Bytes, Str)
	rest_str = Parser.custom(
		|field_utf8ing| {
			match Str.from_utf8(field_utf8ing) {
				Ok(string_val) =>
					Ok({ value: string_val, rest: [] })

				Err(BadUtf8(_)) =>
					Err(ParseError({ message: "Expected a string field, but its contents cannot be parsed as UTF8.", offset: 0 }))
			}
		},
	)

	## Parse one ASCII decimal digit into a `U64` from 0 through 9.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.digit, "0") == Ok(0)
	## expect Utf8.parse_str(Utf8.digit, "not a digit").is_err()
	## ```
	digit : Parser(Bytes, U64)
	digit =
		Parser.custom(
			|input| {
				match input {
					[] =>
						Err(ParseError({ message: "Expected a digit from 0-9 but input was empty.", offset: 0 }))

					[first, .. as others] if first >= '0' and first <= '9' =>
						Ok({ value: (first - '0').to_u64(), rest: others })

					_ =>
						Err(ParseError({ message: "Not a digit", offset: 0 }))
				}
			},
		)

	## Parse one or more ASCII decimal digits into a `U64`, accepting leading zeroes.
	##
	## Fails when the input does not start with a digit or the value does not fit
	## in a `U64`. Signs and decimal points are not accepted.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.digits, "0123") == Ok(123)
	## expect Utf8.parse_str(Utf8.digits, "not a digit").is_err()
	## ```
	digits : Parser(Bytes, U64)
	digits =
		Parser.one_or_more(digit)
			.map(
				|ds| {
					ds.fold(
						Ok(0),
						|result, d| {
							match result {
								Err(problem) => Err(problem)
								Ok(sum) =>
									if sum > (18446744073709551615 - d) / 10 {
										Err("Integer is too large for U64")
									} else {
										Ok(sum * 10 + d)
									}
							}
						},
					)
				},
			)
			.flatten()
}

str_from_codeunit : U8 -> Str
str_from_codeunit = |cu| {
	str_from_utf8([cu])
}

## Bytes of remaining input quoted in failure messages.
excerpt_len : U64
excerpt_len = 32

## Render the start of the remaining input for a failure message.
##
## Failures are cheap and frequent (every losing branch of `alt`/`one_of`, the
## last iteration of `many`), so quoting the whole remaining input made those
## combinators quadratic in the input length. Quote a bounded prefix instead.
excerpt : Utf8.Bytes -> Str
excerpt = |bytes| {
	if bytes.len() <= excerpt_len {
		str_from_utf8(bytes)
	} else {
		Str.concat(str_from_utf8(bytes.sublist({ start: 0, len: excerpt_len })), "…")
	}
}

# Failure messages quote only a bounded prefix of the remaining input.
expect {
	long = List.repeat('b', 1000)
	match Parser.run(Utf8.codeunit('a'), long) {
		Err(ParseError({ message, offset: _ })) => message.count_utf8_bytes() < 200
		Ok(_) => False
	}
}

str_from_utf8 : Utf8.Bytes -> Str
str_from_utf8 = |bytes| {
	# Byte-oriented parsers can stop within a multibyte scalar, so diagnostics
	# and Str leftovers render bytes lossily rather than crashing.
	Str.from_utf8_lossy(bytes)
}

# Any codeunit parser accepts a lowercase ASCII byte.
expect {
	actual = Utf8.parse_str(Utf8.any_codeunit, "a")?
	actual == 'a'
}

# Any codeunit parser accepts a dollar-sign byte.
expect {
	actual = Utf8.parse_str(Utf8.any_codeunit, "\$")?
	actual == 36
}

# Any input parser consumes all bytes and returns them.
expect {
	bytes = "consumes all the input".to_utf8()
	actual = Parser.parse(Utf8.rest, bytes)?
	actual == bytes
}

# -------------------- example snippets used in docs --------------------

parse_u32 : Parser(Utf8.Bytes, U32)
parse_u32 =
	Parser.const(U64.to_u32_wrap).keep(Utf8.digits)

# Digit parsing can be mapped into a U32.
expect {
	actual = Utf8.parse_str(parse_u32, "123")?
	actual == 123.U32
}

color : Parser(Utf8.Bytes, [Red, Green, Blue])
color =
	Parser.one_of([
		Parser.const(Red).skip(Utf8.string("red")),
		Parser.const(Green).skip(Utf8.string("green")),
		Parser.const(Blue).skip(Utf8.string("blue")),
	])

# One-of parsing selects the matching color tag.
expect {
	actual = Utf8.parse_str(color, "green")?
	actual == Green
}

parse_numbers : Parser(Utf8.Bytes, List(U64))
parse_numbers = (Utf8.digits).sep_by(Utf8.codeunit(','))

# Separator parsing returns the list of parsed numbers.
expect {
	actual = Utf8.parse_str(parse_numbers, "1,2,3")?
	actual == [1, 2, 3]
}

# Exact string parsing succeeds when the input matches.
expect {
	actual = Utf8.parse_str(Utf8.string("Foo"), "Foo")?
	actual == "Foo"
}

# Exact string parsing reports non-matching input.
expect Utf8.parse_str(Utf8.string("Foo"), "Bar").is_err()

ignore_text : Parser(Utf8.Bytes, U64)
ignore_text =
	Parser.const(
		|d| {
			d
		},
	)
		.skip(Parser.chomp_until(':'))
		.skip(Utf8.codeunit(':'))
		.keep(Utf8.digits)

# Skipping a prefix can parse the numeric suffix.
expect {
	actual = Utf8.parse_str(ignore_text, "ignore preceding text:123")?
	actual == 123
}

ignore_numbers : Parser(Utf8.Bytes, Str)
ignore_numbers =
	Parser.const(
		|str| {
			str
		},
	)
		.skip(
			Parser.chomp_while(
				|b| {
					b >= '0' and b <= '9'
				},
			),
		)
		.keep(Utf8.string("TEXT"))

# Chomping digits can leave the following text parser result.
expect {
	actual = Utf8.parse_str(ignore_numbers, "0123456789876543210TEXT")?
	actual == "TEXT"
}

is_digit : U8 -> Bool
is_digit = |b| {
	b >= '0' and b <= '9'
}

# Codeunit predicates can accept digit bytes.
expect {
	actual = Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "0")?
	actual == '0'
}

# Codeunit predicates reject bytes that do not satisfy the predicate.
expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "*").is_err()

at_sign : Parser(Utf8.Bytes, [AtSign])
at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))

# The at-sign parser succeeds on an at-sign byte.
expect {
	actual = Utf8.parse_str(at_sign, "@")?
	actual == AtSign
}

# Partial parsing returns the parsed at-sign tag.
expect {
	actual = Utf8.parse_str_partial(at_sign, "@")?
	actual.value == AtSign
}

# The at-sign parser rejects other bytes.
expect Utf8.parse_str_partial(at_sign, "\$").is_err()

# Partial string parsing renders a leftover that begins within a UTF-8 scalar.
expect {
	actual = Utf8.parse_str_partial(Utf8.any_codeunit, "ӿ")?
	actual.rest == "�"
}

# Complete string parsing reports a leftover as unexpected input at its byte offset.
expect Utf8.parse_str(Utf8.any_codeunit, "ӿ") == Err(ParseError({ message: "unexpected input", offset: 1 }))

Requirement : [Green(U64), Red(U64), Blue(U64)]

RequirementSet : List(Requirement)

Game : { id : U64, requirements : List(RequirementSet) }

parse_game : Str -> Try(Game, [ParsingError])
parse_game = |s| {
	green = Parser.const(|x| Green(x)).keep(Utf8.digits).skip(Utf8.string(" green"))
	red = Parser.const(|x| Red(x)).keep(Utf8.digits).skip(Utf8.string(" red"))
	blue = Parser.const(|x| Blue(x)).keep(Utf8.digits).skip(Utf8.string(" blue"))

	requirement_set : Parser(_, RequirementSet)
	requirement_set = Parser.one_of([green, red, blue]).sep_by(Utf8.string(", "))

	requirements : Parser(_, List(RequirementSet))
	requirements = requirement_set.sep_by(Utf8.string("; "))

	game : Parser(_, Game)
	game =
		Parser.const(
			|id| {
				|r| {
					{ id, requirements: r }
				}
			},
		)
			.skip(Utf8.string("Game "))
			.keep(Utf8.digits)
			.skip(Utf8.string(": "))
			.keep(requirements)

	match Utf8.parse_str(game, s) {
		Ok(g) => Ok(g)
		Err(ParseError(_)) => Err(ParsingError)
	}
}

# Game parsing extracts requirements grouped by reveal.
expect {
	actual = parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")?
	actual
		== {
			id: 1,
			requirements: [
				[Blue(3), Red(4)],
				[Red(1), Green(2), Blue(6)],
				[Green(2)],
			],
		}
}

# Single digit parsing converts ASCII zero into numeric zero.
expect {
	actual = Utf8.parse_str(Utf8.digit, "0")?
	actual == 0
}

# Single digit parsing rejects non-digit text.
expect Utf8.parse_str(Utf8.digit, "not a digit").is_err()

# Multiple digit parsing accepts leading zeroes.
expect {
	actual = Utf8.parse_str(Utf8.digits, "0123")?
	actual == 123
}

# Multiple digit parsing accepts the largest U64.
expect {
	actual = Utf8.parse_str(Utf8.digits, "18446744073709551615")?
	actual == 18446744073709551615
}

# Multiple digit parsing rejects values larger than U64 without crashing.
expect Utf8.parse_str(Utf8.digits, "18446744073709551616").is_err()

# Multiple digit parsing rejects text without a leading digit.
expect Utf8.parse_str(Utf8.digits, "not a digit").is_err()

bool_parser : Parser(Utf8.Bytes, Bool)
bool_parser =
	Parser.one_of([Utf8.string("true"), Utf8.string("false")])
		.map(
			|x| {
				x == "true"
			},
		)

# Boolean parser maps true text to True.
expect {
	actual = Utf8.parse_str(bool_parser, "true")?
	actual == True
}

# Boolean parser maps false text to False.
expect {
	actual = Utf8.parse_str(bool_parser, "false")?
	actual == False
}

# Boolean parser rejects other text.
expect Utf8.parse_str(bool_parser, "not a bool").is_err()

even : Parser(Utf8.Bytes, U64)
even =
	Utf8.digits
		.map(
			|n| {
				if n % 2 == 0 {
					Ok(n)
				} else {
					Err("odd number")
				}
			},
		)
		.flatten()

# Flattening keeps an Ok value from the mapped function.
expect Utf8.parse_str(even, "42") == Ok(42)

# Flattening turns an Err value into a parse failure with its message.
expect Utf8.parse_str(even, "7") == Err(ParseError({ message: "odd number", offset: 0 }))

# Capturing up to a delimiter returns the bytes before it.
expect {
	capture_text = Parser.const(|codeunits| codeunits).keep(Parser.chomp_until(':')).skip(Utf8.codeunit(':'))
	Utf8.parse_str(capture_text, "Roc:") == Ok(['R', 'o', 'c'])
}

# Capturing while a predicate holds returns the matching bytes.
expect {
	capture_numbers =
		Parser.const(|codeunits| codeunits)
			.keep(Parser.chomp_while(|b| b >= '0' and b <= '9'))
			.skip(Utf8.string("TEXT"))
	Utf8.parse_str(capture_numbers, "123TEXT") == Ok(['1', '2', '3'])
}

# Mapping three parsers matches applying a curried constructor three times.
expect {
	space = Utf8.codeunit(' ')
	mapped = Parser.map3(Utf8.digits.skip(space), Utf8.digits.skip(space), Utf8.digits, |x, y, z| Triple(x, y, z))
	applied =
		Parser.const(|x| |y| |z| Triple(x, y, z))
			.keep(Utf8.digits.skip(space))
			.keep(Utf8.digits.skip(space))
			.keep(Utf8.digits)
	Utf8.parse_str(mapped, "1 2 3") == Ok(Triple(1, 2, 3)) and Utf8.parse_str(applied, "1 2 3") == Ok(Triple(1, 2, 3))
}

# A length-prefixed field reads as many bytes as its digit says.
expect {
	sized : Parser(Utf8.Bytes, List(U8))
	sized = Utf8.digit.and_then(|n| Utf8.any_codeunit.many().map(|bytes| bytes.take_first(n)))
	Utf8.parse_str(sized, "2abc") == Ok(['a', 'b'])
}

# Between keeps only the delimited value.
expect {
	between_brackets = |parser| parser.between(Utf8.codeunit('['), Utf8.codeunit(']'))
	Utf8.parse_str(between_brackets(Utf8.digits), "[42]") == Ok(42)
}

# The rest of the input is read as a Str, and invalid UTF-8 fails without crashing.
expect Utf8.parse_str(Utf8.rest_str, "héllo") == Ok("héllo")
expect Utf8.parse_bytes(Utf8.rest_str, [0xFF]).is_err()

# A bad element in the middle of a repetition reports its own failure and offset.
expect {
	numbers = Utf8.digits.sep_by(Utf8.codeunit(','))
	match Utf8.parse_str(numbers, "1,2,x,4") {
		Err(ParseError({ offset, message: _ })) => offset == 4
		Ok(_) => False
	}
}

# Leftover input reports the failure that stopped the parser there.
expect Utf8.parse_str(Utf8.digits, "12 ") == Err(ParseError({ message: "Not a digit", offset: 2 }))

# ParseError composes with `?` in a function that returns other errors too.
expect {
	both : Str -> Try(U64, [ParseError({ message : Str, offset : U64 }), TooBig])
	both = |text| {
		n = Utf8.parse_str(Utf8.digits, text)?
		if n > 100 Err(TooBig) else Ok(n)
	}
	both("7") == Ok(7) and both("700") == Err(TooBig) and both("x").is_err()
}
