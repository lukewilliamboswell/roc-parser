import Diagnostic as DiagnosticCore
import Streaming

## Generic [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator)
## for transforming input into structured values.
##
## A `Parser(input, a, p)` is a value that describes how to read an `a` from the
## front of an `input`. Combine small parsers with `keep`, `skip`, `map`,
## `one_of`, `many` and `sep_by` to build larger ones, then run the result with
## `Utf8.parse_str` (for `Str`) or [Parser.parse] (for any input type).
##
## This parser turns `"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"`
## into `{ id: 1, requirements: [[Blue(3), Red(4)], [Red(1), Green(2), Blue(6)], [Green(2)]] }`
## (the same code is a test in `Utf8.roc`):
## ```roc
## Requirement : [Green(U64), Red(U64), Blue(U64)]
## RequirementSet : List(Requirement)
## Game : { id : U64, requirements : List(RequirementSet) }
##
## parse_game : Str -> Try(Game, [ParsingError])
## parse_game = |s| {
##     green = Parser.const(|x| Green(x)).keep(Utf8.digits({})).skip(Utf8.string(" green"))
##     red = Parser.const(|x| Red(x)).keep(Utf8.digits({})).skip(Utf8.string(" red"))
##     blue = Parser.const(|x| Blue(x)).keep(Utf8.digits({})).skip(Utf8.string(" blue"))
##
##     requirement_set : Parser(_, RequirementSet, _)
##     requirement_set = Parser.one_of([green, red, blue]).sep_by(Utf8.string(", "))
##
##     requirements : Parser(_, List(RequirementSet), _)
##     requirements = requirement_set.sep_by(Utf8.string("; "))
##
##     game : Parser(_, Game, _)
##     game =
##         Parser.const(|id| |r| { id, requirements: r })
##             .skip(Utf8.string("Game "))
##             .keep(Utf8.digits({}))
##             .skip(Utf8.string(": "))
##             .keep(requirements)
##
##     match Utf8.parse_str(game, s) {
##         Ok(g) => Ok(g)
##         Err(ParseError(_)) => Err(ParsingError)
##     }
## }
## ```
##
## Alternatives backtrack: when one alternative of `alt` or `one_of` fails, the
## next one is tried on the original input, however much the failed one read.
##
## Failures report the furthest position any parser reached. When a
## repetition such as `many` stops at a bad element, or an alternative fails
## after reading further than the one that succeeded, that failure is kept
## and reported if the whole parse later fails at or before it.
##
## The representation is internal and may change to improve efficiency or error problems.
Parser(input, a, p) :: { fun : input -> ParserStep(input, a, p) }.{

	## UTF-8 parser constructors and byte primitives.
	Utf8 :: [].{

		## UTF-8 input represented as a list of bytes.
		Bytes : List(U8)

		## Byte-parser failures extended by the surrounding grammar's own tags.
		Problem(p) : [UnexpectedEnd, ExpectedPredicate, ExpectedByte(U8), ExpectedBytes(List(U8)), InvalidUtf8, ExpectedDigit, IntegerOverflow(U64), ..p]

		## Match one UTF-8 code unit when it satisfies the given predicate.
		##
		## Fails on empty input or when the predicate returns `False`.
		##
		## ```roc
		## is_digit : U8 -> Bool
		## is_digit = |b| b >= '0' and b <= '9'
		##
		## expect (Parser.Utf8.codeunit_satisfies(is_digit)).parse_str("0") == Ok('0')
		## expect (Parser.Utf8.codeunit_satisfies(is_digit)).parse_str("*").is_err()
		## ```
		codeunit_satisfies : (U8 -> Bool) -> Parser(Bytes, U8, Problem(p))
		codeunit_satisfies = |check| {
			Parser.custom(
				|input| {
					{ before: start, others: input_rest } = input.split_at(1)

					match start.get(0) {
						Err(OutOfBounds) =>
							Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedEnd, location: InputOffset(0), path: [] }))

						Ok(start_codeunit) => {
							if check(start_codeunit) {
								Ok({ value: start_codeunit, rest: input_rest })
							} else {
								Err(ParseError(Parser.Diagnostic.{ problem: ExpectedPredicate, location: InputOffset(0), path: [] }))
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
		## at_sign : Parser(Parser.Utf8.Bytes, [AtSign], _)
		## at_sign = Parser.const(AtSign).skip(Parser.Utf8.codeunit('@'))
		##
		## expect (at_sign).parse_str("@") == Ok(AtSign)
		## expect (at_sign).run_str("$").is_err()
		## ```
		codeunit : U8 -> Parser(Bytes, U8, Problem(p))
		codeunit = |expected_code_unit| {
			Parser.custom(
				|input| {
					match input {
						[] =>
							Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedEnd, location: InputOffset(0), path: [] }))

						[first, .. as others] if first == expected_code_unit =>
							Ok({ value: expected_code_unit, rest: others })

						_ =>
							Err(ParseError(Parser.Diagnostic.{ problem: ExpectedByte(expected_code_unit), location: InputOffset(0), path: [] }))
					}
				},
			)
		}

		## Match an exact sequence of UTF-8 bytes and return them.
		utf8 : List(U8) -> Parser(Bytes, List(U8), Problem(p))
		utf8 = |expected_string| {
			# Implemented manually instead of a sequence of codeunits
			# to compare the entire expected byte sequence at once
			Parser.custom(
				|input| {
					{ before: start, others: input_rest } = input.split_at(expected_string.len())

					if start == expected_string {
						Ok({ value: expected_string, rest: input_rest })
					} else {
						Err(ParseError(Parser.Diagnostic.{ problem: ExpectedBytes(expected_string), location: InputOffset(0), path: [] }))
					}
				},
			)
		}

		## Match the given `Str` exactly (case-sensitive) and return it.
		##
		## ```roc
		## expect (Parser.Utf8.string("Foo")).parse_str("Foo") == Ok("Foo")
		## expect (Parser.Utf8.string("Foo")).parse_str("Bar").is_err()
		## ```
		string : Str -> Parser(Bytes, Str, Problem(p))
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
		## expect (Parser.Utf8.any_codeunit({})).parse_str("a") == Ok('a')
		## expect (Parser.Utf8.any_codeunit({})).parse_str("$") == Ok('$')
		## ```
		any_codeunit : {} -> Parser(Bytes, U8, Problem(p))
		any_codeunit = |{}| codeunit_satisfies(|_| True)

		## Consume the rest of the input and return it as bytes; never fails.
		##
		## ```roc
		## expect {
		##     bytes = "consumes all the input".to_utf8()
		##     Parser.Utf8.rest({}).parse(bytes) == Ok(bytes)
		## }
		## ```
		rest : {} -> Parser(Bytes, Bytes, Problem(p))
		rest = |{}| Parser.custom(
			|input| {
				Ok({ value: input, rest: [] })
			},
		)

		## Consume the rest of the input as a `Str`, failing if the bytes are not valid UTF-8.
		rest_str : {} -> Parser(Bytes, Str, Problem(p))
		rest_str = |{}| Parser.custom(
			|field_utf8ing| {
				match Str.from_utf8(field_utf8ing) {
					Ok(string_val) =>
						Ok({ value: string_val, rest: [] })

					Err(BadUtf8(error)) =>
						Err(ParseError(Parser.Diagnostic.{ problem: InvalidUtf8, location: InputOffset(error.index), path: [] }))
				}
			},
		)

		## Parse one ASCII decimal digit into a `U64` from 0 through 9.
		##
		## ```roc
		## expect (Parser.Utf8.digit({})).parse_str("0") == Ok(0)
		## expect (Parser.Utf8.digit({})).parse_str("not a digit").is_err()
		## ```
		digit : {} -> Parser(Bytes, U64, Problem(p))
		digit = |{}|
			Parser.custom(
				|input| {
					match input {
						[] =>
							Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedEnd, location: InputOffset(0), path: [] }))

						[first, .. as others] if first >= '0' and first <= '9' =>
							Ok({ value: (first - '0').to_u64(), rest: others })

						_ =>
							Err(ParseError(Parser.Diagnostic.{ problem: ExpectedDigit, location: InputOffset(0), path: [] }))
					}
				},
			)

		## Parse one or more ASCII decimal digits into a `U64`, accepting leading zeroes.
		##
		## Fails when the input does not start with a digit or the value does not fit
		## in a `U64`. Signs and decimal points are not accepted.
		##
		## ```roc
		## expect (Parser.Utf8.digits({})).parse_str("0123") == Ok(123)
		## expect (Parser.Utf8.digits({})).parse_str("not a digit").is_err()
		## ```
		digits : {} -> Parser(Bytes, U64, Problem(p))
		digits = |{}| Parser.custom(
			|input| {
				var $i = 0
				var $sum = 0
				while $i < input.len() {
					d = input.get($i) ?? 0
					if d < '0' or d > '9' {
						break
					}
					value = (d - '0').to_u64()
					if $sum > (18446744073709551615 - value) / 10 {
						return Err(ParseError(Parser.Diagnostic.{ problem: IntegerOverflow(U64.highest), location: InputOffset(0), path: [] }))
					}
					$sum = $sum * 10 + value
					$i = $i + 1
				}
				if $i == 0 {
					if input.is_empty() {
						Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedEnd, location: InputOffset(0), path: [] }))
					} else {
						Err(ParseError(Parser.Diagnostic.{ problem: ExpectedDigit, location: InputOffset(0), path: [] }))
					}
				} else {
					Ok({ value: $sum, rest: input.drop_first($i) })
				}
			},
		)

		## A set of byte values, for finding or skipping runs of bytes 16 at a time.
		##
		## Build a class once, outside any loop, and reuse it: construction derives
		## the lookup tables the vector scan uses. Any set of bytes works. A set
		## whose high-nibble rows have at most eight distinct shapes (every ASCII
		## class, and most practical ones) is scanned with two table lookups per 16
		## bytes; other sets fall back to a byte loop.
		##
		## ```roc
		## spaces = Parser.Utf8.ByteClass.from_bytes([' ', '\t'])
		## expect spaces.contains('\t')
		## expect !spaces.contains('x')
		## ```
		ByteClass :: { lo : U8x16, hi : U8x16, a : U8x16, b : U8x16, c : U8x16, members : List(Bool), kind : [Exact, Nibbles, Bytewise] }.{

			## The class holding exactly these bytes.
			from_bytes : List(U8) -> ByteClass
			from_bytes = |bytes| {
				var $members = List.repeat(False, 256)
				for b in bytes {
					$members = $members.set(b.to_u64(), True) ?? $members
				}
				build_class($members)
			}

			## The class holding every byte `b` for which `check(b)` is true.
			from_predicate : (U8 -> Bool) -> ByteClass
			from_predicate = |check| {
				var $members = List.with_capacity(256)
				var $b = 0.U64
				while $b < 256 {
					$members = $members.append(check($b.to_u8_wrap()))
					$b = $b + 1
				}
				build_class($members)
			}

			## The class holding every byte that is not in this one.
			complement : ByteClass -> ByteClass
			complement = |class| build_class(class.members.map(|m| !m))

			## Whether the byte is in the class.
			contains : ByteClass, U8 -> Bool
			contains = |class, byte| class.members.get(byte.to_u64()) ?? False
		}

		## The index of the first byte at or after `pos` that is in `class`, or
		## the length of `bytes` if there is none. Scans 16 bytes at a time.
		##
		## ```roc
		## markup = Parser.Utf8.ByteClass.from_bytes(['<', '&'])
		## expect Parser.Utf8.find_any("a&b<c".to_utf8(), 0, markup) == 1
		## expect Parser.Utf8.find_any("abc".to_utf8(), 0, markup) == 3
		## ```
		find_any : Bytes, U64, ByteClass -> U64
		find_any = |bytes, pos, class| scan_class(bytes, pos, class, True)

		## The index of the first byte at or after `pos` that is not in `class`,
		## or the length of `bytes` if every remaining byte is. Use it to skip a
		## run of name characters, digits or whitespace.
		##
		## ```roc
		## digits = Parser.Utf8.ByteClass.from_predicate(|b| b >= '0' and b <= '9')
		## expect Parser.Utf8.skip_class("123abc".to_utf8(), 0, digits) == 3
		## ```
		skip_class : Bytes, U64, ByteClass -> U64
		skip_class = |bytes, pos, class| scan_class(bytes, pos, class, False)

		## The index of the next `\n` or `\r` at or after `pos`, or the length of
		## `bytes` if there is none. Scans 16 bytes at a time.
		##
		## ```roc
		## expect Parser.Utf8.find_line_end("ab\r\ncd".to_utf8(), 0) == 2
		## ```
		find_line_end : Bytes, U64 -> U64
		find_line_end = |bytes, pos| {
			cr = U8x16.splat('\r')
			lf = U8x16.splat('\n')
			len = bytes.len()
			var $i = pos
			var $scanning = True
			while $scanning {
				match U8x16.load(bytes, $i) {
					Ok(v) => {
						mask = U8x16.to_bitmask(U8x16.bitwise_or(U8x16.eq_lanes(v, cr), U8x16.eq_lanes(v, lf)))
						if mask != 0 {
							$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
							$scanning = False
						} else {
							$i = $i + 16
						}
					}
					Err(_) => {
						$scanning = False
						while $i < len {
							c = bytes.get($i) ?? 0
							if c == '\n' or c == '\r' {
								break
							}
							$i = $i + 1
						}
					}
				}
			}
			$i
		}

		## Consume the longest run of bytes in `class`, possibly empty, and return
		## it as a slice of the input. It scans 16 bytes at a time, so prefer it to
		## `Parser.chomp_while` for runs longer than a few bytes.
		##
		## ```roc
		## name_char = Parser.Utf8.ByteClass.from_predicate(|b| (b >= 'a' and b <= 'z') or b == '-')
		## expect (Parser.Utf8.span_class(name_char)).run_str("foo-bar=1").map_ok(|r| r.rest) == Ok("=1")
		## ```
		span_class : ByteClass -> Parser(Bytes, Bytes, Problem(p))
		span_class = |class| Parser.custom(
			|input| {
				n = scan_class(input, 0, class, False)
				Ok({ value: input.take_first(n), rest: input.drop_first(n) })
			},
		)
	}

	## Compile-time quotation diagnostics and checked constant conversion.
	Quotation :: [].{

		## Unwrap a checked conversion, or fail with its explanation.
		## A demanded top-level call runs during compilation.
		require : Try(a, error) -> a
		require = |result| match result {
			Ok(value) => value
			Err(failure) => crash Str.inspect(failure)
		}

		## The available location and explanation of a checked conversion failure.
		## Zero line or column means that coordinate is unavailable.
		ErrorProps := {
			name : Str ?? "<literal>",
			format : Str,
			message : Str,
			line : U64 ?? 0,
			column : U64 ?? 0,
		}

		## Build a simple compiler quotation error without inventing coordinates.
		error : ErrorProps -> [BadQuotedBytes(Str)]
		error = |props| {
			location = if props.line == 0 {
				props.name
			} else if props.column == 0 {
				"${props.name}:${props.line.to_str()}"
			} else {
				"${props.name}:${props.line.to_str()}:${props.column.to_str()}"
			}
			BadQuotedBytes("${location}: ${props.format}: ${props.message}")
		}

		## Convert a UTF-8 byte offset to a one-based line and byte column.
		## CRLF, CR, and LF each end one line; offsets past the end are clamped.
		position : Str, U64 -> { line : U64, column : U64 }
		position = |source, offset| {
			bytes = source.to_utf8()
			end = if (offset < bytes.len()) offset else bytes.len()
			var $index = 0
			var $line = 1
			var $column = 1
			while $index < end {
				byte = bytes.get($index) ?? 0
				if byte == '\r' {
					$line = $line + 1
					$column = 1
					if $index + 1 < end and bytes.get($index + 1) == Ok('\n') {
						$index = $index + 1
					}
				} else if byte == '\n' {
					$line = $line + 1
					$column = 1
				} else {
					$column = $column + 1
				}
				$index = $index + 1
			}
			{ line: $line, column: $column }
		}

		## Props for adapting a complete text parser to `from_quote`.
		CheckProps(a, p) := {
			parser : Parser(List(U8), a, p),
			source : Str,
			name : Str ?? "<literal>",
		}

		## Parse all text and convert its furthest error into a quotation error.
		## Use this inside a custom nominal type's `from_quote` method.
		check : CheckProps(a, p) -> Try(a, [BadQuotedBytes(Str)])
		check = |props| {
			match props.parser.parse_str(props.source) {
				Ok(value) => Ok(value)
				Err(ParseError(problem)) => {
					at = DiagnosticCore.line_column(problem.location) ?? { line: 0, column: 0 }
					Err(Parser.Quotation.error({ name: props.name, format: "parser", message: Str.inspect(problem.problem), line: at.line, column: at.column }))
				}
			}
		}

	}

	## Structured failures shared by every format.
	Diagnostic(problem) : DiagnosticCore(problem)
	Location : DiagnosticCore.Location
	PathSegment : DiagnosticCore.PathSegment
	UnavailableReason : DiagnosticCore.UnavailableReason
	Observed : DiagnosticCore.Observed
	RecoveryProps : Streaming.RecoveryProps
	InputEnd : Streaming.InputEnd
	Step(state, item, problem) : Streaming.Step(state, item, problem)
	Decoder(state, item, problem) : Streaming.Decoder(state, item, problem)
	Encoder(state, item, problem) : Streaming.Encoder(state, item, problem)
	ReadEvent(item, problem) : Streaming.ReadEvent(item, problem)
	ReadError(problem) : Streaming.ReadError(problem)
	Report : Streaming.Report
	TextEncoding : Streaming.TextEncoding
	EncodingError : Streaming.EncodingError
	DecoderProps(state, item, problem) : Streaming.DecoderProps(state, item, problem)
	EncoderProps(state, item, problem) : Streaming.EncoderProps(state, item, problem)

	## Configure a custom incremental reader using named state, step and recovery props.
	decoder : DecoderProps(state, item, problem) -> Decoder(state, item, problem)
	decoder = Streaming.decoder

	## Configure a custom incremental writer using named state and callbacks.
	encoder : EncoderProps(state, item, problem) -> Encoder(state, item, problem)
	encoder = Streaming.encoder

	## Parse an entire UTF-8 source, keeping original text coordinates on failure.
	parse_str : Parser(List(U8), a, p), Str -> Try(a, [ParseError(Diagnostic(Problem(p)))])
	parse_str = |parser, source| parser.parse(source.to_utf8()).map_err(|ParseError(error)| ParseError(text_error(source, error)))

	## Parse a UTF-8 prefix. A split scalar is a typed boundary failure.
	run_str : Parser(List(U8), a, p), Str -> Try({ value : a, rest : Str }, [ParseError(Diagnostic(Problem(p)))])
	run_str = |parser, source| {
		parsed = parser.run(source.to_utf8()).map_err(|ParseError(error)| ParseError(text_error(source, error)))?
		match Str.from_utf8(parsed.rest) {
			Ok(rest) => Ok({ value: parsed.value, rest })
			Err(_) => Err(ParseError(DiagnosticCore.{ problem: Utf8Boundary, location: text_location(source, source.count_utf8_bytes() - parsed.rest.len()), path: [] }))
		}
	}

	## Built-in combinator failures and the caller's domain-specific tags.
	Problem(p) : [UnexpectedInput, NoAlternatives, CharacterNotFound, Utf8Boundary, ..p]

	## The result of running a parser on part of an input: the parsed `value`
	## and the `rest` of the input, or a `ParseError` whose `offset` counts the
	## input elements (bytes, for UTF-8 input) read before the failure.
	ParseResult(input, a, p) : Try({ value : a, rest : input }, [ParseError(Diagnostic(Parser.Problem(p)))])

	## Write a custom parser without using the provided combinators.
	##
	## The function receives the remaining input and returns either the parsed
	## `value` with the `rest` it did not consume, or
	## `Err(ParseError({ problem, offset }))`, where `offset` is how far into
	## the given input the failure is (usually `0`).
	custom : (input -> ParseResult(input, a, p)) -> Parser(input, a, p) where [input.len : input -> U64]
	custom = |fun| {
		{
			fun: |input| {
				# Retain only the size for errors, rather than aliasing input while
				# the custom parser consumes it.
				len = input.len()
				match fun(input) {
					Ok({ value, rest }) => Ok({ value, rest, furthest: no_failure({}) })
					Err(ParseError(error)) => Err({
						problem: error.problem,
						remaining: minus(len, relative_offset(error.location)),
						origin: match error.location {
							InputOffset(_) => Infer
							_ => Explicit(error.location)
						},
						path: error.path,
					})
				}
			},
		}
	}

	## Run a parser on the start of an input, returning the parsed value and
	## the input it did not consume.
	##
	## Most parsers consume part of `input` when they succeed. This allows you to string parsers
	## together that run one after the other. The part of the input that the first
	## parser did not consume, is used by the next parser.
	##
	## On failure, the error is the furthest failure seen, with its `offset`
	## counted from the start of `input`. This is mostly useful when creating
	## your own parsing building blocks.
	run : Parser(input, a, p), input -> ParseResult(input, a, p) where [input.len : input -> U64]
	run = |parser, input| {
		len = input.len()
		match step(parser, input) {
			Ok({ value, rest, furthest: _ }) => Ok({ value, rest })
			Err(failure) => Err(ParseError(failure_diagnostic(len, failure)))
		}
	}

	## Run a parser on the given input, expecting it to consume all of it.
	##
	## The input type only needs a `len` method, which is used both to detect
	## leftover input and to turn the furthest failure into an `offset` counted
	## from the start of `input`. Leftover input is a failure too: if some
	## parser failed at or beyond the leftover, its problem is reported;
	## otherwise the problem is `unexpected input`. For UTF-8 text use
	## `Utf8.parse_str` or `Utf8.parse_bytes`.
	parse : Parser(input, a, p), input -> Try(a, [ParseError(Diagnostic(Parser.Problem(p)))]) where [input.len : input -> U64]
	parse = |parser, input| {
		total = input.len()
		match step(parser, input) {
			Ok({ value, rest, furthest }) => {
				remaining = rest.len()
				if remaining == 0 {
					Ok(value)
				} else if furthest.remaining <= remaining {
					Err(ParseError(failure_diagnostic(total, furthest)))
				} else {
					Err(ParseError(DiagnosticCore.{ problem: UnexpectedInput, location: InputOffset(minus(total, remaining)), path: [] }))
				}
			}
			Err(failure) => Err(ParseError(failure_diagnostic(total, failure)))
		}
	}

	## Parser that can never succeed, regardless of the given input.
	## It will always fail with the given error problem.
	##
	## This is mostly useful as a 'base case' if all other parsers
	## in a `one_of` or `alt` have failed, to provide some more descriptive error problem.
	fail : Parser.Problem(p) -> Parser(input, a, p) where [input.len : input -> U64]
	fail = |problem| {
		{ fun: |input| Err({ problem, remaining: input.len(), origin: Infer, path: [] }) }
	}

	## Parser that will always produce the given `a`, without looking at the actual input.
	##
	## This is the usual start of a pipeline: `const` supplies a (curried)
	## constructor function and each `keep` feeds it one parsed value.
	## ```roc
	## parse_u32 : Parser(Utf8.Bytes, U32, _)
	## parse_u32 = Parser.const(U64.to_u32_wrap).keep(Utf8.digits({}))
	##
	## expect Utf8.parse_str(parse_u32, "123") == Ok(123.U32)
	## ```
	const : a -> Parser(_, a, p)
	const = |value| {
		{ fun: |input| Ok({ value, rest: input, furthest: no_failure({}) }) }
	}

	## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
	##
	## The `second` parser starts from the same input as `first` (backtracking).
	## If both fail, the failure that reached further is reported; when both
	## stopped at the same place, the `second` one is.
	alt : Parser(input, a, p), Parser(input, a, p) -> Parser(input, a, p)
	alt = |first, second| {
		{
			fun: |input| {
				match step(first, input) {
					Ok(ok) => Ok(ok)
					Err(first_failure) => {
						match step(second, input) {
							Ok({ value, rest, furthest }) => Ok({ value, rest, furthest: further(first_failure, furthest) })
							# Keep one problem rather than joining them: failures are
							# frequent, and building text on every losing branch is
							# what made combinator-heavy parsers allocate heavily.
							Err(second_failure) => Err(further(first_failure, second_failure))
						}
					}
				}
			},
		}
	}

	## Try a list of parsers in turn, until one of them succeeds.
	##
	## Each parser starts from the same input. An empty list always fails.
	## For UTF-8 input, [Parser.one_of] behaves the same way.
	## ```roc
	## color : Parser(Utf8.Bytes, [Red, Green, Blue], _)
	## color =
	##     Parser.one_of([
	##         Parser.const(Red).skip(Utf8.string("red")),
	##         Parser.const(Green).skip(Utf8.string("green")),
	##         Parser.const(Blue).skip(Utf8.string("blue")),
	##     ])
	##
	## expect Utf8.parse_str(color, "green") == Ok(Green)
	## ```
	one_of : List(Parser(input, a, p)) -> Parser(input, a, p) where [input.len : input -> U64]
	one_of = |parsers| {
		match parsers {
			[] => fail(NoAlternatives)
			[first, .. as others] => others.fold(first, |earlier, later| alt(earlier, later))
		}
	}

	## Translate structured problems while preserving the furthest failure.
	## Wrap a closed native union in a domain tag when combining formats.
	map_problem : Parser(input, a, p), (Parser.Problem(p) -> Parser.Problem(q)) -> Parser(input, a, q)
	map_problem = |parser, transform| {
		convert = |failure| if (failure.remaining == U64.highest) no_failure({}) else { problem: transform(failure.problem), remaining: failure.remaining, origin: failure.origin, path: failure.path }
		{
			fun: |input| match step(parser, input) {
				Ok({ value, rest, furthest }) => Ok({ value, rest, furthest: convert(furthest) })
				Err(failure) => Err(convert(failure))
			},
		}
	}

	## Transforms the result of parsing into something else,
	## using the given transformation function.
	map : Parser(input, a, p), (a -> b) -> Parser(input, b, p)
	map = |parser, transform| {
		{
			fun: |input| {
				match step(parser, input) {
					Ok({ value, rest, furthest }) => Ok({ value: transform(value), rest, furthest })
					Err(failure) => Err(failure)
				}
			},
		}
	}

	## Transforms the result of parsing into something else,
	## using the given two-parameter transformation function.
	map2 : Parser(input, a, p), Parser(input, b, p), (a, b -> c) -> Parser(input, c, p)
	map2 = |parser_a, parser_b, transform| {
		const(|a| |b| transform(a, b))
			.keep(parser_a)
			.keep(parser_b)
	}

	## Transforms the result of parsing into something else,
	## using the given three-parameter transformation function.
	##
	## If you need transformations with more inputs,
	## take a look at `keep`.
	map3 : Parser(input, a, p), Parser(input, b, p), Parser(input, c, p), (a, b, c -> d) -> Parser(input, d, p)
	map3 = |parser_a, parser_b, parser_c, transform| {
		const(|a| |b| |c| transform(a, b, c))
			.keep(parser_a)
			.keep(parser_b)
			.keep(parser_c)
	}

	## Removes a layer of `Try` from running the parser.
	##
	## Use this to map functions that return a `Try` over the parser:
	## an `Err(msg)` value becomes a failure with that problem, reported where
	## the inner parser started.
	##
	## ```roc
	## even : Parser(Utf8.Bytes, U64, _)
	## even =
	##     Utf8.digits({})
	##         .map(|n| if n % 2 == 0 { Ok(n) } else { Err(OddNumber) })
	##         .flatten()
	##
	## expect Utf8.parse_str(even, "42") == Ok(42)
	## expect Utf8.parse_str(even, "7") == Err(ParseError(Parser.Diagnostic.{ problem: OddNumber, location: InputOffset(0), path: [] }))
	## ```
	flatten : Parser(input, Try(a, Parser.Problem(p)), p) -> Parser(input, a, p) where [input.len : input -> U64]
	flatten = |parser| {
		{
			fun: |input| {
				match step(parser, input) {
					Err(failure) => Err(failure)
					Ok({ value: Ok(value), rest, furthest }) => Ok({ value, rest, furthest })
					# The value was read but rejected: report that, not a failure
					# hidden inside the parser that read it.
					Ok({ value: Err(problem), rest: _, furthest: _ }) => Err({ problem, remaining: input.len(), origin: Infer, path: [] })
				}
			},
		}
	}

	## Run `first`, then build the next parser from its value and run that.
	##
	## Use this when what comes next depends on what was read, such as a
	## length prefix. When the next parser is fixed, prefer `keep`, `skip`
	## and `map`, which are simpler and let the parser be built once.
	##
	## ```roc
	## sized : Parser(Utf8.Bytes, List(U8), _)
	## sized = Utf8.digit({}).and_then(|n| Utf8.any_codeunit({}).many().map(|bytes| bytes.take_first(n)))
	## ```
	and_then : Parser(input, a, p), (a -> Parser(input, b, p)) -> Parser(input, b, p)
	and_then = |first, build_next| {
		{
			fun: |input| {
				match step(first, input) {
					Err(failure) => Err(failure)
					Ok({ value, rest, furthest }) => {
						match step(build_next(value), rest) {
							Err(failure) => Err(further(furthest, failure))
							Ok(next) => Ok({ value: next.value, rest: next.rest, furthest: further(furthest, next.furthest) })
						}
					}
				}
			},
		}
	}

	## Runs a parser lazily.
	##
	## This is (only) useful when dealing with a recursive structure.
	## For instance, consider a type `Comment : { message : Str, responses : List(Comment) }`.
	## Without `lazy`, you would ask the compiler to build an infinitely deep parser.
	## (Resulting in a compiler error.)
	##
	## A top-level parser that refers to itself has to be a function, called
	## inside `lazy`: `tree = |{}| ...` with `Parser.lazy(|_| tree({}))`. Top-level
	## parsers that refer to each other need one `lazy` somewhere in the cycle,
	## so that each value can be built before the other is used.
	lazy : ({} -> Parser(input, a, p)) -> Parser(input, a, p)
	lazy = |thunk| {
		{ fun: |input| step(thunk({}), input) }
	}

	## Make a parser optional.
	##
	## Returns `Ok(value)` when the given parser succeeds, and `Err(Missing)`
	## without consuming input when it fails, so the result never fails.
	maybe : Parser(input, a, p) -> Parser(input, Try(a, [Missing]), p)
	maybe = |parser| {
		parser
			.map(|value| Ok(value))
			.alt(const(Err(Missing)))
	}

	## A parser which runs the element parser *zero* or more times on the input,
	## returning a list containing all the parsed elements.
	##
	## Repetition stops at the first element that fails, or that succeeds
	## without consuming any input; that element's value is not included.
	## (Without this rule, a parser such as `chomp_while` or `maybe(p)` that can
	## succeed on empty input would repeat forever.) The failure of the element
	## that stopped the repetition is kept, so if the parse later fails there,
	## that failure is what gets reported.
	many : Parser(input, a, p) -> Parser(input, List(a), p) where [input.len : input -> U64]
	many = |parser| {
		{ fun: |input| many_help(parser, [], input, input.len(), no_failure({})) }
	}

	## A parser which runs the element parser *one* or more times on the input,
	## returning a list containing all the parsed elements.
	##
	## Fails when the first element fails. Also see [Parser.many].
	one_or_more : Parser(input, a, p) -> Parser(input, List(a), p) where [input.len : input -> U64]
	one_or_more = |parser| {
		const(|value| |values| values.prepend(value))
			.keep(parser)
			.keep(many(parser))
	}

	## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
	## and only returns the result of your main parser.
	##
	## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
	##
	## ```roc
	## between_brackets = |parser| parser.between(Utf8.codeunit('['), Utf8.codeunit(']'))
	## ```
	between : Parser(input, a, p), Parser(input, open, p), Parser(input, close, p) -> Parser(input, a, p)
	between = |parser, open, close| {
		const(|value| value)
			.skip(open)
			.keep(parser)
			.skip(close)
	}

	## Parse one or more values separated by `separator`.
	## The separators are consumed and omitted from the result.
	##
	## A trailing separator that is not followed by a value is left unconsumed.
	sep_by_one_or_more : Parser(input, a, p), Parser(input, sep, p) -> Parser(input, List(a), p) where [input.len : input -> U64]
	sep_by_one_or_more = |parser, separator| {
		separated = const(|value| value).skip(separator).keep(parser)

		const(|value| |values| values.prepend(value))
			.keep(parser)
			.keep(many(separated))
	}

	## Parse zero or more values separated by `separator`.
	## The separators are consumed and omitted from the result.
	##
	## ```roc
	## parse_numbers : Parser(Utf8.Bytes, List(U64), _)
	## parse_numbers = Utf8.digits({}).sep_by(Utf8.codeunit(','))
	##
	## expect Utf8.parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])
	## ```
	sep_by : Parser(input, a, p), Parser(input, sep, p) -> Parser(input, List(a), p) where [input.len : input -> U64]
	sep_by = |parser, separator| {
		parser
			.sep_by_one_or_more(separator)
			.alt(const([]))
	}

	## Discard a parser's value while preserving how much input it consumes.
	##
	## Useful with `many` to skip a repeated token without collecting values.
	ignore : Parser(input, a, p) -> Parser(input, {}, p)
	ignore = |parser| {
		parser.map(|_| {})
	}

	## Run a parser producing a function, then a parser producing its argument,
	## and return the function result.
	##
	## Start with `const(|a| |b| ...)` and add one `keep` per argument, as in
	## the module example. The function must be curried (`|a| |b| ...`, not
	## `|a, b| ...`), because its arguments are applied one at a time.
	keep : Parser(input, (a -> b), p), Parser(input, a, p) -> Parser(input, b, p)
	keep = |fun_parser, val_parser| {
		{
			fun: |input| {
				match step(fun_parser, input) {
					Err(failure) => Err(failure)
					Ok({ value: fun_value, rest, furthest }) => {
						match step(val_parser, rest) {
							Err(failure) => Err(further(furthest, failure))
							Ok(next) => Ok({ value: fun_value(next.value), rest: next.rest, furthest: further(furthest, next.furthest) })
						}
					}
				}
			},
		}
	}

	## Run two parsers in sequence, discarding the second parser's value.
	##
	## Both parsers must succeed, and the input read by the second is consumed.
	## ```roc
	## at_sign : Parser(Utf8.Bytes, [AtSign], _)
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str(at_sign, "@") == Ok(AtSign)
	## ```
	skip : Parser(input, a, p), Parser(input, _, p) -> Parser(input, a, p)
	skip = |fun_parser, skip_parser| {
		{
			fun: |input| {
				match step(fun_parser, input) {
					Err(failure) => Err(failure)
					Ok({ value, rest, furthest }) => {
						match step(skip_parser, rest) {
							Err(failure) => Err(further(furthest, failure))
							Ok(next) => Ok({ value, rest: next.rest, furthest: further(furthest, next.furthest) })
						}
					}
				}
			},
		}
	}

	## Match zero or more codeunits until it reaches the given codeunit.
	## The given codeunit is not included in the match and is not consumed.
	## Fails if the codeunit never appears in the remaining input.
	##
	## This can be used with [Parser.skip] to ignore text.
	##
	## ```roc
	## ignore_text : Parser(Utf8.Bytes, U64, _)
	## ignore_text =
	##     Parser.const(|d| d)
	##         .skip(Parser.chomp_until(':'))
	##         .skip(Utf8.codeunit(':'))
	##         .keep(Utf8.digits({}))
	##
	## expect Utf8.parse_str(ignore_text, "ignore preceding text:123") == Ok(123)
	## ```
	##
	## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
	##
	## ```roc
	## capture_text : Parser(Utf8.Bytes, List(U8), _)
	## capture_text =
	##     Parser.const(|codeunits| codeunits)
	##         .keep(Parser.chomp_until(':'))
	##         .skip(Utf8.codeunit(':'))
	##
	## expect Utf8.parse_str(capture_text, "Roc:") == Ok(['R', 'o', 'c'])
	## ```
	##
	## Use [Str.from_utf8_lossy] to turn the results into a `Str`.
	##
	## Also see [Parser.chomp_while].
	chomp_until : a -> Parser(List(a), List(a), p) where [a.is_eq : a, a -> Bool]
	chomp_until = |char| {
		{
			fun: |input| {
				match input.find_first_index(|x| x == char) {
					Ok(index) => Ok({ value: input.sublist({ start: 0, len: index }), rest: input.drop_first(index), furthest: no_failure({}) })
					Err(_) => Err({ problem: CharacterNotFound, remaining: input.len(), origin: Infer, path: [] })
				}
			},
		}
	}

	## Match zero or more codeunits until the check returns false.
	## The codeunit that returned false is not included in the match.
	## Note: a `chomp_while` parser always succeeds, possibly consuming nothing.
	##
	## This can be used with [Parser.skip] to ignore text.
	## This is useful for chomping whitespace or variable names.
	##
	## ```roc
	## ignore_numbers : Parser(Utf8.Bytes, Str, _)
	## ignore_numbers =
	##     Parser.const(|str| str)
	##         .skip(Parser.chomp_while(|b| b >= '0' and b <= '9'))
	##         .keep(Utf8.string("TEXT"))
	##
	## expect Utf8.parse_str(ignore_numbers, "0123456789876543210TEXT") == Ok("TEXT")
	## ```
	##
	## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
	##
	## ```roc
	## capture_numbers : Parser(Utf8.Bytes, List(U8), _)
	## capture_numbers =
	##     Parser.const(|codeunits| codeunits)
	##         .keep(Parser.chomp_while(|b| b >= '0' and b <= '9'))
	##         .skip(Utf8.string("TEXT"))
	##
	## expect Utf8.parse_str(capture_numbers, "123TEXT") == Ok(['1', '2', '3'])
	## ```
	##
	## Use [Str.from_utf8_lossy] to turn the results into a `Str`.
	##
	## Also see [Parser.chomp_until].
	chomp_while : (a -> Bool) -> Parser(List(a), List(a), p)
	chomp_while = |check| {
		{
			fun: |input| {
				index = input.fold_until(
					0,
					|i, elem| {
						if check(elem) {
							Continue((i + 1))
						} else {
							Break(i)
						}
					},
				)

				if index == 0 {
					Ok({ value: [], rest: input, furthest: no_failure({}) })
				} else {
					Ok({ value: input.sublist({ start: 0, len: index }), rest: input.drop_first(index), furthest: no_failure({}) })
				}
			},
		}
	}

	## Run a parser and return the input it consumed, instead of its value.
	##
	## The result is a slice of the input, not a copy, so recognising a token
	## with small parsers and keeping its text costs no allocation.
	##
	## ```roc
	## identifier = Parser.span(Utf8.codeunit('$').skip(Parser.chomp_while(|b| b >= 'a' and b <= 'z')))
	## expect Utf8.parse_str(identifier, "$abc") == Ok("$abc".to_utf8())
	## ```
	span : Parser(List(item), a, p) -> Parser(List(item), List(item), p)
	span = |parser| {
		{
			fun: |input| {
				match step(parser, input) {
					Ok({ value: _, rest, furthest }) => Ok({ value: input.sublist({ start: 0, len: input.len() - rest.len() }), rest, furthest })
					Err(failure) => Err(failure)
				}
			},
		}
	}
}

relative_offset : DiagnosticCore.Location -> U64
relative_offset = |location| match location {
	InputOffset(offset) | ByteOffset(offset) => offset
	Text(at) => at.offset
	_ => 0
}

failure_diagnostic : U64, Failure(p) -> DiagnosticCore(Parser.Problem(p))
failure_diagnostic = |total, failure| DiagnosticCore.{
	problem: failure.problem,
	location: match failure.origin {
		Infer => InputOffset(minus(total, failure.remaining))
		Explicit(location) => location
	},
	path: failure.path,
}

text_location : Str, U64 -> DiagnosticCore.Location
text_location = |source, offset| {
	at = Parser.Quotation.position(source, offset)
	Text({ offset, line: at.line, column: at.column })
}

text_error : Str, DiagnosticCore(p) -> DiagnosticCore(p)
text_error = |source, error| {
	location = match error.location {
		InputOffset(offset) | ByteOffset(offset) => text_location(source, offset)
		_ => error.location
	}
	{ ..error, location }
}

# A failure, positioned by how much input was left when it happened, so that
# failures can be compared for any input type without keeping the input.
Failure(p) : { problem : Parser.Problem(p), remaining : U64, origin : [Infer, Explicit(DiagnosticCore.Location)], path : List(DiagnosticCore.PathSegment) }

# What a parser function returns internally. A success also carries the
# furthest failure seen while producing it (for example, the element that
# stopped a `many`), so that a later failure at or before that point can
# report it instead.
ParserStep(input, a, p) : Try({ value : a, rest : input, furthest : Failure(p) }, Failure(p))

# The `furthest` of a success that saw no failure. Its `remaining` is larger
# than any real input, so every real failure is further.
no_failure : {} -> Failure(p)
no_failure = |{}| { problem: UnexpectedInput, remaining: U64.highest, origin: Infer, path: [] }

# The failure that reached further into the input; `b` on a tie, since it
# happened later.
further : Failure(p), Failure(p) -> Failure(p)
further = |a, b| {
	if a.remaining < b.remaining {
		a
	} else {
		b
	}
}

minus : U64, U64 -> U64
minus = |a, b| {
	if a > b {
		a - b
	} else {
		0
	}
}

step : Parser(input, a, p), input -> ParserStep(input, a, p)
step = |{ fun }, input| {
	fun(input)
}

# Repeat `parser` from `input`, whose length is `len`, collecting `values`.
# The no-progress check compares lengths, so each iteration is O(1).
# Keep repetition in one loop so element parsers can inline across iterations.
many_help : Parser(input, a, p), List(a), input, U64, Failure(p) -> ParserStep(input, List(a), p) where [input.len : input -> U64]
many_help = |parser, values, input, len, furthest| {
	var $values = values
	var $input = input
	var $len = len
	var $furthest = furthest
	while True {
		match step(parser, $input) {
			Err(failure) =>
				return Ok({ value: $values, rest: $input, furthest: further($furthest, failure) })

			Ok(next) => {
				next_len = next.rest.len()
				if next_len == $len {
					# No progress: repeating would loop forever on the same input.
					return Ok({ value: $values, rest: $input, furthest: further($furthest, next.furthest) })
				}
				if $values.is_empty() {
					# Reserve only after progress. The first element's input use
					# estimates cardinality, with bounded speculative capacity.
					consumed = minus($len, next_len)
					estimate = if (consumed == 0) 1 else $len // consumed
					$values = List.with_capacity(if (estimate > 16) 16 else estimate)
				}
				$values = $values.append(next.value)
				$input = next.rest
				$len = next_len
				$furthest = further($furthest, next.furthest)
			}
		}
	}
}

# Chomping until a newline returns the preceding bytes and leaves the newline.
expect {
	input = "# H\nR".to_utf8()
	result = Parser.run(Parser.chomp_until('\n'), input)?
	result == { value: ['#', ' ', 'H'], rest: ['\n', 'R'] }
}

# Chomping until a missing newline reports a parse error.
expect {
	Parser.run(Parser.chomp_until('\n'), []).is_err()
}

# Custom error offsets remain clamped to the input length.
expect {
	parser : Parser(List(U8), {}, [PastInput])
	parser = Parser.custom(|_| Err(ParseError(Parser.Diagnostic.{ problem: PastInput, location: InputOffset(99), path: [] })))
	Parser.run(parser, ['a', 'b']) == Err(ParseError(Parser.Diagnostic.{ problem: PastInput, location: InputOffset(2), path: [] }))
}

# Chomping while a predicate holds leaves the first non-matching byte.
expect {
	input : List(U8)
	input = ['a', 's', '\n', 'd', 'f']
	not_eol = |x| {
		x != '\n'
	}
	result = Parser.run(Parser.chomp_while(not_eol), input)?
	result == { value: ['a', 's'], rest: ['\n', 'd', 'f'] }
}

# Repeating a parser that succeeds without consuming input terminates.
expect {
	result = Parser.run(Parser.many(Parser.chomp_while(|b| b == 'a')), "aab".to_utf8())?
	result == { value: [['a', 'a']], rest: ['b'] }
}

# A repeated element can inspect further than the bytes it accepts. Keep the
# failure of the stopping element, including after earlier successful items.
expect {
	element = Parser.custom(
		|input| match input {
			['a', .. as rest] => Ok({ value: 'a', rest })
			_ => Err(ParseError(Parser.Diagnostic.{ problem: RepeatedLookahead, location: InputOffset(1), path: [] }))
		},
	)
	Parser.parse(Parser.many(element), "abc".to_utf8()) == Err(ParseError(Parser.Diagnostic.{ problem: RepeatedLookahead, location: InputOffset(2), path: [] }))
}

# A successful fallback that consumes nothing still carries the earlier
# alternative's failure, but contributes no value to the repetition.
expect {
	lookahead = Parser.custom(|_| Err(ParseError(Parser.Diagnostic.{ problem: FallbackLookahead, location: InputOffset(1), path: [] })))
	element = lookahead.alt(Parser.const('x'))
	Parser.parse(Parser.many(element), "ab".to_utf8()) == Err(ParseError(Parser.Diagnostic.{ problem: FallbackLookahead, location: InputOffset(1), path: [] }))
}

# Separated repetition stops when separator and element consume nothing.
expect {
	empty = Parser.chomp_while(|b| b == 'z')
	result = Parser.run(Parser.sep_by(empty, empty), "x".to_utf8())?
	result == { value: [[]], rest: ['x'] }
}

# Parsing works for any input with a `len` method, not only bytes.
expect {
	words : List(Str)
	words = ["a", "b"]
	word = |w| Parser.custom(
		|input| {
			match input {
				[first, .. as rest] if first == w => Ok({ value: first, rest })
				_ => Err(ParseError(Parser.Diagnostic.{ problem: ExpectedWord(w), location: InputOffset(0), path: [] }))
			}
		},
	)
	Parser.parse(Parser.many(word("a")).skip(word("b")), words) == Ok(["a"])
}

# Leftover input reports the furthest failure, with its offset.
expect {
	words : List(Str)
	words = ["a", "a", "c"]
	word = |w| Parser.custom(
		|input| {
			match input {
				[first, .. as rest] if first == w => Ok({ value: first, rest })
				_ => Err(ParseError(Parser.Diagnostic.{ problem: ExpectedWord(w), location: InputOffset(0), path: [] }))
			}
		},
	)
	Parser.parse(Parser.many(word("a")), words) == Err(ParseError(Parser.Diagnostic.{ problem: ExpectedWord("a"), location: InputOffset(2), path: [] }))
}

# span returns exactly the consumed prefix.
expect {
	input = "ab;cd".to_utf8()
	Parser.run(Parser.span(Parser.chomp_while(|b| b != ';')), input) == Ok({ value: "ab".to_utf8(), rest: ";cd".to_utf8() })
}

# Mapping preserves branch positions, including a repetition's remembered failure.
expect {
	parser = Parser.many(Parser.chomp_until(';')).map_problem(|problem| Token(problem))
	Parser.parse(parser, ['x']) == Err(ParseError(Parser.Diagnostic.{ problem: Token(CharacterNotFound), location: InputOffset(0), path: [] }))
}

# Derive nibble lookup tables for a byte set. Byte `b` is in the set exactly
# when `lo[b & 15] & hi[b >> 4] != 0`. Each distinct non-empty high-nibble row
# (the set of low nibbles present under that high nibble) gets its own bit, so
# the tables are exact when there are at most eight distinct rows; otherwise
# the class scans byte by byte.
build_class : List(Bool) -> Parser.Utf8.ByteClass
build_class = |members| {
	var $shapes = []
	var $hi = []
	var $h = 0.U64
	while $h < 16 {
		var $row = 0.U16
		var $l = 0.U64
		while $l < 16 {
			if members.get($h * 16 + $l) ?? False {
				$row = $row.bitwise_or((1.U16).shl_wrap($l.to_u8_wrap()))
			}
			$l = $l + 1
		}
		row = $row
		if row == 0 {
			$hi = $hi.append(0.U8)
		} else {
			match $shapes.find_first_index(|s| s == row) {
				Ok(k) => {
					$hi = $hi.append((1.U8).shl_wrap(k.to_u8_wrap()))
				}
				Err(NotFound) => {
					k = $shapes.len()
					$shapes = $shapes.append(row)
					$hi = $hi.append(if (k < 8) (1.U8).shl_wrap(k.to_u8_wrap()) else 0)
				}
			}
		}
		$h = $h + 1
	}
	shapes = $shapes
	var $lo = []
	var $l2 = 0.U64
	while $l2 < 16 {
		bit = (1.U16).shl_wrap($l2.to_u8_wrap())
		var $entry = 0.U8
		var $k = 0.U64
		while $k < shapes.len() and $k < 8 {
			if (shapes.get($k) ?? 0).bitwise_and(bit) != 0 {
				$entry = $entry.bitwise_or((1.U8).shl_wrap($k.to_u8_wrap()))
			}
			$k = $k + 1
		}
		$lo = $lo.append($entry)
		$l2 = $l2 + 1
	}
	lo = U8x16.from_list($lo) ?? U8x16.splat(0)
	hi = U8x16.from_list($hi) ?? U8x16.splat(0)
	# Sets of one to three bytes compare against those bytes directly, which
	# is cheaper than two table lookups. Repeat the last byte to fill all three.
	var $exact = []
	var $m = 0.U64
	while $m < 256 {
		if members.get($m) ?? False {
			$exact = $exact.append($m.to_u8_wrap())
		}
		$m = $m + 1
	}
	exact = $exact
	last = exact.last() ?? 0
	kind = if (exact.len() >= 1 and exact.len() <= 3) Exact else if (shapes.len() <= 8) Nibbles else Bytewise
	{
		lo,
		hi,
		a: U8x16.splat(exact.get(0) ?? last),
		b: U8x16.splat(exact.get(1) ?? last),
		c: U8x16.splat(exact.get(2) ?? last),
		members,
		kind,
	}
}

# The index of the first byte at or after `pos` whose membership in `class`
# equals `want`, or the length of `bytes`.
scan_class : Parser.Utf8.Bytes, U64, Parser.Utf8.ByteClass, Bool -> U64
scan_class = |bytes, pos, class, want| {
	len = bytes.len()
	var $i = pos
	match class.kind {
		Exact => {
			a = class.a
			b = class.b
			c = class.c
			var $scanning = True
			while $scanning {
				match U8x16.load(bytes, $i) {
					Ok(v) => {
						inside = U8x16.to_bitmask(U8x16.bitwise_or(U8x16.bitwise_or(U8x16.eq_lanes(v, a), U8x16.eq_lanes(v, b)), U8x16.eq_lanes(v, c)))
						mask = if (want) inside else inside.bitwise_not()
						if mask != 0 {
							$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
							$scanning = False
						} else {
							$i = $i + 16
						}
					}
					Err(_) => {
						$scanning = False
					}
				}
			}
		}
		Nibbles => {
			lo_table = class.lo
			hi_table = class.hi
			nibble = U8x16.splat(15)
			zero = U8x16.splat(0)
			var $scanning = True
			while $scanning {
				match U8x16.load(bytes, $i) {
					Ok(v) => {
						lo = U8x16.table_lookup(lo_table, U8x16.bitwise_and(v, nibble))
						hi = U8x16.table_lookup(hi_table, U8x16.shr_zf_wrap(v, 4))
						outside = U8x16.to_bitmask(U8x16.eq_lanes(U8x16.bitwise_and(lo, hi), zero))
						mask = if (want) outside.bitwise_not() else outside
						if mask != 0 {
							$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
							$scanning = False
						} else {
							$i = $i + 16
						}
					}
					Err(_) => {
						$scanning = False
					}
				}
			}
		}
		Bytewise => {}
	}
	while $i < len {
		if (class.members.get((bytes.get($i) ?? 0).to_u64()) ?? False) == want {
			break
		}
		$i = $i + 1
	}
	$i
}

# A mismatch retains the expected byte without retaining the input.

expect {
	long = List.repeat('b', 1000)
	match Parser.run(Parser.Utf8.codeunit('a'), long) {
		Err(ParseError({ problem, .. })) => problem == ExpectedByte('a')
		Ok(_) => False
	}
}

# Any codeunit parser accepts a lowercase ASCII byte.
expect {
	actual = (Parser.Utf8.any_codeunit({})).parse_str("a")?
	actual == 'a'
}

# Any codeunit parser accepts a dollar-sign byte.
expect {
	actual = (Parser.Utf8.any_codeunit({})).parse_str("\$")?
	actual == 36
}

# Any input parser consumes all bytes and returns them.
expect {
	bytes = "consumes all the input".to_utf8()
	actual = Parser.parse(Parser.Utf8.rest({}), bytes)?
	actual == bytes
}

# -------------------- example snippets used in docs --------------------

parse_u32 : Parser(Parser.Utf8.Bytes, U32, _)
parse_u32 =
	Parser.const(U64.to_u32_wrap).keep(Parser.Utf8.digits({}))

# Digit parsing can be mapped into a U32.
expect {
	actual = (parse_u32).parse_str("123")?
	actual == 123.U32
}

color : Parser(Parser.Utf8.Bytes, [Red, Green, Blue], _)
color =
	Parser.one_of([
		Parser.const(Red).skip(Parser.Utf8.string("red")),
		Parser.const(Green).skip(Parser.Utf8.string("green")),
		Parser.const(Blue).skip(Parser.Utf8.string("blue")),
	])

# One-of parsing selects the matching color tag.
expect {
	actual = (color).parse_str("green")?
	actual == Green
}

parse_numbers : Parser(Parser.Utf8.Bytes, List(U64), _)
parse_numbers = (Parser.Utf8.digits({})).sep_by(Parser.Utf8.codeunit(','))

# Separator parsing returns the list of parsed numbers.
expect {
	actual = (parse_numbers).parse_str("1,2,3")?
	actual == [1, 2, 3]
}

# Exact string parsing succeeds when the input matches.
expect {
	actual = (Parser.Utf8.string("Foo")).parse_str("Foo")?
	actual == "Foo"
}

# Exact string parsing reports non-matching input.
expect (Parser.Utf8.string("Foo")).parse_str("Bar").is_err()

ignore_text : Parser(Parser.Utf8.Bytes, U64, _)
ignore_text =
	Parser.const(
		|d| {
			d
		},
	)
		.skip(Parser.chomp_until(':'))
		.skip(Parser.Utf8.codeunit(':'))
		.keep(Parser.Utf8.digits({}))

# Skipping a prefix can parse the numeric suffix.
expect {
	actual = (ignore_text).parse_str("ignore preceding text:123")?
	actual == 123
}

ignore_numbers : Parser(Parser.Utf8.Bytes, Str, _)
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
		.keep(Parser.Utf8.string("TEXT"))

# Chomping digits can leave the following text parser result.
expect {
	actual = (ignore_numbers).parse_str("0123456789876543210TEXT")?
	actual == "TEXT"
}

is_digit : U8 -> Bool
is_digit = |b| {
	b >= '0' and b <= '9'
}

# Codeunit predicates can accept digit bytes.
expect {
	actual = (Parser.Utf8.codeunit_satisfies(is_digit)).parse_str("0")?
	actual == '0'
}

# Codeunit predicates reject bytes that do not satisfy the predicate.
expect (Parser.Utf8.codeunit_satisfies(is_digit)).parse_str("*").is_err()

at_sign : Parser(Parser.Utf8.Bytes, [AtSign], _)
at_sign = Parser.const(AtSign).skip(Parser.Utf8.codeunit('@'))

# The at-sign parser succeeds on an at-sign byte.
expect {
	actual = (at_sign).parse_str("@")?
	actual == AtSign
}

# Partial parsing returns the parsed at-sign tag.
expect {
	actual = (at_sign).run_str("@")?
	actual.value == AtSign
}

# The at-sign parser rejects other bytes.
expect (at_sign).run_str("\$").is_err()

## A text-prefix runner rejects a remainder beginning inside a UTF-8 scalar.
expect (Parser.Utf8.any_codeunit({})).run_str("ӿ") == Err(ParseError(Parser.Diagnostic.{ problem: Utf8Boundary, location: Text({ offset: 1, line: 1, column: 2 }), path: [] }))

## Complete text parsing reports original byte and physical coordinates.
expect (Parser.Utf8.any_codeunit({})).parse_str("ӿ") == Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedInput, location: Text({ offset: 1, line: 1, column: 2 }), path: [] }))

Requirement : [Green(U64), Red(U64), Blue(U64)]

RequirementSet : List(Requirement)

Game : { id : U64, requirements : List(RequirementSet) }

parse_game : Str -> Try(Game, [ParsingError])
parse_game = |s| {
	green = Parser.const(|x| Green(x)).keep(Parser.Utf8.digits({})).skip(Parser.Utf8.string(" green"))
	red = Parser.const(|x| Red(x)).keep(Parser.Utf8.digits({})).skip(Parser.Utf8.string(" red"))
	blue = Parser.const(|x| Blue(x)).keep(Parser.Utf8.digits({})).skip(Parser.Utf8.string(" blue"))

	requirement_set : Parser(_, RequirementSet, _)
	requirement_set = Parser.one_of([green, red, blue]).sep_by(Parser.Utf8.string(", "))

	requirements : Parser(_, List(RequirementSet), _)
	requirements = requirement_set.sep_by(Parser.Utf8.string("; "))

	game : Parser(_, Game, _)
	game =
		Parser.const(
			|id| {
				|r| {
					{ id, requirements: r }
				}
			},
		)
			.skip(Parser.Utf8.string("Game "))
			.keep(Parser.Utf8.digits({}))
			.skip(Parser.Utf8.string(": "))
			.keep(requirements)

	match (game).parse_str(s) {
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
	actual = (Parser.Utf8.digit({})).parse_str("0")?
	actual == 0
}

# Single digit parsing rejects non-digit text.
expect (Parser.Utf8.digit({})).parse_str("not a digit").is_err()

# Multiple digit parsing accepts leading zeroes.
expect {
	actual = (Parser.Utf8.digits({})).parse_str("0123")?
	actual == 123
}

# Multiple digit parsing accepts the largest U64.
expect {
	actual = (Parser.Utf8.digits({})).parse_str("18446744073709551615")?
	actual == 18446744073709551615
}

# Multiple digit parsing rejects values larger than U64 without crashing.
expect (Parser.Utf8.digits({})).parse_str("18446744073709551616").is_err()

# Multiple digit parsing rejects text without a leading digit.
expect (Parser.Utf8.digits({})).parse_str("not a digit").is_err()

bool_parser : Parser(Parser.Utf8.Bytes, Bool, _)
bool_parser =
	Parser.one_of([Parser.Utf8.string("true"), Parser.Utf8.string("false")])
		.map(
			|x| {
				x == "true"
			},
		)

# Boolean parser maps true text to True.
expect {
	actual = (bool_parser).parse_str("true")?
	actual == True
}

# Boolean parser maps false text to False.
expect {
	actual = (bool_parser).parse_str("false")?
	actual == False
}

# Boolean parser rejects other text.
expect (bool_parser).parse_str("not a bool").is_err()

even : Parser(Parser.Utf8.Bytes, U64, _)
even =
	Parser.Utf8.digits({})
		.map(
			|n| {
				if n % 2 == 0 {
					Ok(n)
				} else {
					Err(OddNumber)
				}
			},
		)
		.flatten()

# Flattening keeps an Ok value from the mapped function.
expect (even).parse_str("42") == Ok(42)

# Flattening retains a caller-defined structured rejection.
expect (even).parse_str("7") == Err(ParseError(Parser.Diagnostic.{ problem: OddNumber, location: Text({ offset: 0, line: 1, column: 1 }), path: [] }))

# Capturing up to a delimiter returns the bytes before it.
expect {
	capture_text = Parser.const(|codeunits| codeunits).keep(Parser.chomp_until(':')).skip(Parser.Utf8.codeunit(':'))
	(capture_text).parse_str("Roc:") == Ok(['R', 'o', 'c'])
}

# Capturing while a predicate holds returns the matching bytes.
expect {
	capture_numbers =
		Parser.const(|codeunits| codeunits)
			.keep(Parser.chomp_while(|b| b >= '0' and b <= '9'))
			.skip(Parser.Utf8.string("TEXT"))
	(capture_numbers).parse_str("123TEXT") == Ok(['1', '2', '3'])
}

# Mapping three parsers matches applying a curried constructor three times.
expect {
	space = Parser.Utf8.codeunit(' ')
	mapped = Parser.map3(Parser.Utf8.digits({}).skip(space), Parser.Utf8.digits({}).skip(space), Parser.Utf8.digits({}), |x, y, z| Triple(x, y, z))
	applied =
		Parser.const(|x| |y| |z| Triple(x, y, z))
			.keep(Parser.Utf8.digits({}).skip(space))
			.keep(Parser.Utf8.digits({}).skip(space))
			.keep(Parser.Utf8.digits({}))
	(mapped).parse_str("1 2 3") == Ok(Triple(1, 2, 3)) and (applied).parse_str("1 2 3") == Ok(Triple(1, 2, 3))
}

# A length-prefixed field reads as many bytes as its digit says.
expect {
	sized : Parser(Parser.Utf8.Bytes, List(U8), _)
	sized = Parser.Utf8.digit({}).and_then(|n| Parser.Utf8.any_codeunit({}).many().map(|bytes| bytes.take_first(n)))
	(sized).parse_str("2abc") == Ok(['a', 'b'])
}

# Between keeps only the delimited value.
expect {
	between_brackets = |parser| parser.between(Parser.Utf8.codeunit('['), Parser.Utf8.codeunit(']'))
	(between_brackets(Parser.Utf8.digits({}))).parse_str("[42]") == Ok(42)
}

# The rest of the input is read as a Str, and invalid UTF-8 fails without crashing.
expect (Parser.Utf8.rest_str({})).parse_str("héllo") == Ok("héllo")
expect (Parser.Utf8.rest_str({})).parse([0xFF]).is_err()

# A bad element in the middle of a repetition reports its own failure and offset.
expect {
	numbers = Parser.Utf8.digits({}).sep_by(Parser.Utf8.codeunit(','))
	match (numbers).parse_str("1,2,x,4") {
		Err(ParseError(error)) => error.location == Text({ offset: 4, line: 1, column: 5 })
		Ok(_) => False
	}
}

# Leftover input after a number is reported where the number stopped.
expect (Parser.Utf8.digits({})).parse_str("12 ") == Err(ParseError(Parser.Diagnostic.{ problem: UnexpectedInput, location: Text({ offset: 2, line: 1, column: 3 }), path: [] }))

# ParseError composes with `?` in a function that returns other errors too.
expect {
	both : Str -> Try(U64, [ParseError(Parser.Diagnostic(Parser.Problem(_))), TooBig])
	both = |text| {
		n = (Parser.Utf8.digits({})).parse_str(text)?
		if (n > 100) Err(TooBig) else Ok(n)
	}
	both("7") == Ok(7) and both("700") == Err(TooBig) and both("x").is_err()
}

# Reference scan for the vector primitives: the first index at or after `pos`
# whose membership equals `want`.
scan_reference : List(U8), U64, (U8 -> Bool), Bool -> U64
scan_reference = |bytes, pos, member, want| {
	var $i = pos
	while $i < bytes.len() {
		if member(bytes.get($i) ?? 0) == want {
			break
		}
		$i = $i + 1
	}
	$i
}

every_byte : List(U8)
every_byte = {
	var $all = []
	var $b = 0.U64
	while $b < 256 {
		$all = $all.append($b.to_u8_wrap())
		$b = $b + 1
	}
	$all
}

# Every byte value, at several lane positions of a 16-byte block and in the
# tail, is classified like the scalar reference, for ASCII, high-byte and
# more-than-eight-row classes.
expect {
	classes = [
		|b| b == '<' or b == '&',
		|b| (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') or (b >= '0' and b <= '9') or b == '-' or b == '_' or b == '.' or b == ':' or b >= 0x80,
		|b| b == ' ' or b == '\t' or b == '\n' or b == '\r',
		|b| b % 3 == 0,
		|b| b % 16 <= b / 16,
		|b| b >= 0xC0,
	]
	classes.all(
		|member| {
			class = Parser.Utf8.ByteClass.from_predicate(member)
			every_byte.all(
				|byte| {
					other = if (member('a')) 0x00 else 'a'
					filler = if (member(other)) 0x01 else other
					run = List.repeat(byte, 37)
					[0, 5, 15, 16, 31, 40].all(
						|at| {
							input = List.repeat(filler, 41).set(at, byte) ?? []
							Parser.Utf8.find_any(input, 0, class) == scan_reference(input, 0, member, True)
								and Parser.Utf8.find_any(input, 3, class) == scan_reference(input, 3, member, True)
						},
					)
						and Parser.Utf8.skip_class(run, 0, class) == scan_reference(run, 0, member, False)
				},
			)
		},
	)
}

# Sets of up to eight distinct row shapes scan with vectors; others with bytes.
expect Parser.Utf8.ByteClass.from_predicate(|b| b % 3 == 0).kind == Nibbles
expect Parser.Utf8.ByteClass.from_predicate(|b| b % 16 <= b / 16).kind == Bytewise
expect Parser.Utf8.ByteClass.from_bytes(['<', '&']).kind == Exact

expect Parser.Utf8.find_line_end(List.repeat('a', 40).append('\n'), 0) == 40
expect Parser.Utf8.find_line_end(List.repeat('a', 40).append('\r'), 7) == 40
expect Parser.Utf8.find_line_end("abc".to_utf8(), 0) == 3
expect Parser.Utf8.find_line_end("abc".to_utf8(), 5) == 5
expect Parser.Utf8.find_any([], 0, Parser.Utf8.ByteClass.from_bytes(['x'])) == 0
expect Parser.Utf8.ByteClass.from_bytes(['a']).complement().contains('b')
expect (Parser.Utf8.span_class(Parser.Utf8.ByteClass.from_bytes(['a']))).run_str("aaab").map_ok(|r| r.rest) == Ok("b")

expect Parser.Quotation.check({ parser: Parser.Utf8.digits({}), source: "42" }) == Ok(42)

expect match Parser.Quotation.check({ parser: Parser.Utf8.digits({}), source: "42x", name: "count.txt" }) {
	Err(BadQuotedBytes(message)) => message.starts_with("count.txt:1:3: parser:")
	Ok(_) => False
}

expect {
	parser : Parser(Parser.Utf8.Bytes, U64, _)
	parser = Parser.one_of([
		Parser.const(1).skip(Parser.Utf8.string("ab")).skip(Parser.Utf8.codeunit('x')),
		Parser.const(2).skip(Parser.Utf8.codeunit('a')),
	])
	match Parser.Quotation.check({ parser, source: "abz", name: "branch.txt" }) {
		Err(BadQuotedBytes(message)) => message.starts_with("branch.txt:1:3: parser:")
		Ok(_) => False
	}
}

expect Parser.Quotation.require(Ok(42)) == 42
expect Parser.Quotation.error({ format: "CSV", name: "people.csv", message: "missing age" }) == BadQuotedBytes("people.csv: CSV: missing age")
expect Parser.Quotation.error({ format: "YAML", message: "bad value", line: 2, column: 3 }) == BadQuotedBytes("<literal>:2:3: YAML: bad value")
expect Parser.Quotation.position("a\r\nb\rc\né", 9) == { line: 4, column: 3 }
expect Parser.Quotation.position("a\r\nb", 3) == { line: 2, column: 1 }

## Explicitly unavailable source positions and logical paths survive problem translation.
expect {
	custom = Parser.custom(|_input| Err(ParseError(Parser.Diagnostic.{ problem: MissingValue, location: Unavailable(NotRetained), path: [Record(3), Field(2)] })))
	renamed = custom.map_problem(|problem| Domain(problem))
	renamed.parse("source".to_utf8()) == Err(ParseError(Parser.Diagnostic.{ problem: Domain(MissingValue), location: Unavailable(NotRetained), path: [Record(3), Field(2)] }))
}

## Generic offsets rebase after a prefix and text runners add original CRLF coordinates.
expect {
	suffix = Parser.custom(|_input| Err(ParseError(Parser.Diagnostic.{ problem: MissingSuffix, location: InputOffset(1), path: [Key("suffix")] })))
	parser = Parser.const({}).skip(Parser.Utf8.string("a\r\n")).skip(suffix)
	parser.parse_str("a\r\nxy") == Err(ParseError(Parser.Diagnostic.{ problem: MissingSuffix, location: Text({ offset: 4, line: 2, column: 2 }), path: [Key("suffix")] }))
}

## Explicit line-and-column information is retained even when no source bytes remain.
expect {
	parser = Parser.custom(|_input| Err(ParseError(Parser.Diagnostic.{ problem: MissingValue, location: LineColumn({ line: 8, column: 4 }), path: [] })))
	match parser.parse_str("") {
		Err(ParseError(error)) => error.location == LineColumn({ line: 8, column: 4 })
		Ok(_) => False
	}
}
