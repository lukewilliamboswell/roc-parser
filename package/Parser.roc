## Generic [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator)
## for transforming input into structured values.
##
## A `Parser(input, a)` is a value that describes how to read an `a` from the
## front of an `input`. Combine small parsers with `keep`, `skip`, `map`,
## `one_of`, `many` and `sep_by` to build larger ones, then run the result with
## `Utf8.parse_str` (for `Str`) or `Parser.parse` (for any input type).
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
##     green = Parser.const(|x| Green(x)).keep(Utf8.digits).skip(Utf8.string(" green"))
##     red = Parser.const(|x| Red(x)).keep(Utf8.digits).skip(Utf8.string(" red"))
##     blue = Parser.const(|x| Blue(x)).keep(Utf8.digits).skip(Utf8.string(" blue"))
##
##     requirement_set : Parser(_, RequirementSet)
##     requirement_set = Parser.one_of([green, red, blue]).sep_by(Utf8.string(", "))
##
##     requirements : Parser(_, List(RequirementSet))
##     requirements = requirement_set.sep_by(Utf8.string("; "))
##
##     game : Parser(_, Game)
##     game =
##         Parser.const(|id| |r| { id, requirements: r })
##             .skip(Utf8.string("Game "))
##             .keep(Utf8.digits)
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
## The representation is internal and may change to improve efficiency or error messages.
Parser(input, a) :: { fun : input -> Step(input, a) }.{

	## The result of running a parser on part of an input: the parsed `value`
	## and the `rest` of the input, or a `ParseError` whose `offset` counts the
	## input elements (bytes, for UTF-8 input) read before the failure.
	ParseResult(input, a) : Try({ value : a, rest : input }, [ParseError({ message : Str, offset : U64 })])

	## Write a custom parser without using the provided combinators.
	##
	## The function receives the remaining input and returns either the parsed
	## `value` with the `rest` it did not consume, or
	## `Err(ParseError({ message, offset }))`, where `offset` is how far into
	## the given input the failure is (usually `0`).
	custom : (input -> ParseResult(input, a)) -> Parser(input, a) where [input.len : input -> U64]
	custom = |fun| {
		{
			fun: |input| {
				match fun(input) {
					Ok({ value, rest }) => Ok({ value, rest, furthest: no_failure })
					Err(ParseError({ message, offset })) => Err({ message, remaining: minus(input.len(), offset) })
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
	run : Parser(input, a), input -> ParseResult(input, a) where [input.len : input -> U64]
	run = |parser, input| {
		match step(parser, input) {
			Ok({ value, rest, furthest: _ }) => Ok({ value, rest })
			Err(failure) => Err(ParseError({ message: failure.message, offset: minus(input.len(), failure.remaining) }))
		}
	}

	## Run a parser on the given input, expecting it to consume all of it.
	##
	## The input type only needs a `len` method, which is used both to detect
	## leftover input and to turn the furthest failure into an `offset` counted
	## from the start of `input`. Leftover input is a failure too: if some
	## parser failed at or beyond the leftover, its message is reported;
	## otherwise the message is `unexpected input`. For UTF-8 text use
	## `Utf8.parse_str` or `Utf8.parse_bytes`.
	parse : Parser(input, a), input -> Try(a, [ParseError({ message : Str, offset : U64 })]) where [input.len : input -> U64]
	parse = |parser, input| {
		total = input.len()
		match step(parser, input) {
			Ok({ value, rest, furthest }) => {
				remaining = rest.len()
				if remaining == 0 {
					Ok(value)
				} else if furthest.remaining <= remaining {
					Err(ParseError({ message: furthest.message, offset: minus(total, furthest.remaining) }))
				} else {
					Err(ParseError({ message: "unexpected input", offset: minus(total, remaining) }))
				}
			}
			Err(failure) => Err(ParseError({ message: failure.message, offset: minus(total, failure.remaining) }))
		}
	}

	## Parser that can never succeed, regardless of the given input.
	## It will always fail with the given error message.
	##
	## This is mostly useful as a 'base case' if all other parsers
	## in a `one_of` or `alt` have failed, to provide some more descriptive error message.
	fail : Str -> Parser(input, a) where [input.len : input -> U64]
	fail = |message| {
		{ fun: |input| Err({ message, remaining: input.len() }) }
	}

	## Parser that will always produce the given `a`, without looking at the actual input.
	##
	## This is the usual start of a pipeline: `const` supplies a (curried)
	## constructor function and each `keep` feeds it one parsed value.
	## ```roc
	## parse_u32 : Parser(Utf8.Bytes, U32)
	## parse_u32 = Parser.const(U64.to_u32_wrap).keep(Utf8.digits)
	##
	## expect Utf8.parse_str(parse_u32, "123") == Ok(123.U32)
	## ```
	const : a -> Parser(_, a)
	const = |value| {
		{ fun: |input| Ok({ value, rest: input, furthest: no_failure }) }
	}

	## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
	##
	## The `second` parser starts from the same input as `first` (backtracking).
	## If both fail, the failure that reached further is reported; when both
	## stopped at the same place, the `second` one is.
	alt : Parser(input, a), Parser(input, a) -> Parser(input, a)
	alt = |first, second| {
		{
			fun: |input| {
				match step(first, input) {
					Ok(ok) => Ok(ok)
					Err(first_failure) => {
						match step(second, input) {
							Ok({ value, rest, furthest }) => Ok({ value, rest, furthest: further(first_failure, furthest) })
							# Keep one message rather than joining them: failures are
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
	## For UTF-8 input, `Parser.one_of` behaves the same way.
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
	one_of : List(Parser(input, a)) -> Parser(input, a) where [input.len : input -> U64]
	one_of = |parsers| {
		match parsers {
			[] => fail("one_of: the list of parsers was empty")
			[first, .. as others] => others.fold(first, |earlier, later| alt(earlier, later))
		}
	}

	## Transforms the result of parsing into something else,
	## using the given transformation function.
	map : Parser(input, a), (a -> b) -> Parser(input, b)
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
	map2 : Parser(input, a), Parser(input, b), (a, b -> c) -> Parser(input, c)
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
	map3 : Parser(input, a), Parser(input, b), Parser(input, c), (a, b, c -> d) -> Parser(input, d)
	map3 = |parser_a, parser_b, parser_c, transform| {
		const(|a| |b| |c| transform(a, b, c))
			.keep(parser_a)
			.keep(parser_b)
			.keep(parser_c)
	}

	## Removes a layer of `Try` from running the parser.
	##
	## Use this to map functions that return a `Try` over the parser:
	## an `Err(msg)` value becomes a failure with that message, reported where
	## the inner parser started.
	##
	## ```roc
	## even : Parser(Utf8.Bytes, U64)
	## even =
	##     Utf8.digits
	##         .map(|n| if n % 2 == 0 { Ok(n) } else { Err("odd number") })
	##         .flatten()
	##
	## expect Utf8.parse_str(even, "42") == Ok(42)
	## expect Utf8.parse_str(even, "7") == Err(ParseError({ message: "odd number", offset: 0 }))
	## ```
	flatten : Parser(input, Try(a, Str)) -> Parser(input, a) where [input.len : input -> U64]
	flatten = |parser| {
		{
			fun: |input| {
				match step(parser, input) {
					Err(failure) => Err(failure)
					Ok({ value: Ok(value), rest, furthest }) => Ok({ value, rest, furthest })
					# The value was read but rejected: report that, not a failure
					# hidden inside the parser that read it.
					Ok({ value: Err(message), rest: _, furthest: _ }) => Err({ message, remaining: input.len() })
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
	## sized : Parser(Utf8.Bytes, List(U8))
	## sized = Utf8.digit.and_then(|n| Utf8.any_codeunit.many().map(|bytes| bytes.take_first(n)))
	## ```
	and_then : Parser(input, a), (a -> Parser(input, b)) -> Parser(input, b)
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
	## Mutually recursive top-level parser values currently require a lower-level
	## workaround because of [roc-lang/roc#10098](https://github.com/roc-lang/roc/issues/10098).
	lazy : ({} -> Parser(input, a)) -> Parser(input, a)
	lazy = |thunk| {
		{ fun: |input| step(thunk({}), input) }
	}

	## Make a parser optional.
	##
	## Returns `Ok(value)` when the given parser succeeds, and `Err(Missing)`
	## without consuming input when it fails, so the result never fails.
	maybe : Parser(input, a) -> Parser(input, Try(a, [Missing]))
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
	many : Parser(input, a) -> Parser(input, List(a)) where [input.len : input -> U64]
	many = |parser| {
		{ fun: |input| many_help(parser, [], input, input.len(), no_failure) }
	}

	## A parser which runs the element parser *one* or more times on the input,
	## returning a list containing all the parsed elements.
	##
	## Fails when the first element fails. Also see [Parser.many].
	one_or_more : Parser(input, a) -> Parser(input, List(a)) where [input.len : input -> U64]
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
	between : Parser(input, a), Parser(input, open), Parser(input, close) -> Parser(input, a)
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
	sep_by_one_or_more : Parser(input, a), Parser(input, sep) -> Parser(input, List(a)) where [input.len : input -> U64]
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
	## parse_numbers : Parser(Utf8.Bytes, List(U64))
	## parse_numbers = Utf8.digits.sep_by(Utf8.codeunit(','))
	##
	## expect Utf8.parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])
	## ```
	sep_by : Parser(input, a), Parser(input, sep) -> Parser(input, List(a)) where [input.len : input -> U64]
	sep_by = |parser, separator| {
		parser
			.sep_by_one_or_more(separator)
			.alt(const([]))
	}

	## Discard a parser's value while preserving how much input it consumes.
	##
	## Useful with `many` to skip a repeated token without collecting values.
	ignore : Parser(input, a) -> Parser(input, {})
	ignore = |parser| {
		parser.map(|_| {})
	}

	## Run a parser producing a function, then a parser producing its argument,
	## and return the function result.
	##
	## Start with `const(|a| |b| ...)` and add one `keep` per argument, as in
	## the module example. The function must be curried (`|a| |b| ...`, not
	## `|a, b| ...`), because its arguments are applied one at a time.
	keep : Parser(input, (a -> b)), Parser(input, a) -> Parser(input, b)
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
	## at_sign : Parser(Utf8.Bytes, [AtSign])
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str(at_sign, "@") == Ok(AtSign)
	## ```
	skip : Parser(input, a), Parser(input, _) -> Parser(input, a)
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
	## ignore_text : Parser(Utf8.Bytes, U64)
	## ignore_text =
	##     Parser.const(|d| d)
	##         .skip(Parser.chomp_until(':'))
	##         .skip(Utf8.codeunit(':'))
	##         .keep(Utf8.digits)
	##
	## expect Utf8.parse_str(ignore_text, "ignore preceding text:123") == Ok(123)
	## ```
	##
	## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
	##
	## ```roc
	## capture_text : Parser(Utf8.Bytes, List(U8))
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
	chomp_until : a -> Parser(List(a), List(a)) where [a.is_eq : a, a -> Bool]
	chomp_until = |char| {
		{
			fun: |input| {
				match input.find_first_index(|x| x == char) {
					Ok(index) => Ok({ value: input.sublist({ start: 0, len: index }), rest: input.drop_first(index), furthest: no_failure })
					Err(_) => Err({ message: "character not found", remaining: input.len() })
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
	## ignore_numbers : Parser(Utf8.Bytes, Str)
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
	## capture_numbers : Parser(Utf8.Bytes, List(U8))
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
	chomp_while : (a -> Bool) -> Parser(List(a), List(a))
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
					Ok({ value: [], rest: input, furthest: no_failure })
				} else {
					Ok({ value: input.sublist({ start: 0, len: index }), rest: input.drop_first(index), furthest: no_failure })
				}
			},
		}
	}
}

# A failure, positioned by how much input was left when it happened, so that
# failures can be compared for any input type without keeping the input.
Failure : { message : Str, remaining : U64 }

# What a parser function returns internally. A success also carries the
# furthest failure seen while producing it (for example, the element that
# stopped a `many`), so that a later failure at or before that point can
# report it instead.
Step(input, a) : Try({ value : a, rest : input, furthest : Failure }, Failure)

# The `furthest` of a success that saw no failure. Its `remaining` is larger
# than any real input, so every real failure is further.
no_failure : Failure
no_failure = { message: "", remaining: U64.highest }

# The failure that reached further into the input; `b` on a tie, since it
# happened later.
further : Failure, Failure -> Failure
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

step : Parser(input, a), input -> Step(input, a)
step = |{ fun }, input| {
	fun(input)
}

# Repeat `parser` from `input`, whose length is `len`, collecting `values`.
# The no-progress check compares lengths, so each iteration is O(1).
many_help : Parser(input, a), List(a), input, U64, Failure -> Step(input, List(a)) where [input.len : input -> U64]
many_help = |parser, values, input, len, furthest| {
	match step(parser, input) {
		Err(failure) =>
			Ok({ value: values, rest: input, furthest: further(furthest, failure) })

		Ok(next) => {
			next_len = next.rest.len()
			if next_len == len {
				# No progress: repeating would loop forever on the same input.
				Ok({ value: values, rest: input, furthest: further(furthest, next.furthest) })
			} else {
				many_help(parser, values.append(next.value), next.rest, next_len, further(furthest, next.furthest))
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
				_ => Err(ParseError({ message: "expected ${w}", offset: 0 }))
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
				_ => Err(ParseError({ message: "expected ${w}", offset: 0 }))
			}
		},
	)
	Parser.parse(Parser.many(word("a")), words) == Err(ParseError({ message: "expected a", offset: 2 }))
}
