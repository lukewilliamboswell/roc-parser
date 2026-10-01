## Generic [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator)
## for transforming input into structured values.
##
## A `Parser(input, a)` is a value that describes how to read an `a` from the
## front of an `input`. Combine small parsers with `keep`, `skip`, `map`,
## `one_of`, `many` and `sep_by` to build larger ones, then run the result with
## `String.parse_str` (for `Str`) or `Parser.parse` (for any input type).
##
## This parser turns `"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"`
## into `{ id: 1, requirements: [[Blue(3), Red(4)], [Red(1), Green(2), Blue(6)], [Green(2)]] }`
## (the same code is a test in `String.roc`):
## ```roc
## Requirement : [Green(U64), Red(U64), Blue(U64)]
## RequirementSet : List(Requirement)
## Game : { id : U64, requirements : List(RequirementSet) }
##
## parse_game : Str -> Try(Game, [ParsingError])
## parse_game = |s| {
##     green = Parser.const(|x| Green(x)).keep(String.digits).skip(String.string(" green"))
##     red = Parser.const(|x| Red(x)).keep(String.digits).skip(String.string(" red"))
##     blue = Parser.const(|x| Blue(x)).keep(String.digits).skip(String.string(" blue"))
##
##     requirement_set : Parser(_, RequirementSet)
##     requirement_set = String.one_of([green, red, blue]).sep_by(String.string(", "))
##
##     requirements : Parser(_, List(RequirementSet))
##     requirements = requirement_set.sep_by(String.string("; "))
##
##     game : Parser(_, Game)
##     game =
##         Parser.const(|id| |r| { id, requirements: r })
##             .skip(String.string("Game "))
##             .keep(String.digits)
##             .skip(String.string(": "))
##             .keep(requirements)
##
##     match String.parse_str(game, s) {
##         Ok(g) => Ok(g)
##         Err(ParsingFailure(_)) | Err(ParsingIncomplete(_)) => Err(ParsingError)
##     }
## }
## ```
##
## Alternatives backtrack: when one alternative of `alt` or `one_of` fails, the
## next one is tried on the original input, however much the failed one read.
##
## Opaque type for a parser that will try to parse an `a` from an `input`.
##
## As such, a parser can be considered a recipe for a function of the type
## ```roc
## input -> Try({val: a, input: input}, [ParsingFailure(Str)])
## ```
##
## The representation is internal and may change to improve efficiency or error messages.
Parser(input, a) :: { fun : input -> Parser.ParseResult(input, a) }.{

	## The result of parsing part of an input: either a value and the remaining
	## input, or a `ParsingFailure` message.
	ParseResult(input, a) : Try({ val : a, input : input }, [ParsingFailure(Str)])

	## Write a custom parser without using the provided combinators.
	##
	## The function receives the remaining input and returns either the parsed
	## value with the input it did not consume, or `Err(ParsingFailure(msg))`.
	build_primitive_parser : (input -> ParseResult(input, a)) -> Parser(input, a)
	build_primitive_parser = |fun| {
		{ fun }
	}

	## Most general way of running a parser.
	##
	## Can be thought of as turning the recipe of a parser into its actual parsing function
	## and running this function on the given input.
	##
	## Most parsers consume part of `input` when they succeed. This allows you to string parsers
	## together that run one after the other. The part of the input that the first
	## parser did not consume, is used by the next parser.
	## This is why a parser returns on success both the resulting value and the leftover part of the input.
	##
	## This is mostly useful when creating your own internal parsing building blocks.
	parse_partial : Parser(input, a), input -> ParseResult(input, a)
	parse_partial = |{ fun }, input| {
		fun(input)
	}

	## Run a parser on the given input, expecting it to consume all of it.
	##
	## The `input -> Bool` parameter is used to check whether parsing has 'completed',
	## i.e. how to determine if all of the input has been consumed.
	##
	## For most input types, a parsing run that leaves some unparsed input behind
	## should be considered an error, so leftover input is reported as
	## `Err(ParsingIncomplete(leftover))`. For `Str` input use `String.parse_str`,
	## which supplies the completion check for you.
	parse : Parser(input, a), input, (input -> Bool) -> Try(a, [ParsingFailure(Str), ParsingIncomplete(input)])
	parse = |parser, input, is_parsing_completed| {
		match parser.parse_partial(input) {
			Ok({ val: val, input: leftover }) => {
				if is_parsing_completed(leftover) {
					Ok(val)
				} else {
					Err(ParsingIncomplete(leftover))
				}
			}
			Err(ParsingFailure(msg)) => {
				Err(ParsingFailure(msg))
			}
		}
	}

	## Parser that can never succeed, regardless of the given input.
	## It will always fail with the given error message.
	##
	## This is mostly useful as a 'base case' if all other parsers
	## in a `one_of` or `alt` have failed, to provide some more descriptive error message.
	fail : Str -> Parser(_, _)
	fail = |msg| {
		build_primitive_parser(
			|_input| {
				Err(ParsingFailure(msg))
			},
		)
	}

	## Parser that will always produce the given `a`, without looking at the actual input.
	##
	## This is the usual start of a pipeline: `const` supplies a (curried)
	## constructor function and each `keep` feeds it one parsed value.
	## ```roc
	## parse_u32 : Parser(String.Utf8, U32)
	## parse_u32 = Parser.const(U64.to_u32_wrap).keep(String.digits)
	##
	## expect String.parse_str(parse_u32, "123") == Ok(123.U32)
	## ```
	const : a -> Parser(_, a)
	const = |val| {
		build_primitive_parser(
			|input| {
				Ok({ val: val, input: input })
			},
		)
	}

	## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
	##
	## The `second` parser starts from the same input as `first` (backtracking).
	## If both fail, the failure message joins both messages with `or`.
	alt : Parser(input, a), Parser(input, a) -> Parser(input, a)
	alt = |first, second| {
		build_primitive_parser(
			|input| {
				match parse_partial(first, input) {
					Ok({ val: val, input: rest }) => Ok({ val: val, input: rest })
					Err(ParsingFailure(first_err)) => {
						match parse_partial(second, input) {
							Ok({ val: val, input: rest }) => Ok({ val: val, input: rest })
							Err(ParsingFailure(second_err)) => {
								Err(ParsingFailure("${first_err} or ${second_err}"))
							}
						}
					}
				}
			},
		)
	}

	## Runs a parser building a function, then a parser building a value,
	## and finally returns the result of calling the function with the value.
	##
	## This is useful if you are building up a structure that requires more parameters
	## than there are variants of `map`, `map2`, `map3` etc. for.
	##
	## For instance, the following two are the same:
	## ```roc
	## Parser.map3(String.digits, String.digits, String.digits, |x, y, z| Triple(x, y, z))
	##
	## Parser.const(|x| |y| |z| Triple(x, y, z))
	##     .apply(String.digits)
	##     .apply(String.digits)
	##     .apply(String.digits)
	## ```
	## Indeed, this is how `map`, `map2`, `map3` etc. are implemented under the hood.
	##
	## Currying:
	## Be aware that when using `apply`, you need to explicitly 'curry' the parameters to the construction function.
	## This means that instead of writing `|x, y, z| ...`
	## you'll need to write `|x| |y| |z| ...`.
	## This is because the parameters of the function will be applied one by one as parsing continues.
	apply : Parser(input, (a -> b)), Parser(input, a) -> Parser(input, b)
	apply = |fun_parser, val_parser| {
		combined = |input| {
			{ val: fun_val, input: rest } = fun_parser.parse_partial(input)?
			val_parser.parse_partial(rest)
				.map_ok(
					|{ val: val, input: rest2 }| {
						{ val: fun_val(val), input: rest2 }
					},
				)
		}
		build_primitive_parser(combined)
	}

	## Try a list of parsers in turn, until one of them succeeds.
	##
	## Each parser starts from the same input. An empty list always fails.
	## For UTF-8 input, `String.one_of` behaves the same way.
	## ```roc
	## color : Parser(String.Utf8, [Red, Green, Blue])
	## color =
	##     String.one_of([
	##         Parser.const(Red).skip(String.string("red")),
	##         Parser.const(Green).skip(String.string("green")),
	##         Parser.const(Blue).skip(String.string("blue")),
	##     ])
	##
	## expect String.parse_str(color, "green") == Ok(Green)
	## ```
	one_of : List(Parser(input, a)) -> Parser(input, a)
	one_of = |parsers| {
		parsers.fold_rev(
			fail("oneOf: The list of parsers was empty"),
			|earlier_parser, later_parser| {
				alt(earlier_parser, later_parser)
			},
		)
	}

	## Transforms the result of parsing into something else,
	## using the given transformation function.
	map : Parser(input, a), (a -> b) -> Parser(input, b)
	map = |simple_parser, transform| {
		const(transform)
			.apply(simple_parser)
	}

	## Transforms the result of parsing into something else,
	## using the given two-parameter transformation function.
	map2 : Parser(input, a), Parser(input, b), (a, b -> c) -> Parser(input, c)
	map2 = |parser_a, parser_b, transform| {
		const(
			|a| {
				|b| {
					transform(a, b)
				}
			},
		)
			.apply(parser_a)
			.apply(parser_b)
	}

	## Transforms the result of parsing into something else,
	## using the given three-parameter transformation function.
	##
	## If you need transformations with more inputs,
	## take a look at `apply`.
	map3 : Parser(input, a), Parser(input, b), Parser(input, c), (a, b, c -> d) -> Parser(input, d)
	map3 = |parser_a, parser_b, parser_c, transform| {
		const(
			|a| {
				|b| {
					|c| {
						transform(a, b, c)
					}
				}
			},
		)
			.apply(parser_a)
			.apply(parser_b)
			.apply(parser_c)
	}

	## Removes a layer of `Try` from running the parser.
	##
	## Use this to map functions that return a `Try` over the parser:
	## an `Err(msg)` value becomes `Err(ParsingFailure(msg))`.
	##
	## ```roc
	## even : Parser(String.Utf8, U64)
	## even =
	##     String.digits
	##         .map(|n| if n % 2 == 0 { Ok(n) } else { Err("odd number") })
	##         .flatten()
	##
	## expect String.parse_str(even, "42") == Ok(42)
	## expect String.parse_str(even, "7") == Err(ParsingFailure("odd number"))
	## ```
	flatten : Parser(input, Try(a, Str)) -> Parser(input, a)
	flatten = |parser| {
		build_primitive_parser(
			|input| {
				result = parse_partial(parser, input)

				match result {
					Err(problem) => Err(problem)
					Ok({ val: Ok(val), input: input_rest }) => Ok({ val: val, input: input_rest })
					Ok({ val: Err(problem), input: _inputRest }) => Err(ParsingFailure(problem))
				}
			},
		)
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
		const({})
			|> and_then(thunk)
	}

	## Make a parser optional.
	##
	## Returns `Ok(value)` when the given parser succeeds, and `Err(Nothing)`
	## without consuming input when it fails, so the result never fails.
	maybe : Parser(input, a) -> Parser(input, Try(a, [Nothing]))
	maybe = |parser| {
		parser
			.map(
				|val| {
					Ok(val)
				},
			)
			.alt(const(Err(Nothing)))
	}

	## A parser which runs the element parser *zero* or more times on the input,
	## returning a list containing all the parsed elements.
	##
	## Repetition stops at the first element that fails, or that succeeds
	## without consuming any input; that element's value is not included.
	## (Without this rule, a parser such as `chomp_while` or `maybe(p)` that can
	## succeed on empty input would repeat forever.)
	many : Parser(input, a) -> Parser(input, List(a)) where [input.is_eq : input, input -> Bool]
	many = |parser| {
		build_primitive_parser(
			|input| {
				many_impl(parser, [], input)
			},
		)
	}

	## A parser which runs the element parser *one* or more times on the input,
	## returning a list containing all the parsed elements.
	##
	## Fails when the first element fails. Also see [Parser.many].
	one_or_more : Parser(input, a) -> Parser(input, List(a)) where [input.is_eq : input, input -> Bool]
	one_or_more = |parser| {
		const(
			|val| {
				|vals| {
					List.prepend(vals, val)
				}
			},
		)
			.apply(parser)
			.apply(many(parser))
	}

	## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
	## and only returns the result of your main parser.
	##
	## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
	##
	## ```roc
	## between_brackets = |parser| parser.between(String.codeunit('['), String.codeunit(']'))
	## ```
	between : Parser(input, a), Parser(input, open), Parser(input, close) -> Parser(input, a)
	between = |parser, open, close| {
		const(
			|_| {
				|val| {
					|_| {
						val
					}
				}
			},
		)
			.apply(open)
			.apply(parser)
			.apply(close)
	}

	## Parse one or more values separated by `separator`.
	## The separators are consumed and omitted from the result.
	##
	## A trailing separator that is not followed by a value is left unconsumed.
	sep_by1 : Parser(input, a), Parser(input, sep) -> Parser(input, List(a)) where [input.is_eq : input, input -> Bool]
	sep_by1 = |parser, separator| {
		parser_followed_by_sep =
			const(
				|_| {
					|val| {
						val
					}
				},
			)
				.apply(separator)
				.apply(parser)

		const(
			|val| {
				|vals| {
					vals.prepend(val)
				}
			},
		)
			.apply(parser)
			.apply(many(parser_followed_by_sep))
	}

	## Parse zero or more values separated by `separator`.
	## The separators are consumed and omitted from the result.
	##
	## ```roc
	## parse_numbers : Parser(String.Utf8, List(U64))
	## parse_numbers = String.digits.sep_by(String.codeunit(','))
	##
	## expect String.parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])
	## ```
	sep_by : Parser(input, a), Parser(input, sep) -> Parser(input, List(a)) where [input.is_eq : input, input -> Bool]
	sep_by = |parser, separator| {
		parser
			.sep_by1(separator)
			.alt(const([]))
	}

	## Discard a parser's value while preserving how much input it consumes.
	##
	## Useful with `many` to skip a repeated token without collecting values.
	ignore : Parser(input, a) -> Parser(input, {})
	ignore = |parser| {
		parser.map(
			|_| {
				{}
			},
		)
	}

	## Run a parser producing a function, then a parser producing its argument,
	## and return the function result.
	##
	## This is `apply` for pipelines: start with `const(|a| |b| ...)` and add
	## one `keep` per argument, as in the module example.
	keep : Parser(input, (a -> b)), Parser(input, a) -> Parser(input, b)
	keep = |fun_parser, val_parser| {
		build_primitive_parser(
			|input| {
				match parse_partial(fun_parser, input) {
					Err(msg) => Err(msg)
					Ok({ val: fun_val, input: rest }) => {
						match parse_partial(val_parser, rest) {
							Err(msg2) => Err(msg2)
							Ok({ val: val, input: rest2 }) => {
								Ok({ val: fun_val(val), input: rest2 })
							}
						}
					}
				}
			},
		)
	}

	## Run two parsers in sequence, discarding the second parser's value.
	##
	## Both parsers must succeed, and the input read by the second is consumed.
	## ```roc
	## at_sign : Parser(String.Utf8, [AtSign])
	## at_sign = Parser.const(AtSign).skip(String.codeunit('@'))
	##
	## expect String.parse_str(at_sign, "@") == Ok(AtSign)
	## ```
	skip : Parser(input, a), Parser(input, _) -> Parser(input, a)
	skip = |fun_parser, skip_parser| {
		build_primitive_parser(
			|input| {
				match parse_partial(fun_parser, input) {
					Err(msg) => Err(msg)
					Ok({ val: fun_val, input: rest }) => {
						match parse_partial(skip_parser, rest) {
							Err(msg2) => Err(msg2)
							Ok({ val: _, input: rest2 }) => Ok({ val: fun_val, input: rest2 })
						}
					}
				}
			},
		)
	}

	## Match zero or more codeunits until it reaches the given codeunit.
	## The given codeunit is not included in the match and is not consumed.
	## Fails if the codeunit never appears in the remaining input.
	##
	## This can be used with [Parser.skip] to ignore text.
	##
	## ```roc
	## ignore_text : Parser(String.Utf8, U64)
	## ignore_text =
	##     Parser.const(|d| d)
	##         .skip(Parser.chomp_until(':'))
	##         .skip(String.codeunit(':'))
	##         .keep(String.digits)
	##
	## expect String.parse_str(ignore_text, "ignore preceding text:123") == Ok(123)
	## ```
	##
	## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
	##
	## ```roc
	## capture_text : Parser(String.Utf8, List(U8))
	## capture_text =
	##     Parser.const(|codeunits| codeunits)
	##         .keep(Parser.chomp_until(':'))
	##         .skip(String.codeunit(':'))
	##
	## expect String.parse_str(capture_text, "Roc:") == Ok(['R', 'o', 'c'])
	## ```
	##
	## Use [String.str_from_utf8] to turn the results into a `Str`.
	##
	## Also see [Parser.chomp_while].
	chomp_until : a -> Parser(List(a), List(a)) where [a.is_eq : a, a -> Bool]
	chomp_until = |char| {
		build_primitive_parser(
			|input| {
				match input.find_first_index(
					|x| {
						x == char
					},
				) {
					Ok(index) => {
						val = input.sublist({ start: 0, len: index })
						Ok({ val, input: input.drop_first(index) })
					}
					Err(_) => Err(ParsingFailure("character not found"))
				}
			},
		)
	}

	## Match zero or more codeunits until the check returns false.
	## The codeunit that returned false is not included in the match.
	## Note: a `chomp_while` parser always succeeds, possibly consuming nothing.
	##
	## This can be used with [Parser.skip] to ignore text.
	## This is useful for chomping whitespace or variable names.
	##
	## ```roc
	## ignore_numbers : Parser(String.Utf8, Str)
	## ignore_numbers =
	##     Parser.const(|str| str)
	##         .skip(Parser.chomp_while(|b| b >= '0' and b <= '9'))
	##         .keep(String.string("TEXT"))
	##
	## expect String.parse_str(ignore_numbers, "0123456789876543210TEXT") == Ok("TEXT")
	## ```
	##
	## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
	##
	## ```roc
	## capture_numbers : Parser(String.Utf8, List(U8))
	## capture_numbers =
	##     Parser.const(|codeunits| codeunits)
	##         .keep(Parser.chomp_while(|b| b >= '0' and b <= '9'))
	##         .skip(String.string("TEXT"))
	##
	## expect String.parse_str(capture_numbers, "123TEXT") == Ok(['1', '2', '3'])
	## ```
	##
	## Use [String.str_from_utf8] to turn the results into a `Str`.
	##
	## Also see [Parser.chomp_until].
	chomp_while : (a -> Bool) -> Parser(List(a), List(a))
	chomp_while = |check| {
		build_primitive_parser(
			|input| {
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
					Ok({ val: [], input: input })
				} else {
					Ok({
						val: input.sublist({ start: 0, len: index }),
						input: input.drop_first(index),
					})
				}
			},
		)
	}
}

# Internal utility function. Not exposed to users, since usage is discouraged!
#
# Runs `first_parser` and (only) if it succeeds,
# runs the function `buildNextParser` on its result value.
# This function returns a new parser, which is finally run.
#
# `and_then` is usually more flexible than necessary, and less efficient
# than using `const` with `map` and/or `apply`.
# Consider using those functions first.
and_then : Parser(input, a), (a -> Parser(input, b)) -> Parser(input, b)
and_then = |first_parser, build_next_parser| {
	fun = |input| {
		{ val: first_val, input: rest } = Parser.parse_partial(first_parser, input)?
		next_parser = build_next_parser(first_val)

		Parser.parse_partial(next_parser, rest)
	}

	Parser.build_primitive_parser(fun)
}

many_impl : Parser(input, a), List(a), input -> Parser.ParseResult(input, List(a)) where [input.is_eq : input, input -> Bool]
many_impl = |parser, vals, input| {
	result = Parser.parse_partial(parser, input)

	match result {
		Err(_) =>
			Ok({ val: vals, input: input })

		Ok({ val: val, input: input_rest }) =>
			if input_rest == input {
				# No progress: repeating would loop forever on the same input.
				Ok({ val: vals, input: input })
			} else {
				many_impl(parser, vals.append(val), input_rest)
			}
	}
}

## Chomping until a newline returns the preceding bytes and leaves the newline.
expect {
	input = "# H\nR".to_utf8()
	result = Parser.parse_partial(Parser.chomp_until('\n'), input)?
	result == { val: ['#', ' ', 'H'], input: ['\n', 'R'] }
}

## Chomping until a missing newline reports a parse error.
expect {
	Parser.parse_partial(Parser.chomp_until('\n'), []).is_err()
}

## Chomping while a predicate holds leaves the first non-matching byte.
expect {
	input : List(U8)
	input = ['a', 's', '\n', 'd', 'f']
	not_eol = |x| {
		x != '\n'
	}
	result = Parser.parse_partial(Parser.chomp_while(not_eol), input)?
	result == { val: ['a', 's'], input: ['\n', 'd', 'f'] }
}

## Repeating a parser that succeeds without consuming input terminates.
expect {
	result = Parser.parse_partial(Parser.many(Parser.chomp_while(|b| b == 'a')), "aab".to_utf8())?
	result == { val: [['a', 'a']], input: ['b'] }
}

## Separated repetition stops when separator and element consume nothing.
expect {
	empty = Parser.chomp_while(|b| b == 'z')
	result = Parser.parse_partial(Parser.sep_by(empty, empty), "x".to_utf8())?
	result == { val: [[]], input: ['x'] }
}
