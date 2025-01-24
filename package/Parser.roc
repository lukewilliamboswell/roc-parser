## # Parser
##
## This package implements a basic [Parser Combinator](https://en.wikipedia.org/wiki/Parser_combinator)
## for Roc which is useful for transforming input into a more useful structure.
##
## ## Example
## For example, say we wanted to parse the following string from `in` to `out`:
## ```roc
## in = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
## out =
##     {
##         id: 1,
##         requirements: [
##             [Blue 3, Red 4],
##             [Red 1, Green 2, Blue 6],
##             [Green 2],
##         ]
##     }
## ```
## We could do this using the following:
## ```roc
## Requirement : [Green U64, Red U64, Blue U64]
## RequirementSet : List Requirement
## Game : { id : U64, requirements : List RequirementSet }
##
## parse_game : Str -> Result Game [ParsingError]
## parse_game = \s ->
##     green = const(Green) |> keep(digits) |> skip(string(" green"))
##     red = const(Red) |> keep(digits) |> skip(string(" red"))
##     blue = const(Blue) |> keep(digits) |> skip(string(" blue"))
##
##     requirement_set : Parser _ RequirementSet
##     requirement_set = one_of([green, red, blue]) |> sep_by(string(", "))
##
##     requirements : Parser _ (List RequirementSet)
##     requirements = requirement_set |> sep_by(string("; "))
##
##     game : Parser _ Game
##     game =
##         const(\id -> \r -> { id, requirements: r })
##         |> skip(string("Game "))
##         |> keep(digits)
##         |> skip(string(": "))
##         |> keep(requirements)
##
##     when parse_str(game, s) is
##         Ok(g) -> Ok(g)
##         Err(ParsingFailure(_)) | Err(ParsingIncomplete(_)) -> Err(ParsingError)
## ```
module [
    Parser,
    ParseResult,
    parse,
    parse_partial,
    fail,
    const,
    alt,
    apply,
    one_of,
    map,
    map2,
    map3,
    lazy,
    maybe,
    one_or_more,
    many,
    between,
    sep_by,
    sep_by1,
    ignore,
    build_primitive_parser,
    flatten,
    keep,
    skip,
    chomp_until,
    chomp_while,
]

## Opaque type for a parser that will try to parse an `a` from an `input`.
##
## As such, a parser can be considered a recipe for a function of the type
## ```roc
## input -> Result {val: a, input: input} [ParsingFailure Str]
## ```
##
## How a parser is _actually_ implemented internally is not important
## and this might change between versions;
## for instance to improve efficiency or error messages on parsing failures.
Parser input a := input -> ParseResult input a

## ```roc
## ParseResult input a : Result { val : a, input : input } [ParsingFailure Str]
## ```
ParseResult input a : Result { val : a, input : input } [ParsingFailure Str]

## Write a custom parser without using provided combintors.
build_primitive_parser : (input -> ParseResult input a) -> Parser input a
build_primitive_parser = |fun| @Parser(fun)

## Most general way of running a parser.
##
## Can be thought of as turning the recipe of a parser into its actual parsing function
## and running this function on the given input.
##
## Moat parsers consume part of `input` when they succeed. This allows you to string parsers
## together that run one after the other. The part of the input that the first
## parser did not consume, is used by the next parser.
## This is why a parser returns on success both the resulting value and the leftover part of the input.
##
## This is mostly useful when creating your own internal parsing building blocks.
parse_partial : Parser input a, input -> ParseResult input a
parse_partial = |@Parser(parser), input|
    parser(input)

## Runs a parser on the given input, expecting it to fully consume the input
##
## The `input -> Bool` parameter is used to check whether parsing has 'completed',
## i.e. how to determine if all of the input has been consumed.
##
## For most input types, a parsing run that leaves some unparsed input behind
## should be considered an error.
parse : Parser input a, input, (input -> Bool) -> Result a [ParsingFailure Str, ParsingIncomplete input]
parse = |parser, input, is_parsing_completed|
    when parse_partial(parser, input) is
        Ok({ val: val, input: leftover }) ->
            if is_parsing_completed(leftover) then
                Ok(val)
            else
                Err(ParsingIncomplete(leftover))

        Err(ParsingFailure(msg)) ->
            Err(ParsingFailure(msg))

## Parser that can never succeed, regardless of the given input.
## It will always fail with the given error message.
##
## This is mostly useful as a 'base case' if all other parsers
## in a `oneOf` or `alt` have failed, to provide some more descriptive error message.
fail : Str -> Parser * *
fail = |msg|
    build_primitive_parser(|_input| Err(ParsingFailure(msg)))

## Parser that will always produce the given `a`, without looking at the actual input.
## This is useful as a basic building block, especially in combination with
## `map` and `apply`.
## ```roc
## parse_u32 : Parser (List U8) U32
## parse_u32 =
##     const(Num.to_u32)
##     |> keep(digits)
##
## expect parse_str(parse_u32, "123") == Ok(123u32)
## ```
const : a -> Parser * a
const = |val|
    build_primitive_parser(
        |input|
            Ok({ val: val, input: input }),
    )

## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
alt : Parser input a, Parser input a -> Parser input a
alt = |first, second|
    build_primitive_parser(
        |input|
            when parse_partial(first, input) is
                Ok({ val: val, input: rest }) -> Ok({ val: val, input: rest })
                Err(ParsingFailure(first_err)) ->
                    when parse_partial(second, input) is
                        Ok({ val: val, input: rest }) -> Ok({ val: val, input: rest })
                        Err(ParsingFailure(second_err)) ->
                            Err(ParsingFailure("${first_err} or ${second_err}")),
    )

## Runs a parser building a function, then a parser building a value,
## and finally returns the result of calling the function with the value.
##
## This is useful if you are building up a structure that requires more parameters
## than there are variants of `map`, `map2`, `map3` etc. for.
##
## For instance, the following two are the same:
## ```roc
## const(\x, y, z -> Triple(x, y, z))
## |> map3(String.digits, String.digits, String.digits)
##
## const(\x -> \y -> \z -> Triple(x, y, z))
## |> apply(String.digits)
## |> apply(String.digits)
## |> apply(String.digits)
## ```
## Indeed, this is how `map`, `map2`, `map3` etc. are implemented under the hood.
##
## # Currying
## Be aware that when using `apply`, you need to explicitly 'curry' the parameters to the construction function.
## This means that instead of writing `\x, y, z -> ...`
## you'll need to write `\x -> \y -> \z -> ...`.
## This is because the parameters of the function will be applied one by one as parsing continues.
apply : Parser input (a -> b), Parser input a -> Parser input b
apply = |fun_parser, val_parser|
    combined = |input|
        { val: fun_val, input: rest } = parse_partial(fun_parser, input)?
        parse_partial(val_parser, rest)
        |> Result.map_ok(
            |{ val: val, input: rest2 }|
                { val: fun_val(val), input: rest2 },
        )

    build_primitive_parser(combined)

# Internal utility function. Not exposed to users, since usage is discouraged!
#
# Runs `first_parser` and (only) if it succeeds,
# runs the function `buildNextParser` on its result value.
# This function returns a new parser, which is finally run.
#
# `and_then` is usually more flexible than necessary, and less efficient
# than using `const` with `map` and/or `apply`.
# Consider using those functions first.
and_then : Parser input a, (a -> Parser input b) -> Parser input b
and_then = |first_parser, build_next_parser|
    fun = |input|
        { val: first_val, input: rest } = parse_partial(first_parser, input)?
        next_parser = build_next_parser(first_val)

        parse_partial(next_parser, rest)

    build_primitive_parser(fun)

## Try a list of parsers in turn, until one of them succeeds.
## ```roc
## color : Parser Utf8 [Red, Green, Blue]
## color =
##     one_of(
##         [
##             const(Red) |> skip(string("red")),
##             const(Green) |> skip(string("green")),
##             const(Blue) |> skip(string("blue")),
##         ],
##     )
##
## expect parse_str(color, "green") == Ok(Green)
## ```
one_of : List (Parser input a) -> Parser input a
one_of = |parsers|
    List.walk_backwards(parsers, fail("oneOf: The list of parsers was empty"), |later_parser, earlier_parser| alt(earlier_parser, later_parser))

## Transforms the result of parsing into something else,
## using the given transformation function.
map : Parser input a, (a -> b) -> Parser input b
map = |simple_parser, transform|
    const(transform)
    |> apply(simple_parser)

## Transforms the result of parsing into something else,
## using the given two-parameter transformation function.
map2 : Parser input a, Parser input b, (a, b -> c) -> Parser input c
map2 = |parser_a, parser_b, transform|
    const(|a| |b| transform(a, b))
    |> apply(parser_a)
    |> apply(parser_b)

## Transforms the result of parsing into something else,
## using the given three-parameter transformation function.
##
## If you need transformations with more inputs,
## take a look at `apply`.
map3 : Parser input a, Parser input b, Parser input c, (a, b, c -> d) -> Parser input d
map3 = |parser_a, parser_b, parser_c, transform|
    const(|a| |b| |c| transform(a, b, c))
    |> apply(parser_a)
    |> apply(parser_b)
    |> apply(parser_c)

## Removes a layer of `Result` from running the parser.
##
## Use this to map functions that return a result over the parser,
## where errors are turned into `ParsingFailure`s.
##
## ```roc
## # Parse a number from a List U8
## u64 : Parser Utf8 U64
## u64 =
##     string
##     |> map(
##         \val ->
##             when Str.to_u64(val) is
##                 Ok(num) -> Ok(num)
##                 Err(_) -> Err("${val} is not a U64."),
##     )
##     |> flatten
## ```
flatten : Parser input (Result a Str) -> Parser input a
flatten = |parser|
    build_primitive_parser(
        |input|
            result = parse_partial(parser, input)

            when result is
                Err(problem) -> Err(problem)
                Ok({ val: Ok(val), input: input_rest }) -> Ok({ val: val, input: input_rest })
                Ok({ val: Err(problem), input: _inputRest }) -> Err(ParsingFailure(problem)),
    )

## Runs a parser lazily
##
## This is (only) useful when dealing with a recursive structure.
## For instance, consider a type `Comment : { message: String, responses: List Comment }`.
## Without `lazy`, you would ask the compiler to build an infinitely deep parser.
## (Resulting in a compiler error.)
##
lazy : ({} -> Parser input a) -> Parser input a
lazy = |thunk|
    const({})
    |> and_then(thunk)

maybe : Parser input a -> Parser input (Result a [Nothing])
maybe = |parser|
    alt((parser |> map(|val| Ok(val))), const(Err(Nothing)))

many_impl : Parser input a, List a, input -> ParseResult input (List a)
many_impl = |parser, vals, input|
    result = parse_partial(parser, input)

    when result is
        Err(_) ->
            Ok({ val: vals, input: input })

        Ok({ val: val, input: input_rest }) ->
            many_impl(parser, List.append(vals, val), input_rest)

## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
many : Parser input a -> Parser input (List a)
many = |parser|
    build_primitive_parser(
        |input|
            many_impl(parser, [], input),
    )

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see [Parser.many].
one_or_more : Parser input a -> Parser input (List a)
one_or_more = |parser|
    const(|val| |vals| List.prepend(vals, val))
    |> apply(parser)
    |> apply(many(parser))

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## ```roc
## between_braces = \parser -> parser |> between(scalar('['), scalar(']'))
## ```
between : Parser input a, Parser input open, Parser input close -> Parser input a
between = |parser, open, close|
    const(|_| |val| |_| val)
    |> apply(open)
    |> apply(parser)
    |> apply(close)

sep_by1 : Parser input a, Parser input sep -> Parser input (List a)
sep_by1 = |parser, separator|
    parser_followed_by_sep =
        const(|_| |val| val)
        |> apply(separator)
        |> apply(parser)

    const(|val| |vals| List.prepend(vals, val))
    |> apply(parser)
    |> apply(many(parser_followed_by_sep))

## ```roc
## parse_numbers : Parser (List U8) (List U64)
## parse_numbers =
##     digits |> sep_by(codeunit(','))
##
## expect parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])
## ```
sep_by : Parser input a, Parser input sep -> Parser input (List a)
sep_by = |parser, separator|
    alt(sep_by1(parser, separator), const([]))

ignore : Parser input a -> Parser input {}
ignore = |parser|
    map(parser, |_| {})

keep : Parser input (a -> b), Parser input a -> Parser input b
keep = |fun_parser, val_parser|
    build_primitive_parser(
        |input|
            when parse_partial(fun_parser, input) is
                Err(msg) -> Err(msg)
                Ok({ val: fun_val, input: rest }) ->
                    when parse_partial(val_parser, rest) is
                        Err(msg2) -> Err(msg2)
                        Ok({ val: val, input: rest2 }) ->
                            Ok({ val: fun_val(val), input: rest2 }),
    )

skip : Parser input a, Parser input * -> Parser input a
skip = |fun_parser, skip_parser|
    build_primitive_parser(
        |input|
            when parse_partial(fun_parser, input) is
                Err(msg) -> Err(msg)
                Ok({ val: fun_val, input: rest }) ->
                    when parse_partial(skip_parser, rest) is
                        Err(msg2) -> Err(msg2)
                        Ok({ val: _, input: rest2 }) -> Ok({ val: fun_val, input: rest2 }),
    )

## Match zero or more codeunits until the it reaches the given codeunit.
## The given codeunit is not included in the match.
##
## This can be used with [Parser.skip] to ignore text.
##
## ```roc
## ignore_text : Parser (List U8) U64
## ignore_text =
##     const(\d -> d)
##     |> skip(chomp_until(':'))
##     |> skip(codeunit(':'))
##     |> keep(digits)
##
## expect parse_str(ignore_text, "ignore preceding text:123") == Ok(123)
## ```
##
## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
##
## ```roc
## capture_text : Parser (List U8) (List U8)
## capture_text =
##     const(\codeunits -> codeunits)
##     |> keep(chomp_until(':'))
##     |> skip(codeunit(':'))
##
## expect parse_str(capture_text, "Roc:") == Ok(['R', 'o', 'c'])
## ```
##
## Use [String.str_from_utf8] to turn the results into a `Str`.
##
## Also see [Parser.chomp_while].
chomp_until : a -> Parser (List a) (List a) where a implements Eq
chomp_until = |char|
    build_primitive_parser(
        |input|
            when List.find_first_index(input, |x| Bool.is_eq(x, char)) is
                Ok(index) ->
                    val = List.sublist(input, { start: 0, len: index })
                    Ok({ val, input: List.drop_first(input, index) })

                Err(_) -> Err(ParsingFailure("character not found")),
    )

expect
    input = "# H\nR" |> Str.to_utf8
    result = parse_partial(chomp_until('\n'), input)
    result == Ok({ val: ['#', ' ', 'H'], input: ['\n', 'R'] })

expect
    when parse_partial(chomp_until('\n'), []) is
        Ok(_) -> Bool.false
        Err(ParsingFailure(_)) -> Bool.true

## Match zero or more codeunits until the check returns false.
## The codeunit that returned false is not included in the match.
## Note: a chompWhile parser always succeeds!
##
## This can be used with [Parser.skip] to ignore text.
## This is useful for chomping whitespace or variable names.
##
## ```
## ignore_numbers : Parser (List U8) Str
## ignore_numbers =
##     const(\str -> str)
##     |> skip(chomp_while(\b -> b >= '0' && b <= '9'))
##     |> keep(string("TEXT"))
##
## expect parse_str(ignore_numbers, "0123456789876543210TEXT") == Ok("TEXT")
## ```
##
## This can be used with [Parser.keep] to capture a list of `U8` codeunits.
##
## ```
## capture_numbers : Parser (List U8) (List U8)
## capture_numbers =
##     const(\codeunits -> codeunits)
##     |> keep(chomp_while(\b -> b >= '0' && b <= '9'))
##     |> skip(string("TEXT"))
##
## expect parse_str(capture_numbers, "123TEXT") == Ok(['1', '2', '3'])
## ```
##
## Use [String.str_from_utf8] to turn the results into a `Str`.
##
## Also see [Parser.chomp_until].
chomp_while : (a -> Bool) -> Parser (List a) (List a) where a implements Eq
chomp_while = |check|
    build_primitive_parser(
        |input|
            index = List.walk_until(
                input,
                0,
                |i, elem|
                    if check(elem) then
                        Continue((i + 1))
                    else
                        Break(i),
            )

            if index == 0 then
                Ok({ val: [], input: input })
            else
                Ok(
                    {
                        val: List.sublist(input, { start: 0, len: index }),
                        input: List.drop_first(input, index),
                    },
                ),
    )

expect
    input = [97u8, 's', '\n', 'd', 'f']
    not_eol = |x| Bool.is_not_eq(x, '\n')
    result = parse_partial(chomp_while(not_eol), input)
    result == Ok({ val: [97u8, 's'], input: ['\n', 'd', 'f'] })
