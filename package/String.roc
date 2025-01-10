module [
    Utf8,
    parse_str,
    parse_str_partial,
    parse_utf8,
    parse_utf8_partial,
    string,
    utf8,
    codeunit,
    codeunit_satisfies,
    any_string,
    any_thing,
    any_codeunit,
    one_of,
    digit,
    digits,
    str_from_utf8,
    str_from_ascii,
]

import Parser exposing [Parser]

## ```
## Utf8 : List U8
## ```
Utf8 : List U8

## Parse a [Str] using a [Parser]
## ```
## color : Parser Utf8 [Red, Green, Blue]
## color =
##     oneOf [
##         Parser.const Red |> Parser.skip (string "red"),
##         Parser.const Green |> Parser.skip (string "green"),
##         Parser.const Blue |> Parser.skip (string "blue"),
##     ]
##
## expect parseStr color "green" == Ok Green
## ```
parse_str : Parser Utf8 a, Str -> Result a [ParsingFailure Str, ParsingIncomplete Str]
parse_str = \parser, input ->
    parser
    |> parse_utf8(str_to_raw(input))
    |> Result.map_err(
        \problem ->
            when problem is
                ParsingFailure(msg) -> ParsingFailure(msg)
                ParsingIncomplete(leftover_raw) -> ParsingIncomplete(str_from_utf8(leftover_raw)),
    )

## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
##
##
## ```
## atSign : Parser Utf8 [AtSign]
## atSign = Parser.const AtSign |> Parser.skip (codeunit '@')
##
## expect parseStr atSign "@" == Ok AtSign
## expect parseStrPartial atSign "@" |> Result.map .val == Ok AtSign
## expect parseStrPartial atSign "$" |> Result.isErr
## ```
parse_str_partial : Parser Utf8 a, Str -> Parser.ParseResult Str a
parse_str_partial = \parser, input ->
    parser
    |> parse_utf8_partial(str_to_raw(input))
    |> Result.map(
        \{ val: val, input: rest_raw } ->
            { val: val, input: str_from_utf8(rest_raw) },
    )

## Runs a parser against a string, requiring the parser to consume it fully.
##
## - If the parser succeeds, returns `Ok a`
## - If the parser fails, returns `Err (ParsingFailure Str)`
## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete (List U8))`
parse_utf8 : Parser Utf8 a, Utf8 -> Result a [ParsingFailure Str, ParsingIncomplete Utf8]
parse_utf8 = \parser, input ->
    Parser.parse(parser, input, \leftover -> List.len(leftover) == 0)

## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
parse_utf8_partial : Parser Utf8 a, Utf8 -> Parser.ParseResult Utf8 a
parse_utf8_partial = \parser, input ->
    Parser.parse_partial(parser, input)

## ```
## isDigit : U8 -> Bool
## isDigit = \b -> b >= '0' && b <= '9'
##
## expect parseStr (codeunitSatisfies isDigit) "0" == Ok '0'
## expect parseStr (codeunitSatisfies isDigit) "*" |> Result.isErr
## ```
codeunit_satisfies : (U8 -> Bool) -> Parser Utf8 U8
codeunit_satisfies = \check ->
    Parser.build_primitive_parser(
        \input ->
            { before: start, others: input_rest } = List.split_at(input, 1)

            when List.get(start, 0) is
                Err(OutOfBounds) ->
                    Err(ParsingFailure("expected a codeunit satisfying a condition, but input was empty."))

                Ok(start_codeunit) ->
                    if check(start_codeunit) then
                        Ok({ val: start_codeunit, input: input_rest })
                    else
                        other_char = str_from_codeunit(start_codeunit)
                        input_str = str_from_utf8(input)

                        Err(ParsingFailure("expected a codeunit satisfying a condition but found `$(other_char)`.\n While reading: `$(input_str)`")),
    )

## ```
## atSign : Parser Utf8 [AtSign]
## atSign = Parser.const AtSign |> Parser.skip (codeunit '@')
##
## expect parseStr atSign "@" == Ok AtSign
## expect parseStrPartial atSign "$" |> Result.isErr
## ```
codeunit : U8 -> Parser Utf8 U8
codeunit = \expected_code_unit ->
    Parser.build_primitive_parser(
        \input ->
            when input is
                [] ->
                    Err(ParsingFailure("expected char `$(str_from_codeunit(expected_code_unit))` but input was empty."))

                [first, .. as rest] if first == expected_code_unit ->
                    Ok({ val: expected_code_unit, input: rest })

                [first, ..] ->
                    Err(ParsingFailure("expected char `$(str_from_codeunit(expected_code_unit))` but found `$(str_from_codeunit(first))`.\n While reading: `$(str_from_utf8(input))`")),
    )

## Parse an extact sequence of utf8
utf8 : List U8 -> Parser Utf8 (List U8)
utf8 = \expected_string ->
    # Implemented manually instead of a sequence of codeunits
    # because of efficiency and better error messages
    Parser.build_primitive_parser(
        \input ->
            { before: start, others: input_rest } = List.split_at(input, List.len(expected_string))

            if start == expected_string then
                Ok({ val: expected_string, input: input_rest })
            else
                error_string = str_from_utf8(expected_string)
                other_string = str_from_utf8(start)
                input_string = str_from_utf8(input)

                Err(ParsingFailure("expected string `$(error_string)` but found `$(other_string)`.\nWhile reading: $(input_string)")),
    )

## Parse the given [Str]
## ```
## expect parseStr (string "Foo") "Foo" == Ok "Foo"
## expect parseStr (string "Foo") "Bar" |> Result.isErr
## ```
string : Str -> Parser Utf8 Str
string = \expected_string ->
    str_to_raw(expected_string)
    |> utf8
    |> Parser.map(\_val -> expected_string)

## Matches any [U8] codeunit
## ```
## expect parseStr anyCodeunit "a" == Ok 'a'
## expect parseStr anyCodeunit "$" == Ok '$'
## ```
any_codeunit : Parser Utf8 U8
any_codeunit = codeunit_satisfies(\_ -> Bool.true)

expect parse_str(any_codeunit, "a") == Ok('a')
expect parse_str(any_codeunit, "\$") == Ok(36)

## Matches any [Utf8] and consumes all the input without fail.
## ```
## expect
##     bytes = Str.toUtf8 "consumes all the input"
##     Parser.parse anyThing bytes List.isEmpty == Ok bytes
## ```
any_thing : Parser Utf8 Utf8
any_thing = Parser.build_primitive_parser(\input -> Ok({ val: input, input: [] }))

expect
    bytes = Str.to_utf8("consumes all the input")
    Parser.parse(any_thing, bytes, List.is_empty) == Ok(bytes)

# Matches any string
# as long as it is valid UTF8.
any_string : Parser Utf8 Str
any_string = Parser.build_primitive_parser(
    \field_utf8ing ->
        when Str.from_utf8(field_utf8ing) is
            Ok(string_val) ->
                Ok({ val: string_val, input: [] })

            Err(BadUtf8(_)) ->
                Err(ParsingFailure("Expected a string field, but its contents cannot be parsed as UTF8.")),
)

## ```
## expect parseStr digit "0" == Ok 0
## expect parseStr digit "not a digit" |> Result.isErr
## ```
digit : Parser Utf8 U64
digit =
    Parser.build_primitive_parser(
        \input ->
            when input is
                [] ->
                    Err(ParsingFailure("Expected a digit from 0-9 but input was empty."))

                [first, .. as rest] if first >= '0' && first <= '9' ->
                    Ok({ val: Num.to_u64((first - '0')), input: rest })

                _ ->
                    Err(ParsingFailure("Not a digit")),
    )

## Parse a sequence of digits into a [U64], accepting leading zeroes
## ```
## expect parseStr digits "0123" == Ok 123
## expect parseStr digits "not a digit" |> Result.isErr
## ```
digits : Parser Utf8 U64
digits =
    Parser.one_or_more(digit)
    |> Parser.map(\ds -> List.walk(ds, 0, \sum, d -> sum * 10 + d))

## Try a bunch of different parsers.
##
## The first parser which is tried is the one at the front of the list,
## and the next one is tried until one succeeds or the end of the list was reached.
## ```
## boolParser : Parser Utf8 Bool
## boolParser =
##     oneOf [string "true", string "false"]
##     |> Parser.map (\x -> if x == "true" then Bool.true else Bool.false)
##
## expect parseStr boolParser "true" == Ok Bool.true
## expect parseStr boolParser "false" == Ok Bool.false
## expect parseStr boolParser "not a bool" |> Result.isErr
## ```
one_of : List (Parser Utf8 a) -> Parser Utf8 a
one_of = \parsers ->
    Parser.build_primitive_parser(
        \input ->
            List.walk_until(
                parsers,
                Err(ParsingFailure("(no possibilities)")),
                \_, parser ->
                    when parse_utf8_partial(parser, input) is
                        Ok(val) ->
                            Break(Ok(val))

                        Err(problem) ->
                            Continue(Err(problem)),
            ),
    )

str_from_utf8 : Utf8 -> Str
str_from_utf8 = \raw_str ->
    raw_str
    |> Str.from_utf8
    |> Result.with_default("Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!")

str_to_raw : Str -> Utf8
str_to_raw = \str ->
    str |> Str.to_utf8

str_from_codeunit : U8 -> Str
str_from_codeunit = \cu ->
    str_from_utf8([cu])

str_from_ascii : U8 -> Str
str_from_ascii = \ascii_num ->
    when Str.from_utf8([ascii_num]) is
        Ok(answer) -> answer
        Err(_) -> crash("The number $(Num.to_str(ascii_num)) is not a valid ASCII constant!")

# -------------------- example snippets used in docs --------------------

parse_u32 : Parser Utf8 U32
parse_u32 =
    Parser.const(Num.to_u32)
    |> Parser.keep(digits)

expect parse_str(parse_u32, "123") == Ok(123u32)

color : Parser Utf8 [Red, Green, Blue]
color =
    one_of(
        [
            Parser.const(Red) |> Parser.skip(string("red")),
            Parser.const(Green) |> Parser.skip(string("green")),
            Parser.const(Blue) |> Parser.skip(string("blue")),
        ],
    )

expect parse_str(color, "green") == Ok(Green)

parse_numbers : Parser Utf8 (List U64)
parse_numbers =
    digits |> Parser.sep_by(codeunit(','))

expect parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])

expect parse_str(string("Foo"), "Foo") == Ok("Foo")
expect parse_str(string("Foo"), "Bar") |> Result.is_err

ignore_text : Parser Utf8 U64
ignore_text =
    Parser.const(\d -> d)
    |> Parser.skip(Parser.chomp_until(':'))
    |> Parser.skip(codeunit(':'))
    |> Parser.keep(digits)

expect parse_str(ignore_text, "ignore preceding text:123") == Ok(123)

ignore_numbers : Parser Utf8 Str
ignore_numbers =
    Parser.const(\str -> str)
    |> Parser.skip(Parser.chomp_while(\b -> b >= '0' && b <= '9'))
    |> Parser.keep(string("TEXT"))

expect parse_str(ignore_numbers, "0123456789876543210TEXT") == Ok("TEXT")

is_digit : U8 -> Bool
is_digit = \b -> b >= '0' && b <= '9'

expect parse_str(codeunit_satisfies(is_digit), "0") == Ok('0')
expect parse_str(codeunit_satisfies(is_digit), "*") |> Result.is_err

at_sign : Parser Utf8 [AtSign]
at_sign = Parser.const(AtSign) |> Parser.skip(codeunit('@'))

expect parse_str(at_sign, "@") == Ok(AtSign)
expect parse_str_partial(at_sign, "@") |> Result.map(.val) == Ok(AtSign)
expect parse_str_partial(at_sign, "\$") |> Result.is_err

Requirement : [Green U64, Red U64, Blue U64]
RequirementSet : List Requirement
Game : { id : U64, requirements : List RequirementSet }

parse_game : Str -> Result Game [ParsingError]
parse_game = \s ->
    green = Parser.const(Green) |> Parser.keep(digits) |> Parser.skip(string(" green"))
    red = Parser.const(Red) |> Parser.keep(digits) |> Parser.skip(string(" red"))
    blue = Parser.const(Blue) |> Parser.keep(digits) |> Parser.skip(string(" blue"))

    requirement_set : Parser _ RequirementSet
    requirement_set = (one_of([green, red, blue])) |> Parser.sep_by(string(", "))

    requirements : Parser _ (List RequirementSet)
    requirements = requirement_set |> Parser.sep_by(string("; "))

    game : Parser _ Game
    game =
        Parser.const(\id -> \r -> { id, requirements: r })
        |> Parser.skip(string("Game "))
        |> Parser.keep(digits)
        |> Parser.skip(string(": "))
        |> Parser.keep(requirements)

    when parse_str(game, s) is
        Ok(g) -> Ok(g)
        Err(ParsingFailure(_)) | Err(ParsingIncomplete(_)) -> Err(ParsingError)

expect
    parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    == Ok(
        {
            id: 1,
            requirements: [
                [Blue(3), Red(4)],
                [Red(1), Green(2), Blue(6)],
                [Green(2)],
            ],
        },
    )

expect parse_str(digit, "0") == Ok(0)
expect parse_str(digit, "not a digit") |> Result.is_err

expect parse_str(digits, "0123") == Ok(123)
expect parse_str(digits, "not a digit") |> Result.is_err

bool_parser : Parser Utf8 Bool
bool_parser =
    one_of([string("true"), string("false")])
    |> Parser.map(\x -> if x == "true" then Bool.true else Bool.false)

expect parse_str(bool_parser, "true") == Ok(Bool.true)
expect parse_str(bool_parser, "false") == Ok(Bool.false)
expect parse_str(bool_parser, "not a bool") |> Result.is_err
