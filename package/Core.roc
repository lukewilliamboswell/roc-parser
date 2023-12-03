## # Parser
##
## This package implements a basic [Parser Combinator](https://en.wikipedia.org/wiki/Parser_combinator)
## for Roc which is useful for transforming input into a more useful structure.
##
## ## Example
## For example, say we wanted to parse the following string from `in` to `out`:
## ```
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
## ```
## Requirement : [ Green Nat, Red Nat, Blue Nat ]
## RequirementSet : List Requirement
## Game : { id: Nat, requirements: List RequirementSet }
##
## parseGame : Str -> Result Game [ParsingError]
## parseGame = \s ->
##     green = const Green |> keep digits |> skip (string " green")
##     red = const Red |> keep digits |> skip (string " red")
##     blue = const Blue |> keep digits |> skip (string " blue")
##
##     requirementSet : Parser _ RequirementSet
##     requirementSet = (oneOf [green, red, blue]) |> sepBy (string ", ")
##
##     requirements : Parser _ (List RequirementSet)
##     requirements = requirementSet |> sepBy (string "; ")
##
##     game : Parser _ Game
##     game =
##         const (\id -> \r -> { id, requirements: r })
##         |> skip (string "Game ")
##         |> keep digits
##         |> skip (string ": ")
##         |> keep requirements
##
##     when parseStr game s is
##         Ok g -> Ok g
##         Err (ParsingFailure _) | Err (ParsingIncomplete _) -> Err ParsingError
## ```
interface Core
    exposes [
        Parser,
        ParseResult,
        parse,
        parsePartial,
        fail,
        const,
        alt,
        apply,
        oneOf,
        map,
        map2,
        map3,
        lazy,
        maybe,
        oneOrMore,
        many,
        between,
        sepBy,
        sepBy1,
        ignore,
        buildPrimitiveParser,
        flatten,
        keep,
        skip,
        chompUntil,
        chompWhile,
    ]
    imports []

## Opaque type for a parser that will try to parse an `a` from an `input`.
##
## As such, a parser can be considered a recipe for a function of the type
## ```
## input -> Result {val: a, input: input} [ParsingFailure Str]
## ```
##
## How a parser is _actually_ implemented internally is not important
## and this might change between versions;
## for instance to improve efficiency or error messages on parsing failures.
Parser input a := input -> ParseResult input a

## ```
## ParseResult input a : Result { val : a, input : input } [ParsingFailure Str]
## ```
ParseResult input a : Result { val : a, input : input } [ParsingFailure Str]

## Write a custom parser without using provided combintors.
buildPrimitiveParser : (input -> ParseResult input a) -> Parser input a
buildPrimitiveParser = \fun -> @Parser fun

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
parsePartial : Parser input a, input -> ParseResult input a
parsePartial = \@Parser parser, input ->
    parser input

## Runs a parser on the given input, expecting it to fully consume the input
##
## The `input -> Bool` parameter is used to check whether parsing has 'completed',
## i.e. how to determine if all of the input has been consumed.
##
## For most input types, a parsing run that leaves some unparsed input behind
## should be considered an error.
parse : Parser input a, input, (input -> Bool) -> Result a [ParsingFailure Str, ParsingIncomplete input]
parse = \parser, input, isParsingCompleted ->
    when parsePartial parser input is
        Ok { val: val, input: leftover } ->
            if isParsingCompleted leftover then
                Ok val
            else
                Err (ParsingIncomplete leftover)

        Err (ParsingFailure msg) ->
            Err (ParsingFailure msg)

## Parser that can never succeed, regardless of the given input.
## It will always fail with the given error message.
##
## This is mostly useful as a 'base case' if all other parsers
## in a `oneOf` or `alt` have failed, to provide some more descriptive error message.
fail : Str -> Parser * *
fail = \msg ->
    buildPrimitiveParser \_input -> Err (ParsingFailure msg)

## Parser that will always produce the given `a`, without looking at the actual input.
## This is useful as a basic building block, especially in combination with
## `map` and `apply`.
## ```
## parseU32 : Parser (List U8) U32
## parseU32 =
##     const Num.toU32
##     |> keep digits
##
## expect parseStr parseU32 "123" == Ok 123u32
## ```
const : a -> Parser * a
const = \val ->
    buildPrimitiveParser \input ->
        Ok { val: val, input: input }

## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
alt : Parser input a, Parser input a -> Parser input a
alt = \first, second ->
    buildPrimitiveParser \input ->
        when parsePartial first input is
            Ok { val: val, input: rest } -> Ok { val: val, input: rest }
            Err (ParsingFailure firstErr) ->
                when parsePartial second input is
                    Ok { val: val, input: rest } -> Ok { val: val, input: rest }
                    Err (ParsingFailure secondErr) ->
                        Err (ParsingFailure ("\(firstErr) or \(secondErr)"))

## Runs a parser building a function, then a parser building a value,
## and finally returns the result of calling the function with the value.
##
## This is useful if you are building up a structure that requires more parameters
## than there are variants of `map`, `map2`, `map3` etc. for.
##
## For instance, the following two are the same:
## ```
## const (\x, y, z -> Triple x y z)
## |> map3 String.digits String.digits String.digits
##
## const (\x -> \y -> \z -> Triple x y z)
## |> apply String.digits
## |> apply String.digits
## |> apply String.digits
## ```
## Indeed, this is how `map`, `map2`, `map3` etc. are implemented under the hood.
##
## # Currying
## Be aware that when using `apply`, you need to explicitly 'curry' the parameters to the construction function.
## This means that instead of writing `\x, y, z -> ...`
## you'll need to write `\x -> \y -> \z -> ...`.
## This is because the parameters of the function will be applied one by one as parsing continues.
apply : Parser input (a -> b), Parser input a -> Parser input b
apply = \funParser, valParser ->
    combined = \input ->
        { val: funVal, input: rest } <- Result.try (parsePartial funParser input)
        parsePartial valParser rest
        |> Result.map \{ val: val, input: rest2 } ->
            { val: funVal val, input: rest2 }

    buildPrimitiveParser combined

# Internal utility function. Not exposed to users, since usage is discouraged!
#
# Runs `firstParser` and (only) if it succeeds,
# runs the function `buildNextParser` on its result value.
# This function returns a new parser, which is finally run.
#
# `andThen` is usually more flexible than necessary, and less efficient
# than using `const` with `map` and/or `apply`.
# Consider using those functions first.
andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \firstParser, buildNextParser ->
    fun = \input ->
        { val: firstVal, input: rest } <- Result.try (parsePartial firstParser input)
        nextParser = buildNextParser firstVal

        parsePartial nextParser rest

    buildPrimitiveParser fun

## Try a list of parsers in turn, until one of them succeeds.
## ```
## color : Parser Utf8 [Red, Green, Blue]
## color = 
##     oneOf [
##         const Red |> skip (string "red"), 
##         const Green |> skip (string "green"),
##         const Blue |> skip (string "blue"),
##     ]
## 
## expect parseStr color "green" == Ok Green
## ```
oneOf : List (Parser input a) -> Parser input a
oneOf = \parsers ->
    List.walkBackwards parsers (fail "oneOf: The list of parsers was empty") (\laterParser, earlierParser -> alt earlierParser laterParser)

## Transforms the result of parsing into something else,
## using the given transformation function.
map : Parser input a, (a -> b) -> Parser input b
map = \simpleParser, transform ->
    const transform
    |> apply simpleParser

## Transforms the result of parsing into something else,
## using the given two-parameter transformation function.
map2 : Parser input a, Parser input b, (a, b -> c) -> Parser input c
map2 = \parserA, parserB, transform ->
    const (\a -> \b -> transform a b)
    |> apply parserA
    |> apply parserB

## Transforms the result of parsing into something else,
## using the given three-parameter transformation function.
##
## If you need transformations with more inputs,
## take a look at `apply`.
map3 : Parser input a, Parser input b, Parser input c, (a, b, c -> d) -> Parser input d
map3 = \parserA, parserB, parserC, transform ->
    const (\a -> \b -> \c -> transform a b c)
    |> apply parserA
    |> apply parserB
    |> apply parserC

# ^ And this could be repeated for as high as we want, of course.
# Removes a layer of 'result' from running the parser.
#
# This allows for instance to map functions that return a result over the parser,
# where errors are turned into `ParsingFailure` s.
flatten : Parser input (Result a Str) -> Parser input a
flatten = \parser ->
    buildPrimitiveParser \input ->
        result = parsePartial parser input

        when result is
            Err problem ->
                Err problem

            Ok { val: Ok val, input: inputRest } ->
                Ok { val: val, input: inputRest }

            Ok { val: Err problem, input: _inputRest } ->
                Err (ParsingFailure problem)

## Runs a parser lazily
##
## This is (only) useful when dealing with a recursive structure.
## For instance, consider a type `Comment : { message: String, responses: List Comment }`.
## Without `lazy`, you would ask the compiler to build an infinitely deep parser.
## (Resulting in a compiler error.)
##
lazy : ({} -> Parser input a) -> Parser input a
lazy = \thunk ->
    const {}
    |> andThen thunk

maybe : Parser input a -> Parser input (Result a [Nothing])
maybe = \parser ->
    alt (parser |> map (\val -> Ok val)) (const (Err Nothing))

manyImpl : Parser input a, List a, input -> ParseResult input (List a)
manyImpl = \parser, vals, input ->
    result = parsePartial parser input

    when result is
        Err _ ->
            Ok { val: vals, input: input }

        Ok { val: val, input: inputRest } ->
            manyImpl parser (List.append vals val) inputRest

## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser input a -> Parser input (List a)
many = \parser ->
    buildPrimitiveParser \input ->
        manyImpl parser [] input

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser input a -> Parser input (List a)
oneOrMore = \parser ->
    const (\val -> \vals -> List.prepend vals val)
    |> apply parser
    |> apply (many parser)

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## ```
## betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
## ```
between : Parser input a, Parser input open, Parser input close -> Parser input a
between = \parser, open, close ->
    const (\_ -> \val -> \_ -> val)
    |> apply open
    |> apply parser
    |> apply close

sepBy1 : Parser input a, Parser input sep -> Parser input (List a)
sepBy1 = \parser, separator ->
    parserFollowedBySep =
        const (\_ -> \val -> val)
        |> apply separator
        |> apply parser

    const (\val -> \vals -> List.prepend vals val)
    |> apply parser
    |> apply (many parserFollowedBySep)

## ```
## parseNumbers : Parser (List U8) (List Nat)
## parseNumbers =
##     digits |> sepBy (codeunit ',')
##
## expect parseStr parseNumbers "1,2,3" == Ok [1,2,3]
## ```
sepBy : Parser input a, Parser input sep -> Parser input (List a)
sepBy = \parser, separator ->
    alt (sepBy1 parser separator) (const [])

ignore : Parser input a -> Parser input {}
ignore = \parser ->
    map parser (\_ -> {})

keep : Parser input (a -> b), Parser input a -> Parser input b
keep = \funParser, valParser ->
    buildPrimitiveParser \input ->
        when parsePartial funParser input is
            Err msg -> Err msg
            Ok { val: funVal, input: rest } ->
                when parsePartial valParser rest is
                    Err msg2 -> Err msg2
                    Ok { val: val, input: rest2 } ->
                        Ok { val: funVal val, input: rest2 }

skip : Parser input a, Parser input * -> Parser input a
skip = \funParser, skipParser ->
    buildPrimitiveParser \input ->
        when parsePartial funParser input is
            Err msg -> Err msg
            Ok { val: funVal, input: rest } ->
                when parsePartial skipParser rest is
                    Err msg2 -> Err msg2
                    Ok { val: _, input: rest2 } -> Ok { val: funVal, input: rest2 }
## ```
## ignoreText : Parser (List U8) Nat
## ignoreText =
##     const (\d -> d)
##     |> skip (chompUntil ':')
##     |> skip (codeunit ':')
##     |> keep digits
##
## expect parseStr ignoreText "ignore preceding text:123" == Ok 123
## ```
chompUntil : a -> Parser (List a) (List a) where a implements Eq
chompUntil = \char ->
    buildPrimitiveParser \input ->
        when List.findFirstIndex input (\x -> Bool.isEq x char) is
            Ok index ->
                val = List.sublist input { start: 0, len: index }
                Ok { val, input: List.dropFirst input index }

            Err _ -> Err (ParsingFailure "character not found")

expect
    input = "# H\nR" |> Str.toUtf8
    result = parsePartial (chompUntil '\n') input
    result == Ok { val: ['#', ' ', 'H'], input: ['\n', 'R'] }

expect
    when parsePartial (chompUntil '\n') [] is
        Ok _ -> Bool.false
        Err (ParsingFailure _) -> Bool.true

# Chomp zero or more characters if they pass the test. This is useful for chomping
# whitespace or variable names. Note: a chompWhile parser always succeeds!
## ```
## ignoreNumbers : Parser (List U8) Str
## ignoreNumbers =
##     const (\str -> str)
##     |> skip (chompWhile \b -> b >= '0' && b <= '9')
##     |> keep (string "TEXT")
##
## expect parseStr ignoreNumbers "0123456789876543210TEXT" == Ok "TEXT"
## ```
chompWhile : (a -> Bool) -> Parser (List a) (List a) where a implements Eq
chompWhile = \check ->
    buildPrimitiveParser \input ->
        index : Nat
        index = List.walkUntil input 0 \i, elem ->
            if check elem then
                Continue (i + 1)
            else
                Break i

        if index == 0 then
            Ok { val: [], input: input }
        else
            Ok {
                val: List.sublist input { start: 0, len: index },
                input: List.dropFirst input index,
            }

expect
    input = [97u8, 's', '\n', 'd', 'f']
    notEOL = \x -> Bool.isNotEq x '\n'
    result = parsePartial (chompWhile notEOL) input
    result == Ok { val: [97u8, 's'], input: ['\n', 'd', 'f'] }
