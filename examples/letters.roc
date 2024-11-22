app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    parser: "../package/main.roc",
}

import cli.Stdout
import cli.Stderr
import parser.Parser
import parser.String

main =

    result : Result (List Letter) [ParsingFailure Str, ParsingIncomplete Str]
    result = String.parseStr (Parser.many letterParser) "AAAiBByAABBwBtCCCiAyArBBx"

    when result |> Result.map countLetterAs is
        Ok count -> Stdout.line "I counted $(Num.toStr count) letter A's!"
        Err _ -> Stderr.line "Failed while parsing input"

Letter : [A, B, C, Other]

# Helper to check if a letter is an A tag
isA = \l -> l == A

# Count the number of Letter A's
countLetterAs : List Letter -> U64
countLetterAs = \letters ->
    letters
    |> List.keepIf isA
    |> List.map \_ -> 1
    |> List.sum

# Build a custom parser to convert utf8 input into Letter tags
letterParser : Parser.Parser (List U8) Letter
letterParser = Parser.buildPrimitiveParser \input ->

    valResult =
        when input is
            [] -> Err (ParsingFailure "Nothing to parse")
            ['A', ..] -> Ok A
            ['B', ..] -> Ok B
            ['C', ..] -> Ok C
            _ -> Ok Other

    valResult
    |> Result.map \val -> { val, input: List.dropFirst input 1 }

# Test we can parse a single B letter
expect
    input = "B"
    parser = letterParser
    result = String.parseStr parser input
    result == Ok B

# Test we can parse a number of different letters
expect
    input = "BCXA"
    parser = Parser.many letterParser
    result = String.parseStr parser input
    result == Ok [B, C, Other, A]
