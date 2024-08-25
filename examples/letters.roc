app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    parser: "../package/main.roc",
}

import cli.Task
import cli.Stdout
import cli.Stderr
import parser.Core
import parser.String

main =

    result : Result (List Letter) [ParsingFailure Str, ParsingIncomplete Str]
    result = String.parseStr (Core.many letterParser) "AAAiBByAABBwBtCCCiAyArBBx"

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
letterParser : Core.Parser (List U8) Letter
letterParser = Core.buildPrimitiveParser \input ->

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
    parser = Core.many letterParser
    result = String.parseStr parser input
    result == Ok [B, C, Other, A]
