app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
        parser: "../package/main.roc",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        parser.Core.{ Parser, buildPrimitiveParser, many },
        parser.String.{ parseStr },
    ]
    provides [main] to cli

main =

    result : Result (List Letter) [ParsingFailure Str, ParsingIncomplete Str]
    result = parseStr (many letterParser) "AAAiBByAABBwBtCCCiAyArBBx"

    when result |> Result.map countLetterAs is
        Ok count -> Stdout.line "I counted \(Num.toStr count) letter A's!"
        Err _ -> Stderr.line "Failed while parsing input"

Letter : [A, B, C, Other]

# Helper to check if a letter is an A tag
isA = \l -> l == A

# Count the number of Letter A's
countLetterAs : List Letter -> Nat
countLetterAs = \letters ->
    letters
    |> List.keepIf isA
    |> List.map \_ -> 1
    |> List.sum

# Build a custom parser to convert utf8 input into Letter tags
letterParser : Parser (List U8) Letter
letterParser =
    input <- buildPrimitiveParser

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
    result = parseStr parser input
    result == Ok B

# Test we can parse a number of different letters
expect
    input = "BCXA"
    parser = many letterParser
    result = parseStr parser input
    result == Ok [B, C, Other, A]
