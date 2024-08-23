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

    result : Result (List (List U64)) [ParsingFailure Str, ParsingIncomplete Str]
    result = String.parseStr (Core.many multipleNumbers) "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"

    when result |> Result.map largest is
        Ok count -> Stdout.line "The lagest sum is $(Num.toStr count)"
        Err _ -> Stderr.line "Failed while parsing input"

# Parse a number followed by a newline
singleNumber : Core.Parser (List U8) U64
singleNumber =
    Core.const (\n -> n)
    |> Core.keep (String.digits)
    |> Core.skip (String.string "\n")

expect
    actual = String.parseStr singleNumber "1000\n"
    actual == Ok 1000

# Parse a series of numbers followed by a newline
multipleNumbers : Core.Parser (List U8) (List U64)
multipleNumbers =
    Core.const (\ns -> ns)
    |> Core.keep (Core.many singleNumber)
    |> Core.skip (String.string "\n")

expect
    actual = String.parseStr multipleNumbers "1000\n2000\n3000\n\n"
    actual == Ok [1000, 2000, 3000]

# Sum up the lists and return the largest sum
largest : List (List U64) -> U64
largest = \numbers ->
    numbers
    |> List.map List.sum
    |> List.sortDesc
    |> List.first
    |> Result.withDefault 0

expect largest [[1000, 2000, 3000], [4000], [5000, 6000]] == 11_000
