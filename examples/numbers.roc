app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "../package/main.roc",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        parser.Core.{ Parser, many, const, skip, keep },
        parser.String.{ parseStr, digits, string },
    ]
    provides [main] to cli

main =

    result : Result (List (List U64)) [ParsingFailure Str, ParsingIncomplete Str]
    result = parseStr (many multipleNumbers) "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"

    when result |> Result.map largest is
        Ok count -> Stdout.line "The lagest sum is \(Num.toStr count)"
        Err _ -> Stderr.line "Failed while parsing input"

# Parse a number followed by a newline
singleNumber : Parser (List U8) U64
singleNumber =
    const (\n -> n)
    |> keep (digits)
    |> skip (string "\n")

expect
    actual = parseStr singleNumber "1000\n"
    actual == Ok 1000

# Parse a series of numbers followed by a newline
multipleNumbers : Parser (List U8) (List U64)
multipleNumbers =
    const (\ns -> ns)
    |> keep (many singleNumber)
    |> skip (string "\n")

expect
    actual = parseStr multipleNumbers "1000\n2000\n3000\n\n"
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
