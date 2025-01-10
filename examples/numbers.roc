app [main!] {
    cli: platform "../../basic-cli/platform/main.roc",
    parser: "../package/main.roc",
}

import cli.Stdout
import cli.Stderr
import parser.Parser
import parser.String

main! = \_args ->

    result : Result (List (List U64)) [ParsingFailure Str, ParsingIncomplete Str]
    result = String.parse_str(Parser.many(multiple_numbers), "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n")

    when result |> Result.map(largest) is
        Ok(count) -> Stdout.line!("The lagest sum is $(Num.to_str(count))")
        Err(_) -> Stderr.line!("Failed while parsing input")

# Parse a number followed by a newline
single_number : Parser.Parser (List U8) U64
single_number =
    Parser.const(\n -> n)
    |> Parser.keep(String.digits)
    |> Parser.skip(String.string("\n"))

expect
    actual = String.parse_str(single_number, "1000\n")
    actual == Ok(1000)

# Parse a series of numbers followed by a newline
multiple_numbers : Parser.Parser (List U8) (List U64)
multiple_numbers =
    Parser.const(\ns -> ns)
    |> Parser.keep(Parser.many(single_number))
    |> Parser.skip(String.string("\n"))

expect
    actual = String.parse_str(multiple_numbers, "1000\n2000\n3000\n\n")
    actual == Ok([1000, 2000, 3000])

# Sum up the lists and return the largest sum
largest : List (List U64) -> U64
largest = \numbers ->
    numbers
    |> List.map(List.sum)
    |> List.sort_desc
    |> List.first
    |> Result.with_default(0)

expect largest([[1000, 2000, 3000], [4000], [5000, 6000]]) == 11_000
