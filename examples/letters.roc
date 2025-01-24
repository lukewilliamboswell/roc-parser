app [main!] {
    cli: platform "../../basic-cli/platform/main.roc",
    parser: "../package/main.roc",
}

import cli.Stdout
import cli.Stderr
import parser.Parser
import parser.String

main! = |_args|

    result : Result (List Letter) [ParsingFailure Str, ParsingIncomplete Str]
    result = String.parse_str(Parser.many(letter_parser), "AAAiBByAABBwBtCCCiAyArBBx")

    when result |> Result.map_ok(count_letter_as) is
        Ok(count) -> Stdout.line!("I counted ${Num.to_str(count)} letter A's!")
        Err(_) -> Stderr.line!("Failed while parsing input")

Letter : [A, B, C, Other]

# Helper to check if a letter is an A tag
is_a = |l|
    l == A

# Count the number of Letter A's
count_letter_as : List Letter -> U64
count_letter_as = |letters|
    letters
    |> List.keep_if(is_a)
    |> List.map(|_| 1)
    |> List.sum

# Build a custom parser to convert utf8 input into Letter tags
letter_parser : Parser.Parser (List U8) Letter
letter_parser = Parser.build_primitive_parser(
    |input|
        val_result =
            when input is
                [] -> Err(ParsingFailure("Nothing to parse"))
                ['A', ..] -> Ok(A)
                ['B', ..] -> Ok(B)
                ['C', ..] -> Ok(C)
                _ -> Ok(Other)

        val_result
        |> Result.map_ok(|val| { val, input: List.drop_first(input, 1) }),
)

# Test we can parse a single B letter
expect
    input = "B"
    parser = letter_parser
    result = String.parse_str(parser, input)
    result == Ok(B)

# Test we can parse a number of different letters
expect
    input = "BCXA"
    parser = Parser.many(letter_parser)
    result = String.parse_str(parser, input)
    result == Ok([B, C, Other, A])
