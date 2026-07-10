app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.0.0/AnZoxzoGPtSGQ15EQh6pBeeaHJ7aizP9MQhK81dES3Uq.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import cli.Stdout
import cli.Stderr
import parser.Parser
import parser.String

main! : List(Str) => Try({}, _)
main! = |args| {
	input = args.first() ?? "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"
	result : Try(List(List(U64)), [ParsingFailure(Str), ParsingIncomplete(Str)])
	result = String.parse_str(multiple_numbers.many(), input)

	match result.map_ok(largest) {
		Ok(count) => Stdout.line!("The largest sum is ${count.to_str()}")?
		Err(_) => Stderr.line!("Failed while parsing input")?
	}
	Ok({})
}

# Parse a number followed by a newline
single_number : Parser(List(U8), U64)
single_number = 
	Parser.const(|n| n)
		.keep(String.digits)
		.skip(String.string("\n"))

## A number parser consumes one newline-terminated number.
expect {
	actual = String.parse_str(single_number, "1000\n")?
	actual == 1000
}

# Parse a series of numbers followed by a newline
multiple_numbers : Parser(List(U8), List(U64))
multiple_numbers = 
	Parser.const(|ns| ns)
		.keep(single_number.many())
		.skip(String.string("\n"))

## A list parser consumes numbers until the blank line terminator.
expect {
	actual = String.parse_str(multiple_numbers, "1000\n2000\n3000\n\n")?
	actual == [1000, 2000, 3000]
}

# Sum up the lists and return the largest sum
largest : List(List(U64)) -> U64
largest = |numbers|
	numbers
		.map(List.sum)
		.sort_with(|a, b| if a < b GT else if b > a LT else EQ)
		.first()
		?? 0

## The largest grouped total is selected from the parsed numbers.
expect largest([[1000, 2000, 3000], [4000], [5000, 6000]]) == 11_000
