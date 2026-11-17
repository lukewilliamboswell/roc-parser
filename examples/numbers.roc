app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import cli.Stderr
import parser.Parser
main! : List(OsStr) => Try({}, _)
main! = |args| {
	input = args.get(1).map_ok(OsStr.display) ?? "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"
	result : Try(List(List(U64)), [ParseError(Parser.Diagnostic(Parser.Problem(_)))])
	result = (multiple_numbers.many()).parse_str(input)

	match result.map_ok(largest) {
		Ok(count) => Stdout.line!("The largest sum is ${count.to_str()}")?
		Err(_) => Stderr.line!("Failed while parsing input")?
	}
	Ok({})
}

# Parse a number followed by a newline
single_number : Parser(List(U8), U64, _)
single_number =
	Parser.const(|n| n)
		.keep(Parser.Utf8.digits({}))
		.skip(Parser.Utf8.string("\n"))

## A number parser consumes one newline-terminated number.
expect {
	actual = (single_number).parse_str("1000\n")?
	actual == 1000
}

# Parse a series of numbers followed by a newline
multiple_numbers : Parser(List(U8), List(U64), _)
multiple_numbers =
	Parser.const(|ns| ns)
		.keep(single_number.many())
		.skip(Parser.Utf8.string("\n"))

## A list parser consumes numbers until the blank line terminator.
expect {
	actual = (multiple_numbers).parse_str("1000\n2000\n3000\n\n")?
	actual == [1000, 2000, 3000]
}

# Sum up the lists and return the largest sum
largest : List(List(U64)) -> U64
largest = |numbers|
	numbers
		.map(List.sum)
		.sort_with(|a, b| if (a < b) After else if (a > b) Before else Same)
		.first()
		?? 0

## The largest grouped total is selected from the parsed numbers.
expect largest([[1000, 2000, 3000], [4000], [5000, 6000]]) == 11_000
