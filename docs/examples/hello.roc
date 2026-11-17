app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.Parser
# tag::parse[]
main! : List(OsStr) => Try({}, _)
main! = |args| {
	input = args.get(1).map_ok(OsStr.display) ?? "2024"
	match (Parser.Utf8.digits({})).parse_str(input) {
		Ok(number) => Stdout.line!("Parsed the number ${number.to_str()}")?
		Err(_) => Stdout.line!("Not a number: ${input}")?
	}
	Ok({})
}
# end::parse[]

expect (Parser.Utf8.digits({})).parse_str("42") == Ok(42)
