app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.String

# tag::parse[]
main! : List(OsStr) => Try({}, _)
main! = |args| {
	input = args.get(1).map_ok(OsStr.display) ?? "2024"
	match String.parse_str(String.digits, input) {
		Ok(number) => Stdout.line!("Parsed the number ${number.to_str()}")?
		Err(_) => Stdout.line!("Not a number: ${input}")?
	}
	Ok({})
}
# end::parse[]

expect String.parse_str(String.digits, "42") == Ok(42)
