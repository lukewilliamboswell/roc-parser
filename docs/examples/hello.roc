app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.Utf8

# tag::parse[]
main! : List(OsStr) => Try({}, _)
main! = |args| {
	input = args.get(1).map_ok(OsStr.display) ?? "2024"
	match Utf8.parse_str(Utf8.digits, input) {
		Ok(number) => Stdout.line!("Parsed the number ${number.to_str()}")?
		Err(_) => Stdout.line!("Not a number: ${input}")?
	}
	Ok({})
}
# end::parse[]

expect Utf8.parse_str(Utf8.digits, "42") == Ok(42)
