app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.JsonValue

## Accept a byte budget, parse stdin and write compact or indented JSON.
## Acceptance tests compare these bytes with an independent Python writer.
main! = |args| {
	budget = U64.from_str(args.first().map_ok(OsStr.display) ?? "") ?? 0
	indent = U64.from_str(args.get(1).map_ok(OsStr.display) ?? "") ?? 0
	text = Str.from_utf8(Stdin.read_to_end!()?)?
	match JsonValue.parse_bounded(text, { max_depth: 64, max_items: 16384 }, budget) {
		Err(_) => Stdout.line!("parse limit or invalid JSON")
		Ok(value) => match JsonValue.to_str_bounded(value, { indent: indent }, budget) {
			Err(_) => Stdout.line!("write limit or invalid JSON")
			Ok(output) => Stdout.line!(output)
		}
	}
}
