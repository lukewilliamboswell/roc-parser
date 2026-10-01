app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

# tag::key[]
is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(String.Utf8, Str)
key =
	String.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(String.str_from_utf8)
# end::key[]

# tag::expects[]
expect String.parse_str(key, "name") == Ok("name")
expect String.parse_str(key, "Name").is_err()
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(key, "name")))
}
