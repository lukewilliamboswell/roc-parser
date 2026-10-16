app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

# tag::key[]
is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(Utf8.Bytes, Str)
key =
	Utf8.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(Str.from_utf8_lossy)
# end::key[]

# tag::expects[]
expect Utf8.parse_str(key, "name") == Ok("name")
expect Utf8.parse_str(key, "Name").is_err()
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(key, "name")))
}
