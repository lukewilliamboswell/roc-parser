app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
# tag::key[]
is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(Parser.Utf8.Bytes, Str, _)
key =
	Parser.Utf8.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(Str.from_utf8_lossy)
# end::key[]

# tag::expects[]
expect (key).parse_str("name") == Ok("name")
expect (key).parse_str("Name").is_err()
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect((key).parse_str("name")))
}
