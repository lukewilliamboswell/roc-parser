app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(Parser.Utf8.Bytes, Str, _)
key =
	Parser.Utf8.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(Str.from_utf8_lossy)

# tag::entry[]
value : Parser(Parser.Utf8.Bytes, Str, _)
value =
	Parser.chomp_while(|b| b != '\n')
		.map(Str.from_utf8_lossy)

Entry : { key : Str, value : Str }

entry : Parser(Parser.Utf8.Bytes, Entry, _)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(Parser.Utf8.codeunit('='))
		.keep(value)
# end::entry[]

# tag::expects[]
expect (entry).parse_str("name=roc") == Ok({ key: "name", value: "roc" })
expect (entry).parse_str("empty=") == Ok({ key: "empty", value: "" })
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect((entry).parse_str("name=roc")))
}
