app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(Utf8.Bytes, Str)
key =
	Utf8.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(Utf8.str_from_utf8)

# tag::entry[]
value : Parser(Utf8.Bytes, Str)
value =
	Parser.chomp_while(|b| b != '\n')
		.map(Utf8.str_from_utf8)

Entry : { key : Str, value : Str }

entry : Parser(Utf8.Bytes, Entry)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(Utf8.codeunit('='))
		.keep(value)
# end::entry[]

# tag::expects[]
expect Utf8.parse_str(entry, "name=roc") == Ok({ key: "name", value: "roc" })
expect Utf8.parse_str(entry, "empty=") == Ok({ key: "empty", value: "" })
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(entry, "name=roc")))
}
