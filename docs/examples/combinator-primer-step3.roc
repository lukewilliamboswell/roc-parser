app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

is_key_byte : U8 -> Bool
is_key_byte = |b| (b >= 'a' and b <= 'z') or b == '_'

key : Parser(String.Utf8, Str)
key =
	String.codeunit_satisfies(is_key_byte)
		.one_or_more()
		.map(String.str_from_utf8)

# tag::value[]
Value : [Number(U64), Flag(Bool), Text(Str)]

number : Parser(String.Utf8, Value)
number = String.digits.map(|n| Number(n))

flag : Parser(String.Utf8, Value)
flag =
	String.one_of([
		Parser.const(Flag(Bool.True)).skip(String.string("true")),
		Parser.const(Flag(Bool.False)).skip(String.string("false")),
	])

text : Parser(String.Utf8, Value)
text =
	Parser.chomp_while(|b| b != '"')
		.map(|bytes| Text(String.str_from_utf8(bytes)))
		.between(String.codeunit('"'), String.codeunit('"'))

value : Parser(String.Utf8, Value)
value = String.one_of([number, flag, text])

# end::value[]

Entry : { key : Str, value : Value }

entry : Parser(String.Utf8, Entry)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(String.codeunit('='))
		.keep(value)

# tag::expects[]
expect String.parse_str(value, "8080") == Ok(Number(8080))
expect String.parse_str(value, "true") == Ok(Flag(Bool.True))
expect String.parse_str(value, "\"hello world\"") == Ok(Text("hello world"))
expect String.parse_str(value, "maybe").is_err()
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(entry, "port=8080")))
}
