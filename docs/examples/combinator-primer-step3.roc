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

# tag::value[]
Value : [Number(U64), Flag(Bool), Text(Str)]

number : Parser(Parser.Utf8.Bytes, Value, _)
number = Parser.Utf8.digits({}).map(|n| Number(n))

flag : Parser(Parser.Utf8.Bytes, Value, _)
flag =
	Parser.one_of([
		Parser.const(Flag(Bool.True)).skip(Parser.Utf8.string("true")),
		Parser.const(Flag(Bool.False)).skip(Parser.Utf8.string("false")),
	])

text : Parser(Parser.Utf8.Bytes, Value, _)
text =
	Parser.chomp_while(|b| b != '"')
		.map(|bytes| Text(Str.from_utf8_lossy(bytes)))
		.between(Parser.Utf8.codeunit('"'), Parser.Utf8.codeunit('"'))

value : Parser(Parser.Utf8.Bytes, Value, _)
value = Parser.one_of([number, flag, text])

# end::value[]

Entry : { key : Str, value : Value }

entry : Parser(Parser.Utf8.Bytes, Entry, _)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(Parser.Utf8.codeunit('='))
		.keep(value)

# tag::expects[]
expect (value).parse_str("8080") == Ok(Number(8080))
expect (value).parse_str("true") == Ok(Flag(Bool.True))
expect (value).parse_str("\"hello world\"") == Ok(Text("hello world"))
expect (value).parse_str("maybe").is_err()
# end::expects[]

main! = |_args| {
	Stdout.line!(Str.inspect((entry).parse_str("port=8080")))
}
