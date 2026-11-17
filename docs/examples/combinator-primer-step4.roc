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

Entry : { key : Str, value : Value }

entry : Parser(Parser.Utf8.Bytes, Entry, _)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(Parser.Utf8.codeunit('='))
		.keep(value)

# tag::config[]
config : Parser(Parser.Utf8.Bytes, List(Entry), _)
config = entry.sep_by(Parser.Utf8.codeunit('\n'))
# end::config[]

# tag::expects[]
expect
	(config).parse_str("port=8080\ndebug=true")
		== Ok([{ key: "port", value: Number(8080) }, { key: "debug", value: Flag(Bool.True) }])
# end::expects[]

# tag::report[]
report : Str -> Str
report = |source| {
	match (config).parse_str(source) {
		Ok(entries) => "parsed ${entries.len().to_str()} entries"
		Err(ParseError({ problem, location, .. })) => "failed at ${Str.inspect(location)}: ${Str.inspect(problem)}"
	}
}

report_entry : Str -> Str
report_entry = |source| {
	match (entry).parse_str(source) {
		Ok(_) => "parsed one entry"
		Err(ParseError({ problem, location, .. })) => "failed at ${Str.inspect(location)}: ${Str.inspect(problem)}"
	}
}

main! = |_args| {
	Stdout.line!(report("name=\"demo\"\nport=8080\ndebug=true"))?
	Stdout.line!(report("name=\"demo\"\nport=eighty"))?
	Stdout.line!(report("name=\"demo\"\n"))?
	Stdout.line!(report_entry("port=eighty"))
}
# end::report[]
