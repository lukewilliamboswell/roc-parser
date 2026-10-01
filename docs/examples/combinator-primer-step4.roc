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
		.map(Str.from_utf8_lossy)

Value : [Number(U64), Flag(Bool), Text(Str)]

number : Parser(Utf8.Bytes, Value)
number = Utf8.digits.map(|n| Number(n))

flag : Parser(Utf8.Bytes, Value)
flag =
	Parser.one_of([
		Parser.const(Flag(Bool.True)).skip(Utf8.string("true")),
		Parser.const(Flag(Bool.False)).skip(Utf8.string("false")),
	])

text : Parser(Utf8.Bytes, Value)
text =
	Parser.chomp_while(|b| b != '"')
		.map(|bytes| Text(Str.from_utf8_lossy(bytes)))
		.between(Utf8.codeunit('"'), Utf8.codeunit('"'))

value : Parser(Utf8.Bytes, Value)
value = Parser.one_of([number, flag, text])

Entry : { key : Str, value : Value }

entry : Parser(Utf8.Bytes, Entry)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(Utf8.codeunit('='))
		.keep(value)

# tag::config[]
config : Parser(Utf8.Bytes, List(Entry))
config = entry.sep_by(Utf8.codeunit('\n'))
# end::config[]

# tag::expects[]
expect
	Utf8.parse_str(config, "port=8080\ndebug=true")
		== Ok([{ key: "port", value: Number(8080) }, { key: "debug", value: Flag(Bool.True) }])
# end::expects[]

# tag::report[]
report : Str -> Str
report = |source| {
	match Utf8.parse_str(config, source) {
		Ok(entries) => "parsed ${entries.len().to_str()} entries"
		Err(ParseError({ message, offset })) => "failed at byte ${offset.to_str()}: ${message}"
	}
}

report_entry : Str -> Str
report_entry = |source| {
	match Utf8.parse_str(entry, source) {
		Ok(_) => "parsed one entry"
		Err(ParseError({ message, offset })) => "failed at byte ${offset.to_str()}: ${message}"
	}
}

main! = |_args| {
	Stdout.line!(report("name=\"demo\"\nport=8080\ndebug=true"))?
	Stdout.line!(report("name=\"demo\"\nport=eighty"))?
	Stdout.line!(report("name=\"demo\"\n"))?
	Stdout.line!(report_entry("port=eighty"))
}
# end::report[]
