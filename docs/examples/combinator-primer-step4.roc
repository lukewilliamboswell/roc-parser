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

Entry : { key : Str, value : Value }

entry : Parser(String.Utf8, Entry)
entry =
	Parser.const(|k| |v| { key: k, value: v })
		.keep(key)
		.skip(String.codeunit('='))
		.keep(value)

# tag::config[]
config : Parser(String.Utf8, List(Entry))
config = entry.sep_by(String.codeunit('\n'))
# end::config[]

# tag::expects[]
expect
	String.parse_str(config, "port=8080\ndebug=true")
		== Ok([{ key: "port", value: Number(8080) }, { key: "debug", value: Flag(Bool.True) }])
# end::expects[]

# tag::report[]
report : Str -> Str
report = |source| {
	match String.parse_str(config, source) {
		Ok(entries) => "parsed ${entries.len().to_str()} entries"
		Err(ParsingIncomplete(rest)) => "stopped before: `${rest.replace_each("\n", "\\n")}`"
		Err(ParsingFailure(message)) => "failed: ${message}"
	}
}

report_entry : Str -> Str
report_entry = |source| {
	match String.parse_str(entry, source) {
		Ok(_) => "parsed one entry"
		Err(ParsingIncomplete(rest)) => "stopped before: `${rest}`"
		Err(ParsingFailure(message)) => "failed: ${message}"
	}
}

main! = |_args| {
	Stdout.line!(report("name=\"demo\"\nport=8080\ndebug=true"))?
	Stdout.line!(report("name=\"demo\"\nport=eighty"))?
	Stdout.line!(report("name=\"demo\"\n"))?
	Stdout.line!(report_entry("port=eighty"))
}
# end::report[]
