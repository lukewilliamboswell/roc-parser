app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

# tag::backtrack[]
# Both alternatives start with "ab". When the first fails at "d", the
# second starts again from the beginning of the same input.
abc_or_abd : Parser(Utf8.Bytes, Str)
abc_or_abd = Utf8.one_of([Utf8.string("abc"), Utf8.string("abd")])

expect Utf8.parse_str(abc_or_abd, "abd") == Ok("abd")
# end::backtrack[]

# tag::order[]
# The first alternative that succeeds wins, even if a later one would
# consume more input. Put the longer keyword first.
keyword_short_first : Parser(Utf8.Bytes, Str)
keyword_short_first = Utf8.one_of([Utf8.string("in"), Utf8.string("int")])

keyword_long_first : Parser(Utf8.Bytes, Str)
keyword_long_first = Utf8.one_of([Utf8.string("int"), Utf8.string("in")])

expect Utf8.parse_str_partial(keyword_short_first, "int").map_ok(|r| r.rest) == Ok("t")
expect Utf8.parse_str(keyword_long_first, "int") == Ok("int")
# end::order[]

# tag::progress[]
# chomp_while always succeeds, possibly consuming nothing. `many` stops at
# the first element that makes no progress instead of looping forever.
runs : Parser(Utf8.Bytes, List(List(U8)))
runs = Parser.many(Parser.chomp_while(|b| b == 'a'))

expect Utf8.parse_str_partial(runs, "aab").map_ok(|r| r.rest) == Ok("b")
# end::progress[]

# tag::many-backtracks[]
# A repeated element that fails part-way is not an error for `many`:
# repetition ends before that element, and its input is left unconsumed.
pair : Parser(Utf8.Bytes, U64)
pair = Utf8.digits.skip(Utf8.codeunit(';'))

expect Utf8.parse_str_partial(Parser.many(pair), "1;2;3x").map_ok(|r| r.rest) == Ok("3x")
# end::many-backtracks[]

# tag::flatten[]
# Validate a value and turn a rejection into a parse failure.
port : Parser(Utf8.Bytes, U64)
port =
	Utf8.digits
		.map(
			|n| if n <= 65535 {
				Ok(n)
			} else {
				Err("port ${n.to_str()} is out of range")
			},
		)
		.flatten()

expect Utf8.parse_str(port, "8080") == Ok(8080)
expect Utf8.parse_str(port, "70000") == Err(ParseError({ message: "port 70000 is out of range", offset: 0 }))
# end::flatten[]

# tag::maybe[]
# An optional leading sign, then digits.
signed : Parser(Utf8.Bytes, I64)
signed =
	Parser.const(
		|sign| |n| {
			match sign {
				Ok(_) => -(n.to_i64_wrap())
				Err(Missing) => n.to_i64_wrap()
			}
		},
	)
		.keep(Parser.maybe(Utf8.codeunit('-')))
		.keep(Utf8.digits)

expect Utf8.parse_str(signed, "-42") == Ok(-42)
expect Utf8.parse_str(signed, "42") == Ok(42)
# end::maybe[]

# tag::bytes[]
# Parsers read UTF-8 code units (bytes), not characters. "é" is two bytes.
expect Utf8.parse_str_partial(Utf8.any_codeunit, "é").map_ok(|r| r.value) == Ok(0xC3)
expect Utf8.parse_str(Utf8.string("é"), "é") == Ok("é")
# end::bytes[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(port, "70000")))
}
