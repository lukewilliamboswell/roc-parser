app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

# tag::backtrack[]
# Both alternatives start with "ab". When the first fails at "d", the
# second starts again from the beginning of the same input.
abc_or_abd : Parser(String.Utf8, Str)
abc_or_abd = String.one_of([String.string("abc"), String.string("abd")])

expect String.parse_str(abc_or_abd, "abd") == Ok("abd")
# end::backtrack[]

# tag::order[]
# The first alternative that succeeds wins, even if a later one would
# consume more input. Put the longer keyword first.
keyword_short_first : Parser(String.Utf8, Str)
keyword_short_first = String.one_of([String.string("in"), String.string("int")])

keyword_long_first : Parser(String.Utf8, Str)
keyword_long_first = String.one_of([String.string("int"), String.string("in")])

expect String.parse_str_partial(keyword_short_first, "int").map_ok(|r| r.input) == Ok("t")
expect String.parse_str(keyword_long_first, "int") == Ok("int")
# end::order[]

# tag::progress[]
# chomp_while always succeeds, possibly consuming nothing. `many` stops at
# the first element that makes no progress instead of looping forever.
runs : Parser(String.Utf8, List(List(U8)))
runs = Parser.many(Parser.chomp_while(|b| b == 'a'))

expect String.parse_str_partial(runs, "aab").map_ok(|r| r.input) == Ok("b")
# end::progress[]

# tag::many-backtracks[]
# A repeated element that fails part-way is not an error for `many`:
# repetition ends before that element, and its input is left unconsumed.
pair : Parser(String.Utf8, U64)
pair = String.digits.skip(String.codeunit(';'))

expect String.parse_str_partial(Parser.many(pair), "1;2;3x").map_ok(|r| r.input) == Ok("3x")
# end::many-backtracks[]

# tag::flatten[]
# Validate a value and turn a rejection into a parse failure.
port : Parser(String.Utf8, U64)
port =
	String.digits
		.map(
			|n| if n <= 65535 {
				Ok(n)
			} else {
				Err("port ${n.to_str()} is out of range")
			},
		)
		.flatten()

expect String.parse_str(port, "8080") == Ok(8080)
expect String.parse_str(port, "70000") == Err(ParsingFailure("port 70000 is out of range"))
# end::flatten[]

# tag::maybe[]
# An optional leading sign, then digits.
signed : Parser(String.Utf8, I64)
signed =
	Parser.const(
		|sign| |n| {
			match sign {
				Ok(_) => -(n.to_i64_wrap())
				Err(Nothing) => n.to_i64_wrap()
			}
		},
	)
		.keep(Parser.maybe(String.codeunit('-')))
		.keep(String.digits)

expect String.parse_str(signed, "-42") == Ok(-42)
expect String.parse_str(signed, "42") == Ok(42)
# end::maybe[]

# tag::bytes[]
# Parsers read UTF-8 code units (bytes), not characters. "é" is two bytes.
expect String.parse_str_partial(String.any_codeunit, "é").map_ok(|r| r.val) == Ok(0xC3)
expect String.parse_str(String.string("é"), "é") == Ok("é")
# end::bytes[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(port, "70000")))
}
