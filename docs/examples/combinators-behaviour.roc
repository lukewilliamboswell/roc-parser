app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
# tag::backtrack[]
# Both alternatives start with "ab". When the first fails at "d", the
# second starts again from the beginning of the same input.
abc_or_abd : Parser(Parser.Utf8.Bytes, Str, _)
abc_or_abd = Parser.one_of([Parser.Utf8.string("abc"), Parser.Utf8.string("abd")])

expect (abc_or_abd).parse_str("abd") == Ok("abd")
# end::backtrack[]

# tag::order[]
# The first alternative that succeeds wins, even if a later one would
# consume more input. Put the longer keyword first.
keyword_short_first : Parser(Parser.Utf8.Bytes, Str, _)
keyword_short_first = Parser.one_of([Parser.Utf8.string("in"), Parser.Utf8.string("int")])

keyword_long_first : Parser(Parser.Utf8.Bytes, Str, _)
keyword_long_first = Parser.one_of([Parser.Utf8.string("int"), Parser.Utf8.string("in")])

expect (keyword_short_first).run_str("int").map_ok(|r| r.rest) == Ok("t")
expect (keyword_long_first).parse_str("int") == Ok("int")
# end::order[]

# tag::progress[]
# chomp_while always succeeds, possibly consuming nothing. `many` stops at
# the first element that makes no progress instead of looping forever.
runs : Parser(Parser.Utf8.Bytes, List(List(U8)), [])
runs = Parser.many(Parser.chomp_while(|b| b == 'a'))

expect (runs).run_str("aab").map_ok(|r| r.rest) == Ok("b")
# end::progress[]

# tag::many-backtracks[]
# A repeated element that fails part-way is not an error for `many`:
# repetition ends before that element, and its input is left unconsumed.
pair : Parser(Parser.Utf8.Bytes, U64, _)
pair = Parser.Utf8.digits({}).skip(Parser.Utf8.codeunit(';'))

expect (Parser.many(pair)).run_str("1;2;3x").map_ok(|r| r.rest) == Ok("3x")
# end::many-backtracks[]

# tag::flatten[]
# Validate a value and turn a rejection into a parse failure.
port : Parser(Parser.Utf8.Bytes, U64, _)
port =
	Parser.Utf8.digits({})
		.map(
			|n| if n <= 65535 {
				Ok(n)
			} else {
				Err(PortOutOfRange({ value: n, max: 65535 }))
			},
		)
		.flatten()

expect (port).parse_str("8080") == Ok(8080)
expect (port).parse_str("70000") == Err(ParseError(Parser.Diagnostic.{ problem: PortOutOfRange({ value: 70000, max: 65535 }), location: Text({ offset: 0, line: 1, column: 1 }), path: [] }))
# end::flatten[]

# tag::maybe[]
# An optional leading sign, then digits.
signed : Parser(Parser.Utf8.Bytes, I64, _)
signed =
	Parser.const(
		|sign| |n| {
			match sign {
				Ok(_) => -(n.to_i64_wrap())
				Err(Missing) => n.to_i64_wrap()
			}
		},
	)
		.keep(Parser.maybe(Parser.Utf8.codeunit('-')))
		.keep(Parser.Utf8.digits({}))

expect (signed).parse_str("-42") == Ok(-42)
expect (signed).parse_str("42") == Ok(42)
# end::maybe[]

# tag::bytes[]
# Parsers read UTF-8 code units (bytes), not characters. "é" is two bytes.
expect Parser.Utf8.any_codeunit({}).run("é".to_utf8()).map_ok(|result| result.value) == Ok(0xC3)
expect Parser.Utf8.any_codeunit({}).run_str("é").is_err()
expect (Parser.Utf8.string("é")).parse_str("é") == Ok("é")
# end::bytes[]

main! = |_args| {
	Stdout.line!(Str.inspect((port).parse_str("70000")))
}
