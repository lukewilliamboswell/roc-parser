app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
# tag::applicative[]
# Applicative style: a constructor function, then one `keep` per field and
# one `skip` per piece of punctuation. No step depends on an earlier value.
Point : { x : U64, y : U64 }

point : Parser(Parser.Utf8.Bytes, Point, _)
point =
	Parser.const(|x| |y| { x, y })
		.skip(Parser.Utf8.codeunit('('))
		.keep(Parser.Utf8.digits({}))
		.skip(Parser.Utf8.codeunit(','))
		.keep(Parser.Utf8.digits({}))
		.skip(Parser.Utf8.codeunit(')'))

expect (point).parse_str("(3,4)") == Ok({ x: 3, y: 4 })
# end::applicative[]

# tag::monadic[]
# A parser is a function from input to a result and the rest of the input.
# `custom` wraps such a function directly.
take : U64 -> Parser(Parser.Utf8.Bytes, Parser.Utf8.Bytes, _)
take = |n|
	Parser.custom(
		|input|
			if input.len() >= n {
				Ok({ value: input.take_first(n), rest: input.drop_first(n) })
			} else {
				Err(ParseError(Parser.Diagnostic.{ problem: MissingBytes(n), location: InputOffset(0), path: [] }))
			},
	)

# Monadic style: the next parser depends on a value already read. Here a
# length prefix says how many bytes follow.
length_prefix : Parser(Parser.Utf8.Bytes, U64, _)
length_prefix = Parser.Utf8.digits({}).skip(Parser.Utf8.codeunit(':'))

counted : Parser(Parser.Utf8.Bytes, Str, _)
counted = length_prefix.and_then(take).map(Str.from_utf8_lossy)

expect (counted).parse_str("3:abc") == Ok("abc")
expect (counted).parse_str("3:ab").is_err()
# end::monadic[]

# tag::ordered[]
# Ordered choice, as in a PEG: the first alternative that succeeds wins, and
# a failed alternative is retried from the same input.
sign : Parser(Parser.Utf8.Bytes, [Plus, Minus, PlusPlus], _)
sign =
	Parser.one_of([
		Parser.const(PlusPlus).skip(Parser.Utf8.string("++")),
		Parser.const(Plus).skip(Parser.Utf8.string("+")),
		Parser.const(Minus).skip(Parser.Utf8.string("-")),
	])

expect (sign).parse_str("++") == Ok(PlusPlus)
expect (sign).parse_str("-") == Ok(Minus)
# end::ordered[]

# tag::furthest[]
# Furthest failure: `many` stops at the bad second point, but the error the
# runner reports is that point's own failure, at the byte where it happened.
points : Parser(Parser.Utf8.Bytes, List(Point), _)
points = Parser.many(point)

expect (points).parse_str("(1,2)(3,x)") == Err(ParseError(Parser.Diagnostic.{ problem: ExpectedDigit, location: Text({ offset: 8, line: 1, column: 9 }), path: [] }))
# end::furthest[]

main! = |_args| {
	Stdout.line!(Str.inspect((point).parse_str("(3,4)")))?
	Stdout.line!(Str.inspect((counted).parse_str("3:abc")))?
	Stdout.line!(Str.inspect((sign).parse_str("++")))?
	Stdout.line!(Str.inspect((points).parse_str("(1,2)(3,x)")))
}
