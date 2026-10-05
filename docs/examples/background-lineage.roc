app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

# tag::applicative[]
# Applicative style: a constructor function, then one `keep` per field and
# one `skip` per piece of punctuation. No step depends on an earlier value.
Point : { x : U64, y : U64 }

point : Parser(Utf8.Bytes, Point)
point =
	Parser.const(|x| |y| { x, y })
		.skip(Utf8.codeunit('('))
		.keep(Utf8.digits)
		.skip(Utf8.codeunit(','))
		.keep(Utf8.digits)
		.skip(Utf8.codeunit(')'))

expect Utf8.parse_str(point, "(3,4)") == Ok({ x: 3, y: 4 })
# end::applicative[]

# tag::monadic[]
# A parser is a function from input to a result and the rest of the input.
# `custom` wraps such a function directly.
take : U64 -> Parser(Utf8.Bytes, Utf8.Bytes)
take = |n|
	Parser.custom(
		|input|
			if input.len() >= n {
				Ok({ value: input.take_first(n), rest: input.drop_first(n) })
			} else {
				Err(ParseError({ message: "expected ${n.to_str()} more bytes", offset: 0 }))
			},
	)

# Monadic style: the next parser depends on a value already read. Here a
# length prefix says how many bytes follow.
length_prefix : Parser(Utf8.Bytes, U64)
length_prefix = Utf8.digits.skip(Utf8.codeunit(':'))

counted : Parser(Utf8.Bytes, Str)
counted = length_prefix.and_then(take).map(Str.from_utf8_lossy)

expect Utf8.parse_str(counted, "3:abc") == Ok("abc")
expect Utf8.parse_str(counted, "3:ab").is_err()
# end::monadic[]

# tag::ordered[]
# Ordered choice, as in a PEG: the first alternative that succeeds wins, and
# a failed alternative is retried from the same input.
sign : Parser(Utf8.Bytes, [Plus, Minus, PlusPlus])
sign =
	Parser.one_of([
		Parser.const(PlusPlus).skip(Utf8.string("++")),
		Parser.const(Plus).skip(Utf8.string("+")),
		Parser.const(Minus).skip(Utf8.string("-")),
	])

expect Utf8.parse_str(sign, "++") == Ok(PlusPlus)
expect Utf8.parse_str(sign, "-") == Ok(Minus)
# end::ordered[]

# tag::furthest[]
# Furthest failure: `many` stops at the bad second point, but the error the
# runner reports is that point's own failure, at the byte where it happened.
points : Parser(Utf8.Bytes, List(Point))
points = Parser.many(point)

expect Utf8.parse_str(points, "(1,2)(3,x)") == Err(ParseError({ message: "Not a digit", offset: 8 }))
# end::furthest[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(point, "(3,4)")))?
	Stdout.line!(Str.inspect(Utf8.parse_str(counted, "3:abc")))?
	Stdout.line!(Str.inspect(Utf8.parse_str(sign, "++")))?
	Stdout.line!(Str.inspect(Utf8.parse_str(points, "(1,2)(3,x)")))
}
