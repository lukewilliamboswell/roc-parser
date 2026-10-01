app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

# tag::applicative[]
# Applicative style: a constructor function, then one `keep` per field and
# one `skip` per piece of punctuation. No step depends on an earlier value.
Point : { x : U64, y : U64 }

point : Parser(String.Utf8, Point)
point =
	Parser.const(|x| |y| { x, y })
		.skip(String.codeunit('('))
		.keep(String.digits)
		.skip(String.codeunit(','))
		.keep(String.digits)
		.skip(String.codeunit(')'))

expect String.parse_str(point, "(3,4)") == Ok({ x: 3, y: 4 })
# end::applicative[]

# tag::monadic[]
# A parser is a function from input to a result and the rest of the input.
# `build_primitive_parser` wraps such a function directly.
take : U64 -> Parser(String.Utf8, String.Utf8)
take = |n|
	Parser.build_primitive_parser(
		|input|
			if input.len() >= n {
				Ok({ val: input.take_first(n), input: input.drop_first(n) })
			} else {
				Err(ParsingFailure("expected ${n.to_str()} more bytes"))
			},
	)

# Monadic style: the next parser depends on a value already read. Here a
# length prefix says how many bytes follow.
length_prefix : Parser(String.Utf8, U64)
length_prefix = String.digits.skip(String.codeunit(':'))

counted : Parser(String.Utf8, Str)
counted =
	Parser.build_primitive_parser(
		|input| {
			{ val: n, input: rest } = Parser.parse_partial(length_prefix, input)?
			Parser.parse_partial(take(n), rest)
		},
	)
		.map(String.str_from_utf8)

expect String.parse_str(counted, "3:abc") == Ok("abc")
expect String.parse_str(counted, "3:ab").is_err()
# end::monadic[]

# tag::ordered[]
# Ordered choice, as in a PEG: the first alternative that succeeds wins, and
# a failed alternative is retried from the same input.
sign : Parser(String.Utf8, [Plus, Minus, PlusPlus])
sign =
	String.one_of([
		Parser.const(PlusPlus).skip(String.string("++")),
		Parser.const(Plus).skip(String.string("+")),
		Parser.const(Minus).skip(String.string("-")),
	])

expect String.parse_str(sign, "++") == Ok(PlusPlus)
expect String.parse_str(sign, "-") == Ok(Minus)
# end::ordered[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(point, "(3,4)")))?
	Stdout.line!(Str.inspect(String.parse_str(counted, "3:abc")))?
	Stdout.line!(Str.inspect(String.parse_str(sign, "++")))
}
