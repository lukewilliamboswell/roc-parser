app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import cli.Stdout
import cli.Stderr
import parser.Parser
import parser.String

main! = |_args| {
	result : Try(List(Letter), [ParsingFailure(Str), ParsingIncomplete(Str)])
	result = String.parse_str(letter_parser.many(), "AAAiBByAABBwBtCCCiAyArBBx")

	match result.map_ok(count_letter_as) {
		Ok(count) => Stdout.line!("I counted ${count.to_str()} letter A's!")
		Err(_) => Stderr.line!("Failed while parsing input")
	}
	Ok({})
}

Letter : [A, B, C, Other]

# Helper to check if a letter is an A tag
is_a = |l| l == A

# Count the number of Letter A's
count_letter_as : List(Letter) -> U64
count_letter_as = |letters|
	letters
		.keep_if(is_a)
		.map(|_| 1)
		.sum()

# Build a custom parser to convert utf8 input into Letter tags
letter_parser : Parser(List(U8), Letter)
letter_parser = Parser.build_primitive_parser(
	|input| {
		val_result : Try(Letter, [ParsingFailure(Str)])
		val_result = 
			match input {
				[] => Err(ParsingFailure("Nothing to parse"))
				['A', ..] => Ok(A)
				['B', ..] => Ok(B)
				['C', ..] => Ok(C)
				_ => Ok(Other)
			}

		val_result
			.map_ok(|val| { val, input: input.drop_first(1) })
	},
)

# Test we can parse a single B letter
expect {
	input = "B"
	parser = letter_parser
	result = parser->String.parse_str(input)
	result == Ok(B)
}

# Test we can parse a number of different letters
expect {
	input = "BCXA"
	parser = letter_parser.many()
	result = parser->String.parse_str(input)
	result == Ok([B, C, Other, A])
}
