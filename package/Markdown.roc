import Parser
import String

# # Content values
Markdown := [
	Heading(Level, Str),
	Link({ alt : Str, href : Str }),
	Image({ alt : Str, href : Str }),
	Code({ ext : Str, pre : Str }),
	TODO(Str),
].{
	Level : [One, Two, Three, Four, Five, Six]

	all : Parser(String.Utf8, List(Markdown))
	all = 
		Parser.one_of(
			[
				heading,
				link,
				image,
				# code,
				todo,
			],
		)
			.sep_by(end_of_line)

	## Headings
	##
	## ```
	## expect String.parse_str(heading, "# Foo Bar") == Ok(Heading(One, "Foo Bar"))
	## expect String.parse_str(heading, "Foo Bar\n---") == Ok(Heading(Two, "Foo Bar"))
	## ```
	heading : Parser(String.Utf8, Markdown)
	heading = 
		Parser.one_of(
			[
				inline_heading,
				two_line_heading_level_one,
				two_line_heading_level_two,
			],
		)

	## Links
	##
	## ```roc
	## expect String.parse_str(link, "[roc](https://roc-lang.org)") == Ok(Link("roc", "https://roc-lang.org"))
	## ```
	link : Parser(String.Utf8, Markdown)
	link = 
		Parser.const(
			|alt| {
				|href| {
					Link({ alt, href })
				}
			},
		)
			.skip(String.string("["))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ']'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.string("]("))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ')'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.codeunit(')'))

	## Images
	##
	## ```roc
	## expect String.parse_str(image, "![alt text](/images/logo.png)") == Ok(Image("alt text", "/images/logo.png"))
	## ```
	image : Parser(String.Utf8, Markdown)
	image = 
		Parser.const(
			|alt| {
				|href| {
					Image({ alt, href })
				}
			},
		)
			.skip(String.string("!["))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ']'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.string("]("))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ')'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.codeunit(')'))

	## Parse code blocks using triple backticks
	## supports block extension e.g. ```roc
	##
	## ```roc
	## expect {
	##     text =
	##         \\```roc
	##         \\# some code
	##         \\foo = bar
	##         \\```
	##
	##     a = String.parse_str(code, text)
	##     a == Ok(Code({ ext: "roc", pre: "# some code\nfoo = bar\n" }))
	## }
	## ```
	code : Parser(String.Utf8, Markdown)
	code = 
		Parser.const(|ext| |pre| Code({ ext, pre }))
			.keep(
				Parser.one_of(
					[
						# parse backticks with ext e.g. ```roc
						Parser.const(|i| i)
							.skip(String.string("```"))
							.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
							.skip(end_of_line),

						# parse just backticks e.g. ```
						Parser.const("")
							.skip(String.string("```")),
					],
				),
			)
			.keep(chomp_until_code_block_end)
}

# # temporyary parser for anything that is not yet supported
# # just parse into a TODO tag for now
todo : Parser(String.Utf8, Markdown)
todo = 
	Parser.const(|s| TODO(s))
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

expect {
	a = String.parse_str(todo, "Foo Bar")
	a == Ok(TODO("Foo Bar"))
}

expect {
	a = String.parse_str(Markdown.all, "Foo Bar\n\nBaz")
	a == Ok([TODO("Foo Bar"), TODO(""), TODO("Baz")])
}

end_of_line = Parser.one_of([String.string("\n"), String.string("\r\n")])

not_end_of_line = |b| {
	b != '\n' and b != '\r'
}

expect String.parse_str(Markdown.heading, "# Foo Bar") == Ok(Heading(One, "Foo Bar"))
expect String.parse_str(Markdown.heading, "Foo Bar\n---") == Ok(Heading(Two, "Foo Bar"))

inline_heading = 
	Parser.const(
		|level| {
			|str| {
				Heading(level, str)
			}
		},
	)
		.keep(
			Parser.one_of(
				[
					Parser.const(One).skip(String.string("# ")),
					Parser.const(Two).skip(String.string("## ")),
					Parser.const(Three).skip(String.string("### ")),
					Parser.const(Four).skip(String.string("#### ")),
					Parser.const(Five).skip(String.string("##### ")),
					Parser.const(Six).skip(String.string("###### ")),
				],
			),
		)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

expect {
	a = String.parse_str(inline_heading, "# Foo Bar")
	a == Ok(Heading(One, "Foo Bar"))
}

expect {
	a = String.parse_str_partial(inline_heading, "### Foo Bar\nBaz")
	a == Ok({ val: Heading(Three, "Foo Bar"), input: "\nBaz" })
}

two_line_heading_level_one = 
	Parser.const(
		|str| {
			Heading(One, str)
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("=="))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '='
				},
			),
		)

expect {
	a = String.parse_str(two_line_heading_level_one, "Foo Bar\n==")
	a == Ok(Heading(One, "Foo Bar"))
}

expect {
	a = String.parse_str_partial(two_line_heading_level_one, "Foo Bar\n=============\n")
	a == Ok({ val: Heading(One, "Foo Bar"), input: "\n" })
}

two_line_heading_level_two = 
	Parser.const(
		|str| {
			Heading(Two, str)
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("--"))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '-'
				},
			),
		)

expect {
	a = String.parse_str(two_line_heading_level_two, "Foo Bar\n---")
	a == Ok(Heading(Two, "Foo Bar"))
}

expect {
	a = String.parse_str_partial(two_line_heading_level_two, "Foo Bar\n-----\nApples")
	a == Ok({ val: Heading(Two, "Foo Bar"), input: "\nApples" })
}

expect String.parse_str(Markdown.link, "[roc](https://roc-lang.org)") == Ok(Link({ alt: "roc", href: "https://roc-lang.org" }))

expect {
	a = String.parse_str_partial(Markdown.link, "[roc](https://roc-lang.org)\nApples")
	a == Ok({ val: Link({ alt: "roc", href: "https://roc-lang.org" }), input: "\nApples" })
}

expect String.parse_str(Markdown.image, "![alt text](/images/logo.png)") == Ok(Image({ alt: "alt text", href: "/images/logo.png" }))

expect {
	a = String.parse_str_partial(Markdown.image, "![alt text](/images/logo.png)\nApples")
	a == Ok({ val: Image({ alt: "alt text", href: "/images/logo.png" }), input: "\nApples" })
}

# TODO: fix the following expect
# expect {
# 	text = 
# 		\\```roc
# 		\\# some code
# 		\\foo = bar
# 		\\```
#
# 	a = String.parse_str(Markdown.code, text)
# 	a == Ok(Code({ ext: "roc", pre: "# some code\nfoo = bar\n" }))
# }

chomp_until_code_block_end : Parser(String.Utf8, Str)
chomp_until_code_block_end = 
	Parser.build_primitive_parser(
		|input| {
			chomp_to_code_block_end_help({ val: List.with_capacity(1000), input })
		},
	)
		.map(String.str_from_utf8)

chomp_to_code_block_end_help : { val : String.Utf8, input : String.Utf8 } -> Parser.ParseResult(String.Utf8, String.Utf8)
chomp_to_code_block_end_help = |{ val, input }| {
	match input {
		[] => Err(ParsingFailure("expected ```, ran out of input"))
		['`', '`', '`', .. as rest] => Ok({ val, input: rest })
		[first, .. as rest] => chomp_to_code_block_end_help({ val: val.append(first), input: rest })
	}
}

expect {
	val = "".to_utf8()
	input = "some code\n```".to_utf8()
	expected = "some code\n".to_utf8()
	a = chomp_to_code_block_end_help({ val, input })
	a == Ok({ val: expected, input: [] })
}
