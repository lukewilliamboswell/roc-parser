module [
    Markdown,
    all,
    heading,
    link,
    image,
    code,
]

import Parser exposing [Parser]
import String

Level : [One, Two, Three, Four, Five, Six]

## Content values
Markdown : [
    Heading Level Str,
    Link { alt : Str, href : Str },
    Image { alt : Str, href : Str },
    Code { ext : Str, pre : Str },
    TODO Str,
]

all : Parser String.Utf8 (List Markdown)
all =
    [
        heading,
        link,
        image,
        code,
        todo,
    ]
    |> Parser.one_of
    |> Parser.sep_by(end_of_line)

## temporyary parser for anything that is not yet supported
## just parse into a TODO tag for now
todo : Parser String.Utf8 Markdown
todo =
    Parser.const(TODO) |> Parser.keep((Parser.chomp_while(not_end_of_line) |> Parser.map(String.str_from_utf8)))

expect
    a = String.parse_str(todo, "Foo Bar")
    a == Ok(TODO("Foo Bar"))

expect
    a = String.parse_str(all, "Foo Bar\n\nBaz")
    a == Ok([TODO("Foo Bar"), TODO(""), TODO("Baz")])

end_of_line = Parser.one_of([String.string("\n"), String.string("\r\n")])
not_end_of_line = |b| b != '\n' and b != '\r'

## Headings
##
## ```
## expect String.parseStr(heading, "# Foo Bar") == Ok(Heading One "Foo Bar")
## expect String.parseStr(heading, "Foo Bar\n---") == Ok(Heading Two "Foo Bar")
## ```
heading : Parser String.Utf8 Markdown
heading =
    Parser.one_of(
        [
            inline_heading,
            two_line_heading_level_one,
            two_line_heading_level_two,
        ],
    )

expect String.parse_str(heading, "# Foo Bar") == Ok(Heading(One, "Foo Bar"))
expect String.parse_str(heading, "Foo Bar\n---") == Ok(Heading(Two, "Foo Bar"))

inline_heading =
    Parser.const(|level| |str| Heading(level, str))
    |> Parser.keep(
        Parser.one_of(
            [
                Parser.const(One) |> Parser.skip(String.string("# ")),
                Parser.const(Two) |> Parser.skip(String.string("## ")),
                Parser.const(Three) |> Parser.skip(String.string("### ")),
                Parser.const(Four) |> Parser.skip(String.string("#### ")),
                Parser.const(Five) |> Parser.skip(String.string("##### ")),
                Parser.const(Six) |> Parser.skip(String.string("###### ")),
            ],
        ),
    )
    |> Parser.keep((Parser.chomp_while(not_end_of_line) |> Parser.map(String.str_from_utf8)))

expect
    a = String.parse_str(inline_heading, "# Foo Bar")
    a == Ok(Heading(One, "Foo Bar"))

expect
    a = String.parse_str_partial(inline_heading, "### Foo Bar\nBaz")
    a == Ok({ val: Heading(Three, "Foo Bar"), input: "\nBaz" })

two_line_heading_level_one =
    Parser.const(|str| Heading(One, str))
    |> Parser.keep((Parser.chomp_while(not_end_of_line) |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(end_of_line)
    |> Parser.skip(String.string("=="))
    |> Parser.skip(Parser.chomp_while(|b| not_end_of_line(b) and b == '='))

expect
    a = String.parse_str(two_line_heading_level_one, "Foo Bar\n==")
    a == Ok(Heading(One, "Foo Bar"))

expect
    a = String.parse_str_partial(two_line_heading_level_one, "Foo Bar\n=============\n")
    a == Ok({ val: Heading(One, "Foo Bar"), input: "\n" })

two_line_heading_level_two =
    Parser.const(|str| Heading(Two, str))
    |> Parser.keep((Parser.chomp_while(not_end_of_line) |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(end_of_line)
    |> Parser.skip(String.string("--"))
    |> Parser.skip(Parser.chomp_while(|b| not_end_of_line(b) and b == '-'))

expect
    a = String.parse_str(two_line_heading_level_two, "Foo Bar\n---")
    a == Ok(Heading(Two, "Foo Bar"))

expect
    a = String.parse_str_partial(two_line_heading_level_two, "Foo Bar\n-----\nApples")
    a == Ok({ val: Heading(Two, "Foo Bar"), input: "\nApples" })

## Links
##
## ```roc
## expect String.parse_str(link, "[roc](https://roc-lang.org)") == Ok(Link("roc", "https://roc-lang.org"))
## ```
link : Parser String.Utf8 Markdown
link =
    Parser.const(|alt| |href| Link({ alt, href }))
    |> Parser.skip(String.string("["))
    |> Parser.keep((Parser.chomp_while(|b| b != ']') |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(String.string("]("))
    |> Parser.keep((Parser.chomp_while(|b| b != ')') |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(String.codeunit(')'))

expect String.parse_str(link, "[roc](https://roc-lang.org)") == Ok(Link({ alt: "roc", href: "https://roc-lang.org" }))

expect
    a = String.parse_str_partial(link, "[roc](https://roc-lang.org)\nApples")
    a == Ok({ val: Link({ alt: "roc", href: "https://roc-lang.org" }), input: "\nApples" })

## Images
##
## ```roc
## expect String.parse_str(image, "![alt text](/images/logo.png)") == Ok(Image("alt text", "/images/logo.png"))
## ```
image : Parser String.Utf8 Markdown
image =
    Parser.const(|alt| |href| Image({ alt, href }))
    |> Parser.skip(String.string("!["))
    |> Parser.keep((Parser.chomp_while(|b| b != ']') |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(String.string("]("))
    |> Parser.keep((Parser.chomp_while(|b| b != ')') |> Parser.map(String.str_from_utf8)))
    |> Parser.skip(String.codeunit(')'))

expect String.parse_str(image, "![alt text](/images/logo.png)") == Ok(Image({ alt: "alt text", href: "/images/logo.png" }))

expect
    a = String.parse_str_partial(image, "![alt text](/images/logo.png)\nApples")
    a == Ok({ val: Image({ alt: "alt text", href: "/images/logo.png" }), input: "\nApples" })

## Parse code blocks using triple backticks
## supports block extension e.g. ```roc
##
## ```roc
## expect
##     text =
##         """
##         ```roc
##         # some code
##         foo = bar
##         ```
##         """
##
##     a = String.parse_str(code, text)
##     a == Ok(Code({ ext: "roc", pre: "# some code\nfoo = bar\n" }))
## ```
code : Parser String.Utf8 Markdown
code =

    Parser.const(|ext| |pre| Code({ ext, pre }))
    |> Parser.keep(
        Parser.one_of(
            [
                # parse backticks with ext e.g. ```roc
                Parser.const(|i| i)
                |> Parser.skip(String.string("```"))
                |> Parser.keep((Parser.chomp_while(not_end_of_line) |> Parser.map(String.str_from_utf8)))
                |> Parser.skip(end_of_line),

                # parse just backticks e.g. ```
                Parser.const("") |> Parser.skip(String.string("```")),
            ],
        ),
    )
    |> Parser.keep(chomp_until_code_block_end)

expect
    text =
        """
        ```roc
        # some code
        foo = bar
        ```
        """

    a = String.parse_str(code, text)
    a == Ok(Code({ ext: "roc", pre: "# some code\nfoo = bar\n" }))

chomp_until_code_block_end : Parser String.Utf8 Str
chomp_until_code_block_end =
    Parser.build_primitive_parser(|input| chomp_to_code_block_end_help({ val: List.with_capacity(1000), input }))
    |> Parser.map(String.str_from_utf8)

chomp_to_code_block_end_help : { val : String.Utf8, input : String.Utf8 } -> Parser.ParseResult String.Utf8 String.Utf8
chomp_to_code_block_end_help = |{ val, input }|
    when input is
        [] -> Err(ParsingFailure("expected ```, ran out of input"))
        ['`', '`', '`', .. as rest] -> Ok({ val, input: rest })
        [first, .. as rest] -> chomp_to_code_block_end_help({ val: List.append(val, first), input: rest })

expect
    val = "" |> Str.to_utf8
    input = "some code\n```" |> Str.to_utf8
    expected = "some code\n" |> Str.to_utf8
    a = chomp_to_code_block_end_help({ val, input })
    a == Ok({ val: expected, input: [] })
