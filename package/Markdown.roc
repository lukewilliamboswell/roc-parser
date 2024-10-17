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
    |> Parser.oneOf
    |> Parser.sepBy (endOfLine)

## temporyary parser for anything that is not yet supported
## just parse into a TODO tag for now
todo : Parser String.Utf8 Markdown
todo =
    Parser.const TODO |> Parser.keep (Parser.chompWhile (notEndOfLine) |> Parser.map String.strFromUtf8)

expect
    a = String.parseStr todo "Foo Bar"
    a == Ok (TODO "Foo Bar")

expect
    a = String.parseStr all "Foo Bar\n\nBaz"
    a == Ok [TODO "Foo Bar", TODO "", TODO "Baz"]

endOfLine = Parser.oneOf [String.string "\n", String.string "\r\n"]
notEndOfLine = \b -> b != '\n' && b != '\r'

## Headings
##
## ```
## expect String.parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
## expect String.parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")
## ```
heading : Parser String.Utf8 Markdown
heading =
    Parser.oneOf [
        inlineHeading,
        twoLineHeadingLevelOne,
        twoLineHeadingLevelTwo,
    ]

expect String.parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
expect String.parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")

inlineHeading =
    Parser.const (\level -> \str -> Heading level str)
    |> Parser.keep
        (
            Parser.oneOf [
                Parser.const One |> Parser.skip (String.string "# "),
                Parser.const Two |> Parser.skip (String.string "## "),
                Parser.const Three |> Parser.skip (String.string "### "),
                Parser.const Four |> Parser.skip (String.string "#### "),
                Parser.const Five |> Parser.skip (String.string "##### "),
                Parser.const Six |> Parser.skip (String.string "###### "),
            ]
        )
    |> Parser.keep (Parser.chompWhile (notEndOfLine) |> Parser.map String.strFromUtf8)

expect
    a = String.parseStr inlineHeading "# Foo Bar"
    a == Ok (Heading One "Foo Bar")

expect
    a = String.parseStrPartial inlineHeading "### Foo Bar\nBaz"
    a == Ok { val: Heading Three "Foo Bar", input: "\nBaz" }

twoLineHeadingLevelOne =
    Parser.const (\str -> Heading One str)
    |> Parser.keep (Parser.chompWhile (notEndOfLine) |> Parser.map String.strFromUtf8)
    |> Parser.skip (endOfLine)
    |> Parser.skip (String.string "==")
    |> Parser.skip (Parser.chompWhile (\b -> notEndOfLine b && b == '='))

expect
    a = String.parseStr twoLineHeadingLevelOne "Foo Bar\n=="
    a == Ok (Heading One "Foo Bar")

expect
    a = String.parseStrPartial twoLineHeadingLevelOne "Foo Bar\n=============\n"
    a == Ok { val: Heading One "Foo Bar", input: "\n" }

twoLineHeadingLevelTwo =
    Parser.const (\str -> Heading Two str)
    |> Parser.keep (Parser.chompWhile (notEndOfLine) |> Parser.map String.strFromUtf8)
    |> Parser.skip (endOfLine)
    |> Parser.skip (String.string "--")
    |> Parser.skip (Parser.chompWhile (\b -> notEndOfLine b && b == '-'))

expect
    a = String.parseStr twoLineHeadingLevelTwo "Foo Bar\n---"
    a == Ok (Heading Two "Foo Bar")

expect
    a = String.parseStrPartial twoLineHeadingLevelTwo "Foo Bar\n-----\nApples"
    a == Ok { val: Heading Two "Foo Bar", input: "\nApples" }

## Links
##
## ```
## expect String.parseStr link "[roc](https://roc-lang.org)" == Ok (Link "roc" "https://roc-lang.org")
## ```
link : Parser String.Utf8 Markdown
link =
    Parser.const (\alt -> \href -> Link { alt, href })
    |> Parser.skip (String.string "[")
    |> Parser.keep (Parser.chompWhile (\b -> b != ']') |> Parser.map String.strFromUtf8)
    |> Parser.skip (String.string "](")
    |> Parser.keep (Parser.chompWhile (\b -> b != ')') |> Parser.map String.strFromUtf8)
    |> Parser.skip (String.codeunit ')')

expect String.parseStr link "[roc](https://roc-lang.org)" == Ok (Link { alt: "roc", href: "https://roc-lang.org" })

expect
    a = String.parseStrPartial link "[roc](https://roc-lang.org)\nApples"
    a == Ok { val: Link { alt: "roc", href: "https://roc-lang.org" }, input: "\nApples" }

## Images
##
## ```
## expect String.parseStr image "![alt text](/images/logo.png)" == Ok (Image "alt text" "/images/logo.png")
## ```
image : Parser String.Utf8 Markdown
image =
    Parser.const (\alt -> \href -> Image { alt, href })
    |> Parser.skip (String.string "![")
    |> Parser.keep (Parser.chompWhile (\b -> b != ']') |> Parser.map String.strFromUtf8)
    |> Parser.skip (String.string "](")
    |> Parser.keep (Parser.chompWhile (\b -> b != ')') |> Parser.map String.strFromUtf8)
    |> Parser.skip (String.codeunit ')')

expect String.parseStr image "![alt text](/images/logo.png)" == Ok (Image { alt: "alt text", href: "/images/logo.png" })

expect
    a = String.parseStrPartial image "![alt text](/images/logo.png)\nApples"
    a == Ok { val: Image { alt: "alt text", href: "/images/logo.png" }, input: "\nApples" }

## Parse code blocks using triple backticks
## supports block extension e.g. ```roc
##
## ```
## expect
##     text =
##         """
##         ```roc
##         # some code
##         foo = bar
##         ```
##         """
##
##     a = String.parseStr code text
##     a == Ok (Code { ext: "roc", pre: "# some code\nfoo = bar\n" })
## ```
code : Parser String.Utf8 Markdown
code =

    Parser.const (\ext -> \pre -> Code { ext, pre })
    |> Parser.keep
        (
            Parser.oneOf [
                # parse backticks with ext e.g. ```roc
                Parser.const \i -> i
                |> Parser.skip (String.string "```")
                |> Parser.keep (Parser.chompWhile notEndOfLine |> Parser.map String.strFromUtf8)
                |> Parser.skip (endOfLine),

                # parse just backticks e.g. ```
                Parser.const "" |> Parser.skip (String.string "```"),
            ]
        )
    |> Parser.keep (chompUntilCodeBlockEnd)

expect
    text =
        """
        ```roc
        # some code
        foo = bar
        ```
        """

    a = String.parseStr code text
    a == Ok (Code { ext: "roc", pre: "# some code\nfoo = bar\n" })

chompUntilCodeBlockEnd : Parser String.Utf8 Str
chompUntilCodeBlockEnd =
    Parser.buildPrimitiveParser \input -> chompToCodeBlockEndHelp { val: List.withCapacity 1000, input }
    |> Parser.map String.strFromUtf8

chompToCodeBlockEndHelp : { val : String.Utf8, input : String.Utf8 } -> Parser.ParseResult String.Utf8 String.Utf8
chompToCodeBlockEndHelp = \{ val, input } ->
    when input is
        [] -> Err (ParsingFailure "expected ```, ran out of input")
        ['`', '`', '`', .. as rest] -> Ok { val, input: rest }
        [first, .. as rest] -> chompToCodeBlockEndHelp { val: List.append val first, input: rest }

expect
    val = "" |> Str.toUtf8
    input = "some code\n```" |> Str.toUtf8
    expected = "some code\n" |> Str.toUtf8
    a = chompToCodeBlockEndHelp { val, input }
    a == Ok { val: expected, input: [] }
