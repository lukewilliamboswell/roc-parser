module [
    Markdown,
    all,
    heading,
    link,
    image,
    code,
]

import Core
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

all : Core.Parser String.Utf8 (List Markdown)
all =
    [
        heading,
        link,
        image,
        code,
        todo,
    ]
    |> Core.oneOf
    |> Core.sepBy (endOfLine)

## temporyary parser for anything that is not yet supported
## just parse into a TODO tag for now
todo : Core.Parser String.Utf8 Markdown
todo =
    Core.const TODO |> Core.keep (Core.chompWhile (notEndOfLine) |> Core.map String.strFromUtf8)

expect
    a = String.parseStr todo "Foo Bar"
    a == Ok (TODO "Foo Bar")

expect
    a = String.parseStr all "Foo Bar\n\nBaz"
    a == Ok [TODO "Foo Bar", TODO "", TODO "Baz"]

endOfLine = Core.oneOf [String.string "\n", String.string "\r\n"]
notEndOfLine = \b -> b != '\n' && b != '\r'

## Headings
##
## ```
## expect String.parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
## expect String.parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")
## ```
heading : Core.Parser String.Utf8 Markdown
heading =
    Core.oneOf [
        inlineHeading,
        twoLineHeadingLevelOne,
        twoLineHeadingLevelTwo,
    ]

expect String.parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
expect String.parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")

inlineHeading =
    Core.const (\level -> \str -> Heading level str)
    |> Core.keep
        (
            Core.oneOf [
                Core.const One |> Core.skip (String.string "# "),
                Core.const Two |> Core.skip (String.string "## "),
                Core.const Three |> Core.skip (String.string "### "),
                Core.const Four |> Core.skip (String.string "#### "),
                Core.const Five |> Core.skip (String.string "##### "),
                Core.const Six |> Core.skip (String.string "###### "),
            ]
        )
    |> Core.keep (Core.chompWhile (notEndOfLine) |> Core.map String.strFromUtf8)

expect
    a = String.parseStr inlineHeading "# Foo Bar"
    a == Ok (Heading One "Foo Bar")

expect
    a = String.parseStrPartial inlineHeading "### Foo Bar\nBaz"
    a == Ok { val: Heading Three "Foo Bar", input: "\nBaz" }

twoLineHeadingLevelOne =
    Core.const (\str -> Heading One str)
    |> Core.keep (Core.chompWhile (notEndOfLine) |> Core.map String.strFromUtf8)
    |> Core.skip (endOfLine)
    |> Core.skip (String.string "==")
    |> Core.skip (Core.chompWhile (\b -> notEndOfLine b && b == '='))

expect
    a = String.parseStr twoLineHeadingLevelOne "Foo Bar\n=="
    a == Ok (Heading One "Foo Bar")

expect
    a = String.parseStrPartial twoLineHeadingLevelOne "Foo Bar\n=============\n"
    a == Ok { val: Heading One "Foo Bar", input: "\n" }

twoLineHeadingLevelTwo =
    Core.const (\str -> Heading Two str)
    |> Core.keep (Core.chompWhile (notEndOfLine) |> Core.map String.strFromUtf8)
    |> Core.skip (endOfLine)
    |> Core.skip (String.string "--")
    |> Core.skip (Core.chompWhile (\b -> notEndOfLine b && b == '-'))

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
link : Core.Parser String.Utf8 Markdown
link =
    Core.const (\alt -> \href -> Link { alt, href })
    |> Core.skip (String.string "[")
    |> Core.keep (Core.chompWhile (\b -> b != ']') |> Core.map String.strFromUtf8)
    |> Core.skip (String.string "](")
    |> Core.keep (Core.chompWhile (\b -> b != ')') |> Core.map String.strFromUtf8)
    |> Core.skip (String.codeunit ')')

expect String.parseStr link "[roc](https://roc-lang.org)" == Ok (Link { alt: "roc", href: "https://roc-lang.org" })

expect
    a = String.parseStrPartial link "[roc](https://roc-lang.org)\nApples"
    a == Ok { val: Link { alt: "roc", href: "https://roc-lang.org" }, input: "\nApples" }

## Images
##
## ```
## expect String.parseStr image "![alt text](/images/logo.png)" == Ok (Image "alt text" "/images/logo.png")
## ```
image : Core.Parser String.Utf8 Markdown
image =
    Core.const (\alt -> \href -> Image { alt, href })
    |> Core.skip (String.string "![")
    |> Core.keep (Core.chompWhile (\b -> b != ']') |> Core.map String.strFromUtf8)
    |> Core.skip (String.string "](")
    |> Core.keep (Core.chompWhile (\b -> b != ')') |> Core.map String.strFromUtf8)
    |> Core.skip (String.codeunit ')')

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
code : Core.Parser String.Utf8 Markdown
code =

    Core.const (\ext -> \pre -> Code { ext, pre })
    |> Core.keep
        (
            Core.oneOf [
                # parse backticks with ext e.g. ```roc
                Core.const \i -> i
                |> Core.skip (String.string "```")
                |> Core.keep (Core.chompWhile notEndOfLine |> Core.map String.strFromUtf8)
                |> Core.skip (endOfLine),

                # parse just backticks e.g. ```
                Core.const "" |> Core.skip (String.string "```"),
            ]
        )
    |> Core.keep (chompUntilCodeBlockEnd)

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

chompUntilCodeBlockEnd : Core.Parser String.Utf8 Str
chompUntilCodeBlockEnd =
    Core.buildPrimitiveParser \input -> chompToCodeBlockEndHelp { val: List.withCapacity 1000, input }
    |> Core.map String.strFromUtf8

chompToCodeBlockEndHelp : { val : String.Utf8, input : String.Utf8 } -> Core.ParseResult String.Utf8 String.Utf8
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
