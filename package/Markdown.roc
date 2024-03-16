interface Markdown
    exposes [
        Markdown,
        heading,
        link,
        image,
        code,
    ]
    imports [
        Core.{ Parser, ParseResult, buildPrimitiveParser, const, skip, keep, oneOf, chompWhile, map },
        String.{ Utf8, codeunit, parseStr, parseStrPartial, string, strFromUtf8 },
    ]

Level : [One, Two, Three, Four, Five, Six]

## Content values
Markdown : [
    Heading Level Str,
    Link { alt : Str, href : Str },
    Image { alt : Str, href : Str },
    Code { ext : Str, pre : Str },
]

endOfLine = oneOf [string "\n", string "\r\n"]
notEndOfLine = \b -> b != '\n' && b != '\r'

## Headings
##
## ```
## expect parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
## expect parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")
## ```
heading : Parser Utf8 Markdown
heading =
    oneOf [
        inlineHeading,
        twoLineHeadingLevelOne,
        twoLineHeadingLevelTwo,
    ]

expect parseStr heading "# Foo Bar" == Ok (Heading One "Foo Bar")
expect parseStr heading "Foo Bar\n---" == Ok (Heading Two "Foo Bar")

inlineHeading =
    const (\level -> \str -> Heading level str)
    |> keep
        (
            oneOf [
                const One |> skip (string "# "),
                const Two |> skip (string "## "),
                const Three |> skip (string "### "),
                const Four |> skip (string "#### "),
                const Five |> skip (string "##### "),
                const Six |> skip (string "###### "),
            ]
        )
    |> keep (chompWhile (notEndOfLine) |> map strFromUtf8)

expect
    a = parseStr inlineHeading "# Foo Bar"
    a == Ok (Heading One "Foo Bar")

expect
    a = parseStrPartial inlineHeading "### Foo Bar\nBaz"
    a == Ok { val: Heading Three "Foo Bar", input: "\nBaz" }

twoLineHeadingLevelOne =
    const (\str -> Heading One str)
    |> keep (chompWhile (notEndOfLine) |> map strFromUtf8)
    |> skip (endOfLine)
    |> skip (string "==")
    |> skip (chompWhile (\b -> notEndOfLine b && b == '='))

expect
    a = parseStr twoLineHeadingLevelOne "Foo Bar\n=="
    a == Ok (Heading One "Foo Bar")

expect
    a = parseStrPartial twoLineHeadingLevelOne "Foo Bar\n=============\n"
    a == Ok { val: Heading One "Foo Bar", input: "\n" }

twoLineHeadingLevelTwo =
    const (\str -> Heading Two str)
    |> keep (chompWhile (notEndOfLine) |> map strFromUtf8)
    |> skip (endOfLine)
    |> skip (string "--")
    |> skip (chompWhile (\b -> notEndOfLine b && b == '-'))

expect
    a = parseStr twoLineHeadingLevelTwo "Foo Bar\n---"
    a == Ok (Heading Two "Foo Bar")

expect
    a = parseStrPartial twoLineHeadingLevelTwo "Foo Bar\n-----\nApples"
    a == Ok { val: Heading Two "Foo Bar", input: "\nApples" }

## Links
##
## ```
## expect parseStr link "[roc](https://roc-lang.org)" == Ok (Link "roc" "https://roc-lang.org")
## ```
link : Parser Utf8 Markdown
link =
    const (\alt -> \href -> Link { alt, href })
    |> skip (string "[")
    |> keep (chompWhile (\b -> b != ']') |> map strFromUtf8)
    |> skip (string "](")
    |> keep (chompWhile (\b -> b != ')') |> map strFromUtf8)
    |> skip (codeunit ')')

expect parseStr link "[roc](https://roc-lang.org)" == Ok (Link { alt: "roc", href: "https://roc-lang.org" })

expect
    a = parseStrPartial link "[roc](https://roc-lang.org)\nApples"
    a == Ok { val: Link { alt: "roc", href: "https://roc-lang.org" }, input: "\nApples" }

## Images
##
## ```
## expect parseStr image "![alt text](/images/logo.png)" == Ok (Image "alt text" "/images/logo.png")
## ```
image : Parser Utf8 Markdown
image =
    const (\alt -> \href -> Image { alt, href })
    |> skip (string "![")
    |> keep (chompWhile (\b -> b != ']') |> map strFromUtf8)
    |> skip (string "](")
    |> keep (chompWhile (\b -> b != ')') |> map strFromUtf8)
    |> skip (codeunit ')')

expect parseStr image "![alt text](/images/logo.png)" == Ok (Image { alt: "alt text", href: "/images/logo.png" })

expect
    a = parseStrPartial image "![alt text](/images/logo.png)\nApples"
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
##     a = parseStr code text
##     a == Ok (Code { ext: "roc", pre: "# some code\nfoo = bar\n" })
## ```
code : Parser Utf8 Markdown
code =

    const (\ext -> \pre -> Code { ext, pre })
    |> keep
        (
            oneOf [
                # parse backticks with ext e.g. ```roc
                const \i -> i
                |> skip (string "```")
                |> keep (chompWhile notEndOfLine |> map strFromUtf8)
                |> skip (endOfLine),

                # parse just backticks e.g. ```
                const "" |> skip (string "```"),
            ]
        )
    |> keep (chompUntilCodeBlockEnd)

expect
    text =
        """
        ```roc
        # some code
        foo = bar
        ```
        """

    a = parseStr code text
    a == Ok (Code { ext: "roc", pre: "# some code\nfoo = bar\n" })

chompUntilCodeBlockEnd : Parser Utf8 Str
chompUntilCodeBlockEnd =
    buildPrimitiveParser \input -> chompToCodeBlockEndHelp { val: List.withCapacity 1000, input }
    |> map strFromUtf8

chompToCodeBlockEndHelp : { val : Utf8, input : Utf8 } -> ParseResult Utf8 Utf8
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
