interface Markdown
    exposes [
    ]
    imports [
        Core.{ Parser, const, skip, keep, oneOf, chompWhile, map },
        String.{ Utf8, parseStr, parseStrPartial, string, strFromUtf8 },
    ]

Level : [One, Two, Three, Four, Five, Six]

## Content values
Markdown : [
    Heading Level Str,
]

endOfLine = oneOf [string "\n", string "\r\n"]
notEndOfLine = \b -> b != '\n' && b != '\r'

## Headings
## [reference](https://www.markdownguide.org/basic-syntax/#headings)
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
