interface Markdown
    exposes [
        Markdown,
        heading,
        link,
        image,
    ]
    imports [
        Core.{ Parser, const, skip, keep, oneOf, chompWhile, map },
        String.{ Utf8, codeunit, parseStr, parseStrPartial, string, strFromUtf8 },
    ]

Level : [One, Two, Three, Four, Five, Six]

## Content values
Markdown : [
    ## Heading One "Foo Bar"
    Heading Level Str,

    ## Link "roc" "https://roc-lang.org"
    Link Str Str,

    ## Image "alt text" "/images/logo.png"
    Image Str Str,

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
    const (\label -> \href -> Link label href)
    |> skip (string "[")
    |> keep (chompWhile (\b -> b != ']') |> map strFromUtf8)
    |> skip (string "](")
    |> keep (chompWhile (\b -> b != ')') |> map strFromUtf8)
    |> skip (codeunit ')')

expect parseStr link "[roc](https://roc-lang.org)" == Ok (Link "roc" "https://roc-lang.org")

expect
    a = parseStrPartial link "[roc](https://roc-lang.org)\nApples"
    a == Ok { val: Link "roc" "https://roc-lang.org", input: "\nApples" }

## Images
##
## ```
## expect parseStr image "![alt text](/images/logo.png)" == Ok (Image "alt text" "/images/logo.png")
## ```
image : Parser Utf8 Markdown
image =
    const (\alt -> \href -> Image alt href)
    |> skip (string "![")
    |> keep (chompWhile (\b -> b != ']') |> map strFromUtf8)
    |> skip (string "](")
    |> keep (chompWhile (\b -> b != ')') |> map strFromUtf8)
    |> skip (codeunit ')')

expect parseStr image "![alt text](/images/logo.png)" == Ok (Image "alt text" "/images/logo.png")

expect
    a = parseStrPartial image "![alt text](/images/logo.png)\nApples"
    a == Ok { val: Image "alt text" "/images/logo.png", input: "\nApples" }

