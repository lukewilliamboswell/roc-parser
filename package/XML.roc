interface XML
    exposes [
        Document,
    ]
    imports [
        Core.{ Parser, oneOrMore, many, map, map2, maybe, keep, skip, const },
        String.{ Utf8, oneOf, string, codeunit, codeunitSatisfies, parseStr, strFromUtf8, anyString },
    ]

eq = \value -> \a -> a == value
and = \left, right -> \a -> (left a && right a)
range = \(start, end) -> and (\a -> a >= start) (\a -> a <= end)

neq = \value -> \a -> a != value
or = \left, right -> \a -> (left a || right a)

withDefault : Parser input (Result a [Nothing]), a -> Parser input a
withDefault = \parser, def ->
    parser |> map (\m -> Result.withDefault m def)

# Extensible Markup Language (XML) 1.0 (Fifth Edition)
# https://www.w3.org/TR/xml/
# WIP

Document : List Element

Element : [Child { name : Str, attributes : List Attribute, content : List Element }, Null]

attributeList = (const (\a -> a) |> skip (many s) |> keep attribute) |> oneOrMore

emptyElemTag : Parser (List U8) { name : Str, attributes : List Attribute, content : List Element }
emptyElemTag =
    const (\a -> \b -> { name: a, attributes: b, content: [] })
    |> skip (codeunit '<')
    |> keep name
    |> keep attributeList
    |> skip (many s)
    |> skip (string "/>")

sTag =
    const (\a -> \b -> { name: a, attributes: b })
    |> skip (codeunit '<')
    |> keep name
    |> keep attributeList
    |> skip (many s)
    |> skip (codeunit '>')

eTag =
    const (\a -> { name: a })
    |> skip (string "</")
    |> keep name
    |> skip (many s)
    |> skip (codeunit '>')

isDigit = [('0', '9')] |> List.map range
isLetter = [('A', 'Z'), ('a', 'z')] |> List.map range

isNameStartChar =
    isLetter
    |> List.concat ([':', '_'] |> List.map eq)
    |> List.concat ([('À', 'Ö'), ('Ø', 'ö')] |> List.map range)

# isNameStartChar =
#     isLetter
#     |> or (eq ':')
#     |> or (eq '_')
#     |> or (range ('À', 'Ö'))
#     |> or (range ('Ø', 'ö'))
# TODO: How to add other graphemes?
# |> or (range 'ø' '˿')
# |> or (range 'Ͱ' 'ͽ')
# |> or (range 'Ϳ' '῿')
# |> or (range '‌' '‍')
# |> or (range '⁰' '↏')
# |> or (range 'Ⰰ' '⿯')
# |> or (range '、' '퟿')
# |> or (range '豈' '﷏')
# |> or (range 'ﷰ' '�')
# |> or (range '𐀀' '󯿿')

isNameChar =
    isNameStartChar
    |> List.concat isDigit
    |> List.concat (['-', '.', '·'] |> List.map eq)
# TODO: How to add other graphemes?
# |> or (range '̀' 'ͯ')
# |> or (range '‿' '⁀')

nameStartChar = isNameStartChar |> List.map codeunitSatisfies |> oneOf
nameChar = isNameChar |> List.map codeunitSatisfies |> oneOf

name =
    const (\a -> \b -> b |> List.prepend a |> strFromUtf8)
    |> keep (nameStartChar)
    |> keep (nameChar |> oneOrMore |> maybe |> withDefault [])

isS = [' ', '\t', '\r', '\n'] |> List.map eq

s : Parser Utf8 U8
s = isS |> List.map codeunitSatisfies |> oneOf

attValue =
    [
        const (\x -> x)
        |> skip (codeunit '"')
        |> keep (neq '<' |> and (neq '&') |> and (neq '"') |> codeunitSatisfies)
        |> skip (codeunit '"'),
        const (\x -> x)
        |> skip (codeunit '\'')
        |> keep (neq '<' |> and (neq '&') |> and (neq '\'') |> codeunitSatisfies)
        |> skip (codeunit '\''),
    ]
    |> oneOf
    |> oneOrMore
    |> map strFromUtf8

Attribute : { name : Str, attValue : Str }
attribute : Parser (List U8) Attribute
attribute =
    const (\a -> \b -> { name: a, attValue: b })
    |> keep name
    |> skip (codeunit '=')
    |> keep attValue

entityRef : Parser (List U8) Str
entityRef =
    const (\x -> x)
    |> skip (codeunit '&')
    |> keep name
    |> skip (codeunit ';')

isHexDigit =
    isDigit
    |> List.concat ([('a', 'f'), ('A', 'F')] |> List.map range)

hexDigit = isHexDigit |> List.map codeunitSatisfies |> oneOf
digit = isDigit |> List.map codeunitSatisfies |> oneOf

charRef : Parser (List U8) Str
charRef =
    [
        const (\x -> x)
        |> skip (string "&#")
        |> keep (digit |> oneOrMore)
        |> skip (codeunit ';'),
        const (\x -> x)
        |> skip (string "&#x")
        |> keep (hexDigit |> oneOrMore)
        |> skip (codeunit ';'),
    ]
    |> oneOf
    |> map strFromUtf8

reference = [entityRef, charRef] |> oneOf

isChar : List (U8 -> Bool)
isChar =
    (['\t', '\n', '\r'] |> List.map eq)
    |> List.concat
        (
            [
                # (' ', '퟿'),
                # ('', '�'),
                # ('𐀀', '􏿿'),
            ]
            |> List.map range
        )

char = isChar |> List.map codeunitSatisfies |> oneOf

cDStart = string "<![CDATA["
# cData = (Char* - (Char* ']]>' Char*))
cData = anyString
cDEnd = string "]]>"

cDSect =
    (const \x -> x)
    |> skip cDStart
    |> keep cData
    |> skip cDEnd
