interface XML
    exposes [
        Document,
    ]
    imports [
        Core.{ Parser, oneOrMore, many, map, map2, maybe, keep, skip, const },
        String.{ Utf8, oneOf, string, codeunit, codeunitSatisfies, parseStr, strFromUtf8, anyString },
    ]

# Range : a -> (a, a)
# Interval : [Range, Int]

eq = \value -> \a -> a == value
neq = \value -> \a -> a != value
or = \left, right -> \a -> (left a || right a)
and = \left, right -> \a -> (left a && right a)

range = \(start, end) -> and (\a -> a >= start) (\a -> a <= end)

withDefault : Parser input (Result a [Nothing]), a -> Parser input a
withDefault = \parser, def ->
    parser |> map (\m -> Result.withDefault m def)

# Extensible Markup Language (XML) 1.0 (Fifth Edition)
# https://www.w3.org/TR/xml/

Document : List Element

Element : [Child { name : Str, attributes : List Attribute, content : List Element }, Null]

isDigit = ('0', '9') |> range
isLetter = [('A', 'Z'), ('a', 'z')] |> List.map range

# isNameStartChar2 : List (Int *, Int *)
isNameStartChar2 =
    [
        (':', ':'),
        ('_', '_'),
        ('À', 'Ö'),
        ('Ø', 'ö'),
    ]
    |> List.map range

isNameStartChar =
    isLetter
    |> or (eq ':')
    |> or (eq '_')
    |> or (range ('À', 'Ö'))
    |> or (range ('Ø', 'ö'))
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
    |> or isDigit
    |> or (eq '-')
    |> or (eq '.')
    |> or (eq '·')
# TODO: How to add other graphemes?
# |> or (range '̀' 'ͯ')
# |> or (range '‿' '⁀')

nameStartChar = isNameStartChar |> codeunitSatisfies
nameChar = isNameChar |> codeunitSatisfies

name =
    const (\a -> \b -> b |> List.prepend a |> strFromUtf8)
    |> keep (nameStartChar)
    |> keep (nameChar |> oneOrMore |> maybe |> withDefault [])

s : Parser Utf8 U8
s =
    [' ', '\t', '\r', '\n']
    |> List.map codeunit
    |> oneOf

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

entityRef =
    const (\x -> x)
    |> skip (codeunit '&')
    |> keep name
    |> skip (codeunit ';')

# charRef = [
#     const (\x -> x)
#     |> skip (string "&#")
#     |> keep (0-9)+ ';',
#     const (\x -> x)
#     |> skip (string "&#x")
#     |> keep [0-9a-fA-F]+
#     |> skip (codeunit ';')
# ] oneOf
# create a function graphemes
#  and takes a list of ranges that are valid ex in A-C
#  and takes a list of ranges that are invalid ex out D-E
