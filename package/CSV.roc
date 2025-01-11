module [
    CSV,
    CSVRecord,
    file,
    record,
    parse_str,
    parse_csv,
    parse_str_to_csv_record,
    field,
    string,
    u64,
    f64,
]

import Parser exposing [Parser]
import String

## This is a CSV parser which follows RFC4180
##
## For simplicity's sake, the following things are not yet supported:
## - CSV files with headings
##
## The following however *is* supported
## - A simple LF ("\n") instead of CRLF ("\r\n") to separate records.
CSV : List CSVRecord
CSVRecord : List CSVField
CSVField : String.Utf8

## Attempts to Parser.parse an `a` from a `Str` that is encoded in CSV format.
parse_str : Parser CSVRecord a, Str -> Result (List a) [ParsingFailure Str, SyntaxError Str, ParsingIncomplete CSVRecord]
parse_str = \csv_parser, input ->
    when parse_str_to_csv(input) is
        Err(ParsingIncomplete(rest)) ->
            rest_str = String.str_from_utf8(rest)

            Err(SyntaxError(rest_str))

        Err(ParsingFailure(str)) ->
            Err(ParsingFailure(str))

        Ok(csv_data) ->
            when parse_csv(csv_parser, csv_data) is
                Err(ParsingFailure(str)) ->
                    Err(ParsingFailure(str))

                Err(ParsingIncomplete(problem)) ->
                    Err(ParsingIncomplete(problem))

                Ok(vals) ->
                    Ok(vals)

## Attempts to Parser.parse an `a` from a `CSV` datastructure (a list of lists of bytestring-fields).
parse_csv : Parser CSVRecord a, CSV -> Result (List a) [ParsingFailure Str, ParsingIncomplete CSVRecord]
parse_csv = \csv_parser, csv_data ->
    csv_data
    |> List.map_with_index(\record_fields_list, index -> { record: record_fields_list, index: index })
    |> List.walk_until(
        Ok([]),
        \state, { record: record_fields_list, index: index } ->
            when parse_csv_record(csv_parser, record_fields_list) is
                Err(ParsingFailure(problem)) ->
                    index_str = Num.to_str((index + 1))
                    record_str = record_fields_list |> List.map(String.str_from_utf8) |> List.map(\val -> "\"${val}\"") |> Str.join_with(", ")
                    problem_str = "${problem}\nWhile parsing record no. ${index_str}: `${record_str}`"

                    Break(Err(ParsingFailure(problem_str)))

                Err(ParsingIncomplete(problem)) ->
                    Break(Err(ParsingIncomplete(problem)))

                Ok(val) ->
                    state
                    |> Result.map(\vals -> List.append(vals, val))
                    |> Continue,
    )

## Attempts to Parser.parse an `a` from a `CSVRecord` datastructure (a list of bytestring-fields)
##
## This parser succeeds when all fields of the CSVRecord are consumed by the parser.
parse_csv_record : Parser CSVRecord a, CSVRecord -> Result a [ParsingFailure Str, ParsingIncomplete CSVRecord]
parse_csv_record = \csv_parser, record_fields_list ->
    Parser.parse(csv_parser, record_fields_list, \leftover -> leftover == [])

## Wrapper function to combine a set of fields into your desired `a`
##
## ```roc
## record(\first_name -> \last_name -> \age -> User({ first_name, last_name, age }))
## |> field(string)
## |> field(string)
## |> field(u64)
## ```
record : a -> Parser CSVRecord a
record = Parser.const

## Turns a parser for a `List U8` into a parser that parses part of a `CSVRecord`.
field : Parser String.Utf8 a -> Parser CSVRecord a
field = \field_parser ->
    Parser.build_primitive_parser(
        \fields_list ->
            when List.get(fields_list, 0) is
                Err(OutOfBounds) ->
                    Err(ParsingFailure("expected another CSV field but there are no more fields in this record"))

                Ok(raw_str) ->
                    when String.parse_utf8(field_parser, raw_str) is
                        Ok(val) ->
                            Ok({ val: val, input: List.drop_first(fields_list, 1) })

                        Err(ParsingFailure(reason)) ->
                            field_str = raw_str |> String.str_from_utf8

                            Err(ParsingFailure("Field `${field_str}` could not be parsed. ${reason}"))

                        Err(ParsingIncomplete(reason)) ->
                            reason_str = String.str_from_utf8(reason)
                            fields_str = fields_list |> List.map(String.str_from_utf8) |> Str.join_with(", ")

                            Err(ParsingFailure("The field parser was unable to read the whole field: `${reason_str}` while parsing the first field of leftover ${fields_str})")),
    )

## Parser for a field containing a UTF8-encoded string
string : Parser CSVField Str
string = String.any_string

## Parse a number from a CSV field
u64 : Parser CSVField U64
u64 =
    string
    |> Parser.map(
        \val ->
            when Str.to_u64(val) is
                Ok(num) -> Ok(num)
                Err(_) -> Err("${val} is not a U64."),
    )
    |> Parser.flatten

## Parse a 64-bit float from a CSV field
f64 : Parser CSVField F64
f64 =
    string
    |> Parser.map(
        \val ->
            when Str.to_f64(val) is
                Ok(num) -> Ok(num)
                Err(_) -> Err("${val} is not a F64."),
    )
    |> Parser.flatten

## Attempts to Parser.parse a Str into the internal `CSV` datastructure (A list of lists of bytestring-fields).
parse_str_to_csv : Str -> Result CSV [ParsingFailure Str, ParsingIncomplete String.Utf8]
parse_str_to_csv = \input ->
    Parser.parse(file, Str.to_utf8(input), \leftover -> leftover == [])

## Attempts to Parser.parse a Str into the internal `CSVRecord` datastructure (A list of bytestring-fields).
parse_str_to_csv_record : Str -> Result CSVRecord [ParsingFailure Str, ParsingIncomplete String.Utf8]
parse_str_to_csv_record = \input ->
    Parser.parse(csv_record, Str.to_utf8(input), \leftover -> leftover == [])

# The following are parsers to turn strings into CSV structures
file : Parser String.Utf8 CSV
file = Parser.sep_by(csv_record, end_of_line)

csv_record : Parser String.Utf8 CSVRecord
csv_record = Parser.sep_by1(csv_field, comma)

csv_field : Parser String.Utf8 CSVField
csv_field = Parser.alt(escaped_csv_field, nonescaped_csv_field)

escaped_csv_field : Parser String.Utf8 CSVField
escaped_csv_field = Parser.between(escaped_contents, dquote, dquote)

escaped_contents : Parser String.Utf8 (List U8)
escaped_contents =
    String.one_of(
        [
            twodquotes |> Parser.map(\_ -> '"'),
            comma,
            cr,
            lf,
            textdata,
        ],
    )
    |> Parser.many

twodquotes : Parser String.Utf8 Str
twodquotes = String.string("\"\"")

nonescaped_csv_field : Parser String.Utf8 CSVField
nonescaped_csv_field = Parser.many(textdata)

comma = String.codeunit(',')
dquote = String.codeunit('"')
end_of_line = Parser.alt(Parser.ignore(crlf), Parser.ignore(lf))
cr = String.codeunit('\r')
lf = String.codeunit('\n')
crlf = String.string("\r\n")
textdata = String.codeunit_satisfies(\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126)) # Any printable char except " (34) and , (44)
