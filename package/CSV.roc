module [
    CSV,
    CSVRecord,
    file,
    record,
    parseStr,
    parseCSV,
    parseStrToCSVRecord,
    field,
    string,
    u64,
    f64,
]

import Core
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

## Attempts to Core.parse an `a` from a `Str` that is encoded in CSV format.
parseStr : Core.Parser CSVRecord a, Str -> Result (List a) [ParsingFailure Str, SyntaxError Str, ParsingIncomplete CSVRecord]
parseStr = \csvParser, input ->
    when parseStrToCSV input is
        Err (ParsingIncomplete rest) ->
            restStr = String.strFromUtf8 rest

            Err (SyntaxError restStr)

        Err (ParsingFailure str) ->
            Err (ParsingFailure str)

        Ok csvData ->
            when parseCSV csvParser csvData is
                Err (ParsingFailure str) ->
                    Err (ParsingFailure str)

                Err (ParsingIncomplete problem) ->
                    Err (ParsingIncomplete problem)

                Ok vals ->
                    Ok vals

## Attempts to Core.parse an `a` from a `CSV` datastructure (a list of lists of bytestring-fields).
parseCSV : Core.Parser CSVRecord a, CSV -> Result (List a) [ParsingFailure Str, ParsingIncomplete CSVRecord]
parseCSV = \csvParser, csvData ->
    csvData
    |> List.mapWithIndex (\recordFieldsList, index -> { record: recordFieldsList, index: index })
    |> List.walkUntil (Ok []) \state, { record: recordFieldsList, index: index } ->
        when parseCSVRecord csvParser recordFieldsList is
            Err (ParsingFailure problem) ->
                indexStr = Num.toStr (index + 1)
                recordStr = recordFieldsList |> List.map String.strFromUtf8 |> List.map (\val -> "\"$(val)\"") |> Str.joinWith ", "
                problemStr = "$(problem)\nWhile parsing record no. $(indexStr): `$(recordStr)`"

                Break (Err (ParsingFailure problemStr))

            Err (ParsingIncomplete problem) ->
                Break (Err (ParsingIncomplete problem))

            Ok val ->
                state
                |> Result.map (\vals -> List.append vals val)
                |> Continue

## Attempts to Core.parse an `a` from a `CSVRecord` datastructure (a list of bytestring-fields)
##
## This parser succeeds when all fields of the CSVRecord are consumed by the parser.
parseCSVRecord : Core.Parser CSVRecord a, CSVRecord -> Result a [ParsingFailure Str, ParsingIncomplete CSVRecord]
parseCSVRecord = \csvParser, recordFieldsList ->
    Core.parse csvParser recordFieldsList (\leftover -> leftover == [])

## Wrapper function to combine a set of fields into your desired `a`
##
## ```
## record (\firstName -> \lastName -> \age -> User {firstName, lastName, age})
## |> field string
## |> field string
## |> field u64
## ```
record : a -> Core.Parser CSVRecord a
record = Core.const

## Turns a parser for a `List U8` into a parser that parses part of a `CSVRecord`.
field : Core.Parser String.Utf8 a -> Core.Parser CSVRecord a
field = \fieldParser ->
    Core.buildPrimitiveParser \fieldsList ->
        when List.get fieldsList 0 is
            Err OutOfBounds ->
                Err (ParsingFailure "expected another CSV field but there are no more fields in this record")

            Ok rawStr ->
                when String.parseUtf8 fieldParser rawStr is
                    Ok val ->
                        Ok { val: val, input: List.dropFirst fieldsList 1 }

                    Err (ParsingFailure reason) ->
                        fieldStr = rawStr |> String.strFromUtf8

                        Err (ParsingFailure "Field `$(fieldStr)` could not be parsed. $(reason)")

                    Err (ParsingIncomplete reason) ->
                        reasonStr = String.strFromUtf8 reason
                        fieldsStr = fieldsList |> List.map String.strFromUtf8 |> Str.joinWith ", "

                        Err (ParsingFailure "The field parser was unable to read the whole field: `$(reasonStr)` while parsing the first field of leftover $(fieldsStr))")

## Core.Parser for a field containing a UTF8-encoded string
string : Core.Parser CSVField Str
string = String.anyString

## Parse a number from a CSV field
u64 : Core.Parser CSVField U64
u64 =
    string
    |> Core.map \val ->
        when Str.toU64 val is
            Ok num -> Ok num
            Err _ -> Err "$(val) is not a U64."
    |> Core.flatten

## Parse a 64-bit float from a CSV field
f64 : Core.Parser CSVField F64
f64 =
    string
    |> Core.map \val ->
        when Str.toF64 val is
            Ok num -> Ok num
            Err _ -> Err "$(val) is not a F64."
    |> Core.flatten

## Attempts to Core.parse a Str into the internal `CSV` datastructure (A list of lists of bytestring-fields).
parseStrToCSV : Str -> Result CSV [ParsingFailure Str, ParsingIncomplete String.Utf8]
parseStrToCSV = \input ->
    Core.parse file (Str.toUtf8 input) (\leftover -> leftover == [])

## Attempts to Core.parse a Str into the internal `CSVRecord` datastructure (A list of bytestring-fields).
parseStrToCSVRecord : Str -> Result CSVRecord [ParsingFailure Str, ParsingIncomplete String.Utf8]
parseStrToCSVRecord = \input ->
    Core.parse csvRecord (Str.toUtf8 input) (\leftover -> leftover == [])

# The following are parsers to turn strings into CSV structures
file : Core.Parser String.Utf8 CSV
file = Core.sepBy csvRecord endOfLine

csvRecord : Core.Parser String.Utf8 CSVRecord
csvRecord = Core.sepBy1 csvField comma

csvField : Core.Parser String.Utf8 CSVField
csvField = Core.alt escapedCsvField nonescapedCsvField

escapedCsvField : Core.Parser String.Utf8 CSVField
escapedCsvField = Core.between escapedContents dquote dquote

escapedContents : Core.Parser String.Utf8 (List U8)
escapedContents =
    String.oneOf [
        twodquotes |> Core.map (\_ -> '"'),
        comma,
        cr,
        lf,
        textdata,
    ]
    |> Core.many

twodquotes : Core.Parser String.Utf8 Str
twodquotes = String.string "\"\""

nonescapedCsvField : Core.Parser String.Utf8 CSVField
nonescapedCsvField = Core.many textdata

comma = String.codeunit ','
dquote = String.codeunit '"'
endOfLine = Core.alt (Core.ignore crlf) (Core.ignore lf)
cr = String.codeunit '\r'
lf = String.codeunit '\n'
crlf = String.string "\r\n"
textdata = String.codeunitSatisfies (\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126)) # Any printable char except " (34) and , (44)
