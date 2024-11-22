app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    parser: "../package/main.roc",
}

import parser.Parser as P
import parser.CSV
import parser.String
import cli.Stdout
import cli.Stderr

MovieInfo := { title : Str, releaseYear : U64, actors : List Str }

input : Str
input =
    """
    Airplane!,1980,\"Robert Hays,Julie Hagerty\"
    Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"
    """

main =
    when CSV.parseStr movieInfoParser input is
        Ok movies ->
            moviesString =
                movies
                |> List.map movieInfoExplanation
                |> Str.joinWith ("\n")
            nMovies = List.len movies |> Num.toStr

            Stdout.line "$(nMovies) movies were found:\n\n$(moviesString)\n\nParse success!\n"

        Err problem ->
            when problem is
                ParsingFailure failure ->
                    Stderr.line "Parsing failure: $(failure)\n"

                ParsingIncomplete leftover ->
                    leftoverStr = leftover |> List.map String.strFromUtf8 |> List.map (\val -> "\"$(val)\"") |> Str.joinWith ", "

                    Stderr.line "Parsing incomplete. Following leftover fields while parsing a record: $(leftoverStr)\n"

                SyntaxError error ->
                    Stderr.line "Parsing failure. Syntax error in the CSV: $(error)"

movieInfoParser =
    CSV.record (\title -> \releaseYear -> \actors -> @MovieInfo { title, releaseYear, actors })
    |> P.keep (CSV.field CSV.string)
    |> P.keep (CSV.field CSV.u64)
    |> P.keep (CSV.field actorsParser)

actorsParser =
    CSV.string
    |> P.map \val -> Str.splitOn val ","

movieInfoExplanation = \@MovieInfo { title, releaseYear, actors } ->
    enumeratedActors = enumerate actors
    releaseYearStr = Num.toStr releaseYear

    "The movie '$(title)' was released in $(releaseYearStr) and stars $(enumeratedActors)"

enumerate : List Str -> Str
enumerate = \elements ->
    { before: inits, others: last } = List.splitAt elements (List.len elements - 1)

    last
    |> List.prepend (inits |> Str.joinWith ", ")
    |> Str.joinWith " and "
