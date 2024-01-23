app "example"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
        parser: "../package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        parser.Core.{ Parser, map, keep },
        parser.String.{ strFromUtf8 },
        parser.CSV.{ CSV },
    ]
    provides [main] to pf

MovieInfo := { title : Str, releaseYear : U64, actors : List Str }

input : Str
input = 
    """
    Airplane!,1980,\"Robert Hays,Julie Hagerty\"
    Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"
    """

main : Task {} *
main =
    when CSV.parseStr movieInfoParser input is
        Ok movies ->
            moviesString =
                movies
                |> List.map movieInfoExplanation
                |> Str.joinWith ("\n")
            nMovies = List.len movies |> Num.toStr

            Stdout.line "\(nMovies) movies were found:\n\n\(moviesString)\n\nParse success!\n"

        Err problem ->
            when problem is
                ParsingFailure failure ->
                    Stderr.line "Parsing failure: \(failure)\n"

                ParsingIncomplete leftover ->
                    leftoverStr = leftover |> List.map strFromUtf8 |> List.map (\val -> "\"\(val)\"") |> Str.joinWith ", "

                    Stderr.line "Parsing incomplete. Following leftover fields while parsing a record: \(leftoverStr)\n"

                SyntaxError error ->
                    Stderr.line "Parsing failure. Syntax error in the CSV: \(error)"

movieInfoParser =
    CSV.record (\title -> \releaseYear -> \actors -> @MovieInfo { title, releaseYear, actors })
    |> keep (CSV.field CSV.string)
    |> keep (CSV.field CSV.u64)
    |> keep (CSV.field actorsParser)

actorsParser =
    CSV.string
    |> map \val -> Str.split val ","

movieInfoExplanation = \@MovieInfo { title, releaseYear, actors } ->
    enumeratedActors = enumerate actors
    releaseYearStr = Num.toStr releaseYear

    "The movie '\(title)' was released in \(releaseYearStr) and stars \(enumeratedActors)"

enumerate : List Str -> Str
enumerate = \elements ->
    { before: inits, others: last } = List.split elements (List.len elements - 1)

    last
    |> List.prepend (inits |> Str.joinWith ", ")
    |> Str.joinWith " and "
