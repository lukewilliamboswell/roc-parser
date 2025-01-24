app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    parser: "../package/main.roc",
}

import parser.Parser as P
import parser.CSV
import parser.String
import cli.Stdout
import cli.Stderr

MovieInfo := { title : Str, release_year : U64, actors : List Str }

input : Str
input =
    """
    Airplane!,1980,\"Robert Hays,Julie Hagerty\"
    Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"
    """

main! = |_args|
    when CSV.parse_str(movie_info_parser, input) is
        Ok(movies) ->
            movies_string =
                movies
                |> List.map(movie_info_explanation)
                |> Str.join_with("\n")

            n_movies = List.len(movies) |> Num.to_str

            Stdout.line!("${n_movies} movies were found:\n\n${movies_string}\n\nParse success!\n")

        Err(problem) ->
            when problem is
                ParsingFailure(failure) ->
                    Stderr.line!("Parsing failure: ${failure}\n")

                ParsingIncomplete(leftover) ->
                    leftover_str = leftover |> List.map(String.str_from_utf8) |> List.map(|val| "\"${val}\"") |> Str.join_with(", ")

                    Stderr.line!("Parsing incomplete. Following leftover fields while parsing a record: ${leftover_str}\n")

                SyntaxError(error) ->
                    Stderr.line!("Parsing failure. Syntax error in the CSV: ${error}")

movie_info_parser =
    CSV.record(|title| |release_year| |actors| @MovieInfo({ title, release_year, actors }))
    |> P.keep(CSV.field(CSV.string))
    |> P.keep(CSV.field(CSV.u64))
    |> P.keep(CSV.field(actors_parser))

actors_parser = CSV.string |> P.map(|val| Str.split_on(val, ","))

movie_info_explanation = |@MovieInfo({ title, release_year, actors })|
    enumerated_actors = enumerate(actors)
    release_year_str = Num.to_str(release_year)

    "The movie '${title}' was released in ${release_year_str} and stars ${enumerated_actors}"

enumerate : List Str -> Str
enumerate = |elements|
    { before: inits, others: last } = List.split_at(elements, (List.len(elements) - 1))

    last
    |> List.prepend((inits |> Str.join_with(", ")))
    |> Str.join_with(" and ")
