app [main!] {
    cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import parser.Parser
import parser.CSV
import parser.String
import cli.Stdout
import cli.Stderr

input : Str
input =
    \\Airplane!,1980,\"Robert Hays,Julie Hagerty\"
    \\Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"

main! = |_args| {
    match CSV.parse_str(movie_info_parser, input) {
        Ok(movies) => {
            movies_string =
                movies
                .map(movie_info_explanation)
                ->Str.join_with("\n")

            n_movies = movies.len().to_str()

            Stdout.line!("${n_movies} movies were found:\n\n${movies_string}\n\nParse success!\n")
        }

        Err(problem) => {
            match problem {
                ParsingFailure(failure) => {
                    Stderr.line!("Parsing failure: ${failure}\n")
                }

                ParsingIncomplete(leftover) => {
                    leftover_str =
                        leftover
                        .map(String.str_from_utf8)
                        .map(|val| "\"${val}\"")
                        ->Str.join_with(", ")

                    Stderr.line!("Parsing incomplete. Following leftover fields while parsing a record: ${leftover_str}\n")
                }

                SyntaxError(error) => {
                    Stderr.line!("Parsing failure. Syntax error in the CSV: ${error}")
                }
            }
        }
    }

    Ok({})
}

movie_info_parser =
    CSV.record(|title| |release_year| |actors| { { title, release_year, actors } })
    .keep(CSV.field(CSV.string))
    .keep(CSV.field(CSV.u64))
    .keep(CSV.field(actors_parser))

actors_parser = (CSV.string).map(|val| { val.split_on(",") })

movie_info_explanation = |{ title, release_year, actors }| {
    enumerated_actors = enumerate(actors)
    release_year_str = release_year.to_str()

    "The movie '${title}' was released in ${release_year_str} and stars ${enumerated_actors}"
}

enumerate : List(Str) -> Str
enumerate = |elements| {
    match elements {
        [] => ""
        [actor] => actor
        [.. as inits, last] => 
            [last]
            .prepend(inits->Str.join_with(", "))
            ->Str.join_with(" and ")
    }
}
