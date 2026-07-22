app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.0.0/AnZoxzoGPtSGQ15EQh6pBeeaHJ7aizP9MQhK81dES3Uq.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import parser.Parser
import parser.CSV
import parser.String
import cli.Stdout
import cli.Stderr

input = 
	\\Airplane!,1980,\"Robert Hays,Julie Hagerty\"
	\\Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"

MovieInfo : { title : Str, release_year : U64, actors : List(Str) }

main! : List(Str) => Try({}, _)
main! = |args| {
	csv_input = args.drop_first(1).first() ?? input

	match CSV.parse_str(movie_info_parser, csv_input) {
		Ok(movies) => {
			movies_string = 
				movies
					.map(movie_info_explanation)
					->Str.join_with("\n")

			n_movies = movies.len().to_str()

			Stdout.line!("${n_movies} movies were found:\n\n${movies_string}\n\nParse success!\n")?
		}

		Err(problem) => {
			match problem {
				ParsingFailure(failure) => {
					Stderr.line!("Parsing failure: ${failure}\n")?
				}

				ParsingIncomplete(leftover) => {
					leftover_str = 
						leftover
							.map(String.str_from_utf8)
							.map(|val| "\"${val}\"")
							->Str.join_with(", ")

					Stderr.line!("Parsing incomplete. Following leftover fields while parsing a record: ${leftover_str}\n")?
				}

				SyntaxError(error) => {
					Stderr.line!("Parsing failure. Syntax error in the CSV: ${error}")?
				}
			}
		}
	}

	Ok({})
}

movie_info_parser : Parser(CSV.CSVRecord, MovieInfo)
movie_info_parser = 
	CSV.record(
		|title| |release_year| |actors| {
			{ title, release_year, actors }
		},
	)
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(actors_parser))

actors_parser : Parser(CSV.CSVField, List(Str))
actors_parser = (CSV.string).map(
	|val| {
		val.split_on(",")
	},
)

movie_info_explanation : MovieInfo -> Str
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
