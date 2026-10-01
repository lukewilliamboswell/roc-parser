# tag::header[]
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}
# end::header[]

# tag::body[]
import cli.Stdout
import parser.CSV
import parser.Parser

Planet : { name : Str, moons : U64 }

planet : Parser(CSV.Record, Planet)
planet =
	CSV.record(|name| |moons| { name, moons })
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))

input =
	\\Mercury,0
	\\Earth,1
	\\Mars,2

describe : Str -> Str
describe = |text| {
	match CSV.parse_str(planet, text) {
		Ok(planets) => planets.map(|p| "${p.name}: ${p.moons.to_str()} moons") |> Str.join_with("\n")
		Err(_) => "The CSV did not match the expected columns"
	}
}

main! = |_args| {
	Stdout.line!(describe(input))
}
# end::body[]
