# tag::header[]
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}
# end::header[]

# tag::body[]
import cli.Stdout
import parser.CSV

Planet : { name : Str, moons : U64 }

input =
	\\name,moons
	\\Mercury,0
	\\Earth,1
	\\Mars,2

planets : Str -> Try(List(Planet), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
planets = |text| CSV.parse(text)

describe : Str -> Str
describe = |text| {
	match planets(text) {
		Ok(rows) => Str.join_with(rows.map(|p| "${p.name}: ${p.moons.to_str()} moons"), "\n")
		Err(InvalidCsv(error)) => "line ${error.line.to_str()}: ${error.message}"
		Err(MissingRequiredField(name)) => "no ${name} column"
	}
}

main! = |_args| {
	Stdout.line!(describe(input))
}
# end::body[]
