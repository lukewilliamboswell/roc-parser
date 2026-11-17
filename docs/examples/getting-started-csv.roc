# tag::header[]
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
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

planets : Str -> Try(List(Planet), [InvalidCsv(CSV.Error)])
planets = |text| CSV.decode_str(text, {})

describe : Str -> Str
describe = |text| {
	match planets(text) {
		Ok(rows) => Str.join_with(rows.map(|p| "${p.name}: ${p.moons.to_str()} ${if (p.moons == 1) "moon" else "moons"}"), "\n")
		Err(InvalidCsv(error)) => match error.problem {
			MissingRequiredField(name) => "no ${name} column"
			_ => "${Str.inspect(error.location)}: ${Str.inspect(error.problem)}"
		}
	}
}

main! = |_args| {
	Stdout.line!(describe(input))
}
# end::body[]
