app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.CSV
import parser.Parser
import parser.Utf8

# tag::typed[]
Product : { name : Str, quantity : U64, price : Dec }

products : Str -> Try(List(Product), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
products = |text| CSV.parse(text)

# end::typed[]

# tag::print[]
print_products! : Str => Try({}, _)
print_products! = |text| {
	match products(text) {
		Ok(found) =>
			for p in found {
				Stdout.line!("${p.name}: ${p.quantity.to_str()} at ${p.price.to_str()}")?
			}

		Err(InvalidCsv({ line, column, message, record: _, field: _ })) =>
			Stdout.line!("line ${line.to_str()}, column ${column.to_str()}: ${message}")?

		Err(MissingRequiredField(name)) =>
			Stdout.line!("no column named ${name}")?
	}
	Ok({})
}

# end::print[]

# tag::optional[]
Contact : {
	name : Str,
	email : Try(Str, [Missing]),
	age : Try(U64, [Null]),
	status : [Active, Inactive],
}

contacts : Str -> Try(List(Contact), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
contacts = |text| CSV.parse_normalized(text)

describe_contact : Contact -> Str
describe_contact = |{ name, email, age, status }| {
	email_text =
		match email {
			Ok(address) => address
			Err(Missing) => "no email column"
		}
	age_text =
		match age {
			Ok(years) => years.to_str()
			Err(Null) => "age not given"
		}
	status_text =
		match status {
			Active => "active"
			Inactive => "inactive"
		}
	"${name} (${email_text}, ${age_text}, ${status_text})"
}

# end::optional[]

# tag::headerless[]
points : Str -> Try(List((Str, F64, F64)), [InvalidCsv(CSV.Error)])
points = |text| CSV.parse_headerless(text)

# end::headerless[]

# tag::raw[]
raw_records : Str -> Str
raw_records = |text| {
	match CSV.parse_records(text) {
		Ok(records) =>
			Str.join_with(records.map(|fields| Str.join_with(fields.map(Str.from_utf8_lossy), " | ")), "\n")

		Err(InvalidCsv({ message, line, column, record: _, field: _ })) =>
			"not valid CSV at ${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::raw[]

# tag::header[]
column_names : Str -> Str
column_names = |text| {
	match CSV.parse_records(text) {
		Ok(records) => {
			{ header, rows } = CSV.split_header(records)
			"${Str.join_with(header, ", ")}; ${rows.len().to_str()} rows"
		}

		Err(_) => "not valid CSV"
	}
}

# end::header[]

# tag::combinators[]
Movie : { title : Str, year : U64, cast : List(Str) }

cast : Parser(Utf8.Bytes, List(Str))
cast = CSV.string.map(|text| Str.split_on(text, ";"))

movie : Parser(CSV.Record, Movie)
movie =
	CSV.record(|title| |year| |actors| { title, year, cast: actors })
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(cast))

# end::combinators[]

# tag::errors[]
describe : Str -> Str
describe = |text| {
	match CSV.parse_str(movie, text) {
		Ok(movies) => "${movies.len().to_str()} movies"
		Err(InvalidCsv({ record, field, line, column, message })) =>
			"record ${record.to_str()}, field ${field.to_str()} (line ${line.to_str()}, column ${column.to_str()}): ${message}"
	}
}

# end::errors[]

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	# tag::typed-run[]
	print_products!("name,price,quantity\nbolt,0.15,250\nnut,.05,+40\n")?
	print_products!("name,price\nbolt,0.15\n")?
	print_products!("name,price,quantity\nbolt,0.15,250\nnut,0.05\n")?
	# end::typed-run[]

	# tag::optional-run[]
	found = contacts("Name,Age,Status\nAda,36,Active\nAlan,,Inactive\n") ?? []
	for contact in found {
		Stdout.line!(describe_contact(contact))?
	}
	# end::optional-run[]

	# tag::headerless-run[]
	Stdout.line!(Str.inspect(points("home,-33.87,151.21\nwork,-33.86,151.2\n")))?
	# end::headerless-run[]

	# tag::raw-run[]
	Stdout.line!(raw_records("name,note\r\nwidget,\"says \"\"hi\"\", twice\"\nempty,"))?
	# end::raw-run[]

	# tag::header-run[]
	Stdout.line!(column_names("sku,count\nA1,3\nB2,0\n"))?
	# end::header-run[]

	# tag::dialect-run[]
	Stdout.line!(raw_records("size,6\" pipe\rcount,"))?
	Stdout.line!(raw_records("bolt,\"250"))?
	Stdout.line!("empty input: ${Str.inspect(CSV.parse_records(""))}")?
	# end::dialect-run[]

	# tag::errors-run[]
	Stdout.line!(describe("Airplane!,1980,Robert Hays;Julie Hagerty"))?
	Stdout.line!(describe("Airplane!,1980,Robert Hays\nCaddyshack,soon,Bill Murray"))?
	Stdout.line!(describe("Airplane!,1980,Robert Hays,extra"))?
	# end::errors-run[]

	Ok({})
}
