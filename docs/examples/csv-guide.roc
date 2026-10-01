app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.CSV
import parser.Parser

# tag::raw[]
all_fields : Parser(CSV.CSVRecord, List(Str))
all_fields = Parser.many(CSV.field(CSV.string))

raw_records : Str -> Str
raw_records = |text| {
	match CSV.parse_str(all_fields, text) {
		Ok(records) =>
			records
				.map(|fields| Str.join_with(fields, " | "))
				|> Str.join_with("\n")

		Err(_) => "not valid CSV"
	}
}

# end::raw[]

# tag::typed[]
Product : { name : Str, quantity : U64, price : F64 }

product_parser : Parser(CSV.CSVRecord, Product)
product_parser =
	CSV.record(|name| |quantity| |price| { name, quantity, price })
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(CSV.f64))

# end::typed[]

# tag::errors[]
describe : Str -> Str
describe = |text| {
	match CSV.parse_str(product_parser, text) {
		Ok(products) => "${products.len().to_str()} products"
		Err(SyntaxError(rest)) => "syntax error before: ${rest}"
		Err(ParsingFailure(message)) => "bad record: ${message}"
		Err(ParsingIncomplete(fields)) => "${fields.len().to_str()} extra fields"
	}
}

# end::errors[]

# tag::print[]
print_products! : Str => Try({}, _)
print_products! = |text| {
	match CSV.parse_str(product_parser, text) {
		Ok(products) =>
			for p in products {
				Stdout.line!("${p.name}: ${p.quantity.to_str()} at ${p.price.to_str()}")?
			}

		Err(_) => Stdout.line!("could not decode products")?
	}
	Ok({})
}

# end::print[]

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	# tag::raw-run[]
	Stdout.line!(raw_records("name,note\r\nwidget,\"says \"\"hi\"\", twice\"\nempty,"))?
	# end::raw-run[]

	# tag::dialect-run[]
	Stdout.line!(raw_records("size,6\" pipe\rcount,"))?
	empty = CSV.parse_str(all_fields, "")
	Stdout.line!("empty input: ${Str.inspect(empty)}")?
	# end::dialect-run[]

	# tag::typed-run[]
	print_products!("bolt,250,0.15\nnut,+40,.05\n")?
	# end::typed-run[]

	# tag::errors-run[]
	Stdout.line!(describe("bolt,250,0.15\nnut,forty,0.05"))?
	Stdout.line!(describe("bolt,250,0.15,extra"))?
	Stdout.line!(describe("bolt,\"250"))?
	# end::errors-run[]

	Ok({})
}
