app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.Parser
import parser.Utf8
import parser.Yaml

# tag::decode[]
# (a) Type-directed decoding: the record type is the whole specification.
Service : { name : Str, port : U64 }

decode_service : Str -> Try(Service, [InvalidJson(Str), MissingRequiredField(Str)])
decode_service = |json| Json.parse(json)

expect decode_service("{\"name\": \"web\", \"port\": 8080}") == Ok({ name: "web", port: 8080 })
expect decode_service("{\"name\": \"web\"}") == Err(MissingRequiredField("port"))
# end::decode[]

# tag::decode-formats[]
# roc-parser's CSV and Yaml modules are formats for the same mechanism.
Planet : { name : Str, moons : U64, rings : Try(Bool, [Missing]) }

planets : Str -> Try(List(Planet), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
planets = |text| CSV.parse(text)

expect planets("name,moons\nMars,2\n") == Ok([{ name: "Mars", moons: 2, rings: Err(Missing) }])
expect planets("name\nMars\n") == Err(MissingRequiredField("moons"))

service : Str -> Try(Service, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
service = |text| Yaml.decode(text)

expect service("name: web\nport: 8080\n") == Ok({ name: "web", port: 8080 })
expect service("name: web\n") == Err(MissingRequiredField("port"))
# end::decode-formats[]

# tag::tree[]
# (b) A ready-made format parser: read the whole tree, then decide what to
# do with each key, whatever keys the file happens to contain.
top_level_keys : Str -> Try(List(Str), [NotAMapping, InvalidYaml(Yaml.Error)])
top_level_keys = |text| {
	match Yaml.parse_str(text)? {
		Mapping(entries) => Ok(entries.map(|entry| entry.key))
		_ => Err(NotAMapping)
	}
}

expect top_level_keys("name: web\nport: 8080\nextra: [1, 2]") == Ok(["name", "port", "extra"])

# The tree methods follow a path without a match per level.
port_of : Str -> Try(I64, [Missing, WrongType, InvalidYaml(Yaml.Error)])
port_of = |text| Yaml.parse_str(text)?.get_path(["server", "port"])?.as_i64()

expect port_of("server:\n  port: 8080\n") == Ok(8080)
expect port_of("server: {}\n") == Err(Missing)
# end::tree[]

# tag::combinators[]
# (c) A parser of your own: a legacy "host:port" line, where the port must
# be digits and the grammar is yours to define.
Endpoint : { host : Str, port : U64 }

endpoint : Parser(Utf8.Bytes, Endpoint)
endpoint =
	Parser.const(|host| |port| { host, port })
		.keep(Parser.chomp_while(|b| b != ':').map(Str.from_utf8_lossy))
		.skip(Utf8.codeunit(':'))
		.keep(Utf8.digits)

expect Utf8.parse_str(endpoint, "example.com:443") == Ok({ host: "example.com", port: 443 })
expect Utf8.parse_str(endpoint, "example.com:https").is_err()
# end::combinators[]

# tag::split[]
# (d) No parser at all: one separator and no nesting is a job for `Str`.
fields : Str -> List(Str)
fields = |line| line.split_on("\t")

expect fields("a\tb\tc") == ["a", "b", "c"]
# end::split[]

main! = |_args| {
	Stdout.line!(Str.inspect(decode_service("{\"name\": \"web\", \"port\": 8080}")))?
	Stdout.line!(Str.inspect(planets("name,moons\nMars,2\n")))?
	Stdout.line!(Str.inspect(service("name: web\n")))?
	Stdout.line!(Str.inspect(top_level_keys("name: web\nport: 8080\nextra: [1, 2]")))?
	Stdout.line!(Str.inspect(Utf8.parse_str(endpoint, "example.com:443")))?
	Stdout.line!(Str.inspect(fields("a\tb\tc")))
}
