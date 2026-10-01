app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
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

# tag::tree[]
# (b) A ready-made format parser: read the whole tree, then decide what to
# do with each key, whatever keys the file happens to contain.
top_level_keys : Str -> Try(List(Str), [NotAMapping, YamlError(Yaml.Error)])
top_level_keys = |text| {
	match Yaml.parse_str(text)? {
		Mapping(entries) => Ok(entries.map(|entry| entry.key))
		_ => Err(NotAMapping)
	}
}

expect top_level_keys("name: web\nport: 8080\nextra: [1, 2]") == Ok(["name", "port", "extra"])
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
	Stdout.line!(Str.inspect(top_level_keys("name: web\nport: 8080\nextra: [1, 2]")))?
	Stdout.line!(Str.inspect(Utf8.parse_str(endpoint, "example.com:443")))?
	Stdout.line!(Str.inspect(fields("a\tb\tc")))
}
