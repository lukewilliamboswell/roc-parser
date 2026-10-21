app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.Literal
import parser.RocSyntax
import parser.Utf8
import parser.Yaml

# tag::imported[]
import "assets/compile-time-settings.yaml" as settings_text : Str

Config : { name : Str, port : U16 }

config : Config
config = Literal.require(
	Yaml.validate_as({
		source: settings_text,
		name: "compile-time-settings.yaml",
		unknown_keys: Reject,
	}),
)

# end::imported[]

# tag::quoted[]
Person : { name : Str, age : U8 }

people : CSV.Decoded(Person)
people =
	\\name,age
	\\Ada,36

table : CSV
table =
	\\name,age
	\\Ada,36

# end::quoted[]

# tag::configured[]
answer = Literal.require(
	Yaml.validate({
		source: "answer: yes",
		lints: { legacy_booleans: False },
	}),
)

# end::configured[]

# tag::custom[]
Count := { value : U64 }.{
	from_quote : Str -> Try(Count, [BadQuotedBytes(Str)])
	from_quote = |source| Utf8.parse_quote({ parser: Utf8.digits, source }).map_ok(|value| Count.{ value })
}

count : Count
count = "42"
# end::custom[]

import "assets/compile-time-source.roc" as source_text : Str
source = Literal.require(RocSyntax.validate({ source: source_text, name: "compile-time-source.roc" }))

main! = |_args| {
	person = people.value.first() ?? { name: "", age: 0 }
	Stdout.line!("${config.name} on port ${config.port.to_str()}")?
	Stdout.line!("${person.name} is ${person.age.to_str()}")?
	Stdout.line!("${table.records.len().to_str()} CSV records")?
	Stdout.line!("${count.value.to_str()} from custom parser")?
	Ok({})
}

expect answer.get("answer") == Ok(Text("yes"))
expect source.consumed
