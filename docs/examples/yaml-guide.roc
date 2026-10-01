app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.Yaml

# tag::config[]
config_text =
	\\# deployment settings
	\\name: web
	\\replicas: 3
	\\debug: false
	\\ports: [80, 443]
	\\owners:
	\\  - name: Ada
	\\    email: ada@example.com
	\\  - name: Grace

# end::config[]

# tag::navigate[]
Owner : { name : Str, email : Try(Str, [Missing]) }

Config : {
	name : Str,
	replicas : U8,
	debug : Bool,
	ports : List(U16),
	owners : List(Owner),
}

## The record type tells `Yaml.decode` what to expect.
load_config : Str -> Try(Config, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
load_config = |text| Yaml.decode(text)

summarize : Str -> Try(Str, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
summarize = |text| {
	config = load_config(text)?
	owner_names = config.owners.map(|owner| owner.name)

	Ok("${config.name}: ${config.replicas.to_str()} replicas, owners ${Str.join_with(owner_names, " and ")}")
}

# end::navigate[]

# tag::strict[]
decode_strict = Yaml.decoder({ keys: KebabCase, unknown_keys: Reject })

load : Str -> Try({ user_id : U64 }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
load = |text| decode_strict(text)

# end::strict[]
expect load("user-id: 7\n") == Ok({ user_id: 7 })
expect load("user-id: 7\nuser-name: ada\n").is_err()

# tag::tree[]
## Explore a document without a record type.
first_owner : Str -> Try(Str, [InvalidYaml(Yaml.Error), Missing, WrongType])
first_owner = |text| {
	config = Yaml.parse_str(text)?
	name = config.get_path(["owners", "0", "name"])?
	name.as_str()
}

# end::tree[]

# tag::frontmatter[]
## Split a Markdown file into its YAML frontmatter and body.
frontmatter : Str -> Try({ meta : Yaml, body : Str }, [NoFrontmatter, InvalidYaml(Yaml.Error)])
frontmatter = |markdown| {
	match Str.split_on(markdown, "\n---\n") {
		[header, .. as rest] if Str.starts_with(header, "---\n") =>
			Ok({ meta: Yaml.parse_str(header)?, body: Str.join_with(rest, "\n---\n") })

		_ => Err(NoFrontmatter)
	}
}

# end::frontmatter[]

# tag::errors[]
report : Str -> Str
report = |text| {
	match Yaml.parse_str(text) {
		Ok(value) => Yaml.to_inspect(value)
		Err(InvalidYaml({ line, column, message })) => "line ${line.to_str()}, column ${column.to_str()}: ${message}"
	}
}

# end::errors[]

show! : Str => {}
show! = |text| {
	match Yaml.parse_str(text) {
		Ok(value) => Stdout.line!(Yaml.to_inspect(value)) ?? {}
		Err(_) => Stdout.line!("error") ?? {}
	}
}

print_summary! : Str => Try({}, _)
print_summary! = |text| {
	match summarize(text) {
		Ok(summary) => Stdout.line!(summary)?
		Err(_) => Stdout.line!("invalid configuration")?
	}
	Ok({})
}

print_frontmatter! : Str => Try({}, _)
print_frontmatter! = |markdown| {
	match frontmatter(markdown) {
		Ok({ meta, body }) => {
			Stdout.line!(Yaml.to_inspect(meta))?
			Stdout.line!(body)?
		}

		Err(_) => Stdout.line!("no frontmatter")?
	}
	Ok({})
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	# tag::navigate-run[]
	print_summary!(config_text)?
	# end::navigate-run[]

	# tag::frontmatter-run[]
	print_frontmatter!("---\ntitle: Hello\ntags: [roc, yaml]\n---\n# Hello\n")?
	# end::frontmatter-run[]

	# tag::scalars-run[]
	show!("[~, null, true, FALSE, 42, -7, 0x1F, 0o17, 1.5, 6.02e23, .inf, -.Inf, .nan, 1.2.3, 1_000, 'true', 2026-10-01]")
	# end::scalars-run[]

	# tag::block-run[]
	show!(
		\\literal: |
		\\  line one
		\\  line two
		\\folded: >
		\\  joined
		\\  into one line
		\\
		\\  new paragraph
		\\stripped: |-
		\\  no final newline
		\\kept: |+
		\\  trailing blank lines kept
		\\
		\\indented: |2
		\\    starts with two spaces
		,
	)
	# end::block-run[]

	# tag::sequences-run[]
	show!(
		\\steps:
		\\- run: build
		\\  name: compile
		\\- - nested
		\\  - compact
		,
	)
	# end::sequences-run[]

	# tag::quoted-run[]
	show!(
		\\single: 'it''s # not a comment'
		\\plain: text # this is a comment
		,
	)
	show!(
		"double: \"tab\\there, \\u00e9, \\x41\"",
	)
	# end::quoted-run[]

	# tag::errors-run[]
	Stdout.line!(report("server:\n\tport: 80"))?
	Stdout.line!(report("base: &defaults\n  port: 80"))?
	Stdout.line!(report("one: 1\n---\ntwo: 2"))?
	Stdout.line!(report("name: a\nname: b"))?
	Stdout.line!(report("list: [1,\n  2]"))?
	Stdout.line!(report("1: one\n01: zero-one"))?
	# end::errors-run[]

	# tag::tree-run[]
	Stdout.line!(Str.inspect(first_owner(config_text)))?
	Stdout.line!(Str.inspect(first_owner("owners: []")))?
	# end::tree-run[]

	# tag::decode-errors-run[]
	Stdout.line!(Str.inspect(load_config("name: web\nreplicas: 300\n")))?
	Stdout.line!(Str.inspect(load_config("name: web\nreplicas: 3\ndebug: no\n")))?
	Stdout.line!(Str.inspect(load_config("name: web\nreplicas: 3\ndebug: false\nports: []\n")))?
	# end::decode-errors-run[]

	Ok({})
}
