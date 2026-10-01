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
## Find the value stored under `key` in a mapping.
get : Yaml, Str -> Try(Yaml, [NotFound(Str)])
get = |yaml, key| {
	match yaml {
		Mapping(entries) =>
			match entries.find_first(|entry| entry.key == key) {
				Ok(entry) => Ok(entry.value)
				Err(_) => Err(NotFound(key))
			}

		_ => Err(NotFound(key))
	}
}

summarize : Str -> Try(Str, [NotFound(Str), WrongType(Str), YamlError(Yaml.Error)])
summarize = |text| {
	config = Yaml.parse_str(text)?

	name =
		match get(config, "name")? {
			String(s) => s
			_ => return Err(WrongType("name"))
		}

	replicas =
		match get(config, "replicas")? {
			Int(n) => n
			_ => return Err(WrongType("replicas"))
		}

	owners =
		match get(config, "owners")? {
			Sequence(items) => items
			_ => return Err(WrongType("owners"))
		}

	owner_names =
		owners.map(
			|owner| {
				match get(owner, "name") {
					Ok(String(s)) => s
					_ => "?"
				}
			},
		)

	Ok("${name}: ${replicas.to_str()} replicas, owners ${Str.join_with(owner_names, " and ")}")
}

# end::navigate[]

# tag::frontmatter[]
## Split a Markdown file into its YAML frontmatter and body.
frontmatter : Str -> Try({ meta : Yaml, body : Str }, [NoFrontmatter, YamlError(Yaml.Error)])
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
		Err(YamlError({ line, column, message })) => "line ${line.to_str()}, column ${column.to_str()}: ${message}"
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

	Ok({})
}
