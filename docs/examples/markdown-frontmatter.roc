app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Utf8
import parser.Markdown
import parser.Yaml

# tag::frontmatter[]
post =
	\\---
	\\title: Hello
	\\draft: false
	\\---
	\\# Hello
	\\
	\\First post.

main! = |_args| {
	blocks = Utf8.parse_str(Markdown.all, post)?
	match blocks {
		[Frontmatter({ raw }), .. as body] => {
			Stdout.line!("raw: ${Str.inspect(raw)}")?
			meta = Yaml.parse_str(raw)?
			Stdout.line!("meta: ${meta.to_inspect()}")?
			Stdout.line!("body blocks: ${body.len().to_str()}")?
		}

		_ => Stdout.line!("no frontmatter")?
	}
	Ok({})
}
# end::frontmatter[]
