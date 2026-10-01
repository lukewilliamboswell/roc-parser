app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
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

main! = |_args| show!(post)

show! = |text| {
	blocks = Markdown.parse_str(text)
	match Markdown.frontmatter(blocks) {
		Ok(raw) => {
			Stdout.line!("raw: ${Str.inspect(raw)}")?
			meta = Yaml.parse_str(raw)?
			Stdout.line!("meta: ${meta.to_inspect()}")?
			# The frontmatter is the first block.
			Stdout.line!("body blocks: ${(blocks.len() - 1).to_str()}")?
		}

		Err(Missing) => Stdout.line!("no frontmatter")?
	}
	Ok({})
}
# end::frontmatter[]
