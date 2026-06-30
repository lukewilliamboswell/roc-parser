app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import cli.Stdout
import parser.String
import parser.Markdown

content : Str
content = 
	\\# Title
	\\
	\\This is some text
	\\
	\\[roc website](https://roc-lang.org)
	\\
	\\## Sub-title
	\\
	\\```roc
	\\# some code
	\\foo = bar
	\\```

main! = |_args| {
	parsed = 
		String.parse_str(Markdown.all, content)
			.map_ok(
				|nodes| {
					render_content(nodes, "")
				},
			)
			?? "PARSING ERROR"

	Stdout.line!(parsed)
	Ok({})
}

render_content : List(Markdown.Markdown), Str -> Str
render_content = |nodes, buf| {
	match nodes {
		[] =>
			buf # base case

		[Heading(level, str), .. as rest] =>
			render_content(rest, buf.concat("HEADING: ${Str.inspect(level)} ${str}\n"))

		[Link({ alt, href }), .. as rest] =>
			render_content(rest, buf.concat("LINK: alt: ${Str.inspect(alt)}, ref: ${Str.inspect(href)}\n"))

		[Image({ alt, href }), .. as rest] =>
			render_content(rest, buf.concat("IMAGE: alt: ${Str.inspect(alt)}, ref: ${Str.inspect(href)}\n"))

		[Code({ ext, pre }), .. as rest] =>
			render_content(rest, buf.concat("CODE: ext: ${Str.inspect(ext)}, pre: ${Str.inspect(pre)}\n"))

		[TODO(line), .. as rest] =>
			render_content(rest, buf.concat("TODO: ${line}\n"))
		}
}
