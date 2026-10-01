app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Markdown

# tag::inlines[]
show_inlines! = |text| {
	inlines = Markdown.parse_inlines(text)
	for inline in inlines {
		Stdout.line!(Str.inspect(inline))?
	}
	Ok({})
}

# end::inlines[]

# tag::references[]
document =
	\\See [the guide][guide] and ![logo][].
	\\
	\\[guide]: https://example.com/guide "The guide"
	\\[logo]: /logo.png

# end::references[]

main! = |_args| {
	# tag::calls[]
	show_inlines!("*em* **strong** ~~gone~~ `x + 1`")?
	show_inlines!("[Roc](https://roc-lang.org \"Home\") and ![a cat](cat.png)")?
	show_inlines!("<https://example.com> www.example.com me@example.com")?
	show_inlines!("one\ntwo  \nthree")?
	# end::calls[]
	Stdout.line!("--")?
	# tag::resolve[]
	blocks = Markdown.parse_str(document)
	for block in blocks {
		Stdout.line!(Str.inspect(block))?
	}
	# end::resolve[]
	Stdout.line!("--")?
	show_inlines!("See [the guide][guide].")?
	Ok({})
}
