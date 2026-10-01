app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Markdown

# tag::parse[]
source =
	\\# Shopping
	\\
	\\Buy these *today*:
	\\
	\\- [x] apples
	\\- [ ] pears
	\\
	\\> Quoted
	\\
	\\```roc
	\\main = 1
	\\```
	\\
	\\| Item | Qty |
	\\| :--- | --: |
	\\| pear | 2 |
	\\
	\\***
	\\
	\\<div>raw</div>

main! = |_args| {
	blocks = Markdown.parse_str(source)
	for block in blocks {
		Stdout.line!(Str.inspect(block))?
	}
	loose_list!()
}

# end::parse[]

# tag::loose[]
loose_list! = || {
	blocks = Markdown.parse_str("3. first\n\n4. second\n")
	for block in blocks {
		Stdout.line!(Str.inspect(block))?
	}
	Ok({})
}
# end::loose[]
