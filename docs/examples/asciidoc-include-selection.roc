app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.AsciiDoc

import "assets/asciidoc-greet.rs" as greet_rs : Str

# tag::example[]
## Explain a problem, as a strict documentation build would.
describe : AsciiDoc.IncludeProblem -> Str
describe = |problem| {
	match problem {
		MissingTag(name) => "tag '${name}' never opens"
		UnclosedTag(name) => "tag '${name}' is not closed"
		UnmatchedEnd(name) => "end of '${name}' matches no open tag"
		InvalidLines(entry) => "lines entry '${entry}' is invalid"
		EmptySelection => "nothing is selected"
	}
}

## Select like `include::greet.rs[attributes]`, rejecting any problem.
strict : Str -> Str
strict = |attributes| {
	selection = AsciiDoc.select_include(greet_rs, attributes)
	if selection.problems.is_empty() {
		"${attributes} selects lines ${Str.inspect(selection.lines)}:\n${selection.text}"
	} else {
		"${attributes} is rejected: ${Str.join_with(selection.problems.map(describe), "; ")}"
	}
}

main! = |_| {
	for attributes in ["lines=3..4", "tag=body,indent=0", "tag=bdy", "lines=4..3"] {
		Stdout.line!(strict(attributes))?
	}
	Ok({})
}
# end::example[]
