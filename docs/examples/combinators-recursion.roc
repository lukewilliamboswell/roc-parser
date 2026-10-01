app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

# tag::lazy[]
# A tree is a number or a bracketed, comma-separated list of trees: [1,[2,3]]
Tree := [Leaf(U64), Node(List(Tree))]

tree : Parser(String.Utf8, Tree)
tree =
	String.one_of([
		String.digits.map(|n| Leaf(n)),
		Parser.lazy(|_| tree)
			.sep_by(String.codeunit(','))
			.between(String.codeunit('['), String.codeunit(']'))
			.map(|children| Node(children)),
	])

leaves : Tree -> U64
leaves = |t| {
	match t {
		Leaf(_) => 1
		Node(children) => children.fold(0, |sum, child| sum + leaves(child))
	}
}

expect String.parse_str(tree, "[1,[2,3],[]]").map_ok(leaves) == Ok(3)
# end::lazy[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(tree, "[1,[2,3],[]]").map_ok(leaves)))
}
