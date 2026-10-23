app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

# tag::lazy[]
# A tree is a number or a bracketed, comma-separated list of trees: [1,[2,3]]
Tree := [Leaf(U64), Node(List(Tree))]

tree : Parser(Utf8.Bytes, Tree)
tree =
	Parser.one_of([
		Utf8.digits.map(|n| Leaf(n)),
		Parser.lazy(|_| tree)
			.sep_by(Utf8.codeunit(','))
			.between(Utf8.codeunit('['), Utf8.codeunit(']'))
			.map(|children| Node(children)),
	])

leaves : Tree -> U64
leaves = |t| {
	match t {
		Leaf(_) => 1
		Node(children) => children.fold(0, |sum, child| sum + leaves(child))
	}
}

expect Utf8.parse_str(tree, "[1,[2,3],[]]").map_ok(leaves) == Ok(3)
# end::lazy[]

# tag::mutual[]
# A sum of two terms, where a term is a number or a bracketed sum: 1+(2+3)
term : Parser(Utf8.Bytes, U64)
term =
	Utf8.digits
		.alt(Parser.lazy(|_| expr).between(Utf8.codeunit('('), Utf8.codeunit(')')))

expr : Parser(Utf8.Bytes, U64)
expr =
	Parser.const(|x| |y| x + y)
		.keep(term)
		.skip(Utf8.codeunit('+'))
		.keep(term)

expect Utf8.parse_str(expr, "1+(2+3)") == Ok(6)
# end::mutual[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(tree, "[1,[2,3],[]]").map_ok(leaves)))?
	Stdout.line!(Str.inspect(Utf8.parse_str(expr, "1+(2+3)")))
}
