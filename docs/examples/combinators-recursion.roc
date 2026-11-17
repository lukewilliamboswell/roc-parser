app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
# tag::lazy[]
# A tree is a number or a bracketed, comma-separated list of trees: [1,[2,3]]
Tree := [Leaf(U64), Node(List(Tree))]

tree : {} -> Parser(Parser.Utf8.Bytes, Tree, _)
tree = |{}|
	Parser.one_of([
		Parser.Utf8.digits({}).map(|n| Leaf(n)),
		Parser.lazy(|_| tree({}))
			.sep_by(Parser.Utf8.codeunit(','))
			.between(Parser.Utf8.codeunit('['), Parser.Utf8.codeunit(']'))
			.map(|children| Node(children)),
	])

leaves : Tree -> U64
leaves = |t| {
	match t {
		Leaf(_) => 1
		Node(children) => children.fold(0, |sum, child| sum + leaves(child))
	}
}

expect (tree({})).parse_str("[1,[2,3],[]]").map_ok(leaves) == Ok(3)
# end::lazy[]

# tag::mutual[]
# A sum of two terms, where a term is a number or a bracketed sum: 1+(2+3)
term : Parser(Parser.Utf8.Bytes, U64, _)
term =
	Parser.Utf8.digits({})
		.alt(Parser.lazy(|_| expr).between(Parser.Utf8.codeunit('('), Parser.Utf8.codeunit(')')))

expr : Parser(Parser.Utf8.Bytes, U64, _)
expr =
	Parser.const(|x| |y| x + y)
		.keep(term)
		.skip(Parser.Utf8.codeunit('+'))
		.keep(term)

expect (expr).parse_str("1+(2+3)") == Ok(6)
# end::mutual[]

main! = |_args| {
	Stdout.line!(Str.inspect((tree({})).parse_str("[1,[2,3],[]]").map_ok(leaves)))?
	Stdout.line!(Str.inspect((expr).parse_str("1+(2+3)")))
}
