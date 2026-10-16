app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## The generator constructs its own expression tree, then prints it. Parsing
## must recover that tree; no parser output is used to compute expectations.
Expr := [Number(Str), Name(Str), Add(Expr, Expr), Mul(Expr, Expr), Call(Expr)]
Case : { source : Str, expected : Expr }

build : List(U8), U64, U64 -> (Expr, U64)
build = |bytes, pos, depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 { choice % 2 } else { choice % 5 }
	if kind == 0 {
		(Number(U8.to_str(bytes.get(pos + 1) ?? 0)), pos + 2)
	} else if kind == 1 {
		(Name("v${U8.to_str(bytes.get(pos + 1) ?? 0)}"), pos + 2)
	} else {
		(left, next) = build(bytes, pos + 1, depth - 1)
		if kind == 4 { (Call(left), next) } else {
			(right, end) = build(bytes, next, depth - 1)
			(if kind == 2 { Add(left, right) } else { Mul(left, right) }, end)
		}
	}
}

render : Expr -> Str
render = |expr| match expr {
	Number(text) | Name(text) => text
	Add(a, b) => "(${render(a)} + ${render(b)})"
	Mul(a, b) => "(${render(a)} * ${render(b)})"
	Call(arg) => "f(${render(arg)})"
}

generate : List(U8) -> Case
generate = |bytes| {
	(expr, _) = build(bytes, 0, 4)
	{ source: render(expr), expected: expr }
}

child : RocSyntax.Parsed, RocSyntax.Node, U64 -> U64
child = |parsed, node, offset| parsed.children.get(node.children_start + offset) ?? parsed.nodes.len()

lexeme : RocSyntax.Parsed, RocSyntax.Node -> Str
lexeme = |parsed, node| Str.from_utf8_lossy(parsed.source.drop_first(node.start).take_first(node.end - node.start))

matches : RocSyntax.Parsed, U64, Expr -> Bool
matches = |parsed, id, expr| {
	match parsed.nodes.get(id) {
		Err(_) => False
		Ok(node) => {
			if node.kind == File or node.kind == Parenthesized { return matches(parsed, child(parsed, node, 0), expr) }
			match expr {
				Number(text) => node.kind == NumberLiteral and lexeme(parsed, node) == text
				Name(text) => node.kind == Identifier and lexeme(parsed, node) == text
				Add(a, b) | Mul(a, b) => {
					left_id = child(parsed, node, 0)
					right_id = child(parsed, node, 1)
					operator = match expr {
						Add(_, _) => "+"
						_ => "*"
					}
					left = parsed.nodes.get(left_id) ?? node
					right = parsed.nodes.get(right_id) ?? node
					gap = Str.from_utf8_lossy(parsed.source.drop_first(left.end).take_first(right.start - left.end))
					node.kind == Binary and node.children_len == 2 and Str.trim(gap) == operator and matches(parsed, left_id, a) and matches(parsed, right_id, b)
				}
				Call(arg) => node.kind == Call and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("f")) and matches(parsed, child(parsed, node, 1), arg)
			}
		}
	}
}

test : Case -> Fuzz.Outcome
test = |input| {
	parsed = RocSyntax.parse_expr_str(input.source)
	if parsed.status != Complete or !matches(parsed, parsed.root, input.expected) {
		crash "generated syntax tree mismatch: ${input.source}\nexpected=${Str.inspect(input.expected)}\nactual=${Str.inspect(parsed)}"
	}
	# Ungrouped arithmetic independently requires multiplication below addition.
	precedence = RocSyntax.parse_expr_str("${input.source} + 2 * 3")
	if !matches(precedence, precedence.root, Add(input.expected, Mul(Number("2"), Number("3")))) { crash "wrong multiplication precedence" }
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-structured", generator: Fuzz.map(Fuzz.raw_bytes, generate), test, show: |input| input.source })
