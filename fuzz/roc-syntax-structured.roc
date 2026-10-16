app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## The generator constructs its own expression tree, then prints it. Parsing
## must recover that tree; no parser output is used to compute expectations.
Expr := [Number(Str), Name(Str), Add(Expr, Expr), Mul(Expr, Expr), Call(Expr), Text(Str), Sequence(Expr, Expr), Pair(Expr, Expr), Function(Pat, Expr), Conditional(Expr, Expr), Fields(Expr, Expr), Access(Expr), OptionalAccess(Expr), DebugExpr(Expr), CrashExpr(Expr), ReturnExpr(Expr), Guarded(Pat, Expr, Expr), Alternatives(Expr), Interpolated(Expr), TypedNumber(Str)]
Pat := [Bind(Str), Wild, Mutable(Str), Tagged(Pat), Nominal(Pat), PatPair(Pat, Pat), PatList(Pat), PatRecord(Pat), Alias(Pat)]
Case : { source : Str, expected : Expr }

build : List(U8), U64, U64 -> (Expr, U64)
build = |bytes, pos, depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 { choice % 4 } else { choice % 20 }
	if kind == 0 {
		(Number(U8.to_str(bytes.get(pos + 1) ?? 0)), pos + 2)
	} else if kind == 1 {
		(Name("v${U8.to_str(bytes.get(pos + 1) ?? 0)}"), pos + 2)
	} else if kind == 2 {
		(Text(if choice % 2 == 0 { "\"hello\"" } else { "\"two words\"" }), pos + 1)
	} else if depth == 0 or kind == 19 {
		(TypedNumber(U8.to_str(bytes.get(pos + 1) ?? 0)), pos + 2)
	} else {
		(left, next) = build(bytes, pos + 1, depth - 1)
		if kind == 3 { (Call(left), next) } else if kind == 4 {
			(pattern, end) = build_pattern(bytes, next, 2)
			(Function(pattern, left), end)
		} else if kind == 5 { (Access(left), next) } else if kind == 12 { (DebugExpr(left), next) } else if kind == 13 { (CrashExpr(left), next) } else if kind == 14 { (ReturnExpr(left), next) } else if kind == 15 { (OptionalAccess(left), next) } else if kind == 17 { (Alternatives(left), next) } else if kind == 18 { (Interpolated(left), next) } else {
			(right, end) = build(bytes, next, depth - 1)
			(if kind == 16 {
				(pattern, _) = build_pattern(bytes, end, 2)
				Guarded(pattern, left, right)
			} else if kind == 6 { Add(left, right) } else if kind == 7 { Mul(left, right) } else if kind == 8 { Sequence(left, right) } else if kind == 9 { Pair(left, right) } else if kind == 10 { Conditional(left, right) } else { Fields(left, right) }, end)
		}
	}
}

build_pattern : List(U8), U64, U64 -> (Pat, U64)
build_pattern = |bytes, pos, depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 { choice % 3 } else { choice % 9 }
	if kind == 0 { (Bind("p${U8.to_str(choice)}"), pos + 1) } else if kind == 1 { (Wild, pos + 1) } else if kind == 2 { (Mutable("count"), pos + 1) } else {
		(inner, next) = build_pattern(bytes, pos + 1, depth - 1)
		if kind == 3 { (Tagged(inner), next) } else if kind == 4 { (Nominal(inner), next) } else if kind == 5 {
			(other, end) = build_pattern(bytes, next, depth - 1)
			(PatPair(inner, other), end)
		} else if kind == 6 { (PatList(inner), next) } else if kind == 7 { (PatRecord(inner), next) } else {
			# A repeated ungrouped alias is invalid in the compiler grammar.
			base = match inner { Alias(_) => Bind("named") _ => inner }
			(Alias(base), next)
		}
	}
}

render_pattern : Pat -> Str
render_pattern = |pattern| match pattern {
	Bind(name) => name
	Wild => "_"
	Mutable(name) => "var ${name}"
	Tagged(inner) => "Some(${render_pattern(inner)})"
	Nominal(inner) => "Box.(${render_pattern(inner)})"
	PatPair(a, b) => "(${render_pattern(a)}, ${render_pattern(b)})"
	PatList(inner) => "[${render_pattern(inner)}, .. as rest]"
	PatRecord(inner) => "{ value: ${render_pattern(inner)}, other }"
	Alias(inner) => "${render_pattern(inner)} as whole"
}

render : Expr -> Str
render = |expr| match expr {
	Number(text) | Name(text) | Text(text) => text
	TypedNumber(text) => "${text}.U64"
	Interpolated(value) => "\"prefix \${${render(value)}} suffix\""
	Add(a, b) => "(${render(a)} + ${render(b)})"
	Mul(a, b) => "(${render(a)} * ${render(b)})"
	Call(arg) => "f(${render(arg)})"
	Sequence(a, b) => "[${render(a)}, ${render(b)}]"
	Pair(a, b) => "(${render(a)}, ${render(b)})"
	Function(pattern, body) => "(|${render_pattern(pattern)}| ${render(body)})"
	Conditional(a, b) => "(if True (${render(a)}) else (${render(b)}))"
	Fields(a, b) => "{ first: ${render(a)}, second: ${render(b)} }"
	Access(base) => "(${render(base)}).value"
	OptionalAccess(base) => "(${render(base)}).?value"
	DebugExpr(value) => "(dbg (${render(value)}))"
	CrashExpr(value) => "(crash (${render(value)}))"
	ReturnExpr(value) => "(return (${render(value)}))"
	Guarded(pattern, guard, body) => "(match subject { ${render_pattern(pattern)} if (${render(guard)}) => (${render(body)}) })"
	Alternatives(body) => "(match subject { Some(x) | None => (${render(body)}) })"
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
			if node.kind == File or node.kind == Parenthesized { return node.children_len == 1 and matches(parsed, child(parsed, node, 0), expr) }
			match expr {
				Number(text) => node.kind == NumberLiteral and lexeme(parsed, node) == text
				Name(text) => node.kind == Identifier and lexeme(parsed, node) == text
				Text(text) => node.kind == StringLiteral and lexeme(parsed, node) == text
				TypedNumber(text) => node.kind == TypedLiteral and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Number(text)) and matches(parsed, child(parsed, node, 1), Name("U64"))
				Interpolated(value) => {
					first = parsed.nodes.get(child(parsed, node, 0)) ?? node
					interpolation = parsed.nodes.get(child(parsed, node, 1)) ?? node
					last = parsed.nodes.get(child(parsed, node, 2)) ?? node
					node.kind == StringLiteral and node.children_len == 3 and first.kind == StringSegment and lexeme(parsed, first) == "prefix " and interpolation.kind == Interpolation and interpolation.children_len == 1 and matches(parsed, child(parsed, interpolation, 0), value) and last.kind == StringSegment and lexeme(parsed, last) == " suffix"
				}
				Sequence(a, b) | Pair(a, b) => {
					kind = match expr { Sequence(_, _) => ListLiteral _ => Tuple }
					node.kind == kind and node.children_len == 2 and matches(parsed, child(parsed, node, 0), a) and matches(parsed, child(parsed, node, 1), b)
				}
				Function(pattern, body) => node.kind == Lambda and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), pattern) and matches(parsed, child(parsed, node, 1), body)
				DebugExpr(value) | CrashExpr(value) | ReturnExpr(value) => {
					kind = match expr { DebugExpr(_) => Debug CrashExpr(_) => Crash _ => Return }
					node.kind == kind and node.children_len == 1 and matches(parsed, child(parsed, node, 0), value)
				}
				Guarded(pattern, guard, body) => {
					branch = parsed.nodes.get(child(parsed, node, 1)) ?? node
					node.kind == Match and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("subject")) and branch.kind == MatchBranch and branch.children_len == 3 and matches_pattern(parsed, child(parsed, branch, 0), pattern) and matches(parsed, child(parsed, branch, 1), guard) and matches(parsed, child(parsed, branch, 2), body)
				}
				Alternatives(body) => {
					branch = parsed.nodes.get(child(parsed, node, 1)) ?? node
					pattern = parsed.nodes.get(child(parsed, branch, 0)) ?? node
					second = parsed.nodes.get(child(parsed, pattern, 1)) ?? node
					node.kind == Match and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("subject")) and branch.kind == MatchBranch and branch.children_len == 2 and pattern.kind == AlternativePattern and pattern.children_len == 2 and matches_pattern(parsed, child(parsed, pattern, 0), Tagged(Bind("x"))) and second.kind == TagPattern and second.children_len == 1 and matches(parsed, child(parsed, second, 0), Name("None")) and matches(parsed, child(parsed, branch, 1), body)
				}
				Conditional(a, b) => node.kind == If and node.children_len == 3 and matches(parsed, child(parsed, node, 0), Name("True")) and matches(parsed, child(parsed, node, 1), a) and matches(parsed, child(parsed, node, 2), b)
				Fields(a, b) => node.kind == Record and node.children_len == 2 and matches_field(parsed, child(parsed, node, 0), "first", a) and matches_field(parsed, child(parsed, node, 1), "second", b)
				Access(base) | OptionalAccess(base) => {
					kind = match expr { Access(_) => FieldAccess _ => OptionalFieldAccess }
					node.kind == kind and node.children_len == 2 and matches(parsed, child(parsed, node, 0), base) and matches(parsed, child(parsed, node, 1), Name("value"))
				}
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

matches_pattern : RocSyntax.Parsed, U64, Pat -> Bool
matches_pattern = |parsed, id, pattern| {
	match parsed.nodes.get(id) {
		Err(_) => False
		Ok(node) => match pattern {
			Bind(name) => node.kind == IdentifierPattern and node.children_len == 0 and lexeme(parsed, node) == name
			Wild => node.kind == WildcardPattern and node.children_len == 0 and lexeme(parsed, node) == "_"
			Mutable(name) => node.kind == MutablePattern and node.children_len == 1 and matches(parsed, child(parsed, node, 0), Name(name))
			Tagged(inner) | Nominal(inner) => {
				kind = match pattern { Tagged(_) => TagPattern _ => NominalPattern }
				name = match pattern { Tagged(_) => "Some" _ => "Box" }
				node.kind == kind and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name(name)) and matches_pattern(parsed, child(parsed, node, 1), inner)
			}
			PatPair(a, b) => node.kind == TuplePattern and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), a) and matches_pattern(parsed, child(parsed, node, 1), b)
			PatList(inner) => {
				rest = parsed.nodes.get(child(parsed, node, 1)) ?? node
				node.kind == ListPattern and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), inner) and rest.kind == RestPattern and rest.children_len == 1 and matches(parsed, child(parsed, rest, 0), Name("rest"))
			}
			PatRecord(inner) => {
				field = parsed.nodes.get(child(parsed, node, 0)) ?? node
				pun = parsed.nodes.get(child(parsed, node, 1)) ?? node
				node.kind == RecordPattern and node.children_len == 2 and field.kind == PatternField and field.children_len == 2 and matches(parsed, child(parsed, field, 0), Name("value")) and matches_pattern(parsed, child(parsed, field, 1), inner) and pun.kind == PatternField and pun.children_len == 1 and matches(parsed, child(parsed, pun, 0), Name("other"))
			}
			Alias(inner) => node.kind == AliasPattern and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), inner) and matches(parsed, child(parsed, node, 1), Name("whole"))
		}
	}
}

matches_field : RocSyntax.Parsed, U64, Str, Expr -> Bool
matches_field = |parsed, id, name, expr| {
	match parsed.nodes.get(id) {
		Err(_) => False
		Ok(node) => node.kind == RecordField and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name(name)) and matches(parsed, child(parsed, node, 1), expr)
	}
}

test : Case -> Fuzz.Outcome
test = |input| {
	parsed = RocSyntax.parse_expr_str(input.source)
	if parsed.status != Complete or !matches(parsed, parsed.root, input.expected) {
		crash "generated syntax tree mismatch: ${input.source}\nexpected=${Str.inspect(input.expected)}\nactual=${Str.inspect(parsed)}"
	}
	# Ungrouped arithmetic independently requires multiplication below addition.
	precedence = RocSyntax.parse_expr_str("(${input.source}) + 2 * 3")
	if !matches(precedence, precedence.root, Add(input.expected, Mul(Number("2"), Number("3")))) { crash "wrong multiplication precedence" }
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-structured", generator: Fuzz.map(Fuzz.raw_bytes, generate), test, show: |input| input.source })
