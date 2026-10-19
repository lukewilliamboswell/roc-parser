app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## The generator constructs its own expression tree, then prints it. Parsing
## must recover that tree; no parser output is used to compute expectations.
Expr := [Number(Str), Name(Str), Add(Expr, Expr), Mul(Expr, Expr), Call(Expr), Text(Str), Sequence(Expr, Expr), Pair(Expr, Expr), Function(Pat, Expr), Conditional(Expr, Expr), Fields(Expr, Expr), Access(Expr), OptionalAccess(Expr), DebugExpr(Expr), CrashExpr(Expr), ReturnExpr(Expr), Guarded(Pat, Expr, Expr), Alternatives(Expr), Interpolated(Expr), TypedNumber(Str), UpdateRecord(Expr, Expr), BuildRecord(Expr), NominalFields(Expr), ApplyNominal(Expr), UnsetFields(Expr), BlockExpr(List(Stmt), Expr)]

Pat := [Bind(Str), Wild, Mutable(Str), Tagged(Pat), Nominal(Pat), PatPair(Pat, Pat), PatList(Pat), PatRecord(Pat), Alias(Pat)]

Stmt := [LetStmt(Expr), VarStmt(Expr), ExpectStmt(Expr), WhileStmt(Expr), ForStmt(Pat, Expr, Expr)]

TypeModel := [NamedType(Str), VariableType, ApplyType(TypeModel), PairType(TypeModel, TypeModel), FunctionType(TypeModel, TypeModel), RecordType(TypeModel)]

FileModel : { header : U8, body_type : TypeModel, value_type : TypeModel, body : Expr }

Expected := [Expression(Expr), CompleteFile(FileModel)]

Case : { source : Str, expected : Expected }

build : List(U8), U64, U64, U64 -> (Expr, U64)
build = |bytes, pos, depth, pattern_depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 {
		choice % 4
	} else {
		choice % 25
	}
	if kind == 0 {
		(Number(U8.to_str(bytes.get(pos + 1) ?? 0)), pos + 2)
	} else if kind == 1 {
		(Name("v${U8.to_str(bytes.get(pos + 1) ?? 0)}"), pos + 2)
	} else if kind == 2 {
		(
			Text(
				if choice % 2 == 0 {
					"\"hello\""
				} else {
					"\"two words\""
				},
			),
			pos + 1,
		)
	} else if depth == 0 or kind == 19 {
		(TypedNumber(U8.to_str(bytes.get(pos + 1) ?? 0)), pos + 2)
	} else {
		(left, next) = build(bytes, pos + 1, depth - 1, pattern_depth)
		if kind == 3 {
			(Call(left), next)
		} else if kind == 4 {
			(pattern, end) = build_pattern(bytes, next, pattern_depth)
			(Function(pattern, left), end)
		} else if kind == 5 {
			(Access(left), next)
		} else if kind == 12 {
			(DebugExpr(left), next)
		} else if kind == 13 {
			(CrashExpr(left), next)
		} else if kind == 14 {
			(ReturnExpr(left), next)
		} else if kind == 15 {
			(OptionalAccess(left), next)
		} else if kind == 17 {
			(Alternatives(left), next)
		} else if kind == 18 {
			(Interpolated(left), next)
		} else if kind == 21 {
			(BuildRecord(left), next)
		} else if kind == 22 {
			(NominalFields(left), next)
		} else if kind == 23 {
			(ApplyNominal(left), next)
		} else if kind == 24 {
			(UnsetFields(left), next)
		} else {
			(right, end) = build(bytes, next, depth - 1, pattern_depth)
			(
				if kind == 16 {
					(pattern, _) = build_pattern(bytes, end, pattern_depth)
					Guarded(pattern, left, right)
				} else if kind == 20 {
					UpdateRecord(left, right)
				} else if kind == 6 {
					Add(left, right)
				} else if kind == 7 {
					Mul(left, right)
				} else if kind == 8 {
					Sequence(left, right)
				} else if kind == 9 {
					Pair(left, right)
				} else if kind == 10 {
					Conditional(left, right)
				} else {
					Fields(left, right)
				},
				end,
			)
		}
	}
}

build_pattern : List(U8), U64, U64 -> (Pat, U64)
build_pattern = |bytes, pos, depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 {
		choice % 3
	} else {
		choice % 9
	}
	if kind == 0 {
		(Bind("p${U8.to_str(choice)}"), pos + 1)
	} else if kind == 1 {
		(Wild, pos + 1)
	} else if kind == 2 {
		(Mutable("count"), pos + 1)
	} else {
		(inner, next) = build_pattern(bytes, pos + 1, depth - 1)
		if kind == 3 {
			(Tagged(inner), next)
		} else if kind == 4 {
			(Nominal(inner), next)
		} else if kind == 5 {
			(other, end) = build_pattern(bytes, next, depth - 1)
			(PatPair(inner, other), end)
		} else if kind == 6 {
			(PatList(inner), next)
		} else if kind == 7 {
			(PatRecord(inner), next)
		} else {
			# A repeated ungrouped alias is invalid in the compiler grammar.
			base = match inner {
				Alias(_) => Bind("named")
				_ => inner
			}
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
	BlockExpr(statements, last) => "{\n${Str.join_with(statements.map(render_statement), "\n")}\n${render(last)}\n}"
	Number(text) | Name(text) | Text(text) => text
	TypedNumber(text) => "${text}.U64"
	Interpolated(value) => "\"prefix \${${render(value)}} suffix\""
	Add(a, b) => "${render_operand(a, 10, False)} + ${render_operand(b, 10, True)}"
	Mul(a, b) => "${render_operand(a, 20, False)} * ${render_operand(b, 20, True)}"
	Call(arg) => "f(${render(arg)})"
	Sequence(a, b) => "[${render(a)}, ${render(b)}]"
	Pair(a, b) => "(${render(a)}, ${render(b)})"
	Function(pattern, body) => "(|${render_pattern(pattern)}| ${render(body)})"
	Conditional(a, b) => "(if True (${render(a)}) else (${render(b)}))"
	Fields(a, b) => "{ first: ${render(a)}, second: ${render(b)} }"
	UpdateRecord(base, value) => "{ ..(${render(base)}), first: ${render(value)} }"
	BuildRecord(value) => "{ first: ${render(value)} }.Box"
	NominalFields(value) => "Box.{ first: ${render(value)} }"
	ApplyNominal(value) => "Box.(${render(value)})"
	UnsetFields(value) => "{ first: _, second: ${render(value)} }"
	Access(base) => "(${render(base)}).value"
	OptionalAccess(base) => "(${render(base)}).?value"
	DebugExpr(value) => "(dbg (${render(value)}))"
	CrashExpr(value) => "(crash (${render(value)}))"
	ReturnExpr(value) => "(return (${render(value)}))"
	Guarded(pattern, guard, body) => "(match subject { ${render_pattern(pattern)} if (${render(guard)}) => (${render(body)}) })"
	Alternatives(body) => "(match subject { Some(x) | None => (${render(body)}) })"
}

render_operand : Expr, U64, Bool -> Str
render_operand = |expr, parent_precedence, right| {
	precedence = match expr {
		Add(_, _) => 10
		Mul(_, _) => 20
		_ => 30
	}
	text = render(expr)
	if precedence < parent_precedence or (right and precedence == parent_precedence) {
		"(${text})"
	} else {
		text
	}
}

build_type : List(U8), U64, U64 -> (TypeModel, U64)
build_type = |bytes, pos, depth| {
	choice = bytes.get(pos) ?? 0
	kind = if depth == 0 {
		choice % 2
	} else {
		choice % 6
	}
	if kind == 0 {
		(NamedType("U64"), pos + 1)
	} else if kind == 1 {
		(VariableType, pos + 1)
	} else {
		(left, next) = build_type(bytes, pos + 1, depth - 1)
		if kind == 2 {
			(ApplyType(left), next)
		} else if kind == 5 {
			(RecordType(left), next)
		} else {
			(right, end) = build_type(bytes, next, depth - 1)
			(if (kind == 3) PairType(left, right) else FunctionType(left, right), end)
		}
	}
}

render_type : TypeModel -> Str
render_type = |ty| match ty {
	NamedType(name) => name
	VariableType => "a"
	ApplyType(inner) => "List((${render_type(inner)}))"
	PairType(a, b) => "((${render_type(a)}), (${render_type(b)}))"
	FunctionType(a, b) => "(${render_type(a)}) -> (${render_type(b)})"
	RecordType(inner) => "{ value : ${render_type(inner)} }"
}

render_statement : Stmt -> Str
render_statement = |statement| match statement {
	LetStmt(value) => "local = ${render(value)}"
	VarStmt(value) => "var count = ${render(value)}"
	ExpectStmt(value) => "expect (${render(value)})"
	WhileStmt(value) => "while True {\n${render(value)}\n}"
	ForStmt(pattern, list, body) => "for ${render_pattern(pattern)} in [${render(list)}] {\n${render(body)}\n}"
}

build_block : List(U8), U64, U64, U64, U64 -> (Expr, U64)
build_block = |bytes, pos, depth, pattern_depth, count| {
	var $statements = []
	var $next = pos
	for _ in 0..<count {
		kind = (bytes.get($next) ?? 0) % 5
		(value, after_value) = build(bytes, $next + 1, depth, pattern_depth)
		$next = after_value
		statement = if kind == 0 {
			LetStmt(value)
		} else if kind == 1 {
			VarStmt(value)
		} else if kind == 2 {
			ExpectStmt(value)
		} else if kind == 3 {
			WhileStmt(value)
		} else {
			(pattern, after_pattern) = build_pattern(bytes, $next, pattern_depth)
			(other, after_other) = build(bytes, after_pattern, depth, pattern_depth)
			$next = after_other
			ForStmt(pattern, value, other)
		}
		$statements = $statements.append(statement)
	}
	(last, end) = build(bytes, $next, depth, pattern_depth)
	(BlockExpr($statements, last), end)
}

render_file : FileModel -> Str
render_file = |file| {
	header = if file.header == 0 {
		"module [main]"
	} else if file.header == 1 {
		"hosted [main]"
	} else {
		"app [main] { pf: platform \"platform/main.roc\" }"
	}
	"${header}\nimport Utility as Tools exposing [value]\nGenerated(a) := ${render_type(file.body_type)}\nmain : ${render_type(file.value_type)}\nmain = ${render(file.body)}\n"
}

generate : List(U8) -> Case
generate = |bytes| {
	# Mutated bytes select bounded independent recursion/sequence budgets.
	# Missing controls choose cheap leaves; all family choices remain reachable.
	family = (bytes.get(0) ?? 0) % 3
	depth = U8.to_u64((bytes.get(1) ?? 0) % 7)
	pattern_depth = U8.to_u64((bytes.get(2) ?? 0) % 5)
	type_depth = U8.to_u64((bytes.get(3) ?? 0) % 5)
	count = U8.to_u64((bytes.get(4) ?? 0) % 4) + 1
	if family == 0 {
		(expr, _) = build(bytes, 5, depth, pattern_depth)
		{ source: render(expr), expected: Expression(expr) }
	} else {
		(body, next) = build_block(bytes, 5, depth, pattern_depth, count)
		if family == 1 {
			{ source: render(body), expected: Expression(body) }
		} else {
			(body_type, after_type) = build_type(bytes, next, type_depth)
			(value_type, _) = build_type(bytes, after_type, type_depth)
			file = { header: (bytes.get(5) ?? 0) % 3, body_type, value_type, body }
			{ source: render_file(file), expected: CompleteFile(file) }
		}
	}
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
			if node.kind == File or node.kind == Parenthesized or node.kind == ExpressionStatement {
				return node.children_len == 1 and matches(parsed, child(parsed, node, 0), expr)
			}
			match expr {
				BlockExpr(statements, last) => {
					node.kind == Block and node.children_len == statements.len() + 1 and matches_statements(parsed, node, statements) and matches(parsed, child(parsed, node, statements.len()), last)
				}
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
					kind = match expr {
						Sequence(_, _) => ListLiteral
						_ => Tuple
					}
					node.kind == kind and node.children_len == 2 and matches(parsed, child(parsed, node, 0), a) and matches(parsed, child(parsed, node, 1), b)
				}
				Function(pattern, body) => node.kind == Lambda and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), pattern) and matches(parsed, child(parsed, node, 1), body)
				DebugExpr(value) | CrashExpr(value) | ReturnExpr(value) => {
					kind = match expr {
						DebugExpr(_) => Debug
						CrashExpr(_) => Crash
						_ => Return
					}
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
				UpdateRecord(base, value) => node.kind == RecordUpdate and node.children_len == 2 and matches(parsed, child(parsed, node, 0), base) and matches_field(parsed, child(parsed, node, 1), "first", value)
				BuildRecord(value) => node.kind == RecordBuilder and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("Box")) and matches_field(parsed, child(parsed, node, 1), "first", value)
				NominalFields(value) => {
					backing = parsed.nodes.get(child(parsed, node, 1)) ?? node
					node.kind == NominalRecord and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("Box")) and backing.kind == Record and backing.children_len == 1 and matches_field(parsed, child(parsed, backing, 0), "first", value)
				}
				ApplyNominal(value) => node.kind == NominalApply and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("Box")) and matches(parsed, child(parsed, node, 1), value)
				UnsetFields(value) => {
					unset = parsed.nodes.get(child(parsed, node, 0)) ?? node
					node.kind == Record and node.children_len == 2 and unset.kind == UnsetField and unset.children_len == 1 and matches(parsed, child(parsed, unset, 0), Name("first")) and matches_field(parsed, child(parsed, node, 1), "second", value)
				}
				Access(base) | OptionalAccess(base) => {
					kind = match expr {
						Access(_) => FieldAccess
						_ => OptionalFieldAccess
					}
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
				kind = match pattern {
					Tagged(_) => TagPattern
					_ => NominalPattern
				}
				name = match pattern {
					Tagged(_) => "Some"
					_ => "Box"
				}
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

named_kind : RocSyntax.Parsed, U64, RocSyntax.NodeKind, Str -> Bool
named_kind = |parsed, id, kind, text| match parsed.nodes.get(id) {
	Err(_) => False
	Ok(node) => node.kind == kind and lexeme(parsed, node) == text
}

matches_statements : RocSyntax.Parsed, RocSyntax.Node, List(Stmt) -> Bool
matches_statements = |parsed, node, statements| {
	var $ok = True
	var $index = 0
	for statement in statements {
		$ok = $ok and matches_statement(parsed, child(parsed, node, $index), statement)
		$index = $index + 1
	}
	$ok
}

matches_statement : RocSyntax.Parsed, U64, Stmt -> Bool
matches_statement = |parsed, id, statement| match parsed.nodes.get(id) {
	Err(_) => False
	Ok(node) => match statement {
		LetStmt(value) => node.kind == Declaration and node.children_len == 2 and matches_pattern(parsed, child(parsed, node, 0), Bind("local")) and matches(parsed, child(parsed, node, 1), value)
		VarStmt(value) => node.kind == VariableDeclaration and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("count")) and matches(parsed, child(parsed, node, 1), value)
		ExpectStmt(value) => node.kind == Expect and node.children_len == 1 and matches(parsed, child(parsed, node, 0), value)
		WhileStmt(value) => node.kind == While and node.children_len == 2 and matches(parsed, child(parsed, node, 0), Name("True")) and matches(parsed, child(parsed, node, 1), BlockExpr([], value))
		ForStmt(pattern, list, body) => node.kind == For and node.children_len == 3 and matches_pattern(parsed, child(parsed, node, 0), pattern) and matches_single_list(parsed, child(parsed, node, 1), list) and matches(parsed, child(parsed, node, 2), BlockExpr([], body))
	}
}

matches_single_list : RocSyntax.Parsed, U64, Expr -> Bool
matches_single_list = |parsed, id, value| match parsed.nodes.get(id) {
	Err(_) => False
	Ok(node) => node.kind == ListLiteral and node.children_len == 1 and matches(parsed, child(parsed, node, 0), value)
}

matches_type : RocSyntax.Parsed, U64, TypeModel -> Bool
matches_type = |parsed, id, ty| match parsed.nodes.get(id) {
	Err(_) => False
	Ok(node) => {
		if node.kind == TypeParenthesized {
			return node.children_len == 1 and matches_type(parsed, child(parsed, node, 0), ty)
		}
		match ty {
			NamedType(name) => node.kind == TypeName and node.children_len == 0 and lexeme(parsed, node) == name
			VariableType => node.kind == TypeVariable and node.children_len == 0 and lexeme(parsed, node) == "a"
			ApplyType(inner) => node.kind == TypeApply and node.children_len == 2 and named_kind(parsed, child(parsed, node, 0), TypeName, "List") and matches_type(parsed, child(parsed, node, 1), inner)
			PairType(a, b) => node.kind == TypeTuple and node.children_len == 2 and matches_type(parsed, child(parsed, node, 0), a) and matches_type(parsed, child(parsed, node, 1), b)
			FunctionType(a, b) => {
				left = parsed.nodes.get(child(parsed, node, 0)) ?? node
				right = parsed.nodes.get(child(parsed, node, 1)) ?? node
				gap = Str.from_utf8_lossy(parsed.source.drop_first(left.end).take_first(right.start - left.end))
				node.kind == TypeFunction and node.children_len == 2 and Str.trim(gap) == "->" and matches_type(parsed, child(parsed, node, 0), a) and matches_type(parsed, child(parsed, node, 1), b)
			}
			RecordType(inner) => {
				field = parsed.nodes.get(child(parsed, node, 0)) ?? node
				node.kind == TypeRecord and node.children_len == 1 and field.kind == TypeField and field.children_len == 2 and matches(parsed, child(parsed, field, 0), Name("value")) and matches_type(parsed, child(parsed, field, 1), inner)
			}
		}
	}
}

matches_exposed : RocSyntax.Parsed, U64, Str -> Bool
matches_exposed = |parsed, id, name| match parsed.nodes.get(id) {
	Err(_) => False
	Ok(node) => node.kind == ExposedItem and node.children_len == 1 and matches(parsed, child(parsed, node, 0), Name(name))
}

matches_file : RocSyntax.Parsed, FileModel -> Bool
matches_file = |parsed, file| {
	root = parsed.nodes.get(parsed.root) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
	if root.kind != File or root.children_len != 5 {
		return False
	}
	header = parsed.nodes.get(child(parsed, root, 0)) ?? root
	imported = parsed.nodes.get(child(parsed, root, 1)) ?? root
	alias = parsed.nodes.get(child(parsed, imported, 1)) ?? root
	type_decl = parsed.nodes.get(child(parsed, root, 2)) ?? root
	type_header = parsed.nodes.get(child(parsed, type_decl, 0)) ?? root
	annotation = parsed.nodes.get(child(parsed, root, 3)) ?? root
	declaration = parsed.nodes.get(child(parsed, root, 4)) ?? root
	header_ok = header.kind == Header and matches_exposed(parsed, child(parsed, header, 0), "main") and if file.header < 2 {
		header.children_len == 1 and lexeme(parsed, header).starts_with(if (file.header == 0) "module" else "hosted")
	} else {
		deps = parsed.nodes.get(child(parsed, header, 1)) ?? root
		field = parsed.nodes.get(child(parsed, deps, 0)) ?? root
		platform_node = parsed.nodes.get(child(parsed, field, 1)) ?? root
		header.children_len == 2 and lexeme(parsed, header).starts_with("app") and deps.kind == Dependencies and deps.children_len == 1 and field.kind == DependencyField and field.children_len == 2 and matches(parsed, child(parsed, field, 0), Name("pf")) and platform_node.kind == PlatformReference and platform_node.children_len == 1 and matches(parsed, child(parsed, platform_node, 0), Text("\"platform/main.roc\""))
	}
	header_ok and imported.kind == Import and imported.children_len == 3 and named_kind(parsed, child(parsed, imported, 0), ImportPath, "Utility") and alias.kind == ImportAlias and alias.children_len == 1 and matches(parsed, child(parsed, alias, 0), Name("Tools")) and matches_exposed(parsed, child(parsed, imported, 2), "value") and type_decl.kind == TypeDeclaration and type_decl.children_len == 2 and type_header.kind == TypeHeader and type_header.children_len == 2 and named_kind(parsed, child(parsed, type_header, 0), TypeName, "Generated") and matches_type(parsed, child(parsed, type_header, 1), VariableType) and matches_type(parsed, child(parsed, type_decl, 1), file.body_type) and annotation.kind == TypeAnnotation and annotation.children_len == 2 and matches(parsed, child(parsed, annotation, 0), Name("main")) and matches_type(parsed, child(parsed, annotation, 1), file.value_type) and declaration.kind == Declaration and declaration.children_len == 2 and matches_pattern(parsed, child(parsed, declaration, 0), Bind("main")) and matches(parsed, child(parsed, declaration, 1), file.body)
}

test : Case -> Fuzz.Outcome
test = |input| {
	parsed = match input.expected {
		Expression(_) => RocSyntax.parse_expr_str(input.source)
		CompleteFile(_) => RocSyntax.parse_str(input.source)
	}
	agrees = match input.expected {
		Expression(expr) => matches(parsed, parsed.root, expr)
		CompleteFile(file) => matches_file(parsed, file)
	}
	if parsed.status != Complete or !agrees {
		crash "generated syntax tree mismatch: ${input.source}\nexpected=${Str.inspect(input.expected)}\nactual=${Str.inspect(parsed)}"
	}
	match input.expected {
		Expression(expr) => {
			precedence = RocSyntax.parse_expr_str("(${input.source}) + 2 * 3")
			if !matches(precedence, precedence.root, Add(expr, Mul(Number("2"), Number("3")))) {
				crash "wrong multiplication precedence"
			}
		}
		CompleteFile(_) => {}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-structured", generator: Fuzz.map(Fuzz.raw_bytes, generate), test, show: |input| input.source })

expect render(Add(Number("1"), Mul(Number("2"), Number("3")))) == "1 + 2 * 3"
expect render(Mul(Add(Number("1"), Number("2")), Number("3"))) == "(1 + 2) * 3"
expect render(Add(Number("1"), Add(Number("2"), Number("3")))) == "1 + (2 + 3)"
