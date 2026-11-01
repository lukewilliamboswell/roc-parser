app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## Writes a file from the input's choices, noting the kind and byte range of each
## syntax node as it goes. Parsing must find exactly those nodes, in the same
## order, so a span cannot be off even when the tree around it is right. Nothing
## here consults the parser. Comments and newlines inside delimiters put trivia
## where a span computed from the wrong token would show.

Span : { kind : RocSyntax.NodeKind, start : U64, end : U64 }

## `trivia` is off inside an interpolation, which cannot span lines.
Out : { text : List(U8), spans : List(Span), next : U64, trivia : Bool }

Case : { source : Str, spans : List(Span) }

put : Out, Str -> Out
put = |out, text| { ..out, text: out.text.concat(text.to_utf8()) }

## Closes a node begun at `start`. Children finish first, as in the parser.
mark : Out, RocSyntax.NodeKind, U64 -> Out
mark = |out, kind, start| { ..out, spans: out.spans.append({ kind, start, end: out.text.len() }) }

token : Out, Str, RocSyntax.NodeKind -> Out
token = |out, text, kind| mark(put(out, text), kind, out.text.len())

pick : List(U8), Out, U64 -> (U64, Out)
pick = |bytes, out, count| (U8.to_u64(bytes.get(out.next) ?? 0) % count, { ..out, next: out.next + 1 })

## Trivia that is legal between tokens inside a delimiter.
gap : List(U8), Out -> Out
gap = |bytes, out| {
	(choice, after) = pick(bytes, out, 5)
	if !out.trivia or choice == 0 {
		after
	} else if choice == 1 {
		put(after, " ")
	} else if choice == 2 {
		put(after, "\n\t")
	} else if choice == 3 {
		put(after, " # note\n")
	} else {
		put(after, "\n\t# note\n\t")
	}
}

arrow : List(U8), Out -> Out
arrow = |bytes, out| {
	(choice, after) = pick(bytes, out, 2)
	put(after, if (choice == 0) " -> " else " => ")
}

smaller : U64 -> U64
smaller = |depth| if (depth == 0) 0 else depth - 1

## A parameter list and its return type. The function ends with that return type
## and begins wherever the caller noted the first argument, not at a delimiter.
function : List(U8), Out, U64, U64, Bool -> Out
function = |bytes, out, start, depth, commas| {
	first = type_atom(bytes, out, depth)
	(more, counted) = pick(bytes, first, 2)
	arguments = if more == 0 or !commas {
		counted
	} else {
		type_atom(bytes, put(counted, ", "), depth)
	}
	mark(type_atom(bytes, arrow(bytes, arguments), depth), TypeFunction, start)
}

## In a type application a comma separates arguments, so `commas` keeps a
## function written there to one parameter.
type_full : List(U8), Out, U64, Bool -> Out
type_full = |bytes, out, depth, commas| {
	(choice, after) = pick(bytes, out, 2)
	if choice == 0 {
		type_atom(bytes, after, depth)
	} else {
		function(bytes, after, after.text.len(), depth, commas)
	}
}

type_atom : List(U8), Out, U64 -> Out
type_atom = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 3 else 9)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 0 {
		token(after, "U8", TypeName)
	} else if choice == 1 {
		token(after, "a", TypeVariable)
	} else if choice == 2 {
		token(after, "Foo.Bar", TypeName)
	} else if choice == 3 {
		opened = gap(bytes, put(token(after, "List", TypeName), "("))
		mark(put(gap(bytes, type_full(bytes, opened, inner, False)), ")"), TypeApply, start)
	} else if choice == 4 {
		opened = gap(bytes, put(after, "("))
		mark(put(gap(bytes, type_full(bytes, opened, inner, True)), ")"), TypeParenthesized, start)
	} else if choice == 5 {
		opened = gap(bytes, put(after, "("))
		first = type_atom(bytes, opened, inner)
		mark(put(gap(bytes, type_atom(bytes, put(first, ", "), inner)), ")"), TypeTuple, start)
	} else if choice == 6 {
		opened = gap(bytes, put(after, "{"))
		field_start = opened.text.len()
		named = put(token(opened, "field", Identifier), " : ")
		field = mark(type_full(bytes, named, inner, True), TypeField, field_start)
		mark(put(gap(bytes, field), "}"), TypeRecord, start)
	} else if choice == 7 {
		opened = gap(bytes, put(after, "["))
		plain = put(token(opened, "None", TypeName), ", ")
		tag_start = plain.text.len()
		named = put(token(plain, "Some", TypeName), "(")
		tag = mark(put(type_full(bytes, named, inner, False), ")"), TypeApply, tag_start)
		mark(put(gap(bytes, tag), "]"), TypeTagUnion, start)
	} else {
		mark(type_atom(bytes, arrow(bytes, put(after, "()")), inner), TypeFunction, start)
	}
}

## An alias can't directly follow another without grouping.
pattern : List(U8), Out, U64 -> Out
pattern = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, 2)
	bound = pattern_atom(bytes, after, depth)
	if choice == 0 {
		bound
	} else {
		mark(token(put(bound, " as "), "whole", Identifier), AliasPattern, after.text.len())
	}
}

pattern_atom : List(U8), Out, U64 -> Out
pattern_atom = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 2 else 6)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 0 {
		token(after, "p", IdentifierPattern)
	} else if choice == 1 {
		token(after, "_", WildcardPattern)
	} else if choice == 2 {
		opened = gap(bytes, put(token(after, "Some", Identifier), "("))
		mark(put(gap(bytes, pattern(bytes, opened, inner)), ")"), TagPattern, start)
	} else if choice == 3 {
		opened = gap(bytes, put(after, "("))
		first = pattern(bytes, opened, inner)
		mark(put(gap(bytes, pattern(bytes, put(first, ", "), inner)), ")"), TuplePattern, start)
	} else if choice == 4 {
		opened = gap(bytes, put(after, "["))
		first = put(pattern(bytes, opened, inner), ", ")
		mark(put(gap(bytes, token(first, "..", RestPattern)), "]"), ListPattern, start)
	} else {
		opened = gap(bytes, put(after, "{"))
		pun_start = opened.text.len()
		pun = put(mark(token(opened, "pun", Identifier), PatternField, pun_start), ", ")
		field_start = pun.text.len()
		named = put(token(pun, "field", Identifier), ": ")
		field = mark(pattern(bytes, named, inner), PatternField, field_start)
		mark(put(gap(bytes, field), "}"), RecordPattern, start)
	}
}

## An expression that can stand beside a binary operator or a call's closing
## delimiter without grouping.
expr_atom : List(U8), Out, U64 -> Out
expr_atom = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 3 else 14)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 0 {
		token(after, "7", NumberLiteral)
	} else if choice == 1 {
		token(after, "value", Identifier)
	} else if choice == 2 {
		opened = put(after, "\"")
		segment = token(opened, "text", StringSegment)
		mark(put(segment, "\""), StringLiteral, start)
	} else if choice == 3 {
		opened = gap(bytes, put(after, "("))
		mark(put(gap(bytes, expr_full(bytes, opened, inner)), ")"), Parenthesized, start)
	} else if choice == 4 {
		opened = gap(bytes, put(after, "("))
		first = expr_atom(bytes, opened, inner)
		mark(put(gap(bytes, expr_atom(bytes, put(first, ", "), inner)), ")"), Tuple, start)
	} else if choice == 5 {
		opened = gap(bytes, put(after, "["))
		first = expr_atom(bytes, opened, inner)
		mark(put(gap(bytes, expr_atom(bytes, put(first, ", "), inner)), "]"), ListLiteral, start)
	} else if choice == 6 {
		opened = gap(bytes, put(token(after, "call", Identifier), "("))
		mark(put(gap(bytes, expr_atom(bytes, opened, inner)), ")"), Call, start)
	} else if choice == 7 {
		base = expr_atom(bytes, put(after, "("), inner)
		grouped = mark(put(base, ")"), Parenthesized, start)
		mark(token(put(grouped, "."), "field", Identifier), FieldAccess, start)
	} else if choice == 8 {
		opened = gap(bytes, put(after, "{"))
		pun_start = opened.text.len()
		pun = put(mark(token(opened, "pun", Identifier), RecordField, pun_start), ", ")
		field_start = pun.text.len()
		named = put(token(pun, "field", Identifier), ": ")
		field = mark(expr_atom(bytes, named, inner), RecordField, field_start)
		mark(put(gap(bytes, field), "}"), Record, start)
	} else if choice == 9 {
		first = token(put(after, "\""), "a", StringSegment)
		hole_start = first.text.len()
		held = expr_full(bytes, { ..put(first, "\${"), trivia: False }, inner)
		hole = mark(put({ ..held, trivia: after.trivia }, "}"), Interpolation, hole_start)
		mark(put(token(hole, "b", StringSegment), "\""), StringLiteral, start)
	} else if choice == 10 {
		mark(put(token(after, "value", Identifier), "?"), Suffix, start)
	} else if choice == 11 {
		mark(token(put(after, "-"), "value", Identifier), Unary, start)
	} else if choice == 12 {
		named = put(token(after, "value", Identifier), "->")
		call_start = named.text.len()
		opened = gap(bytes, put(token(named, "call", Identifier), "("))
		call = mark(put(gap(bytes, expr_atom(bytes, opened, inner)), ")"), Call, call_start)
		mark(call, ArrowCall, start)
	} else {
		opened = gap(bytes, put(token(after, "Box", Identifier), ".("))
		mark(put(gap(bytes, expr_atom(bytes, opened, inner)), ")"), NominalApply, start)
	}
}

## Everything `expr_atom` writes, plus forms that have to be grouped elsewhere.
expr_full : List(U8), Out, U64 -> Out
expr_full = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 1 else 6)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 0 or choice == 1 {
		expr_atom(bytes, after, depth)
	} else if choice == 2 {
		left = put(expr_atom(bytes, after, inner), " + ")
		mark(expr_atom(bytes, left, inner), Binary, start)
	} else if choice == 3 {
		bound = pattern(bytes, put(after, "|"), inner)
		mark(expr_atom(bytes, put(bound, "| "), inner), Lambda, start)
	} else if choice == 4 {
		tested = put(expr_atom(bytes, put(after, "if ("), inner), ")")
		wrapped = mark(tested, Parenthesized, start + 3)
		yes_start = wrapped.text.len() + 1
		yes = mark(put(expr_atom(bytes, put(wrapped, " ("), inner), ")"), Parenthesized, yes_start)
		mark(expr_atom(bytes, put(yes, " else "), inner), If, start)
	} else if after.trivia {
		matched = put(token(put(after, "match "), "value", Identifier), " {\n\t")
		branch_start = matched.text.len()
		bound = put(pattern(bytes, matched, inner), " => ")
		branch = mark(expr_atom(bytes, bound, inner), MatchBranch, branch_start)
		mark(put(branch, "\n}"), Match, start)
	} else {
		expr_atom(bytes, after, inner)
	}
}

statement : List(U8), Out, U64 -> Out
statement = |bytes, out, depth| {
	start = out.text.len()
	annotated = type_full(bytes, put(token(out, "name", Identifier), " : "), depth, True)
	definition_start = annotated.text.len() + 1
	marked = mark(annotated, TypeAnnotation, start)
	named = put(token(put(marked, "\n"), "name", IdentifierPattern), " = ")
	put(mark(expr_full(bytes, named, depth), Declaration, definition_start), "\n")
}

generate : List(U8) -> Case
generate = |bytes| {
	depth = U8.to_u64((bytes.get(0) ?? 0) % 5)
	count = U8.to_u64((bytes.get(1) ?? 0) % 3) + 1
	var $out = { text: [], spans: [], next: 2, trivia: True }
	for _ in 0..<count {
		$out = statement(bytes, $out, depth)
	}
	{ source: Str.from_utf8_lossy($out.text), spans: $out.spans }
}

## The tree as written: children first, without the file and its implicit header.
reachable : RocSyntax.Parsed, U64, List(Span) -> List(Span)
reachable = |parsed, id, found| match parsed.nodes.get(id) {
	Err(_) => crash "invalid node index"
	Ok(node) => {
		var $found = found
		var $offset = 0
		while $offset < node.children_len {
			$found = reachable(parsed, parsed.children.get(node.children_start + $offset) ?? parsed.nodes.len(), $found)
			$offset = $offset + 1
		}
		if node.kind == File or node.kind == Header {
			$found
		} else {
			$found.append({ kind: node.kind, start: node.start, end: node.end })
		}
	}
}

first_difference : List(Span), List(Span) -> U64
first_difference = |left, right| {
	var $index = 0
	while $index < left.len() and left.get($index) == right.get($index) {
		$index = $index + 1
	}
	$index
}

test : Case -> Fuzz.Outcome
test = |input| {
	parsed = RocSyntax.parse_str(input.source)
	if parsed.status != Complete {
		crash "generated source did not parse: ${input.source}\n${Str.inspect(parsed.diagnostics)}"
	}
	actual = reachable(parsed, parsed.root, [])
	if actual != input.spans {
		at = first_difference(input.spans, actual)
		crash "span mismatch at node ${at.to_str()}: ${input.source}\nexpected=${Str.inspect(input.spans.get(at))}\nactual=${Str.inspect(actual.get(at))}"
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-span-model", generator: Fuzz.map(Fuzz.raw_bytes, generate), test, show: |input| input.source })

expect {
	# These bytes choose a parenthesized function after a comment, which begins
	# at its first argument.
	case = generate([1, 0, 0, 4, 3, 1])
	functions = case.spans.keep_if(|span| span.kind == TypeFunction).map(|span| (span.start, span.end))
	case.source == "name : ( # note\nU8 -> U8)\nname = 7\n" and functions == [(16, 24)]
}
