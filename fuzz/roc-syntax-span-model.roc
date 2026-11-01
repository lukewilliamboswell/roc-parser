app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.5.0/9WT86CuwM7ZJBSs8iTd1jLyrUFTTiGp5hcJE63TwswGE.tar.zst",
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
	(choice, after) = pick(bytes, out, if (depth == 0) 3 else 11)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 9 {
		token(after, "_", TypeWildcard)
	} else if choice == 10 {
		token(after, "_ignored", TypeVariable)
	} else if choice == 0 {
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

## What a match branch can test that a binding can't.
branch_pattern : List(U8), Out, U64 -> Out
branch_pattern = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, 12)
	start = after.text.len()
	if choice == 0 {
		token(after, "1", NumberLiteral)
	} else if choice == 1 {
		token(after, "1.5", NumberLiteral)
	} else if choice == 2 {
		typed(after, start, |o| token(o, "1", NumberLiteral), "U8")
	} else if choice == 3 {
		text_literal(after, "s")
	} else if choice == 4 {
		token(after, "'c'", CharacterLiteral)
	} else if choice == 5 {
		left = mark(token(after, "None", Identifier), TagPattern, start)
		right_start = left.text.len() + 3
		mark(mark(token(put(left, " | "), "Other", Identifier), TagPattern, right_start), AlternativePattern, start)
	} else if choice == 6 {
		opened = put(token(after, "Box", Identifier), ".(")
		mark(put(pattern(bytes, opened, depth), ")"), NominalPattern, start)
	} else if choice == 7 {
		record_start = start + 4
		field_start = record_start + 2
		named = token(put(token(after, "Box", Identifier), ".{ "), "pun", Identifier)
		mark(mark(put(mark(named, PatternField, field_start), " }"), RecordPattern, record_start), NominalPattern, start)
	} else if choice == 8 {
		first = put(pattern(bytes, put(after, "["), depth), ", ")
		rest_start = first.text.len()
		rest = mark(token(put(first, ".. as "), "others", Identifier), RestPattern, rest_start)
		mark(put(rest, "]"), ListPattern, start)
	} else if choice == 9 {
		first = token(put(after, "\""), "a", StringSegment)
		hole_start = first.text.len()
		hole = mark(put(token(put(first, "\${"), "b", IdentifierPattern), "}"), CapturePattern, hole_start)
		mark(put(token(hole, "c", StringSegment), "\""), PatternStringLiteral, start)
	} else if choice == 10 {
		mark(token(put(after, "var "), "\$count", Identifier), MutablePattern, start)
	} else {
		pattern(bytes, after, depth)
	}
}

## An expression that can stand beside a binary operator or a call's closing
## delimiter without grouping.
expr_atom : List(U8), Out, U64 -> Out
expr_atom = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 3 else 26)
	start = after.text.len()
	inner = smaller(depth)
	if choice >= 14 {
		expr_more(bytes, after, inner, choice - 14)
	} else if choice == 0 {
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

## A literal with a type suffix, whose literal `write` has just been noted.
typed : Out, U64, (Out -> Out), Str -> Out
typed = |out, start, write, suffix| mark(token(put(write(out), "."), suffix, Identifier), TypedLiteral, start)

## The rest of the atoms: literal spellings, postfix forms and record variants.
expr_more : List(U8), Out, U64, U64 -> Out
expr_more = |bytes, out, depth, choice| {
	start = out.text.len()
	if choice == 0 {
		typed(out, start, |o| token(o, "7", NumberLiteral), "U8")
	} else if choice == 1 {
		token(out, "1.5", NumberLiteral)
	} else if choice == 2 {
		typed(out, start, |o| token(o, "2.5", NumberLiteral), "Dec")
	} else if choice == 3 {
		token(out, "'c'", CharacterLiteral)
	} else if choice == 4 {
		typed(out, start, |o| token(o, "'c'", CharacterLiteral), "U8")
	} else if choice == 5 {
		typed(out, start, |o| text_literal(o, "s"), "Foo")
	} else if choice == 6 {
		named = put(token(out, "value", Identifier), ".?")
		mark(token(named, "field", Identifier), OptionalFieldAccess, start)
	} else if choice == 7 {
		named = put(token(out, "value", Identifier), ".")
		method = mark(token(named, "method", Identifier), FieldAccess, start)
		mark(put(expr_atom(bytes, put(method, "("), depth), ")"), Call, start)
	} else if choice == 8 {
		mark(token(put(token(out, "value", Identifier), "."), "0", Identifier), FieldAccess, start)
	} else if choice == 9 {
		written = record_field(bytes, put(token(out, "Box", Identifier), ".{ "), depth)
		record_start = start + 4
		mark(mark(put(written, " }"), Record, record_start), NominalRecord, start)
	} else if choice == 10 {
		# The record's mapper comes first among its children but last in the source.
		field_start = out.spans.len()
		written = put(record_field(bytes, put(out, "{ "), depth), " }.")
		mapper_start = written.text.len()
		mapper = { kind: Identifier, start: mapper_start, end: mapper_start + 3 }
		fields = written.spans.drop_first(field_start)
		reordered = written.spans.take_first(field_start).append(mapper).concat(fields)
		mark({ ..put(written, "Box"), spans: reordered }, RecordBuilder, start)
	} else {
		written = record_field(bytes, put(token(put(out, "{ .."), "value", Identifier), ", "), depth)
		mark(put(written, " }"), RecordUpdate, start)
	}
}

record_field : List(U8), Out, U64 -> Out
record_field = |bytes, out, depth| {
	start = out.text.len()
	named = put(token(out, "field", Identifier), ": ")
	mark(expr_atom(bytes, named, depth), RecordField, start)
}

## Everything `expr_atom` writes, plus forms that have to be grouped elsewhere.
expr_full : List(U8), Out, U64 -> Out
expr_full = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, if (depth == 0) 1 else 13)
	start = after.text.len()
	inner = smaller(depth)
	if choice == 0 or choice == 1 {
		expr_atom(bytes, after, depth)
	} else if choice == 5 and after.trivia {
		block(bytes, after, inner)
	} else if choice == 6 {
		mark(expr_atom(bytes, put(after, "dbg "), inner), Debug, start)
	} else if choice == 7 {
		mark(text_literal(put(after, "crash "), "message"), Crash, start)
	} else if choice == 8 {
		mark(expr_atom(bytes, put(after, "return "), inner), Return, start)
	} else if choice == 9 {
		mark(token(put(token(after, "value", Identifier), " |> "), "call", Identifier), ArrowCall, start)
	} else if choice == 11 {
		token(after, "...", Ellipsis)
	} else if choice == 12 and after.trivia {
		for_loop(bytes, after, inner)
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
		(guarded, chosen) = pick(bytes, branch_pattern(bytes, matched, inner), 2)
		bound = if (guarded == 0) chosen else put(token(put(chosen, " if "), "value", Identifier), "")
		arrowed = put(bound, " => ")
		branch = mark(expr_atom(bytes, arrowed, inner), MatchBranch, branch_start)
		mark(put(branch, "\n}"), Match, start)
	} else {
		expr_atom(bytes, after, inner)
	}
}

## One statement of a block and the line break after it.
block_statement : List(U8), Out, U64 -> Out
block_statement = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, 6)
	start = after.text.len()
	finished = if choice == 5 {
		tested = put(token(put(after, "if "), "value", Identifier), " ")
		mark(mark(token(tested, "7", NumberLiteral), If, start), ExpressionStatement, start)
	} else if choice == 0 {
		named = put(token(after, "local", IdentifierPattern), " = ")
		mark(expr_atom(bytes, named, depth), Declaration, start)
	} else if choice == 1 {
		named = put(token(put(after, "var "), "\$count", Identifier), " = ")
		mark(expr_atom(bytes, named, depth), VariableDeclaration, start)
	} else if choice == 2 {
		mark(expr_atom(bytes, put(after, "expect "), depth), Expect, start)
	} else if choice == 3 {
		tested = put(token(put(after, "while "), "value", Identifier), " ")
		body_start = tested.text.len()
		broken = token(put(tested, "{ "), "break", Break)
		mark(mark(put(broken, " }"), Block, body_start), While, start)
	} else {
		for_loop(bytes, after, depth)
	}
	put(finished, "\n\t")
}

for_loop : List(U8), Out, U64 -> Out
for_loop = |bytes, out, depth| {
	start = out.text.len()
	bound = put(pattern(bytes, put(out, "for "), depth), " in ")
	listed = put(token(bound, "value", Identifier), " ")
	body_start = listed.text.len()
	last_start = body_start + 2
	ended = mark(token(put(listed, "{ "), "value", Identifier), ExpressionStatement, last_start)
	mark(mark(put(ended, " }"), Block, body_start), For, start)
}

block : List(U8), Out, U64 -> Out
block = |bytes, out, depth| {
	start = out.text.len()
	(count, counted) = pick(bytes, put(out, "{\n\t"), 3)
	var $out = counted
	for _ in 0..<(count + 1) {
		$out = block_statement(bytes, $out, depth)
	}
	last_start = $out.text.len()
	last = mark(expr_atom(bytes, $out, depth), ExpressionStatement, last_start)
	mark(put(last, "\n}"), Block, start)
}

exposed : Out, Str -> Out
exposed = |out, name| {
	start = out.text.len()
	mark(token(out, name, Identifier), ExposedItem, start)
}

text_literal : Out, Str -> Out
text_literal = |out, text| {
	start = out.text.len()
	mark(put(token(put(out, "\""), text, StringSegment), "\""), StringLiteral, start)
}

## `name: value` in a dependency record, with the value written by `value`.
dependency : Out, Str, (Out -> Out) -> Out
dependency = |out, name, value| {
	start = out.text.len()
	mark(value(put(token(out, name, Identifier), ": ")), DependencyField, start)
}

package_record : Out -> Out
package_record = |out| {
	start = out.text.len()
	field = dependency(put(out, "{ "), "dep", |o| text_literal(o, "d.roc"))
	mark(put(field, " }"), Dependencies, start)
}

symbols : Out, RocSyntax.NodeKind -> Out
symbols = |out, kind| {
	start = out.text.len()
	entry_start = start + 2
	named = put(text_literal(put(out, "{ "), "m"), ": ")
	entry = mark(token(named, "run", Identifier), SymbolMapping, entry_start)
	mark(put(entry, " }"), kind, start)
}

target_section : Out -> Out
target_section = |out| {
	start = out.text.len()
	directory_start = start + 2
	directory = mark(text_literal(put(out, "{ inputs_dir: "), "t"), TargetInputsDirectory, directory_start)
	entry_start = directory.text.len() + 2
	named = put(token(put(directory, ", "), "linux", Identifier), ": ")
	config_start = named.text.len()
	field_start = config_start + 2
	output = put(token(put(named, "{ "), "output", Identifier), ": ")
	field = mark(token(output, "Exe", Identifier), TargetField, field_start)
	list_field_start = field.text.len() + 2
	inputs = put(token(put(field, ", "), "inputs", Identifier), ": ")
	list_start = inputs.text.len()
	file_start = list_start + 1
	app_file = mark(token(put(inputs, "["), "app", Identifier), TargetFile, file_start)
	object_start = app_file.text.len() + 2
	object = mark(text_literal(put(app_file, ", "), "s.o"), TargetFile, object_start)
	listed = mark(mark(put(object, "]"), TargetList, list_start), TargetField, list_field_start)
	config = mark(put(listed, " }"), TargetConfig, config_start)
	mark(put(mark(config, TargetEntry, entry_start), " }"), PlatformTargets, start)
}

required_values : Out, U64, List(U8) -> Out
required_values = |out, depth, bytes| {
	start = out.text.len()
	entry_start = start + 2
	alias_start = entry_start + 1
	opened = put(out, "{ [")
	alias = mark(token(put(token(opened, "Model", TypeName), " : "), "model", TypeVariable), RequiredType, alias_start)
	named = put(token(put(alias, "] for "), "main", Identifier), " : ")
	mark(put(mark(type_full(bytes, named, depth, True), RequiredValue, entry_start), " }"), PlatformRequires, start)
}

## A file header, or none. Each kind holds what only that kind can.
file_header : List(U8), Out, U64 -> Out
file_header = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, 4)
	if choice == 0 {
		after
	} else if choice == 1 {
		listed = put(exposed(put(exposed(put(after, "module ["), "name"), ", "), "Other"), "]")
		put(mark(listed, Header, 0), "\n")
	} else if choice == 2 {
		provided = put(exposed(put(after, "app ["), "name"), "] ")
		dependencies_start = provided.text.len()
		reference_start = dependencies_start + 6
		named = put(token(put(provided, "{ "), "pf", Identifier), ": ")
		platform_field = mark(mark(text_literal(put(named, "platform "), "p.roc"), PlatformReference, reference_start), DependencyField, dependencies_start + 2)
		other = dependency(put(platform_field, ", "), "dep", |o| text_literal(o, "d.roc"))
		put(mark(mark(put(other, " }"), Dependencies, dependencies_start), Header, 0), "\n")
	} else {
		named = text_literal(put(after, "platform "), "p")
		required = required_values(put(named, "\n\trequires "), depth, bytes)
		listed = put(exposed(put(required, "\n\texposes ["), "name"), "]")
		provided = symbols(put(package_record(put(listed, "\n\tpackages ")), "\n\tprovides "), PlatformProvides)
		host_map = symbols(put(provided, "\n\thosted "), PlatformHosted)
		put(mark(target_section(put(host_map, "\n\ttargets: ")), Header, 0), "\n")
	}
}

file_imported : Out -> Out
file_imported = |out| {
	start = out.text.len()
	named = put(token(put(text_literal(put(out, "import "), "d.txt"), " as "), "data", Identifier), " : ")
	put(mark(token(named, "Str", Identifier), FileImport, start), "\n")
}

imported : Out -> Out
imported = |out| {
	start = out.text.len()
	path = put(token(put(out, "import "), "Foo", ImportPath), " as ")
	alias_start = path.text.len()
	alias = mark(token(path, "Bar", Identifier), ImportAlias, alias_start)
	put(mark(put(exposed(put(alias, " exposing ["), "name"), "]"), Import, start), "\n")
}

## A where clause on a type that has just been written.
where_clause : List(U8), Out, U64 -> Out
where_clause = |bytes, out, depth| {
	(choice, after) = pick(bytes, out, 2)
	start = after.text.len() + 1
	item_start = start + 7
	opened = token(put(after, " where ["), "a", TypeVariable)
	item = if choice == 0 {
		named = put(token(put(opened, "."), "method", Identifier), " : ")
		mark(type_full(bytes, named, depth, True), WhereMethod, item_start)
	} else {
		mark(token(put(opened, "."), "Alias", TypeName), WhereAlias, item_start)
	}
	mark(put(item, "]"), WhereClause, start)
}

type_declaration : List(U8), Out, U64 -> Out
type_declaration = |bytes, out, depth| {
	start = out.text.len()
	opened = put(token(out, "Box", TypeName), "(")
	param = mark(put(token(opened, "a", TypeVariable), ")"), TypeHeader, start)
	body = type_atom(bytes, put(param, " := "), depth)
	(associated, counted) = pick(bytes, body, 2)
	if associated == 0 {
		put(mark(counted, TypeDeclaration, start), "\n")
	} else {
		block_start = counted.text.len() + 1
		definition_start = block_start + 2
		named = put(token(put(counted, ".{ "), "made", IdentifierPattern), " = ")
		definition = mark(expr_atom(bytes, named, depth), Declaration, definition_start)
		closed = mark(put(definition, " }"), AssociatedBlock, block_start)
		put(mark(closed, TypeDeclaration, start), "\n")
	}
}

statement : List(U8), Out, U64 -> Out
statement = |bytes, out, depth| {
	(choice, chosen) = pick(bytes, out, 5)
	if choice == 0 {
		type_declaration(bytes, chosen, depth)
	} else {
		definition(bytes, chosen, depth, choice == 1)
	}
}

definition : List(U8), Out, U64, Bool -> Out
definition = |bytes, out, depth, constrained| {
	start = out.text.len()
	written = type_full(bytes, put(token(out, "name", Identifier), " : "), depth, True)
	annotated = if (constrained) where_clause(bytes, written, depth) else written
	definition_start = annotated.text.len() + 1
	marked = mark(annotated, TypeAnnotation, start)
	named = put(token(put(marked, "\n"), "name", IdentifierPattern), " = ")
	(multiline, chosen) = pick(bytes, named, 8)
	literal_start = chosen.text.len()
	literal = |o| mark(token(put(o, "\"\"\""), "text", StringSegment), MultilineStringLiteral, literal_start)
	value = if multiline == 0 {
		# Only a line break ends a multiline string, so it can't nest here.
		literal(chosen)
	} else if multiline == 1 {
		mark(token(put(literal(chosen), "\n."), "Str", Identifier), TypedLiteral, literal_start)
	} else {
		expr_full(bytes, chosen, depth)
	}
	put(mark(value, Declaration, definition_start), "\n")
}

generate : List(U8) -> Case
generate = |bytes| {
	depth = U8.to_u64((bytes.get(0) ?? 0) % 5)
	count = U8.to_u64((bytes.get(1) ?? 0) % 3) + 1
	headed = file_header(bytes, { text: [], spans: [], next: 2, trivia: True }, depth)
	(with_import, counted) = pick(bytes, headed, 3)
	var $out = if (with_import == 0) counted else if (with_import == 1) imported(counted) else file_imported(counted)
	for _ in 0..<count {
		$out = statement(bytes, $out, depth)
	}
	{ source: Str.from_utf8_lossy($out.text), spans: $out.spans }
}

## The tree as written: children first, without the file around it.
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
		if node.kind == File {
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

target = Fuzz.target_with({ name: "roc-syntax-span-model", examples: [], generator: Fuzz.map(Fuzz.raw_bytes, generate), test, show: |input| input.source })

expect {
	# These bytes choose a parenthesized function after a comment, which begins
	# at its first argument.
	case = generate([1, 0, 0, 0, 2, 0, 4, 3, 1, 0, 0, 0, 0, 0, 1])
	functions = case.spans.keep_if(|span| span.kind == TypeFunction).map(|span| (span.start, span.end))
	case.source == "name : ( # note\nU8 -> U8)\nname = 7\n" and functions == [(16, 24)]
}
