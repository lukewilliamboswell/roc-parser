app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## In a complete parse every node covers whole tokens, keeps its delimiters
## paired and sits where its children say it does. Recovery can stop anywhere, so
## a recovered tree only has to keep each child to its own bytes: a node that
## claims a token it left for its parent would overlap whatever comes next.
## These hold for any source, so this target needs no reference parser.

## Token facts by byte offset, which keeps each node's check constant time.
Edges : { starts : List(Bool), ends : List(Bool), depth : List(U64) }

replace : List(a), U64, a -> List(a)
replace = |list, index, value| match list.set(index, value) {
	Ok(updated) => updated
	Err(_) => crash "token outside its source"
}

edges : RocSyntax.Parsed -> Edges
edges = |parsed| {
	size = parsed.source.len() + 1
	var $starts = List.repeat(False, size)
	var $ends = List.repeat(False, size)
	var $depth = List.repeat(0, size)
	var $level = 0
	for token in parsed.tokens {
		match token.kind {
			Whitespace | Comment | EndOfFile => {}
			_ => {
				$starts = replace($starts, token.start, True)
				$ends = replace($ends, token.end, True)
			}
		}
		$level = match token.kind {
			OpenRound | NoSpaceOpenRound | OpenSquare | OpenCurly | InterpolationStart => $level + 1
			CloseRound | CloseSquare | CloseCurly | InterpolationEnd => $level - 1
			_ => $level
		}
		$depth = replace($depth, token.end, $level)
	}
	{ starts: $starts, ends: $ends, depth: $depth }
}

## Nodes that begin with their first child.
left_anchored : RocSyntax.Node -> Bool
left_anchored = |node| match node.kind {
	AliasPattern | AlternativePattern | ArrowCall | Binary | Call | Declaration | FieldAccess | MatchBranch | MethodArrowCall | NominalApply | NominalPattern | NominalRecord | OptionalFieldAccess | PatternField | RecordField | Suffix | TagPattern | TypeApply | TypeField | TypedLiteral | WhereAlias | WhereMethod => True
	# Without arguments the function begins with its own empty parentheses.
	TypeFunction => node.children_len > 1
	_ => False
}

## Nodes that end with their last child.
right_anchored : RocSyntax.Node -> Bool
right_anchored = |node| match node.kind {
	AliasPattern | AlternativePattern | ArrowCall | Binary | Declaration | FieldAccess | If | Lambda | MatchBranch | MethodArrowCall | OptionalFieldAccess | PatternField | RecordField | TypeField | TypeFunction | Unary => True
	_ => False
}

## These keep a child that appears later in the source first.
reordered : RocSyntax.Node -> Bool
reordered = |node| node.kind == RecordBuilder or node.kind == TypeDeclaration

children_of : RocSyntax.Parsed, RocSyntax.Node -> List(RocSyntax.Node)
children_of = |parsed, node| {
	var $found = []
	var $offset = 0
	while $offset < node.children_len {
		id = parsed.children.get(node.children_start + $offset) ?? parsed.nodes.len()
		match parsed.nodes.get(id) {
			Ok(child) => {
				$found = $found.append(child)
			}
			Err(_) => crash "invalid child index"
		}
		$offset = $offset + 1
	}
	$found
}

check_order : List(RocSyntax.Node), RocSyntax.Node, Str -> {}
check_order = |found, node, text| {
	if !reordered(node) {
		var $previous = node.start
		for child in found {
			if child.start < $previous {
				crash "children overlap or are out of order: ${text}"
			}
			$previous = child.end
		}
	}
}

check_anchors : List(RocSyntax.Node), RocSyntax.Node, Str -> {}
check_anchors = |found, node, text| {
	if left_anchored(node) {
		if (found.first() ?? node).start != node.start {
			crash "node does not begin with its first child: ${text}"
		}
	}
	if right_anchored(node) {
		if (found.last() ?? node).end != node.end {
			crash "node does not end with its last child: ${text}"
		}
	}
}

check_node : RocSyntax.Parsed, Edges, RocSyntax.Node -> {}
check_node = |parsed, facts, node| {
	text = Str.inspect(node.kind)
	# An implicit header has no source, and a file also covers leading trivia.
	if node.kind != File and node.start != node.end {
		if !(facts.starts.get(node.start) ?? False) {
			crash "node does not begin at a token: ${text}"
		}
		if !(facts.ends.get(node.end) ?? False) {
			crash "node does not end at a token: ${text}"
		}
		if (facts.depth.get(node.start) ?? 0) != (facts.depth.get(node.end) ?? 0) {
			crash "node holds half of a delimiter pair: ${text}"
		}
	}
	check_anchors(children_of(parsed, node), node, text)
}

check : RocSyntax.Parsed -> {}
check = |parsed| {
	for node in parsed.nodes {
		check_order(children_of(parsed, node), node, Str.inspect(node.kind))
	}
	if parsed.status == Complete {
		facts = edges(parsed)
		for node in parsed.nodes {
			check_node(parsed, facts, node)
		}
	}
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	check(RocSyntax.parse_bytes(bytes))
	check(RocSyntax.parse_expr_bytes(bytes))
	match Str.from_utf8(bytes) {
		Ok(source) => {
			check(RocSyntax.parse_statement_str(source))
			check(RocSyntax.parse_header_str(source))
		}
		Err(_) => {}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-spans", examples: [], generator: Fuzz.raw_bytes, test, show: Str.inspect })
