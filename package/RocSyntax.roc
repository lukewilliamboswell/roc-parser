## Lossless syntax scaffolding for the Zig Roc compiler syntax pinned in
## `.roc-version`. Byte spans are half-open. Comments and whitespace are tokens.
##
## Expressions support identifiers, numbers, strings without interpolation,
## calls, field access, lists, tuples, unary and binary operators. Files support
## simple value declarations and expression statements. Other constructs remain
## source-backed `UnsupportedNode` values with an `UnsupportedSyntax` diagnostic.
## This is an evolving syntax API, not a complete Roc validity checker.
##
## Parsing retains the source once and stores nodes/children in flat lists.
## Nesting is bounded at 128; documents over 65,536 tokens retain all tokens but
## skip syntax construction with `Limited` status. Malformed input returns a
## partial tree and diagnostics, including arbitrary non-UTF-8 source bytes.
RocSyntax :: [].{
	## A UTF-8 byte interval; `end` is excluded.
	Span : { start : U64, end : U64 }

	## Lexical classes. Operator and punctuation spelling is available by span.
	TokenKind : [Whitespace, Comment, LowerIdent, UpperIdent, Keyword, Number, StringStart, StringPart, StringEnd, SingleQuote, OpenRound, NoSpaceOpenRound, CloseRound, OpenSquare, CloseSquare, OpenCurly, CloseCurly, Operator, Punctuation, Invalid, EndOfFile]
	## Lossless token. Only EOF has an empty span.
	Token : { kind : TokenKind, start : U64, end : U64 }
	## Syntax categories. Children are indices into `Parsed.nodes`.
	NodeKind : [File, Declaration, ExpressionStatement, Identifier, NumberLiteral, StringLiteral, CharacterLiteral, Unary, Binary, Call, FieldAccess, ListLiteral, Tuple, Parenthesized, ErrorNode, UnsupportedNode]
	## Flat node. Its children occupy a contiguous interval in `Parsed.children`.
	Node : { kind : NodeKind, start : U64, end : U64, children_start : U64, children_len : U64 }
	## Structured issues; human-facing text can be rendered outside parsing.
	DiagnosticCode : [InvalidByte, InvalidUtf8, UnclosedString, UnclosedDelimiter, UnexpectedToken, UnsupportedSyntax, ResourceLimit]
	Diagnostic : { code : DiagnosticCode, start : U64, end : U64 }
	## `Complete` means the implemented grammar consumed the input without issues.
	## It does not establish type correctness or complete language conformance.
	Status : [Complete, Recovered, Unsupported, Limited]
	Parsed : { source : List(U8), tokens : List(Token), nodes : List(Node), children : List(U64), diagnostics : List(Diagnostic), status : Status, root : U64 }

	## Parse source text; offsets refer to UTF-8 bytes, not Unicode characters.
	parse_str : Str -> Parsed
	parse_str = |source| parse_bytes(source.to_utf8())

	## Parse arbitrary source bytes, retaining invalid bytes and incomplete syntax.
	parse_bytes : List(U8) -> Parsed
	parse_bytes = |source| parse_source(source, False)

	## Parse one expression. Extra significant tokens produce a diagnostic.
	parse_expression : Str -> Parsed
	parse_expression = |source| parse_source(source.to_utf8(), True)

	## Parse one expression from UTF-8 text.
	parse_expr_str : Str -> Parsed
	parse_expr_str = |source| parse_source(source.to_utf8(), True)

	## Parse one expression from arbitrary bytes, preserving malformed input.
	parse_expr_bytes : List(U8) -> Parsed
	parse_expr_bytes = |source| parse_source(source, True)
}

Lexed : { tokens : List(RocSyntax.Token), diagnostics : List(RocSyntax.Diagnostic), unsupported : Bool }
State : { source : List(U8), tokens : List(RocSyntax.Token), pos : U64, nodes : List(RocSyntax.Node), children : List(U64), diagnostics : List(RocSyntax.Diagnostic), unsupported : Bool, limited : Bool }
Built : { state : State, node : U64 }

byte : List(U8), U64 -> U8
byte = |bytes, at| bytes.get(at) ?? 0

is_space : U8 -> Bool
is_space = |b| b == ' ' or b == '\t' or b == '\n' or b == '\r'
is_lower : U8 -> Bool
is_lower = |b| b >= 'a' and b <= 'z'
is_upper : U8 -> Bool
is_upper = |b| b >= 'A' and b <= 'Z'
is_digit : U8 -> Bool
is_digit = |b| b >= '0' and b <= '9'
is_ident : U8 -> Bool
is_ident = |b| is_lower(b) or is_upper(b) or is_digit(b) or b == '_' or b == '!'

matches : List(U8), U64, Str -> Bool
matches = |bytes, at, text| {
	wanted = text.to_utf8()
	var $yes = at + wanted.len() <= bytes.len()
	var $i = 0
	while $yes and $i < wanted.len() {
		$yes = byte(bytes, at + $i) == byte(wanted, $i)
		$i = $i + 1
	}
	$yes
}

keyword : List(U8), U64, U64 -> Bool
keyword = |source, start, end| {
	# Fixed table: at most 34 short comparisons per identifier, independent of
	# input size. No lexeme strings are allocated during classification.
	words = ["app", "as", "crash", "dbg", "else", "expect", "exposes", "exposing", "for", "for!", "generates", "has", "hosted", "if", "implements", "import", "imports", "in", "interface", "match", "module", "package", "packages", "platform", "provides", "requires", "return", "targets", "var", "where", "while", "with", "break"]
	words.any(|word| word.to_utf8().len() == end - start and matches(source, start, word))
}

push_diagnostic : List(RocSyntax.Diagnostic), RocSyntax.DiagnosticCode, U64, U64 -> List(RocSyntax.Diagnostic)
push_diagnostic = |diagnostics, code, start, end| {
	# Bounded diagnostic output; parsing still records error status once full.
	if diagnostics.len() < 256 diagnostics.append({ code, start, end }) else diagnostics
}

utf8_diagnostics : List(U8) -> List(RocSyntax.Diagnostic)
utf8_diagnostics = |source| {
	var $out = []
	var $i = 0
	while $i < source.len() {
		b = byte(source, $i)
		width = if b < 0x80 1 else if b >= 0xC2 and b <= 0xDF 2 else if b >= 0xE0 and b <= 0xEF 3 else if b >= 0xF0 and b <= 0xF4 4 else 0
		var $valid = width > 0 and $i + width <= source.len()
		var $j = 1
		while $valid and $j < width {
			c = byte(source, $i + $j)
			$valid = c >= 0x80 and c <= 0xBF
			$j = $j + 1
		}
		second = byte(source, $i + 1)
		$valid = $valid and !(b == 0xE0 and second < 0xA0) and !(b == 0xED and second >= 0xA0) and !(b == 0xF0 and second < 0x90) and !(b == 0xF4 and second >= 0x90)
		if $valid { $i = $i + width } else {
			$out = push_diagnostic($out, InvalidUtf8, $i, $i + 1)
			$i = $i + 1
		}
	}
	$out
}

lex : List(U8) -> Lexed
lex = |source| {
	var $tokens = []
	var $diagnostics = utf8_diagnostics(source)
	var $unsupported = False
	var $i = 0
	while $i < source.len() {
		start = $i
		b = byte(source, start)
		var $kind = Invalid
		$i = $i + 1
		if is_space(b) {
			$kind = Whitespace
			while $i < source.len() and is_space(byte(source, $i)) { $i = $i + 1 }
		} else if b == '#' {
			$kind = Comment
			while $i < source.len() and byte(source, $i) != '\n' and byte(source, $i) != '\r' { $i = $i + 1 }
		} else if is_lower(b) or is_upper(b) or b == '_' or b == '$' or b == '@' {
			while $i < source.len() and is_ident(byte(source, $i)) { $i = $i + 1 }
			$kind = if keyword(source, start, $i) Keyword else if is_upper(b) or b == '@' UpperIdent else LowerIdent
		} else if is_digit(b) {
			$kind = Number
			# Prefixes, exponents and underscores are kept as one number lexeme.
			while $i < source.len() and (is_digit(byte(source, $i)) or is_lower(byte(source, $i)) or is_upper(byte(source, $i)) or byte(source, $i) == '_') { $i = $i + 1 }
			if byte(source, $i) == '.' and is_digit(byte(source, $i + 1)) {
				$i = $i + 1
				while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') { $i = $i + 1 }
			}
		} else if b == '"' {
			width = if matches(source, start, "\"\"\"") 3 else 1
			$tokens = $tokens.append({ kind: StringStart, start, end: start + width })
			$i = start + width
			part = $i
			var $closed = False
			while $i < source.len() and !$closed {
				if byte(source, $i) == '\\' {
					$i = if $i + 1 < source.len() $i + 2 else $i + 1
				} else if matches(source, $i, if width == 3 "\"\"\"" else "\"") {
					$closed = True
				} else {
					if matches(source, $i, "\${") {
						$unsupported = True
						$diagnostics = push_diagnostic($diagnostics, UnsupportedSyntax, $i, $i + 2)
					}
					$i = $i + 1
				}
			}
			if $i > part { $tokens = $tokens.append({ kind: StringPart, start: part, end: $i }) }
			if $closed {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + width })
				$i = $i + width
			} else { $diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i) }
		} else if b == '\'' {
			$kind = SingleQuote
			var $closed = False
			while $i < source.len() and !$closed and byte(source, $i) != '\n' {
				if byte(source, $i) == '\\' {
					$i = if $i + 1 < source.len() $i + 2 else $i + 1
				} else if byte(source, $i) == '\'' {
					$i = $i + 1
					$closed = True
				} else { $i = $i + 1 }
			}
			if !$closed { $diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i) }
		} else {
			$kind = match b {
				'(' => if start > 0 and !is_space(byte(source, start - 1)) NoSpaceOpenRound else OpenRound
				')' => CloseRound
				'[' => OpenSquare
				']' => CloseSquare
				'{' => OpenCurly
				'}' => CloseCurly
				',' | '.' | ':' => Punctuation
				'+' | '-' | '*' | '/' | '%' | '^' | '=' | '!' | '?' | '<' | '>' | '|' | '&' | '\\' => Operator
				_ => Invalid
			}
			# Longest known spelling, never an unbounded punctuation run.
			operators = ["..<", "..=", "...", "->", "=>", "<-", "|>", "==", "!=", "<=", ">=", "??", "//", ":=", "::", "..", ".*", ".?"]
			var $found = False
			for op in operators {
				if !$found and matches(source, start, op) {
					$i = start + op.to_utf8().len()
					$kind = if b == '.' or b == ':' Punctuation else Operator
					$found = True
				}
			}
			if $kind == Invalid { $diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i) }
		}
		if b != '"' { $tokens = $tokens.append({ kind: $kind, start, end: $i }) }
	}
	{ tokens: $tokens.append({ kind: EndOfFile, start: source.len(), end: source.len() }), diagnostics: $diagnostics, unsupported: $unsupported }
}

peek : State -> RocSyntax.Token
peek = |s| s.tokens.get(s.pos) ?? { kind: EndOfFile, start: s.source.len(), end: s.source.len() }

skip_trivia : State -> State
skip_trivia = |s| {
	var $pos = s.pos
	var $done = False
	while !$done {
		kind = (s.tokens.get($pos) ?? { kind: EndOfFile, start: s.source.len(), end: s.source.len() }).kind
		if kind == Whitespace or kind == Comment { $pos = $pos + 1 } else { $done = True }
	}
	{ ..s, pos: $pos }
}

spelling : State, Str -> Bool
spelling = |s, text| {
	t = peek(s)
	t.end - t.start == text.to_utf8().len() and matches(s.source, t.start, text)
}

issue : State, RocSyntax.DiagnosticCode, U64, U64 -> State
issue = |s, code, start, end| {
	{ ..s, diagnostics: push_diagnostic(s.diagnostics, code, start, end), unsupported: s.unsupported or code == UnsupportedSyntax, limited: s.limited or code == ResourceLimit }
}

add_node : State, RocSyntax.NodeKind, U64, U64, List(U64) -> Built
add_node = |s, kind, start, end, ids| {
	index = s.nodes.len()
	node = { kind, start, end, children_start: s.children.len(), children_len: ids.len() }
	{ state: { ..s, nodes: s.nodes.append(node), children: s.children.concat(ids) }, node: index }
}

node_start : State, U64 -> U64
node_start = |s, id| (s.nodes.get(id) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).start
node_end : State, U64 -> U64
node_end = |s, id| (s.nodes.get(id) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).end

# Exact binding powers from src/parse/Parser.zig at 7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5.
binding : State -> { left : U64, right : U64 }
binding = |s| {
	if spelling(s, "*") or spelling(s, "/") or spelling(s, "//") or spelling(s, "%") { { left: 32, right: 33 } }
	else if spelling(s, "+") or (spelling(s, "-") and is_space(byte(s.source, peek(s).end))) { { left: 22, right: 23 } }
	else if spelling(s, "??") { { left: 20, right: 21 } }
	else if spelling(s, "?") { { left: 18, right: 19 } }
	else if spelling(s, "==") { { left: 17, right: 17 } }
	else if spelling(s, "!=") { { left: 15, right: 15 } }
	else if spelling(s, "<") { { left: 13, right: 13 } }
	else if spelling(s, ">") { { left: 11, right: 11 } }
	else if spelling(s, "<=") { { left: 9, right: 9 } }
	else if spelling(s, ">=") { { left: 7, right: 7 } }
	else if spelling(s, "and") { { left: 6, right: 5 } }
	else if spelling(s, "or") { { left: 4, right: 3 } }
	else if spelling(s, "..<") or spelling(s, "..=") { { left: 2, right: 3 } }
	else { { left: 0, right: 0 } }
}

expression : State, U64, U64 -> Built
expression = |initial, minimum, depth| {
	s = skip_trivia(initial)
	first = peek(s)
	if depth >= 128 {
		limited = issue(s, ResourceLimit, first.start, first.end)
		add_node({ ..limited, pos: limited.pos + if first.kind == EndOfFile 0 else 1 }, ErrorNode, first.start, first.end, [])
	} else {
		built = primary(s, depth)
		var $s = built.state
		var $left = built.node
		var $done = False
		while !$done {
			look = skip_trivia($s)
			t = peek(look)
			bp = binding(look)
			if t.kind == NoSpaceOpenRound {
				start = node_start(look, $left)
				args = collection({ ..look, pos: look.pos + 1 }, CloseRound, depth + 1)
				ids = [$left].concat(args.ids)
				call = add_node(args.state, Call, start, args.end, ids)
				$s = call.state
				$left = call.node
			} else if spelling(look, ".") and t.start == node_end(look, $left) {
				field_state = { ..look, pos: look.pos + 1 }
				field = peek(field_state)
				if field.kind == LowerIdent or field.kind == UpperIdent or field.kind == Number {
					start = node_start(field_state, $left)
					leaf = add_node({ ..field_state, pos: field_state.pos + 1 }, Identifier, field.start, field.end, [])
					access = add_node(leaf.state, FieldAccess, start, field.end, [$left, leaf.node])
					$s = access.state
					$left = access.node
				} else { $done = True }
			} else if bp.left > 0 and bp.left >= minimum {
				start = node_start(look, $left)
				right = expression({ ..look, pos: look.pos + 1 }, bp.right, depth + 1)
				end = node_end(right.state, right.node)
				binary = add_node(right.state, Binary, start, end, [$left, right.node])
				$s = binary.state
				$left = binary.node
			} else { $done = True }
		}
		{ state: $s, node: $left }
	}
}

Collection : { state : State, ids : List(U64), end : U64, comma : Bool }
collection : State, RocSyntax.TokenKind, U64 -> Collection
collection = |initial, closing, depth| {
	var $s = initial
	var $ids = []
	var $end = peek(initial).start
	var $done = False
	var $comma = False
	while !$done {
		look = skip_trivia($s)
		t = peek(look)
		if t.kind == closing {
			$s = { ..look, pos: look.pos + 1 }
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			item = expression(look, 0, depth)
			$ids = $ids.append(item.node)
			$end = node_end(item.state, item.node)
			$s = skip_trivia(item.state)
			if spelling($s, ",") {
				$comma = True
				$s = { ..$s, pos: $s.pos + 1 }
			} else if peek($s).kind != closing and peek($s).kind != EndOfFile {
				bad = peek($s)
				$s = issue($s, UnexpectedToken, bad.start, bad.end)
				if $s.pos <= before { $s = { ..$s, pos: $s.pos + 1 } }
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: $comma }
}

primary : State, U64 -> Built
primary = |s, depth| {
	t = peek(s)
	next = { ..s, pos: s.pos + if t.kind == EndOfFile 0 else 1 }
	match t.kind {
		LowerIdent | UpperIdent => add_node(next, Identifier, t.start, t.end, [])
		Number => add_node(next, NumberLiteral, t.start, t.end, [])
		SingleQuote => add_node(next, CharacterLiteral, t.start, t.end, [])
		StringStart => {
			var $s = next
			var $end = t.end
			if peek($s).kind == StringPart {
				$end = peek($s).end
				$s = { ..$s, pos: $s.pos + 1 }
			}
			if peek($s).kind == StringEnd {
				$end = peek($s).end
				$s = { ..$s, pos: $s.pos + 1 }
			}
			add_node($s, StringLiteral, t.start, $end, [])
		}
		OpenRound | NoSpaceOpenRound | OpenSquare => {
			items = collection(next, if t.kind == OpenSquare CloseSquare else CloseRound, depth + 1)
			kind = if t.kind == OpenSquare ListLiteral else if items.comma or items.ids.len() != 1 Tuple else Parenthesized
			add_node(items.state, kind, t.start, items.end, items.ids)
		}
		Operator if spelling(s, "-") or spelling(s, "!") => {
			operand = expression(next, 40, depth + 1)
			end = node_end(operand.state, operand.node)
			add_node(operand.state, Unary, t.start, end, [operand.node])
		}
		EndOfFile | CloseRound | CloseSquare | CloseCurly => {
			err = issue(s, UnexpectedToken, t.start, t.end)
			add_node(err, ErrorNode, t.start, t.start, [])
		}
		Invalid => add_node(next, ErrorNode, t.start, t.end, [])
		_ => {
			unknown = issue(next, UnsupportedSyntax, t.start, t.end)
			add_node(unknown, UnsupportedNode, t.start, t.end, [])
		}
	}
}

parse_source : List(U8), Bool -> RocSyntax.Parsed
parse_source = |source, expression_only| {
	lexed = lex(source)
	initial = { source, tokens: lexed.tokens, pos: 0, nodes: [], children: [], diagnostics: lexed.diagnostics, unsupported: lexed.unsupported, limited: False }
	var $s = initial
	var $roots = []
	if $s.tokens.len() > 65536 {
		$s = issue($s, ResourceLimit, 0, source.len())
	} else {
		var $done = False
		while !$done {
			look = skip_trivia($s)
			t = peek(look)
			if t.kind == EndOfFile { $s = look
				$done = True } else {
				before = look.pos
				var $built = expression(look, 0, 0)
				if !expression_only {
					after = skip_trivia($built.state)
					if t.kind == LowerIdent and spelling(after, "=") {
						value = expression({ ..after, pos: after.pos + 1 }, 0, 0)
						end = node_end(value.state, value.node)
						$built = add_node(value.state, Declaration, t.start, end, [$built.node, value.node])
					} else {
						end = node_end($built.state, $built.node)
						$built = add_node($built.state, ExpressionStatement, t.start, end, [$built.node])
					}
				}
				$roots = $roots.append($built.node)
				$s = $built.state
				if $s.pos <= before {
					$s = issue($s, UnexpectedToken, t.start, t.end)
					$s = { ..$s, pos: before + 1 }
				}
				if expression_only {
					$s = skip_trivia($s)
					if peek($s).kind != EndOfFile {
						extra = peek($s)
						$s = issue($s, UnexpectedToken, extra.start, extra.end)
					}
					$done = True
				}
			}
		}
	}
	root = add_node($s, File, 0, source.len(), $roots)
	finished = root.state
	has_error = finished.diagnostics.any(|d| d.code != UnsupportedSyntax)
	status = if finished.limited Limited else if has_error Recovered else if finished.unsupported Unsupported else Complete
	{ source: finished.source, tokens: finished.tokens, nodes: finished.nodes, children: finished.children, diagnostics: finished.diagnostics, status, root: root.node }
}

expect {
	p = RocSyntax.parse_str("answer = 1 + 2 * 3\n")
	p.status == Complete and p.nodes.len() == 8 and (p.nodes.get(p.root) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == File
}

expect {
	p = RocSyntax.parse_expression("f(1, [2, 3]).field")
	p.status == Complete and p.nodes.any(|n| n.kind == Call) and p.nodes.any(|n| n.kind == FieldAccess) and p.nodes.any(|n| n.kind == ListLiteral)
}

expect {
	p = RocSyntax.parse_str("# hello\nx = \"é\"\n")
	p.tokens.fold(0, |at, token| if token.start == at token.end else 999999) == p.source.len() and p.tokens.any(|t| t.kind == Comment) and p.status == Complete
}

expect {
	p = RocSyntax.parse_str("bad = [1, 2\n")
	p.status == Recovered and p.diagnostics.any(|d| d.code == UnclosedDelimiter)
}

expect {
	p = RocSyntax.parse_bytes([0xFF, 0x00, 'x'])
	p.status == Recovered and p.diagnostics.any(|d| d.code == InvalidUtf8) and p.tokens.len() == 4
}

expect {
	p = RocSyntax.parse_str("import Foo\nx = 1\n")
	p.status == Unsupported and p.nodes.any(|n| n.kind == Declaration)
}
