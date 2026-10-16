## Lossless syntax scaffolding for the Zig Roc compiler syntax pinned in
## `.roc-version`. Byte spans are half-open. Comments and whitespace are tokens.
##
## Expressions support identifiers, numbers, strings without interpolation,
## calls, field access, lists, tuples, records, blocks, simple lambdas, if/else,
## simple match branches, unary and binary operators. Files support module
## headers, simple imports, value declarations and simple type annotations.
## Other constructs remain
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
	NodeKind : [File, Declaration, ExpressionStatement, Lambda, If, Match, MatchBranch, Pattern, Block, Record, RecordField, Header, Import, TypeAnnotation, TypeExpression, Identifier, NumberLiteral, StringLiteral, CharacterLiteral, Unary, Binary, Call, FieldAccess, ListLiteral, Tuple, Parenthesized, ErrorNode, UnsupportedNode]
	## Flat node. Its children occupy a contiguous interval in `Parsed.children`.
	Node : { kind : NodeKind, start : U64, end : U64, children_start : U64, children_len : U64 }
	## Structured issues; human-facing text can be rendered outside parsing.
	DiagnosticCode : [InvalidByte, InvalidUtf8, UnclosedString, UnclosedDelimiter, UnexpectedToken, InvalidNumber, InvalidEscape, InvalidCharacter, UnsupportedSyntax, ResourceLimit]
	## An issue code and its precise source byte interval.
	Diagnostic : { code : DiagnosticCode, start : U64, end : U64 }
	## `Complete` means the implemented grammar consumed the input without issues.
	## It does not establish type correctness or complete language conformance.
	Status : [Complete, Recovered, Unsupported, Limited]
	## Source-backed tokens and a flat, indexed syntax tree with recovery status.
	Parsed : { source : List(U8), tokens : List(Token), nodes : List(Node), children : List(U64), diagnostics : List(Diagnostic), status : Status, root : U64, consumed : Bool }

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
is_ident = |b| is_lower(b) or is_upper(b) or is_digit(b) or b == '_' or b == '!' or b == '$' or b >= 0x80

matches : List(U8), U64, Str -> Bool
matches = |bytes, at, text| {
	len = text.count_utf8_bytes()
	at + len <= bytes.len() and Str.from_utf8_lossy(bytes.sublist({ start: at, len })) == text
}

keyword : List(U8), U64, U64 -> Bool
keyword = |source, start, end| {
	# All keywords fit Roc's inline string representation. No per-keyword
	# byte-list conversion or heap-built table is needed in this hot path.
	if end - start > 10 False else match Str.from_utf8_lossy(source.sublist({ start, len: end - start })) {
		"app" | "as" | "crash" | "dbg" | "else" | "expect" | "exposes" | "exposing" | "for" | "for!" | "generates" | "has" | "hosted" | "if" | "implements" | "import" | "imports" | "in" | "interface" | "match" | "module" | "package" | "packages" | "platform" | "provides" | "requires" | "return" | "targets" | "var" | "where" | "while" | "with" | "break" => True
		_ => False
	}
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
	var $last_significant = EndOfFile
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
			$kind = if matches(source, start, "and") and $i - start == 3 Operator else if matches(source, start, "or") and $i - start == 2 Operator else if keyword(source, start, $i) Keyword else if is_upper(b) or b == '@' UpperIdent else LowerIdent
			var $j = start + 1
			while $j < $i {
				if byte(source, $j) == '$' or byte(source, $j) >= 0x80 { $diagnostics = push_diagnostic($diagnostics, InvalidByte, $j, $j + 1) }
				$j = $j + 1
			}
		} else if is_digit(b) or (b == '-' and is_digit(byte(source, start + 1)) and (start == 0 or is_space(byte(source, start - 1)) or !can_end_expression($last_significant))) {
			$kind = Number
			number = scan_number(source, start + if b == '-' 1 else 0)
			$i = number.end
			if !number.valid { $diagnostics = push_diagnostic($diagnostics, InvalidNumber, start, $i) }
		} else if b == '"' {
			width = 1
			$tokens = $tokens.append({ kind: StringStart, start, end: start + width })
			$i = start + width
			part = $i
			var $closed = False
			while $i < source.len() and !$closed {
				if byte(source, $i) == '\\' {
					escape = scan_escape(source, $i)
					if !escape.valid { $diagnostics = push_diagnostic($diagnostics, InvalidEscape, $i, escape.end) }
					$i = escape.end
				} else if byte(source, $i) == '\n' or byte(source, $i) == '\r' {
					$closed = True
				} else if matches(source, $i, "\"") {
					$closed = True
				} else {
					if nonprintable(source, $i) { $diagnostics = push_diagnostic($diagnostics, InvalidCharacter, $i, $i + 1) }
					if matches(source, $i, "\${") {
						$unsupported = True
						$diagnostics = push_diagnostic($diagnostics, UnsupportedSyntax, $i, $i + 2)
					}
					$i = $i + 1
				}
			}
			if $i > part { $tokens = $tokens.append({ kind: StringPart, start: part, end: $i }) }
			if $closed and byte(source, $i) == '"' {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + width })
				$i = $i + width
			} else { $diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i) }
		} else if b == '\'' {
			$kind = SingleQuote
			var $closed = False
			while $i < source.len() and !$closed and byte(source, $i) != '\n' {
				if byte(source, $i) == '\\' {
					escape = scan_escape(source, $i)
					if !escape.valid { $diagnostics = push_diagnostic($diagnostics, InvalidEscape, $i, escape.end) }
					$i = escape.end
				} else if byte(source, $i) == '\'' {
					$i = $i + 1
					$closed = True
				} else { $i = $i + 1 }
			}
			if !$closed { $diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i) } else {
				var $chars = 0
				var $j = start + 1
				while $j + 1 < $i {
					if byte(source, $j) == '\\' { $j = scan_escape(source, $j).end
						$chars = $chars + 1 }
					else {
						if byte(source, $j) < 0x80 or byte(source, $j) >= 0xC0 { $chars = $chars + 1 }
						$j = $j + 1
					}
				}
				if $chars != 1 { $diagnostics = push_diagnostic($diagnostics, InvalidCharacter, start, $i) }
			}
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
			width = operator_width(source, start)
			if width > 1 {
				$i = start + width
				$kind = if b == '.' or b == ':' Punctuation else Operator
			}
			if $kind == Invalid { $diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i) }
		}
		if b != '"' { $tokens = $tokens.append({ kind: $kind, start, end: $i }) }
		if b == '"' { $last_significant = StringEnd }
		else if $kind != Whitespace and $kind != Comment { $last_significant = $kind }
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
	t.end - t.start == text.count_utf8_bytes() and matches(s.source, t.start, text)
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
	t = peek(s)
	text = if t.end - t.start <= 3 Str.from_utf8_lossy(s.source.sublist({ start: t.start, len: t.end - t.start })) else ""
	match text {
		"*" | "/" | "//" | "%" => { left: 32, right: 33 }
		"+" => { left: 22, right: 23 }
		"-" if binary_minus(s) => { left: 22, right: 23 }
		"??" => { left: 20, right: 21 }
		"?" => { left: 18, right: 19 }
		"==" => { left: 17, right: 17 }
		"!=" => { left: 15, right: 15 }
		"<" => { left: 13, right: 13 }
		">" => { left: 11, right: 11 }
		"<=" => { left: 9, right: 9 }
		">=" => { left: 7, right: 7 }
		"and" => { left: 6, right: 5 }
		"or" => { left: 4, right: 3 }
		"..<" | "..=" => { left: 2, right: 3 }
		_ => { left: 0, right: 0 }
	}
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
		OpenCurly => curly(next, t.start, depth + 1)
		Keyword if spelling(s, "if") => parse_if(next, t.start, depth + 1)
		Keyword if spelling(s, "match") => parse_match(next, t.start, depth + 1)
		Operator if spelling(s, "|") => parse_lambda(next, t.start, depth + 1)
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
				built = if expression_only expression(look, 0, 0) else statement(look, 0)
				$roots = $roots.append(built.node)
				$s = built.state
				if !expression_only {
					look_next = skip_trivia($s)
					end = node_end(look_next, built.node)
					if peek(look_next).kind != EndOfFile and !line_break(look_next.source, end, peek(look_next).start) {
						$s = issue($s, UnexpectedToken, peek(look_next).start, peek(look_next).end)
					}
				}
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
	{ source: finished.source, tokens: finished.tokens, nodes: finished.nodes, children: finished.children, diagnostics: finished.diagnostics, status, root: root.node, consumed: peek(skip_trivia(finished)).kind == EndOfFile }
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
	p.status == Complete and p.nodes.any(|n| n.kind == Declaration)
}

Scanned : { end : U64, valid : Bool }
scan_number : List(U8), U64 -> Scanned
scan_number = |source, start| {
	var $i = start
	var $valid = True
	var $base = 10
	if byte(source, start) == '0' {
		prefix = byte(source, start + 1)
		if prefix == 'x' or prefix == 'o' or prefix == 'b' {
			$base = if prefix == 'x' 16 else if prefix == 'o' 8 else 2
			$i = start + 2
		} else if is_digit(prefix) { $valid = False }
	}
	var $digits = 0.U64
	var $done = False
	while $i < source.len() and !$done {
		b = byte(source, $i)
		digit = if is_digit(b) U8.to_u64(b - '0') else if b >= 'a' and b <= 'f' U8.to_u64(b - 'a') + 10 else if b >= 'A' and b <= 'F' U8.to_u64(b - 'A') + 10 else 99
		if b == '_' { $i = $i + 1 }
		else if digit < $base { $digits = $digits + 1
			$i = $i + 1 }
		else { $done = True }
	}
	$valid = $valid and $digits > 0
	if $base == 10 {
		if byte(source, $i) == '.' and (is_digit(byte(source, $i + 1)) or byte(source, $i + 1) == 'e' or byte(source, $i + 1) == 'E') {
			$i = $i + 1
			while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') { $i = $i + 1 }
		}
		if byte(source, $i) == 'e' or byte(source, $i) == 'E' {
			$i = $i + 1
			if byte(source, $i) == '+' or byte(source, $i) == '-' { $i = $i + 1 }
			var $exponent_digits = 0.U64
			while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') {
				if is_digit(byte(source, $i)) { $exponent_digits = $exponent_digits + 1 }
				$i = $i + 1
			}
			$valid = $valid and $exponent_digits > 0
		}
	}
	# Old numeric suffix spellings are intentionally unsupported: the pinned
	# parser diagnoses these deprecated forms. Retain them in the number span.
	if $i < source.len() and (is_ident(byte(source, $i))) {
		$valid = False
		while $i < source.len() and is_ident(byte(source, $i)) { $i = $i + 1 }
	}
	{ end: $i, valid: $valid }
}

scan_escape : List(U8), U64 -> Scanned
scan_escape = |source, start| {
	b = byte(source, start + 1)
	if b == 'u' and byte(source, start + 2) == '(' {
		var $i = start + 3
		var $digits = 0.U64
		var $value = 0
		var $valid = True
		while $i < source.len() and byte(source, $i) != ')' and byte(source, $i) != '\n' and byte(source, $i) != '"' and byte(source, $i) != '\'' {
			c = byte(source, $i)
			digit = if is_digit(c) U8.to_u64(c - '0') else if c >= 'a' and c <= 'f' U8.to_u64(c - 'a') + 10 else if c >= 'A' and c <= 'F' U8.to_u64(c - 'A') + 10 else 99
			$valid = $valid and digit < 16
			if $digits < 7 { $value = $value * 16 + digit }
			$digits = $digits + 1
			$i = $i + 1
		}
		closed = byte(source, $i) == ')'
		{ end: $i + if closed 1 else 0, valid: $valid and closed and $digits > 0 and $digits <= 6 and $value <= 0x10FFFF and !($value >= 0xD800 and $value <= 0xDFFF) }
	} else {
		{ end: if start + 1 < source.len() start + 2 else start + 1, valid: b == 'n' or b == 'r' or b == 't' or b == '\\' or b == '"' or b == '\'' or b == '$' }
	}
}

line_break : List(U8), U64, U64 -> Bool
line_break = |source, start, end| {
	var $i = start
	var $found = False
	while $i < end and !$found {
		$found = byte(source, $i) == '\n' or byte(source, $i) == '\r'
		$i = $i + 1
	}
	$found
}

# Statements are parsed independently of expressions, so '=' cannot be
# accidentally accepted as a binary expression operator.
statement : State, U64 -> Built
statement = |initial, depth| {
	s = skip_trivia(initial)
	t = peek(s)
	after_name = skip_trivia({ ..s, pos: s.pos + 1 })
	if depth >= 128 {
		bad = issue(s, ResourceLimit, t.start, t.end)
		add_node({ ..bad, pos: bad.pos + if t.kind == EndOfFile 0 else 1 }, ErrorNode, t.start, t.end, [])
	} else if t.kind == LowerIdent and spelling(after_name, "=") {
		name = add_node(after_name, Identifier, t.start, t.end, [])
		value = expression({ ..name.state, pos: name.state.pos + 1 }, 0, depth)
		end = node_end(value.state, value.node)
		add_node(value.state, Declaration, t.start, end, [name.node, value.node])
	} else if t.kind == LowerIdent and spelling(after_name, ":") {
		name = add_node(after_name, Identifier, t.start, t.end, [])
		ty = type_expression({ ..name.state, pos: name.state.pos + 1 }, depth)
		end = node_end(ty.state, ty.node)
		add_node(ty.state, TypeAnnotation, t.start, end, [name.node, ty.node])
	} else if spelling(s, "module") {
		look = skip_trivia({ ..s, pos: s.pos + 1 })
		if peek(look).kind == OpenSquare {
			items = collection({ ..look, pos: look.pos + 1 }, CloseSquare, depth + 1)
			add_node(items.state, Header, t.start, items.end, items.ids)
		} else {
			bad = issue(look, UnsupportedSyntax, t.start, peek(look).end)
			add_node(bad, UnsupportedNode, t.start, t.end, [])
		}
	} else if spelling(s, "import") {
		imported = expression({ ..s, pos: s.pos + 1 }, 0, depth)
		end = node_end(imported.state, imported.node)
		add_node(imported.state, Import, t.start, end, [imported.node])
	} else {
		value = expression(s, 0, depth)
		end = node_end(value.state, value.node)
		add_node(value.state, ExpressionStatement, t.start, end, [value.node])
	}
}

type_expression : State, U64 -> Built
type_expression = |initial, depth| {
	if depth >= 128 {
		t = peek(initial)
		bad = issue(initial, ResourceLimit, t.start, t.end)
		add_node(bad, ErrorNode, t.start, t.start, [])
	} else {
		left = expression(initial, 40, depth + 1)
		look = skip_trivia(left.state)
		if spelling(look, "->") {
			start = node_start(look, left.node)
			right = type_expression({ ..look, pos: look.pos + 1 }, depth + 1)
			end = node_end(right.state, right.node)
			add_node(right.state, TypeExpression, start, end, [left.node, right.node])
		} else { left }
	}
}

parse_lambda : State, U64, U64 -> Built
parse_lambda = |initial, start, depth| {
	var $s = initial
	var $ids = []
	var $done = False
	while !$done {
		$s = skip_trivia($s)
		t = peek($s)
		if spelling($s, "|") {
			$s = { ..$s, pos: $s.pos + 1 }
			$done = True
		} else if t.kind == LowerIdent {
			param = add_node({ ..$s, pos: $s.pos + 1 }, Pattern, t.start, t.end, [])
			$ids = $ids.append(param.node)
			$s = skip_trivia(param.state)
			if spelling($s, ",") { $s = { ..$s, pos: $s.pos + 1 } }
			else if !spelling($s, "|") {
				$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
				$done = True
			}
		} else {
			$s = issue($s, if t.kind == EndOfFile UnclosedDelimiter else UnsupportedSyntax, t.start, t.end)
			$done = True
		}
	}
	body = expression($s, 0, depth)
	end = node_end(body.state, body.node)
	add_node(body.state, Lambda, start, end, $ids.append(body.node))
}

parse_if : State, U64, U64 -> Built
parse_if = |initial, start, depth| {
	condition = expression(initial, 0, depth)
	look = skip_trivia(condition.state)
	then_branch = expression(look, 0, depth)
	after = skip_trivia(then_branch.state)
	if spelling(after, "else") {
		other = expression({ ..after, pos: after.pos + 1 }, 0, depth)
		end = node_end(other.state, other.node)
		add_node(other.state, If, start, end, [condition.node, then_branch.node, other.node])
	} else {
		end = node_end(after, then_branch.node)
		add_node(after, If, start, end, [condition.node, then_branch.node])
	}
}

curly : State, U64, U64 -> Built
curly = |initial, start, depth| {
	look = skip_trivia(initial)
	first = peek(look)
	after = skip_trivia({ ..look, pos: look.pos + 1 })
	record = first.kind == CloseCurly or (first.kind == LowerIdent and (spelling(after, ":") or spelling(after, ",") or peek(after).kind == CloseCurly))
	var $s = initial
	var $ids = []
	var $end = first.start
	var $done = False
	while !$done {
		$s = skip_trivia($s)
		t = peek($s)
		if t.kind == CloseCurly {
			$end = t.end
			$s = { ..$s, pos: $s.pos + 1 }
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$end = t.start
			$s = issue($s, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = $s.pos
			if record {
				name = add_node({ ..$s, pos: $s.pos + 1 }, Identifier, t.start, t.end, [])
				var $field = name
				field_look = skip_trivia(name.state)
				if t.kind != LowerIdent {
					bad = issue(field_look, UnsupportedSyntax, t.start, t.end)
					$field = add_node(bad, UnsupportedNode, t.start, t.end, [])
				} else if spelling(field_look, ":") {
					value = expression({ ..field_look, pos: field_look.pos + 1 }, 0, depth)
					end = node_end(value.state, value.node)
					$field = add_node(value.state, RecordField, t.start, end, [name.node, value.node])
				} else { $field = add_node(field_look, RecordField, t.start, t.end, [name.node]) }
				$ids = $ids.append($field.node)
				$end = node_end($field.state, $field.node)
				$s = skip_trivia($field.state)
				if spelling($s, ",") { $s = { ..$s, pos: $s.pos + 1 } }
				else if peek($s).kind != CloseCurly and peek($s).kind != EndOfFile { $s = issue($s, UnexpectedToken, peek($s).start, peek($s).end) }
			} else {
				item = statement($s, depth)
				$ids = $ids.append(item.node)
				$end = node_end(item.state, item.node)
				$s = item.state
				look_next = skip_trivia($s)
				if peek(look_next).kind != CloseCurly and peek(look_next).kind != EndOfFile and !line_break(look_next.source, $end, peek(look_next).start) {
					$s = issue($s, UnexpectedToken, peek(look_next).start, peek(look_next).end)
				}
			}
			if $s.pos <= before { $s = { ..$s, pos: before + 1 } }
		}
	}
	add_node($s, if record Record else Block, start, $end, $ids)
}

parse_match : State, U64, U64 -> Built
parse_match = |initial, start, depth| {
	value = expression(initial, 0, depth)
	look = skip_trivia(value.state)
	if peek(look).kind != OpenCurly {
		bad = issue(look, UnexpectedToken, peek(look).start, peek(look).end)
		add_node(bad, Match, start, node_end(bad, value.node), [value.node])
	} else {
		var $s = { ..look, pos: look.pos + 1 }
		var $ids = [value.node]
		var $end = peek(look).end
		var $done = False
		while !$done {
			$s = skip_trivia($s)
			t = peek($s)
			if t.kind == CloseCurly {
				$end = t.end
				$s = { ..$s, pos: $s.pos + 1 }
				$done = True
			} else if t.kind == EndOfFile {
				$s = issue($s, UnclosedDelimiter, t.start, t.end)
				$end = t.start
				$done = True
			} else {
				before = $s.pos
				pattern_value = expression($s, 0, depth)
				pattern_end = node_end(pattern_value.state, pattern_value.node)
				pattern = add_node(pattern_value.state, Pattern, t.start, pattern_end, [pattern_value.node])
				arrow = skip_trivia(pattern.state)
				if spelling(arrow, "=>") {
					branch_value = expression({ ..arrow, pos: arrow.pos + 1 }, 0, depth)
					branch_end = node_end(branch_value.state, branch_value.node)
					branch = add_node(branch_value.state, MatchBranch, t.start, branch_end, [pattern.node, branch_value.node])
					$ids = $ids.append(branch.node)
					$s = skip_trivia(branch.state)
					$end = branch_end
					if spelling($s, ",") { $s = { ..$s, pos: $s.pos + 1 } }
				} else {
					$s = issue(arrow, UnsupportedSyntax, t.start, peek(arrow).end)
					$ids = $ids.append(pattern.node)
				}
				if $s.pos <= before { $s = { ..$s, pos: before + 1 } }
			}
		}
		add_node($s, Match, start, $end, $ids)
	}
}

expect {
	p = RocSyntax.parse_str("module [inc]\ninc : U64 -> U64\ninc = |x| x + 1\n")
	p.status == Complete and p.nodes.any(|n| n.kind == Header) and p.nodes.any(|n| n.kind == TypeAnnotation) and p.nodes.any(|n| n.kind == Lambda)
}

expect {
	p = RocSyntax.parse_expression("if x > 0 { x } else { 0 - x }")
	p.status == Complete and p.nodes.any(|n| n.kind == If) and p.nodes.any(|n| n.kind == Block)
}

expect {
	p = RocSyntax.parse_expression("{ name: \"Roc\", count: 42, flag }")
	p.status == Complete and p.nodes.any(|n| n.kind == Record) and p.nodes.keep_if(|n| n.kind == RecordField).len() == 3
}

expect {
	p = RocSyntax.parse_expression("match item { Some(x) => x, None => 0 }")
	p.status == Complete and p.nodes.keep_if(|n| n.kind == MatchBranch).len() == 2
}

expect {
	p = RocSyntax.parse_expression("{ x = 1\n y = x + 2\n y }")
	p.status == Complete and p.nodes.keep_if(|n| n.kind == Declaration).len() == 2
}

expect {
	RocSyntax.parse_expression("12cats").status == Recovered and RocSyntax.parse_expression("0x").status == Recovered and RocSyntax.parse_expression("01").status == Recovered and RocSyntax.parse_expression("1.2e-3").status == Complete and RocSyntax.parse_expression("0xFF").status == Complete
}

expect {
	RocSyntax.parse_expression("\"\\q\"").status == Recovered and RocSyntax.parse_expression("'ab'").status == Recovered and RocSyntax.parse_expression("'é'").status == Complete and RocSyntax.parse_expression("\"\\u(1F600)\"").status == Complete
}

expect {
	p = RocSyntax.parse_expression("1 + 2 * 3")
	# Nodes are postorder: the multiply covers 2 * 3, and is the right child
	# of the addition, independently of the flattened tree storage order.
	binaries = p.nodes.keep_if(|n| n.kind == Binary)
	binaries.map(|n| (n.start, n.end)) == [(4, 9), (0, 9)]
}

expect {
	# The pinned compiler makes equality right-associative.
	p = RocSyntax.parse_expression("1 == 2 == 3")
	p.nodes.keep_if(|n| n.kind == Binary).map(|n| (n.start, n.end)) == [(5, 11), (0, 11)]
}

expect {
	RocSyntax.parse_str("a b").status == Recovered and RocSyntax.parse_expression("1 2").status == Recovered
}

can_end_expression : RocSyntax.TokenKind -> Bool
can_end_expression = |kind| {
	kind == LowerIdent or kind == UpperIdent or kind == Number or kind == StringEnd or kind == SingleQuote or kind == CloseRound or kind == CloseSquare or kind == CloseCurly
}

binary_minus : State -> Bool
binary_minus = |s| {
	t = peek(s)
	next = byte(s.source, t.end)
	previous = s.tokens.get(if s.pos > 0 s.pos - 1 else 0) ?? t
	is_space(next) or next == '#' or (is_digit(next) and previous.end == t.start and can_end_expression(previous.kind))
}

expect {
	p = RocSyntax.parse_expr_str("-1")
	p.status == Complete and p.tokens.first().map_ok(|t| (t.kind, t.start, t.end)) == Ok((Number, 0, 2)) and p.nodes.any(|n| n.kind == NumberLiteral)
}

expect {
	p = RocSyntax.parse_expr_str("x-1")
	p.status == Complete and p.nodes.any(|n| n.kind == Binary) and p.tokens.keep_if(|t| t.kind == Number).map(|t| (t.start, t.end)) == [(2, 3)]
}

expect {
	p = RocSyntax.parse_expr_str("1.0e3")
	p.status == Complete and p.tokens.first().map_ok(|t| t.end) == Ok(5) and p.consumed
}

expect {
	RocSyntax.parse_expr_str("(1 +").consumed and !RocSyntax.parse_expr_str("1 2").consumed
}

nonprintable : List(U8), U64 -> Bool
nonprintable = |source, at| {
	b = byte(source, at)
	(b < 0x20 and b != '\t') or b == 0x7F or (b == 0xC2 and byte(source, at + 1) >= 0x80 and byte(source, at + 1) <= 0x9F) or (b == 0xEF and byte(source, at + 1) == 0xBF and byte(source, at + 2) >= 0xBE)
}

expect {
	p = RocSyntax.parse_expr_str("if s.is_empty() \"empty\" else \"not empty\"")
	p.status == Complete and p.nodes.any(|n| n.kind == If)
}

expect {
	p = RocSyntax.parse_str("x = \"unfinished\ny = 42\n")
	p.status == Recovered and p.nodes.keep_if(|n| n.kind == Declaration).len() == 2
}

operator_width : List(U8), U64 -> U64
operator_width = |source, at| {
	a = byte(source, at)
	b = byte(source, at + 1)
	c = byte(source, at + 2)
	if a == '.' and b == '.' and (c == '.' or c == '<' or c == '=') 3
	else if (a == '-' and b == '>') or (a == '=' and (b == '>' or b == '=')) or (a == '<' and (b == '-' or b == '=')) or (a == '>' and b == '=') or (a == '|' and b == '>') or (a == '!' and b == '=') or (a == '?' and b == '?') or (a == '/' and b == '/') or (a == ':' and (b == '=' or b == ':')) or (a == '.' and (b == '.' or b == '*' or b == '?')) 2
	else 1
}
