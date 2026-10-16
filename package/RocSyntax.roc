import RocSyntaxBuffer

## Lossless syntax scaffolding for the Zig Roc compiler syntax at
## revision `7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5`.
## `.roc-version` pins the implementation toolchain separately.
## Byte spans are half-open. Comments and whitespace are tokens.
##
## Expressions support identifiers, numbers, strings without interpolation,
## calls, field access, lists, tuples, records, blocks, simple lambdas, if/else,
## simple match branches, unary and binary operators. Files support module
## headers, simple imports, value declarations and simple type annotations.
## Other constructs remain
## source-backed `UnsupportedNode` values with an `UnsupportedSyntax` diagnostic.
## This is an evolving syntax API, not a complete Roc validity checker.
##
## Parsing retains the source once and builds persistent radix buffers, then
## returns nodes/children as flat lists. Retained recursive states copy bounded
## radix paths instead of the entire accumulated tree.
## Nesting is bounded at 128; documents over 1,048,576 tokens retain all tokens but
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
	NodeKind : [File, Declaration, ExpressionStatement, Lambda, If, Match, MatchBranch, Pattern, Block, Record, RecordField, Header, Import, TypeAnnotation, TypeExpression, Identifier, NumberLiteral, StringLiteral, CharacterLiteral, Unary, Binary, Call, FieldAccess, Suffix, ListLiteral, Tuple, Parenthesized, ErrorNode, UnsupportedNode]

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

	## Lexer-only result; suitable for highlighting without constructing a tree.
	Tokenized : { source : List(U8), tokens : List(Token), diagnostics : List(Diagnostic), status : Status }

	## Stable presentation categories, independent of theme colors.
	HighlightCategory : [Trivia, CommentText, Name, TypeName, ReservedWord, NumericLiteral, StringText, Symbol, InvalidText]

	## Lossless lexer entry point for arbitrary source bytes.
	tokenize_bytes : List(U8) -> Tokenized
	tokenize_bytes = |source| {
		lexed = lex(source)
		status = if lexed.unsupported Unsupported else if lexed.diagnostics.is_empty() Complete else Recovered
		{ source, tokens: lexed.tokens, diagnostics: lexed.diagnostics, status }
	}

	## Lossless lexer entry point for UTF-8 source text.
	tokenize_str : Str -> Tokenized
	tokenize_str = |source| tokenize_bytes(source.to_utf8())

	## Map one lexical kind to a theme category without allocating.
	## Semantic highlighting can refine names using the syntax tree.
	highlight_category : TokenKind -> HighlightCategory
	highlight_category = |kind| match kind {
		Whitespace | EndOfFile => Trivia
		Comment => CommentText
		LowerIdent => Name
		UpperIdent => TypeName
		Keyword => ReservedWord
		Number => NumericLiteral
		StringStart | StringPart | StringEnd | SingleQuote => StringText
		Invalid => InvalidText
		_ => Symbol
	}

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

State := { source : List(U8), tokens : List(RocSyntax.Token), pos : U64, nodes : RocSyntaxBuffer(RocSyntax.Node), children : RocSyntaxBuffer(U64), diagnostics : List(RocSyntax.Diagnostic), unsupported : Bool, limited : Bool }

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
		if $valid {
			$i = $i + width
		} else {
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
			while $i < source.len() and is_space(byte(source, $i)) {
				$i = $i + 1
			}
		} else if b == '#' {
			$kind = Comment
			while $i < source.len() and byte(source, $i) != '\n' and byte(source, $i) != '\r' {
				$i = $i + 1
			}
		} else if is_lower(b) or is_upper(b) or b == '_' or b == '$' or b == '@' {
			while $i < source.len() and is_ident(byte(source, $i)) {
				$i = $i + 1
			}
			$kind = if matches(source, start, "and") and $i - start == 3 Operator else if matches(source, start, "or") and $i - start == 2 Operator else if keyword(source, start, $i) Keyword else if is_upper(b) or b == '@' UpperIdent else LowerIdent
			if b == '_' and byte(source, start + 1) == '_' {
				$diagnostics = push_diagnostic($diagnostics, UnexpectedToken, start, $i)
			}
			var $j = start + 1
			while $j < $i {
				if byte(source, $j) == '$' or byte(source, $j) >= 0x80 {
					$diagnostics = push_diagnostic($diagnostics, InvalidByte, $j, $j + 1)
				}
				$j = $j + 1
			}
		} else if is_digit(b) or (b == '-' and is_digit(byte(source, start + 1)) and (start == 0 or is_space(byte(source, start - 1)) or !can_end_expression($last_significant))) {
			$kind = Number
			number = scan_number(source, start + if b == '-' 1 else 0)
			$i = number.end
			tuple_index = start > 0 and byte(source, start - 1) == '.' and source.sublist({ start, len: $i - start }).all(is_digit)
			if !number.valid and !tuple_index {
				$diagnostics = push_diagnostic($diagnostics, InvalidNumber, start, $i)
			}
		} else if b == '"' {
			width = 1
			$tokens = $tokens.append({ kind: StringStart, start, end: start + width })
			$i = start + width
			part = $i
			var $closed = False
			while $i < source.len() and !$closed {
				if byte(source, $i) == '\\' {
					escape = scan_escape(source, $i)
					if !escape.valid {
						$diagnostics = push_diagnostic($diagnostics, InvalidEscape, $i, escape.end)
					}
					$i = escape.end
				} else if byte(source, $i) == '\n' or byte(source, $i) == '\r' {
					$closed = True
				} else if matches(source, $i, "\"") {
					$closed = True
				} else {
					if nonprintable(source, $i) {
						$diagnostics = push_diagnostic($diagnostics, InvalidCharacter, $i, $i + 1)
					}
					if matches(source, $i, "\${") {
						$unsupported = True
						$diagnostics = push_diagnostic($diagnostics, UnsupportedSyntax, $i, $i + 2)
					}
					$i = $i + 1
				}
			}
			if $i > part {
				$tokens = $tokens.append({ kind: StringPart, start: part, end: $i })
			}
			if $closed and byte(source, $i) == '"' {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + width })
				$i = $i + width
			} else {
				$diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i)
			}
		} else if b == '\'' {
			$kind = SingleQuote
			var $closed = False
			while $i < source.len() and !$closed and byte(source, $i) != '\n' {
				if byte(source, $i) == '\\' {
					escape = scan_escape(source, $i)
					if !escape.valid {
						$diagnostics = push_diagnostic($diagnostics, InvalidEscape, $i, escape.end)
					}
					$i = escape.end
				} else if byte(source, $i) == '\'' {
					$i = $i + 1
					$closed = True
				} else {
					$i = $i + 1
				}
			}
			if !$closed {
				$diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i)
			} else {
				var $chars = 0
				var $j = start + 1
				while $j + 1 < $i {
					if byte(source, $j) == '\\' {
						$j = scan_escape(source, $j).end
						$chars = $chars + 1
					}
						else {
							if byte(source, $j) < 0x80 or byte(source, $j) >= 0xC0 {
								$chars = $chars + 1
							}
							$j = $j + 1
						}
				}
				if $chars != 1 {
					$diagnostics = push_diagnostic($diagnostics, InvalidCharacter, start, $i)
				}
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
			if $kind == Invalid {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i)
			}
		}
		if b != '"' {
			$tokens = $tokens.append({ kind: $kind, start, end: $i })
		}
		if b == '"' {
			$last_significant = StringEnd
		}
			else if $kind != Whitespace and $kind != Comment {
				$last_significant = $kind
			}
	}
	{ tokens: $tokens.append({ kind: EndOfFile, start: source.len(), end: source.len() }), diagnostics: $diagnostics, unsupported: $unsupported }
}

peek : State -> RocSyntax.Token
peek = |s| s.tokens.get(s.pos) ?? { kind: EndOfFile, start: s.source.len(), end: s.source.len() }

skip_trivia : State -> State
skip_trivia = |State.{ source, tokens, pos, nodes, children, diagnostics, unsupported, limited }| {
	var $pos = pos
	var $done = False
	while !$done {
		kind = (tokens.get($pos) ?? { kind: EndOfFile, start: source.len(), end: source.len() }).kind
		if kind == Whitespace or kind == Comment {
			$pos = $pos + 1
		} else {
			$done = True
		}
	}
	State.{ source, tokens, pos: $pos, nodes, children, diagnostics, unsupported, limited }
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
add_node = |State.{ source, tokens, pos, nodes, children, diagnostics, unsupported, limited }, kind, start, end, ids| {
	index = nodes.len()
	node = { kind, start, end, children_start: children.len(), children_len: ids.len() }
	{ state: State.{ source, tokens, pos, nodes: nodes.append(node), children: children.concat_list(ids), diagnostics, unsupported, limited }, node: index }
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
		add_node(at_pos(limited, limited.pos + if first.kind == EndOfFile 0 else 1), ErrorNode, first.start, first.end, [])
	} else {
		{ state: built_state, node: built_node } = primary(s, depth)
		var $s = built_state
		var $left = built_node
		var $qualified = first.kind == UpperIdent
		var $done = False
		while !$done {
			look = skip_trivia($s)
			# Drop the previous loop-state owner before builders grow its buffers.
			$s = vacant
			t = peek(look)
			bp = binding(look)
			if spelling(look, "?") and t.start == node_end(look, $left) {
				start = node_start(look, $left)
				{ state: suffix_state, node: suffix_node } = add_node(at_pos(look, look.pos + 1), Suffix, start, t.end, [$left])
				$s = suffix_state
				$left = suffix_node
				$qualified = False
			} else if t.kind == NoSpaceOpenRound {
				start = node_start(look, $left)
				{ state: args_state, ids: args_ids, end: args_end, comma: _ } = collection(at_pos(look, look.pos + 1), CloseRound, depth + 1)
				ids = [$left].concat(args_ids)
				{ state: call_state, node: call_node } = add_node(args_state, Call, start, args_end, ids)
				$s = call_state
				$left = call_node
				$qualified = False
			} else if spelling(look, ".") {
				field_state = at_pos(look, look.pos + 1)
				field = peek(field_state)
				valid_name = (field.kind == LowerIdent and is_lower(byte(field_state.source, field.start))) or ($qualified and field.kind == UpperIdent and is_upper(byte(field_state.source, field.start)))
				valid_index = field.kind == Number and field_state.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit)
				if valid_name or valid_index {
					start = node_start(field_state, $left)
					{ state: leaf_state, node: leaf_node } = add_node(at_pos(field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
					{ state: access_state, node: access_node } = add_node(leaf_state, FieldAccess, start, field.end, [$left, leaf_node])
					$s = access_state
					$left = access_node
					$qualified = $qualified and field.kind == UpperIdent
				} else {
					$s = look
					$done = True
				}
			} else if bp.left > 0 and bp.left >= minimum {
				start = node_start(look, $left)
				{ state: right_state, node: right_node } = expression(at_pos(look, look.pos + 1), bp.right, depth + 1)
				end = node_end(right_state, right_node)
				{ state: binary_state, node: binary_node } = add_node(right_state, Binary, start, end, [$left, right_node])
				$s = binary_state
				$left = binary_node
				$qualified = False
			} else {
				$s = look
				$done = True
			}
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
		$s = vacant
		t = peek(look)
		if t.kind == closing {
			$s = at_pos(look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			{ state: item_state, node: item_node } = expression(look, 0, depth)
			$ids = $ids.append(item_node)
			$end = node_end(item_state, item_node)
			$s = skip_trivia(item_state)
			if spelling($s, ",") {
				$comma = True
				$s = at_pos($s, $s.pos + 1)
			} else if peek($s).kind != closing and peek($s).kind != EndOfFile {
				bad = peek($s)
				$s = issue($s, UnexpectedToken, bad.start, bad.end)
				if $s.pos <= before {
					$s = at_pos($s, $s.pos + 1)
				}
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: $comma }
}

primary : State, U64 -> Built
primary = |s, depth| {
	t = peek(s)
	match t.kind {
		LowerIdent if spelling(s, "_") => {
			bad = issue(at_pos(s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(bad, ErrorNode, t.start, t.end, [])
		}
		UpperIdent if byte(s.source, t.start) == '@' => {
			bad = issue(at_pos(s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(bad, ErrorNode, t.start, t.end, [])
		}
		LowerIdent | UpperIdent => add_node(at_pos(s, s.pos + 1), Identifier, t.start, t.end, [])
		Number => add_node(at_pos(s, s.pos + 1), NumberLiteral, t.start, t.end, [])
		SingleQuote => add_node(at_pos(s, s.pos + 1), CharacterLiteral, t.start, t.end, [])
		StringStart => {
			var $s = at_pos(s, s.pos + 1)
			var $end = t.end
			if peek($s).kind == StringPart {
				$end = peek($s).end
				$s = at_pos($s, $s.pos + 1)
			}
			if peek($s).kind == StringEnd {
				$end = peek($s).end
				$s = at_pos($s, $s.pos + 1)
			}
			add_node($s, StringLiteral, t.start, $end, [])
		}
		OpenCurly => curly(at_pos(s, s.pos + 1), t.start, depth + 1)
		Keyword if spelling(s, "if") => parse_if(at_pos(s, s.pos + 1), t.start, depth + 1)
		Keyword if spelling(s, "match") => parse_match(at_pos(s, s.pos + 1), t.start, depth + 1)
		Operator if spelling(s, "|") => parse_lambda(at_pos(s, s.pos + 1), t.start, depth + 1)
		OpenRound | NoSpaceOpenRound | OpenSquare => {
			{ state: items_state, ids: items_ids, end: items_end, comma: items_comma } = collection(at_pos(s, s.pos + 1), if t.kind == OpenSquare CloseSquare else CloseRound, depth + 1)
			kind = if t.kind == OpenSquare ListLiteral else if items_comma or items_ids.len() != 1 Tuple else Parenthesized
			add_node(items_state, kind, t.start, items_end, items_ids)
		}
		Operator if spelling(s, "-") and binary_minus(s) => {
			bad = issue(at_pos(s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(bad, ErrorNode, t.start, t.end, [])
		}
		Operator if spelling(s, "-") or spelling(s, "!") => {
			{ state: operand_state, node: operand_node } = expression(at_pos(s, s.pos + 1), 40, depth + 1)
			end = node_end(operand_state, operand_node)
			add_node(operand_state, Unary, t.start, end, [operand_node])
		}
		EndOfFile | CloseRound | CloseSquare | CloseCurly => {
			err = issue(s, UnexpectedToken, t.start, t.end)
			add_node(err, ErrorNode, t.start, t.start, [])
		}
		Invalid => add_node(at_pos(s, s.pos + 1), ErrorNode, t.start, t.end, [])
		_ => {
			unknown = issue(at_pos(s, s.pos + 1), UnsupportedSyntax, t.start, t.end)
			add_node(unknown, UnsupportedNode, t.start, t.end, [])
		}
	}
}

parse_source : List(U8), Bool -> RocSyntax.Parsed
parse_source = |source, expression_only| {
	lexed = lex(source)
	initial = State.{ source, tokens: lexed.tokens, pos: 0, nodes: RocSyntaxBuffer.empty({}), children: RocSyntaxBuffer.empty({}), diagnostics: lexed.diagnostics, unsupported: lexed.unsupported, limited: False }
	var $s = initial
	var $roots = []
	if $s.tokens.len() > 1048576 {
		$s = issue($s, ResourceLimit, 0, source.len())
	} else {
		var $done = False
		while !$done {
			look = skip_trivia($s)
			# Drop the previous loop-state owner before builders grow its buffers.
			$s = vacant
			t = peek(look)
			if t.kind == EndOfFile {
				$s = look
				$done = True
			} else {
				before = look.pos
				{ state: built_state, node: built_node } = if expression_only expression(look, 0, 0) else statement(look, 0)
				$roots = $roots.append(built_node)
				$s = built_state
				if !expression_only {
					if ($s.nodes.get(built_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == ExpressionStatement {
						$s = issue($s, UnexpectedToken, t.start, node_end($s, built_node))
					}
					look_next = skip_trivia($s)
					end = node_end(look_next, built_node)
					if peek(look_next).kind != EndOfFile and !line_break(look_next.source, end, peek(look_next).start) {
						$s = issue($s, UnexpectedToken, peek(look_next).start, peek(look_next).end)
					}
				}
				if $s.pos <= before {
					$s = issue($s, UnexpectedToken, t.start, t.end)
					$s = at_pos($s, before + 1)
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
	{ state: root_state, node: root_node } = add_node($s, File, 0, source.len(), $roots)
	nodes = root_state.nodes.to_list()
	first_start = (source_token(root_state, significant_position(root_state.tokens, 0))).start
	var $header_count = 0.U64
	var $bad_header = False
	for n in nodes {
		if n.kind == Header {
			$header_count = $header_count + 1
			$bad_header = $bad_header or n.start != first_start
		}
	}
	finished = if $header_count > 1 or $bad_header issue(root_state, UnexpectedToken, first_start, source.len()) else root_state
	has_error = finished.diagnostics.any(|d| d.code != UnsupportedSyntax)
	status = if finished.limited Limited else if finished.unsupported Unsupported else if has_error Recovered else Complete
	{ source: finished.source, tokens: finished.tokens, nodes, children: finished.children.to_list(), diagnostics: finished.diagnostics, status, root: root_node, consumed: peek(skip_trivia(finished)).kind == EndOfFile }
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
		} else if is_digit(prefix) {
			$valid = False
		}
	}
	var $digits = 0.U64
	var $done = False
	while $i < source.len() and !$done {
		b = byte(source, $i)
		digit = if is_digit(b) U8.to_u64(b - '0') else if b >= 'a' and b <= 'f' U8.to_u64(b - 'a') + 10 else if b >= 'A' and b <= 'F' U8.to_u64(b - 'A') + 10 else 99
		if b == '_' {
			$i = $i + 1
		}
			else if digit < $base {
				$digits = $digits + 1
				$i = $i + 1
			}
				else {
					$done = True
				}
	}
	$valid = $valid and $digits > 0
	if $base == 10 {
		if byte(source, $i) == '.' and (is_digit(byte(source, $i + 1)) or byte(source, $i + 1) == 'e' or byte(source, $i + 1) == 'E') {
			$i = $i + 1
			while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') {
				$i = $i + 1
			}
		}
		if byte(source, $i) == 'e' or byte(source, $i) == 'E' {
			$i = $i + 1
			if byte(source, $i) == '+' or byte(source, $i) == '-' {
				$i = $i + 1
			}
			var $exponent_digits = 0.U64
			while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') {
				if is_digit(byte(source, $i)) {
					$exponent_digits = $exponent_digits + 1
				}
				$i = $i + 1
			}
			$valid = $valid and $exponent_digits > 0
		}
	}
	# Old numeric suffix spellings are intentionally unsupported: the pinned
	# parser diagnoses these deprecated forms. Retain them in the number span.
	if $i < source.len() and (is_ident(byte(source, $i))) {
		$valid = False
		while $i < source.len() and is_ident(byte(source, $i)) {
			$i = $i + 1
		}
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
			if $digits < 7 {
				$value = $value * 16 + digit
			}
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
	following_pos = significant_position(s.tokens, s.pos + 1)
	following = s.tokens.get(following_pos) ?? t
	if depth >= 128 {
		bad = issue(s, ResourceLimit, t.start, t.end)
		add_node(at_pos(bad, bad.pos + if t.kind == EndOfFile 0 else 1), ErrorNode, t.start, t.end, [])
	} else if t.kind == LowerIdent and token_spelling(s.source, following, "=") {
		{ state: name_state, node: name_node } = add_node(at_pos(s, following_pos), Identifier, t.start, t.end, [])
		{ state: value_state, node: value_node } = expression(at_pos(name_state, name_state.pos + 1), 0, depth)
		end = node_end(value_state, value_node)
		add_node(value_state, Declaration, t.start, end, [name_node, value_node])
	} else if t.kind == LowerIdent and token_spelling(s.source, following, ":") {
		{ state: name_state, node: name_node } = add_node(at_pos(s, following_pos), Identifier, t.start, t.end, [])
		{ state: ty_state, node: ty_node } = type_expression(at_pos(name_state, name_state.pos + 1), depth)
		end = node_end(ty_state, ty_node)
		add_node(ty_state, TypeAnnotation, t.start, end, [name_node, ty_node])
	} else if spelling(s, "module") {
		look = skip_trivia(at_pos(s, s.pos + 1))
		if peek(look).kind == OpenSquare {
			{ state: items_state, ids: items_ids, end: items_end, comma: _ } = collection(at_pos(look, look.pos + 1), CloseSquare, depth + 1)
			validated = if items_ids.all(|id| (items_state.nodes.get(id) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == Identifier) items_state else issue(items_state, UnexpectedToken, t.start, items_end)
			add_node(validated, Header, t.start, items_end, items_ids)
		} else {
			bad = issue(look, UnsupportedSyntax, t.start, peek(look).end)
			add_node(bad, UnsupportedNode, t.start, t.end, [])
		}
	} else if spelling(s, "import") {
		{ state: imported_state, node: imported_node } = expression(at_pos(s, s.pos + 1), 0, depth)
		end = node_end(imported_state, imported_node)
		validated = if valid_import(imported_state, imported_node) imported_state else issue(imported_state, UnexpectedToken, t.start, end)
		add_node(validated, Import, t.start, end, [imported_node])
	} else {
		{ state: value_state, node: value_node } = expression(s, 0, depth)
		end = node_end(value_state, value_node)
		add_node(value_state, ExpressionStatement, t.start, end, [value_node])
	}
}

type_expression : State, U64 -> Built
type_expression = |initial, depth| {
	if depth >= 128 {
		t = peek(initial)
		bad = issue(initial, ResourceLimit, t.start, t.end)
		add_node(bad, ErrorNode, t.start, t.start, [])
	} else {
		{ state: left_state, node: left_node } = expression(initial, 40, depth + 1)
		look = skip_trivia(left_state)
		if spelling(look, "->") {
			start = node_start(look, left_node)
			validated = if type_tree_supported(look, left_node) look else issue(look, UnsupportedSyntax, start, node_end(look, left_node))
			{ state: right_state, node: right_node } = type_expression(at_pos(validated, validated.pos + 1), depth + 1)
			end = node_end(right_state, right_node)
			add_node(right_state, TypeExpression, start, end, [left_node, right_node])
		} else {
			if type_tree_supported(left_state, left_node) {
				{ state: left_state, node: left_node }
			}
				else {
					start = node_start(left_state, left_node)
					end = node_end(left_state, left_node)
					{ state: issue(left_state, UnsupportedSyntax, start, end), node: left_node }
				}
		}
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
			$s = at_pos($s, $s.pos + 1)
			$done = True
		} else if t.kind == LowerIdent {
			{ state: param_state, node: param_node } = add_node(at_pos($s, $s.pos + 1), Pattern, t.start, t.end, [])
			$ids = $ids.append(param_node)
			$s = skip_trivia(param_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			}
				else if !spelling($s, "|") {
					$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
					$done = True
				}
		} else {
			$s = issue($s, if t.kind == EndOfFile UnclosedDelimiter else UnsupportedSyntax, t.start, t.end)
			$done = True
		}
	}
	{ state: body_state, node: body_node } = expression($s, 0, depth)
	end = node_end(body_state, body_node)
	add_node(body_state, Lambda, start, end, $ids.append(body_node))
}

parse_if : State, U64, U64 -> Built
parse_if = |initial, start, depth| {
	{ state: condition_state, node: condition_node } = expression(initial, 0, depth)
	look = skip_trivia(condition_state)
	{ state: then_branch_state, node: then_branch_node } = expression(look, 0, depth)
	after = skip_trivia(then_branch_state)
	if spelling(after, "else") {
		{ state: other_state, node: other_node } = expression(at_pos(after, after.pos + 1), 0, depth)
		end = node_end(other_state, other_node)
		add_node(other_state, If, start, end, [condition_node, then_branch_node, other_node])
	} else {
		end = node_end(after, then_branch_node)
		add_node(after, If, start, end, [condition_node, then_branch_node])
	}
}

curly : State, U64, U64 -> Built
curly = |initial, start, depth| {
	look = skip_trivia(initial)
	first = peek(look)
	after = skip_trivia(at_pos(look, look.pos + 1))
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
			$s = at_pos($s, $s.pos + 1)
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$end = t.start
			$s = issue($s, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = $s.pos
			if record {
				{ state: name_state, node: name_node } = add_node(at_pos($s, $s.pos + 1), Identifier, t.start, t.end, [])
				var $field_state = name_state
				var $field_node = name_node
				field_look = skip_trivia(name_state)
				if t.kind != LowerIdent {
					bad = issue(field_look, UnsupportedSyntax, t.start, t.end)
					{ state: next_field_state, node: next_field_node } = add_node(bad, UnsupportedNode, t.start, t.end, [])
					$field_state = next_field_state
					$field_node = next_field_node
				} else if spelling(field_look, ":") {
					{ state: value_state, node: value_node } = expression(at_pos(field_look, field_look.pos + 1), 0, depth)
					end = node_end(value_state, value_node)
					{ state: next_field_state, node: next_field_node } = add_node(value_state, RecordField, t.start, end, [name_node, value_node])
					$field_state = next_field_state
					$field_node = next_field_node
				} else {
					{ state: next_field_state, node: next_field_node } = add_node(field_look, RecordField, t.start, t.end, [name_node])
					$field_state = next_field_state
					$field_node = next_field_node
				}
				$ids = $ids.append($field_node)
				$end = node_end($field_state, $field_node)
				$s = skip_trivia($field_state)
				if spelling($s, ",") {
					$s = at_pos($s, $s.pos + 1)
				}
					else if peek($s).kind != CloseCurly and peek($s).kind != EndOfFile {
						$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
					}
			} else {
				{ state: item_state, node: item_node } = statement($s, depth)
				$ids = $ids.append(item_node)
				$end = node_end(item_state, item_node)
				$s = item_state
				look_next = skip_trivia($s)
				if peek(look_next).kind != CloseCurly and peek(look_next).kind != EndOfFile and !line_break(look_next.source, $end, peek(look_next).start) {
					$s = issue($s, UnexpectedToken, peek(look_next).start, peek(look_next).end)
				}
			}
			if $s.pos <= before {
				$s = at_pos($s, before + 1)
			}
		}
	}
	add_node($s, if record Record else Block, start, $end, $ids)
}

parse_match : State, U64, U64 -> Built
parse_match = |initial, start, depth| {
	{ state: value_state, node: value_node } = expression(initial, 0, depth)
	look = skip_trivia(value_state)
	if peek(look).kind != OpenCurly {
		bad = issue(look, UnexpectedToken, peek(look).start, peek(look).end)
		add_node(bad, Match, start, node_end(bad, value_node), [value_node])
	} else {
		var $s = at_pos(look, look.pos + 1)
		var $ids = [value_node]
		var $end = peek(look).end
		var $done = False
		while !$done {
			$s = skip_trivia($s)
			t = peek($s)
			if t.kind == CloseCurly {
				$end = t.end
				$s = at_pos($s, $s.pos + 1)
				$done = True
			} else if t.kind == EndOfFile {
				$s = issue($s, UnclosedDelimiter, t.start, t.end)
				$end = t.start
				$done = True
			} else {
				before = $s.pos
				{ state: pattern_value_state, node: pattern_value_node } = expression($s, 0, depth)
				pattern_end = node_end(pattern_value_state, pattern_value_node)
				pattern_validated = if valid_pattern(pattern_value_state, pattern_value_node) pattern_value_state else issue(pattern_value_state, UnexpectedToken, t.start, pattern_end)
				{ state: pattern_state, node: pattern_node } = add_node(pattern_validated, Pattern, t.start, pattern_end, [pattern_value_node])
				arrow = skip_trivia(pattern_state)
				if spelling(arrow, "=>") {
					{ state: branch_value_state, node: branch_value_node } = expression(at_pos(arrow, arrow.pos + 1), 0, depth)
					branch_end = node_end(branch_value_state, branch_value_node)
					{ state: branch_state, node: branch_node } = add_node(branch_value_state, MatchBranch, t.start, branch_end, [pattern_node, branch_value_node])
					$ids = $ids.append(branch_node)
					$s = skip_trivia(branch_state)
					$end = branch_end
					if spelling($s, ",") {
						$s = at_pos($s, $s.pos + 1)
					}
				} else {
					$s = issue(arrow, UnsupportedSyntax, t.start, peek(arrow).end)
					$ids = $ids.append(pattern_node)
				}
				if $s.pos <= before {
					$s = at_pos($s, before + 1)
				}
			}
		}
		if $ids.len() == 1 {
			$s = issue($s, UnexpectedToken, start, $end)
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
	is_space(next) or next == '#' or (is_digit(next) and previous.end == t.start and can_end_expression(previous.kind)) or (!is_digit(next) and !is_lower(next) and !is_upper(next) and next != '_' and next != '(' and next < 0x80)
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

# This small type subset has the same identifier/application syntax as values.
# Reject unsupported value-only nodes rather than falsely calling them valid types.
type_tree_supported : State, U64 -> Bool
type_tree_supported = |s, root| {
	var $work = [root]
	var $depth = 1
	var $valid = True
	while $depth > 0 and $valid {
		$depth = $depth - 1
		id = $work.get($depth) ?? s.nodes.len()
		match s.nodes.get(id) {
			Err(_) => {
				$valid = False
			}
			Ok(n) => {
				$valid = n.kind == Identifier or n.kind == Call or n.kind == Parenthesized or n.kind == Tuple or n.kind == ListLiteral or n.kind == TypeExpression
				var $i = 0
				while $valid and $i < n.children_len {
					child = s.children.get(n.children_start + $i) ?? s.nodes.len()
					$work = if $depth < $work.len() $work.set($depth, child) ?? $work else $work.append(child)
					$depth = $depth + 1
					$i = $i + 1
				}
			}
		}
	}
	$valid
}

expect {
	p = RocSyntax.parse_expr_str("result?")
	p.status == Complete and p.nodes.any(|n| n.kind == Suffix)
}

expect {
	RocSyntax.parse_expr_str("- value").status == Recovered and RocSyntax.parse_str("x : 42").status == Unsupported
}

expect {
	["x.1e3", "x.0xff", "x.Foo", "x._", "x. field", "@Foo", "@a", "__", "__a"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

expect {
	["x .field", "x\n.field", "x.01", "x.a!", "_a"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["Foo.Bar", "Foo.Bar.Baz", "Foo .Bar", "Foo.Bar.baz"].all(|source| RocSyntax.parse_expr_str(source).status == Complete) and ["Foo.bar.Bar", "Foo.0.Bar", "Foo().Bar", "(Foo).Bar"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

expect {
	["x : 1 -> U64", "x : \"hi\" -> U64", "x : [1] -> U64", "x : U64 -> 1 -> U64"].all(|source| RocSyntax.parse_str(source).status != Complete) and RocSyntax.parse_str("x : U64 -> U64").status == Complete
}

# Empty lists own no buffers. Loop parsers move their old state into a local,
# then clear the loop variable before invoking a growing builder. Keeping both
# records alive would force copy-on-write at every append in native Roc builds.
vacant : State
vacant = State.{ source: [], tokens: [], pos: 0, nodes: RocSyntaxBuffer.empty({}), children: RocSyntaxBuffer.empty({}), diagnostics: [], unsupported: False, limited: False }

expect {
	["-!h", "-'i'", "match 42 {}", "person.@"].all(|source| RocSyntax.parse_expr_str(source).status != Complete) and ["F\n", "modu\n", "S\n0\n", "x = _\n\ny = { a: _ + 1 }"].all(|source| RocSyntax.parse_str(source).status != Complete)
}

# Consume a state when advancing. Avoid a record spread retaining an alias to
# its growing buffers across a recursive parse call.
at_pos : State, U64 -> State
at_pos = |State.{ source, tokens, pos: _, nodes, children, diagnostics, unsupported, limited }, position| {
	State.{ source, tokens, pos: position, nodes, children, diagnostics, unsupported, limited }
}

significant_position : List(RocSyntax.Token), U64 -> U64
significant_position = |tokens, position| {
	var $pos = position
	var $done = False
	while !$done {
		match tokens.get($pos) {
			Ok(t) if t.kind == Whitespace or t.kind == Comment => {
				$pos = $pos + 1
			}
			_ => {
				$done = True
			}
		}
	}
	$pos
}

token_spelling : List(U8), RocSyntax.Token, Str -> Bool
token_spelling = |source, token, text| token.end - token.start == text.count_utf8_bytes() and matches(source, token.start, text)

source_token : State, U64 -> RocSyntax.Token
source_token = |s, pos| s.tokens.get(pos) ?? { kind: EndOfFile, start: s.source.len(), end: s.source.len() }

valid_import : State, U64 -> Bool
valid_import = |s, id| {
	match s.nodes.get(id) {
		Ok(n) if n.kind == Identifier => is_upper(byte(s.source, n.start))
		Ok(n) if n.kind == FieldAccess => {
			last = s.children.get(n.children_start + 1) ?? s.nodes.len()
			match s.nodes.get(last) {
				Ok(field) => is_upper(byte(s.source, field.start))
				Err(_) => False
			}
		}
		_ => False
	}
}

valid_pattern : State, U64 -> Bool
valid_pattern = |s, root| {
	var $work = [root]
	var $depth = 1
	var $valid = True
	while $depth > 0 and $valid {
		$depth = $depth - 1
		id = $work.get($depth) ?? s.nodes.len()
		match s.nodes.get(id) {
			Err(_) => {
				$valid = False
			}
			Ok(n) => {
				$valid = n.kind == Identifier or n.kind == NumberLiteral or n.kind == StringLiteral or n.kind == CharacterLiteral or n.kind == Call or n.kind == Tuple or n.kind == ListLiteral or n.kind == Parenthesized
				var $i = 0
				while $valid and $i < n.children_len {
					child = s.children.get(n.children_start + $i) ?? s.nodes.len()
					$work = if $depth < $work.len() $work.set($depth, child) ?? $work else $work.append(child)
					$depth = $depth + 1
					$i = $i + 1
				}
			}
		}
	}
	$valid
}

expect {
	["module [1 + 2]", "import 123", "import lower", "import f(1)", "module []\nmodule []"].all(|source| RocSyntax.parse_str(source).status != Complete) and RocSyntax.parse_expr_str("match x { 1 + 2 => 3 }").status != Complete
}

expect {
	source = "module []\nanswer = 42 # hello\n"
	tokens = RocSyntax.tokenize_str(source)
	parsed = RocSyntax.parse_str(source)
	tokens.status == Complete and tokens.tokens == parsed.tokens and tokens.source == parsed.source and tokens.diagnostics == parsed.diagnostics
}

expect {
	tokens = RocSyntax.tokenize_bytes([0xFF, 0x22])
	tokens.status == Recovered and !tokens.diagnostics.is_empty() and tokens.source == [0xFF, 0x22] and RocSyntax.highlight_category(Comment) == CommentText and RocSyntax.highlight_category(UpperIdent) == TypeName
}
