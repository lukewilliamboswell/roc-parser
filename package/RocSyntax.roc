import RocSyntaxBuffer

## Lossless parsing of Roc source into tokens, syntax trees and diagnostics.
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
	TokenKind : [Whitespace, Comment, LowerIdent, UpperIdent, Keyword, Number, StringStart, MultilineStringStart, StringPart, StringEnd, InterpolationStart, InterpolationEnd, SingleQuote, OpenRound, NoSpaceOpenRound, CloseRound, OpenSquare, CloseSquare, OpenCurly, CloseCurly, Operator, Punctuation, Invalid, EndOfFile]

	## Lossless token. Only EOF has an empty span.
	Token : { kind : TokenKind, start : U64, end : U64 }

	## Syntax categories. Children are indices into `Parsed.nodes`.
	NodeKind : [File, Declaration, VariableDeclaration, Expect, For, ForStream, While, ExpressionStatement, Lambda, If, Match, MatchBranch, Pattern, IdentifierPattern, WildcardPattern, PatternStringLiteral, CapturePattern, MutablePattern, TagPattern, NominalPattern, TuplePattern, ListPattern, RecordPattern, PatternField, RestPattern, AliasPattern, AlternativePattern, Block, Record, RecordField, Header, Dependencies, DependencyField, PlatformReference, Import, FileImport, ImportPath, ImportAlias, ExposedItem, ExposedWildcard, TypeAnnotation, TypeExpression, Identifier, NumberLiteral, StringLiteral, MultilineStringLiteral, StringSegment, Interpolation, TypedLiteral, CharacterLiteral, Unary, Binary, Call, FieldAccess, OptionalFieldAccess, Suffix, Debug, Crash, Return, Break, Ellipsis, ListLiteral, Tuple, Parenthesized, ErrorNode, UnsupportedNode]

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
		StringStart | MultilineStringStart | StringPart | StringEnd | SingleQuote => StringText
		Invalid => InvalidText
		_ => Symbol
	}

	## Parse source text; offsets refer to UTF-8 bytes, not Unicode characters.
	parse_str : Str -> Parsed
	parse_str = |source| parse_bytes(source.to_utf8())

	## Parse arbitrary source bytes, retaining invalid bytes and incomplete syntax.
	parse_bytes : List(U8) -> Parsed
	parse_bytes = |source| parse_source(source, FileMode)

	## Parse one expression. Extra significant tokens produce a diagnostic.
	parse_expression : Str -> Parsed
	parse_expression = |source| parse_source(source.to_utf8(), ExpressionMode)

	## Parse one expression from UTF-8 text.
	parse_expr_str : Str -> Parsed
	parse_expr_str = |source| parse_source(source.to_utf8(), ExpressionMode)

	## Parse one expression from arbitrary bytes, preserving malformed input.
	parse_expr_bytes : List(U8) -> Parsed
	parse_expr_bytes = |source| parse_source(source, ExpressionMode)

	## Parse one statement in a body, including expression statements and locals.
	parse_statement_str : Str -> Parsed
	parse_statement_str = |source| parse_source(source.to_utf8(), StatementMode)

	## Parse one file header and require complete input consumption.
	parse_header_str : Str -> Parsed
	parse_header_str = |source| parse_source(source.to_utf8(), HeaderMode)
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
	var $last_significant = EndOfFile
	var $i = 0
	var $contexts = []
	var $resume = False
	var $resume_multiline = False
	while $i < source.len() or $resume {
		start = $i
		b = byte(source, start)
		var $kind = Invalid
		var $emitted = False
		$i = $i + 1
		if $resume or b == '"' or (b == '\\' and byte(source, start + 1) == '\\') {
			$emitted = True
			multiline = if $resume $resume_multiline else b == '\\' or matches(source, start, "\"\"\"")
			if $resume {
				$i = start
			} else {
				width = if b == '\\' 2 else if multiline 3 else 1
				$tokens = $tokens.append({ kind: if multiline MultilineStringStart else StringStart, start, end: start + width })
				$i = start + width
			}
			$resume = False
			part = $i
			var $done = False
			while $i < source.len() and !$done {
				c = byte(source, $i)
				if c == '\\' {
					escape = scan_escape(source, $i)
					if !escape.valid {
						$diagnostics = push_diagnostic($diagnostics, InvalidEscape, $i, escape.end)
					}
					$i = escape.end
				} else if c == '\n' or (!multiline and c == '"') or (c == '$' and byte(source, $i + 1) == '{') {
					$done = True
				} else {
					if nonprintable(source, $i) {
						$diagnostics = push_diagnostic($diagnostics, InvalidCharacter, $i, $i + 1)
					}
					$i = $i + 1
				}
			}
			if $i > part {
				$tokens = $tokens.append({ kind: StringPart, start: part, end: $i })
			}
			if byte(source, $i) == '$' and byte(source, $i + 1) == '{' {
				$tokens = $tokens.append({ kind: InterpolationStart, start: $i, end: $i + 2 })
				$contexts = $contexts.append({ multiline, depth: 0.U64 })
				$i = $i + 2
				$last_significant = InterpolationStart
			} else if !multiline and $i < source.len() and byte(source, $i) == '"' {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + 1 })
				$i = $i + 1
				$last_significant = StringEnd
			} else if !multiline {
				$diagnostics = push_diagnostic($diagnostics, UnclosedString, start, $i)
			}
		} else if b == '}' and !$contexts.is_empty() and ($contexts.last() ?? { multiline: False, depth: 0 }).depth == 0 {
			context = $contexts.last() ?? { multiline: False, depth: 0 }
			$contexts = $contexts.drop_last(1)
			$kind = InterpolationEnd
			$resume = True
			$resume_multiline = context.multiline
		} else if is_space(b) {
			$kind = Whitespace
			if b == '\r' and byte(source, start + 1) != '\n' {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, start + 1)
			}
			while $i < source.len() and is_space(byte(source, $i)) {
				if byte(source, $i) == '\r' and byte(source, $i + 1) != '\n' {
					$diagnostics = push_diagnostic($diagnostics, InvalidByte, $i, $i + 1)
				}
				$i = $i + 1
			}
		} else if b == '#' {
			$kind = Comment
			while $i < source.len() and byte(source, $i) != '\n' and byte(source, $i) != '\r' {
				$i = $i + 1
			}
		} else if is_lower(b) or is_upper(b) or b == '_' or b == '$' or b == '@' or b >= 0x80 {
			next = byte(source, start + 1)
			standalone_underscore = b == '_' and !(is_lower(next) or is_upper(next) or is_digit(next))
			bad_dollar = b == '$' and !(is_lower(next) or is_upper(next))
			bad_opaque = b == '@' and !(is_lower(next) or is_upper(next) or is_digit(next) or next == '_' or next >= 0x80)
			if !standalone_underscore and !bad_dollar and !bad_opaque {
				while $i < source.len() and is_ident(byte(source, $i)) {
					$i = $i + 1
				}
			}
			$kind = if standalone_underscore LowerIdent else if b == '_' LowerIdent else if b == '@' UpperIdent else if bad_dollar Invalid else if matches(source, start, "and") and $i - start == 3 Operator else if matches(source, start, "or") and $i - start == 2 Operator else if keyword(source, start, $i) Keyword else if is_upper(b) or (b == '$' and is_upper(next)) UpperIdent else LowerIdent
			if bad_dollar or bad_opaque or b >= 0x80 {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i)
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
					if nonprintable(source, $i) {
						$diagnostics = push_diagnostic($diagnostics, InvalidCharacter, $i, $i + 1)
					}
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
		if !$emitted {
			$tokens = $tokens.append({ kind: $kind, start, end: $i })
		}
		if !$contexts.is_empty() and ($kind == OpenCurly or $kind == CloseCurly) {
			index = $contexts.len() - 1
			context = $contexts.last() ?? { multiline: False, depth: 0 }
			next_depth = if $kind == OpenCurly context.depth + 1 else if context.depth > 0 context.depth - 1 else 0
			$contexts = $contexts.set(index, { multiline: context.multiline, depth: next_depth }) ?? $contexts
		}
		if $kind != Invalid and $kind != Whitespace and $kind != Comment {
			$last_significant = $kind
		}
	}
	if !$contexts.is_empty() {
		$diagnostics = push_diagnostic($diagnostics, UnclosedDelimiter, source.len(), source.len())
	}

	{ tokens: $tokens.append({ kind: EndOfFile, start: source.len(), end: source.len() }), diagnostics: $diagnostics, unsupported: False }
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

expression : State, U64, U64, Bool -> Built
expression = |initial, minimum, depth, punned| {
	s = skip_trivia(initial)
	first = peek(s)
	if depth >= 128 {
		limited = issue(s, ResourceLimit, first.start, first.end)
		add_node(at_pos(limited, limited.pos + if first.kind == EndOfFile 0 else 1), ErrorNode, first.start, first.end, [])
	} else {
		{ state: built_state, node: built_node } = primary(s, depth, punned)
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
				validated = if is_optional_access(look, $left) issue(look, UnexpectedToken, t.start, t.end) else look
				start = node_start(look, $left)
				{ state: args_state, ids: args_ids, end: args_end, comma: _ } = collection(at_pos(validated, validated.pos + 1), CloseRound, depth + 1)
				ids = [$left].concat(args_ids)
				{ state: call_state, node: call_node } = add_node(args_state, Call, start, args_end, ids)
				$s = call_state
				$left = call_node
				$qualified = False
			} else if spelling(look, ".") or spelling(look, ".?") {
				optional = spelling(look, ".?")
				field_state = at_pos(look, look.pos + 1)
				field = peek(field_state)
				valid_name = (field.kind == LowerIdent and is_lower(byte(field_state.source, field.start))) or ($qualified and field.kind == UpperIdent and is_upper(byte(field_state.source, field.start)))
				valid_index = field.kind == Number and field_state.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit)
				if (valid_name and (!optional or field.kind == LowerIdent)) or (valid_index and !optional) {
					start = node_start(field_state, $left)
					{ state: leaf_state, node: leaf_node } = add_node(at_pos(field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
					{ state: access_state, node: access_node } = add_node(leaf_state, if optional OptionalFieldAccess else FieldAccess, start, field.end, [$left, leaf_node])
					$s = access_state
					$left = access_node
					$qualified = $qualified and field.kind == UpperIdent
				} else {
					$s = look
					$done = True
				}
			} else if bp.left > 0 and bp.left >= minimum {
				start = node_start(look, $left)
				{ state: right_state, node: right_node } = expression(at_pos(look, look.pos + 1), bp.right, depth + 1, False)
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
			{ state: item_state, node: item_node } = expression(look, 0, depth, False)
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

primary : State, U64, Bool -> Built
primary = |s, depth, punned| {
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
		Number => literal_suffix(add_node(at_pos(s, s.pos + 1), NumberLiteral, t.start, t.end, []), False)
		SingleQuote => literal_suffix(add_node(at_pos(s, s.pos + 1), CharacterLiteral, t.start, t.end, []), False)
		StringStart | MultilineStringStart => parse_string(s, depth + 1)
		OpenCurly => curly(at_pos(s, s.pos + 1), t.start, depth + 1, punned)
		Keyword if spelling(s, "dbg") or spelling(s, "crash") or spelling(s, "return") => {
			kind = if spelling(s, "dbg") Debug else if spelling(s, "crash") Crash else Return
			{ state: value_state, node: value_node } = expression(at_pos(s, s.pos + 1), 0, depth + 1, False)
			end = node_end(value_state, value_node)
			add_node(value_state, kind, t.start, end, [value_node])
		}
		Keyword if spelling(s, "break") => add_node(at_pos(s, s.pos + 1), Break, t.start, t.end, [])
		Punctuation if spelling(s, "...") => add_node(at_pos(s, s.pos + 1), Ellipsis, t.start, t.end, [])
		Keyword if spelling(s, "if") => parse_if(at_pos(s, s.pos + 1), t.start, depth + 1)
		Keyword if spelling(s, "for") or spelling(s, "for!") => parse_for(at_pos(s, s.pos + 1), t.start, spelling(s, "for!"), depth + 1)
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
			{ state: operand_state, node: operand_node } = expression(at_pos(s, s.pos + 1), 40, depth + 1, False)
			end = node_end(operand_state, operand_node)
			add_node(operand_state, Unary, t.start, end, [operand_node])
		}
		EndOfFile | InterpolationEnd | CloseRound | CloseSquare | CloseCurly => {
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

ParseMode : [FileMode, ExpressionMode, StatementMode, HeaderMode]

parse_source : List(U8), ParseMode -> RocSyntax.Parsed
parse_source = |source, mode| {
	expression_only = mode == ExpressionMode
	single = mode != FileMode
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
				{ state: built_state, node: built_node } = if expression_only expression(look, 0, 0, False) else statement(look, if mode == StatementMode 1 else 0)
				$roots = $roots.append(built_node)
				$s = built_state
				if mode == FileMode or mode == HeaderMode {
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
				if mode == HeaderMode and ($s.nodes.get(built_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind != Header {
					$s = issue($s, UnexpectedToken, t.start, node_end($s, built_node))
				}
				if single {
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
	if mode == HeaderMode and $roots.is_empty() {
		{ state: header_state, node: header_node } = add_node($s, Header, 0, 0, [])
		$s = header_state
		$roots = [header_node]
	} else if single and $roots.is_empty() {
		$s = issue($s, UnexpectedToken, source.len(), source.len())
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
		var $prefix_at = start + 1
		while byte(source, $prefix_at) == '_' and $prefix_at < source.len() {
			$prefix_at = $prefix_at + 1
		}
		prefix = byte(source, $prefix_at)
		if prefix == 'x' or prefix == 'o' or prefix == 'b' {
			$base = if prefix == 'x' 16 else if prefix == 'o' 8 else 2
			$i = $prefix_at + 1
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
		var $decimal = False
		if byte(source, $i) == '.' and (is_digit(byte(source, $i + 1)) or byte(source, $i + 1) == 'e' or byte(source, $i + 1) == 'E') {
			$decimal = True
			$i = $i + 1
			while $i < source.len() and (is_digit(byte(source, $i)) or byte(source, $i) == '_') {
				$i = $i + 1
			}
		}
		if (byte(source, $i) == 'e' or byte(source, $i) == 'E') and (byte(source, start) != '0' or $decimal) {
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
	if $i < source.len() and is_ident(byte(source, $i)) and byte(source, $i) != '!' {
		$valid = False
		while $i < source.len() and is_ident(byte(source, $i)) and byte(source, $i) != '!' {
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
			if $value <= 0x10FFFF and digit < 16 {
				$value = $value * 16 + digit
			} else {
				$valid = False
			}
			$digits = $digits + 1
			$i = $i + 1
		}
		closed = byte(source, $i) == ')'
		{ end: $i + if closed 1 else 0, valid: $valid and closed and $digits > 0 and $value <= 0x10FFFF and !($value >= 0xD800 and $value <= 0xDFFF) }
	} else {
		{ end: if start + 1 < source.len() and b != '\n' start + 2 else start + 1, valid: b == 'n' or b == 'r' or b == 't' or b == '\\' or b == '"' or b == '\'' or b == '$' }
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
		{ state: name_state, node: name_node } = parse_pattern(s, False, depth + 1)
		after_name = skip_trivia(name_state)
		{ state: value_state, node: value_node } = expression(at_pos(after_name, after_name.pos + 1), 0, depth, False)
		end = node_end(value_state, value_node)
		add_node(value_state, Declaration, t.start, end, [name_node, value_node])
	} else if t.kind == LowerIdent and token_spelling(s.source, following, ":") {
		{ state: name_state, node: name_node } = add_node(at_pos(s, following_pos), Identifier, t.start, t.end, [])
		{ state: ty_state, node: ty_node } = type_expression(at_pos(name_state, name_state.pos + 1), depth)
		end = node_end(ty_state, ty_node)
		add_node(ty_state, TypeAnnotation, t.start, end, [name_node, ty_node])
	} else if spelling(s, "var") {
		name_state = skip_trivia(at_pos(s, s.pos + 1))
		name = peek(name_state)
		validated = if depth == 0 or name.kind != LowerIdent or !(is_lower(byte(name_state.source, name.start)) or byte(name_state.source, name.start) == '$') issue(name_state, UnexpectedToken, name.start, name.end) else name_state
		{ state: named_state, node: named_node } = add_node(at_pos(validated, validated.pos + if name.kind == EndOfFile 0 else 1), Identifier, name.start, name.end, [])
		look = skip_trivia(named_state)
		if spelling(look, "=") {
			{ state: value_state, node: value_node } = expression(at_pos(look, look.pos + 1), 0, depth + 1, False)
			end = node_end(value_state, value_node)
			add_node(value_state, VariableDeclaration, t.start, end, [named_node, value_node])
		} else if spelling(look, ":") {
			{ state: ty_state, node: ty_node } = type_expression(at_pos(look, look.pos + 1), depth + 1)
			end = node_end(ty_state, ty_node)
			add_node(ty_state, TypeAnnotation, t.start, end, [named_node, ty_node])
		} else {
			add_node(look, VariableDeclaration, t.start, name.end, [named_node])
		}
	} else if spelling(s, "expect") {
		{ state: value_state, node: value_node } = expression(at_pos(s, s.pos + 1), 0, depth + 1, False)
		end = node_end(value_state, value_node)
		add_node(value_state, Expect, t.start, end, [value_node])
	} else if spelling(s, "while") {
		{ state: condition_state, node: condition_node } = expression(at_pos(s, s.pos + 1), 0, depth + 1, False)
		{ state: body_state, node: body_node } = expression(condition_state, 0, depth + 1, False)
		end = node_end(body_state, body_node)
		add_node(body_state, While, t.start, end, [condition_node, body_node])
	} else if spelling(s, "for") or spelling(s, "for!") or spelling(s, "dbg") or spelling(s, "crash") or spelling(s, "return") or spelling(s, "break") {
		primary(s, depth + 1, False)
	} else if (t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenCurly or t.kind == UpperIdent) and has_pattern_assignment(s) {
		{ state: pattern_state, node: pattern_node } = parse_pattern(s, False, depth + 1)
		look = skip_trivia(pattern_state)
		validated = if spelling(look, "=") look else issue(look, UnexpectedToken, peek(look).start, peek(look).end)
		{ state: value_state, node: value_node } = expression(at_pos(validated, validated.pos + if spelling(validated, "=") 1 else 0), 0, depth + 1, False)
		end = node_end(value_state, value_node)
		add_node(value_state, Declaration, t.start, end, [pattern_node, value_node])
	} else if spelling(s, "app") or spelling(s, "package") {
		{ state: exposed_state, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(at_pos(s, s.pos + 1))
		look = skip_trivia(exposed_state)
		if peek(look).kind == OpenCurly {
			{ state: deps_state, node: deps_node } = dependency_record(look, spelling(s, "app"), depth + 1)
			end = node_end(deps_state, deps_node)
			validated = if depth == 0 deps_state else issue(deps_state, UnexpectedToken, t.start, end)
			add_node(validated, Header, t.start, end, exposed_ids.append(deps_node))
		} else {
			validated = if depth == 0 and spelling(s, "package") look else issue(look, UnexpectedToken, peek(look).start, peek(look).end)
			add_node(validated, Header, t.start, exposed_end, exposed_ids)
		}
	} else if spelling(s, "module") or spelling(s, "hosted") {
		{ state: items_state, ids: items_ids, end: items_end, comma: _ } = exposed_collection(at_pos(s, s.pos + 1))
		validated = if depth == 0 items_state else issue(items_state, UnexpectedToken, t.start, items_end)
		add_node(validated, Header, t.start, items_end, items_ids)
	} else if spelling(s, "import") {
		validated = if depth == 0 s else issue(s, UnexpectedToken, t.start, t.end)
		parse_import(validated, depth + 1)
	} else {
		{ state: value_state, node: value_node } = expression(s, 0, depth, True)
		end = node_end(value_state, value_node)
		add_node(value_state, ExpressionStatement, t.start, end, [value_node])
	}
}

exposed_collection : State -> Collection
exposed_collection = |initial| {
	look = skip_trivia(initial)
	opening = peek(look)
	var $s = if opening.kind == OpenSquare at_pos(look, look.pos + 1) else issue(look, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenSquare
	while !$done {
		item_state = skip_trivia($s)
		$s = vacant
		t = peek(item_state)
		if t.kind == CloseSquare {
			$s = at_pos(item_state, item_state.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseCurly or t.kind == CloseRound {
			$s = issue(item_state, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			valid_start = t.kind == UpperIdent or (t.kind == LowerIdent and is_lower(byte(item_state.source, t.start)))
			validated = if valid_start item_state else issue(item_state, UnexpectedToken, t.start, t.end)
			{ state: name_state, node: name_node } = add_node(at_pos(validated, validated.pos + 1), Identifier, t.start, t.end, [])
			var $name = name_node
			$s = skip_trivia(name_state)
			var $upper = t.kind == UpperIdent
			var $qualified = t.kind == UpperIdent
			var $chain_done = False
			while !$chain_done {
				if $qualified and spelling($s, ".") and (peek(at_pos($s, $s.pos + 1)).kind == UpperIdent or peek(at_pos($s, $s.pos + 1)).kind == LowerIdent) {
					field_state = at_pos($s, $s.pos + 1)
					field = peek(field_state)
					{ state: named_state, node: named_node } = add_node(at_pos(field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
					{ state: access_state, node: access_node } = add_node(named_state, FieldAccess, t.start, field.end, [$name, named_node])
					$s = skip_trivia(access_state)
					$name = access_node
					$upper = field.kind == UpperIdent
					$qualified = $upper
				} else {
					$chain_done = True
				}
			}
			var $children = [$name]
			var $wildcard = False
			$end = node_end($s, $name)
			if spelling($s, "as") {
				alias_state = skip_trivia(at_pos($s, $s.pos + 1))
				alias = peek(alias_state)
				valid_alias = if $upper alias.kind == UpperIdent else if t.kind == UpperIdent alias.kind == LowerIdent or alias.kind == UpperIdent else alias.kind == LowerIdent
				checked = if valid_alias alias_state else issue(alias_state, UnexpectedToken, alias.start, alias.end)
				{ state: alias_built, node: alias_node } = add_node(at_pos(checked, checked.pos + if alias.kind == EndOfFile 0 else 1), Identifier, alias.start, alias.end, [])
				$s = alias_built
				$children = $children.append(alias_node)
				$end = alias.end
			} else if $upper and spelling($s, ".*") {
				$end = peek($s).end
				$s = at_pos($s, $s.pos + 1)
				$wildcard = True
			}
			{ state: exposed_state, node: exposed_node } = add_node($s, if $wildcard ExposedWildcard else ExposedItem, t.start, $end, $children)
			$ids = $ids.append(exposed_node)
			$s = skip_trivia(exposed_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			} else if peek($s).kind != CloseSquare {
				$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: False }
}

parse_import : State, U64 -> Built
parse_import = |initial, depth| {
	start = peek(initial).start
	look = skip_trivia(at_pos(initial, initial.pos + 1))
	if peek(look).kind == StringStart {
		{ state: path_state, node: path_node } = parse_string(look, depth + 1)
		after_path = skip_trivia(path_state)
		validated = if spelling(after_path, "as") at_pos(after_path, after_path.pos + 1) else issue(after_path, UnexpectedToken, peek(after_path).start, peek(after_path).end)
		name_state = skip_trivia(validated)
		name = peek(name_state)
		checked = if name.kind == LowerIdent and !spelling(name_state, "_") name_state else issue(name_state, UnexpectedToken, name.start, name.end)
		{ state: named_state, node: named_node } = add_node(at_pos(checked, checked.pos + if name.kind == EndOfFile 0 else 1), Identifier, name.start, name.end, [])
		colon = skip_trivia(named_state)
		ty_start = if spelling(colon, ":") at_pos(colon, colon.pos + 1) else issue(colon, UnexpectedToken, peek(colon).start, peek(colon).end)
		{ state: ty_state, node: ty_node } = file_import_type(ty_start)
		end = node_end(ty_state, ty_node)
		path_info = ty_state.nodes.get(path_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
		path_child = ty_state.children.get(path_info.children_start) ?? ty_state.nodes.len()
		part_info = ty_state.nodes.get(path_child) ?? path_info
		plain_path = path_info.kind == StringLiteral and path_info.children_len == 1 and part_info.kind == StringSegment
		checked_type = if plain_path ty_state else issue(ty_state, UnexpectedToken, start, end)
		add_node(checked_type, FileImport, start, end, [path_node, named_node, ty_node])
	} else {
		var $s = look
		path_start = peek(look).start
		var $end = path_start
		var $local = True
		if spelling($s, "/") {
			$s = skip_trivia(at_pos($s, $s.pos + 1))
		} else if spelling($s, ".") and token_spelling($s.source, source_token($s, significant_position($s.tokens, $s.pos + 1)), "/") {
			$s = skip_trivia(at_pos($s, significant_position($s.tokens, $s.pos + 1) + 1))
		} else if spelling($s, "..") {
			while spelling($s, "..") {
				parent = skip_trivia(at_pos($s, $s.pos + 1))
				if spelling(parent, "/") {
					$s = skip_trivia(at_pos(parent, parent.pos + 1))
				} else {
					$s = issue(parent, UnexpectedToken, peek(parent).start, peek(parent).end)
				}
			}
		} else if peek($s).kind == LowerIdent {
			next = skip_trivia(at_pos($s, $s.pos + 1))
			$local = spelling(next, "/")
		}
		first = peek($s)
		var $upper = first.kind == UpperIdent
		if first.kind != UpperIdent and first.kind != LowerIdent {
			$s = issue($s, UnexpectedToken, first.start, first.end)
		} else {
			$end = first.end
			$s = skip_trivia(at_pos($s, $s.pos + 1))
		}
		if $local {
			while spelling($s, "/") {
				$s = skip_trivia(at_pos($s, $s.pos + 1))
				part = peek($s)
				if part.kind == LowerIdent or part.kind == UpperIdent {
					$upper = part.kind == UpperIdent
					$end = part.end
					$s = skip_trivia(at_pos($s, $s.pos + 1))
				} else {
					$s = issue($s, UnexpectedToken, part.start, part.end)
				}
			}
			if !$upper {
				$s = issue($s, UnexpectedToken, first.start, $end)
			}
		}
		var $nested = 0.U64
		while spelling($s, ".") and peek(at_pos($s, $s.pos + 1)).kind == UpperIdent {
			field_state = at_pos($s, $s.pos + 1)
			$end = peek(field_state).end
			$s = skip_trivia(at_pos(field_state, field_state.pos + 1))
			$nested = $nested + 1
		}
		if !$local and $nested == 0 {
			$s = issue($s, UnexpectedToken, first.start, $end)
		}
		{ state: path_state, node: path_node } = add_node($s, ImportPath, path_start, $end, [])
		$s = path_state
		var $ids = [path_node]
		if spelling($s, "as") {
			alias_state = skip_trivia(at_pos($s, $s.pos + 1))
			alias = peek(alias_state)
			checked = if alias.kind == UpperIdent alias_state else issue(alias_state, UnexpectedToken, alias.start, alias.end)
			{ state: named_state, node: named_node } = add_node(at_pos(checked, checked.pos + if alias.kind == EndOfFile 0 else 1), Identifier, alias.start, alias.end, [])
			{ state: alias_built, node: alias_node } = add_node(named_state, ImportAlias, alias.start, alias.end, [named_node])
			$s = skip_trivia(alias_built)
			$ids = $ids.append(alias_node)
			$end = alias.end
		}
		if spelling($s, "exposing") {
			{ state: exposed_state, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(at_pos($s, $s.pos + 1))
			$s = exposed_state
			$ids = $ids.concat(exposed_ids)
			$end = exposed_end
		}
		add_node($s, Import, start, $end, $ids)
	}
}

type_expression : State, U64 -> Built
type_expression = |initial, depth| {
	if depth >= 128 {
		t = peek(initial)
		bad = issue(initial, ResourceLimit, t.start, t.end)
		add_node(bad, ErrorNode, t.start, t.start, [])
	} else {
		{ state: left_state, node: left_node } = expression(initial, 40, depth + 1, False)
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

# Patterns have their own grammar: expression operators and value-only postfixes
# must never make an invalid binding pattern appear complete.
parse_pattern : State, Bool, U64 -> Built
parse_pattern = |initial, alternatives, depth| {
	s = skip_trivia(initial)
	t = peek(s)
	if depth >= 128 {
		bad = issue(s, ResourceLimit, t.start, t.end)
		add_node(bad, ErrorNode, t.start, t.start, [])
	} else {
		{ state: first_state, node: first_node } = pattern_primary(s, alternatives, depth + 1)
		var $s = skip_trivia(first_state)
		var $node = first_node
		if alternatives and spelling($s, "|") {
			var $ids = [first_node]
			while spelling($s, "|") {
				{ state: next_state, node: next_node } = pattern_primary(skip_trivia(at_pos($s, $s.pos + 1)), alternatives, depth + 1)
				$ids = $ids.append(next_node)
				$s = skip_trivia(next_state)
			}
			end = node_end($s, $ids.last() ?? first_node)
			{ state: alt_state, node: alt_node } = add_node($s, AlternativePattern, t.start, end, $ids)
			$s = alt_state
			$node = alt_node
		} else if spelling($s, "as") {
			name_state = skip_trivia(at_pos($s, $s.pos + 1))
			name = peek(name_state)
			if name.kind == LowerIdent and (is_lower(byte(name_state.source, name.start)) or byte(name_state.source, name.start) == '$') {
				{ state: named_state, node: named_node } = add_node(at_pos(name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
				{ state: alias_state, node: alias_node } = add_node(named_state, AliasPattern, t.start, name.end, [first_node, named_node])
				$s = alias_state
				$node = alias_node
			} else {
				$s = issue(name_state, UnexpectedToken, name.start, name.end)
			}
		}
		{ state: $s, node: $node }
	}
}

pattern_primary : State, Bool, U64 -> Built
pattern_primary = |s, alternatives, depth| {
	t = peek(s)
	if t.kind == LowerIdent {
		add_node(at_pos(s, s.pos + 1), if spelling(s, "_") WildcardPattern else IdentifierPattern, t.start, t.end, [])
	} else if spelling(s, "var") {
		look = skip_trivia(at_pos(s, s.pos + 1))
		name = peek(look)
		if name.kind == LowerIdent and (is_lower(byte(look.source, name.start)) or byte(look.source, name.start) == '$') {
			{ state: name_state, node: name_node } = add_node(at_pos(look, look.pos + 1), Identifier, name.start, name.end, [])
			add_node(name_state, MutablePattern, t.start, name.end, [name_node])
		} else {
			bad = issue(look, UnexpectedToken, name.start, name.end)
			add_node(bad, ErrorNode, t.start, name.start, [])
		}
	} else if spelling(s, "..") {
		look = skip_trivia(at_pos(s, s.pos + 1))
		if spelling(look, "as") {
			name_state = skip_trivia(at_pos(look, look.pos + 1))
			name = peek(name_state)
			if name.kind == LowerIdent and !spelling(name_state, "_") {
				{ state: named_state, node: named_node } = add_node(at_pos(name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
				add_node(named_state, RestPattern, t.start, name.end, [named_node])
			} else {
				bad = issue(name_state, UnexpectedToken, name.start, name.end)
				add_node(bad, RestPattern, t.start, t.end, [])
			}
		} else {
			validated = if peek(look).kind == LowerIdent issue(look, UnexpectedToken, peek(look).start, peek(look).end) else look
			add_node(validated, RestPattern, t.start, t.end, [])
		}
	} else if t.kind == UpperIdent {
		{ state: name_state, node: name_node } = add_node(at_pos(s, s.pos + 1), Identifier, t.start, t.end, [])
		var $s = name_state
		var $name = name_node
		var $done = False
		while !$done {
			look = skip_trivia($s)
			if spelling(look, ".") and peek(at_pos(look, look.pos + 1)).kind == UpperIdent {
				field_state = at_pos(look, look.pos + 1)
				field = peek(field_state)
				{ state: field_built, node: field_node } = add_node(at_pos(field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
				{ state: access_state, node: access_node } = add_node(field_built, FieldAccess, t.start, field.end, [$name, field_node])
				$s = access_state
				$name = access_node
			} else {
				$s = look
				$done = True
			}
		}
		var $nominal = False
		if spelling($s, ".") {
			$nominal = True
			$s = skip_trivia(at_pos($s, $s.pos + 1))
		}
		if peek($s).kind == NoSpaceOpenRound {
			{ state: args_state, ids: args, end, comma: _ } = pattern_collection(at_pos($s, $s.pos + 1), CloseRound, True, depth)
			add_node(args_state, if $nominal NominalPattern else TagPattern, t.start, end, [$name].concat(args))
		} else if $nominal and peek($s).kind == OpenCurly {
			{ state: record_state, node: record_node } = pattern_record($s, alternatives, depth)
			end = node_end(record_state, record_node)
			add_node(record_state, NominalPattern, t.start, end, [$name, record_node])
		} else {
			validated = if $nominal issue($s, UnexpectedToken, peek($s).start, peek($s).end) else $s
			add_node(validated, TagPattern, t.start, node_end(validated, $name), [$name])
		}
	} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare {
		closing = if t.kind == OpenSquare CloseSquare else CloseRound
		{ state: items_state, ids, end, comma: _ } = pattern_collection(at_pos(s, s.pos + 1), closing, True, depth)
		add_node(items_state, if t.kind == OpenSquare ListPattern else TuplePattern, t.start, end, ids)
	} else if t.kind == OpenCurly {
		pattern_record(s, alternatives, depth)
	} else if t.kind == StringStart {
		pattern_string(s)
	} else if t.kind == Number or t.kind == SingleQuote {
		primary(s, depth, False)
	} else {
		bad = issue(s, UnexpectedToken, t.start, t.end)
		add_node(at_pos(bad, bad.pos + if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly 0 else 1), ErrorNode, t.start, t.end, [])
	}
}

pattern_collection : State, RocSyntax.TokenKind, Bool, U64 -> Collection
pattern_collection = |initial, closing, alternatives, depth| {
	var $s = initial
	var $ids = []
	var $end = peek(initial).start
	var $done = False
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
			{ state: item_state, node: item_node } = parse_pattern(look, alternatives, depth + 1)
			$ids = $ids.append(item_node)
			$end = node_end(item_state, item_node)
			$s = skip_trivia(item_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			} else if peek($s).kind != closing and peek($s).kind != EndOfFile {
				$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
			}
			if $s.pos <= look.pos {
				$s = at_pos($s, look.pos + 1)
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: False }
}

pattern_record : State, Bool, U64 -> Built
pattern_record = |initial, alternatives, depth| {
	start = peek(initial).start
	var $s = at_pos(initial, initial.pos + 1)
	var $ids = []
	var $end = peek(initial).end
	var $done = False
	while !$done {
		look = skip_trivia($s)
		$s = vacant
		t = peek(look)
		if t.kind == CloseCurly {
			$s = at_pos(look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$s = issue(look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			{ state: field_state, node: field_node } = if spelling(look, "..") {
				name_state = skip_trivia(at_pos(look, look.pos + 1))
				name = peek(name_state)
				if name.kind == LowerIdent and (is_lower(byte(name_state.source, name.start)) or byte(name_state.source, name.start) == '$') {
					{ state: named_state, node: named_node } = add_node(at_pos(name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
					add_node(named_state, RestPattern, t.start, name.end, [named_node])
				} else {
					add_node(name_state, RestPattern, t.start, t.end, [])
				}
			} else if t.kind == LowerIdent and is_lower(byte(look.source, t.start)) {
				{ state: name_state, node: name_node } = add_node(at_pos(look, look.pos + 1), Identifier, t.start, t.end, [])
				after = skip_trivia(name_state)
				if spelling(after, ":") {
					{ state: value_state, node: value_node } = parse_pattern(at_pos(after, after.pos + 1), alternatives, depth + 1)
					end = node_end(value_state, value_node)
					add_node(value_state, PatternField, t.start, end, [name_node, value_node])
				} else {
					validated = if spelling(after, ",") or peek(after).kind == CloseCurly after else issue(after, UnexpectedToken, peek(after).start, peek(after).end)
					add_node(validated, PatternField, t.start, t.end, [name_node])
				}
			} else {
				bad = issue(at_pos(look, look.pos + 1), UnexpectedToken, t.start, t.end)
				add_node(bad, ErrorNode, t.start, t.end, [])
			}
			$ids = $ids.append(field_node)
			$end = node_end(field_state, field_node)
			$s = skip_trivia(field_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			}
			if $s.pos <= look.pos {
				$s = at_pos($s, look.pos + 1)
			}
		}
	}
	add_node($s, RecordPattern, start, $end, $ids)
}

parse_lambda : State, U64, U64 -> Built
parse_lambda = |initial, start, depth| {
	var $s = initial
	var $ids = []
	var $done = False
	while !$done {
		look = skip_trivia($s)
		$s = vacant
		t = peek(look)
		if spelling(look, "|") {
			$s = at_pos(look, look.pos + 1)
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(look, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = look.pos
			{ state: param_state, node: param_node } = parse_pattern(look, False, depth + 1)
			$ids = $ids.append(param_node)
			$s = skip_trivia(param_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			}
			if $s.pos <= before {
				$s = issue(at_pos($s, before + 1), UnexpectedToken, t.start, t.end)
			}
		}
	}
	{ state: body_state, node: body_node } = expression($s, 0, depth, False)
	end = node_end(body_state, body_node)
	add_node(body_state, Lambda, start, end, $ids.append(body_node))
}

parse_if : State, U64, U64 -> Built
parse_if = |initial, start, depth| {
	{ state: condition_state, node: condition_node } = expression(initial, 0, depth, False)
	look = skip_trivia(condition_state)
	{ state: then_branch_state, node: then_branch_node } = expression(look, 0, depth, False)
	after = skip_trivia(then_branch_state)
	if spelling(after, "else") {
		{ state: other_state, node: other_node } = expression(at_pos(after, after.pos + 1), 0, depth, False)
		end = node_end(other_state, other_node)
		add_node(other_state, If, start, end, [condition_node, then_branch_node, other_node])
	} else {
		end = node_end(after, then_branch_node)
		add_node(after, If, start, end, [condition_node, then_branch_node])
	}
}

curly : State, U64, U64, Bool -> Built
curly = |initial, start, depth, punned| {
	look = skip_trivia(initial)
	first = peek(look)
	after = skip_trivia(at_pos(look, look.pos + 1))
	record = first.kind == CloseCurly or (first.kind == LowerIdent and (spelling(after, ":") or spelling(after, ",") or (punned and peek(after).kind == CloseCurly)))
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
					{ state: value_state, node: value_node } = expression(at_pos(field_look, field_look.pos + 1), 0, depth, True)
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
	{ state: value_state, node: value_node } = expression(initial, 0, depth, False)
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
				{ state: pattern_state, node: pattern_node } = parse_pattern($s, True, depth + 1)
				var $arrow = skip_trivia(pattern_state)
				var $branch_ids = [pattern_node]
				if spelling($arrow, "if") {
					{ state: guard_state, node: guard_node } = expression(at_pos($arrow, $arrow.pos + 1), 0, depth + 1, False)
					$arrow = skip_trivia(guard_state)
					$branch_ids = $branch_ids.append(guard_node)
				}
				arrow = $arrow
				if spelling(arrow, "=>") {
					{ state: branch_value_state, node: branch_value_node } = expression(at_pos(arrow, arrow.pos + 1), 0, depth, False)
					branch_end = node_end(branch_value_state, branch_value_node)
					{ state: branch_state, node: branch_node } = add_node(branch_value_state, MatchBranch, t.start, branch_end, $branch_ids.append(branch_value_node))
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

expect {
	["dbg 1 + 2", "crash \"failed\"", "return f(1)", "break", "...", "value.?field", "value.?field.other?"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["dbg", "crash", "return", "value.?0", "value.?Upper", "value.? field", "value.?field(1)"].all(|source| RocSyntax.parse_expr_str(source).status == Recovered)
}

# A direct optional-field call is forbidden. A following required field with
# arguments is a method call and starts a separate postfix operation.
is_optional_access : State, U64 -> Bool
is_optional_access = |s, root| {
	match s.nodes.get(root) {
		Ok(n) => n.kind == OptionalFieldAccess
		_ => False
	}
}

expect {
	["|_| 1", "|Some(x), (a, b), {name}, [first, .. as rest]| x", "|var count| count", "|| {}", "match x { _ => 0 }", "match x { Some(a) | Other(a) => a, None => 0 }", "match x { {a: Some(v), ..rest} => v }", "match x { [first, .. as rest] if first > 0 => rest, _ => [] }", "|Thing.(value)| value", "|Thing.{field}| field", "|(a, _) as pair| pair"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["|1 + 2| 0", "match x { 1 + 2 => 0 }", "match x { [a b] => 0 }", "|var _| 0", "match x { [.. old] => 0 }", "match x { _ as Upper => 0 }"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

# Only inspect potential destructuring starts; ordinary definitions take the
# constant-lookahead path above. Delimiters shield nested assignments.
has_pattern_assignment : State -> Bool
has_pattern_assignment = |s| {
	var $pos = s.pos
	var $nesting = 0.U64
	var $found = False
	var $done = False
	while !$done {
		t = source_token(s, $pos)
		if t.kind == EndOfFile {
			$done = True
		} else if $nesting == 0 and token_spelling(s.source, t, "=") {
			$found = True
			$done = True
		} else if $nesting == 0 and ((t.kind == Whitespace and line_break(s.source, t.start, t.end)) or t.kind == CloseCurly) {
			$done = True
		} else {
			if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare or t.kind == OpenCurly {
				$nesting = $nesting + 1
			}
			if t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
				if $nesting > 0 {
					$nesting = $nesting - 1
				} else {
					$done = True
				}
			}
			$pos = $pos + 1
		}
	}
	$found
}

parse_for : State, U64, Bool, U64 -> Built
parse_for = |initial, start, stream, depth| {
	{ state: pattern_state, node: pattern_node } = parse_pattern(initial, False, depth + 1)
	look = skip_trivia(pattern_state)
	validated = if spelling(look, "in") at_pos(look, look.pos + 1) else issue(look, UnexpectedToken, peek(look).start, peek(look).end)
	{ state: list_state, node: list_node } = expression(validated, 0, depth + 1, False)
	{ state: body_state, node: body_node } = expression(list_state, 0, depth + 1, False)
	end = node_end(body_state, body_node)
	add_node(body_state, if stream ForStream else For, start, end, [pattern_node, list_node, body_node])
}

expect {
	["(a, b) = pair", "{name, count} = value", "Some(x) = result", "expect 1 == 1", "dbg 1", "for x in items { dbg x }", "while True { break }", "main = { var count = 0\n count = count + 1\n expect count > 0\n count }"].all(|source| RocSyntax.parse_str(source).status == Complete)
}

expect {
	RocSyntax.parse_str("var count = 0").status == Recovered and RocSyntax.parse_expr_str("for (a, b) in pairs { a + b }").status == Complete and RocSyntax.parse_expr_str("for! item in stream { dbg item }").status == Complete
}

expect {
	# Current compiler line-prefix strings and nested interpolation lex losslessly.
	["\"hello \${name}!\"", "\"\${{ a: 1 }}\"", "\"\${\"inner \${x}\"}\"", "\\\\hello\n\\\\world", "\"\"\"hello\n\"\"\"world", "\"\"\"hello\"\"\"", "0_xFF", "0__b10", "\"\\u(0000000000000000000000000041)\"", "'\\u(00000041)'", "$Type", "_a", "@Foo"].all(
		|source| {
			lexed = RocSyntax.tokenize_str(source)
			lexed.status == Complete and lexed.tokens.drop_last(1).all(|t| t.start < t.end) and lexed.tokens.fold(0.U64, |end, t| if t.start == end t.end else 18446744073709551615) == source.count_utf8_bytes()
		},
	)
}

expect {
	["0_1", "0_0", "0e3", "0e+3", "'\r'", "'\u(1)'", "\"a\rb\"", "\"\${x", "\"\${x}", "$1"].all(|source| RocSyntax.tokenize_str(source).status == Recovered)
}

expect {
	lexed = RocSyntax.tokenize_str("\"\${{ a: 1 }}\"")
	lexed.tokens.map(|t| t.kind) == [StringStart, InterpolationStart, OpenCurly, Whitespace, LowerIdent, Punctuation, Whitespace, Number, Whitespace, CloseCurly, InterpolationEnd, StringEnd, EndOfFile]
}

expect {
	lexed = RocSyntax.tokenize_str("\\\\a\${b}c\n\\\\d")
	lexed.tokens.map(|t| t.kind) == [MultilineStringStart, StringPart, InterpolationStart, LowerIdent, InterpolationEnd, StringPart, Whitespace, MultilineStringStart, StringPart, EndOfFile]
}

expect {
	RocSyntax.tokenize_str("_ _name @Name $Upper $lower __").tokens.map(|t| t.kind) == [LowerIdent, Whitespace, LowerIdent, Whitespace, UpperIdent, Whitespace, UpperIdent, Whitespace, LowerIdent, Whitespace, LowerIdent, LowerIdent, EndOfFile]
}

expect {
	RocSyntax.tokenize_str("0_.1e3").status == Complete and RocSyntax.tokenize_str("a=(0\r.e)\n").status == Recovered and RocSyntax.tokenize_str("a=0\r\n").status == Complete
}

parse_string : State, U64 -> Built
parse_string = |initial, depth| {
	first = peek(initial)
	multiline = first.kind == MultilineStringStart
	var $s = at_pos(initial, initial.pos + 1)
	var $ids = []
	var $end = first.end
	var $done = False
	while !$done {
		look = if multiline skip_trivia($s) else $s
		$s = vacant
		t = peek(look)
		if t.kind == StringPart {
			{ state: part_state, node: part_node } = add_node(at_pos(look, look.pos + 1), StringSegment, t.start, t.end, [])
			$s = part_state
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			{ state: value_state, node: value_node } = expression(at_pos(look, look.pos + 1), 0, depth + 1, False)
			after = skip_trivia(value_state)
			close = peek(after)
			closed = close.kind == InterpolationEnd
			validated = if closed at_pos(after, after.pos + 1) else issue(after, UnclosedDelimiter, close.start, close.end)
			end = if closed close.end else node_end(validated, value_node)
			{ state: interpolation_state, node: interpolation_node } = add_node(validated, Interpolation, t.start, end, [value_node])
			$s = interpolation_state
			$ids = $ids.append(interpolation_node)
			$end = end
			if !closed {
				$done = True
			}
		} else if multiline and t.kind == MultilineStringStart {
			$s = at_pos(look, look.pos + 1)
			$end = t.end
		} else if !multiline and t.kind == StringEnd {
			$s = at_pos(look, look.pos + 1)
			$end = t.end
			$done = True
		} else {
			$s = if multiline look else issue(look, UnclosedString, first.start, $end)
			$done = True
		}
	}
	literal_suffix(add_node($s, if multiline MultilineStringLiteral else StringLiteral, first.start, $end, $ids), multiline)
}

# A literal's adjacent .Upper chain names its type, rather than a value field.
literal_suffix : Built, Bool -> Built
literal_suffix = |built, allow_trivia| {
	var $s = built.state
	var $type_ids = []
	var $end = node_end($s, built.node)
	var $done = False
	while !$done {
		look = if allow_trivia and $type_ids.is_empty() skip_trivia($s) else $s
		t = peek(look)
		name_state = at_pos(look, look.pos + 1)
		name = peek(name_state)
		if spelling(look, ".") and (allow_trivia and $type_ids.is_empty() or t.start == $end) and name.kind == UpperIdent and is_upper(byte(name_state.source, name.start)) {
			{ state: named_state, node: named_node } = add_node(at_pos(name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
			$s = named_state
			$type_ids = $type_ids.append(named_node)
			$end = name.end
		} else {
			$done = True
		}
	}
	if $type_ids.is_empty() { state: $s, node: built.node } else {
		start = node_start($s, built.node)
		add_node($s, TypedLiteral, start, $end, [built.node].concat($type_ids))
	}
}

expect {
	["\"hello \${name}!\"", "\"\${{ a: 1 }}\"", "\"\${\"inner \${x}\"}\"", "\\\\hello\n\\\\world", "\"\"\"hello\n\"\"\"world", "42.U8", "1.0.F64", "'a'.U32", "\"hello\".MyText", "0xFF.Gui.Color"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["\"\${}\"", "\"\${x\"", "\"\${x}", "1 .U8"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

file_import_type : State -> Built
file_import_type = |initial| {
	s = skip_trivia(initial)
	t = peek(s)
	valid_name = spelling(s, "Str") or spelling(s, "List")
	checked = if valid_name s else issue(s, UnexpectedToken, t.start, t.end)
	{ state: name_state, node: name_node } = add_node(at_pos(checked, checked.pos + if t.kind == EndOfFile 0 else 1), Identifier, t.start, t.end, [])
	if spelling(s, "List") {
		look = skip_trivia(name_state)
		opening = peek(look)
		after = if opening.kind == OpenRound or opening.kind == NoSpaceOpenRound at_pos(look, look.pos + 1) else issue(look, UnexpectedToken, opening.start, opening.end)
		arg_state = skip_trivia(after)
		arg = peek(arg_state)
		validated = if spelling(arg_state, "U8") arg_state else issue(arg_state, UnexpectedToken, arg.start, arg.end)
		{ state: arg_built, node: arg_node } = add_node(at_pos(validated, validated.pos + if arg.kind == EndOfFile 0 else 1), Identifier, arg.start, arg.end, [])
		close_state = skip_trivia(arg_built)
		close = peek(close_state)
		closed = close.kind == CloseRound
		finished = if closed at_pos(close_state, close_state.pos + 1) else issue(close_state, UnclosedDelimiter, close.start, close.end)
		add_node(finished, TypeExpression, t.start, if closed close.end else arg.end, [name_node, arg_node])
	} else {
		{ state: name_state, node: name_node }
	}
}

expect {
	["import json.Json", "import json.Json as MyJson exposing [parse, Value]", "import http.Client.Auth as Auth exposing [get as fetch, Request.*]", "import ../Utils", "import ../../lib/Parser.Config", "import /root/Module", "import ./relative/Module", "import \"data.txt\" as text : Str", "import \"bytes.bin\" as data : List ( U8 )", "module [run as start, Foo.*, Foo.bar as Baz]", "hosted [main!]"].all(|source| RocSyntax.parse_str(source).status == Complete)
}

expect {
	["import lower", "import pkg.lower", "import json.Json as lower", "import Foo exposing [1]", "import Foo exposing [a b]", "import \"data\" as text : U64", "import \"data\" as text : List(U16)", "import \"\${name}\" as text : Str", "module [1 + 2]", "module [value as Type]"].all(|source| RocSyntax.parse_str(source).status != Complete)
}

expect {
	RocSyntax.parse_expr_str("{ x }").nodes.any(|n| n.kind == Block) and RocSyntax.parse_expr_str("{ { x } }").nodes.any(|n| n.kind == Record) and RocSyntax.parse_expr_str("{ field: { x } }").nodes.keep_if(|n| n.kind == Record).len() == 2
}

expect {
	RocSyntax.parse_expr_str("value.?field.other(1)").status == Complete and RocSyntax.parse_expr_str("value.?field.0(1)").status == Complete
}

expect {
	RocSyntax.parse_statement_str("var count = 0").status == Complete and RocSyntax.parse_statement_str("1 + 2").status == Complete and RocSyntax.parse_statement_str("x = 1\ny = 2").status == Recovered and RocSyntax.parse_header_str("module [run]").status == Complete and RocSyntax.parse_header_str("x = 1").status == Recovered and RocSyntax.parse_header_str("module []\nx = 1").status == Recovered
}

dependency_record : State, Bool, U64 -> Built
dependency_record = |initial, strict, depth| {
	start = peek(initial).start
	var $s = at_pos(initial, initial.pos + 1)
	var $ids = []
	var $end = peek(initial).end
	var $platform = False
	var $version = False
	var $done = False
	while !$done {
		look = skip_trivia($s)
		$s = vacant
		t = peek(look)
		if t.kind == CloseCurly {
			$end = t.end
			$s = at_pos(look, look.pos + 1)
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$s = issue(look, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = look.pos
			valid_name = t.kind == LowerIdent and !spelling(look, "_") and byte(look.source, t.start) != '_'
			checked = if valid_name look else issue(look, UnexpectedToken, t.start, t.end)
			{ state: name_state, node: name_node } = add_node(at_pos(checked, checked.pos + 1), Identifier, t.start, t.end, [])
			var $field_ids = [name_node]
			$s = skip_trivia(name_state)
			$end = t.end
			is_version = token_spelling($s.source, t, "roc")
			if spelling($s, ":") {
				value_start = skip_trivia(at_pos($s, $s.pos + 1))
				is_platform = spelling(value_start, "platform")
				value_state = if is_platform skip_trivia(at_pos(value_start, value_start.pos + 1)) else value_start
				value_token = peek(value_state)
				valid_start = if is_platform value_token.kind == StringStart or spelling(value_state, "glue") else !strict or value_token.kind == StringStart
				validated = if valid_start and (!is_platform or !$platform) value_state else issue(value_state, UnexpectedToken, value_token.start, value_token.end)
				{ state: parsed_state, node: parsed_node } = expression(validated, 0, depth + 1, False)
				var $value_state = parsed_state
				var $value_node = parsed_node
				$end = node_end(parsed_state, parsed_node)
				if is_platform {
					{ state: platform_state, node: platform_node } = add_node(parsed_state, PlatformReference, peek(value_start).start, $end, [parsed_node])
					$value_state = platform_state
					$value_node = platform_node
					$platform = True
				}
				$s = $value_state
				$field_ids = $field_ids.append($value_node)
				if is_version {
					n = $s.nodes.get(parsed_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
					part_id = $s.children.get(n.children_start) ?? $s.nodes.len()
					part = $s.nodes.get(part_id) ?? n
					version_text = Str.from_utf8_lossy($s.source.sublist({ start: part.start, len: part.end - part.start }))
					if $version or is_platform or n.kind != StringLiteral or n.children_len != 1 or part.kind != StringSegment or !valid_roc_version(version_text) {
						$s = issue($s, UnexpectedToken, t.start, $end)
					}
				}
			} else if strict or is_version {
				$s = issue($s, UnexpectedToken, t.start, $end)
			}
			$version = $version or is_version
			{ state: field_state, node: field_node } = add_node($s, DependencyField, t.start, $end, $field_ids)
			$ids = $ids.append(field_node)
			$s = skip_trivia(field_state)
			if spelling($s, ",") {
				$s = at_pos($s, $s.pos + 1)
			} else if peek($s).kind != CloseCurly {
				$s = issue($s, UnexpectedToken, peek($s).start, peek($s).end)
			}
			if $s.pos <= before {
				$s = at_pos($s, before + 1)
			}
		}
	}
	add_node($s, Dependencies, start, $end, $ids)
}

version_digits : Str, U64, U64, U64 -> Bool
version_digits = |text, minimum, maximum, limit| {
	bytes = text.to_utf8()
	var $value = 0.U64
	if bytes.len() < minimum or bytes.len() > maximum or !bytes.all(is_digit) {
		return False
	}
	for digit in bytes {
		$value = $value * 10 + U8.to_u64(digit - '0')
	}
	$value <= limit
}

valid_roc_version : Str -> Bool
valid_roc_version = |text| {
	parts = Str.split_on(text, "-")
	first = parts.first() ?? ""
	if first == "nightly" {
		year = parts.get(1) ?? ""
		month = parts.get(2) ?? ""
		day = parts.get(3) ?? ""
		commit = parts.get(4) ?? ""
		named_month = match month {
			"January" | "February" | "March" | "April" | "May" | "June" | "July" | "August" | "September" | "October" | "November" | "December" => True
			_ => False
		}
		month_number = version_digits(month, 1, 2, 12) and month.to_utf8().any(|b| b != '0')
		parts.len() == 5 and version_digits(year, 4, 4, 65535) and (named_month or month_number) and version_digits(day, 1, 2, 31) and day.to_utf8().any(|b| b != '0') and !commit.is_empty() and commit.to_utf8().all(|b| is_digit(b) or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F'))
	} else {
		numbers = Str.split_on(first, ".")
		valid_numbers = numbers.len() == 3 and numbers.all(|part| version_digits(part, 1, 10, 4294967295))
		if parts.len() == 1 valid_numbers else {
			suffix = text.to_utf8().drop_first(first.count_utf8_bytes() + 1)
			valid_numbers and !suffix.is_empty() and suffix.all(|b| is_digit(b) or is_lower(b) or is_upper(b) or b == '.' or b == '-')
		}
	}
}

expect {
	["app [main!] { pf: platform \"../platform/main.roc\" }", "app [] {}", "app [] { pf: platform glue }", "package [Parser, parse]", "package [parse] { util, other: value }", "app [] { roc: \"nightly-2026-July-31-123c5d7\" }", "package [] { roc: \"1.0.0-rc1\" }"].all(|source| RocSyntax.parse_header_str(source).status == Complete)
}

expect {
	["app []", "app [] { pkg }", "app [] { pkg: 42 }", "app [] { a: platform glue, b: platform glue }", "app [] { roc: platform glue }", "package [] { roc: \"bad\" }", "package [] { roc: \"1.0.0\", roc: \"1.0.1\" }"].all(|source| RocSyntax.parse_header_str(source).status == Recovered)
}

pattern_string : State -> Built
pattern_string = |initial| {
	first = peek(initial)
	var $s = at_pos(initial, initial.pos + 1)
	var $ids = []
	var $end = first.end
	var $captures = False
	var $done = False
	while !$done {
		t = peek($s)
		if t.kind == StringPart {
			{ state: part_state, node: part_node } = add_node(at_pos($s, $s.pos + 1), StringSegment, t.start, t.end, [])
			$s = part_state
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			name_state = skip_trivia(at_pos($s, $s.pos + 1))
			name = peek(name_state)
			checked = if name.kind == LowerIdent name_state else issue(name_state, UnexpectedToken, name.start, name.end)
			{ state: named_state, node: named_node } = add_node(at_pos(checked, checked.pos + if name.kind == EndOfFile or name.kind == InterpolationEnd 0 else 1), if spelling(checked, "_") WildcardPattern else IdentifierPattern, name.start, name.end, [])
			close_state = skip_trivia(named_state)
			close = peek(close_state)
			closed = close.kind == InterpolationEnd
			finished = if closed at_pos(close_state, close_state.pos + 1) else issue(close_state, UnclosedDelimiter, close.start, close.end)
			$end = if closed close.end else name.end
			{ state: capture_state, node: capture_node } = add_node(finished, CapturePattern, t.start, $end, [named_node])
			$s = capture_state
			$ids = $ids.append(capture_node)
			$captures = True
			if !closed {
				$done = True
			}
		} else if t.kind == StringEnd {
			$s = at_pos($s, $s.pos + 1)
			$end = t.end
			$done = True
		} else {
			$s = issue($s, UnclosedString, first.start, $end)
			$done = True
		}
	}
	add_node($s, if $captures PatternStringLiteral else StringLiteral, first.start, $end, $ids)
}

expect {
	RocSyntax.parse_header_str("").status == Complete and RocSyntax.parse_expr_str("match text { \"hello \${name}\" => name, \"\${_}\" => \"\" }").status == Complete and RocSyntax.parse_expr_str("match text { \"\${a + b}\" => 0 }").status != Complete and RocSyntax.parse_expr_str("match text { \"x\".Str => 0 }").status != Complete
}
