import RocSyntaxBuffer

## Lossless parsing of Roc source into tokens, syntax trees and diagnostics.
## Byte spans are half-open. Comments and whitespace are tokens.
##
## Expressions include interpolated and multiline strings, typed literals,
## calls, field access, operators, pipes, nominal construction and control flow.
## Binding patterns, type declarations, annotations, constraints, imports and
## module, app, package and platform headers retain their syntax structure.
## Parsing does not resolve names or establish that a program typechecks.
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
	NodeKind : [File, Declaration, VariableDeclaration, Expect, For, ForStream, While, ExpressionStatement, Lambda, If, Match, MatchBranch, Pattern, IdentifierPattern, WildcardPattern, PatternStringLiteral, CapturePattern, MutablePattern, TagPattern, NominalPattern, TuplePattern, ListPattern, RecordPattern, PatternField, RestPattern, AliasPattern, AlternativePattern, Block, Record, RecordUpdate, UnsetField, RecordBuilder, NominalRecord, NominalApply, RecordField, Header, Dependencies, DependencyField, PlatformReference, Import, FileImport, ImportPath, ImportAlias, ExposedItem, ExposedWildcard, TypeAnnotation, TypeExpression, Identifier, NumberLiteral, StringLiteral, MultilineStringLiteral, StringSegment, Interpolation, TypedLiteral, ArrowCall, MethodArrowCall, CharacterLiteral, Unary, Binary, Call, FieldAccess, OptionalFieldAccess, Suffix, Debug, Crash, Return, Break, Ellipsis, ListLiteral, Tuple, Parenthesized, ErrorNode, UnsupportedNode, TypeName, TypeVariable, TypeWildcard, TypeApply, TypeFunction, TypeTuple, TypeRecord, TypeField, TypeTagUnion, TypeExtension, TypeParenthesized, TypeDeclaration, TypeHeader, WhereClause, WhereMethod, WhereAlias, AssociatedBlock, PlatformRequires, RequiredValue, RequiredType, PlatformProvides, PlatformHosted, SymbolMapping, PlatformTargets, TargetInputsDirectory, TargetEntry, TargetConfig, TargetField, TargetList, TargetFile]

	## Flat node. Its children occupy a contiguous interval in `Parsed.children`.
	Node : { kind : NodeKind, start : U64, end : U64, children_start : U64, children_len : U64 }

	## Structured issues; human-facing text can be rendered outside parsing.
	DiagnosticCode : [InvalidByte, InvalidUtf8, BidiControlInSource, Utf8ByteOrderMark, UnclosedString, UnclosedDelimiter, UnexpectedToken, InvalidNumber, InvalidEscape, InvalidCharacter, UnsupportedSyntax, ResourceLimit]

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

Ctx : { source : List(U8), tokens : List(RocSyntax.Token) }

State := { pos : U64, nodes : RocSyntaxBuffer(RocSyntax.Node), children : RocSyntaxBuffer(U64), node_tail : List(RocSyntax.Node), child_tail : List(U64), diagnostics : List(RocSyntax.Diagnostic), unsupported : Bool, limited : Bool, in_guard : Bool, last : U64 }

# Parsers return the produced node in `State.last` rather than a nested
# `{ state, node }` record. Destructuring a nested result projects and increments
# the whole state before the result is released, so later appends copy shared
# tails (roc-lang/roc#12327).
Built : State

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
			third = byte(source, $i + 2)
			bidi = (b == 0xD8 and second == 0x9C) or (b == 0xE2 and ((second == 0x80 and (third == 0x8E or third == 0x8F or (third >= 0xAA and third <= 0xAE))) or (second == 0x81 and third >= 0xA6 and third <= 0xA9)))
			if bidi {
				$out = push_diagnostic($out, BidiControlInSource, $i, $i + width)
			}
			if $i == 0 and b == 0xEF and second == 0xBB and third == 0xBF {
				$out = push_diagnostic($out, Utf8ByteOrderMark, 0, 3)
			}
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
	bom = byte(source, 0) == 0xEF and byte(source, 1) == 0xBB and byte(source, 2) == 0xBF
	var $tokens = if bom [{ kind: Invalid, start: 0, end: 3 }] else []
	var $diagnostics = utf8_diagnostics(source)
	var $i = if bom 3 else 0
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
			} else if !multiline and $i < source.len() and byte(source, $i) == '"' {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + 1 })
				$i = $i + 1
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
			previous = $tokens.last() ?? { kind: EndOfFile, start: 0, end: 0 }
			field_name = previous.end == start and (token_spelling(source, previous, ".") or token_spelling(source, previous, ".?")) and is_lower(b)
			$kind = if field_name LowerIdent else if standalone_underscore LowerIdent else if b == '_' LowerIdent else if b == '@' UpperIdent else if bad_dollar Invalid else if matches(source, start, "and") and $i - start == 3 Operator else if matches(source, start, "or") and $i - start == 2 Operator else if keyword(source, start, $i) Keyword else if is_upper(b) or (b == '$' and is_upper(next)) UpperIdent else LowerIdent
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
		} else if is_digit(b) or (b == '-' and is_digit(byte(source, start + 1)) and (start == 0 or is_space(byte(source, start - 1)) or !can_end_token(source, $tokens.last() ?? { kind: EndOfFile, start: 0, end: 0 }))) {
			$kind = Number
			number = scan_number(source, start + if b == '-' 1 else 0)
			$i = number.end
			if start > 0 and byte(source, start - 1) == '.' and is_digit(b) {
				var $digits_end = start
				while is_digit(byte(source, $digits_end)) {
					$digits_end = $digits_end + 1
				}
				if byte(source, $digits_end) == '.' {
					$i = $digits_end
				}
			}
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
				$kind = if b == '.' and width == 3 and (byte(source, start + 2) == '<' or byte(source, start + 2) == '=') Operator else if b == '.' or b == ':' Punctuation else Operator
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
	}
	if !$contexts.is_empty() {
		$diagnostics = push_diagnostic($diagnostics, UnclosedDelimiter, source.len(), source.len())
	}

	{ tokens: $tokens.append({ kind: EndOfFile, start: source.len(), end: source.len() }), diagnostics: $diagnostics, unsupported: False }
}

peek : Ctx, State -> RocSyntax.Token
peek = |ctx, s| ctx.tokens.get(s.pos) ?? { kind: EndOfFile, start: ctx.source.len(), end: ctx.source.len() }

skip_trivia : Ctx, State -> State
skip_trivia = |ctx, State.{ pos, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last }| {
	var $pos = pos
	var $done = False
	while !$done {
		kind = (ctx.tokens.get($pos) ?? { kind: EndOfFile, start: ctx.source.len(), end: ctx.source.len() }).kind
		if kind == Whitespace or kind == Comment {
			$pos = $pos + 1
		} else {
			$done = True
		}
	}
	State.{ pos: $pos, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last }
}

spelling : Ctx, State, Str -> Bool
spelling = |ctx, s, text| {
	t = peek(ctx, s)
	t.end - t.start == text.count_utf8_bytes() and matches(ctx.source, t.start, text)
}

issue : Ctx, State, RocSyntax.DiagnosticCode, U64, U64 -> State
issue = |_ctx, s, code, start, end| {
	{ ..s, diagnostics: push_diagnostic(s.diagnostics, code, start, end), unsupported: s.unsupported or code == UnsupportedSyntax, limited: s.limited or code == ResourceLimit }
}

add_node : Ctx, State, RocSyntax.NodeKind, U64, U64, List(U64) -> Built
add_node = |_ctx, State.{ pos, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last: _ }, kind, start, end, ids| {
	# Active tails are direct State fields: a tail nested in the buffer record
	# is projected, incremented and then reserved, copying it on every append.
	index = nodes.len() + node_tail.len()
	node = { kind, start, end, children_start: children.len() + child_tail.len(), children_len: ids.len() }
	{ buffer: next_nodes, tail: next_node_tail } = push_tail(nodes, node_tail, node)
	var $children = children
	var $child_tail = child_tail
	for id in ids {
		{ buffer: c, tail: t } = push_tail($children, $child_tail, id)
		$children = c
		$child_tail = t
	}
	State.{ pos, nodes: next_nodes, children: $children, node_tail: next_node_tail, child_tail: $child_tail, diagnostics, unsupported, limited, in_guard, last: index }
}

push_tail : RocSyntaxBuffer(a), List(a), a -> { buffer : RocSyntaxBuffer(a), tail : List(a) }
push_tail = |buffer, tail, value| {
	if tail.len() < 32 { buffer, tail: tail.append(value) } else { buffer: buffer.archive(tail), tail: List.with_capacity(32).append(value) }
}

with_last : Ctx, State, U64 -> State
with_last = |_ctx, State.{ pos, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last: _ }, node| {
	State.{ pos, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last: node }
}

node_get : Ctx, State, U64 -> Try(RocSyntax.Node, [OutOfBounds])
node_get = |_ctx, s, id| s.nodes.get_with(s.node_tail, id)

child_get : Ctx, State, U64 -> Try(U64, [OutOfBounds])
child_get = |_ctx, s, index| s.children.get_with(s.child_tail, index)

node_count : Ctx, State -> U64
node_count = |_ctx, s| s.nodes.len() + s.node_tail.len()

node_start : Ctx, State, U64 -> U64
node_start = |ctx, s, id| (node_get(ctx, s, id) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).start

node_end : Ctx, State, U64 -> U64
node_end = |ctx, s, id| (node_get(ctx, s, id) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).end

# Exact binding powers from src/parse/Parser.zig at 7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5.
binding : Ctx, State -> { left : U64, right : U64 }
binding = |ctx, s| {
	t = peek(ctx, s)
	text = if t.end - t.start <= 3 Str.from_utf8_lossy(ctx.source.sublist({ start: t.start, len: t.end - t.start })) else ""
	match text {
		"*" | "/" | "//" | "%" => { left: 32, right: 33 }
		"+" => { left: 22, right: 23 }
		"-" if binary_minus(ctx, s) => { left: 22, right: 23 }
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

expression : Ctx, State, U64, U64, Bool -> Built
expression = |ctx, initial, minimum, depth, punned| {
	s = skip_trivia(ctx, initial)
	first = peek(ctx, s)
	sync = declaration_sync(ctx, s)
	if sync.sync {
		return add_node(ctx, issue(ctx, s, UnexpectedToken, sync.start, sync.end), ErrorNode, sync.end, sync.end, [])
	}
	if depth >= 128 {
		limited = issue(ctx, s, ResourceLimit, first.start, first.end)
		add_node(ctx, at_pos(ctx, limited, limited.pos + if first.kind == EndOfFile 0 else 1), ErrorNode, first.start, first.end, [])
	} else {
		next_pos = significant_position(ctx.tokens, s.pos + 1)
		package_qualified = (minimum == 100 or minimum == 101) and first.kind == LowerIdent and token_spelling(ctx.source, source_token(ctx, s, next_pos), ".") and source_token(ctx, s, next_pos + 1).kind == UpperIdent
		built_state = primary(ctx, s, depth, punned)
		built_node = built_state.last
		grouped_tuple = (minimum == 100 or minimum == 101) and (node_get(ctx, built_state, built_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == Tuple
		var $s = if grouped_tuple issue(ctx, built_state, UnexpectedToken, first.start, first.end) else built_state
		var $left = built_node
		var $qualified = first.kind == UpperIdent or package_qualified
		var $done = False
		var $target_done = False
		while !$done {
			look = skip_trivia(ctx, $s)
			# Drop the previous loop-state owner before builders grow its buffers.
			$s = vacant
			t = peek(ctx, look)
			bp = binding(ctx, look)
			arrow_name = peek(ctx, at_pos(ctx, look, look.pos + 1))
			arrow_field = spelling(ctx, look, ".") and t.start == node_end(ctx, look, $left) and (($qualified and (arrow_name.kind == LowerIdent or arrow_name.kind == UpperIdent)) or (first.kind == LowerIdent and $left == built_node and arrow_name.kind == LowerIdent))
			pipe_boundary = (minimum == 100 or minimum == 101) and t.start > node_end(ctx, look, $left) and ((spelling(ctx, look, ".") and (peek(ctx, at_pos(ctx, look, look.pos + 1)).kind == LowerIdent or peek(ctx, at_pos(ctx, look, look.pos + 1)).kind == UpperIdent or peek(ctx, at_pos(ctx, look, look.pos + 1)).kind == Number)) or spelling(ctx, look, ".?"))
			if pipe_boundary or ((minimum >= 100 or look.in_guard) and (spelling(ctx, look, "->") or spelling(ctx, look, "=>") or spelling(ctx, look, "|>"))) or (minimum == 101 and (($target_done and t.kind != NoSpaceOpenRound) or spelling(ctx, look, "?") or ((spelling(ctx, look, ".") or spelling(ctx, look, ".?")) and !arrow_field))) {
				$s = look
				$done = True
			} else if spelling(ctx, look, "->") or spelling(ctx, look, "=>") or spelling(ctx, look, "|>") {
				pipe = spelling(ctx, look, "|>")
				rhs = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
				rhs_token = peek(ctx, rhs)
				literal = rhs_token.kind == Number or rhs_token.kind == StringStart or rhs_token.kind == MultilineStringStart or rhs_token.kind == SingleQuote or rhs_token.kind == OpenSquare or rhs_token.kind == OpenCurly
				valid_start = (rhs_token.kind == LowerIdent and byte(ctx.source, rhs_token.start) != '_') or rhs_token.kind == UpperIdent or rhs_token.kind == OpenRound or rhs_token.kind == NoSpaceOpenRound or (pipe and literal)
				validated = if valid_start rhs else issue(ctx, rhs, UnexpectedToken, rhs_token.start, rhs_token.end)
				target_state = expression(ctx, validated, if pipe 100 else 101, depth + 1, False)
				target_node = target_state.last
				method = pipe and method_target(ctx, target_state, target_node)
				checked = if !pipe or !literal or pipe_has_method(ctx, target_state, target_node) target_state else issue(ctx, target_state, UnexpectedToken, rhs_token.start, node_end(ctx, target_state, target_node))
				start = node_start(ctx, checked, $left)
				end = node_end(ctx, checked, target_node)
				arrow_state = add_node(ctx, checked, if method MethodArrowCall else ArrowCall, start, end, [$left, target_node])
				arrow_node = arrow_state.last
				$s = arrow_state
				$left = arrow_node
				$qualified = False
			} else if spelling(ctx, look, "?") and t.start == node_end(ctx, look, $left) {
				start = node_start(ctx, look, $left)
				suffix_state = add_node(ctx, at_pos(ctx, look, look.pos + 1), Suffix, start, t.end, [$left])
				suffix_node = suffix_state.last
				$s = suffix_state
				$left = suffix_node
				$qualified = False
			} else if t.kind == NoSpaceOpenRound {
				validated = if is_optional_access(ctx, look, $left) issue(ctx, look, UnexpectedToken, t.start, t.end) else look
				start = node_start(ctx, look, $left)
				{ state: args_state, ids: args_ids, end: args_end, comma: _ } = collection(ctx, at_pos(ctx, validated, validated.pos + 1), CloseRound, depth + 1)
				ids = [$left].concat(args_ids)
				call_state = add_node(ctx, args_state, Call, start, args_end, ids)
				call_node = call_state.last
				$s = call_state
				$left = call_node
				$qualified = False
				$target_done = minimum == 101
				question_state = skip_trivia(ctx, call_state)
				if minimum == 100 and !method_target(ctx, call_state, call_node) and spelling(ctx, question_state, "?") and peek(ctx, question_state).start == args_end {
					$done = True
				}
			} else if spelling(ctx, look, ".") and peek(ctx, skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))).kind == OpenCurly {
				opening_state = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
				backing_state = curly(ctx, at_pos(ctx, opening_state, opening_state.pos + 1), peek(ctx, opening_state).start, depth + 1, True, True)
				backing_node = backing_state.last
				start = node_start(ctx, backing_state, $left)
				end = node_end(ctx, backing_state, backing_node)
				nominal_state = add_node(ctx, backing_state, NominalRecord, start, end, [$left, backing_node])
				nominal_node = nominal_state.last
				$s = nominal_state
				$left = nominal_node
				$qualified = False
			} else if spelling(ctx, look, ".") and $qualified and peek(ctx, at_pos(ctx, look, look.pos + 1)).kind == NoSpaceOpenRound {
				opening_state = at_pos(ctx, look, look.pos + 1)
				{ state: args_state, ids: args, end, comma: _ } = collection(ctx, at_pos(ctx, opening_state, opening_state.pos + 1), CloseRound, depth + 1)
				start = node_start(ctx, args_state, $left)
				nominal_state = add_node(ctx, args_state, NominalApply, start, end, [$left].concat(args))
				nominal_node = nominal_state.last
				$s = nominal_state
				$left = nominal_node
				$qualified = False
			} else if spelling(ctx, look, ".") or spelling(ctx, look, ".?") {
				optional = spelling(ctx, look, ".?")
				field_state = at_pos(ctx, look, look.pos + 1)
				field = peek(ctx, field_state)
				valid_name = (field.kind == LowerIdent and is_lower(byte(ctx.source, field.start))) or ($qualified and field.kind == UpperIdent and is_upper(byte(ctx.source, field.start)))
				valid_index = field.kind == Number and ctx.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit)
				if (valid_name and (!optional or field.kind == LowerIdent)) or (valid_index and !optional) {
					start = node_start(ctx, field_state, $left)
					leaf_state = add_node(ctx, at_pos(ctx, field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
					leaf_node = leaf_state.last
					access_state = add_node(ctx, leaf_state, if optional OptionalFieldAccess else FieldAccess, start, field.end, [$left, leaf_node])
					access_node = access_state.last
					$s = access_state
					$left = access_node
					$qualified = $qualified and field.kind == UpperIdent
				} else {
					$s = look
					$done = True
				}
			} else if spelling(ctx, look, "..") or spelling(ctx, look, ".*") {
				$s = issue(ctx, at_pos(ctx, look, look.pos + 1), UnexpectedToken, t.start, t.end)
				$done = True
			} else if bp.left > 0 and bp.left >= minimum {
				start = node_start(ctx, look, $left)
				right_state = expression(ctx, at_pos(ctx, look, look.pos + 1), bp.right, depth + 1, False)
				right_node = right_state.last
				end = node_end(ctx, right_state, right_node)
				binary_state = add_node(ctx, right_state, Binary, start, end, [$left, right_node])
				binary_node = binary_state.last
				$s = binary_state
				$left = binary_node
				$qualified = False
			} else {
				$s = look
				$done = True
			}
		}
		with_last(ctx, $s, $left)
	}
}

Collection : { state : State, ids : List(U64), end : U64, comma : Bool }

collection : Ctx, State, RocSyntax.TokenKind, U64 -> Collection
collection = |ctx, initial, closing, depth| {
	var $s = initial
	var $ids = []
	var $end = peek(ctx, initial).start
	var $done = False
	var $comma = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		sync = declaration_sync(ctx, look)
		if sync.sync {
			$s = issue(ctx, look, UnclosedDelimiter, sync.start, sync.end)
			$end = sync.end
			$done = True
		} else if t.kind == closing {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else {
			before = look.pos
			item_state = expression(ctx, look, 0, depth, False)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$end = node_end(ctx, item_state, item_node)
			$s = skip_trivia(ctx, item_state)
			if spelling(ctx, $s, ",") {
				$comma = True
				$s = at_pos(ctx, $s, $s.pos + 1)
			} else if peek(ctx, $s).kind != closing and peek(ctx, $s).kind != EndOfFile {
				bad = peek(ctx, $s)
				$s = issue(ctx, $s, UnexpectedToken, bad.start, bad.end)
				if $s.pos <= before {
					$s = at_pos(ctx, $s, $s.pos + 1)
				}
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: $comma }
}

primary : Ctx, State, U64, Bool -> Built
primary = |ctx, s, depth, punned| {
	t = peek(ctx, s)
	match t.kind {
		LowerIdent if spelling(ctx, s, "_") => {
			bad = issue(ctx, at_pos(ctx, s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(ctx, bad, ErrorNode, t.start, t.end, [])
		}
		UpperIdent if byte(ctx.source, t.start) == '@' => {
			bad = issue(ctx, at_pos(ctx, s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(ctx, bad, ErrorNode, t.start, t.end, [])
		}
		LowerIdent | UpperIdent => add_node(ctx, at_pos(ctx, s, s.pos + 1), Identifier, t.start, t.end, [])
		Number => {
			after = at_pos(ctx, s, s.pos + 1)
			dot = peek(ctx, after)
			field_state = at_pos(ctx, after, after.pos + 1)
			field = peek(ctx, field_state)
			if integer_token(ctx.source, t) and spelling(ctx, after, ".") and dot.start == t.end and field.kind == Number and ctx.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit) {
				bad = issue(ctx, at_pos(ctx, field_state, field_state.pos + 1), UnexpectedToken, dot.start, field.end)
				add_node(ctx, bad, ErrorNode, t.start, field.end, [])
			} else {
				literal_suffix(ctx, add_node(ctx, after, NumberLiteral, t.start, t.end, []), False)
			}
		}
		SingleQuote => literal_suffix(ctx, add_node(ctx, at_pos(ctx, s, s.pos + 1), CharacterLiteral, t.start, t.end, []), False)
		StringStart | MultilineStringStart => parse_string(ctx, s, depth + 1)
		OpenCurly => curly(ctx, at_pos(ctx, s, s.pos + 1), t.start, depth + 1, punned, False)
		Keyword if spelling(ctx, s, "dbg") or spelling(ctx, s, "crash") or spelling(ctx, s, "return") => {
			kind = if spelling(ctx, s, "dbg") Debug else if spelling(ctx, s, "crash") Crash else Return
			value_state = expression(ctx, at_pos(ctx, s, s.pos + 1), 0, depth + 1, False)
			value_node = value_state.last
			end = node_end(ctx, value_state, value_node)
			add_node(ctx, value_state, kind, t.start, end, [value_node])
		}
		Keyword if spelling(ctx, s, "break") => add_node(ctx, at_pos(ctx, s, s.pos + 1), Break, t.start, t.end, [])
		Punctuation if spelling(ctx, s, "...") => add_node(ctx, at_pos(ctx, s, s.pos + 1), Ellipsis, t.start, t.end, [])
		Keyword if spelling(ctx, s, "if") => parse_if(ctx, at_pos(ctx, s, s.pos + 1), t.start, depth + 1)
		Keyword if spelling(ctx, s, "for") or spelling(ctx, s, "for!") => parse_for(ctx, at_pos(ctx, s, s.pos + 1), t.start, spelling(ctx, s, "for!"), depth + 1)
		Keyword if spelling(ctx, s, "match") => parse_match(ctx, at_pos(ctx, s, s.pos + 1), t.start, depth + 1)
		Operator if spelling(ctx, s, "|") => parse_lambda(ctx, at_pos(ctx, s, s.pos + 1), t.start, depth + 1)
		OpenRound | NoSpaceOpenRound | OpenSquare => {
			{ state: items_state, ids: items_ids, end: items_end, comma: items_comma } = collection(ctx, at_pos(ctx, s, s.pos + 1), if t.kind == OpenSquare CloseSquare else CloseRound, depth + 1)
			kind = if t.kind == OpenSquare ListLiteral else if items_comma or items_ids.len() != 1 Tuple else Parenthesized
			add_node(ctx, items_state, kind, t.start, items_end, items_ids)
		}
		Operator if spelling(ctx, s, "-") and binary_minus(ctx, s) => {
			bad = issue(ctx, at_pos(ctx, s, s.pos + 1), UnexpectedToken, t.start, t.end)
			add_node(ctx, bad, ErrorNode, t.start, t.end, [])
		}
		Operator if spelling(ctx, s, "-") or spelling(ctx, s, "!") => {
			operand_state = expression(ctx, at_pos(ctx, s, s.pos + 1), 40, depth + 1, False)
			operand_node = operand_state.last
			end = node_end(ctx, operand_state, operand_node)
			add_node(ctx, operand_state, Unary, t.start, end, [operand_node])
		}
		EndOfFile | InterpolationEnd | CloseRound | CloseSquare | CloseCurly => {
			err = issue(ctx, s, UnexpectedToken, t.start, t.end)
			add_node(ctx, err, ErrorNode, t.start, t.start, [])
		}
		Invalid => add_node(ctx, at_pos(ctx, s, s.pos + 1), ErrorNode, t.start, t.end, [])
		_ => {
			unknown = issue(ctx, at_pos(ctx, s, s.pos + 1), UnsupportedSyntax, t.start, t.end)
			add_node(ctx, unknown, UnsupportedNode, t.start, t.end, [])
		}
	}
}

ParseMode : [FileMode, ExpressionMode, StatementMode, HeaderMode]

parse_source : List(U8), ParseMode -> RocSyntax.Parsed
parse_source = |source, mode| {
	expression_only = mode == ExpressionMode
	single = mode != FileMode
	lexed = lex(source)
	ctx = { source, tokens: lexed.tokens }
	initial = State.{ pos: 0, nodes: RocSyntaxBuffer.empty({}), children: RocSyntaxBuffer.empty({}), node_tail: List.with_capacity(32), child_tail: List.with_capacity(32), diagnostics: lexed.diagnostics, unsupported: lexed.unsupported, limited: False, in_guard: False, last: 0 }
	var $s = initial
	var $roots = []
	if ctx.tokens.len() > 1048576 {
		$s = issue(ctx, $s, ResourceLimit, 0, source.len())
	} else {
		var $done = False
		while !$done {
			look = skip_trivia(ctx, $s)
			# Drop the previous loop-state owner before builders grow its buffers.
			$s = vacant
			t = peek(ctx, look)
			if t.kind == EndOfFile {
				$s = look
				$done = True
			} else {
				before = look.pos
				built_state = if expression_only expression(ctx, look, 0, 0, False) else statement(ctx, look, if mode == StatementMode 1 else 0)
				built_node = built_state.last
				$roots = $roots.append(built_node)
				$s = built_state
				if mode == FileMode or mode == HeaderMode {
					if (node_get(ctx, $s, built_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == ExpressionStatement {
						$s = issue(ctx, $s, UnexpectedToken, t.start, node_end(ctx, $s, built_node))
					}
				}
				if $s.pos <= before {
					$s = issue(ctx, $s, UnexpectedToken, t.start, t.end)
					$s = at_pos(ctx, $s, before + 1)
				}
				if mode == HeaderMode and (node_get(ctx, $s, built_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind != Header {
					$s = issue(ctx, $s, UnexpectedToken, t.start, node_end(ctx, $s, built_node))
				}
				if single {
					$s = skip_trivia(ctx, $s)
					if peek(ctx, $s).kind != EndOfFile {
						extra = peek(ctx, $s)
						$s = issue(ctx, $s, UnexpectedToken, extra.start, extra.end)
					}
					$done = True
				}
			}
		}
	}
	if mode == HeaderMode and $roots.is_empty() {
		header_state = add_node(ctx, $s, Header, 0, 0, [])
		header_node = header_state.last
		$s = header_state
		$roots = [header_node]
	} else if single and $roots.is_empty() {
		$s = issue(ctx, $s, UnexpectedToken, source.len(), source.len())
	}
	root_state = add_node(ctx, $s, File, 0, source.len(), $roots)
	root_node = root_state.last
	nodes = root_state.nodes.to_list_with(root_state.node_tail)
	first_start = (source_token(ctx, root_state, significant_position(ctx.tokens, 0))).start
	var $header_count = 0.U64
	var $bad_header = False
	for n in nodes {
		if n.kind == Header {
			$header_count = $header_count + 1
			$bad_header = $bad_header or n.start != first_start
		}
	}
	finished = if $header_count > 1 or $bad_header issue(ctx, root_state, UnexpectedToken, first_start, source.len()) else root_state
	has_error = finished.diagnostics.any(|d| d.code != UnsupportedSyntax)
	status = if finished.limited Limited else if finished.unsupported Unsupported else if has_error Recovered else Complete
	{ source: ctx.source, tokens: ctx.tokens, nodes, children: finished.children.to_list_with(finished.child_tail), diagnostics: finished.diagnostics, status, root: root_node, consumed: peek(ctx, skip_trivia(ctx, finished)).kind == EndOfFile }
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
statement : Ctx, State, U64 -> Built
statement = |ctx, initial, depth| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	following_pos = significant_position(ctx.tokens, s.pos + 1)
	following = ctx.tokens.get(following_pos) ?? t
	if depth >= 128 {
		bad = issue(ctx, s, ResourceLimit, t.start, t.end)
		add_node(ctx, at_pos(ctx, bad, bad.pos + if t.kind == EndOfFile 0 else 1), ErrorNode, t.start, t.end, [])
	} else if (t.kind == UpperIdent and (type_declaration_start(ctx, s) or (depth == 0 and !((following.kind == NoSpaceOpenRound or token_spelling(ctx.source, following, ".")) and has_pattern_assignment(ctx, s))))) or (t.kind == LowerIdent and token_spelling(ctx.source, following, ".") and peek(ctx, at_pos(ctx, s, following_pos + 1)).kind == UpperIdent) {
		parse_type_declaration(ctx, s, depth + 1)
	} else if t.kind == LowerIdent and token_spelling(ctx.source, following, "=") {
		name_state = parse_pattern(ctx, s, False, depth + 1)
		name_node = name_state.last
		after_name = skip_trivia(ctx, name_state)
		value_state = expression(ctx, at_pos(ctx, after_name, after_name.pos + 1), 0, depth, False)
		value_node = value_state.last
		end = node_end(ctx, value_state, value_node)
		add_node(ctx, value_state, Declaration, t.start, end, [name_node, value_node])
	} else if t.kind == LowerIdent and token_spelling(ctx.source, following, ":") {
		validated = if spelling(ctx, s, "_") issue(ctx, s, UnexpectedToken, t.start, t.end) else s
		name_state = add_node(ctx, at_pos(ctx, validated, following_pos), Identifier, t.start, t.end, [])
		name_node = name_state.last
		parse_annotation_after_name(ctx, at_pos(ctx, name_state, name_state.pos + 1), name_node, t.start, depth)
	} else if spelling(ctx, s, "var") {
		name_state = skip_trivia(ctx, at_pos(ctx, s, s.pos + 1))
		name = peek(ctx, name_state)
		validated = if depth == 0 or name.kind != LowerIdent or !(is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') issue(ctx, name_state, UnexpectedToken, name.start, name.end) else name_state
		named_state = add_node(ctx, at_pos(ctx, validated, validated.pos + if name.kind == EndOfFile 0 else 1), Identifier, name.start, name.end, [])
		named_node = named_state.last
		look = skip_trivia(ctx, named_state)
		if spelling(ctx, look, "=") {
			value_state = expression(ctx, at_pos(ctx, look, look.pos + 1), 0, depth + 1, False)
			value_node = value_state.last
			end = node_end(ctx, value_state, value_node)
			add_node(ctx, value_state, VariableDeclaration, t.start, end, [named_node, value_node])
		} else if spelling(ctx, look, ":") {
			parse_annotation_after_name(ctx, at_pos(ctx, look, look.pos + 1), named_node, t.start, depth + 1)
		} else {
			add_node(ctx, look, VariableDeclaration, t.start, name.end, [named_node])
		}
	} else if spelling(ctx, s, "expect") {
		value_state = expression(ctx, at_pos(ctx, s, s.pos + 1), 0, depth + 1, False)
		value_node = value_state.last
		end = node_end(ctx, value_state, value_node)
		add_node(ctx, value_state, Expect, t.start, end, [value_node])
	} else if spelling(ctx, s, "while") {
		condition_state = expression(ctx, at_pos(ctx, s, s.pos + 1), 0, depth + 1, False)
		condition_node = condition_state.last
		body_state = expression(ctx, condition_state, 0, depth + 1, False)
		body_node = body_state.last
		end = node_end(ctx, body_state, body_node)
		add_node(ctx, body_state, While, t.start, end, [condition_node, body_node])
	} else if spelling(ctx, s, "for") or spelling(ctx, s, "for!") or spelling(ctx, s, "dbg") or spelling(ctx, s, "crash") or spelling(ctx, s, "return") or spelling(ctx, s, "break") {
		primary(ctx, s, depth + 1, False)
	} else if (t.kind == OpenRound or t.kind == OpenCurly or t.kind == UpperIdent) and has_pattern_assignment(ctx, s) {
		pattern_state = parse_pattern(ctx, s, False, depth + 1)
		pattern_node = pattern_state.last
		look = skip_trivia(ctx, pattern_state)
		validated = if spelling(ctx, look, "=") look else issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end)
		value_state = expression(ctx, at_pos(ctx, validated, validated.pos + if spelling(ctx, validated, "=") 1 else 0), 0, depth + 1, False)
		value_node = value_state.last
		end = node_end(ctx, value_state, value_node)
		add_node(ctx, value_state, Declaration, t.start, end, [pattern_node, value_node])
	} else if spelling(ctx, s, "platform") {
		validated = if depth == 0 s else issue(ctx, s, UnexpectedToken, t.start, t.end)
		parse_platform_header(ctx, validated, depth + 1)
	} else if spelling(ctx, s, "app") or spelling(ctx, s, "package") {
		{ state: exposed_state, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(ctx, at_pos(ctx, s, s.pos + 1))
		look = skip_trivia(ctx, exposed_state)
		if peek(ctx, look).kind == OpenCurly {
			deps_state = dependency_record(ctx, look, spelling(ctx, s, "app"), depth + 1)
			deps_node = deps_state.last
			end = node_end(ctx, deps_state, deps_node)
			validated = if depth == 0 deps_state else issue(ctx, deps_state, UnexpectedToken, t.start, end)
			add_node(ctx, validated, Header, t.start, end, exposed_ids.append(deps_node))
		} else {
			validated = if depth == 0 and spelling(ctx, s, "package") look else issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end)
			add_node(ctx, validated, Header, t.start, exposed_end, exposed_ids)
		}
	} else if spelling(ctx, s, "module") or spelling(ctx, s, "hosted") {
		{ state: items_state, ids: items_ids, end: items_end, comma: _ } = exposed_collection(ctx, at_pos(ctx, s, s.pos + 1))
		validated = if depth == 0 items_state else issue(ctx, items_state, UnexpectedToken, t.start, items_end)
		add_node(ctx, validated, Header, t.start, items_end, items_ids)
	} else if spelling(ctx, s, "import") {
		validated = if depth == 0 s else issue(ctx, s, UnexpectedToken, t.start, t.end)
		parse_import(ctx, validated, depth + 1)
	} else {
		value_state = expression(ctx, s, 0, depth, True)
		value_node = value_state.last
		end = node_end(ctx, value_state, value_node)
		add_node(ctx, value_state, ExpressionStatement, t.start, end, [value_node])
	}
}

exposed_collection : Ctx, State -> Collection
exposed_collection = |ctx, initial| {
	look = skip_trivia(ctx, initial)
	opening = peek(ctx, look)
	var $s = if opening.kind == OpenSquare at_pos(ctx, look, look.pos + 1) else issue(ctx, look, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenSquare
	while !$done {
		item_state = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, item_state)
		if t.kind == CloseSquare {
			$s = at_pos(ctx, item_state, item_state.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseCurly or t.kind == CloseRound {
			$s = issue(ctx, item_state, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			valid_start = (t.kind == UpperIdent and byte(ctx.source, t.start) != '@') or (t.kind == LowerIdent and (is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$'))
			validated = if valid_start item_state else issue(ctx, item_state, UnexpectedToken, t.start, t.end)
			name_state = add_node(ctx, at_pos(ctx, validated, validated.pos + 1), Identifier, t.start, t.end, [])
			name_node = name_state.last
			var $name = name_node
			$s = skip_trivia(ctx, name_state)
			var $upper = t.kind == UpperIdent
			var $qualified = t.kind == UpperIdent
			var $chain_done = False
			while !$chain_done {
				if $qualified and spelling(ctx, $s, ".") and peek(ctx, $s).start == node_end(ctx, $s, $name) and (is_upper(byte(ctx.source, peek(ctx, at_pos(ctx, $s, $s.pos + 1)).start)) or is_lower(byte(ctx.source, peek(ctx, at_pos(ctx, $s, $s.pos + 1)).start))) {
					field_state = at_pos(ctx, $s, $s.pos + 1)
					field = peek(ctx, field_state)
					named_state = add_node(ctx, at_pos(ctx, field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
					named_node = named_state.last
					access_state = add_node(ctx, named_state, FieldAccess, t.start, field.end, [$name, named_node])
					access_node = access_state.last
					$s = skip_trivia(ctx, access_state)
					$name = access_node
					$upper = True
					$qualified = True
				} else {
					$chain_done = True
				}
			}
			var $children = [$name]
			var $wildcard = False
			$end = node_end(ctx, $s, $name)
			if spelling(ctx, $s, "as") {
				alias_state = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				alias = peek(ctx, alias_state)
				valid_alias = if $upper alias.kind == UpperIdent and byte(ctx.source, alias.start) != '@' else alias.kind == LowerIdent and byte(ctx.source, alias.start) != '_'
				checked = if valid_alias alias_state else issue(ctx, alias_state, UnexpectedToken, alias.start, alias.end)
				alias_built = add_node(ctx, at_pos(ctx, checked, checked.pos + if alias.kind == EndOfFile 0 else 1), Identifier, alias.start, alias.end, [])
				alias_node = alias_built.last
				$s = alias_built
				$children = $children.append(alias_node)
				$end = alias.end
			} else if $upper and spelling(ctx, $s, ".*") {
				$end = peek(ctx, $s).end
				$s = at_pos(ctx, $s, $s.pos + 1)
				$wildcard = True
			}
			exposed_state = add_node(ctx, $s, if $wildcard ExposedWildcard else ExposedItem, t.start, $end, $children)
			exposed_node = exposed_state.last
			$ids = $ids.append(exposed_node)
			$s = skip_trivia(ctx, exposed_state)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			} else if peek(ctx, $s).kind != CloseSquare {
				$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: False }
}

parse_import : Ctx, State, U64 -> Built
parse_import = |ctx, initial, depth| {
	start = peek(ctx, initial).start
	look = skip_trivia(ctx, at_pos(ctx, initial, initial.pos + 1))
	if peek(ctx, look).kind == StringStart {
		path_state = parse_string(ctx, look, depth + 1)
		path_node = path_state.last
		after_path = skip_trivia(ctx, path_state)
		validated = if spelling(ctx, after_path, "as") at_pos(ctx, after_path, after_path.pos + 1) else issue(ctx, after_path, UnexpectedToken, peek(ctx, after_path).start, peek(ctx, after_path).end)
		name_state = skip_trivia(ctx, validated)
		name = peek(ctx, name_state)
		checked = if name.kind == LowerIdent and !spelling(ctx, name_state, "_") name_state else issue(ctx, name_state, UnexpectedToken, name.start, name.end)
		named_state = add_node(ctx, at_pos(ctx, checked, checked.pos + if name.kind == EndOfFile 0 else 1), Identifier, name.start, name.end, [])
		named_node = named_state.last
		colon = skip_trivia(ctx, named_state)
		ty_start = if spelling(ctx, colon, ":") at_pos(ctx, colon, colon.pos + 1) else issue(ctx, colon, UnexpectedToken, peek(ctx, colon).start, peek(ctx, colon).end)
		ty_state = file_import_type(ctx, ty_start)
		ty_node = ty_state.last
		end = node_end(ctx, ty_state, ty_node)
		path_info = node_get(ctx, ty_state, path_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
		path_child = child_get(ctx, ty_state, path_info.children_start) ?? node_count(ctx, ty_state)
		part_info = node_get(ctx, ty_state, path_child) ?? path_info
		plain_path = path_info.kind == StringLiteral and path_info.children_len == 1 and part_info.kind == StringSegment
		checked_type = if plain_path ty_state else issue(ctx, ty_state, UnexpectedToken, start, end)
		add_node(ctx, checked_type, FileImport, start, end, [path_node, named_node, ty_node])
	} else {
		var $s = look
		path_start = peek(ctx, look).start
		var $end = path_start
		var $local = True
		if spelling(ctx, $s, "/") {
			$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		} else if spelling(ctx, $s, ".") and token_spelling(ctx.source, source_token(ctx, $s, significant_position(ctx.tokens, $s.pos + 1)), "/") {
			$s = skip_trivia(ctx, at_pos(ctx, $s, significant_position(ctx.tokens, $s.pos + 1) + 1))
		} else if spelling(ctx, $s, "..") {
			while spelling(ctx, $s, "..") {
				parent = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				if spelling(ctx, parent, "/") {
					$s = skip_trivia(ctx, at_pos(ctx, parent, parent.pos + 1))
				} else {
					$s = issue(ctx, parent, UnexpectedToken, peek(ctx, parent).start, peek(ctx, parent).end)
				}
			}
		} else if peek(ctx, $s).kind == LowerIdent {
			next = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
			$local = spelling(ctx, next, "/")
		}
		first = peek(ctx, $s)
		$end = U64.max($end, first.start)
		var $upper = first.kind == UpperIdent
		if first.kind != UpperIdent and first.kind != LowerIdent {
			$s = issue(ctx, $s, UnexpectedToken, first.start, first.end)
		} else {
			$end = first.end
			$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		}
		if $local {
			while spelling(ctx, $s, "/") {
				$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				part = peek(ctx, $s)
				if part.kind == LowerIdent or part.kind == UpperIdent {
					$upper = part.kind == UpperIdent
					$end = part.end
					$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				} else {
					$s = issue(ctx, $s, UnexpectedToken, part.start, part.end)
				}
			}
			if !$upper {
				$s = issue(ctx, $s, UnexpectedToken, first.start, $end)
			}
		}
		var $nested = 0.U64
		while spelling(ctx, $s, ".") and peek(ctx, at_pos(ctx, $s, $s.pos + 1)).kind == UpperIdent {
			field_state = at_pos(ctx, $s, $s.pos + 1)
			$end = peek(ctx, field_state).end
			$s = skip_trivia(ctx, at_pos(ctx, field_state, field_state.pos + 1))
			$nested = $nested + 1
		}
		if !$local and $nested == 0 {
			$s = issue(ctx, $s, UnexpectedToken, first.start, $end)
		}
		path_state = add_node(ctx, $s, ImportPath, path_start, $end, [])
		path_node = path_state.last
		$s = path_state
		var $ids = [path_node]
		if spelling(ctx, $s, "as") {
			alias_state = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
			alias = peek(ctx, alias_state)
			checked = if alias.kind == UpperIdent alias_state else issue(ctx, alias_state, UnexpectedToken, alias.start, alias.end)
			named_state = add_node(ctx, at_pos(ctx, checked, checked.pos + if alias.kind == EndOfFile 0 else 1), Identifier, alias.start, alias.end, [])
			named_node = named_state.last
			alias_built = add_node(ctx, named_state, ImportAlias, alias.start, alias.end, [named_node])
			alias_node = alias_built.last
			$s = skip_trivia(ctx, alias_built)
			$ids = $ids.append(alias_node)
			$end = alias.end
		}
		if spelling(ctx, $s, "exposing") {
			{ state: exposed_state, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(ctx, at_pos(ctx, $s, $s.pos + 1))
			$s = exposed_state
			$ids = $ids.concat(exposed_ids)
			$end = exposed_end
		}
		add_node(ctx, $s, Import, start, $end, $ids)
	}
}

# Dedicated type syntax never delegates to value-expression parsing, except
# record-field defaults, which the language explicitly defines as expressions.
TypeMode : [FullType, TypeItem, TypeArgument]

type_expression : Ctx, State, TypeMode, U64 -> Built
type_expression = |ctx, initial, mode, depth| {
	atom_state = type_atom(ctx, initial, depth + 1)
	atom_node = atom_state.last
	look = skip_trivia(ctx, atom_state)
	if mode != TypeArgument and (spelling(ctx, look, "->") or spelling(ctx, look, "=>") or (mode == FullType and type_comma_begins_arguments(ctx, look))) {
		var $s = look
		var $args = [atom_node]
		var $done = False
		while !$done and spelling(ctx, $s, ",") {
			arg_state = type_expression(ctx, at_pos(ctx, $s, $s.pos + 1), TypeArgument, depth + 1)
			arg_node = arg_state.last
			$args = $args.append(arg_node)
			$s = skip_trivia(ctx, arg_state)
			$done = peek(ctx, $s).kind == EndOfFile
		}
		if spelling(ctx, $s, "->") or spelling(ctx, $s, "=>") {
			ret_state = type_expression(ctx, at_pos(ctx, $s, $s.pos + 1), TypeArgument, depth + 1)
			ret_node = ret_state.last
			end = node_end(ctx, ret_state, ret_node)
			add_node(ctx, ret_state, TypeFunction, node_start(ctx, ret_state, atom_node), end, $args.append(ret_node))
		} else {
			bad = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
			add_node(ctx, bad, ErrorNode, node_start(ctx, bad, atom_node), U64.max(peek(ctx, bad).start, node_end(ctx, bad, $args.last() ?? atom_node)), $args)
		}
	} else {
		with_last(ctx, atom_state, atom_node)
	}
}

# A comma in a record/where constraint may begin its next field, rather than
# the argument list of a function. Only bounded significant lookahead is used.
type_comma_begins_arguments : Ctx, State -> Bool
type_comma_begins_arguments = |ctx, s| {
	if !spelling(ctx, s, ",") {
		return False
	}
	a = skip_trivia(ctx, at_pos(ctx, s, s.pos + 1))
	if peek(ctx, a).kind == CloseCurly or peek(ctx, a).kind == CloseSquare or spelling(ctx, a, "..") {
		return False
	}
	b = skip_trivia(ctx, at_pos(ctx, a, a.pos + 1))
	if peek(ctx, a).kind == LowerIdent and (spelling(ctx, b, ":") or spelling(ctx, b, "?")) {
		return False
	}
	if peek(ctx, a).kind == LowerIdent and spelling(ctx, b, ".") {
		c = skip_trivia(ctx, at_pos(ctx, b, b.pos + 1))
		d = skip_trivia(ctx, at_pos(ctx, c, c.pos + 1))
		if spelling(ctx, d, ":") {
			return False
		}
	}
	True
}

type_atom : Ctx, State, U64 -> Built
type_atom = |ctx, initial, depth| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	sync = declaration_sync(ctx, s)
	if sync.sync {
		return add_node(ctx, issue(ctx, s, UnexpectedToken, sync.start, sync.end), ErrorNode, sync.end, sync.end, [])
	}
	if depth >= 128 {
		bad = issue(ctx, s, ResourceLimit, t.start, t.end)
		return add_node(ctx, bad, ErrorNode, t.start, t.start, [])
	}
	if (t.kind == LowerIdent or t.kind == UpperIdent) and byte(ctx.source, t.start) != '@' {
		var $s = at_pos(ctx, s, s.pos + 1)
		var $end = t.end
		var $qualified = False
		var $done = False
		while !$done {
			look = skip_trivia(ctx, $s)
			if byte(ctx.source, t.start) != '_' and spelling(ctx, look, ".") and peek(ctx, look).start == $end {
				name_state = at_pos(ctx, look, look.pos + 1)
				name = peek(ctx, name_state)
				if (name.kind == LowerIdent or name.kind == UpperIdent) and name.start == peek(ctx, look).end and byte(ctx.source, name.start) != '_' and byte(ctx.source, name.start) != '@' {
					$s = at_pos(ctx, name_state, name_state.pos + 1)
					$end = name.end
					$qualified = True
				} else {
					$done = True
				}
			} else {
				$done = True
			}
		}
		kind = if $qualified or t.kind == UpperIdent TypeName else if token_spelling(ctx.source, t, "_") TypeWildcard else TypeVariable
		base_state = add_node(ctx, $s, kind, t.start, $end, [])
		base_node = base_state.last
		look = skip_trivia(ctx, base_state)
		if kind == TypeName and peek(ctx, look).kind == NoSpaceOpenRound {
			items = type_collection(ctx, at_pos(ctx, look, look.pos + 1), CloseRound, TypeItem, depth + 1)
			add_node(ctx, items.state, TypeApply, t.start, items.end, [base_node].concat(items.ids))
		} else {
			with_last(ctx, base_state, base_node)
		}
	} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound {
		type_parenthesized(ctx, s, depth + 1)
	} else if t.kind == OpenSquare {
		type_fields(ctx, s, False, depth + 1)
	} else if t.kind == OpenCurly {
		type_fields(ctx, s, True, depth + 1)
	} else {
		bad = issue(ctx, s, UnexpectedToken, t.start, t.end)
		advance = t.kind != EndOfFile and t.kind != CloseRound and t.kind != CloseSquare and t.kind != CloseCurly and !spelling(ctx, s, ",")
		add_node(ctx, at_pos(ctx, bad, bad.pos + if advance 1 else 0), ErrorNode, t.start, t.end, [])
	}
}

type_collection : Ctx, State, RocSyntax.TokenKind, TypeMode, U64 -> Collection
type_collection = |ctx, initial, closing, mode, depth| {
	var $s = initial
	var $ids = []
	var $end = peek(ctx, initial).start
	var $comma = False
	var $done = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == closing {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else {
			before = look.pos
			item_state = type_expression(ctx, look, mode, depth + 1)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$s = skip_trivia(ctx, item_state)
			$end = node_end(ctx, $s, item_node)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
				$comma = True
			} else if peek(ctx, $s).kind != closing {
				$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
				$done = True
			}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: $comma }
}

type_parenthesized : Ctx, State, U64 -> Built
type_parenthesized = |ctx, initial, depth| {
	opening = peek(ctx, initial)
	var $s = skip_trivia(ctx, at_pos(ctx, initial, initial.pos + 1))
	var $ids = []
	var $comma = False
	var $done = False
	while !$done {
		if spelling(ctx, $s, "->") or spelling(ctx, $s, "=>") {
			ret_state = type_expression(ctx, at_pos(ctx, $s, $s.pos + 1), TypeArgument, depth + 1)
			ret_node = ret_state.last
			fn_state = add_node(ctx, ret_state, TypeFunction, opening.start, node_end(ctx, ret_state, ret_node), $ids.append(ret_node))
			fn_node = fn_state.last
			$s = skip_trivia(ctx, fn_state)
			$ids = [fn_node]
			$comma = False
			$done = True
		} else if peek(ctx, $s).kind == CloseRound or peek(ctx, $s).kind == EndOfFile {
			$done = True
		} else {
			before = $s.pos
			item_state = type_expression(ctx, $s, TypeArgument, depth + 1)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$s = skip_trivia(ctx, item_state)
			if spelling(ctx, $s, ",") {
				$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				$comma = True
			} else if !spelling(ctx, $s, "->") and !spelling(ctx, $s, "=>") {
				$done = True
			}
			if $s.pos <= before {
				$done = True
			}
		}
	}
	closing = peek(ctx, $s)
	if closing.kind == CloseRound {
		$s = at_pos(ctx, $s, $s.pos + 1)
	} else {
		$s = issue(ctx, $s, UnclosedDelimiter, closing.start, closing.end)
	}
	end = U64.max(if closing.kind == CloseRound closing.end else closing.start, node_end(ctx, $s, $ids.last() ?? node_count(ctx, $s)))
	if $ids.is_empty() and (spelling(ctx, skip_trivia(ctx, $s), "->") or spelling(ctx, skip_trivia(ctx, $s), "=>")) {
		arrow = skip_trivia(ctx, $s)
		ret_state = type_expression(ctx, at_pos(ctx, arrow, arrow.pos + 1), TypeArgument, depth + 1)
		ret_node = ret_state.last
		add_node(ctx, ret_state, TypeFunction, opening.start, node_end(ctx, ret_state, ret_node), [ret_node])
	} else {
		add_node(ctx, $s, if $ids.len() == 1 and !$comma TypeParenthesized else TypeTuple, opening.start, end, $ids)
	}
}

type_fields : Ctx, State, Bool, U64 -> Built
type_fields = |ctx, initial, record, depth| {
	opening = peek(ctx, initial)
	closing = if record CloseCurly else CloseSquare
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = opening.end
	var $done = False
	var $extension = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == closing {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else if $extension {
			$s = issue(ctx, look, UnexpectedToken, t.start, t.end)
			$done = True
		} else if spelling(ctx, look, "..") {
			after = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
			if peek(ctx, after).kind == LowerIdent and !spelling(ctx, after, "_") {
				ext_state = type_expression(ctx, after, TypeArgument, depth + 1)
				ext_node = ext_state.last
				built_state = add_node(ctx, ext_state, TypeExtension, t.start, node_end(ctx, ext_state, ext_node), [ext_node])
				built_node = built_state.last
				$s = skip_trivia(ctx, built_state)
				$ids = $ids.append(built_node)
			} else {
				ext_state = add_node(ctx, after, TypeExtension, t.start, t.end, [])
				ext_node = ext_state.last
				$s = skip_trivia(ctx, ext_state)
				$ids = $ids.append(ext_node)
			}
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
			$end = node_end(ctx, $s, $ids.last() ?? 0)
			$extension = True
		} else {
			before = look.pos
			if record {
				validated = if t.kind == LowerIdent look else issue(ctx, look, UnexpectedToken, t.start, t.end)
				name_state = add_node(ctx, at_pos(ctx, validated, validated.pos + 1), Identifier, t.start, t.end, [])
				name_node = name_state.last
				var $field = skip_trivia(ctx, name_state)
				if spelling(ctx, $field, "?") {
					if byte(ctx.source, t.start) == '_' {
						$field = issue(ctx, $field, UnexpectedToken, t.start, t.end)
					}
					$field = skip_trivia(ctx, at_pos(ctx, $field, $field.pos + 1))
				}
				if spelling(ctx, $field, ":") {
					$field = at_pos(ctx, $field, $field.pos + 1)
				}
					else {
						$field = issue(ctx, $field, UnexpectedToken, peek(ctx, $field).start, peek(ctx, $field).end)
					}
				ty_state = type_expression(ctx, $field, FullType, depth + 1)
				ty_node = ty_state.last
				var $field_ids = [name_node, ty_node]
				$field = skip_trivia(ctx, ty_state)
				var $field_end = node_end(ctx, ty_state, ty_node)
				if spelling(ctx, $field, "??") {
					default_state = expression(ctx, at_pos(ctx, $field, $field.pos + 1), 0, depth + 1, False)
					default_node = default_state.last
					$field_ids = $field_ids.append(default_node)
					$field_end = node_end(ctx, default_state, default_node)
					$field = default_state
				}
				field_state = add_node(ctx, $field, TypeField, t.start, $field_end, $field_ids)
				field_node = field_state.last
				$s = skip_trivia(ctx, field_state)
				$ids = $ids.append(field_node)
			} else {
				item_state = type_expression(ctx, look, TypeItem, depth + 1)
				item_node = item_state.last
				$s = skip_trivia(ctx, item_state)
				$ids = $ids.append(item_node)
			}
			$end = node_end(ctx, $s, $ids.last() ?? 0)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}

			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, if record TypeRecord else TypeTagUnion, opening.start, $end, $ids)
}

parse_annotation_after_name : Ctx, State, U64, U64, U64 -> Built
parse_annotation_after_name = |ctx, initial, name, start, depth| {
	ty_state = type_expression(ctx, initial, FullType, depth + 1)
	ty_node = ty_state.last
	look = skip_trivia(ctx, ty_state)
	if spelling(ctx, look, "where") {
		where_state = parse_where_clause(ctx, look, depth + 1)
		where_node = where_state.last
		add_node(ctx, where_state, TypeAnnotation, start, node_end(ctx, where_state, where_node), [name, ty_node, where_node])
	} else {
		add_node(ctx, ty_state, TypeAnnotation, start, node_end(ctx, ty_state, ty_node), [name, ty_node])
	}
}

parse_where_clause : Ctx, State, U64 -> Built
parse_where_clause = |ctx, initial, depth| {
	start = peek(ctx, initial).start
	look = skip_trivia(ctx, at_pos(ctx, initial, initial.pos + 1))
	opening = peek(ctx, look)
	var $s = if opening.kind == OpenSquare at_pos(ctx, look, look.pos + 1) else issue(ctx, look, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $done = opening.kind != OpenSquare or depth >= 128
	var $end = opening.end
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, start, opening.end)
	}
	while !$done {
		receiver_state = skip_trivia(ctx, $s)
		$s = vacant
		receiver = peek(ctx, receiver_state)
		if receiver.kind == CloseSquare {
			$s = at_pos(ctx, receiver_state, receiver_state.pos + 1)
			$end = receiver.end
			$done = True
		} else if receiver.kind == EndOfFile or receiver.kind == CloseCurly or receiver.kind == CloseRound {
			$s = issue(ctx, receiver_state, UnclosedDelimiter, receiver.start, receiver.end)
			$end = receiver.start
			$done = True
		} else {
			before = receiver_state.pos
			valid_receiver = receiver.kind == LowerIdent and byte(ctx.source, receiver.start) != '_'
			validated = if valid_receiver receiver_state else issue(ctx, receiver_state, UnexpectedToken, receiver.start, receiver.end)
			var_state = add_node(ctx, at_pos(ctx, validated, validated.pos + 1), TypeVariable, receiver.start, receiver.end, [])
			var_node = var_state.last
			dot = skip_trivia(ctx, var_state)
			name_state = if spelling(ctx, dot, ".") at_pos(ctx, dot, dot.pos + 1) else issue(ctx, dot, UnexpectedToken, peek(ctx, dot).start, peek(ctx, dot).end)
			name = peek(ctx, name_state)
			if name.kind == LowerIdent and name.start == peek(ctx, dot).end and byte(ctx.source, name.start) != '_' {
				method_state = add_node(ctx, at_pos(ctx, name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
				method_node = method_state.last
				colon = skip_trivia(ctx, method_state)
				after_colon = if spelling(ctx, colon, ":") at_pos(ctx, colon, colon.pos + 1) else issue(ctx, colon, UnexpectedToken, peek(ctx, colon).start, peek(ctx, colon).end)
				ty_state = type_expression(ctx, after_colon, FullType, depth + 1)
				ty_node = ty_state.last
				clause_state = add_node(ctx, ty_state, WhereMethod, receiver.start, node_end(ctx, ty_state, ty_node), [var_node, method_node, ty_node])
				clause_node = clause_state.last
				$s = skip_trivia(ctx, clause_state)
				$ids = $ids.append(clause_node)
			} else if name.kind == UpperIdent and name.start == peek(ctx, dot).end and byte(ctx.source, name.start) != '@' {
				alias_state = type_where_alias_name(ctx, name_state, depth + 1)
				alias_node = alias_state.last
				clause_state = add_node(ctx, alias_state, WhereAlias, receiver.start, node_end(ctx, alias_state, alias_node), [var_node, alias_node])
				clause_node = clause_state.last
				$s = skip_trivia(ctx, clause_state)
				$ids = $ids.append(clause_node)
			} else {
				$s = issue(ctx, name_state, UnexpectedToken, name.start, name.end)
				$done = True
			}
			if !$ids.is_empty() {
				$end = node_end(ctx, $s, $ids.last() ?? 0)
			}
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
				else if peek(ctx, $s).kind != CloseSquare {
					$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					$done = True
				}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	if $ids.is_empty() {
		$s = issue(ctx, $s, UnexpectedToken, start, $end)
	}
	add_node(ctx, $s, WhereClause, start, $end, $ids)
}

parse_type_declaration : Ctx, State, U64 -> Built
parse_type_declaration = |ctx, initial, depth| {
	s = skip_trivia(ctx, initial)
	first = peek(ctx, s)
	where_alias = first.kind == LowerIdent
	var $s = s
	var $receiver = []
	if where_alias {
		if byte(ctx.source, first.start) == '_' {
			$s = issue(ctx, $s, UnexpectedToken, first.start, first.end)
		}
		receiver_state = add_node(ctx, at_pos(ctx, $s, $s.pos + 1), TypeVariable, first.start, first.end, [])
		receiver_node = receiver_state.last
		$receiver = [receiver_node]
		dot = skip_trivia(ctx, receiver_state)
		$s = if spelling(ctx, dot, ".") at_pos(ctx, dot, dot.pos + 1) else issue(ctx, dot, UnexpectedToken, peek(ctx, dot).start, peek(ctx, dot).end)
	}
	name = peek(ctx, $s)
	validated = if name.kind == UpperIdent and byte(ctx.source, name.start) != '@' $s else issue(ctx, $s, UnexpectedToken, name.start, name.end)
	named_state = add_node(ctx, at_pos(ctx, validated, validated.pos + if name.kind == EndOfFile 0 else 1), TypeName, name.start, name.end, [])
	name_node = named_state.last
	$s = skip_trivia(ctx, named_state)
	var $params = [name_node]
	var $header_end = name.end
	if peek(ctx, $s).kind == OpenRound or peek(ctx, $s).kind == NoSpaceOpenRound {
		$s = at_pos(ctx, $s, $s.pos + 1)
		var $done = False
		while !$done {
			look = skip_trivia(ctx, $s)
			$s = vacant
			p = peek(ctx, look)
			if p.kind == CloseRound {
				$s = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
				$header_end = p.end
				$done = True
			} else if p.kind == EndOfFile or p.kind == CloseCurly or p.kind == CloseSquare {
				$s = issue(ctx, look, UnclosedDelimiter, p.start, p.end)
				$done = True
			} else {
				validated_arg = if p.kind == LowerIdent look else issue(ctx, look, UnexpectedToken, p.start, p.end)
				param_state = add_node(ctx, at_pos(ctx, validated_arg, validated_arg.pos + 1), if token_spelling(ctx.source, p, "_") TypeWildcard else TypeVariable, p.start, p.end, [])
				param_node = param_state.last
				$params = $params.append(param_node)
				$header_end = p.end
				$s = skip_trivia(ctx, param_state)
				if spelling(ctx, $s, ",") {
					$s = at_pos(ctx, $s, $s.pos + 1)
				}
					else if peek(ctx, $s).kind != CloseRound {
						$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
						$done = True
					}
			}
		}
	}
	header_state = add_node(ctx, $s, TypeHeader, name.start, $header_end, $params)
	header_node = header_state.last
	operator_state = skip_trivia(ctx, header_state)
	nominal = spelling(ctx, operator_state, ":=") or spelling(ctx, operator_state, "::")
	valid_operator = spelling(ctx, operator_state, ":") or (!where_alias and nominal)
	checked = if valid_operator operator_state else issue(ctx, operator_state, UnexpectedToken, peek(ctx, operator_state).start, peek(ctx, operator_state).end)
	after_operator = skip_trivia(ctx, at_pos(ctx, checked, checked.pos + if valid_operator 1 else 0))
	var $ids = [header_node].concat($receiver)
	if where_alias {
		if spelling(ctx, after_operator, "where") {
			where_state = parse_where_clause(ctx, after_operator, depth + 1)
			where_node = where_state.last
			$s = where_state
			$ids = $ids.append(where_node)
		} else {
			$s = issue(ctx, after_operator, UnexpectedToken, peek(ctx, after_operator).start, peek(ctx, after_operator).end)
		}
	} else {
		ty_state = type_expression(ctx, after_operator, FullType, depth + 1)
		ty_node = ty_state.last
		$s = skip_trivia(ctx, ty_state)
		$ids = $ids.append(ty_node)
		if spelling(ctx, $s, "where") {
			where_state = parse_where_clause(ctx, $s, depth + 1)
			where_node = where_state.last
			$s = skip_trivia(ctx, where_state)
			$ids = $ids.append(where_node)
		}
	}
	look = skip_trivia(ctx, $s)
	if spelling(ctx, look, ".") and peek(ctx, skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))).kind == OpenCurly {
		associated_state = if nominal and !where_alias look else issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end)
		associated_end = parse_associated_block(ctx, skip_trivia(ctx, at_pos(ctx, associated_state, associated_state.pos + 1)), depth + 1)
		associated_node = associated_end.last
		$s = associated_end
		$ids = $ids.append(associated_node)
	}
	end = $ids.fold(first.end, |furthest, id| U64.max(furthest, node_end(ctx, $s, id)))
	add_node(ctx, $s, TypeDeclaration, first.start, end, $ids)
}

parse_associated_block : Ctx, State, U64 -> Built
parse_associated_block = |ctx, initial, depth| {
	opening = peek(ctx, initial)
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = opening.end
	var $done = depth >= 128
	if $done {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else {
			before = look.pos
			item_state = statement(ctx, if spelling(ctx, look, "var") issue(ctx, look, UnexpectedToken, t.start, t.end) else look, depth + 1)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$s = item_state
			if (t.kind == UpperIdent or peek(ctx, skip_trivia(ctx, item_state)).kind == CloseCurly) and (node_get(ctx, item_state, item_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).kind == ExpressionStatement {
				$s = issue(ctx, $s, UnexpectedToken, t.start, node_end(ctx, $s, item_node))
			}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, AssociatedBlock, opening.start, $end, $ids)
}

# Patterns have their own grammar: expression operators and value-only postfixes
# must never make an invalid binding pattern appear complete.
parse_pattern : Ctx, State, Bool, U64 -> Built
parse_pattern = |ctx, initial, alternatives, depth| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	if depth >= 128 {
		bad = issue(ctx, s, ResourceLimit, t.start, t.end)
		add_node(ctx, bad, ErrorNode, t.start, t.start, [])
	} else {
		first_state = pattern_primary(ctx, s, alternatives, depth + 1)
		first_node = first_state.last
		var $s = skip_trivia(ctx, first_state)
		var $node = first_node
		if alternatives and spelling(ctx, $s, "|") {
			var $ids = [first_node]
			while spelling(ctx, $s, "|") {
				next_state = pattern_primary(ctx, skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1)), alternatives, depth + 1)
				next_node = next_state.last
				$ids = $ids.append(next_node)
				$s = skip_trivia(ctx, next_state)
			}
			end = node_end(ctx, $s, $ids.last() ?? first_node)
			alt_state = add_node(ctx, $s, AlternativePattern, t.start, end, $ids)
			alt_node = alt_state.last
			$s = alt_state
			$node = alt_node
		} else if spelling(ctx, $s, "as") {
			name_state = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
			name = peek(ctx, name_state)
			if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
				named_state = add_node(ctx, at_pos(ctx, name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
				named_node = named_state.last
				alias_state = add_node(ctx, named_state, AliasPattern, t.start, name.end, [first_node, named_node])
				alias_node = alias_state.last
				$s = alias_state
				$node = alias_node
			} else {
				$s = issue(ctx, name_state, UnexpectedToken, name.start, name.end)
			}
		}
		with_last(ctx, $s, $node)
	}
}

pattern_primary : Ctx, State, Bool, U64 -> Built
pattern_primary = |ctx, s, alternatives, depth| {
	t = peek(ctx, s)
	if t.kind == LowerIdent {
		add_node(ctx, at_pos(ctx, s, s.pos + 1), if spelling(ctx, s, "_") WildcardPattern else IdentifierPattern, t.start, t.end, [])
	} else if spelling(ctx, s, "var") {
		look = skip_trivia(ctx, at_pos(ctx, s, s.pos + 1))
		name = peek(ctx, look)
		if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
			name_state = add_node(ctx, at_pos(ctx, look, look.pos + 1), Identifier, name.start, name.end, [])
			name_node = name_state.last
			add_node(ctx, name_state, MutablePattern, t.start, name.end, [name_node])
		} else {
			bad = issue(ctx, look, UnexpectedToken, name.start, name.end)
			add_node(ctx, bad, ErrorNode, t.start, name.start, [])
		}
	} else if spelling(ctx, s, "..") {
		look = skip_trivia(ctx, at_pos(ctx, s, s.pos + 1))
		if spelling(ctx, look, "as") {
			name_state = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
			name = peek(ctx, name_state)
			if name.kind == LowerIdent and !spelling(ctx, name_state, "_") {
				named_state = add_node(ctx, at_pos(ctx, name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
				named_node = named_state.last
				add_node(ctx, named_state, RestPattern, t.start, name.end, [named_node])
			} else {
				bad = issue(ctx, name_state, UnexpectedToken, name.start, name.end)
				add_node(ctx, bad, RestPattern, t.start, t.end, [])
			}
		} else {
			validated = if peek(ctx, look).kind == LowerIdent and !spelling(ctx, look, "_") issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end) else look
			add_node(ctx, validated, RestPattern, t.start, t.end, [])
		}
	} else if t.kind == UpperIdent and byte(ctx.source, t.start) == '@' {
		bad = issue(ctx, at_pos(ctx, s, s.pos + 1), UnexpectedToken, t.start, t.end)
		add_node(ctx, bad, ErrorNode, t.start, t.end, [])
	} else if t.kind == UpperIdent {
		name_state = add_node(ctx, at_pos(ctx, s, s.pos + 1), Identifier, t.start, t.end, [])
		name_node = name_state.last
		var $s = name_state
		var $name = name_node
		var $done = False
		while !$done {
			look = skip_trivia(ctx, $s)
			if spelling(ctx, look, ".") and peek(ctx, look).start == node_end(ctx, look, $name) and peek(ctx, at_pos(ctx, look, look.pos + 1)).kind == UpperIdent {
				field_state = at_pos(ctx, look, look.pos + 1)
				field = peek(ctx, field_state)
				field_built = add_node(ctx, at_pos(ctx, field_state, field_state.pos + 1), Identifier, field.start, field.end, [])
				field_node = field_built.last
				access_state = add_node(ctx, field_built, FieldAccess, t.start, field.end, [$name, field_node])
				access_node = access_state.last
				$s = access_state
				$name = access_node
			} else {
				$s = look
				$done = True
			}
		}
		var $nominal = False
		if spelling(ctx, $s, ".") {
			$nominal = True
			$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		}
		if peek(ctx, $s).kind == NoSpaceOpenRound {
			{ state: args_state, ids: args, end, comma: _ } = pattern_collection(ctx, at_pos(ctx, $s, $s.pos + 1), CloseRound, True, depth)
			add_node(ctx, args_state, if $nominal NominalPattern else TagPattern, t.start, end, [$name].concat(args))
		} else if $nominal and peek(ctx, $s).kind == OpenCurly {
			record_state = pattern_record(ctx, $s, alternatives, depth)
			record_node = record_state.last
			end = node_end(ctx, record_state, record_node)
			add_node(ctx, record_state, NominalPattern, t.start, end, [$name, record_node])
		} else {
			validated = if $nominal issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end) else $s
			add_node(ctx, validated, TagPattern, t.start, node_end(ctx, validated, $name), [$name])
		}
	} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare {
		closing = if t.kind == OpenSquare CloseSquare else CloseRound
		{ state: items_state, ids, end, comma: _ } = pattern_collection(ctx, at_pos(ctx, s, s.pos + 1), closing, True, depth)
		add_node(ctx, items_state, if t.kind == OpenSquare ListPattern else TuplePattern, t.start, end, ids)
	} else if t.kind == OpenCurly {
		pattern_record(ctx, s, alternatives, depth)
	} else if t.kind == StringStart {
		pattern_string(ctx, s)
	} else if t.kind == Number or t.kind == SingleQuote {
		primary(ctx, s, depth, False)
	} else {
		bad = issue(ctx, s, UnexpectedToken, t.start, t.end)
		add_node(ctx, at_pos(ctx, bad, bad.pos + if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly 0 else 1), ErrorNode, t.start, t.end, [])
	}
}

pattern_collection : Ctx, State, RocSyntax.TokenKind, Bool, U64 -> Collection
pattern_collection = |ctx, initial, closing, alternatives, depth| {
	var $s = initial
	var $ids = []
	var $end = peek(ctx, initial).start
	var $done = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == closing {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else {
			item_state = parse_pattern(ctx, look, alternatives, depth + 1)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$end = node_end(ctx, item_state, item_node)
			$s = skip_trivia(ctx, item_state)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			} else if peek(ctx, $s).kind != closing and peek(ctx, $s).kind != EndOfFile {
				$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
			}
			if $s.pos <= look.pos {
				$s = at_pos(ctx, $s, look.pos + 1)
			}
		}
	}
	{ state: $s, ids: $ids, end: $end, comma: False }
}

pattern_record : Ctx, State, Bool, U64 -> Built
pattern_record = |ctx, initial, alternatives, depth| {
	start = peek(ctx, initial).start
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = peek(ctx, initial).end
	var $done = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = if t.start > $end t.start else $end
			$done = True
		} else {
			field_state = if spelling(ctx, look, "..") {
				name_state = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
				name = peek(ctx, name_state)
				if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
					named_state = add_node(ctx, at_pos(ctx, name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
					named_node = named_state.last
					add_node(ctx, named_state, RestPattern, t.start, name.end, [named_node])
				} else {
					add_node(ctx, name_state, RestPattern, t.start, t.end, [])
				}
			} else if t.kind == LowerIdent and (is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$') {
				name_state = add_node(ctx, at_pos(ctx, look, look.pos + 1), Identifier, t.start, t.end, [])
				name_node = name_state.last
				after = skip_trivia(ctx, name_state)
				if spelling(ctx, after, ":") {
					value_state = parse_pattern(ctx, at_pos(ctx, after, after.pos + 1), alternatives, depth + 1)
					value_node = value_state.last
					end = node_end(ctx, value_state, value_node)
					add_node(ctx, value_state, PatternField, t.start, end, [name_node, value_node])
				} else {
					validated = if spelling(ctx, after, ",") or peek(ctx, after).kind == CloseCurly after else issue(ctx, after, UnexpectedToken, peek(ctx, after).start, peek(ctx, after).end)
					add_node(ctx, validated, PatternField, t.start, t.end, [name_node])
				}
			} else {
				bad = issue(ctx, at_pos(ctx, look, look.pos + 1), UnexpectedToken, t.start, t.end)
				add_node(ctx, bad, ErrorNode, t.start, t.end, [])
			}
			field_node = field_state.last
			$ids = $ids.append(field_node)
			$end = node_end(ctx, field_state, field_node)
			$s = skip_trivia(ctx, field_state)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
			if $s.pos <= look.pos {
				$s = at_pos(ctx, $s, look.pos + 1)
			}
		}
	}
	add_node(ctx, $s, RecordPattern, start, $end, $ids)
}

parse_lambda : Ctx, State, U64, U64 -> Built
parse_lambda = |ctx, initial, start, depth| {
	var $s = initial
	var $ids = []
	var $done = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if spelling(ctx, look, "|") {
			$s = at_pos(ctx, look, look.pos + 1)
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = look.pos
			param_state = parse_pattern(ctx, look, False, depth + 1)
			param_node = param_state.last
			$ids = $ids.append(param_node)
			$s = skip_trivia(ctx, param_state)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
			if $s.pos <= before {
				$s = issue(ctx, at_pos(ctx, $s, before + 1), UnexpectedToken, t.start, t.end)
			}
		}
	}
	body_state = expression(ctx, $s, 0, depth, False)
	body_node = body_state.last
	end = node_end(ctx, body_state, body_node)
	add_node(ctx, body_state, Lambda, start, end, $ids.append(body_node))
}

parse_if : Ctx, State, U64, U64 -> Built
parse_if = |ctx, initial, start, depth| {
	condition_state = expression(ctx, initial, 0, depth, False)
	condition_node = condition_state.last
	look = skip_trivia(ctx, condition_state)
	then_branch_state = expression(ctx, look, 0, depth, False)
	then_branch_node = then_branch_state.last
	after = skip_trivia(ctx, then_branch_state)
	if spelling(ctx, after, "else") {
		other_state = expression(ctx, at_pos(ctx, after, after.pos + 1), 0, depth, False)
		other_node = other_state.last
		end = node_end(ctx, other_state, other_node)
		add_node(ctx, other_state, If, start, end, [condition_node, then_branch_node, other_node])
	} else {
		end = node_end(ctx, after, then_branch_node)
		add_node(ctx, after, If, start, end, [condition_node, then_branch_node])
	}
}

curly : Ctx, State, U64, U64, Bool, Bool -> Built
curly = |ctx, initial, start, depth, punned, forced| {
	look = skip_trivia(ctx, initial)
	first = peek(ctx, look)
	after = skip_trivia(ctx, at_pos(ctx, look, look.pos + 1))
	record = forced or first.kind == CloseCurly or spelling(ctx, look, "..") or (first.kind == LowerIdent and (spelling(ctx, after, ":") or spelling(ctx, after, ",") or (punned and peek(ctx, after).kind == CloseCurly)) and !(spelling(ctx, after, ":") and record_starts_block(ctx, look)))
	var $s = initial
	var $ids = []
	var $end = first.start
	var $done = False
	var $extension = False
	if spelling(ctx, look, "..") {
		base_state = expression(ctx, at_pos(ctx, look, look.pos + 1), 0, depth + 1, False)
		base_node = base_state.last
		comma = skip_trivia(ctx, base_state)
		$s = if spelling(ctx, comma, ",") at_pos(ctx, comma, comma.pos + 1) else issue(ctx, comma, UnexpectedToken, peek(ctx, comma).start, peek(ctx, comma).end)
		$ids = [base_node]
		$end = node_end(ctx, $s, base_node)
		$extension = True
	}
	while !$done {
		$s = skip_trivia(ctx, $s)
		t = peek(ctx, $s)
		sync = declaration_sync(ctx, $s)
		if record and sync.sync {
			$s = issue(ctx, $s, UnclosedDelimiter, sync.start, sync.end)
			$end = sync.end
			$done = True
		} else if t.kind == CloseCurly {
			$end = t.end
			$s = at_pos(ctx, $s, $s.pos + 1)
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$end = if t.start > $end t.start else $end
			$s = issue(ctx, $s, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = $s.pos
			if record {
				name_state = add_node(ctx, at_pos(ctx, $s, $s.pos + 1), Identifier, t.start, t.end, [])
				name_node = name_state.last
				var $field_state = name_state
				var $field_node = name_node
				field_look = skip_trivia(ctx, name_state)
				if t.kind != LowerIdent or !(is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$') {
					bad = issue(ctx, field_look, UnsupportedSyntax, t.start, t.end)
					next_field_state = add_node(ctx, bad, UnsupportedNode, t.start, t.end, [])
					next_field_node = next_field_state.last
					$field_state = next_field_state
					$field_node = next_field_node
				} else if spelling(ctx, field_look, ":") and token_spelling(ctx.source, source_token(ctx, field_look, significant_position(ctx.tokens, field_look.pos + 1)), "_") and (token_spelling(ctx.source, source_token(ctx, field_look, significant_position(ctx.tokens, significant_position(ctx.tokens, field_look.pos + 1) + 1)), ",") or source_token(ctx, field_look, significant_position(ctx.tokens, significant_position(ctx.tokens, field_look.pos + 1) + 1)).kind == CloseCurly) {
					value_pos = significant_position(ctx.tokens, field_look.pos + 1)
					unset_end = source_token(ctx, field_look, value_pos).end
					unset_state = add_node(ctx, at_pos(ctx, field_look, value_pos + 1), UnsetField, t.start, unset_end, [name_node])
					unset_node = unset_state.last
					$field_state = unset_state
					$field_node = unset_node
				} else if spelling(ctx, field_look, ":") {
					value_state = expression(ctx, at_pos(ctx, field_look, field_look.pos + 1), 0, depth, True)
					value_node = value_state.last
					end = node_end(ctx, value_state, value_node)
					next_field_state = add_node(ctx, value_state, RecordField, t.start, end, [name_node, value_node])
					next_field_node = next_field_state.last
					$field_state = next_field_state
					$field_node = next_field_node
				} else {
					next_field_state = add_node(ctx, field_look, RecordField, t.start, t.end, [name_node])
					next_field_node = next_field_state.last
					$field_state = next_field_state
					$field_node = next_field_node
				}
				$ids = $ids.append($field_node)
				$end = node_end(ctx, $field_state, $field_node)
				$s = skip_trivia(ctx, $field_state)
				if spelling(ctx, $s, ",") {
					$s = at_pos(ctx, $s, $s.pos + 1)
				}
					else if peek(ctx, $s).kind != CloseCurly and peek(ctx, $s).kind != EndOfFile and (peek(ctx, $s).kind != LowerIdent or (node_get(ctx, $s, $field_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }).children_len != 2) {
						$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					}
			} else {
				item_state = statement(ctx, $s, depth)
				item_node = item_state.last
				$ids = $ids.append(item_node)
				$end = node_end(ctx, item_state, item_node)
				$s = item_state
			}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	if record and !$extension and !forced {
		look_suffix = skip_trivia(ctx, $s)
		dot = peek(ctx, look_suffix)
		name_state = at_pos(ctx, look_suffix, look_suffix.pos + 1)
		if spelling(ctx, look_suffix, ".") and dot.start == $end and peek(ctx, name_state).kind == UpperIdent {
			var $mapper_ids = []
			var $mapper_state = name_state
			var $mapper_end = peek(ctx, name_state).end
			var $mapper_done = False
			while !$mapper_done {
				t = peek(ctx, $mapper_state)
				named_state = add_node(ctx, at_pos(ctx, $mapper_state, $mapper_state.pos + 1), Identifier, t.start, t.end, [])
				named_node = named_state.last
				$mapper_state = named_state
				$mapper_ids = $mapper_ids.append(named_node)
				$mapper_end = t.end
				if spelling(ctx, $mapper_state, ".") and peek(ctx, $mapper_state).start == $mapper_end and peek(ctx, at_pos(ctx, $mapper_state, $mapper_state.pos + 1)).kind == UpperIdent {
					$mapper_state = at_pos(ctx, $mapper_state, $mapper_state.pos + 1)
				} else {
					$mapper_done = True
				}
			}
			# Keep the mapper as a normal qualified identifier tree.
			var $mapper = $mapper_ids.first() ?? 0
			var $index = 1.U64
			while $index < $mapper_ids.len() {
				field = $mapper_ids.get($index) ?? $mapper
				access_state = add_node(ctx, $mapper_state, FieldAccess, node_start(ctx, $mapper_state, $mapper), node_end(ctx, $mapper_state, field), [$mapper, field])
				access_node = access_state.last
				$mapper_state = access_state
				$mapper = access_node
				$index = $index + 1
			}
			return add_node(ctx, $mapper_state, RecordBuilder, start, $mapper_end, [$mapper].concat($ids))
		}
	}
	add_node(ctx, $s, if $extension RecordUpdate else if record Record else Block, start, $end, $ids)
}

parse_match : Ctx, State, U64, U64 -> Built
parse_match = |ctx, initial, start, depth| {
	value_state = expression(ctx, initial, 0, depth, False)
	value_node = value_state.last
	look = skip_trivia(ctx, value_state)
	if peek(ctx, look).kind != OpenCurly {
		bad = issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end)
		add_node(ctx, bad, Match, start, node_end(ctx, bad, value_node), [value_node])
	} else {
		var $s = at_pos(ctx, look, look.pos + 1)
		var $ids = [value_node]
		var $end = peek(ctx, look).end
		var $done = False
		while !$done {
			$s = skip_trivia(ctx, $s)
			t = peek(ctx, $s)
			if t.kind == CloseCurly {
				$end = t.end
				$s = at_pos(ctx, $s, $s.pos + 1)
				$done = True
			} else if t.kind == EndOfFile {
				$s = issue(ctx, $s, UnclosedDelimiter, t.start, t.end)
				$end = if t.start > $end t.start else $end
				$done = True
			} else {
				before = $s.pos
				pattern_state = parse_pattern(ctx, $s, True, depth + 1)
				pattern_node = pattern_state.last
				var $arrow = skip_trivia(ctx, pattern_state)
				var $branch_ids = [pattern_node]
				if spelling(ctx, $arrow, "if") {
					outer_guard = $arrow.in_guard
					guard_start = at_pos(ctx, { ..$arrow, in_guard: True }, $arrow.pos + 1)
					guard_state = expression(ctx, guard_start, 0, depth + 1, False)
					guard_node = guard_state.last
					$arrow = skip_trivia(ctx, { ..guard_state, in_guard: outer_guard })
					$branch_ids = $branch_ids.append(guard_node)
				}
				arrow = $arrow
				if spelling(ctx, arrow, "=>") {
					branch_value_state = expression(ctx, at_pos(ctx, arrow, arrow.pos + 1), 0, depth, False)
					branch_value_node = branch_value_state.last
					branch_end = node_end(ctx, branch_value_state, branch_value_node)
					branch_state = add_node(ctx, branch_value_state, MatchBranch, t.start, branch_end, $branch_ids.append(branch_value_node))
					branch_node = branch_state.last
					$ids = $ids.append(branch_node)
					$s = skip_trivia(ctx, branch_state)
					$end = branch_end
					if spelling(ctx, $s, ",") {
						$s = at_pos(ctx, $s, $s.pos + 1)
					}
				} else {
					$s = issue(ctx, arrow, UnsupportedSyntax, t.start, peek(ctx, arrow).end)
					$ids = $ids.append(pattern_node)
				}
				if $s.pos <= before {
					$s = at_pos(ctx, $s, before + 1)
				}
			}
		}
		if $ids.len() == 1 {
			$s = issue(ctx, $s, UnexpectedToken, start, $end)
		}
		add_node(ctx, $s, Match, start, $end, $ids)
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
	kind == LowerIdent or kind == UpperIdent or kind == Number or kind == StringEnd or kind == SingleQuote or kind == CloseRound or kind == CloseSquare or kind == CloseCurly or kind == InterpolationEnd
}

can_end_token : List(U8), RocSyntax.Token -> Bool
can_end_token = |source, token| can_end_expression(token.kind) or token_spelling(source, token, "...")

binary_minus : Ctx, State -> Bool
binary_minus = |ctx, s| {
	t = peek(ctx, s)
	next = byte(ctx.source, t.end)
	previous = ctx.tokens.get(if s.pos > 0 s.pos - 1 else 0) ?? t
	if matches(ctx.source, t.end, "...") {
		return previous.end == t.start and can_end_token(ctx.source, previous)
	}
	is_space(next) or next == '#' or (is_digit(next) and previous.end == t.start and can_end_token(ctx.source, previous)) or (!is_digit(next) and !is_lower(next) and !is_upper(next) and next != '_' and next != '(' and next < 0x80)
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

expect {
	p = RocSyntax.parse_expr_str("result?")
	p.status == Complete and p.nodes.any(|n| n.kind == Suffix)
}

expect {
	RocSyntax.parse_expr_str("- value").status == Recovered and RocSyntax.parse_str("x : 42").status == Recovered
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
vacant = State.{ pos: 0, nodes: RocSyntaxBuffer.empty({}), children: RocSyntaxBuffer.empty({}), node_tail: [], child_tail: [], diagnostics: [], unsupported: False, limited: False, in_guard: False, last: 0 }

expect {
	["-!h", "-'i'", "match 42 {}", "person.@"].all(|source| RocSyntax.parse_expr_str(source).status != Complete) and ["F\n", "modu\n", "S\n0\n", "x = _\n\ny = { a: _ + 1 }"].all(|source| RocSyntax.parse_str(source).status != Complete)
}

# Consume a state when advancing. Avoid a record spread retaining an alias to
# its growing buffers across a recursive parse call.
at_pos : Ctx, State, U64 -> State
at_pos = |_ctx, State.{ pos: _, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last }, position| {
	State.{ pos: position, nodes, children, node_tail, child_tail, diagnostics, unsupported, limited, in_guard, last }
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

source_token : Ctx, State, U64 -> RocSyntax.Token
source_token = |ctx, _s, pos| ctx.tokens.get(pos) ?? { kind: EndOfFile, start: ctx.source.len(), end: ctx.source.len() }

valid_import : Ctx, State, U64 -> Bool
valid_import = |ctx, s, id| {
	match node_get(ctx, s, id) {
		Ok(n) if n.kind == Identifier => is_upper(byte(ctx.source, n.start))
		Ok(n) if n.kind == FieldAccess => {
			last = child_get(ctx, s, n.children_start + 1) ?? node_count(ctx, s)
			match node_get(ctx, s, last) {
				Ok(field) => is_upper(byte(ctx.source, field.start))
				Err(_) => False
			}
		}
		_ => False
	}
}

valid_pattern : Ctx, State, U64 -> Bool
valid_pattern = |ctx, s, root| {
	var $work = [root]
	var $depth = 1
	var $valid = True
	while $depth > 0 and $valid {
		$depth = $depth - 1
		id = $work.get($depth) ?? node_count(ctx, s)
		match node_get(ctx, s, id) {
			Err(_) => {
				$valid = False
			}
			Ok(n) => {
				$valid = n.kind == Identifier or n.kind == NumberLiteral or n.kind == StringLiteral or n.kind == CharacterLiteral or n.kind == Call or n.kind == Tuple or n.kind == ListLiteral or n.kind == Parenthesized
				var $i = 0
				while $valid and $i < n.children_len {
					child = child_get(ctx, s, n.children_start + $i) ?? node_count(ctx, s)
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
is_optional_access : Ctx, State, U64 -> Bool
is_optional_access = |ctx, s, root| {
	match node_get(ctx, s, root) {
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
has_pattern_assignment : Ctx, State -> Bool
has_pattern_assignment = |ctx, initial| {
	var $s = initial
	first = peek(ctx, $s)
	if first.kind == UpperIdent {
		if byte(ctx.source, first.start) == '@' {
			return False
		}
		$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		var $name_end = first.end
		var $done = False
		while !$done and spelling(ctx, $s, ".") {
			field_state = at_pos(ctx, $s, $s.pos + 1)
			field = peek(ctx, field_state)
			if peek(ctx, $s).start == $name_end and field.kind == UpperIdent {
				$name_end = field.end
				$s = skip_trivia(ctx, at_pos(ctx, field_state, field_state.pos + 1))
			} else {
				$done = True
			}
		}
		if spelling(ctx, $s, ".") {
			$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		}
		if peek(ctx, $s).kind != NoSpaceOpenRound and peek(ctx, $s).kind != OpenCurly {
			return False
		}
	}
	opening = peek(ctx, $s)
	if opening.kind != OpenRound and opening.kind != NoSpaceOpenRound and opening.kind != OpenCurly {
		return False
	}
	var $pos = $s.pos + 1
	var $nesting = 1.U64
	while $nesting > 0 {
		t = source_token(ctx, $s, $pos)
		if t.kind == EndOfFile {
			return False
		}
		if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare or t.kind == OpenCurly {
			$nesting = $nesting + 1
		}
		if t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$nesting = $nesting - 1
		}
		$pos = $pos + 1
	}
	token_spelling(ctx.source, source_token(ctx, $s, significant_position(ctx.tokens, $pos)), "=")
}

parse_for : Ctx, State, U64, Bool, U64 -> Built
parse_for = |ctx, initial, start, stream, depth| {
	pattern_state = parse_pattern(ctx, initial, False, depth + 1)
	pattern_node = pattern_state.last
	look = skip_trivia(ctx, pattern_state)
	validated = if spelling(ctx, look, "in") at_pos(ctx, look, look.pos + 1) else issue(ctx, look, UnexpectedToken, peek(ctx, look).start, peek(ctx, look).end)
	list_state = expression(ctx, validated, 0, depth + 1, False)
	list_node = list_state.last
	body_state = expression(ctx, list_state, 0, depth + 1, False)
	body_node = body_state.last
	end = U64.max(node_end(ctx, body_state, body_node), U64.max(node_end(ctx, body_state, pattern_node), node_end(ctx, body_state, list_node)))
	add_node(ctx, body_state, if stream ForStream else For, start, end, [pattern_node, list_node, body_node])
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

parse_string : Ctx, State, U64 -> Built
parse_string = |ctx, initial, depth| {
	first = peek(ctx, initial)
	multiline = first.kind == MultilineStringStart
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = first.end
	var $done = False
	while !$done {
		look = if multiline skip_trivia(ctx, $s) else $s
		$s = vacant
		t = peek(ctx, look)
		if t.kind == StringPart {
			part_state = add_node(ctx, at_pos(ctx, look, look.pos + 1), StringSegment, t.start, t.end, [])
			part_node = part_state.last
			$s = part_state
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			value_state = expression(ctx, at_pos(ctx, look, look.pos + 1), 0, depth + 1, False)
			value_node = value_state.last
			after = skip_trivia(ctx, value_state)
			close = peek(ctx, after)
			closed = close.kind == InterpolationEnd
			validated = if closed at_pos(ctx, after, after.pos + 1) else issue(ctx, after, UnclosedDelimiter, close.start, close.end)
			end = if closed close.end else node_end(ctx, validated, value_node)
			interpolation_state = add_node(ctx, validated, Interpolation, t.start, end, [value_node])
			interpolation_node = interpolation_state.last
			$s = interpolation_state
			$ids = $ids.append(interpolation_node)
			$end = end
			if !closed {
				$done = True
			}
		} else if multiline and t.kind == MultilineStringStart {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
		} else if !multiline and t.kind == StringEnd {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else {
			$s = if multiline look else issue(ctx, look, UnclosedString, first.start, $end)
			$done = True
		}
	}
	literal_suffix(ctx, add_node(ctx, $s, if multiline MultilineStringLiteral else StringLiteral, first.start, $end, $ids), multiline)
}

# A literal's adjacent .Upper chain names its type, rather than a value field.
literal_suffix : Ctx, Built, Bool -> Built
literal_suffix = |ctx, built_state, allow_trivia| {
	built_node = built_state.last
	var $s = built_state
	var $type_ids = []
	var $end = node_end(ctx, $s, built_node)
	var $done = False
	while !$done {
		look = if allow_trivia and $type_ids.is_empty() skip_trivia(ctx, $s) else $s
		t = peek(ctx, look)
		name_state = at_pos(ctx, look, look.pos + 1)
		name = peek(ctx, name_state)
		if spelling(ctx, look, ".") and (allow_trivia and $type_ids.is_empty() or t.start == $end) and name.kind == UpperIdent and is_upper(byte(ctx.source, name.start)) {
			named_state = add_node(ctx, at_pos(ctx, name_state, name_state.pos + 1), Identifier, name.start, name.end, [])
			named_node = named_state.last
			$s = named_state
			$type_ids = $type_ids.append(named_node)
			$end = name.end
		} else {
			$done = True
		}
	}
	if $type_ids.is_empty() with_last(ctx, $s, built_node) else {
		start = node_start(ctx, $s, built_node)
		add_node(ctx, $s, TypedLiteral, start, $end, [built_node].concat($type_ids))
	}
}

expect {
	["\"hello \${name}!\"", "\"\${{ a: 1 }}\"", "\"\${\"inner \${x}\"}\"", "\\\\hello\n\\\\world", "\"\"\"hello\n\"\"\"world", "42.U8", "1.0.F64", "'a'.U32", "\"hello\".MyText", "0xFF.Gui.Color"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["\"\${}\"", "\"\${x\"", "\"\${x}", "1 .U8"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

file_import_type : Ctx, State -> Built
file_import_type = |ctx, initial| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	valid_name = spelling(ctx, s, "Str") or spelling(ctx, s, "List")
	checked = if valid_name s else issue(ctx, s, UnexpectedToken, t.start, t.end)
	name_state = add_node(ctx, at_pos(ctx, checked, checked.pos + if t.kind == EndOfFile 0 else 1), Identifier, t.start, t.end, [])
	name_node = name_state.last
	if spelling(ctx, s, "List") {
		look = skip_trivia(ctx, name_state)
		opening = peek(ctx, look)
		after = if opening.kind == OpenRound or opening.kind == NoSpaceOpenRound at_pos(ctx, look, look.pos + 1) else issue(ctx, look, UnexpectedToken, opening.start, opening.end)
		arg_state = skip_trivia(ctx, after)
		arg = peek(ctx, arg_state)
		validated = if spelling(ctx, arg_state, "U8") arg_state else issue(ctx, arg_state, UnexpectedToken, arg.start, arg.end)
		arg_built = add_node(ctx, at_pos(ctx, validated, validated.pos + if arg.kind == EndOfFile 0 else 1), Identifier, arg.start, arg.end, [])
		arg_node = arg_built.last
		close_state = skip_trivia(ctx, arg_built)
		close = peek(ctx, close_state)
		closed = close.kind == CloseRound
		finished = if closed at_pos(ctx, close_state, close_state.pos + 1) else issue(ctx, close_state, UnclosedDelimiter, close.start, close.end)
		add_node(ctx, finished, TypeExpression, t.start, if closed close.end else arg.end, [name_node, arg_node])
	} else {
		with_last(ctx, name_state, name_node)
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

dependency_record : Ctx, State, Bool, U64 -> Built
dependency_record = |ctx, initial, strict, depth| {
	start = peek(ctx, initial).start
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = peek(ctx, initial).end
	var $platform = False
	var $version = False
	var $seen = Set.empty()
	var $done = False
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$end = t.end
			$s = at_pos(ctx, look, look.pos + 1)
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = look.pos
			valid_name = t.kind == LowerIdent and !spelling(ctx, look, "_") and byte(ctx.source, t.start) != '_'
			checked = if valid_name look else issue(ctx, look, UnexpectedToken, t.start, t.end)
			name_state = add_node(ctx, at_pos(ctx, checked, checked.pos + 1), Identifier, t.start, t.end, [])
			name_node = name_state.last
			var $field_ids = [name_node]
			$s = skip_trivia(ctx, name_state)
			$end = t.end
			is_version = token_spelling(ctx.source, t, "roc")
			if !is_version {
				dependency_name = Str.from_utf8_lossy(ctx.source.sublist({ start: t.start, len: t.end - t.start }))
				if $seen.contains(dependency_name) {
					$s = issue(ctx, $s, UnexpectedToken, t.start, t.end)
				}
				$seen = $seen.insert(dependency_name)
			}
			if spelling(ctx, $s, ":") {
				value_start = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				is_platform = spelling(ctx, value_start, "platform")
				value_state = if is_platform skip_trivia(ctx, at_pos(ctx, value_start, value_start.pos + 1)) else value_start
				value_token = peek(ctx, value_state)
				valid_start = if is_platform value_token.kind == StringStart or spelling(ctx, value_state, "glue") else !strict or value_token.kind == StringStart
				validated = if valid_start and (!is_platform or !$platform) value_state else issue(ctx, value_state, UnexpectedToken, value_token.start, value_token.end)
				parsed_state = expression(ctx, validated, 0, depth + 1, False)
				parsed_node = parsed_state.last
				var $value_state = parsed_state
				var $value_node = parsed_node
				$end = node_end(ctx, parsed_state, parsed_node)
				if is_platform {
					platform_state = add_node(ctx, parsed_state, PlatformReference, peek(ctx, value_start).start, $end, [parsed_node])
					platform_node = platform_state.last
					$value_state = platform_state
					$value_node = platform_node
					$platform = True
				}
				$s = $value_state
				$field_ids = $field_ids.append($value_node)
				if !is_version and !(is_platform and token_spelling(ctx.source, value_token, "glue")) and !plain_dependency_path(ctx, $s, parsed_node) {
					$s = issue(ctx, $s, UnexpectedToken, t.start, $end)
				}
				if is_version {
					n = node_get(ctx, $s, parsed_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
					part_id = child_get(ctx, $s, n.children_start) ?? node_count(ctx, $s)
					part = node_get(ctx, $s, part_id) ?? n
					version_text = Str.from_utf8_lossy(ctx.source.sublist({ start: part.start, len: part.end - part.start }))
					if $version or is_platform or n.kind != StringLiteral or n.children_len != 1 or part.kind != StringSegment or !valid_roc_version(version_text) {
						$s = issue(ctx, $s, UnexpectedToken, t.start, $end)
					}
				}
			} else {
				$s = issue(ctx, $s, UnexpectedToken, t.start, $end)
			}
			$version = $version or is_version
			field_state = add_node(ctx, $s, DependencyField, t.start, $end, $field_ids)
			field_node = field_state.last
			$ids = $ids.append(field_node)
			$s = skip_trivia(ctx, field_state)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			} else if peek(ctx, $s).kind != CloseCurly {
				$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
			}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, Dependencies, start, $end, $ids)
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
	["app [main!] { pf: platform \"../platform/main.roc\" }", "app [] {}", "app [] { pf: platform glue }", "package [Parser, parse]", "package [parse] { util: \"util.roc\", other: \"other.roc\" }", "app [] { roc: \"nightly-2026-July-31-123c5d7\" }", "package [] { roc: \"1.0.0-rc1\" }"].all(|source| RocSyntax.parse_header_str(source).status == Complete)
}

expect {
	["app []", "app [] { pkg }", "app [] { pkg: 42 }", "app [] { a: platform glue, b: platform glue }", "app [] { roc: platform glue }", "package [] { roc: \"bad\" }", "package [] { roc: \"1.0.0\", roc: \"1.0.1\" }"].all(|source| RocSyntax.parse_header_str(source).status == Recovered)
}

pattern_string : Ctx, State -> Built
pattern_string = |ctx, initial| {
	first = peek(ctx, initial)
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = first.end
	var $captures = False
	var $done = False
	while !$done {
		t = peek(ctx, $s)
		if t.kind == StringPart {
			part_state = add_node(ctx, at_pos(ctx, $s, $s.pos + 1), StringSegment, t.start, t.end, [])
			part_node = part_state.last
			$s = part_state
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			name_state = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
			name = peek(ctx, name_state)
			checked = if name.kind == LowerIdent name_state else issue(ctx, name_state, UnexpectedToken, name.start, name.end)
			named_state = add_node(ctx, at_pos(ctx, checked, checked.pos + if name.kind == EndOfFile or name.kind == InterpolationEnd 0 else 1), if spelling(ctx, checked, "_") WildcardPattern else IdentifierPattern, name.start, name.end, [])
			named_node = named_state.last
			close_state = skip_trivia(ctx, named_state)
			close = peek(ctx, close_state)
			closed = close.kind == InterpolationEnd
			finished = if closed at_pos(ctx, close_state, close_state.pos + 1) else issue(ctx, close_state, UnclosedDelimiter, close.start, close.end)
			$end = if closed close.end else name.end
			capture_state = add_node(ctx, finished, CapturePattern, t.start, $end, [named_node])
			capture_node = capture_state.last
			$s = capture_state
			$ids = $ids.append(capture_node)
			$captures = True
			if !closed {
				$done = True
			}
		} else if t.kind == StringEnd {
			$s = at_pos(ctx, $s, $s.pos + 1)
			$end = t.end
			$done = True
		} else {
			$s = issue(ctx, $s, UnclosedString, first.start, $end)
			$done = True
		}
	}
	add_node(ctx, $s, if $captures PatternStringLiteral else StringLiteral, first.start, $end, $ids)
}

expect {
	RocSyntax.parse_header_str("").status == Complete and RocSyntax.parse_expr_str("match text { \"hello \${name}\" => name, \"\${_}\" => \"\" }").status == Complete and RocSyntax.parse_expr_str("match text { \"\${a + b}\" => 0 }").status != Complete and RocSyntax.parse_expr_str("match text { \"x\".Str => 0 }").status != Complete
}

# Only receiver/callee ancestors count: methods buried in arguments cannot
# make a literal a valid pipe target.
pipe_has_method : Ctx, State, U64 -> Bool
pipe_has_method = |ctx, s, root| {
	var $id = root
	var $found = False
	var $done = False
	while !$done {
		if method_target(ctx, s, $id) {
			$found = True
			$done = True
		} else {
			match node_get(ctx, s, $id) {
				Ok(n) if n.kind == Call or n.kind == FieldAccess or n.kind == OptionalFieldAccess or n.kind == Suffix or n.kind == NominalRecord or n.kind == NominalApply => {
					$id = child_get(ctx, s, n.children_start) ?? node_count(ctx, s)
				}
				_ => {
					$done = True
				}
			}
		}
	}
	$found
}

# An assignment head after a line break cannot continue an expression. Keep
# its tokens for the enclosing statement loop and blame the damaged line.
declaration_sync : Ctx, State -> { sync : Bool, start : U64, end : U64 }
declaration_sync = |ctx, initial| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	next = skip_trivia(ctx, at_pos(ctx, s, s.pos + 1))
	if t.kind != LowerIdent or !spelling(ctx, next, "=") {
		return { sync: False, start: t.start, end: t.start }
	}
	var $pos = s.pos
	var $newline = False
	var $done = False
	var $start = t.start
	var $end = t.start
	while $pos > 0 and !$done {
		$pos = $pos - 1
		previous = source_token(ctx, s, $pos)
		if previous.kind == Whitespace or previous.kind == Comment {
			$newline = $newline or line_break(ctx.source, previous.start, previous.end)
		} else {
			$start = previous.start
			$end = previous.end
			$done = True
		}
	}
	{ sync: $newline, start: $start, end: $end }
}

method_target : Ctx, State, U64 -> Bool
method_target = |ctx, s, root| {
	match node_get(ctx, s, root) {
		Ok(call) if call.kind == Call => {
			callee_id = child_get(ctx, s, call.children_start) ?? node_count(ctx, s)
			match node_get(ctx, s, callee_id) {
				Ok(callee) if callee.kind == FieldAccess => {
					receiver = child_get(ctx, s, callee.children_start) ?? node_count(ctx, s)
					field_id = child_get(ctx, s, callee.children_start + 1) ?? node_count(ctx, s)
					field = node_get(ctx, s, field_id) ?? callee
					is_lower(byte(ctx.source, field.start)) and !namespace_receiver(ctx, s, receiver)
				}
				_ => False
			}
		}
		_ => False
	}
}

namespace_receiver : Ctx, State, U64 -> Bool
namespace_receiver = |ctx, s, root| {
	var $id = root
	var $upper_segment = False
	var $done = False
	var $qualified = False
	while !$done {
		match node_get(ctx, s, $id) {
			Ok(n) if n.kind == Identifier => {
				$qualified = is_upper(byte(ctx.source, n.start)) or $upper_segment
				$done = True
			}
			Ok(n) if n.kind == FieldAccess => {
				field_id = child_get(ctx, s, n.children_start + 1) ?? node_count(ctx, s)
				field = node_get(ctx, s, field_id) ?? n
				if is_upper(byte(ctx.source, field.start)) {
					$upper_segment = True
					$id = child_get(ctx, s, n.children_start) ?? node_count(ctx, s)
				} else {
					$done = True
				}
			}
			_ => {
				$done = True
			}
		}
	}
	$qualified
}

expect {
	["value->f", "value->f(1)", "value->Str.concat(\"x\")", "value->(|x| x + 1)", "value |> f(1).field", "value |> receiver.method(1)", "value |> (f)(1)", "value |> [1, 2].map(f)", "value->f(1).field", "1 + 2->f |> g"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["value->.f(1)", "value |> .f(1)", "value->1", "value |> [1, 2]", "value |> 1 + 2"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
}

expect {
	RocSyntax.parse_expr_str("tuple.0.1").status == Complete and RocSyntax.parse_expr_str("tuple.01.002").status == Complete
}

# Distinguish local type declarations from uppercase tag/call expressions.
type_declaration_start : Ctx, State -> Bool
type_declaration_start = |ctx, initial| {
	var $s = skip_trivia(ctx, at_pos(ctx, initial, initial.pos + 1))
	if peek(ctx, $s).kind == OpenRound or peek(ctx, $s).kind == NoSpaceOpenRound {
		$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
		var $done = False
		while !$done {
			if peek(ctx, $s).kind == CloseRound {
				$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				$done = True
			} else if peek(ctx, $s).kind == LowerIdent {
				$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				if spelling(ctx, $s, ",") {
					$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				}
					else if peek(ctx, $s).kind != CloseRound {
						return False
					}
			} else {
				return False
			}
		}
	}
	spelling(ctx, $s, ":") or spelling(ctx, $s, ":=") or spelling(ctx, $s, "::")
}

# Where aliases deliberately accept spaced qualification segments.
type_where_alias_name : Ctx, State, U64 -> Built
type_where_alias_name = |ctx, initial, depth| {
	first = peek(ctx, initial)
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $end = first.end
	var $done = False
	while !$done {
		dot = skip_trivia(ctx, $s)
		if spelling(ctx, dot, ".") {
			name_state = at_pos(ctx, dot, dot.pos + 1)
			name = peek(ctx, name_state)
			if name.kind == UpperIdent and name.start == peek(ctx, dot).end and byte(ctx.source, name.start) != '@' {
				$s = at_pos(ctx, name_state, name_state.pos + 1)
				$end = name.end
			} else {
				$done = True
			}
		} else {
			$done = True
		}
	}
	named_state = add_node(ctx, $s, TypeName, first.start, $end, [])
	named_node = named_state.last
	look = skip_trivia(ctx, named_state)
	if peek(ctx, look).kind == NoSpaceOpenRound {
		items = type_collection(ctx, at_pos(ctx, look, look.pos + 1), CloseRound, TypeItem, depth + 1)
		add_node(ctx, items.state, TypeApply, first.start, items.end, [named_node].concat(items.ids))
	} else {
		with_last(ctx, named_state, named_node)
	}
}

expect {
	# Dedicated type grammar covers every non-malformed TypeAnno family.
	["f : a", "f : _", "f : _item", "f : A.B", "f : a.A(b)", "f : List(a -> b)", "f : a,b -> c", "f : (a,b) -> c", "f : (a,b -> c)", "f : () -> a", "f : ()", "f : (a,)", "f : {a: U8,b:Str}", "f : {a: U8, ..}", "f : {a: U8, ..ext}", "f : [A,..,]", "f : [A,..r]", "f : [a,U8]", "f : {a: U8 ?? 1}", "f : {a ?: U8}", "f : {_: U8}", "f : U8 => Str", "Foo(a,_b,_) : List(a)", "Foo := [A,B].{x = 1}", "Foo :: U8.{ x : U8\nx = 1 }", "f : a -> b where [a.to_b : a -> b]", "f : a -> b where [a.Foo(b), b.Bar]", "a.Sort : where [a.order : a,a -> [Before,Same,After]]", "Holder := {}.{a.Show : where [a.to_str : a -> Str]}\nf : a -> Str where [a .Holder .Show]"].all(|source| RocSyntax.parse_str(source).status == Complete)
}

expect {
	# Value syntax, invalid qualification, and ambiguous arrows remain rejected.
	["f : a -> b -> c", "f : a(b)", "f : A ()", "f : A(1)", "f : 0", "f : [1]", "f : {a: U8 = 1}", "f : {_ ?: U8}", "f : {a:?U8}", "f : A. B", "f : A .B", "f : @Foo", "f : __", "f : _a.A", "f : [A,..r,B]", "f : {a:U8,..r,b:Str}", "f : a,b", "Foo = U8", "Foo(U8) : U8", "Foo(a b) : U8", "Foo : U8.{x = 1}", "f : a -> b where []", "f : U8 where [x]", "f : U8 where [_a.foo : U8]", "f : U8 where [a.Foo : U8]", "a.Sort := where [a.foo : a]", "Foo := [A].{ x = 5\nx }"].all(|source| RocSyntax.parse_str(source).status != Complete)
}

expect {
	p = RocSyntax.parse_str("Box(a) := { value ?: List(a), ..ext }.{ make : a -> Box(a)\n make = |x| x }")
	p.status == Complete and [TypeDeclaration, TypeHeader, TypeVariable, TypeRecord, TypeField, TypeApply, TypeExtension, AssociatedBlock, TypeAnnotation, TypeFunction].all(|kind| p.nodes.any(|n| n.kind == kind))
}

expect {
	# Error recovery keeps all type children within their parent source spans.
	["Pair(a, b+ : (", "a.Sort := where [a.foo : a]", "f : U8 where [a.method -> b]", "f : {a: U8 + 1}", "f : [A, ..r, B]"].all(
		|source| {
			p = RocSyntax.parse_str(source)
			p.status != Complete and p.nodes.all(
				|n| p.children.sublist({ start: n.children_start, len: n.children_len }).all(
					|id| match p.nodes.get(id) {
						Ok(child) => n.start <= child.start and child.end <= n.end
						Err(_) => False
					},
				),
			)
		},
	)
}

expect {
	# The pinned parser accepts adjacent fields/tags without an explicit comma.
	["f : {a: U8 b: Str}", "f : [A B]", "f : [A(a) B(b)]", "f : {a ?: U8 b ?: Str}", "f : {a: U8 ?? 1 b: Str}"].all(|source| RocSyntax.parse_str(source).status == Complete)
}

record_starts_block : Ctx, State -> Bool
record_starts_block = |ctx, s| {
	var $pos = s.pos + 2
	var $nesting = 0.U64
	var $found = False
	var $done = False
	while !$done {
		t = source_token(ctx, s, $pos)
		if t.kind == EndOfFile {
			$done = True
		} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare or t.kind == OpenCurly {
			$nesting = $nesting + 1
		} else if t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			if $nesting == 0 {
				$done = True
			} else {
				$nesting = $nesting - 1
			}
		} else if $nesting == 0 and t.kind == LowerIdent and token_spelling(ctx.source, source_token(ctx, s, significant_position(ctx.tokens, $pos + 1)), "=") {
			$found = True
			$done = True
		}
		$pos = $pos + 1
	}
	$found
}

expect {
	["{ ..person, age: 31 }", "{ ..person, }", "{ field: _ }", "{ x: 1 }.Foo", "{ x: 1 }.Mod.Foo", "Foo.{ x: 1 }", "mapper.{ ..base, x: 1 }", "Foo.(1)", "Mod.Foo.(1, 2)", "{ x : U64\n x = 1\n x }"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["{ ..person }", "{ ..person, x: 1 }.Foo", "0.(1)", "value.(1)", "{ field: _ + 1 }"].all(|source| RocSyntax.parse_expr_str(source).status != Complete) and RocSyntax.parse_str("Foo := {}.{ var x = 1 }").status != Complete
}

expect {
	RocSyntax.parse_expr_str("match person { { $name } => $name }").status == Complete
}

expect {
	RocSyntax.parse_expr_str("...").status == Complete and RocSyntax.parse_expr_str("value\n...").status == Recovered and RocSyntax.parse_expr_str("{ return Tag\n... }").status == Complete
}

expect {
	# Minimized raw-fuzz recovery: a mismatched closer overlaps an error child.
	p = RocSyntax.parse_bytes([89, 123, 1, 93])
	p.status != Complete and p.nodes.all(
		|n| p.children.sublist({ start: n.children_start, len: n.children_len }).all(
			|id| match p.nodes.get(id) {
				Ok(child) => n.start <= child.start and child.end <= n.end
				Err(_) => False
			},
		),
	)
}

expect {
	["x = 1 y = 2", "main!=|0|\"\"_=\"\"", "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}", "module[E.a.*]"].all(|source| RocSyntax.parse_str(source).status == Complete) and RocSyntax.parse_expr_str("{1 2}").status == Complete
}

expect {
	["module[Foo.bar as lower]", "module[Foo._]", "module[Foo._a]", "module[Foo.$bar]"].all(|source| RocSyntax.parse_str(source).status != Complete) and RocSyntax.parse_expr_str("x.$field").status != Complete
}

# Platform headers use dedicated collections: target configuration is data,
# and its atoms deliberately do not inherit value expression operators.
parse_platform_header : Ctx, State, U64 -> Built
parse_platform_header = |ctx, initial, depth| {
	start = peek(ctx, initial).start
	name_state = platform_header_string(ctx, skip_trivia(ctx, at_pos(ctx, initial, initial.pos + 1)), True)
	name_node = name_state.last
	requires_state = platform_requires(ctx, platform_header_keyword(ctx, name_state, "requires"), depth + 1)
	requires_node = requires_state.last
	exposed_start = platform_header_keyword(ctx, requires_state, "exposes")
	exposed = exposed_collection(ctx, exposed_start)
	packages_start = platform_header_keyword(ctx, exposed.state, "packages")
	checked_packages = if peek(ctx, packages_start).kind == OpenCurly packages_start else issue(ctx, packages_start, UnexpectedToken, peek(ctx, packages_start).start, peek(ctx, packages_start).end)
	packages_state = dependency_record(ctx, checked_packages, False, depth + 1)
	packages_node = packages_state.last
	package_info = node_get(ctx, packages_state, packages_node) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
	var $s = packages_state
	var $index = 0.U64
	while $index < package_info.children_len {
		field_id = child_get(ctx, $s, package_info.children_start + $index) ?? packages_node
		field = node_get(ctx, $s, field_id) ?? package_info
		if field.children_len > 1 {
			value_id = child_get(ctx, $s, field.children_start + 1) ?? field_id
			value = node_get(ctx, $s, value_id) ?? field
			if value.kind == PlatformReference {
				$s = issue(ctx, $s, UnexpectedToken, value.start, value.end)
			}
		}
		$index = $index + 1
	}
	provides_state = platform_symbol_map(ctx, platform_header_keyword(ctx, $s, "provides"), PlatformProvides, depth + 1)
	provides_node = provides_state.last
	$s = skip_trivia(ctx, provides_state)
	var $ids = [name_node, requires_node].concat(exposed.ids).append(packages_node).append(provides_node)
	if spelling(ctx, $s, "hosted") {
		hosted_state = platform_symbol_map(ctx, skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1)), PlatformHosted, depth + 1)
		hosted_node = hosted_state.last
		$s = skip_trivia(ctx, hosted_state)
		$ids = $ids.append(hosted_node)
	}
	if spelling(ctx, $s, "targets") {
		targets_state = platform_targets(ctx, platform_header_keyword(ctx, at_pos(ctx, $s, $s.pos + 1), ":"), depth + 1)
		targets_node = targets_state.last
		$s = targets_state
		$ids = $ids.append(targets_node)
	}
	end = $ids.fold(start, |furthest, id| U64.max(furthest, node_end(ctx, $s, id)))
	add_node(ctx, $s, Header, start, end, $ids)
}

platform_header_keyword : Ctx, State, Str -> State
platform_header_keyword = |ctx, initial, text| {
	s = skip_trivia(ctx, initial)
	if spelling(ctx, s, text) skip_trivia(ctx, at_pos(ctx, s, s.pos + 1)) else issue(ctx, s, UnexpectedToken, peek(ctx, s).start, peek(ctx, s).end)
}

# Platform names and symbol keys must be plain strings (empty is valid).
# Target strings intentionally consume uninterpreted tokens like the compiler.
platform_header_string : Ctx, State, Bool -> Built
platform_header_string = |ctx, initial, plain| {
	s = skip_trivia(ctx, initial)
	opening = peek(ctx, s)
	if opening.kind != StringStart {
		bad = issue(ctx, s, UnexpectedToken, opening.start, opening.end)
		return add_node(ctx, at_pos(ctx, bad, bad.pos + if opening.kind == EndOfFile 0 else 1), ErrorNode, opening.start, opening.end, [])
	}
	var $s = at_pos(ctx, s, s.pos + 1)
	var $end = opening.end
	var $parts = 0.U64
	var $plain = True
	while peek(ctx, $s).kind != StringEnd and peek(ctx, $s).kind != EndOfFile {
		t = peek(ctx, $s)
		$plain = $plain and t.kind == StringPart
		$parts = $parts + 1
		$end = t.end
		$s = at_pos(ctx, $s, $s.pos + 1)
	}
	var $ids = []
	if $end > opening.end {
		part_state = add_node(ctx, $s, StringSegment, opening.end, $end, [])
		part_node = part_state.last
		$s = part_state
		$ids = [part_node]
	}
	closing = peek(ctx, $s)
	if closing.kind == StringEnd {
		$end = closing.end
		$s = at_pos(ctx, $s, $s.pos + 1)
	} else {
		$s = issue(ctx, $s, UnclosedString, opening.start, $end)
	}
	if plain and (!$plain or $parts > 1) {
		$s = issue(ctx, $s, UnexpectedToken, opening.start, $end)
	}
	add_node(ctx, $s, StringLiteral, opening.start, $end, $ids)
}

platform_requires : Ctx, State, U64 -> Built
platform_requires = |ctx, initial, depth| {
	opening = peek(ctx, initial)
	var $s = if opening.kind == OpenCurly at_pos(ctx, initial, initial.pos + 1) else issue(ctx, initial, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenCurly or depth >= 128
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	initial_look = skip_trivia(ctx, $s)
	if peek(ctx, initial_look).kind == CloseCurly {
		$end = peek(ctx, initial_look).end
		$s = skip_trivia(ctx, at_pos(ctx, initial_look, initial_look.pos + 1))
		if peek(ctx, $s).kind == OpenCurly {
			$s = at_pos(ctx, $s, $s.pos + 1)
		}
			else {
				$done = True
			}
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$end = t.end
			$s = at_pos(ctx, look, look.pos + 1)
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseSquare or t.kind == CloseRound {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			var $entry_ids = []
			$s = look
			if t.kind == OpenSquare {
				$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
				var $aliases_done = False
				while !$aliases_done {
					alias = peek(ctx, $s)
					if alias.kind == CloseSquare {
						$s = at_pos(ctx, $s, $s.pos + 1)
						$aliases_done = True
					} else if alias.kind == EndOfFile or alias.kind == CloseCurly or alias.kind == CloseRound {
						$s = issue(ctx, $s, UnclosedDelimiter, alias.start, alias.end)
						$aliases_done = True
					} else {
						checked = if alias.kind == UpperIdent and byte(ctx.source, alias.start) != '@' $s else issue(ctx, $s, UnexpectedToken, alias.start, alias.end)
						alias_state = add_node(ctx, at_pos(ctx, checked, checked.pos + 1), TypeName, alias.start, alias.end, [])
						alias_node = alias_state.last
						rigid_state = platform_header_keyword(ctx, alias_state, ":")
						rigid = peek(ctx, rigid_state)
						checked_rigid = if rigid.kind == LowerIdent and byte(ctx.source, rigid.start) != '_' rigid_state else issue(ctx, rigid_state, UnexpectedToken, rigid.start, rigid.end)
						rigid_built = add_node(ctx, at_pos(ctx, checked_rigid, checked_rigid.pos + if rigid.kind == EndOfFile 0 else 1), TypeVariable, rigid.start, rigid.end, [])
						rigid_node = rigid_built.last
						built_state = add_node(ctx, rigid_built, RequiredType, alias.start, rigid.end, [alias_node, rigid_node])
						built_node = built_state.last
						$s = skip_trivia(ctx, built_state)
						$entry_ids = $entry_ids.append(built_node)
						if spelling(ctx, $s, ",") {
							$s = skip_trivia(ctx, at_pos(ctx, $s, $s.pos + 1))
						}
							else if peek(ctx, $s).kind != CloseSquare {
								$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
								$aliases_done = True
							}
					}
				}
				$s = platform_header_keyword(ctx, $s, "for")
			}
			name = peek(ctx, skip_trivia(ctx, $s))
			checked = if name.kind == LowerIdent and byte(ctx.source, name.start) != '_' skip_trivia(ctx, $s) else issue(ctx, skip_trivia(ctx, $s), UnexpectedToken, name.start, name.end)
			named_state = add_node(ctx, at_pos(ctx, checked, checked.pos + if name.kind == EndOfFile 0 else 1), Identifier, name.start, name.end, [])
			named_node = named_state.last
			type_state = type_expression(ctx, platform_header_keyword(ctx, named_state, ":"), FullType, depth + 1)
			type_node = type_state.last
			$entry_ids = $entry_ids.append(named_node).append(type_node)
			entry_end = $entry_ids.fold(t.end, |furthest, id| U64.max(furthest, node_end(ctx, type_state, id)))
			entry_state = add_node(ctx, type_state, RequiredValue, t.start, entry_end, $entry_ids)
			entry_node = entry_state.last
			$s = skip_trivia(ctx, entry_state)
			$ids = $ids.append(entry_node)
			$end = entry_end
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
				else if peek(ctx, $s).kind != CloseCurly {
					$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					$done = True
				}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, PlatformRequires, opening.start, $end, $ids)
}

platform_symbol_map : Ctx, State, RocSyntax.NodeKind, U64 -> Built
platform_symbol_map = |ctx, initial, kind, depth| {
	opening = peek(ctx, initial)
	var $s = if opening.kind == OpenCurly at_pos(ctx, initial, initial.pos + 1) else issue(ctx, initial, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenCurly or depth >= 128
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			symbol_state = platform_header_string(ctx, look, True)
			symbol_node = symbol_state.last
			target_state = platform_header_keyword(ctx, symbol_state, ":")
			target = peek(ctx, target_state)
			var $target_state = target_state
			var $target_end = target.end
			if target.kind == UpperIdent and byte(ctx.source, target.start) != '@' {
				$target_state = at_pos(ctx, target_state, target_state.pos + 1)
				var $path_done = False
				var $lower = False
				while !$path_done {
					dot = peek(ctx, $target_state)
					name_state = at_pos(ctx, $target_state, $target_state.pos + 1)
					name = peek(ctx, name_state)
					if token_spelling(ctx.source, dot, ".") and dot.start == $target_end and name.start == dot.end and (name.kind == UpperIdent or name.kind == LowerIdent) and (is_lower(byte(ctx.source, name.start)) or is_upper(byte(ctx.source, name.start))) {
						$target_end = name.end
						$target_state = at_pos(ctx, name_state, name_state.pos + 1)
						if name.kind == LowerIdent {
							$lower = True
							$path_done = True
						}
					} else {
						$path_done = True
					}
				}
				if !$lower {
					$target_state = issue(ctx, $target_state, UnexpectedToken, target.start, $target_end)
				}
			} else if target.kind == LowerIdent and byte(ctx.source, target.start) != '_' {
				$target_state = at_pos(ctx, target_state, target_state.pos + 1)
			} else {
				$target_state = issue(ctx, at_pos(ctx, target_state, target_state.pos + if target.kind == EndOfFile 0 else 1), UnexpectedToken, target.start, target.end)
			}
			named_state = add_node(ctx, $target_state, Identifier, target.start, $target_end, [])
			named_node = named_state.last
			entry_end = U64.max(node_end(ctx, named_state, symbol_node), $target_end)
			entry_state = add_node(ctx, named_state, SymbolMapping, t.start, entry_end, [symbol_node, named_node])
			entry_node = entry_state.last
			$s = skip_trivia(ctx, entry_state)
			$ids = $ids.append(entry_node)
			$end = entry_end
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
				else if peek(ctx, $s).kind != CloseCurly {
					$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					$done = True
				}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, kind, opening.start, $end, $ids)
}

platform_targets : Ctx, State, U64 -> Built
platform_targets = |ctx, initial, depth| {
	opening = peek(ctx, initial)
	var $s = if opening.kind == OpenCurly at_pos(ctx, initial, initial.pos + 1) else issue(ctx, initial, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenCurly or depth >= 128
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			valid_name = t.kind == LowerIdent and byte(ctx.source, t.start) != '_'
			checked = if valid_name look else issue(ctx, look, UnexpectedToken, t.start, t.end)
			named_state = add_node(ctx, at_pos(ctx, checked, checked.pos + 1), Identifier, t.start, t.end, [])
			named_node = named_state.last
			value_state = platform_header_keyword(ctx, named_state, ":")
			if spelling(ctx, look, "inputs_dir") and peek(ctx, value_state).kind == StringStart {
				path_state = platform_header_string(ctx, value_state, False)
				path_node = path_state.last
				entry_state = add_node(ctx, path_state, TargetInputsDirectory, t.start, node_end(ctx, path_state, path_node), [path_node])
				entry_node = entry_state.last
				$s = skip_trivia(ctx, entry_state)
				$ids = $ids.append(entry_node)
				$end = node_end(ctx, $s, entry_node)
			} else if peek(ctx, value_state).kind == OpenCurly {
				config_state = platform_target_config(ctx, value_state, depth + 1)
				config_node = config_state.last
				entry_state = add_node(ctx, config_state, TargetEntry, t.start, node_end(ctx, config_state, config_node), [named_node, config_node])
				entry_node = entry_state.last
				$s = skip_trivia(ctx, entry_state)
				$ids = $ids.append(entry_node)
				$end = node_end(ctx, $s, entry_node)
			} else {
				$s = issue(ctx, value_state, UnexpectedToken, peek(ctx, value_state).start, peek(ctx, value_state).end)
				$done = True
			}
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, PlatformTargets, opening.start, $end, $ids)
}

platform_target_config : Ctx, State, U64 -> Built
platform_target_config = |ctx, initial, depth| {
	opening = peek(ctx, initial)
	var $s = at_pos(ctx, initial, initial.pos + 1)
	var $ids = []
	var $end = opening.end
	var $done = depth >= 128
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseCurly {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			checked = if t.kind == LowerIdent and byte(ctx.source, t.start) != '_' look else issue(ctx, look, UnexpectedToken, t.start, t.end)
			name_state = add_node(ctx, at_pos(ctx, checked, checked.pos + 1), Identifier, t.start, t.end, [])
			name_node = name_state.last
			after = skip_trivia(ctx, name_state)
			var $field_ids = [name_node]
			$s = after
			$end = t.end
			if peek(ctx, after).kind != CloseCurly and !spelling(ctx, after, ",") {
				value_state = platform_header_keyword(ctx, after, ":")
				files = spelling(ctx, look, "inputs")
				built_state = if files platform_target_list(ctx, value_state, True, depth + 1) else platform_target_value(ctx, value_state, False, depth + 1)
				built_node = built_state.last
				$s = built_state
				$field_ids = $field_ids.append(built_node)
				$end = node_end(ctx, built_state, built_node)
			}
			field_state = add_node(ctx, $s, TargetField, t.start, $end, $field_ids)
			field_node = field_state.last
			$s = skip_trivia(ctx, field_state)
			$ids = $ids.append(field_node)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
				else if peek(ctx, $s).kind != CloseCurly {
					$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					$done = True
				}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, TargetConfig, opening.start, $end, $ids)
}

platform_target_list : Ctx, State, Bool, U64 -> Built
platform_target_list = |ctx, initial, files, depth| {
	opening = peek(ctx, initial)
	var $s = if opening.kind == OpenSquare at_pos(ctx, initial, initial.pos + 1) else issue(ctx, initial, UnexpectedToken, opening.start, opening.end)
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenSquare or depth >= 128
	if depth >= 128 {
		$s = issue(ctx, $s, ResourceLimit, opening.start, opening.end)
	}
	while !$done {
		look = skip_trivia(ctx, $s)
		$s = vacant
		t = peek(ctx, look)
		if t.kind == CloseSquare {
			$s = at_pos(ctx, look, look.pos + 1)
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseCurly or t.kind == CloseRound {
			$s = issue(ctx, look, UnclosedDelimiter, t.start, t.end)
			$end = t.start
			$done = True
		} else {
			before = look.pos
			item_state = platform_target_value(ctx, look, files, depth + 1)
			item_node = item_state.last
			$ids = $ids.append(item_node)
			$s = skip_trivia(ctx, item_state)
			$end = node_end(ctx, item_state, item_node)
			if spelling(ctx, $s, ",") {
				$s = at_pos(ctx, $s, $s.pos + 1)
			}
				else if peek(ctx, $s).kind != CloseSquare {
					$s = issue(ctx, $s, UnexpectedToken, peek(ctx, $s).start, peek(ctx, $s).end)
					$done = True
				}
			if $s.pos <= before {
				$s = at_pos(ctx, $s, before + 1)
			}
		}
	}
	add_node(ctx, $s, TargetList, opening.start, $end, $ids)
}

platform_target_value : Ctx, State, Bool, U64 -> Built
platform_target_value = |ctx, initial, file, depth| {
	s = skip_trivia(ctx, initial)
	t = peek(ctx, s)
	if depth >= 128 {
		return add_node(ctx, issue(ctx, s, ResourceLimit, t.start, t.end), ErrorNode, t.start, t.start, [])
	}
	if t.kind == StringStart {
		string_state = platform_header_string(ctx, s, False)
		string_node = string_state.last
		if file add_node(ctx, string_state, TargetFile, t.start, node_end(ctx, string_state, string_node), [string_node]) else with_last(ctx, string_state, string_node)
	} else if !file and t.kind == OpenSquare {
		platform_target_list(ctx, s, False, depth + 1)
	} else if (t.kind == LowerIdent and byte(ctx.source, t.start) != '_') or (file and spelling(ctx, s, "app")) or (!file and t.kind == UpperIdent and byte(ctx.source, t.start) != '@') {
		name_state = add_node(ctx, at_pos(ctx, s, s.pos + 1), Identifier, t.start, t.end, [])
		name_node = name_state.last
		if file add_node(ctx, name_state, TargetFile, t.start, t.end, [name_node]) else with_last(ctx, name_state, name_node)
	} else if !file and t.kind == Number {
		raw = ctx.source.sublist({ start: t.start, len: t.end - t.start })
		is_int = raw.any(|b| b == 'x' or b == 'o' or b == 'b') or !raw.any(|b| b == '.' or b == 'e' or b == 'E')
		checked = if is_int s else issue(ctx, s, UnexpectedToken, t.start, t.end)
		add_node(ctx, at_pos(ctx, checked, checked.pos + 1), NumberLiteral, t.start, t.end, [])
	} else {
		add_node(ctx, at_pos(ctx, issue(ctx, s, UnexpectedToken, t.start, t.end), s.pos + if t.kind == EndOfFile 0 else 1), ErrorNode, t.start, t.end, [])
	}
}

expect {
	# Empty names, requires type aliases, symbol paths, and target configuration.
	["platform \"p\" requires {} exposes [] packages {} provides {}", "platform \"\" requires {} exposes [] packages {} provides {}", "platform \"name\\ntext\" requires {} exposes [] packages {} provides {}", "platform \"p\" requires {}{} exposes [] packages {} provides {}", "platform \"p\" requires {main : U8} exposes [] packages {} provides {}", "platform \"p\" requires {}{main : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[Model:model] for main : model -> U64} exposes [] packages {} provides {}", "platform \"p\" requires {[A:a, B:b,] for run : a,b => Str, other : U8,} exposes [] packages {} provides {}", "platform \"p\" requires {[] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[$A:$a] for $run : $a -> U8} exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [run, Foo.*, Foo.bar as Bar] packages {} provides {}", "platform \"p\" requires {} exposes [] packages {dep: \"url\", roc: \"1.0.0\"} provides {}", "platform \"p\" requires {} exposes [] packages {} provides {\"\": run}", "platform \"p\" requires {} exposes [] packages {} provides {\"main\": Main.run!, \"more\": Main.Nested.call}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": Host.Idx.get!}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:\"targets/\", linux:{inputs:[app,\"start.o\",custom], output:Exe, shared:True, level:3, flags:[\"-lc\",Symbol,thing,[1,2]]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{output, inputs, custom}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{n:-1,n2:0xFE,n3:0_xFE}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{} windows:{}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[\"\",app,win_gui]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:[]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:\"a\",inputs_dir:\"b\"}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:\"\${x}\"}}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {} targets: {}"].all(|source| RocSyntax.parse_header_str(source).status == Complete)
}

expect {
	# Platform sections are ordered and their data atoms have restricted grammars.
	["platform \"\${x}\" requires {} exposes [] packages {} provides {}", "platform \"\"\"name requires {} exposes [] packages {} provides {}", "platform p requires {} exposes [] packages {} provides {}", "platform \"p\" requires {[A:_a] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[A:U8] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[a:a] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[A:a] run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {run U8} exposes [] packages {} provides {}", "platform \"p\" requires {run : 1} exposes [] packages {} provides {}", "platform \"p\" requires {run : U8 other : U8} exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [1] packages {} provides {}", "platform \"p\" requires {} exposes {} packages {} provides {}", "platform \"p\" requires {} exposing [] packages {} provides {}", "platform \"p\" requires {} exposes [] packages {dep: platform \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc: \"bad\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc: \"1.0.0\",roc:\"1.0.1\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc} provides {}", "platform \"p\" requires {} exposes [] packages {_dep: \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {x: \"url\" y: \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {dep: _} provides {}", "platform \"p\" requires {} exposes [] packages {} provides {\"symbol\": $Module.$method}", "platform \"p\" requires {} exposes [] packages {} provides {\"\${x}\": run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Upper}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module .run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module. run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": _run}", "platform \"p\" requires {} exposes [] packages {} provides {x: run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": run(1)}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module.lower.more}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": run \"y\": other}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted []", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": Upper}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": call} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{output:Exe inputs:[app]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {other:\"dir/\"}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:[]}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:[]}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[1]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[Upper]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:\"file\"}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:1.5}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:app}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:Module.name}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:{}}}", "platform \"p\" missing_requires {} exposes [] packages {} provides {}", "platform \"p\" requires {} missing_exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [] missing_packages {} provides {}", "platform \"p\" requires {} exposes [] packages {} missing_provides {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {", "platform \"p\" requires {} exposes [] packages {} provides {} provides {}"].all(|source| RocSyntax.parse_header_str(source).status != Complete)
}

plain_dependency_path : Ctx, State, U64 -> Bool
plain_dependency_path = |ctx, s, id| {
	match node_get(ctx, s, id) {
		Ok(n) if n.kind == StringLiteral => {
			var $valid = True
			var $i = 0.U64
			while $i < n.children_len and $valid {
				child_id = child_get(ctx, s, n.children_start + $i) ?? node_count(ctx, s)
				match node_get(ctx, s, child_id) {
					Ok(child) if child.kind == StringSegment => {
						$valid = !ctx.source.sublist({ start: child.start, len: child.end - child.start }).contains('\\')
					}
					_ => {
						$valid = False
					}
				}
				$i = $i + 1
			}
			$valid
		}
		_ => False
	}
}

expect {
	["package [] {dep}", "package [] {dep:1}", "app [] {dep: \"a\", dep: \"b\"}", "package [] {dep: \"\${path}\"}", "package [] {dep: \"a\\nb\"}", "platform \"p\" requires {} exposes [] packages {dep} provides {}", "platform \"p\" requires {} exposes [] packages {dep:1} provides {}"].all(|source| RocSyntax.parse_header_str(source).status == Recovered)
}

expect {
	RocSyntax.parse_expr_str("-...").status == Complete and RocSyntax.parse_expr_str("x-...").status == Complete and RocSyntax.parse_expr_str("x -...").status == Recovered and RocSyntax.parse_expr_str("- ...").status == Recovered
}

expect {
	["...-1", "...-...", "{ _ = 1\n-... }", "a - -...", "a - ..."].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

integer_token : List(U8), RocSyntax.Token -> Bool
integer_token = |source, t| {
	start = t.start + if byte(source, t.start) == '-' 1 else 0
	if byte(source, start) == '0' {
		var $base = start + 1
		while byte(source, $base) == '_' {
			$base = $base + 1
		}
		b = byte(source, $base)
		if b == 'x' or b == 'b' or b == 'o' {
			return True
		}
	}
	!source.sublist({ start, len: t.end - start }).any(|b| b == '.' or b == 'e' or b == 'E')
}

expect {
	["0xff.0", "0b10.00", "x -> _name", "x |> _", "match value { @Foo => 1 }", "|@A| 0", "match value { A .B => 0 }", "module [A .b]", "module [A .B]", "module [x as _name]", "module [x as _]", "module [A as @A]"].all(|source| (if source.starts_with("module") RocSyntax.parse_str(source) else RocSyntax.parse_expr_str(source)).status != Complete)
}

expect {
	["0xff .0", "1.0.0", "x->f(1)(2)", "x |> f .field", "x |> f .?field", "{ A() A() A() 0 }"].all(|source| RocSyntax.parse_expr_str(source).status == Complete) and RocSyntax.parse_str("Foo := U64.{ 1 x = 2 }").status == Complete and RocSyntax.parse_str("Foo := U64.{ 1 x = 2 3 }").status != Complete
}

expect {
	["x |> 1.method().field", "x |> 1.method()?(1)", "x |> f(1)?(1)", "x->X .field", "x |> A.fn .else", "x.?if -> f", "Box.import", "{a:1 b:2}"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	p = RocSyntax.parse_str("C,)")
	p.status != Complete and p.nodes.all(
		|n| p.children.sublist({ start: n.children_start, len: n.children_len }).all(
			|id| {
				child = p.nodes.get(id) ?? n
				child.start >= n.start and child.end <= n.end
			},
		),
	)
}

expect {
	["\u(61C)", "\u(200E)", "\u(200F)", "\u(202A)", "\u(202B)", "\u(202C)", "\u(202D)", "\u(202E)", "\u(2066)", "\u(2067)", "\u(2068)", "\u(2069)"].all(
		|control| {
			["# ".concat(control).concat("\nx = 1"), "\"".concat(control).concat("\""), "'".concat(control).concat("'")].all(
				|source| {
					p = RocSyntax.parse_str(source)
					p.status != Complete and p.diagnostics.any(|d| d.code == BidiControlInSource)
				},
			)
		},
	)
}

expect {
	p = RocSyntax.parse_str("\u(FEFF)x = 1")
	p.status != Complete and p.diagnostics.any(|d| d.code == Utf8ByteOrderMark and d.start == 0 and d.end == 3) and RocSyntax.parse_expr_str("\"\u(FEFF)\"").status == Complete
}
