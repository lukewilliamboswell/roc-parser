## Lossless parsing of Roc source into tokens, syntax trees and diagnostics.
## Byte spans are half-open. Comments and whitespace are tokens.
##
## Expressions include interpolated and multiline strings, typed literals,
## calls, field access, operators, pipes, nominal construction and control flow.
## Binding patterns, type declarations, annotations, constraints, imports and
## module, app, package and platform headers retain their syntax structure.
## Parsing does not resolve names or establish that a program typechecks.
##
## Parsing retains the source once and appends nodes and children to flat lists
## that each parser receives and returns directly, so they stay uniquely owned.
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
		status = if (lexed.unsupported) Unsupported else if (lexed.diagnostics.is_empty()) Complete else Recovered
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
keyword = |source, start, end| keyword_bytes(source, start, end)

keyword_bytes : List(U8), U64, U64 -> Bool
keyword_bytes = |source, start, end| {
	len = end - start
	if len == 0 or len > 10 {
		False
	} else {
		b0 = byte(source, start)
		if len == 2 {
			b1 = byte(source, start + 1)
			if (b0 == 'a' and b1 == 's') True else if (b0 == 'i' and b1 == 'f') True else if (b0 == 'i' and b1 == 'n') True else False
		} else if len == 3 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			if (b0 == 'a' and b1 == 'p' and b2 == 'p') True else if (b0 == 'd' and b1 == 'b' and b2 == 'g') True else if (b0 == 'f' and b1 == 'o' and b2 == 'r') True else if (b0 == 'h' and b1 == 'a' and b2 == 's') True else if (b0 == 'v' and b1 == 'a' and b2 == 'r') True else False
		} else if len == 4 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			if (b0 == 'e' and b1 == 'l' and b2 == 's' and b3 == 'e') True else if (b0 == 'f' and b1 == 'o' and b2 == 'r' and b3 == '!') True else if (b0 == 'w' and b1 == 'i' and b2 == 't' and b3 == 'h') True else False
		} else if len == 5 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			if (b0 == 'c' and b1 == 'r' and b2 == 'a' and b3 == 's' and b4 == 'h') True else if (b0 == 'm' and b1 == 'a' and b2 == 't' and b3 == 'c' and b4 == 'h') True else if (b0 == 'w' and b1 == 'h' and b2 == 'e' and b3 == 'r' and b4 == 'e') True else if (b0 == 'w' and b1 == 'h' and b2 == 'i' and b3 == 'l' and b4 == 'e') True else if (b0 == 'b' and b1 == 'r' and b2 == 'e' and b3 == 'a' and b4 == 'k') True else False
		} else if len == 6 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'e' and b4 == 'c' and b5 == 't') True else if (b0 == 'h' and b1 == 'o' and b2 == 's' and b3 == 't' and b4 == 'e' and b5 == 'd') True else if (b0 == 'i' and b1 == 'm' and b2 == 'p' and b3 == 'o' and b4 == 'r' and b5 == 't') True else if (b0 == 'm' and b1 == 'o' and b2 == 'd' and b3 == 'u' and b4 == 'l' and b5 == 'e') True else if (b0 == 'r' and b1 == 'e' and b2 == 't' and b3 == 'u' and b4 == 'r' and b5 == 'n') True else False
		} else if len == 7 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'o' and b4 == 's' and b5 == 'e' and b6 == 's') True else if (b0 == 'i' and b1 == 'm' and b2 == 'p' and b3 == 'o' and b4 == 'r' and b5 == 't' and b6 == 's') True else if (b0 == 'p' and b1 == 'a' and b2 == 'c' and b3 == 'k' and b4 == 'a' and b5 == 'g' and b6 == 'e') True else if (b0 == 't' and b1 == 'a' and b2 == 'r' and b3 == 'g' and b4 == 'e' and b5 == 't' and b6 == 's') True else False
		} else if len == 8 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			b7 = byte(source, start + 7)
			if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'o' and b4 == 's' and b5 == 'i' and b6 == 'n' and b7 == 'g') True else if (b0 == 'p' and b1 == 'a' and b2 == 'c' and b3 == 'k' and b4 == 'a' and b5 == 'g' and b6 == 'e' and b7 == 's') True else if (b0 == 'p' and b1 == 'l' and b2 == 'a' and b3 == 't' and b4 == 'f' and b5 == 'o' and b6 == 'r' and b7 == 'm') True else if (b0 == 'p' and b1 == 'r' and b2 == 'o' and b3 == 'v' and b4 == 'i' and b5 == 'd' and b6 == 'e' and b7 == 's') True else if (b0 == 'r' and b1 == 'e' and b2 == 'q' and b3 == 'u' and b4 == 'i' and b5 == 'r' and b6 == 'e' and b7 == 's') True else False
		} else if len == 9 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			b7 = byte(source, start + 7)
			b8 = byte(source, start + 8)
			if (b0 == 'g' and b1 == 'e' and b2 == 'n' and b3 == 'e' and b4 == 'r' and b5 == 'a' and b6 == 't' and b7 == 'e' and b8 == 's') True else if (b0 == 'i' and b1 == 'n' and b2 == 't' and b3 == 'e' and b4 == 'r' and b5 == 'f' and b6 == 'a' and b7 == 'c' and b8 == 'e') True else False
		} else if len == 10 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			b7 = byte(source, start + 7)
			b8 = byte(source, start + 8)
			b9 = byte(source, start + 9)
			if (b0 == 'i' and b1 == 'm' and b2 == 'p' and b3 == 'l' and b4 == 'e' and b5 == 'm' and b6 == 'e' and b7 == 'n' and b8 == 't' and b9 == 's') True else False
		} else {
			False
		}
	}
}

push_diagnostic : List(RocSyntax.Diagnostic), RocSyntax.DiagnosticCode, U64, U64 -> List(RocSyntax.Diagnostic)
push_diagnostic = |diagnostics, code, start, end| {
	# Bounded diagnostic output; parsing still records error status once full.
	if (diagnostics.len() < 256) diagnostics.append({ code, start, end }) else diagnostics
}

Starts : { starts : List(U64), count : U64 }

## Stage 1: token-start bitmask. A byte starts a token unless it continues a run of
## plain identifier bytes [A-Za-z0-9_!] or of space/tab/newline. Bytes past the end are starts.
## `count` (popcount) bounds the number of tokens the simple subset can produce.
stage1_starts : List(U8) -> Starts
stage1_starts = |source| {
	len = source.len()
	nwords = (len + 63).shr_zf_wrap(6)
	s_zero = U8x16.splat(0)
	s_nl = U8x16.splat('\n')
	s_sp = U8x16.splat(' ')
	s_tab = U8x16.splat('\t')
	s_20 = U8x16.splat(0x20)
	s_a = U8x16.splat('a')
	s_25 = U8x16.splat(25)
	s_0 = U8x16.splat('0')
	s_9 = U8x16.splat(9)
	s_us = U8x16.splat('_')
	s_bang = U8x16.splat('!')
	weights = U64x2.splat(0x8040201008040201).to_u8x16_bits()
	var $starts = List.with_capacity(nwords)
	var $count = 0.U64
	var $last_id = U8x16.splat(0)
	var $last_sp = U8x16.splat(0)
	var $off = 0.U64
	while $off + 64 <= len {
		off = $off
		v0 = U8x16.load(source, off + 0) ?? s_zero
		id0 = v0.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v0.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v0.eq_lanes(s_us)).bitwise_or(v0.eq_lanes(s_bang))
		sp0 = v0.eq_lanes(s_sp).bitwise_or(v0.eq_lanes(s_nl)).bitwise_or(v0.eq_lanes(s_tab))
		st0 = id0.bitwise_and(U8x16.concat_shift_bytes($last_id, id0, 15)).bitwise_or(sp0.bitwise_and(U8x16.concat_shift_bytes($last_sp, sp0, 15))).bitwise_not().bitwise_and(weights)
		v1 = U8x16.load(source, off + 16) ?? s_zero
		id1 = v1.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v1.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v1.eq_lanes(s_us)).bitwise_or(v1.eq_lanes(s_bang))
		sp1 = v1.eq_lanes(s_sp).bitwise_or(v1.eq_lanes(s_nl)).bitwise_or(v1.eq_lanes(s_tab))
		st1 = id1.bitwise_and(U8x16.concat_shift_bytes(id0, id1, 15)).bitwise_or(sp1.bitwise_and(U8x16.concat_shift_bytes(sp0, sp1, 15))).bitwise_not().bitwise_and(weights)
		v2 = U8x16.load(source, off + 32) ?? s_zero
		id2 = v2.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v2.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v2.eq_lanes(s_us)).bitwise_or(v2.eq_lanes(s_bang))
		sp2 = v2.eq_lanes(s_sp).bitwise_or(v2.eq_lanes(s_nl)).bitwise_or(v2.eq_lanes(s_tab))
		st2 = id2.bitwise_and(U8x16.concat_shift_bytes(id1, id2, 15)).bitwise_or(sp2.bitwise_and(U8x16.concat_shift_bytes(sp1, sp2, 15))).bitwise_not().bitwise_and(weights)
		v3 = U8x16.load(source, off + 48) ?? s_zero
		id3 = v3.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v3.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v3.eq_lanes(s_us)).bitwise_or(v3.eq_lanes(s_bang))
		sp3 = v3.eq_lanes(s_sp).bitwise_or(v3.eq_lanes(s_nl)).bitwise_or(v3.eq_lanes(s_tab))
		st3 = id3.bitwise_and(U8x16.concat_shift_bytes(id2, id3, 15)).bitwise_or(sp3.bitwise_and(U8x16.concat_shift_bytes(sp2, sp3, 15))).bitwise_not().bitwise_and(weights)
		st_ab = st0.even_lanes(st1).plus_wrap(st0.odd_lanes(st1))
		st_cd = st2.even_lanes(st3).plus_wrap(st2.odd_lanes(st3))
		st_q = st_ab.even_lanes(st_cd).plus_wrap(st_ab.odd_lanes(st_cd))
		st_o = st_q.even_lanes(st_q).plus_wrap(st_q.odd_lanes(st_q))
		word = st_o.to_u64x2_bits().get_lane(0)
		$starts = $starts.append(word)
		$count = $count + U8.to_u64(U64.count_one_bits(word))
		$last_id = id3
		$last_sp = sp3
		$off = $off + 64
	}
	if $off < len {
		tail_bytes = source.sublist({ start: $off, len: len - $off }).concat(List.repeat(0, 64))
		off = 0
		v0 = U8x16.load(tail_bytes, off + 0) ?? s_zero
		id0 = v0.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v0.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v0.eq_lanes(s_us)).bitwise_or(v0.eq_lanes(s_bang))
		sp0 = v0.eq_lanes(s_sp).bitwise_or(v0.eq_lanes(s_nl)).bitwise_or(v0.eq_lanes(s_tab))
		st0 = id0.bitwise_and(U8x16.concat_shift_bytes($last_id, id0, 15)).bitwise_or(sp0.bitwise_and(U8x16.concat_shift_bytes($last_sp, sp0, 15))).bitwise_not().bitwise_and(weights)
		v1 = U8x16.load(tail_bytes, off + 16) ?? s_zero
		id1 = v1.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v1.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v1.eq_lanes(s_us)).bitwise_or(v1.eq_lanes(s_bang))
		sp1 = v1.eq_lanes(s_sp).bitwise_or(v1.eq_lanes(s_nl)).bitwise_or(v1.eq_lanes(s_tab))
		st1 = id1.bitwise_and(U8x16.concat_shift_bytes(id0, id1, 15)).bitwise_or(sp1.bitwise_and(U8x16.concat_shift_bytes(sp0, sp1, 15))).bitwise_not().bitwise_and(weights)
		v2 = U8x16.load(tail_bytes, off + 32) ?? s_zero
		id2 = v2.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v2.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v2.eq_lanes(s_us)).bitwise_or(v2.eq_lanes(s_bang))
		sp2 = v2.eq_lanes(s_sp).bitwise_or(v2.eq_lanes(s_nl)).bitwise_or(v2.eq_lanes(s_tab))
		st2 = id2.bitwise_and(U8x16.concat_shift_bytes(id1, id2, 15)).bitwise_or(sp2.bitwise_and(U8x16.concat_shift_bytes(sp1, sp2, 15))).bitwise_not().bitwise_and(weights)
		v3 = U8x16.load(tail_bytes, off + 48) ?? s_zero
		id3 = v3.bitwise_or(s_20).minus_wrap(s_a).lte_lanes(s_25).bitwise_or(v3.minus_wrap(s_0).lte_lanes(s_9)).bitwise_or(v3.eq_lanes(s_us)).bitwise_or(v3.eq_lanes(s_bang))
		sp3 = v3.eq_lanes(s_sp).bitwise_or(v3.eq_lanes(s_nl)).bitwise_or(v3.eq_lanes(s_tab))
		st3 = id3.bitwise_and(U8x16.concat_shift_bytes(id2, id3, 15)).bitwise_or(sp3.bitwise_and(U8x16.concat_shift_bytes(sp2, sp3, 15))).bitwise_not().bitwise_and(weights)
		st_ab = st0.even_lanes(st1).plus_wrap(st0.odd_lanes(st1))
		st_cd = st2.even_lanes(st3).plus_wrap(st2.odd_lanes(st3))
		st_q = st_ab.even_lanes(st_cd).plus_wrap(st_ab.odd_lanes(st_cd))
		st_o = st_q.even_lanes(st_q).plus_wrap(st_q.odd_lanes(st_q))
		word = st_o.to_u64x2_bits().get_lane(0)
		$starts = $starts.append(word)
		$count = $count + U8.to_u64(U64.count_one_bits(word))
		$last_id = id3
		$last_sp = sp3
	}
	{ starts: $starts, count: $count }
}

## Next token-start position strictly after `at`.
next_start : List(U64), U64 -> U64
next_start = |starts, at| {
	from = at + 1
	var $w = from.shr_zf_wrap(6)
	var $bits = (starts.get($w) ?? U64.highest).shr_zf_wrap(U64.to_u8_wrap(from.bitwise_and(63)))
	var $base = from
	while $bits == 0 {
		$w = $w + 1
		$base = $w.shl_wrap(6)
		$bits = starts.get($w) ?? U64.highest
	}
	$base + U8.to_u64(U64.count_trailing_zero_bits($bits))
}

## Bitmask of plain identifier bytes [A-Za-z0-9_!] in a block.
plain_ident_bits : U8x16 -> U16
plain_ident_bits = |v| {
	letter = v.bitwise_or(U8x16.splat(0x20)).minus_wrap(U8x16.splat('a')).lte_lanes(U8x16.splat(25))
	digit = v.minus_wrap(U8x16.splat('0')).lte_lanes(U8x16.splat(9))
	letter.bitwise_or(digit).bitwise_or(v.eq_lanes(U8x16.splat('_'))).bitwise_or(v.eq_lanes(U8x16.splat('!'))).to_bitmask()
}

## End of the run of plain identifier bytes starting at `at`.
ident_end_inline : List(U8), U64 -> U64
ident_end_inline = |source, at| {
	var $i = at
	var $result = at
	var $done = False
	while !$done {
		match U8x16.load(source, $i) {
			Ok(v) => {
				clear = plain_ident_bits(v).bitwise_not()
				if clear != 0 {
					$result = $i + U8.to_u64(U16.count_trailing_zero_bits(clear))
					$done = True
				} else {
					$i = $i + 16
				}
			}
			Err(_) => {
				while $i < source.len() and (is_lower(byte(source, $i)) or is_upper(byte(source, $i)) or is_digit(byte(source, $i)) or byte(source, $i) == '_' or byte(source, $i) == '!') {
					$i = $i + 1
				}
				$result = $i
				$done = True
			}
		}
	}
	$result
}

## End of the run of space/tab/newline bytes starting at `at`.
space_end_inline : List(U8), U64 -> U64
space_end_inline = |source, at| {
	var $i = at
	var $result = at
	var $done = False
	while !$done {
		match U8x16.load(source, $i) {
			Ok(v) => {
				clear = v.eq_lanes(U8x16.splat(' ')).bitwise_or(v.eq_lanes(U8x16.splat('\n'))).bitwise_or(v.eq_lanes(U8x16.splat('\t'))).to_bitmask().bitwise_not()
				if clear != 0 {
					$result = $i + U8.to_u64(U16.count_trailing_zero_bits(clear))
					$done = True
				} else {
					$i = $i + 16
				}
			}
			Err(_) => {
				while $i < source.len() and (byte(source, $i) == ' ' or byte(source, $i) == '\n' or byte(source, $i) == '\t') {
					$i = $i + 1
				}
				$result = $i
				$done = True
			}
		}
	}
	$result
}

## Inline SIMD: first newline or CR at or after `at`, else the end of the source.
comment_end : List(U8), U64 -> U64
comment_end = |source, at| {
	var $i = at
	var $result = source.len()
	var $done = False
	while !$done {
		match U8x16.load(source, $i) {
			Ok(v) => {
				m = v.eq_lanes(U8x16.splat('\n')).bitwise_or(v.eq_lanes(U8x16.splat('\r'))).to_bitmask()
				if m != 0 {
					$result = $i + U8.to_u64(U16.count_trailing_zero_bits(m))
					$done = True
				} else {
					$i = $i + 16
				}
			}
			Err(_) => {
				while $i < source.len() and byte(source, $i) != '\n' and byte(source, $i) != '\r' {
					$i = $i + 1
				}
				$result = $i
				$done = True
			}
		}
	}
	$result
}

utf8_diagnostics_inline : List(U8) -> List(RocSyntax.Diagnostic)
utf8_diagnostics_inline = |source| {
	var $out = []
	var $i = 0
	len = source.len()
	while $i < len {
		high = match U8x16.load(source, $i) {
			Ok(v) => U16.to_u64(v.to_bitmask())
			Err(_) => 0x10000
		}
		if high == 0 {
			$i = $i + 16
		} else {
			at = if (high == 0x10000) $i else $i + U8.to_u64(U64.count_trailing_zero_bits(high))
			b = byte(source, at)
			if b < 0x80 {
				$i = at + 1
			} else {
				width = if (b >= 0xC2 and b <= 0xDF) 2 else if (b >= 0xE0 and b <= 0xEF) 3 else if (b >= 0xF0 and b <= 0xF4) 4 else 0
				var $valid = width > 0 and at + width <= len
				var $j = 1
				while $valid and $j < width {
					c = byte(source, at + $j)
					$valid = c >= 0x80 and c <= 0xBF
					$j = $j + 1
				}
				second = byte(source, at + 1)
				$valid = $valid and !(b == 0xE0 and second < 0xA0) and !(b == 0xED and second >= 0xA0) and !(b == 0xF0 and second < 0x90) and !(b == 0xF4 and second >= 0x90)
				if $valid {
					third = byte(source, at + 2)
					bidi = (b == 0xD8 and second == 0x9C) or (b == 0xE2 and ((second == 0x80 and (third == 0x8E or third == 0x8F or (third >= 0xAA and third <= 0xAE))) or (second == 0x81 and third >= 0xA6 and third <= 0xA9)))
					if bidi {
						$out = push_diagnostic($out, BidiControlInSource, at, at + width)
					}
					if at == 0 and b == 0xEF and second == 0xBB and third == 0xBF {
						$out = push_diagnostic($out, Utf8ByteOrderMark, 0, 3)
					}
					$i = at + width
				} else {
					$out = push_diagnostic($out, InvalidUtf8, at, at + 1)
					$i = at + 1
				}
			}
		}
	}
	$out
}

scalar_space_end : List(U8), U64 -> U64
scalar_space_end = |source, at| {
	var $i = at
	while $i < source.len() and (byte(source, $i) == ' ' or byte(source, $i) == '\n' or byte(source, $i) == '\t') {
		$i = $i + 1
	}
	$i
}

scalar_ident_end : List(U8), U64 -> U64
scalar_ident_end = |source, at| {
	var $i = at
	while $i < source.len() and (is_lower(byte(source, $i)) or is_upper(byte(source, $i)) or is_digit(byte(source, $i)) or byte(source, $i) == '_' or byte(source, $i) == '!') {
		$i = $i + 1
	}
	$i
}

## Hybrid lexer: tight loop for common tokens driven by the stage-1 start index,
## original scalar rules for strings, characters, interpolation, CR, `$`/`@`/non-ASCII names.
lex : List(U8) -> Lexed
lex = |source| {
	len = source.len()
	bom = byte(source, 0) == 0xEF and byte(source, 1) == 0xBB and byte(source, 2) == 0xBF
	index = stage1_starts(source)
	starts = index.starts
	var $tokens = List.with_capacity(index.count + 2)
	var $diagnostics = utf8_diagnostics_inline(source)
	var $i = if (bom) 3 else 0
	var $contexts = []
	if bom {
		$tokens = $tokens.append({ kind: Invalid, start: 0, end: 3 })
	}
	while $i < len {
		start = $i
		b = byte(source, start)
		if b == ' ' or b == '\n' or b == '\t' {
			$i = next_start(starts, start)
			if byte(source, $i) == '\r' {
				while $i < len and is_space(byte(source, $i)) {
					if byte(source, $i) == '\r' and byte(source, $i + 1) != '\n' {
						$diagnostics = push_diagnostic($diagnostics, InvalidByte, $i, $i + 1)
					}
					$i = $i + 1
				}
			}
			$tokens = $tokens.append({ kind: Whitespace, start, end: $i })
		} else if is_lower(b) or is_upper(b) or b == '_' {
			next = byte(source, start + 1)
			if b == '_' and !(is_lower(next) or is_upper(next) or is_digit(next)) {
				$i = start + 1
				$tokens = $tokens.append({ kind: LowerIdent, start, end: $i })
			} else {
				$i = next_start(starts, start)
				c0 = byte(source, $i)
				if c0 == '$' or c0 >= 0x80 {
					while $i < len and is_ident(byte(source, $i)) {
						$i = $i + 1
					}
					var $j = start + 1
					while $j < $i {
						if byte(source, $j) == '$' or byte(source, $j) >= 0x80 {
							$diagnostics = push_diagnostic($diagnostics, InvalidByte, $j, $j + 1)
						}
						$j = $j + 1
					}
				}
				before = if (start > 0) byte(source, start - 1) else 0
				field_name = if is_lower(b) and (before == '.' or before == '?') {
					previous = $tokens.last() ?? { kind: EndOfFile, start: 0, end: 0 }
					previous.end == start and (spelled1(source, previous, '.') or spelled2(source, previous, '.', '?'))
				} else {
					False
				}
				kind = if (field_name) LowerIdent else if (b == '_') LowerIdent else if ($i - start == 3 and b == 'a' and next == 'n' and byte(source, start + 2) == 'd') Operator else if ($i - start == 2 and b == 'o' and next == 'r') Operator else if (is_upper(b)) UpperIdent else if (keyword_swar(source, start, $i)) Keyword else LowerIdent
				$tokens = $tokens.append({ kind, start, end: $i })
			}
		} else if b == '#' {
			$i = comment_end(source, start + 1)
			$tokens = $tokens.append({ kind: Comment, start, end: $i })
		} else if is_digit(b) or (b == '-' and is_digit(byte(source, start + 1)) and (start == 0 or is_space(byte(source, start - 1)) or !can_end_token(source, $tokens.last() ?? { kind: EndOfFile, start: 0, end: 0 }))) {
			number = scan_number(source, start + if (b == '-') 1 else 0)
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
			$tokens = $tokens.append({ kind: Number, start, end: $i })
		} else if b == '"' or (b == '\\' and byte(source, start + 1) == '\\') or (b == '}' and !$contexts.is_empty() and ($contexts.last() ?? { multiline: False, depth: 0 }).depth == 0) {
			# String start, or the string continuation after an interpolation's closing brace.
			resumed = b == '}'
			context = $contexts.last() ?? { multiline: False, depth: 0 }
			multiline = if (resumed) context.multiline else b == '\\' or bytes3(source, start, '"', '"', '"')
			diag_start = if (resumed) start + 1 else start
			if resumed {
				$contexts = $contexts.drop_last(1)
				$tokens = $tokens.append({ kind: InterpolationEnd, start, end: start + 1 })
				$i = start + 1
			} else {
				width = if (b == '\\') 2 else if (multiline) 3 else 1
				$tokens = $tokens.append({ kind: if (multiline) MultilineStringStart else StringStart, start, end: start + width })
				$i = start + width
			}
			part = $i
			var $done = False
			while $i < len and !$done {
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
			} else if !multiline and $i < len and byte(source, $i) == '"' {
				$tokens = $tokens.append({ kind: StringEnd, start: $i, end: $i + 1 })
				$i = $i + 1
			} else if !multiline {
				$diagnostics = push_diagnostic($diagnostics, UnclosedString, diag_start, $i)
			}
		} else if b == '{' or b == '}' {
			kind = if (b == '{') OpenCurly else CloseCurly
			$i = start + 1
			$tokens = $tokens.append({ kind, start, end: $i })
			if !$contexts.is_empty() {
				top = $contexts.len() - 1
				context = $contexts.last() ?? { multiline: False, depth: 0 }
				next_depth = if (b == '{') context.depth + 1 else if (context.depth > 0) context.depth - 1 else 0
				$contexts = $contexts.set(top, { multiline: context.multiline, depth: next_depth }) ?? $contexts
			}
		} else if b == '\'' {
			$i = start + 1
			var $closed = False
			while $i < len and !$closed and byte(source, $i) != '\n' {
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
					} else {
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
			$tokens = $tokens.append({ kind: SingleQuote, start, end: $i })
		} else if b == '$' or b == '@' or b >= 0x80 {
			# Rare name starts: the original identifier rules, unchanged.
			$i = start + 1
			next = byte(source, start + 1)
			bad_dollar = b == '$' and !(is_lower(next) or is_upper(next))
			bad_opaque = b == '@' and !(is_lower(next) or is_upper(next) or is_digit(next) or next == '_' or next >= 0x80)
			var $unusual = False
			if !bad_dollar and !bad_opaque {
				while $i < len and is_ident(byte(source, $i)) {
					c = byte(source, $i)
					$unusual = $unusual or c == '$' or c >= 0x80
					$i = $i + 1
				}
			}
			kind = if (b == '@') UpperIdent else if (bad_dollar) Invalid else if (b == '$' and is_upper(next)) UpperIdent else if (keyword(source, start, $i)) Keyword else LowerIdent
			if bad_dollar or bad_opaque or b >= 0x80 {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i)
			}
			var $j = if ($unusual) start + 1 else $i
			while $j < $i {
				if byte(source, $j) == '$' or byte(source, $j) >= 0x80 {
					$diagnostics = push_diagnostic($diagnostics, InvalidByte, $j, $j + 1)
				}
				$j = $j + 1
			}
			$tokens = $tokens.append({ kind, start, end: $i })
		} else if b == '\r' {
			$i = start + 1
			if byte(source, start + 1) != '\n' {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, start + 1)
			}
			while $i < len and is_space(byte(source, $i)) {
				if byte(source, $i) == '\r' and byte(source, $i + 1) != '\n' {
					$diagnostics = push_diagnostic($diagnostics, InvalidByte, $i, $i + 1)
				}
				$i = $i + 1
			}
			$tokens = $tokens.append({ kind: Whitespace, start, end: $i })
		} else {
			$i = start + 1
			var $kind = match b {
				'(' => if (start > 0 and !is_space(byte(source, start - 1))) NoSpaceOpenRound else OpenRound
				')' => CloseRound
				'[' => OpenSquare
				']' => CloseSquare
				',' | '.' | ':' => Punctuation
				'+' | '-' | '*' | '/' | '%' | '^' | '=' | '!' | '?' | '<' | '>' | '|' | '&' | '\\' => Operator
				_ => Invalid
			}
			width = if ($kind == Operator or $kind == Punctuation) operator_width(source, start) else 1
			if width > 1 {
				$i = start + width
				$kind = if (b == '.' and width == 3 and (byte(source, start + 2) == '<' or byte(source, start + 2) == '=')) Operator else if (b == '.' or b == ':') Punctuation else Operator
			}
			if $kind == Invalid {
				$diagnostics = push_diagnostic($diagnostics, InvalidByte, start, $i)
			}
			$tokens = $tokens.append({ kind: $kind, start, end: $i })
		}
		# A lone space after a token is a whitespace token of its own; take it
		# here rather than on another trip through the dispatch above.
		if byte(source, $i) == ' ' and !is_space(byte(source, $i + 1)) {
			$tokens = $tokens.append({ kind: Whitespace, start: $i, end: $i + 1 })
			$i = $i + 1
		}
	}
	if !$contexts.is_empty() {
		$diagnostics = push_diagnostic($diagnostics, UnclosedDelimiter, len, len)
	}
	{ tokens: $tokens.append({ kind: EndOfFile, start: len, end: len }), diagnostics: $diagnostics, unsupported: False }
}

## SWAR keyword test: compare the identifier's first 8 bytes as one U64 (simdjson-style atom check).
keyword_swar : List(U8), U64, U64 -> Bool
keyword_swar = |source, start, end| {
	len = end - start
	if len < 2 or len > 10 {
		False
	} else {
		match U64.from_le_bytes(source, start) {
			Ok(w) => {
				lo = if (len >= 8) w else w.bitwise_and((1.U64).shl_wrap(U64.to_u8_wrap(len.shl_wrap(3))) - 1)
				if (len == 2) lo == 0x0000000000007361 or lo == 0x0000000000006669 or lo == 0x0000000000006e69
				else if (len == 3) lo == 0x0000000000707061 or lo == 0x0000000000676264 or lo == 0x0000000000726f66 or lo == 0x0000000000736168 or lo == 0x0000000000726176
				else if (len == 4) lo == 0x0000000065736c65 or lo == 0x0000000021726f66 or lo == 0x0000000068746977
				else if (len == 5) lo == 0x0000006873617263 or lo == 0x000000686374616d or lo == 0x0000006572656877 or lo == 0x000000656c696877 or lo == 0x0000006b61657262
				else if (len == 6) lo == 0x0000746365707865 or lo == 0x0000646574736f68 or lo == 0x000074726f706d69 or lo == 0x0000656c75646f6d or lo == 0x00006e7275746572
				else if (len == 7) lo == 0x007365736f707865 or lo == 0x007374726f706d69 or lo == 0x006567616b636170 or lo == 0x0073746567726174
				else if (len == 8) lo == 0x676e69736f707865 or lo == 0x736567616b636170 or lo == 0x6d726f6674616c70 or lo == 0x73656469766f7270 or lo == 0x7365726975716572
				else if (len == 9) (lo == 0x65746172656e6567 and byte(source, start + 8) == 's') or (lo == 0x6361667265746e69 and byte(source, start + 8) == 'e')
				else lo == 0x6e656d656c706d69 and byte(source, start + 8) == 't' and byte(source, start + 9) == 's'
			}
			Err(_) => keyword_bytes(source, start, end)
		}
	}
}

ParseMode : [FileMode, ExpressionMode, StatementMode, HeaderMode]

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
	p.tokens.fold(0, |at, token| if (token.start == at) token.end else 999999) == p.source.len() and p.tokens.any(|t| t.kind == Comment) and p.status == Complete
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
			$base = if (prefix == 'x') 16 else if (prefix == 'o') 8 else 2
			$i = $prefix_at + 1
		} else if is_digit(prefix) {
			$valid = False
		}
	}
	var $digits = 0.U64
	var $done = False
	while $i < source.len() and !$done {
		b = byte(source, $i)
		digit = if (is_digit(b)) U8.to_u64(b - '0') else if (b >= 'a' and b <= 'f') U8.to_u64(b - 'a') + 10 else if (b >= 'A' and b <= 'F') U8.to_u64(b - 'A') + 10 else 99
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
			digit = if (is_digit(c)) U8.to_u64(c - '0') else if (c >= 'a' and c <= 'f') U8.to_u64(c - 'a') + 10 else if (c >= 'A' and c <= 'F') U8.to_u64(c - 'A') + 10 else 99
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
		{ end: $i + if (closed) 1 else 0, valid: $valid and closed and $digits > 0 and $value <= 0x10FFFF and !($value >= 0xD800 and $value <= 0xDFFF) }
	} else {
		{ end: if (start + 1 < source.len() and b != '\n') start + 2 else start + 1, valid: b == 'n' or b == 'r' or b == 't' or b == '\\' or b == '"' or b == '\'' or b == '$' }
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

# Dedicated type syntax never delegates to value-expression parsing, except
# record-field defaults, which the language explicitly defines as expressions.
TypeMode : [FullType, TypeItem, TypeArgument]

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
can_end_token = |source, token| can_end_expression(token.kind) or spelled3(source, token, '.', '.', '.')

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
	if (a == '.' and b == '.' and (c == '.' or c == '<' or c == '=')) 3
	else if ((a == '-' and b == '>') or (a == '=' and (b == '>' or b == '=')) or (a == '<' and (b == '-' or b == '=')) or (a == '>' and b == '=') or (a == '|' and b == '>') or (a == '!' and b == '=') or (a == '?' and b == '?') or (a == '/' and b == '/') or (a == ':' and (b == '=' or b == ':')) or (a == '.' and (b == '.' or b == '*' or b == '?'))) 2
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

expect {
	["-!h", "-'i'", "match 42 {}", "person.@"].all(|source| RocSyntax.parse_expr_str(source).status != Complete) and ["F\n", "modu\n", "S\n0\n", "x = _\n\ny = { a: _ + 1 }"].all(|source| RocSyntax.parse_str(source).status != Complete)
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

## Byte checks for short fixed spellings; `token_spelling` builds a `Str`.
bytes3 : List(U8), U64, U8, U8, U8 -> Bool
bytes3 = |source, at, a, b, c| byte(source, at) == a and byte(source, at + 1) == b and byte(source, at + 2) == c

spelled1 : List(U8), RocSyntax.Token, U8 -> Bool
spelled1 = |source, token, a| token.end - token.start == 1 and byte(source, token.start) == a

spelled2 : List(U8), RocSyntax.Token, U8, U8 -> Bool
spelled2 = |source, token, a, b| token.end - token.start == 2 and byte(source, token.start) == a and byte(source, token.start + 1) == b

spelled3 : List(U8), RocSyntax.Token, U8, U8, U8 -> Bool
spelled3 = |source, token, a, b, c| token.end - token.start == 3 and bytes3(source, token.start, a, b, c)

token_spelling : List(U8), RocSyntax.Token, Str -> Bool
token_spelling = |source, token, text| token.end - token.start == text.count_utf8_bytes() and matches(source, token.start, text)

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

expect {
	["|_| 1", "|Some(x), (a, b), {name}, [first, .. as rest]| x", "|var count| count", "|| {}", "match x { _ => 0 }", "match x { Some(a) | Other(a) => a, None => 0 }", "match x { {a: Some(v), ..rest} => v }", "match x { [first, .. as rest] if first > 0 => rest, _ => [] }", "|Thing.(value)| value", "|Thing.{field}| field", "|(a, _) as pair| pair"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["|1 + 2| 0", "match x { 1 + 2 => 0 }", "match x { [a b] => 0 }", "|var _| 0", "match x { [.. old] => 0 }", "match x { _ as Upper => 0 }"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
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
			lexed.status == Complete and lexed.tokens.drop_last(1).all(|t| t.start < t.end) and lexed.tokens.fold(0.U64, |end, t| if (t.start == end) t.end else 18446744073709551615) == source.count_utf8_bytes()
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

expect {
	["\"hello \${name}!\"", "\"\${{ a: 1 }}\"", "\"\${\"inner \${x}\"}\"", "\\\\hello\n\\\\world", "\"\"\"hello\n\"\"\"world", "42.U8", "1.0.F64", "'a'.U32", "\"hello\".MyText", "0xFF.Gui.Color"].all(|source| RocSyntax.parse_expr_str(source).status == Complete)
}

expect {
	["\"\${}\"", "\"\${x\"", "\"\${x}", "1 .U8"].all(|source| RocSyntax.parse_expr_str(source).status != Complete)
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
		if (parts.len() == 1) valid_numbers else {
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

expect {
	RocSyntax.parse_header_str("").status == Complete and RocSyntax.parse_expr_str("match text { \"hello \${name}\" => name, \"\${_}\" => \"\" }").status == Complete and RocSyntax.parse_expr_str("match text { \"\${a + b}\" => 0 }").status != Complete and RocSyntax.parse_expr_str("match text { \"x\".Str => 0 }").status != Complete
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

expect {
	# Empty names, requires type aliases, symbol paths, and target configuration.
	["platform \"p\" requires {} exposes [] packages {} provides {}", "platform \"\" requires {} exposes [] packages {} provides {}", "platform \"name\\ntext\" requires {} exposes [] packages {} provides {}", "platform \"p\" requires {}{} exposes [] packages {} provides {}", "platform \"p\" requires {main : U8} exposes [] packages {} provides {}", "platform \"p\" requires {}{main : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[Model:model] for main : model -> U64} exposes [] packages {} provides {}", "platform \"p\" requires {[A:a, B:b,] for run : a,b => Str, other : U8,} exposes [] packages {} provides {}", "platform \"p\" requires {[] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[$A:$a] for $run : $a -> U8} exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [run, Foo.*, Foo.bar as Bar] packages {} provides {}", "platform \"p\" requires {} exposes [] packages {dep: \"url\", roc: \"1.0.0\"} provides {}", "platform \"p\" requires {} exposes [] packages {} provides {\"\": run}", "platform \"p\" requires {} exposes [] packages {} provides {\"main\": Main.run!, \"more\": Main.Nested.call}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": Host.Idx.get!}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:\"targets/\", linux:{inputs:[app,\"start.o\",custom], output:Exe, shared:True, level:3, flags:[\"-lc\",Symbol,thing,[1,2]]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{output, inputs, custom}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{n:-1,n2:0xFE,n3:0_xFE}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{} windows:{}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[\"\",app,win_gui]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:[]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:\"a\",inputs_dir:\"b\"}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:\"\${x}\"}}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {} targets: {}"].all(|source| RocSyntax.parse_header_str(source).status == Complete)
}

expect {
	# Platform sections are ordered and their data atoms have restricted grammars.
	["platform \"\${x}\" requires {} exposes [] packages {} provides {}", "platform \"\"\"name requires {} exposes [] packages {} provides {}", "platform p requires {} exposes [] packages {} provides {}", "platform \"p\" requires {[A:_a] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[A:U8] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[a:a] for run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {[A:a] run : U8} exposes [] packages {} provides {}", "platform \"p\" requires {run U8} exposes [] packages {} provides {}", "platform \"p\" requires {run : 1} exposes [] packages {} provides {}", "platform \"p\" requires {run : U8 other : U8} exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [1] packages {} provides {}", "platform \"p\" requires {} exposes {} packages {} provides {}", "platform \"p\" requires {} exposing [] packages {} provides {}", "platform \"p\" requires {} exposes [] packages {dep: platform \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc: \"bad\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc: \"1.0.0\",roc:\"1.0.1\"} provides {}", "platform \"p\" requires {} exposes [] packages {roc} provides {}", "platform \"p\" requires {} exposes [] packages {_dep: \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {x: \"url\" y: \"url\"} provides {}", "platform \"p\" requires {} exposes [] packages {dep: _} provides {}", "platform \"p\" requires {} exposes [] packages {} provides {\"symbol\": $Module.$method}", "platform \"p\" requires {} exposes [] packages {} provides {\"\${x}\": run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Upper}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module .run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module. run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": _run}", "platform \"p\" requires {} exposes [] packages {} provides {x: run}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": run(1)}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": Module.lower.more}", "platform \"p\" requires {} exposes [] packages {} provides {\"x\": run \"y\": other}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted []", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": Upper}", "platform \"p\" requires {} exposes [] packages {} provides {} hosted {\"host\": call} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{output:Exe inputs:[app]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {other:\"dir/\"}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {inputs_dir:[]}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:[]}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[1]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:[Upper]}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{inputs:\"file\"}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:1.5}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:app}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:Module.name}}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {linux:{value:{}}}", "platform \"p\" missing_requires {} exposes [] packages {} provides {}", "platform \"p\" requires {} missing_exposes [] packages {} provides {}", "platform \"p\" requires {} exposes [] missing_packages {} provides {}", "platform \"p\" requires {} exposes [] packages {} missing_provides {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {} hosted {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets {}", "platform \"p\" requires {} exposes [] packages {} provides {} targets: {", "platform \"p\" requires {} exposes [] packages {} provides {} provides {}"].all(|source| RocSyntax.parse_header_str(source).status != Complete)
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
	start = t.start + if (byte(source, t.start) == '-') 1 else 0
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
	["0xff.0", "0b10.00", "x -> _name", "x |> _", "match value { @Foo => 1 }", "|@A| 0", "match value { A .B => 0 }", "module [A .b]", "module [A .B]", "module [x as _name]", "module [x as _]", "module [A as @A]"].all(|source| (if (source.starts_with("module")) RocSyntax.parse_str(source) else RocSyntax.parse_expr_str(source)).status != Complete)
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

# ---- Parser core -------------------------------------------------------------
# Growing buffers are never stored in a record that is passed into a function.
# Each parser takes `pos`, `nodes`, `children` and `diags` as separate
# parameters and returns them in a flat result record that its caller
# destructures immediately. Optimized Roc builds then keep the buffers uniquely
# owned and append in place. Bundling them in a threaded state record made the
# compiler infer that record as borrowed, so callers kept a reference across
# each call and every append copied (see roc-lang/roc#12327).

Ctx : { source : List(U8), tokens : List(RocSyntax.Token), texts : List(TokenText), in_guard : Bool }

## Result of a node-producing parser.
R : { pos : U64, nodes : List(RocSyntax.Node), children : List(U64), diags : List(RocSyntax.Diagnostic), node : U64 }

## Result of a delimited item collection.
## Expression collection: the first item is kept apart so the common
## one-item case (calls, parentheses) does not allocate an id list.
X : { pos : U64, nodes : List(RocSyntax.Node), children : List(U64), diags : List(RocSyntax.Diagnostic), count : U64, first : U64, rest : List(U64), end : U64, comma : Bool }

C : { pos : U64, nodes : List(RocSyntax.Node), children : List(U64), diags : List(RocSyntax.Diagnostic), ids : List(U64), end : U64, comma : Bool }

## Result of an emitted node.
E : { nodes : List(RocSyntax.Node), children : List(U64), node : U64 }

## Token at a position, or end of file.
tok : Ctx, U64 -> RocSyntax.Token
tok = |ctx, pos| ctx.tokens.get(pos) ?? { kind: EndOfFile, start: ctx.source.len(), end: ctx.source.len() }

## First non-trivia position at or after `pos`.
sig : Ctx, U64 -> U64
sig = |ctx, pos| significant_position(ctx.tokens, pos)

## Whether the token at `pos` is spelled exactly `text`.
spell : Ctx, U64, Str -> Bool
spell = |ctx, pos, text| token_spelling(ctx.source, tok(ctx, pos), text)

## Record a diagnostic. Output is capped at 256 when parsing finishes; one
## beyond-cap entry per status-affecting code is retained so status stays exact.
diag : List(RocSyntax.Diagnostic), RocSyntax.DiagnosticCode, U64, U64 -> List(RocSyntax.Diagnostic)
diag = |diags, code, start, end| {
	if diags.len() < 256 {
		diags.append({ code, start, end })
	} else if (code == UnsupportedSyntax or code == ResourceLimit) and !diags.drop_first(256).any(|d| d.code == code) {
		diags.append({ code, start, end })
	} else {
		diags
	}
}

## Append `ids` one at a time: `concat` reallocates to an exact fit, so
## appending through it gives up the geometric growth `append` keeps.
append_ids : List(U64), List(U64) -> List(U64)
append_ids = |children, ids| {
	var $c = children
	var $i = 0
	while $i < ids.len() {
		$c = $c.append(ids.get($i) ?? 0)
		$i = $i + 1
	}
	$c
}

## Append a node whose children are `ids`, returning its index.
emit : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, List(U64) -> E
emit = |nodes, children, kind, start, end, ids| {
	index = nodes.len()
	{ nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: ids.len() }), children: append_ids(children, ids), node: index }
}

## Append a node and return it as a parser result at `pos`.
finish : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, List(U64) -> R
finish = |pos, nodes, children, diags, kind, start, end, ids| {
	index = nodes.len()
	{ pos, nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: ids.len() }), children: append_ids(children, ids), diags, node: index }
}

missing_node : RocSyntax.Node
missing_node = { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }

nget : List(RocSyntax.Node), U64 -> RocSyntax.Node
nget = |nodes, id| nodes.get(id) ?? missing_node

nstart : List(RocSyntax.Node), U64 -> U64
nstart = |nodes, id| (nodes.get(id) ?? missing_node).start

nend : List(RocSyntax.Node), U64 -> U64
nend = |nodes, id| (nodes.get(id) ?? missing_node).end

## Spellings the grammar compares, classified once per token after lexing so
## parsing compares tags rather than validating and comparing source text.
TokenText : [OtherText, Dot, Comma, Colon, Underscore, DotDot, FatArrow, Arrow, As, Equals, DotQuestion, Question, Bar, Slash, Where, Var, ForBang, Dbg, Crash, App, Pipe, Return, Platform, Package, If, Hosted, For, Break, DotStar, Ellipsis, ColonEquals, DoubleColon, Minus, While, Module, Match, In, Import, Exposing, Expect, Else, DoubleQuestion, Bang, Exposes, Packages, Provides, Requires, Targets, Star, DoubleSlash, Percent, Plus, EqEq, NotEq, Less, Greater, LessEq, GreaterEq, And, Or, RangeExclusive, RangeInclusive]

token_text : List(U8), RocSyntax.Token -> TokenText
token_text = |source, t| {
	# Brackets, numbers, uppercase names and trivia never spell a `TokenText`.
	match t.kind {
		LowerIdent | Keyword | Operator | Punctuation | StringPart | Invalid => token_text_bytes(source, t.start, t.end)
		_ => OtherText
	}
}

# Generated byte comparisons avoid building a string per token.
token_text_bytes : List(U8), U64, U64 -> TokenText
token_text_bytes = |source, start, end| {
	len = end - start
	if len == 0 or len > 8 {
		OtherText
	} else {
		b0 = byte(source, start)
		if len == 1 {
			if (b0 == '.') Dot else if (b0 == ',') Comma else if (b0 == ':') Colon else if (b0 == '_') Underscore else if (b0 == '=') Equals else if (b0 == '?') Question else if (b0 == '|') Bar else if (b0 == '/') Slash else if (b0 == '-') Minus else if (b0 == '!') Bang else if (b0 == '*') Star else if (b0 == '%') Percent else if (b0 == '+') Plus else if (b0 == '<') Less else if (b0 == '>') Greater else OtherText
		} else if len == 2 {
			b1 = byte(source, start + 1)
			if (b0 == '.' and b1 == '.') DotDot else if (b0 == '=' and b1 == '>') FatArrow else if (b0 == '-' and b1 == '>') Arrow else if (b0 == 'a' and b1 == 's') As else if (b0 == '.' and b1 == '?') DotQuestion else if (b0 == '|' and b1 == '>') Pipe else if (b0 == 'i' and b1 == 'f') If else if (b0 == '.' and b1 == '*') DotStar else if (b0 == ':' and b1 == '=') ColonEquals else if (b0 == ':' and b1 == ':') DoubleColon else if (b0 == 'i' and b1 == 'n') In else if (b0 == '?' and b1 == '?') DoubleQuestion else if (b0 == '/' and b1 == '/') DoubleSlash else if (b0 == '=' and b1 == '=') EqEq else if (b0 == '!' and b1 == '=') NotEq else if (b0 == '<' and b1 == '=') LessEq else if (b0 == '>' and b1 == '=') GreaterEq else if (b0 == 'o' and b1 == 'r') Or else OtherText
		} else if len == 3 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			if (b0 == 'v' and b1 == 'a' and b2 == 'r') Var else if (b0 == 'd' and b1 == 'b' and b2 == 'g') Dbg else if (b0 == 'a' and b1 == 'p' and b2 == 'p') App else if (b0 == 'f' and b1 == 'o' and b2 == 'r') For else if (b0 == '.' and b1 == '.' and b2 == '.') Ellipsis else if (b0 == 'a' and b1 == 'n' and b2 == 'd') And else if (b0 == '.' and b1 == '.' and b2 == '<') RangeExclusive else if (b0 == '.' and b1 == '.' and b2 == '=') RangeInclusive else OtherText
		} else if len == 4 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			if (b0 == 'f' and b1 == 'o' and b2 == 'r' and b3 == '!') ForBang else if (b0 == 'e' and b1 == 'l' and b2 == 's' and b3 == 'e') Else else OtherText
		} else if len == 5 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			if (b0 == 'w' and b1 == 'h' and b2 == 'e' and b3 == 'r' and b4 == 'e') Where else if (b0 == 'c' and b1 == 'r' and b2 == 'a' and b3 == 's' and b4 == 'h') Crash else if (b0 == 'b' and b1 == 'r' and b2 == 'e' and b3 == 'a' and b4 == 'k') Break else if (b0 == 'w' and b1 == 'h' and b2 == 'i' and b3 == 'l' and b4 == 'e') While else if (b0 == 'm' and b1 == 'a' and b2 == 't' and b3 == 'c' and b4 == 'h') Match else OtherText
		} else if len == 6 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			if (b0 == 'r' and b1 == 'e' and b2 == 't' and b3 == 'u' and b4 == 'r' and b5 == 'n') Return else if (b0 == 'h' and b1 == 'o' and b2 == 's' and b3 == 't' and b4 == 'e' and b5 == 'd') Hosted else if (b0 == 'm' and b1 == 'o' and b2 == 'd' and b3 == 'u' and b4 == 'l' and b5 == 'e') Module else if (b0 == 'i' and b1 == 'm' and b2 == 'p' and b3 == 'o' and b4 == 'r' and b5 == 't') Import else if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'e' and b4 == 'c' and b5 == 't') Expect else OtherText
		} else if len == 7 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			if (b0 == 'p' and b1 == 'a' and b2 == 'c' and b3 == 'k' and b4 == 'a' and b5 == 'g' and b6 == 'e') Package else if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'o' and b4 == 's' and b5 == 'e' and b6 == 's') Exposes else if (b0 == 't' and b1 == 'a' and b2 == 'r' and b3 == 'g' and b4 == 'e' and b5 == 't' and b6 == 's') Targets else OtherText
		} else if len == 8 {
			b1 = byte(source, start + 1)
			b2 = byte(source, start + 2)
			b3 = byte(source, start + 3)
			b4 = byte(source, start + 4)
			b5 = byte(source, start + 5)
			b6 = byte(source, start + 6)
			b7 = byte(source, start + 7)
			if (b0 == 'p' and b1 == 'l' and b2 == 'a' and b3 == 't' and b4 == 'f' and b5 == 'o' and b6 == 'r' and b7 == 'm') Platform else if (b0 == 'e' and b1 == 'x' and b2 == 'p' and b3 == 'o' and b4 == 's' and b5 == 'i' and b6 == 'n' and b7 == 'g') Exposing else if (b0 == 'p' and b1 == 'a' and b2 == 'c' and b3 == 'k' and b4 == 'a' and b5 == 'g' and b6 == 'e' and b7 == 's') Packages else if (b0 == 'p' and b1 == 'r' and b2 == 'o' and b3 == 'v' and b4 == 'i' and b5 == 'd' and b6 == 'e' and b7 == 's') Provides else if (b0 == 'r' and b1 == 'e' and b2 == 'q' and b3 == 'u' and b4 == 'i' and b5 == 'r' and b6 == 'e' and b7 == 's') Requires else OtherText
		} else {
			OtherText
		}
	}
}

is_text : Ctx, U64, TokenText -> Bool
is_text = |ctx, pos, text| (ctx.texts.get(pos) ?? OtherText) == text

## Append a node whose children are `first` followed by `rest`.
emit_after : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, U64, List(U64) -> E
emit_after = |nodes, children, kind, start, end, first, rest| {
	index = nodes.len()
	{ nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: rest.len() + 1 }), children: append_ids(children.append(first), rest), node: index }
}

## Append a node whose children are `lead` then a collection's items.
emit_items : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, U64, U64, U64, List(U64) -> E
emit_items = |nodes, children, kind, start, end, lead, count, first, rest| {
	index = nodes.len()
	children_start = children.len()
	c1 = children.append(lead)
	c2 = if (count > 0) append_ids(c1.append(first), rest) else c1
	{ nodes: nodes.append({ kind, start, end, children_start, children_len: count + 1 }), children: c2, node: index }
}

## Append a node whose children are `lead` then a collection's items.
finish_lead_items : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64, U64, U64, List(U64) -> R
finish_lead_items = |pos, nodes, children, diags, kind, start, end, lead, count, first, rest| {
	index = nodes.len()
	children_start = children.len()
	c1 = children.append(lead)
	c2 = if (count > 0) append_ids(c1.append(first), rest) else c1
	{ pos, nodes: nodes.append({ kind, start, end, children_start, children_len: count + 1 }), children: c2, diags, node: index }
}

## Append a node whose children are a collection's items.
finish_items : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64, U64, List(U64) -> R
finish_items = |pos, nodes, children, diags, kind, start, end, count, first, rest| {
	index = nodes.len()
	children_start = children.len()
	c1 = if (count > 0) append_ids(children.append(first), rest) else children
	{ pos, nodes: nodes.append({ kind, start, end, children_start, children_len: count }), children: c1, diags, node: index }
}

finish_after : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64, List(U64) -> R
finish_after = |pos, nodes, children, diags, kind, start, end, first, rest| {
	index = nodes.len()
	{ pos, nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: rest.len() + 1 }), children: append_ids(children.append(first), rest), diags, node: index }
}

## Append a node with one child, without a transient child list.
emit1 : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, U64 -> E
emit1 = |nodes, children, kind, start, end, a| {
	index = nodes.len()
	{ nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 1 }), children: children.append(a), node: index }
}

## Append a node with two children, without a transient child list.
emit2 : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, U64, U64 -> E
emit2 = |nodes, children, kind, start, end, a, b| {
	index = nodes.len()
	{ nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 2 }), children: children.append(a).append(b), node: index }
}

emit3 : List(RocSyntax.Node), List(U64), RocSyntax.NodeKind, U64, U64, U64, U64, U64 -> E
emit3 = |nodes, children, kind, start, end, a, b, c| {
	index = nodes.len()
	{ nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 3 }), children: children.append(a).append(b).append(c), node: index }
}

finish3 : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64, U64, U64 -> R
finish3 = |pos, nodes, children, diags, kind, start, end, a, b, c| {
	index = nodes.len()
	{ pos, nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 3 }), children: children.append(a).append(b).append(c), diags, node: index }
}

finish1 : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64 -> R
finish1 = |pos, nodes, children, diags, kind, start, end, a| {
	index = nodes.len()
	{ pos, nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 1 }), children: children.append(a), diags, node: index }
}

finish2 : U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64, U64, U64, U64 -> R
finish2 = |pos, nodes, children, diags, kind, start, end, a, b| {
	index = nodes.len()
	{ pos, nodes: nodes.append({ kind, start, end, children_start: children.len(), children_len: 2 }), children: children.append(a).append(b), diags, node: index }
}

# ---- Entry, statements and expressions ---------------------------------------

source_token : Ctx, U64 -> RocSyntax.Token
source_token = |ctx, pos| tok(ctx, pos)

# Exact binding powers from src/parse/Parser.zig at 7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5.
binding : Ctx, U64, TokenText -> { left : U64, right : U64 }
binding = |ctx, pos, text| {
	match text {
		Star | Slash | DoubleSlash | Percent => { left: 32, right: 33 }
		Plus => { left: 22, right: 23 }
		Minus if binary_minus(ctx, pos) => { left: 22, right: 23 }
		DoubleQuestion => { left: 20, right: 21 }
		Question => { left: 18, right: 19 }
		EqEq => { left: 17, right: 17 }
		NotEq => { left: 15, right: 15 }
		Less => { left: 13, right: 13 }
		Greater => { left: 11, right: 11 }
		LessEq => { left: 9, right: 9 }
		GreaterEq => { left: 7, right: 7 }
		And => { left: 6, right: 5 }
		Or => { left: 4, right: 3 }
		RangeExclusive | RangeInclusive => { left: 2, right: 3 }
		_ => { left: 0, right: 0 }
	}
}

## Texts that may extend an expression: suffixes, arrows and binary operators.
continues_expression : TokenText -> Bool
continues_expression = |text| {
	match text {
		Dot | DotQuestion | Arrow | FatArrow | Pipe | Question | DotDot | DotStar | Star | Slash | DoubleSlash | Percent | Plus | Minus | DoubleQuestion | EqEq | NotEq | Less | Greater | LessEq | GreaterEq | And | Or | RangeExclusive | RangeInclusive => True
		_ => False
	}
}

expression : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64, Bool -> R
expression = |ctx, initial, nodes, children, diags, minimum, depth, punned| {
	s = sig(ctx, initial)
	first = tok(ctx, s)
	sync = declaration_sync(ctx, s)
	if sync.sync {
		return finish(s, nodes, children, diag(diags, UnexpectedToken, sync.start, sync.end), ErrorNode, sync.end, sync.end, [])
	}
	if depth >= 128 {
		return finish(s + if (first.kind == EndOfFile) 0 else 1, nodes, children, diag(diags, ResourceLimit, first.start, first.end), ErrorNode, first.start, first.end, [])
	}
	next_pos = significant_position(ctx.tokens, s + 1)
	package_qualified = (minimum == 100 or minimum == 101) and first.kind == LowerIdent and is_text(ctx, next_pos, Dot) and source_token(ctx, next_pos + 1).kind == UpperIdent
	{ pos: built_state, nodes: n1, children: c1, diags: d1, node: built_node } = primary(ctx, s, nodes, children, diags, depth, punned)
	grouped_tuple = (minimum == 100 or minimum == 101) and nget(n1, built_node).kind == Tuple
	var $s = built_state
	var $left = built_node
	var $qualified = first.kind == UpperIdent or package_qualified
	var $done = False
	var $target_done = False
	var $n = n1
	var $c = c1
	var $d = if (grouped_tuple) diag(d1, UnexpectedToken, first.start, first.end) else d1
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		lt = ctx.texts.get(look) ?? OtherText
		# Most operands end at a token no suffix or operator rule accepts.
		if t.kind != NoSpaceOpenRound and !continues_expression(lt) {
			$s = look
			$done = True
		} else {
			bp = binding(ctx, look, lt)
			arrow_name = tok(ctx, look + 1)
			left_end = nend($n, $left)
			arrow_field = lt == Dot and t.start == left_end and (($qualified and (arrow_name.kind == LowerIdent or arrow_name.kind == UpperIdent)) or (first.kind == LowerIdent and $left == built_node and arrow_name.kind == LowerIdent))
			pipe_boundary = (minimum == 100 or minimum == 101) and t.start > left_end and ((lt == Dot and (arrow_name.kind == LowerIdent or arrow_name.kind == UpperIdent or arrow_name.kind == Number)) or lt == DotQuestion)
			if pipe_boundary or ((minimum >= 100 or ctx.in_guard) and (lt == Arrow or lt == FatArrow or lt == Pipe)) or (minimum == 101 and (($target_done and t.kind != NoSpaceOpenRound) or lt == Question or ((lt == Dot or lt == DotQuestion) and !arrow_field))) {
				$s = look
				$done = True
			} else if lt == Arrow or lt == FatArrow or lt == Pipe {
				pipe = lt == Pipe
				rhs = sig(ctx, look + 1)
				rhs_token = tok(ctx, rhs)
				literal = rhs_token.kind == Number or rhs_token.kind == StringStart or rhs_token.kind == MultilineStringStart or rhs_token.kind == SingleQuote or rhs_token.kind == OpenSquare or rhs_token.kind == OpenCurly
				valid_start = (rhs_token.kind == LowerIdent and byte(ctx.source, rhs_token.start) != '_') or rhs_token.kind == UpperIdent or rhs_token.kind == OpenRound or rhs_token.kind == NoSpaceOpenRound or (pipe and literal)
				if !valid_start {
					$d = diag($d, UnexpectedToken, rhs_token.start, rhs_token.end)
				}
				{ pos: target_state, nodes: $n, children: $c, diags: $d, node: target_node } = expression(ctx, rhs, $n, $c, $d, if (pipe) 100 else 101, depth + 1, False)
				method = pipe and method_target(ctx, $n, $c, target_node)
				if pipe and literal and !pipe_has_method(ctx, $n, $c, target_node) {
					target_end = nend($n, target_node)
					$d = diag($d, UnexpectedToken, rhs_token.start, target_end)
				}
				start = nstart($n, $left)
				end = nend($n, target_node)
				{ nodes: $n, children: $c, node: arrow_node } = emit2($n, $c, if (method) MethodArrowCall else ArrowCall, start, end, $left, target_node)
				$s = target_state
				$left = arrow_node
				$qualified = False
			} else if lt == Question and t.start == left_end {
				start = nstart($n, $left)
				{ nodes: $n, children: $c, node: suffix_node } = emit1($n, $c, Suffix, start, t.end, $left)
				$s = look + 1
				$left = suffix_node
				$qualified = False
			} else if t.kind == NoSpaceOpenRound {
				if is_optional_access(ctx, $n, $c, $left) {
					$d = diag($d, UnexpectedToken, t.start, t.end)
				}
				start = nstart($n, $left)
				{ pos: args_state, nodes: $n, children: $c, diags: $d, count: args_count, first: args_first, rest: args_rest, end: args_end, comma: _ } = collection(ctx, look + 1, $n, $c, $d, CloseRound, depth + 1)
				{ nodes: $n, children: $c, node: call_node } = emit_items($n, $c, Call, start, args_end, $left, args_count, args_first, args_rest)
				$s = args_state
				$left = call_node
				$qualified = False
				$target_done = minimum == 101
				question_state = sig(ctx, args_state)
				if minimum == 100 and !method_target(ctx, $n, $c, call_node) and is_text(ctx, question_state, Question) and tok(ctx, question_state).start == args_end {
					$done = True
				}
			} else if lt == Dot and tok(ctx, sig(ctx, look + 1)).kind == OpenCurly {
				opening_state = sig(ctx, look + 1)
				{ pos: backing_state, nodes: $n, children: $c, diags: $d, node: backing_node } = curly(ctx, opening_state + 1, $n, $c, $d, tok(ctx, opening_state).start, depth + 1, True, True)
				start = nstart($n, $left)
				end = nend($n, backing_node)
				{ nodes: $n, children: $c, node: nominal_node } = emit2($n, $c, NominalRecord, start, end, $left, backing_node)
				$s = backing_state
				$left = nominal_node
				$qualified = False
			} else if lt == Dot and $qualified and tok(ctx, look + 1).kind == NoSpaceOpenRound {
				opening_state = look + 1
				{ pos: args_state, nodes: $n, children: $c, diags: $d, count: args_count, first: args_first, rest: args_rest, end, comma: _ } = collection(ctx, opening_state + 1, $n, $c, $d, CloseRound, depth + 1)
				start = nstart($n, $left)
				{ nodes: $n, children: $c, node: nominal_node } = emit_items($n, $c, NominalApply, start, end, $left, args_count, args_first, args_rest)
				$s = args_state
				$left = nominal_node
				$qualified = False
			} else if lt == Dot or lt == DotQuestion {
				optional = lt == DotQuestion
				field_state = look + 1
				field = tok(ctx, field_state)
				valid_name = (field.kind == LowerIdent and is_lower(byte(ctx.source, field.start))) or ($qualified and field.kind == UpperIdent and is_upper(byte(ctx.source, field.start)))
				valid_index = field.kind == Number and ctx.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit)
				if (valid_name and (!optional or field.kind == LowerIdent)) or (valid_index and !optional) {
					start = nstart($n, $left)
					{ nodes: $n, children: $c, node: leaf_node } = emit($n, $c, Identifier, field.start, field.end, [])
					{ nodes: $n, children: $c, node: access_node } = emit2($n, $c, if (optional) OptionalFieldAccess else FieldAccess, start, field.end, $left, leaf_node)
					$s = field_state + 1
					$left = access_node
					$qualified = $qualified and field.kind == UpperIdent
				} else {
					$s = look
					$done = True
				}
			} else if lt == DotDot or lt == DotStar {
				$d = diag($d, UnexpectedToken, t.start, t.end)
				$s = look + 1
				$done = True
			} else if bp.left > 0 and bp.left >= minimum {
				start = nstart($n, $left)
				{ pos: right_state, nodes: $n, children: $c, diags: $d, node: right_node } = expression(ctx, look + 1, $n, $c, $d, bp.right, depth + 1, False)
				end = nend($n, right_node)
				{ nodes: $n, children: $c, node: binary_node } = emit2($n, $c, Binary, start, end, $left, right_node)
				$s = right_state
				$left = binary_node
				$qualified = False
			} else {
				$s = look
				$done = True
			}
		}
	}
	{ pos: $s, nodes: $n, children: $c, diags: $d, node: $left }
}

collection : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.TokenKind, U64 -> X
collection = |ctx, initial, nodes, children, diags, closing, depth| {
	var $s = initial
	var $count = 0
	var $first = 0
	var $rest = []
	var $end = tok(ctx, initial).start
	var $done = False
	var $comma = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		sync = declaration_sync(ctx, look)
		if sync.sync {
			$d = diag($d, UnclosedDelimiter, sync.start, sync.end)
			$s = look
			$end = sync.end
			$done = True
		} else if t.kind == closing {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else {
			before = look
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = expression(ctx, look, $n, $c, $d, 0, depth, False)
			if $count == 0 {
				$first = item_node
			} else {
				$rest = $rest.append(item_node)
			}
			$count = $count + 1
			$end = nend($n, item_node)
			$s = sig(ctx, item_state)
			if is_text(ctx, $s, Comma) {
				$comma = True
				$s = $s + 1
			} else if tok(ctx, $s).kind != closing and tok(ctx, $s).kind != EndOfFile {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				if $s <= before {
					$s = $s + 1
				}
			}
		}
	}
	{ pos: $s, nodes: $n, children: $c, diags: $d, count: $count, first: $first, rest: $rest, end: $end, comma: $comma }
}

primary : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, Bool -> R
primary = |ctx, s, nodes, children, diags, depth, punned| {
	t = tok(ctx, s)
	# Hot leaves stay here; every other form goes to `primary_compound`. It is
	# called from more than one site so it stays out of line: Roc inlines
	# single-use functions, and the LLVM backend gives every call site in a
	# function its own stack slots (roc-lang/roc#12423), so inlining the
	# compound forms here made each nested operand cost a 6.5 KB frame.
	match t.kind {
		LowerIdent if !is_text(ctx, s, Underscore) => finish(s + 1, nodes, children, diags, Identifier, t.start, t.end, [])
		UpperIdent if byte(ctx.source, t.start) != '@' => finish(s + 1, nodes, children, diags, Identifier, t.start, t.end, [])
		OpenRound | NoSpaceOpenRound | OpenSquare => {
			{ pos: items_state, nodes: n1, children: c1, diags: d1, count: items_count, first: items_first, rest: items_rest, end: items_end, comma: items_comma } = collection(ctx, s + 1, nodes, children, diags, if (t.kind == OpenSquare) CloseSquare else CloseRound, depth + 1)
			kind = if (t.kind == OpenSquare) ListLiteral else if (items_comma or items_count != 1) Tuple else Parenthesized
			finish_items(items_state, n1, c1, d1, kind, t.start, items_end, items_count, items_first, items_rest)
		}
		Keyword => primary_compound(ctx, s, nodes, children, diags, depth, punned)
		_ => primary_compound(ctx, s, nodes, children, diags, depth, punned)
	}
}

primary_compound : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, Bool -> R
primary_compound = |ctx, s, nodes, children, diags, depth, punned| {
	t = tok(ctx, s)
	match t.kind {
		LowerIdent if is_text(ctx, s, Underscore) => finish(s + 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
		UpperIdent if byte(ctx.source, t.start) == '@' => finish(s + 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
		LowerIdent | UpperIdent => finish(s + 1, nodes, children, diags, Identifier, t.start, t.end, [])
		Number => {
			after = s + 1
			dot = tok(ctx, after)
			field_state = after + 1
			field = tok(ctx, field_state)
			if integer_token(ctx.source, t) and is_text(ctx, after, Dot) and dot.start == t.end and field.kind == Number and ctx.source.sublist({ start: field.start, len: field.end - field.start }).all(is_digit) {
				finish(field_state + 1, nodes, children, diag(diags, UnexpectedToken, dot.start, field.end), ErrorNode, t.start, field.end, [])
			} else {
				{ nodes: n1, children: c1, node: literal_node } = emit(nodes, children, NumberLiteral, t.start, t.end, [])
				literal_suffix(ctx, after, n1, c1, diags, literal_node, False)
			}
		}
		SingleQuote => {
			{ nodes: n1, children: c1, node: literal_node } = emit(nodes, children, CharacterLiteral, t.start, t.end, [])
			literal_suffix(ctx, s + 1, n1, c1, diags, literal_node, False)
		}
		StringStart | MultilineStringStart => parse_string(ctx, s, nodes, children, diags, depth + 1)
		OpenCurly => curly(ctx, s + 1, nodes, children, diags, t.start, depth + 1, punned, False)
		Keyword if is_text(ctx, s, Dbg) or is_text(ctx, s, Crash) or is_text(ctx, s, Return) => {
			kind = if (is_text(ctx, s, Dbg)) Debug else if (is_text(ctx, s, Crash)) Crash else Return
			{ pos: value_state, nodes: n1, children: c1, diags: d1, node: value_node } = expression(ctx, s + 1, nodes, children, diags, 0, depth + 1, False)
			end = nend(n1, value_node)
			finish1(value_state, n1, c1, d1, kind, t.start, end, value_node)
		}
		Keyword if is_text(ctx, s, Break) => finish(s + 1, nodes, children, diags, Break, t.start, t.end, [])
		Punctuation if is_text(ctx, s, Ellipsis) => finish(s + 1, nodes, children, diags, Ellipsis, t.start, t.end, [])
		Keyword if is_text(ctx, s, If) => parse_if(ctx, s + 1, nodes, children, diags, t.start, depth + 1)
		Keyword if is_text(ctx, s, For) or is_text(ctx, s, ForBang) => parse_for(ctx, s + 1, nodes, children, diags, t.start, is_text(ctx, s, ForBang), depth + 1)
		Keyword if is_text(ctx, s, Match) => parse_match(ctx, s + 1, nodes, children, diags, t.start, depth + 1)
		Operator if is_text(ctx, s, Bar) => parse_lambda(ctx, s + 1, nodes, children, diags, t.start, depth + 1)
		OpenRound | NoSpaceOpenRound | OpenSquare => {
			{ pos: items_state, nodes: n1, children: c1, diags: d1, count: items_count, first: items_first, rest: items_rest, end: items_end, comma: items_comma } = collection(ctx, s + 1, nodes, children, diags, if (t.kind == OpenSquare) CloseSquare else CloseRound, depth + 1)
			kind = if (t.kind == OpenSquare) ListLiteral else if (items_comma or items_count != 1) Tuple else Parenthesized
			finish_items(items_state, n1, c1, d1, kind, t.start, items_end, items_count, items_first, items_rest)
		}
		Operator if is_text(ctx, s, Minus) and binary_minus(ctx, s) => finish(s + 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
		Operator if is_text(ctx, s, Minus) or is_text(ctx, s, Bang) => {
			{ pos: operand_state, nodes: n1, children: c1, diags: d1, node: operand_node } = expression(ctx, s + 1, nodes, children, diags, 40, depth + 1, False)
			end = nend(n1, operand_node)
			finish1(operand_state, n1, c1, d1, Unary, t.start, end, operand_node)
		}
		EndOfFile | InterpolationEnd | CloseRound | CloseSquare | CloseCurly => finish(s, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.start, [])
		Invalid => finish(s + 1, nodes, children, diags, ErrorNode, t.start, t.end, [])
		_ => finish(s + 1, nodes, children, diag(diags, UnsupportedSyntax, t.start, t.end), UnsupportedNode, t.start, t.end, [])
	}
}

parse_source : List(U8), ParseMode -> RocSyntax.Parsed
parse_source = |source, mode| {
	expression_only = mode == ExpressionMode
	single = mode != FileMode
	lexed = lex(source)
	texts = if (lexed.tokens.len() > 1048576) [] else lexed.tokens.map(|t| token_text(source, t))
	ctx = { source, tokens: lexed.tokens, texts, in_guard: False }
	# Roughly one node and child per two tokens; lists still grow when needed.
	reserve = if (lexed.tokens.len() > 1048576) 0 else lexed.tokens.len() // 2 + 8
	too_many = ctx.tokens.len() > 1048576
	var $s = 0
	var $roots = []
	var $done = too_many
	var $n = List.with_capacity(reserve)
	var $c = List.with_capacity(reserve)
	var $d = if (too_many) diag(lexed.diagnostics, ResourceLimit, 0, source.len()) else lexed.diagnostics
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == EndOfFile {
			$s = look
			$done = True
		} else {
			before = look
			{ pos: built_state, nodes: $n, children: $c, diags: $d, node: built_node } = if (expression_only) expression(ctx, look, $n, $c, $d, 0, 0, False) else statement(ctx, look, $n, $c, $d, if (mode == StatementMode) 1 else 0)
			$roots = $roots.append(built_node)
			$s = built_state
			if mode == FileMode or mode == HeaderMode {
				if nget($n, built_node).kind == ExpressionStatement {
					built_end = nend($n, built_node)
					$d = diag($d, UnexpectedToken, t.start, built_end)
				}
			}
			if $s <= before {
				$d = diag($d, UnexpectedToken, t.start, t.end)
				$s = before + 1
			}
			if mode == HeaderMode and nget($n, built_node).kind != Header {
				built_end = nend($n, built_node)
				$d = diag($d, UnexpectedToken, t.start, built_end)
			}
			if single {
				$s = sig(ctx, $s)
				if tok(ctx, $s).kind != EndOfFile {
					extra = tok(ctx, $s)
					$d = diag($d, UnexpectedToken, extra.start, extra.end)
				}
				$done = True
			}
		}
	}
	end_state = $s
	roots = $roots
	n1 = $n
	c1 = $c
	d1 = $d
	header_missing = mode == HeaderMode and roots.is_empty()
	d2 = if (!header_missing and single and roots.is_empty()) diag(d1, UnexpectedToken, source.len(), source.len()) else d1
	{ nodes: n2, children: c2, roots: file_roots } = if header_missing {
		{ nodes: hn, children: hc, node: header_node } = emit(n1, c1, Header, 0, 0, [])
		{ nodes: hn, children: hc, roots: [header_node] }
	} else {
		{ nodes: n1, children: c1, roots }
	}
	{ nodes: n3, children: c3, node: root_node } = emit(n2, c2, File, 0, source.len(), file_roots)
	first_start = (source_token(ctx, significant_position(ctx.tokens, 0))).start
	var $header_count = 0.U64
	var $bad_header = False
	var $index = 0.U64
	while $index < n3.len() {
		n = nget(n3, $index)
		if n.kind == Header {
			$header_count = $header_count + 1
			$bad_header = $bad_header or n.start != first_start
		}
		$index = $index + 1
	}
	d3 = if ($header_count > 1 or $bad_header) diag(d2, UnexpectedToken, first_start, source.len()) else d2
	unsupported = lexed.unsupported or d3.any(|d| d.code == UnsupportedSyntax)
	limited = d3.any(|d| d.code == ResourceLimit)
	diagnostics = if (d3.len() > 256) d3.take_first(256) else d3
	has_error = diagnostics.any(|d| d.code != UnsupportedSyntax)
	status = if (limited) Limited else if (unsupported) Unsupported else if (has_error) Recovered else Complete
	{ source, tokens: ctx.tokens, nodes: n3, children: c3, diagnostics, status, root: root_node, consumed: tok(ctx, sig(ctx, end_state)).kind == EndOfFile }
}

# Statements are parsed independently of expressions, so '=' cannot be
# accidentally accepted as a binary expression operator.
statement : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
statement = |ctx, initial, nodes, children, diags, depth| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	following_text = ctx.texts.get(significant_position(ctx.tokens, s + 1)) ?? OtherText
	# Expression statements and plain declarations stay here; every other form
	# goes to `statement_form`, called from two sites so it stays out of line
	# (see `primary`): inlined, its forms gave each statement a 13 KB frame.
	plain = depth < 128 and match t.kind {
		# A field name after `.` can still spell a keyword such as `var`.
		LowerIdent => (ctx.texts.get(s) ?? OtherText) == OtherText and !(following_text == Equals or following_text == Colon or following_text == Dot)
		UpperIdent => depth > 0 and !type_declaration_start(ctx, s) and !has_pattern_assignment(ctx, s)
		OpenRound | OpenCurly => !has_pattern_assignment(ctx, s)
		Keyword => False
		_ => True
	}
	if plain {
		{ pos: value_state, nodes: n1, children: c1, diags: d1, node: value_node } = expression(ctx, s, nodes, children, diags, 0, depth, True)
		end = nend(n1, value_node)
		finish1(value_state, n1, c1, d1, ExpressionStatement, t.start, end, value_node)
	} else if depth < 128 and t.kind == LowerIdent and following_text == Equals {
		{ pos: name_state, nodes: n1, children: c1, diags: d1, node: name_node } = parse_pattern(ctx, s, nodes, children, diags, False, depth + 1)
		after_name = sig(ctx, name_state)
		{ pos: value_state, nodes: n2, children: c2, diags: d2, node: value_node } = expression(ctx, after_name + 1, n1, c1, d1, 0, depth, False)
		end = nend(n2, value_node)
		finish2(value_state, n2, c2, d2, Declaration, t.start, end, name_node, value_node)
	} else if t.kind == Keyword {
		statement_form(ctx, initial, nodes, children, diags, depth)
	} else {
		statement_form(ctx, initial, nodes, children, diags, depth)
	}
}

statement_form : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
statement_form = |ctx, initial, nodes, children, diags, depth| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	following_pos = significant_position(ctx.tokens, s + 1)
	following = ctx.tokens.get(following_pos) ?? t
	text = ctx.texts.get(s) ?? OtherText
	following_text = ctx.texts.get(following_pos) ?? OtherText
	if depth >= 128 {
		finish(s + if (t.kind == EndOfFile) 0 else 1, nodes, children, diag(diags, ResourceLimit, t.start, t.end), ErrorNode, t.start, t.end, [])
	} else if (t.kind == UpperIdent and (type_declaration_start(ctx, s) or (depth == 0 and !((following.kind == NoSpaceOpenRound or following_text == Dot) and has_pattern_assignment(ctx, s))))) or (t.kind == LowerIdent and following_text == Dot and tok(ctx, following_pos + 1).kind == UpperIdent) {
		parse_type_declaration(ctx, s, nodes, children, diags, depth + 1)
	} else if t.kind == LowerIdent and following_text == Equals {
		{ pos: name_state, nodes: n1, children: c1, diags: d1, node: name_node } = parse_pattern(ctx, s, nodes, children, diags, False, depth + 1)
		after_name = sig(ctx, name_state)
		{ pos: value_state, nodes: n2, children: c2, diags: d2, node: value_node } = expression(ctx, after_name + 1, n1, c1, d1, 0, depth, False)
		end = nend(n2, value_node)
		finish2(value_state, n2, c2, d2, Declaration, t.start, end, name_node, value_node)
	} else if t.kind == LowerIdent and following_text == Colon {
		d1 = if (text == Underscore) diag(diags, UnexpectedToken, t.start, t.end) else diags
		{ nodes: n1, children: c1, node: name_node } = emit(nodes, children, Identifier, t.start, t.end, [])
		parse_annotation_after_name(ctx, following_pos + 1, n1, c1, d1, name_node, t.start, depth)
	} else if text == Var {
		name_state = sig(ctx, s + 1)
		name = tok(ctx, name_state)
		d1 = if (depth == 0 or name.kind != LowerIdent or !(is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$')) diag(diags, UnexpectedToken, name.start, name.end) else diags
		{ nodes: n1, children: c1, node: named_node } = emit(nodes, children, Identifier, name.start, name.end, [])
		look = sig(ctx, name_state + if (name.kind == EndOfFile) 0 else 1)
		if is_text(ctx, look, Equals) {
			{ pos: value_state, nodes: n2, children: c2, diags: d2, node: value_node } = expression(ctx, look + 1, n1, c1, d1, 0, depth + 1, False)
			end = nend(n2, value_node)
			finish2(value_state, n2, c2, d2, VariableDeclaration, t.start, end, named_node, value_node)
		} else if is_text(ctx, look, Colon) {
			parse_annotation_after_name(ctx, look + 1, n1, c1, d1, named_node, t.start, depth + 1)
		} else {
			finish1(look, n1, c1, d1, VariableDeclaration, t.start, name.end, named_node)
		}
	} else if text == Expect {
		{ pos: value_state, nodes: n1, children: c1, diags: d1, node: value_node } = expression(ctx, s + 1, nodes, children, diags, 0, depth + 1, False)
		end = nend(n1, value_node)
		finish1(value_state, n1, c1, d1, Expect, t.start, end, value_node)
	} else if text == While {
		{ pos: condition_state, nodes: n1, children: c1, diags: d1, node: condition_node } = expression(ctx, s + 1, nodes, children, diags, 0, depth + 1, False)
		{ pos: body_state, nodes: n2, children: c2, diags: d2, node: body_node } = expression(ctx, condition_state, n1, c1, d1, 0, depth + 1, False)
		end = nend(n2, body_node)
		finish2(body_state, n2, c2, d2, While, t.start, end, condition_node, body_node)
	} else if text == For or text == ForBang or text == Dbg or text == Crash or text == Return or text == Break {
		primary(ctx, s, nodes, children, diags, depth + 1, False)
	} else if (t.kind == OpenRound or t.kind == OpenCurly or t.kind == UpperIdent) and has_pattern_assignment(ctx, s) {
		{ pos: pattern_state, nodes: n1, children: c1, diags: d1, node: pattern_node } = parse_pattern(ctx, s, nodes, children, diags, False, depth + 1)
		look = sig(ctx, pattern_state)
		assigned = is_text(ctx, look, Equals)
		d2 = if (assigned) d1 else diag(d1, UnexpectedToken, tok(ctx, look).start, tok(ctx, look).end)
		{ pos: value_state, nodes: n2, children: c2, diags: d3, node: value_node } = expression(ctx, look + if (assigned) 1 else 0, n1, c1, d2, 0, depth + 1, False)
		end = nend(n2, value_node)
		finish2(value_state, n2, c2, d3, Declaration, t.start, end, pattern_node, value_node)
	} else if text == Platform {
		parse_platform_header(ctx, s, nodes, children, if (depth != 0) diag(diags, UnexpectedToken, t.start, t.end) else diags, depth + 1)
	} else if text == App or text == Package {
		{ pos: exposed_state, nodes: n1, children: c1, diags: d1, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(ctx, s + 1, nodes, children, diags)
		look = sig(ctx, exposed_state)
		if tok(ctx, look).kind == OpenCurly {
			{ pos: deps_state, nodes: n2, children: c2, diags: d2, node: deps_node } = dependency_record(ctx, look, n1, c1, d1, text == App, depth + 1)
			end = nend(n2, deps_node)
			d3 = if (depth != 0) diag(d2, UnexpectedToken, t.start, end) else d2
			finish(deps_state, n2, c2, d3, Header, t.start, end, exposed_ids.append(deps_node))
		} else {
			d2 = if (!(depth == 0 and text == Package)) diag(d1, UnexpectedToken, tok(ctx, look).start, tok(ctx, look).end) else d1
			finish(look, n1, c1, d2, Header, t.start, exposed_end, exposed_ids)
		}
	} else if text == Module or text == Hosted {
		{ pos: items_state, nodes: n1, children: c1, diags: d1, ids: items_ids, end: items_end, comma: _ } = exposed_collection(ctx, s + 1, nodes, children, diags)
		d2 = if (depth != 0) diag(d1, UnexpectedToken, t.start, items_end) else d1
		finish(items_state, n1, c1, d2, Header, t.start, items_end, items_ids)
	} else if text == Import {
		parse_import(ctx, s, nodes, children, if (depth != 0) diag(diags, UnexpectedToken, t.start, t.end) else diags, depth + 1)
	} else {
		{ pos: value_state, nodes: n1, children: c1, diags: d1, node: value_node } = expression(ctx, s, nodes, children, diags, 0, depth, True)
		end = nend(n1, value_node)
		finish1(value_state, n1, c1, d1, ExpressionStatement, t.start, end, value_node)
	}
}

has_pattern_assignment : Ctx, U64 -> Bool
has_pattern_assignment = |ctx, initial| {
	var $s = initial
	first = tok(ctx, $s)
	if first.kind == UpperIdent {
		if byte(ctx.source, first.start) == '@' {
			return False
		}
		$s = sig(ctx, $s + 1)
		var $name_end = first.end
		var $done = False
		while !$done and is_text(ctx, $s, Dot) {
			field_state = $s + 1
			field = tok(ctx, field_state)
			if tok(ctx, $s).start == $name_end and field.kind == UpperIdent {
				$name_end = field.end
				$s = sig(ctx, field_state + 1)
			} else {
				$done = True
			}
		}
		if is_text(ctx, $s, Dot) {
			$s = sig(ctx, $s + 1)
		}
		if tok(ctx, $s).kind != NoSpaceOpenRound and tok(ctx, $s).kind != OpenCurly {
			return False
		}
	}
	opening = tok(ctx, $s)
	if opening.kind != OpenRound and opening.kind != NoSpaceOpenRound and opening.kind != OpenCurly {
		return False
	}
	var $pos = $s + 1
	var $nesting = 1.U64
	while $nesting > 0 {
		t = source_token(ctx, $pos)
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
	is_text(ctx, significant_position(ctx.tokens, $pos), Equals)
}

declaration_sync : Ctx, U64 -> { sync : Bool, start : U64, end : U64 }
declaration_sync = |ctx, initial| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	next = sig(ctx, s + 1)
	if t.kind != LowerIdent or !is_text(ctx, next, Equals) {
		return { sync: False, start: t.start, end: t.start }
	}
	var $pos = s
	var $newline = False
	var $done = False
	var $start = t.start
	var $end = t.start
	while $pos > 0 and !$done {
		$pos = $pos - 1
		previous = source_token(ctx, $pos)
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

binary_minus : Ctx, U64 -> Bool
binary_minus = |ctx, s| {
	t = tok(ctx, s)
	next = byte(ctx.source, t.end)
	previous = ctx.tokens.get(if (s > 0) s - 1 else 0) ?? t
	if bytes3(ctx.source, t.end, '.', '.', '.') {
		return previous.end == t.start and can_end_token(ctx.source, previous)
	}
	is_space(next) or next == '#' or (is_digit(next) and previous.end == t.start and can_end_token(ctx.source, previous)) or (!is_digit(next) and !is_lower(next) and !is_upper(next) and next != '_' and next != '(' and next < 0x80)
}

is_optional_access : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
is_optional_access = |_ctx, nodes, _children, root| {
	match nodes.get(root) {
		Ok(n) => n.kind == OptionalFieldAccess
		_ => False
	}
}

# A literal's adjacent .Upper chain names its type, rather than a value field.
literal_suffix : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, Bool -> R
literal_suffix = |ctx, built_state, nodes, children, diags, built_node, allow_trivia| {
	var $s = built_state
	var $type_ids = []
	var $end = nend(nodes, built_node)
	var $done = False
	var $n = nodes
	var $c = children
	while !$done {
		look = if (allow_trivia and $type_ids.is_empty()) sig(ctx, $s) else $s
		t = tok(ctx, look)
		name_state = look + 1
		name = tok(ctx, name_state)
		if is_text(ctx, look, Dot) and (allow_trivia and $type_ids.is_empty() or t.start == $end) and name.kind == UpperIdent and is_upper(byte(ctx.source, name.start)) {
			{ nodes: $n, children: $c, node: named_node } = emit($n, $c, Identifier, name.start, name.end, [])
			$s = name_state + 1
			$type_ids = $type_ids.append(named_node)
			$end = name.end
		} else {
			$done = True
		}
	}
	if ($type_ids.is_empty()) { pos: $s, nodes: $n, children: $c, diags, node: built_node } else {
		start = nstart($n, built_node)
		finish_after($s, $n, $c, diags, TypedLiteral, start, $end, built_node, $type_ids)
	}
}

pipe_has_method : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
pipe_has_method = |ctx, nodes, children, root| {
	var $id = root
	var $found = False
	var $done = False
	while !$done {
		if method_target(ctx, nodes, children, $id) {
			$found = True
			$done = True
		} else {
			match nodes.get($id) {
				Ok(n) if n.kind == Call or n.kind == FieldAccess or n.kind == OptionalFieldAccess or n.kind == Suffix or n.kind == NominalRecord or n.kind == NominalApply => {
					$id = children.get(n.children_start) ?? nodes.len()
				}
				_ => {
					$done = True
				}
			}
		}
	}
	$found
}

method_target : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
method_target = |ctx, nodes, children, root| {
	match nodes.get(root) {
		Ok(call) if call.kind == Call => {
			callee_id = children.get(call.children_start) ?? nodes.len()
			match nodes.get(callee_id) {
				Ok(callee) if callee.kind == FieldAccess => {
					receiver = children.get(callee.children_start) ?? nodes.len()
					field_id = children.get(callee.children_start + 1) ?? nodes.len()
					field = nodes.get(field_id) ?? callee
					is_lower(byte(ctx.source, field.start)) and !namespace_receiver(ctx, nodes, children, receiver)
				}
				_ => False
			}
		}
		_ => False
	}
}

namespace_receiver : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
namespace_receiver = |ctx, nodes, children, root| {
	var $id = root
	var $upper_segment = False
	var $done = False
	var $qualified = False
	while !$done {
		match nodes.get($id) {
			Ok(n) if n.kind == Identifier => {
				$qualified = is_upper(byte(ctx.source, n.start)) or $upper_segment
				$done = True
			}
			Ok(n) if n.kind == FieldAccess => {
				field_id = children.get(n.children_start + 1) ?? nodes.len()
				field = nodes.get(field_id) ?? n
				if is_upper(byte(ctx.source, field.start)) {
					$upper_segment = True
					$id = children.get(n.children_start) ?? nodes.len()
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

# ---- Headers, imports and dependencies ---------------------------------------

exposed_collection : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic) -> C
exposed_collection = |ctx, initial, nodes, children, diags| {
	look = sig(ctx, initial)
	opening = tok(ctx, look)
	opened = opening.kind == OpenSquare
	var $n = nodes
	var $c = children
	var $d = if (opened) diags else diag(diags, UnexpectedToken, opening.start, opening.end)
	var $s = if (opened) look + 1 else look
	var $ids = []
	var $end = opening.end
	var $done = !opened
	while !$done {
		item_state = sig(ctx, $s)
		t = tok(ctx, item_state)
		if t.kind == CloseSquare {
			$s = item_state + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseCurly or t.kind == CloseRound {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = item_state
			$done = True
		} else {
			valid_start = (t.kind == UpperIdent and byte(ctx.source, t.start) != '@') or (t.kind == LowerIdent and (is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$'))
			if !valid_start {
				$d = diag($d, UnexpectedToken, t.start, t.end)
			}
			validated = item_state
			{ nodes: $n, children: $c, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
			name_state = validated + 1
			var $name = name_node
			$s = sig(ctx, name_state)
			var $upper = t.kind == UpperIdent
			var $qualified = t.kind == UpperIdent
			var $chain_done = False
			while !$chain_done {
				name_end = nend($n, $name)
				if $qualified and is_text(ctx, $s, Dot) and tok(ctx, $s).start == name_end and (is_upper(byte(ctx.source, tok(ctx, $s + 1).start)) or is_lower(byte(ctx.source, tok(ctx, $s + 1).start))) {
					field_state = $s + 1
					field = tok(ctx, field_state)
					{ nodes: $n, children: $c, node: named_node } = emit($n, $c, Identifier, field.start, field.end, [])
					{ nodes: $n, children: $c, node: access_node } = emit2($n, $c, FieldAccess, t.start, field.end, $name, named_node)
					$s = sig(ctx, field_state + 1)
					$name = access_node
					$upper = True
					$qualified = True
				} else {
					$chain_done = True
				}
			}
			var $children = [$name]
			var $wildcard = False
			$end = nend($n, $name)
			if is_text(ctx, $s, As) {
				alias_state = sig(ctx, $s + 1)
				alias = tok(ctx, alias_state)
				valid_alias = if ($upper) alias.kind == UpperIdent and byte(ctx.source, alias.start) != '@' else alias.kind == LowerIdent and byte(ctx.source, alias.start) != '_'
				if !valid_alias {
					$d = diag($d, UnexpectedToken, alias.start, alias.end)
				}
				checked = alias_state
				{ nodes: $n, children: $c, node: alias_node } = emit($n, $c, Identifier, alias.start, alias.end, [])
				$s = checked + if (alias.kind == EndOfFile) 0 else 1
				$children = $children.append(alias_node)
				$end = alias.end
			} else if $upper and is_text(ctx, $s, DotStar) {
				$end = tok(ctx, $s).end
				$s = $s + 1
				$wildcard = True
			}
			{ nodes: $n, children: $c, node: exposed_node } = emit($n, $c, if ($wildcard) ExposedWildcard else ExposedItem, t.start, $end, $children)
			$ids = $ids.append(exposed_node)
			$s = sig(ctx, $s)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseSquare {
				$d = diag($d, UnexpectedToken, tok(ctx, $s).start, tok(ctx, $s).end)
			}
		}
	}
	{ pos: $s, nodes: $n, children: $c, diags: $d, ids: $ids, end: $end, comma: False }
}

parse_import : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_import = |ctx, initial, nodes, children, diags, depth| {
	start = tok(ctx, initial).start
	look = sig(ctx, initial + 1)
	if tok(ctx, look).kind == StringStart {
		{ pos: path_state, nodes: n1, children: c1, diags: d1, node: path_node } = parse_string(ctx, look, nodes, children, diags, depth + 1)
		after_path = sig(ctx, path_state)
		has_as = is_text(ctx, after_path, As)
		d2 = if (has_as) d1 else diag(d1, UnexpectedToken, tok(ctx, after_path).start, tok(ctx, after_path).end)
		validated = if (has_as) after_path + 1 else after_path
		name_state = sig(ctx, validated)
		name = tok(ctx, name_state)
		d3 = if (!(name.kind == LowerIdent and !is_text(ctx, name_state, Underscore))) diag(d2, UnexpectedToken, name.start, name.end) else d2
		checked = name_state
		{ nodes: n2, children: c2, node: named_node } = emit(n1, c1, Identifier, name.start, name.end, [])
		named_state = checked + if (name.kind == EndOfFile) 0 else 1
		colon = sig(ctx, named_state)
		has_colon = is_text(ctx, colon, Colon)
		d4 = if (has_colon) d3 else diag(d3, UnexpectedToken, tok(ctx, colon).start, tok(ctx, colon).end)
		ty_start = if (has_colon) colon + 1 else colon
		{ pos: ty_state, nodes: n3, children: c3, diags: d5, node: ty_node } = file_import_type(ctx, ty_start, n2, c2, d4)
		end = nend(n3, ty_node)
		path_info = nget(n3, path_node)
		path_child = c3.get(path_info.children_start) ?? n3.len()
		part_info = n3.get(path_child) ?? path_info
		plain_path = path_info.kind == StringLiteral and path_info.children_len == 1 and part_info.kind == StringSegment
		finish3(ty_state, n3, c3, if (plain_path) d5 else diag(d5, UnexpectedToken, start, end), FileImport, start, end, path_node, named_node, ty_node)
	} else {
		path_start = tok(ctx, look).start
		{ pos: prefix_state, diags: d1, local } = if is_text(ctx, look, Slash) {
			{ pos: sig(ctx, look + 1), diags, local: True }
		} else if is_text(ctx, look, Dot) and is_text(ctx, significant_position(ctx.tokens, look + 1), Slash) {
			{ pos: sig(ctx, significant_position(ctx.tokens, look + 1) + 1), diags, local: True }
		} else if is_text(ctx, look, DotDot) {
			var $s = look
			var $d = diags
			while is_text(ctx, $s, DotDot) {
				parent = sig(ctx, $s + 1)
				if is_text(ctx, parent, Slash) {
					$s = sig(ctx, parent + 1)
				} else {
					$d = diag($d, UnexpectedToken, tok(ctx, parent).start, tok(ctx, parent).end)
					$s = parent
				}
			}
			{ pos: $s, diags: $d, local: True }
		} else if tok(ctx, look).kind == LowerIdent {
			{ pos: look, diags, local: is_text(ctx, sig(ctx, look + 1), Slash) }
		} else {
			{ pos: look, diags, local: True }
		}
		first = tok(ctx, prefix_state)
		first_valid = first.kind == UpperIdent or first.kind == LowerIdent
		d2 = if (first_valid) d1 else diag(d1, UnexpectedToken, first.start, first.end)
		first_end = if (first_valid) first.end else U64.max(path_start, first.start)
		first_state = if (first_valid) sig(ctx, prefix_state + 1) else prefix_state
		{ pos: local_state, diags: d3, end: local_end } = if local {
			var $s = first_state
			var $d = d2
			var $end = first_end
			var $upper = first.kind == UpperIdent
			while is_text(ctx, $s, Slash) {
				$s = sig(ctx, $s + 1)
				part = tok(ctx, $s)
				if part.kind == LowerIdent or part.kind == UpperIdent {
					$upper = part.kind == UpperIdent
					$end = part.end
					$s = sig(ctx, $s + 1)
				} else {
					$d = diag($d, UnexpectedToken, part.start, part.end)
				}
			}
			{ pos: $s, diags: if ($upper) $d else diag($d, UnexpectedToken, first.start, $end), end: $end }
		} else {
			{ pos: first_state, diags: d2, end: first_end }
		}
		var $s = local_state
		var $end = local_end
		var $nested = 0.U64
		while is_text(ctx, $s, Dot) and tok(ctx, $s + 1).kind == UpperIdent {
			field_state = $s + 1
			$end = tok(ctx, field_state).end
			$s = sig(ctx, field_state + 1)
			$nested = $nested + 1
		}
		path_end = $end
		path_state = $s
		d4 = if (!local and $nested == 0) diag(d3, UnexpectedToken, first.start, path_end) else d3
		{ nodes: n1, children: c1, node: path_node } = emit(nodes, children, ImportPath, path_start, path_end, [])
		{ pos: alias_state, nodes: n2, children: c2, diags: d5, ids: alias_ids, end: alias_end } = if is_text(ctx, path_state, As) {
			alias_pos = sig(ctx, path_state + 1)
			alias = tok(ctx, alias_pos)
			da = if (alias.kind != UpperIdent) diag(d4, UnexpectedToken, alias.start, alias.end) else d4
			checked = alias_pos
			{ nodes: na, children: ca, node: named_node } = emit(n1, c1, Identifier, alias.start, alias.end, [])
			named_state = checked + if (alias.kind == EndOfFile) 0 else 1
			{ nodes: nb, children: cb, node: alias_node } = emit1(na, ca, ImportAlias, alias.start, alias.end, named_node)
			{ pos: sig(ctx, named_state), nodes: nb, children: cb, diags: da, ids: [path_node, alias_node], end: alias.end }
		} else {
			{ pos: path_state, nodes: n1, children: c1, diags: d4, ids: [path_node], end: path_end }
		}
		if is_text(ctx, alias_state, Exposing) {
			{ pos: exposed_state, nodes: n3, children: c3, diags: d6, ids: exposed_ids, end: exposed_end, comma: _ } = exposed_collection(ctx, alias_state + 1, n2, c2, d5)
			finish(exposed_state, n3, c3, d6, Import, start, exposed_end, alias_ids.concat(exposed_ids))
		} else {
			finish(alias_state, n2, c2, d5, Import, start, alias_end, alias_ids)
		}
	}
}

file_import_type : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic) -> R
file_import_type = |ctx, initial, nodes, children, diags| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	valid_name = spell(ctx, s, "Str") or spell(ctx, s, "List")
	d1 = if (valid_name) diags else diag(diags, UnexpectedToken, t.start, t.end)
	checked = s
	{ nodes: n1, children: c1, node: name_node } = emit(nodes, children, Identifier, t.start, t.end, [])
	name_state = checked + if (t.kind == EndOfFile) 0 else 1
	if spell(ctx, s, "List") {
		look = sig(ctx, name_state)
		opening = tok(ctx, look)
		opened = opening.kind == OpenRound or opening.kind == NoSpaceOpenRound
		d2 = if (opened) d1 else diag(d1, UnexpectedToken, opening.start, opening.end)
		after = if (opened) look + 1 else look
		arg_state = sig(ctx, after)
		arg = tok(ctx, arg_state)
		d3 = if (spell(ctx, arg_state, "U8")) d2 else diag(d2, UnexpectedToken, arg.start, arg.end)
		validated = arg_state
		{ nodes: n2, children: c2, node: arg_node } = emit(n1, c1, Identifier, arg.start, arg.end, [])
		arg_built = validated + if (arg.kind == EndOfFile) 0 else 1
		close_state = sig(ctx, arg_built)
		close = tok(ctx, close_state)
		closed = close.kind == CloseRound
		d4 = if (closed) d3 else diag(d3, UnclosedDelimiter, close.start, close.end)
		finished = if (closed) close_state + 1 else close_state
		finish2(finished, n2, c2, d4, TypeExpression, t.start, if (closed) close.end else arg.end, name_node, arg_node)
	} else {
		{ pos: name_state, nodes: n1, children: c1, diags: d1, node: name_node }
	}
}

dependency_record : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
dependency_record = |ctx, initial, nodes, children, diags, strict, depth| {
	start = tok(ctx, initial).start
	var $n = nodes
	var $c = children
	var $d = diags
	var $s = initial + 1
	var $ids = []
	var $end = tok(ctx, initial).end
	var $platform = False
	var $version = False
	var $seen = Set.empty()
	var $done = False
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$end = t.end
			$s = look + 1
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$done = True
		} else {
			before = look
			valid_name = t.kind == LowerIdent and !is_text(ctx, look, Underscore) and byte(ctx.source, t.start) != '_'
			if !valid_name {
				$d = diag($d, UnexpectedToken, t.start, t.end)
			}
			checked = look
			{ nodes: $n, children: $c, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
			name_state = checked + 1
			var $field_ids = [name_node]
			$s = sig(ctx, name_state)
			$end = t.end
			is_version = token_spelling(ctx.source, t, "roc")
			if !is_version {
				dependency_name = Str.from_utf8_lossy(ctx.source.sublist({ start: t.start, len: t.end - t.start }))
				if $seen.contains(dependency_name) {
					$d = diag($d, UnexpectedToken, t.start, t.end)
				}
				$seen = $seen.insert(dependency_name)
			}
			if is_text(ctx, $s, Colon) {
				value_start = sig(ctx, $s + 1)
				is_platform = is_text(ctx, value_start, Platform)
				value_state = if (is_platform) sig(ctx, value_start + 1) else value_start
				value_token = tok(ctx, value_state)
				valid_start = if (is_platform) value_token.kind == StringStart or spell(ctx, value_state, "glue") else !strict or value_token.kind == StringStart
				if !(valid_start and (!is_platform or !$platform)) {
					$d = diag($d, UnexpectedToken, value_token.start, value_token.end)
				}
				validated = value_state
				{ pos: parsed_state, nodes: $n, children: $c, diags: $d, node: parsed_node } = expression(ctx, validated, $n, $c, $d, 0, depth + 1, False)
				var $value_node = parsed_node
				$end = nend($n, parsed_node)
				if is_platform {
					platform_start = tok(ctx, value_start).start
					{ nodes: $n, children: $c, node: platform_node } = emit1($n, $c, PlatformReference, platform_start, $end, parsed_node)
					$value_node = platform_node
					$platform = True
				}
				$s = parsed_state
				$field_ids = $field_ids.append($value_node)
				if !is_version and !(is_platform and token_spelling(ctx.source, value_token, "glue")) and !plain_dependency_path(ctx, $n, $c, parsed_node) {
					$d = diag($d, UnexpectedToken, t.start, $end)
				}
				if is_version {
					n = nget($n, parsed_node)
					part_id = $c.get(n.children_start) ?? $n.len()
					part = $n.get(part_id) ?? n
					version_text = Str.from_utf8_lossy(ctx.source.sublist({ start: part.start, len: part.end - part.start }))
					if $version or is_platform or n.kind != StringLiteral or n.children_len != 1 or part.kind != StringSegment or !valid_roc_version(version_text) {
						$d = diag($d, UnexpectedToken, t.start, $end)
					}
				}
			} else {
				$d = diag($d, UnexpectedToken, t.start, $end)
			}
			$version = $version or is_version
			{ nodes: $n, children: $c, node: field_node } = emit($n, $c, DependencyField, t.start, $end, $field_ids)
			$ids = $ids.append(field_node)
			$s = sig(ctx, $s)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseCurly {
				$d = diag($d, UnexpectedToken, tok(ctx, $s).start, tok(ctx, $s).end)
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	finish($s, $n, $c, $d, Dependencies, start, $end, $ids)
}

valid_import : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
valid_import = |ctx, nodes, children, id| {
	match nodes.get(id) {
		Ok(n) if n.kind == Identifier => is_upper(byte(ctx.source, n.start))
		Ok(n) if n.kind == FieldAccess => {
			last = children.get(n.children_start + 1) ?? nodes.len()
			match nodes.get(last) {
				Ok(field) => is_upper(byte(ctx.source, field.start))
				Err(_) => False
			}
		}
		_ => False
	}
}

plain_dependency_path : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
plain_dependency_path = |ctx, nodes, children, id| {
	match nodes.get(id) {
		Ok(n) if n.kind == StringLiteral => {
			var $valid = True
			var $i = 0.U64
			while $i < n.children_len and $valid {
				child_id = children.get(n.children_start + $i) ?? nodes.len()
				match nodes.get(child_id) {
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

# ---- Type syntax -------------------------------------------------------------

type_expression : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), TypeMode, U64 -> R
type_expression = |ctx, initial, nodes, children, diags, mode, depth| {
	{ pos: atom_state, nodes: n1, children: c1, diags: d1, node: atom_node } = type_atom(ctx, initial, nodes, children, diags, depth + 1)
	look = sig(ctx, atom_state)
	if mode != TypeArgument and (is_text(ctx, look, Arrow) or is_text(ctx, look, FatArrow) or (mode == FullType and type_comma_begins_arguments(ctx, look))) {
		var $s = look
		var $args = [atom_node]
		var $done = False
		var $n = n1
		var $c = c1
		var $d = d1
		while !$done and is_text(ctx, $s, Comma) {
			{ pos: arg_state, nodes: $n, children: $c, diags: $d, node: arg_node } = type_expression(ctx, $s + 1, $n, $c, $d, TypeArgument, depth + 1)
			$args = $args.append(arg_node)
			$s = sig(ctx, arg_state)
			$done = tok(ctx, $s).kind == EndOfFile
		}
		s2 = $s
		args = $args
		n2 = $n
		c2 = $c
		d2 = $d
		if is_text(ctx, s2, Arrow) or is_text(ctx, s2, FatArrow) {
			{ pos: ret_state, nodes: n3, children: c3, diags: d3, node: ret_node } = type_expression(ctx, s2 + 1, n2, c2, d2, TypeArgument, depth + 1)
			end = nend(n3, ret_node)
			start = nstart(n3, atom_node)
			finish(ret_state, n3, c3, d3, TypeFunction, start, end, args.append(ret_node))
		} else {
			bad = tok(ctx, s2)
			start = nstart(n2, atom_node)
			end = U64.max(bad.start, nend(n2, args.last() ?? atom_node))
			finish(s2, n2, c2, diag(d2, UnexpectedToken, bad.start, bad.end), ErrorNode, start, end, args)
		}
	} else {
		{ pos: atom_state, nodes: n1, children: c1, diags: d1, node: atom_node }
	}
}

# A comma in a record/where constraint may begin its next field, rather than
# the argument list of a function. Only bounded significant lookahead is used.
type_comma_begins_arguments : Ctx, U64 -> Bool
type_comma_begins_arguments = |ctx, s| {
	if !is_text(ctx, s, Comma) {
		return False
	}
	a = sig(ctx, s + 1)
	if tok(ctx, a).kind == CloseCurly or tok(ctx, a).kind == CloseSquare or is_text(ctx, a, DotDot) {
		return False
	}
	b = sig(ctx, a + 1)
	if tok(ctx, a).kind == LowerIdent and (is_text(ctx, b, Colon) or is_text(ctx, b, Question)) {
		return False
	}
	if tok(ctx, a).kind == LowerIdent and is_text(ctx, b, Dot) {
		c = sig(ctx, b + 1)
		d = sig(ctx, c + 1)
		if is_text(ctx, d, Colon) {
			return False
		}
	}
	True
}

type_atom : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
type_atom = |ctx, initial, nodes, children, diags, depth| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	sync = declaration_sync(ctx, s)
	if sync.sync {
		finish(s, nodes, children, diag(diags, UnexpectedToken, sync.start, sync.end), ErrorNode, sync.end, sync.end, [])
	} else if depth >= 128 {
		finish(s, nodes, children, diag(diags, ResourceLimit, t.start, t.end), ErrorNode, t.start, t.start, [])
	} else if (t.kind == LowerIdent or t.kind == UpperIdent) and byte(ctx.source, t.start) != '@' {
		var $s = s + 1
		var $end = t.end
		var $qualified = False
		var $done = False
		while !$done {
			look = sig(ctx, $s)
			if byte(ctx.source, t.start) != '_' and is_text(ctx, look, Dot) and tok(ctx, look).start == $end {
				name_state = look + 1
				name = tok(ctx, name_state)
				if (name.kind == LowerIdent or name.kind == UpperIdent) and name.start == tok(ctx, look).end and byte(ctx.source, name.start) != '_' and byte(ctx.source, name.start) != '@' {
					$s = name_state + 1
					$end = name.end
					$qualified = True
				} else {
					$done = True
				}
			} else {
				$done = True
			}
		}
		kind = if ($qualified or t.kind == UpperIdent) TypeName else if (spelled1(ctx.source, t, '_')) TypeWildcard else TypeVariable
		after = $s
		{ nodes: n1, children: c1, node: base_node } = emit(nodes, children, kind, t.start, $end, [])
		look = sig(ctx, after)
		if kind == TypeName and tok(ctx, look).kind == NoSpaceOpenRound {
			{ pos: items_state, nodes: n2, children: c2, diags: d1, ids: items_ids, end: items_end, comma: _ } = type_collection(ctx, look + 1, n1, c1, diags, CloseRound, TypeItem, depth + 1)
			finish_after(items_state, n2, c2, d1, TypeApply, t.start, items_end, base_node, items_ids)
		} else {
			{ pos: after, nodes: n1, children: c1, diags, node: base_node }
		}
	} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound {
		type_parenthesized(ctx, s, nodes, children, diags, depth + 1)
	} else if t.kind == OpenSquare {
		type_fields(ctx, s, nodes, children, diags, False, depth + 1)
	} else if t.kind == OpenCurly {
		type_fields(ctx, s, nodes, children, diags, True, depth + 1)
	} else {
		advance = t.kind != EndOfFile and t.kind != CloseRound and t.kind != CloseSquare and t.kind != CloseCurly and !is_text(ctx, s, Comma)
		finish(s + if (advance) 1 else 0, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
	}
}

type_collection : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.TokenKind, TypeMode, U64 -> C
type_collection = |ctx, initial, nodes, children, diags, closing, mode, depth| {
	var $s = initial
	var $ids = []
	var $end = tok(ctx, initial).start
	var $comma = False
	var $done = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == closing {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else {
			before = look
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = type_expression(ctx, look, $n, $c, $d, mode, depth + 1)
			$ids = $ids.append(item_node)
			$s = sig(ctx, item_state)
			$end = nend($n, item_node)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
				$comma = True
			} else if tok(ctx, $s).kind != closing {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ pos: $s, nodes: $n, children: $c, diags: $d, ids: $ids, end: $end, comma: $comma }
}

type_parenthesized : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
type_parenthesized = |ctx, initial, nodes, children, diags, depth| {
	opening = tok(ctx, initial)
	var $s = sig(ctx, initial + 1)
	var $ids = []
	var $comma = False
	var $done = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		if is_text(ctx, $s, Arrow) or is_text(ctx, $s, FatArrow) {
			{ pos: ret_state, nodes: $n, children: $c, diags: $d, node: ret_node } = type_expression(ctx, $s + 1, $n, $c, $d, TypeArgument, depth + 1)
			ret_end = nend($n, ret_node)
			{ nodes: $n, children: $c, node: fn_node } = emit($n, $c, TypeFunction, opening.start, ret_end, $ids.append(ret_node))
			$s = sig(ctx, ret_state)
			$ids = [fn_node]
			$comma = False
			$done = True
		} else if tok(ctx, $s).kind == CloseRound or tok(ctx, $s).kind == EndOfFile {
			$done = True
		} else {
			before = $s
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = type_expression(ctx, $s, $n, $c, $d, TypeArgument, depth + 1)
			$ids = $ids.append(item_node)
			$s = sig(ctx, item_state)
			if is_text(ctx, $s, Comma) {
				$s = sig(ctx, $s + 1)
				$comma = True
			} else if !is_text(ctx, $s, Arrow) and !is_text(ctx, $s, FatArrow) {
				$done = True
			}
			if $s <= before {
				$done = True
			}
		}
	}
	ids = $ids
	comma = $comma
	n1 = $n
	c1 = $c
	closing = tok(ctx, $s)
	closed = closing.kind == CloseRound
	s1 = if (closed) $s + 1 else $s
	d1 = if (closed) $d else diag($d, UnclosedDelimiter, closing.start, closing.end)
	end = U64.max(if (closed) closing.end else closing.start, nend(n1, ids.last() ?? n1.len()))
	arrow = sig(ctx, s1)
	if ids.is_empty() and (is_text(ctx, arrow, Arrow) or is_text(ctx, arrow, FatArrow)) {
		{ pos: ret_state, nodes: n2, children: c2, diags: d2, node: ret_node } = type_expression(ctx, arrow + 1, n1, c1, d1, TypeArgument, depth + 1)
		ret_end = nend(n2, ret_node)
		finish1(ret_state, n2, c2, d2, TypeFunction, opening.start, ret_end, ret_node)
	} else {
		finish(s1, n1, c1, d1, if (ids.len() == 1 and !comma) TypeParenthesized else TypeTuple, opening.start, end, ids)
	}
}

type_fields : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
type_fields = |ctx, initial, nodes, children, diags, record, depth| {
	opening = tok(ctx, initial)
	closing = if (record) CloseCurly else CloseSquare
	var $s = initial + 1
	var $ids = []
	var $end = opening.end
	var $done = False
	var $extension = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == closing {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else if $extension {
			$d = diag($d, UnexpectedToken, t.start, t.end)
			$s = look
			$done = True
		} else if is_text(ctx, look, DotDot) {
			after = sig(ctx, look + 1)
			if tok(ctx, after).kind == LowerIdent and !is_text(ctx, after, Underscore) {
				{ pos: ext_state, nodes: $n, children: $c, diags: $d, node: ext_node } = type_expression(ctx, after, $n, $c, $d, TypeArgument, depth + 1)
				ext_end = nend($n, ext_node)
				{ nodes: $n, children: $c, node: built_node } = emit1($n, $c, TypeExtension, t.start, ext_end, ext_node)
				$s = sig(ctx, ext_state)
				$ids = $ids.append(built_node)
			} else {
				{ nodes: $n, children: $c, node: ext_node } = emit($n, $c, TypeExtension, t.start, t.end, [])
				$s = sig(ctx, after)
				$ids = $ids.append(ext_node)
			}
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			}
			$end = nend($n, $ids.last() ?? 0)
			$extension = True
		} else {
			before = look
			if record {
				if t.kind != LowerIdent {
					$d = diag($d, UnexpectedToken, t.start, t.end)
				}
				validated = look
				{ nodes: $n, children: $c, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
				var $field = sig(ctx, validated + 1)
				if is_text(ctx, $field, Question) {
					if byte(ctx.source, t.start) == '_' {
						$d = diag($d, UnexpectedToken, t.start, t.end)
					}
					$field = sig(ctx, $field + 1)
				}
				if is_text(ctx, $field, Colon) {
					$field = $field + 1
				} else {
					bad = tok(ctx, $field)
					$d = diag($d, UnexpectedToken, bad.start, bad.end)
				}
				{ pos: ty_state, nodes: $n, children: $c, diags: $d, node: ty_node } = type_expression(ctx, $field, $n, $c, $d, FullType, depth + 1)
				var $field_ids = [name_node, ty_node]
				$field = sig(ctx, ty_state)
				var $field_end = nend($n, ty_node)
				if is_text(ctx, $field, DoubleQuestion) {
					{ pos: default_state, nodes: $n, children: $c, diags: $d, node: default_node } = expression(ctx, $field + 1, $n, $c, $d, 0, depth + 1, False)
					$field_ids = $field_ids.append(default_node)
					$field_end = nend($n, default_node)
					$field = default_state
				}
				{ nodes: $n, children: $c, node: field_node } = emit($n, $c, TypeField, t.start, $field_end, $field_ids)
				$s = sig(ctx, $field)
				$ids = $ids.append(field_node)
			} else {
				{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = type_expression(ctx, look, $n, $c, $d, TypeItem, depth + 1)
				$s = sig(ctx, item_state)
				$ids = $ids.append(item_node)
			}
			$end = nend($n, $ids.last() ?? 0)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			}

			if $s <= before {
				$s = before + 1
			}
		}
	}
	finish($s, $n, $c, $d, if (record) TypeRecord else TypeTagUnion, opening.start, $end, $ids)
}

parse_annotation_after_name : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64, U64 -> R
parse_annotation_after_name = |ctx, initial, nodes, children, diags, name, start, depth| {
	{ pos: ty_state, nodes: n1, children: c1, diags: d1, node: ty_node } = type_expression(ctx, initial, nodes, children, diags, FullType, depth + 1)
	look = sig(ctx, ty_state)
	if is_text(ctx, look, Where) {
		{ pos: where_state, nodes: n2, children: c2, diags: d2, node: where_node } = parse_where_clause(ctx, look, n1, c1, d1, depth + 1)
		end = nend(n2, where_node)
		finish3(where_state, n2, c2, d2, TypeAnnotation, start, end, name, ty_node, where_node)
	} else {
		end = nend(n1, ty_node)
		finish2(ty_state, n1, c1, d1, TypeAnnotation, start, end, name, ty_node)
	}
}

parse_where_clause : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_where_clause = |ctx, initial, nodes, children, diags, depth| {
	start = tok(ctx, initial).start
	look = sig(ctx, initial + 1)
	opening = tok(ctx, look)
	d1 = if (opening.kind != OpenSquare) diag(diags, UnexpectedToken, opening.start, opening.end) else diags
	d2 = if (depth >= 128) diag(d1, ResourceLimit, start, opening.end) else d1
	var $s = if (opening.kind == OpenSquare) look + 1 else look
	var $ids = []
	var $done = opening.kind != OpenSquare or depth >= 128
	var $end = opening.end
	var $n = nodes
	var $c = children
	var $d = d2
	while !$done {
		receiver_state = sig(ctx, $s)
		receiver = tok(ctx, receiver_state)
		if receiver.kind == CloseSquare {
			$s = receiver_state + 1
			$end = receiver.end
			$done = True
		} else if receiver.kind == EndOfFile or receiver.kind == CloseCurly or receiver.kind == CloseRound {
			$d = diag($d, UnclosedDelimiter, receiver.start, receiver.end)
			$s = receiver_state
			$end = receiver.start
			$done = True
		} else {
			before = receiver_state
			valid_receiver = receiver.kind == LowerIdent and byte(ctx.source, receiver.start) != '_'
			if !valid_receiver {
				$d = diag($d, UnexpectedToken, receiver.start, receiver.end)
			}
			validated = receiver_state
			{ nodes: $n, children: $c, node: var_node } = emit($n, $c, TypeVariable, receiver.start, receiver.end, [])
			dot = sig(ctx, validated + 1)
			dotted = is_text(ctx, dot, Dot)
			if !dotted {
				$d = diag($d, UnexpectedToken, tok(ctx, dot).start, tok(ctx, dot).end)
			}
			name_state = if (dotted) dot + 1 else dot
			name = tok(ctx, name_state)
			if name.kind == LowerIdent and name.start == tok(ctx, dot).end and byte(ctx.source, name.start) != '_' {
				{ nodes: $n, children: $c, node: method_node } = emit($n, $c, Identifier, name.start, name.end, [])
				colon = sig(ctx, name_state + 1)
				colon_found = is_text(ctx, colon, Colon)
				if !colon_found {
					$d = diag($d, UnexpectedToken, tok(ctx, colon).start, tok(ctx, colon).end)
				}
				after_colon = if (colon_found) colon + 1 else colon
				{ pos: ty_state, nodes: $n, children: $c, diags: $d, node: ty_node } = type_expression(ctx, after_colon, $n, $c, $d, FullType, depth + 1)
				ty_end = nend($n, ty_node)
				{ nodes: $n, children: $c, node: clause_node } = emit3($n, $c, WhereMethod, receiver.start, ty_end, var_node, method_node, ty_node)
				$s = sig(ctx, ty_state)
				$ids = $ids.append(clause_node)
			} else if name.kind == UpperIdent and name.start == tok(ctx, dot).end and byte(ctx.source, name.start) != '@' {
				{ pos: alias_state, nodes: $n, children: $c, diags: $d, node: alias_node } = type_where_alias_name(ctx, name_state, $n, $c, $d, depth + 1)
				alias_end = nend($n, alias_node)
				{ nodes: $n, children: $c, node: clause_node } = emit2($n, $c, WhereAlias, receiver.start, alias_end, var_node, alias_node)
				$s = sig(ctx, alias_state)
				$ids = $ids.append(clause_node)
			} else {
				$d = diag($d, UnexpectedToken, name.start, name.end)
				$s = name_state
				$done = True
			}
			if !$ids.is_empty() {
				$end = nend($n, $ids.last() ?? 0)
			}
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseSquare {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	finish($s, $n, $c, if ($ids.is_empty()) diag($d, UnexpectedToken, start, $end) else $d, WhereClause, start, $end, $ids)
}

parse_type_declaration : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_type_declaration = |ctx, initial, nodes, children, diags, depth| {
	s = sig(ctx, initial)
	first = tok(ctx, s)
	where_alias = first.kind == LowerIdent
	{ pos: s1, nodes: n1, children: c1, diags: d1, receiver } = if where_alias {
		da = if (byte(ctx.source, first.start) == '_') diag(diags, UnexpectedToken, first.start, first.end) else diags
		{ nodes: na, children: ca, node: receiver_node } = emit(nodes, children, TypeVariable, first.start, first.end, [])
		dot = sig(ctx, s + 1)
		if is_text(ctx, dot, Dot) {
			{ pos: dot + 1, nodes: na, children: ca, diags: da, receiver: [receiver_node] }
		} else {
			{ pos: dot, nodes: na, children: ca, diags: diag(da, UnexpectedToken, tok(ctx, dot).start, tok(ctx, dot).end), receiver: [receiver_node] }
		}
	} else {
		{ pos: s, nodes, children, diags, receiver: [] }
	}
	name = tok(ctx, s1)
	d2 = if (!(name.kind == UpperIdent and byte(ctx.source, name.start) != '@')) diag(d1, UnexpectedToken, name.start, name.end) else d1
	validated = s1
	{ nodes: n2, children: c2, node: name_node } = emit(n1, c1, TypeName, name.start, name.end, [])
	s2 = sig(ctx, validated + if (name.kind == EndOfFile) 0 else 1)
	has_params = tok(ctx, s2).kind == OpenRound or tok(ctx, s2).kind == NoSpaceOpenRound
	var $s = if (has_params) s2 + 1 else s2
	var $params = [name_node]
	var $header_end = name.end
	var $done = !has_params
	var $n = n2
	var $c = c2
	var $d = d2
	while !$done {
		look = sig(ctx, $s)
		p = tok(ctx, look)
		if p.kind == CloseRound {
			$s = sig(ctx, look + 1)
			$header_end = p.end
			$done = True
		} else if p.kind == EndOfFile or p.kind == CloseCurly or p.kind == CloseSquare {
			$d = diag($d, UnclosedDelimiter, p.start, p.end)
			$s = look
			$done = True
		} else {
			if p.kind != LowerIdent {
				$d = diag($d, UnexpectedToken, p.start, p.end)
			}
			validated_arg = look
			{ nodes: $n, children: $c, node: param_node } = emit($n, $c, if (spelled1(ctx.source, p, '_')) TypeWildcard else TypeVariable, p.start, p.end, [])
			$params = $params.append(param_node)
			$header_end = p.end
			$s = sig(ctx, validated_arg + 1)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseRound {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
		}
	}
	s3 = $s
	d3 = $d
	{ nodes: n4, children: c4, node: header_node } = emit($n, $c, TypeHeader, name.start, $header_end, $params)
	operator_state = sig(ctx, s3)
	nominal = is_text(ctx, operator_state, ColonEquals) or is_text(ctx, operator_state, DoubleColon)
	valid_operator = is_text(ctx, operator_state, Colon) or (!where_alias and nominal)
	d4 = if (!valid_operator) diag(d3, UnexpectedToken, tok(ctx, operator_state).start, tok(ctx, operator_state).end) else d3
	after_operator = sig(ctx, operator_state + if (valid_operator) 1 else 0)
	ids0 = [header_node].concat(receiver)
	{ pos: s5, nodes: n5, children: c5, diags: d5, ids: ids1 } = if where_alias {
		if is_text(ctx, after_operator, Where) {
			{ pos: where_state, nodes: na, children: ca, diags: da, node: where_node } = parse_where_clause(ctx, after_operator, n4, c4, d4, depth + 1)
			{ pos: where_state, nodes: na, children: ca, diags: da, ids: ids0.append(where_node) }
		} else {
			{ pos: after_operator, nodes: n4, children: c4, diags: diag(d4, UnexpectedToken, tok(ctx, after_operator).start, tok(ctx, after_operator).end), ids: ids0 }
		}
	} else {
		{ pos: ty_state, nodes: na, children: ca, diags: da, node: ty_node } = type_expression(ctx, after_operator, n4, c4, d4, FullType, depth + 1)
		after_ty = sig(ctx, ty_state)
		ty_ids = ids0.append(ty_node)
		if is_text(ctx, after_ty, Where) {
			{ pos: where_state, nodes: nb, children: cb, diags: db, node: where_node } = parse_where_clause(ctx, after_ty, na, ca, da, depth + 1)
			{ pos: sig(ctx, where_state), nodes: nb, children: cb, diags: db, ids: ty_ids.append(where_node) }
		} else {
			{ pos: after_ty, nodes: na, children: ca, diags: da, ids: ty_ids }
		}
	}
	# Nodes are append-only, so ends of the existing ids are final here.
	var $end = first.end
	for id in ids1 {
		$end = U64.max($end, nend(n5, id))
	}
	ids_end = $end
	look = sig(ctx, s5)
	if is_text(ctx, look, Dot) and tok(ctx, sig(ctx, look + 1)).kind == OpenCurly {
		d6 = if (!(nominal and !where_alias)) diag(d5, UnexpectedToken, tok(ctx, look).start, tok(ctx, look).end) else d5
		associated_state = look
		{ pos: associated_end, nodes: n7, children: c7, diags: d7, node: associated_node } = parse_associated_block(ctx, sig(ctx, associated_state + 1), n5, c5, d6, depth + 1)
		ids = ids1.append(associated_node)
		end = U64.max(ids_end, nend(n7, associated_node))
		finish(associated_end, n7, c7, d7, TypeDeclaration, first.start, end, ids)
	} else {
		finish(s5, n5, c5, d5, TypeDeclaration, first.start, ids_end, ids1)
	}
}

parse_associated_block : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_associated_block = |ctx, initial, nodes, children, diags, depth| {
	opening = tok(ctx, initial)
	var $s = initial + 1
	var $ids = []
	var $end = opening.end
	var $done = depth >= 128
	var $n = nodes
	var $c = children
	var $d = if (depth >= 128) diag(diags, ResourceLimit, opening.start, opening.end) else diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else {
			before = look
			if is_text(ctx, look, Var) {
				$d = diag($d, UnexpectedToken, t.start, t.end)
			}
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = statement(ctx, look, $n, $c, $d, depth + 1)
			$ids = $ids.append(item_node)
			$s = item_state
			if (t.kind == UpperIdent or tok(ctx, sig(ctx, item_state)).kind == CloseCurly) and nget($n, item_node).kind == ExpressionStatement {
				item_end = nend($n, item_node)
				$d = diag($d, UnexpectedToken, t.start, item_end)
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node: block_node } = emit($n, $c, AssociatedBlock, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node: block_node }
}

# Distinguish local type declarations from uppercase tag/call expressions.
type_declaration_start : Ctx, U64 -> Bool
type_declaration_start = |ctx, initial| {
	var $s = sig(ctx, initial + 1)
	if tok(ctx, $s).kind == OpenRound or tok(ctx, $s).kind == NoSpaceOpenRound {
		$s = sig(ctx, $s + 1)
		var $done = False
		while !$done {
			if tok(ctx, $s).kind == CloseRound {
				$s = sig(ctx, $s + 1)
				$done = True
			} else if tok(ctx, $s).kind == LowerIdent {
				$s = sig(ctx, $s + 1)
				if is_text(ctx, $s, Comma) {
					$s = sig(ctx, $s + 1)
				} else if tok(ctx, $s).kind != CloseRound {
					return False
				}
			} else {
				return False
			}
		}
	}
	is_text(ctx, $s, Colon) or is_text(ctx, $s, ColonEquals) or is_text(ctx, $s, DoubleColon)
}

# Where aliases deliberately accept spaced qualification segments.
type_where_alias_name : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
type_where_alias_name = |ctx, initial, nodes, children, diags, depth| {
	first = tok(ctx, initial)
	var $s = initial + 1
	var $end = first.end
	var $done = False
	while !$done {
		dot = sig(ctx, $s)
		if is_text(ctx, dot, Dot) {
			name_state = dot + 1
			name = tok(ctx, name_state)
			if name.kind == UpperIdent and name.start == tok(ctx, dot).end and byte(ctx.source, name.start) != '@' {
				$s = name_state + 1
				$end = name.end
			} else {
				$done = True
			}
		} else {
			$done = True
		}
	}
	after = $s
	{ nodes: n1, children: c1, node: named_node } = emit(nodes, children, TypeName, first.start, $end, [])
	look = sig(ctx, after)
	if tok(ctx, look).kind == NoSpaceOpenRound {
		{ pos: items_state, nodes: n2, children: c2, diags: d1, ids: items_ids, end: items_end, comma: _ } = type_collection(ctx, look + 1, n1, c1, diags, CloseRound, TypeItem, depth + 1)
		finish_after(items_state, n2, c2, d1, TypeApply, first.start, items_end, named_node, items_ids)
	} else {
		{ pos: after, nodes: n1, children: c1, diags, node: named_node }
	}
}

# ---- Patterns ----------------------------------------------------------------

# Patterns have their own grammar: expression operators and value-only postfixes
# must never make an invalid binding pattern appear complete.
parse_pattern : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
parse_pattern = |ctx, initial, nodes, children, diags, alternatives, depth| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	if depth >= 128 {
		finish(s, nodes, children, diag(diags, ResourceLimit, t.start, t.end), ErrorNode, t.start, t.start, [])
	} else {
		{ pos: first_state, nodes: n1, children: c1, diags: d1, node: first_node } = pattern_primary(ctx, s, nodes, children, diags, alternatives, depth + 1)
		look = sig(ctx, first_state)
		if alternatives and is_text(ctx, look, Bar) {
			var $n = n1
			var $c = c1
			var $d = d1
			var $s = look
			var $ids = [first_node]
			while is_text(ctx, $s, Bar) {
				{ pos: next_state, nodes: $n, children: $c, diags: $d, node: next_node } = pattern_primary(ctx, sig(ctx, $s + 1), $n, $c, $d, alternatives, depth + 1)
				$ids = $ids.append(next_node)
				$s = sig(ctx, next_state)
			}
			end = nend($n, $ids.last() ?? first_node)
			{ nodes: n2, children: c2, node: alt_node } = emit($n, $c, AlternativePattern, t.start, end, $ids)
			{ pos: $s, nodes: n2, children: c2, diags: $d, node: alt_node }
		} else if is_text(ctx, look, As) {
			name_state = sig(ctx, look + 1)
			name = tok(ctx, name_state)
			if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
				{ nodes: n2, children: c2, node: named_node } = emit(n1, c1, Identifier, name.start, name.end, [])
				finish2(name_state + 1, n2, c2, d1, AliasPattern, t.start, name.end, first_node, named_node)
			} else {
				{ pos: name_state, nodes: n1, children: c1, diags: diag(d1, UnexpectedToken, name.start, name.end), node: first_node }
			}
		} else {
			{ pos: look, nodes: n1, children: c1, diags: d1, node: first_node }
		}
	}
}

pattern_primary : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
pattern_primary = |ctx, s, nodes, children, diags, alternatives, depth| {
	t = tok(ctx, s)
	if t.kind == LowerIdent {
		finish(s + 1, nodes, children, diags, if (is_text(ctx, s, Underscore)) WildcardPattern else IdentifierPattern, t.start, t.end, [])
	} else if is_text(ctx, s, Var) {
		look = sig(ctx, s + 1)
		name = tok(ctx, look)
		if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
			{ nodes: n1, children: c1, node: name_node } = emit(nodes, children, Identifier, name.start, name.end, [])
			finish1(look + 1, n1, c1, diags, MutablePattern, t.start, name.end, name_node)
		} else {
			finish(look, nodes, children, diag(diags, UnexpectedToken, name.start, name.end), ErrorNode, t.start, name.start, [])
		}
	} else if is_text(ctx, s, DotDot) {
		look = sig(ctx, s + 1)
		if is_text(ctx, look, As) {
			name_state = sig(ctx, look + 1)
			name = tok(ctx, name_state)
			if name.kind == LowerIdent and !is_text(ctx, name_state, Underscore) {
				{ nodes: n1, children: c1, node: named_node } = emit(nodes, children, Identifier, name.start, name.end, [])
				finish1(name_state + 1, n1, c1, diags, RestPattern, t.start, name.end, named_node)
			} else {
				finish(name_state, nodes, children, diag(diags, UnexpectedToken, name.start, name.end), RestPattern, t.start, t.end, [])
			}
		} else {
			next = tok(ctx, look)
			d1 = if (next.kind == LowerIdent and !is_text(ctx, look, Underscore)) diag(diags, UnexpectedToken, next.start, next.end) else diags
			finish(look, nodes, children, d1, RestPattern, t.start, t.end, [])
		}
	} else if t.kind == UpperIdent and byte(ctx.source, t.start) == '@' {
		finish(s + 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
	} else if t.kind == UpperIdent {
		{ nodes: n1, children: c1, node: name_node } = emit(nodes, children, Identifier, t.start, t.end, [])
		var $n = n1
		var $c = c1
		var $s = s + 1
		var $name = name_node
		var $done = False
		while !$done {
			look = sig(ctx, $s)
			if is_text(ctx, look, Dot) and tok(ctx, look).start == nend($n, $name) and tok(ctx, look + 1).kind == UpperIdent {
				field_state = look + 1
				field = tok(ctx, field_state)
				{ nodes: $n, children: $c, node: field_node } = emit($n, $c, Identifier, field.start, field.end, [])
				{ nodes: $n, children: $c, node: access_node } = emit2($n, $c, FieldAccess, t.start, field.end, $name, field_node)
				$s = field_state + 1
				$name = access_node
			} else {
				$s = look
				$done = True
			}
		}
		n2 = $n
		c2 = $c
		qualified = $name
		after_name = $s
		nominal = is_text(ctx, after_name, Dot)
		after = if (nominal) sig(ctx, after_name + 1) else after_name
		if tok(ctx, after).kind == NoSpaceOpenRound {
			{ pos: args_state, nodes: n3, children: c3, diags: d1, ids: args, end, comma: _ } = pattern_collection(ctx, after + 1, n2, c2, diags, CloseRound, True, depth)
			finish_after(args_state, n3, c3, d1, if (nominal) NominalPattern else TagPattern, t.start, end, qualified, args)
		} else if nominal and tok(ctx, after).kind == OpenCurly {
			{ pos: record_state, nodes: n3, children: c3, diags: d1, node: record_node } = pattern_record(ctx, after, n2, c2, diags, alternatives, depth)
			end = nend(n3, record_node)
			finish2(record_state, n3, c3, d1, NominalPattern, t.start, end, qualified, record_node)
		} else {
			d1 = if (nominal) diag(diags, UnexpectedToken, tok(ctx, after).start, tok(ctx, after).end) else diags
			end = nend(n2, qualified)
			finish1(after, n2, c2, d1, TagPattern, t.start, end, qualified)
		}
	} else if t.kind == OpenRound or t.kind == NoSpaceOpenRound or t.kind == OpenSquare {
		closing = if (t.kind == OpenSquare) CloseSquare else CloseRound
		{ pos: items_state, nodes: n1, children: c1, diags: d1, ids, end, comma: _ } = pattern_collection(ctx, s + 1, nodes, children, diags, closing, True, depth)
		finish(items_state, n1, c1, d1, if (t.kind == OpenSquare) ListPattern else TuplePattern, t.start, end, ids)
	} else if t.kind == OpenCurly {
		pattern_record(ctx, s, nodes, children, diags, alternatives, depth)
	} else if t.kind == StringStart {
		pattern_string(ctx, s, nodes, children, diags)
	} else if t.kind == Number or t.kind == SingleQuote {
		primary(ctx, s, nodes, children, diags, depth, False)
	} else {
		finish(s + if (t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly) 0 else 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
	}
}

pattern_collection : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.TokenKind, Bool, U64 -> C
pattern_collection = |ctx, initial, nodes, children, diags, closing, alternatives, depth| {
	var $s = initial
	var $ids = []
	var $end = tok(ctx, initial).start
	var $done = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == closing {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare or t.kind == CloseCurly {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else {
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = parse_pattern(ctx, look, $n, $c, $d, alternatives, depth + 1)
			$ids = $ids.append(item_node)
			$end = nend($n, item_node)
			$s = sig(ctx, item_state)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != closing and tok(ctx, $s).kind != EndOfFile {
				$d = diag($d, UnexpectedToken, tok(ctx, $s).start, tok(ctx, $s).end)
			}
			if $s <= look {
				$s = look + 1
			}
		}
	}
	{ pos: $s, nodes: $n, children: $c, diags: $d, ids: $ids, end: $end, comma: False }
}

pattern_record : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
pattern_record = |ctx, initial, nodes, children, diags, alternatives, depth| {
	start = tok(ctx, initial).start
	var $s = initial + 1
	var $ids = []
	var $end = tok(ctx, initial).end
	var $done = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = if (t.start > $end) t.start else $end
			$done = True
		} else {
			# Branches thread immutable names; only the destructure below writes the loop vars.
			{ pos: field_state, nodes: $n, children: $c, diags: $d, node: field_node } = if is_text(ctx, look, DotDot) {
				name_state = sig(ctx, look + 1)
				name = tok(ctx, name_state)
				if name.kind == LowerIdent and (is_lower(byte(ctx.source, name.start)) or byte(ctx.source, name.start) == '$') {
					{ nodes: n1, children: c1, node: named_node } = emit($n, $c, Identifier, name.start, name.end, [])
					finish1(name_state + 1, n1, c1, $d, RestPattern, t.start, name.end, named_node)
				} else {
					finish(name_state, $n, $c, $d, RestPattern, t.start, t.end, [])
				}
			} else if t.kind == LowerIdent and (is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$') {
				{ nodes: n1, children: c1, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
				after = sig(ctx, look + 1)
				if is_text(ctx, after, Colon) {
					{ pos: value_state, nodes: n2, children: c2, diags: d2, node: value_node } = parse_pattern(ctx, after + 1, n1, c1, $d, alternatives, depth + 1)
					end = nend(n2, value_node)
					finish2(value_state, n2, c2, d2, PatternField, t.start, end, name_node, value_node)
				} else {
					next = tok(ctx, after)
					d2 = if (!(is_text(ctx, after, Comma) or next.kind == CloseCurly)) diag($d, UnexpectedToken, next.start, next.end) else $d
					finish1(after, n1, c1, d2, PatternField, t.start, t.end, name_node)
				}
			} else {
				finish(look + 1, $n, $c, diag($d, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
			}
			$ids = $ids.append(field_node)
			$end = nend($n, field_node)
			$s = sig(ctx, field_state)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			}
			if $s <= look {
				$s = look + 1
			}
		}
	}
	finish($s, $n, $c, $d, RecordPattern, start, $end, $ids)
}

pattern_string : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic) -> R
pattern_string = |ctx, initial, nodes, children, diags| {
	first = tok(ctx, initial)
	var $s = initial + 1
	var $ids = []
	var $end = first.end
	var $captures = False
	var $done = False
	var $n = nodes
	var $c = children
	var $d = diags
	while !$done {
		t = tok(ctx, $s)
		if t.kind == StringPart {
			{ nodes: $n, children: $c, node: part_node } = emit($n, $c, StringSegment, t.start, t.end, [])
			$s = $s + 1
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			name_state = sig(ctx, $s + 1)
			name = tok(ctx, name_state)
			if name.kind != LowerIdent {
				$d = diag($d, UnexpectedToken, name.start, name.end)
			}
			{ nodes: $n, children: $c, node: named_node } = emit($n, $c, if (is_text(ctx, name_state, Underscore)) WildcardPattern else IdentifierPattern, name.start, name.end, [])
			close_state = sig(ctx, name_state + if (name.kind == EndOfFile or name.kind == InterpolationEnd) 0 else 1)
			close = tok(ctx, close_state)
			closed = close.kind == InterpolationEnd
			if !closed {
				$d = diag($d, UnclosedDelimiter, close.start, close.end)
			}
			$end = if (closed) close.end else name.end
			{ nodes: $n, children: $c, node: capture_node } = emit1($n, $c, CapturePattern, t.start, $end, named_node)
			$s = if (closed) close_state + 1 else close_state
			$ids = $ids.append(capture_node)
			$captures = True
			if !closed {
				$done = True
			}
		} else if t.kind == StringEnd {
			$s = $s + 1
			$end = t.end
			$done = True
		} else {
			$d = diag($d, UnclosedString, first.start, $end)
			$done = True
		}
	}
	finish($s, $n, $c, $d, if ($captures) PatternStringLiteral else StringLiteral, first.start, $end, $ids)
}

valid_pattern : Ctx, List(RocSyntax.Node), List(U64), U64 -> Bool
valid_pattern = |_ctx, nodes, children, root| {
	var $work = [root]
	var $depth = 1
	var $valid = True
	while $depth > 0 and $valid {
		$depth = $depth - 1
		id = $work.get($depth) ?? nodes.len()
		match nodes.get(id) {
			Err(_) => {
				$valid = False
			}
			Ok(n) => {
				$valid = n.kind == Identifier or n.kind == NumberLiteral or n.kind == StringLiteral or n.kind == CharacterLiteral or n.kind == Call or n.kind == Tuple or n.kind == ListLiteral or n.kind == Parenthesized
				var $i = 0
				while $valid and $i < n.children_len {
					child = children.get(n.children_start + $i) ?? nodes.len()
					$work = if ($depth < $work.len()) $work.set($depth, child) ?? $work else $work.append(child)
					$depth = $depth + 1
					$i = $i + 1
				}
			}
		}
	}
	$valid
}

# ---- Blocks, records, control flow and strings --------------------------------

parse_lambda : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64 -> R
parse_lambda = |ctx, initial, nodes, children, diags, start, depth| {
	var $n = nodes
	var $c = children
	var $d = diags
	var $s = initial
	var $ids = []
	var $done = False
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if is_text(ctx, look, Bar) {
			$s = look + 1
			$done = True
		} else if t.kind == EndOfFile {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$done = True
		} else {
			before = look
			{ pos: param_state, nodes: $n, children: $c, diags: $d, node: param_node } = parse_pattern(ctx, look, $n, $c, $d, False, depth + 1)
			$ids = $ids.append(param_node)
			$s = sig(ctx, param_state)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			}
			if $s <= before {
				$d = diag($d, UnexpectedToken, t.start, t.end)
				$s = before + 1
			}
		}
	}
	n1 = $n
	c1 = $c
	d1 = $d
	{ pos: body_state, nodes: n2, children: c2, diags: d2, node: body_node } = expression(ctx, $s, n1, c1, d1, 0, depth, False)
	end = nend(n2, body_node)
	finish(body_state, n2, c2, d2, Lambda, start, end, $ids.append(body_node))
}

parse_if : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64 -> R
parse_if = |ctx, initial, nodes, children, diags, start, depth| {
	{ pos: condition_state, nodes: n1, children: c1, diags: d1, node: condition_node } = expression(ctx, initial, nodes, children, diags, 0, depth, False)
	look = sig(ctx, condition_state)
	{ pos: then_branch_state, nodes: n2, children: c2, diags: d2, node: then_branch_node } = expression(ctx, look, n1, c1, d1, 0, depth, False)
	after = sig(ctx, then_branch_state)
	if is_text(ctx, after, Else) {
		{ pos: other_state, nodes: n3, children: c3, diags: d3, node: other_node } = expression(ctx, after + 1, n2, c2, d2, 0, depth, False)
		end = nend(n3, other_node)
		finish3(other_state, n3, c3, d3, If, start, end, condition_node, then_branch_node, other_node)
	} else {
		end = nend(n2, then_branch_node)
		finish2(after, n2, c2, d2, If, start, end, condition_node, then_branch_node)
	}
}

curly : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64, Bool, Bool -> R
curly = |ctx, initial, nodes, children, diags, start, depth, punned, forced| {
	look = sig(ctx, initial)
	first = tok(ctx, look)
	after = sig(ctx, look + 1)
	record = forced or first.kind == CloseCurly or is_text(ctx, look, DotDot) or (first.kind == LowerIdent and (is_text(ctx, after, Colon) or is_text(ctx, after, Comma) or (punned and tok(ctx, after).kind == CloseCurly)) and !(is_text(ctx, after, Colon) and record_starts_block(ctx, look)))
	extension = is_text(ctx, look, DotDot)
	{ pos: s1, nodes: n1, children: c1, diags: d1, count: count1, first: first1, rest: rest1, end: end1 } = if extension {
		{ pos: base_state, nodes: na, children: ca, diags: da, node: base_node } = expression(ctx, look + 1, nodes, children, diags, 0, depth + 1, False)
		comma = sig(ctx, base_state)
		has_comma = is_text(ctx, comma, Comma)
		base_end = nend(na, base_node)
		{ pos: if (has_comma) comma + 1 else comma, nodes: na, children: ca, diags: if (has_comma) da else diag(da, UnexpectedToken, tok(ctx, comma).start, tok(ctx, comma).end), count: 1, first: base_node, rest: [], end: base_end }
	} else {
		{ pos: initial, nodes, children, diags, count: 0, first: 0, rest: [], end: first.start }
	}
	var $n = n1
	var $c = c1
	var $d = d1
	var $s = s1
	var $count = count1
	var $first = first1
	var $rest = rest1
	var $end = end1
	var $done = False
	while !$done {
		$s = sig(ctx, $s)
		t = tok(ctx, $s)
		sync = declaration_sync(ctx, $s)
		if record and sync.sync {
			$d = diag($d, UnclosedDelimiter, sync.start, sync.end)
			$end = sync.end
			$done = True
		} else if t.kind == CloseCurly {
			$end = t.end
			$s = $s + 1
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseRound or t.kind == CloseSquare {
			$end = if (t.start > $end) t.start else $end
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$done = True
		} else {
			before = $s
			if record {
				name_state = $s + 1
				{ nodes: $n, children: $c, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
				var $field_state = name_state
				var $field_node = name_node
				field_look = sig(ctx, name_state)
				if t.kind != LowerIdent or !(is_lower(byte(ctx.source, t.start)) or byte(ctx.source, t.start) == '$') {
					$d = diag($d, UnsupportedSyntax, t.start, t.end)
					{ nodes: $n, children: $c, node: next_field_node } = emit($n, $c, UnsupportedNode, t.start, t.end, [])
					$field_state = field_look
					$field_node = next_field_node
				} else if is_text(ctx, field_look, Colon) and is_text(ctx, significant_position(ctx.tokens, field_look + 1), Underscore) and (is_text(ctx, significant_position(ctx.tokens, significant_position(ctx.tokens, field_look + 1) + 1), Comma) or source_token(ctx, significant_position(ctx.tokens, significant_position(ctx.tokens, field_look + 1) + 1)).kind == CloseCurly) {
					value_pos = significant_position(ctx.tokens, field_look + 1)
					unset_end = source_token(ctx, value_pos).end
					{ nodes: $n, children: $c, node: unset_node } = emit1($n, $c, UnsetField, t.start, unset_end, name_node)
					$field_state = value_pos + 1
					$field_node = unset_node
				} else if is_text(ctx, field_look, Colon) {
					{ pos: value_state, nodes: $n, children: $c, diags: $d, node: value_node } = expression(ctx, field_look + 1, $n, $c, $d, 0, depth, True)
					end = nend($n, value_node)
					{ nodes: $n, children: $c, node: next_field_node } = emit2($n, $c, RecordField, t.start, end, name_node, value_node)
					$field_state = value_state
					$field_node = next_field_node
				} else {
					{ nodes: $n, children: $c, node: next_field_node } = emit1($n, $c, RecordField, t.start, t.end, name_node)
					$field_state = field_look
					$field_node = next_field_node
				}
				if $count == 0 {
					$first = $field_node
				} else {
					$rest = $rest.append($field_node)
				}
				$count = $count + 1
				$end = nend($n, $field_node)
				$s = sig(ctx, $field_state)
				if is_text(ctx, $s, Comma) {
					$s = $s + 1
				} else if tok(ctx, $s).kind != CloseCurly and tok(ctx, $s).kind != EndOfFile and (tok(ctx, $s).kind != LowerIdent or nget($n, $field_node).children_len != 2) {
					$d = diag($d, UnexpectedToken, tok(ctx, $s).start, tok(ctx, $s).end)
				}
			} else {
				{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = statement(ctx, $s, $n, $c, $d, depth)
				if $count == 0 {
					$first = item_node
				} else {
					$rest = $rest.append(item_node)
				}
				$count = $count + 1
				$end = nend($n, item_node)
				$s = item_state
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	n2 = $n
	c2 = $c
	d2 = $d
	s2 = $s
	count2 = $count
	first2 = $first
	rest2 = $rest
	end2 = $end
	look_suffix = sig(ctx, s2)
	dot = tok(ctx, look_suffix)
	name_state = look_suffix + 1
	builder = record and !extension and !forced and is_text(ctx, look_suffix, Dot) and dot.start == end2 and tok(ctx, name_state).kind == UpperIdent
	if builder {
		var $bn = n2
		var $bc = c2
		var $mapper_ids = []
		var $mapper_state = name_state
		var $mapper_end = tok(ctx, name_state).end
		var $mapper_done = False
		while !$mapper_done {
			t = tok(ctx, $mapper_state)
			{ nodes: $bn, children: $bc, node: named_node } = emit($bn, $bc, Identifier, t.start, t.end, [])
			$mapper_state = $mapper_state + 1
			$mapper_ids = $mapper_ids.append(named_node)
			$mapper_end = t.end
			if is_text(ctx, $mapper_state, Dot) and tok(ctx, $mapper_state).start == $mapper_end and tok(ctx, $mapper_state + 1).kind == UpperIdent {
				$mapper_state = $mapper_state + 1
			} else {
				$mapper_done = True
			}
		}
		# Keep the mapper as a normal qualified identifier tree.
		var $mapper = $mapper_ids.first() ?? 0
		var $index = 1.U64
		while $index < $mapper_ids.len() {
			field = $mapper_ids.get($index) ?? $mapper
			access_start = nstart($bn, $mapper)
			access_end = nend($bn, field)
			{ nodes: $bn, children: $bc, node: access_node } = emit2($bn, $bc, FieldAccess, access_start, access_end, $mapper, field)
			$mapper = access_node
			$index = $index + 1
		}
		finish_lead_items($mapper_state, $bn, $bc, d2, RecordBuilder, start, $mapper_end, $mapper, count2, first2, rest2)
	} else {
		finish_items(s2, n2, c2, d2, if (extension) RecordUpdate else if (record) Record else Block, start, end2, count2, first2, rest2)
	}
}

parse_match : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, U64 -> R
parse_match = |ctx, initial, nodes, children, diags, start, depth| {
	{ pos: value_state, nodes: n1, children: c1, diags: d1, node: value_node } = expression(ctx, initial, nodes, children, diags, 0, depth, False)
	look = sig(ctx, value_state)
	if tok(ctx, look).kind != OpenCurly {
		end = nend(n1, value_node)
		finish1(look, n1, c1, diag(d1, UnexpectedToken, tok(ctx, look).start, tok(ctx, look).end), Match, start, end, value_node)
	} else {
		var $n = n1
		var $c = c1
		var $d = d1
		var $s = look + 1
		var $ids = [value_node]
		var $end = tok(ctx, look).end
		var $done = False
		while !$done {
			$s = sig(ctx, $s)
			t = tok(ctx, $s)
			if t.kind == CloseCurly {
				$end = t.end
				$s = $s + 1
				$done = True
			} else if t.kind == EndOfFile {
				$d = diag($d, UnclosedDelimiter, t.start, t.end)
				$end = if (t.start > $end) t.start else $end
				$done = True
			} else {
				before = $s
				{ pos: pattern_state, nodes: $n, children: $c, diags: $d, node: pattern_node } = parse_pattern(ctx, $s, $n, $c, $d, True, depth + 1)
				var $arrow = sig(ctx, pattern_state)
				var $branch_ids = [pattern_node]
				if is_text(ctx, $arrow, If) {
					# Only the guard expression sees `in_guard`; the arrow and the
					# branch value continue with the outer flag.
					{ pos: guard_state, nodes: $n, children: $c, diags: $d, node: guard_node } = expression({ ..ctx, in_guard: True }, $arrow + 1, $n, $c, $d, 0, depth + 1, False)
					$arrow = sig(ctx, guard_state)
					$branch_ids = $branch_ids.append(guard_node)
				}
				arrow = $arrow
				if is_text(ctx, arrow, FatArrow) {
					{ pos: branch_value_state, nodes: $n, children: $c, diags: $d, node: branch_value_node } = expression(ctx, arrow + 1, $n, $c, $d, 0, depth, False)
					branch_end = nend($n, branch_value_node)
					{ nodes: $n, children: $c, node: branch_node } = emit($n, $c, MatchBranch, t.start, branch_end, $branch_ids.append(branch_value_node))
					$ids = $ids.append(branch_node)
					$s = sig(ctx, branch_value_state)
					$end = branch_end
					if is_text(ctx, $s, Comma) {
						$s = $s + 1
					}
				} else {
					$d = diag($d, UnsupportedSyntax, t.start, tok(ctx, arrow).end)
					$s = arrow
					$ids = $ids.append(pattern_node)
				}
				if $s <= before {
					$s = before + 1
				}
			}
		}
		n2 = $n
		c2 = $c
		d2 = $d
		ids2 = $ids
		end2 = $end
		if ids2.len() == 1 {
			finish($s, n2, c2, diag(d2, UnexpectedToken, start, end2), Match, start, end2, ids2)
		} else {
			finish($s, n2, c2, d2, Match, start, end2, ids2)
		}
	}
}

parse_for : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64, Bool, U64 -> R
parse_for = |ctx, initial, nodes, children, diags, start, stream, depth| {
	{ pos: pattern_state, nodes: n1, children: c1, diags: d1, node: pattern_node } = parse_pattern(ctx, initial, nodes, children, diags, False, depth + 1)
	look = sig(ctx, pattern_state)
	in_found = is_text(ctx, look, In)
	d2 = if (in_found) d1 else diag(d1, UnexpectedToken, tok(ctx, look).start, tok(ctx, look).end)
	validated = if (in_found) look + 1 else look
	{ pos: list_state, nodes: n2, children: c2, diags: d3, node: list_node } = expression(ctx, validated, n1, c1, d2, 0, depth + 1, False)
	{ pos: body_state, nodes: n3, children: c3, diags: d4, node: body_node } = expression(ctx, list_state, n2, c2, d3, 0, depth + 1, False)
	end = U64.max(nend(n3, body_node), U64.max(nend(n3, pattern_node), nend(n3, list_node)))
	finish3(body_state, n3, c3, d4, if (stream) ForStream else For, start, end, pattern_node, list_node, body_node)
}

parse_string : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_string = |ctx, initial, nodes, children, diags, depth| {
	first = tok(ctx, initial)
	multiline = first.kind == MultilineStringStart
	var $n = nodes
	var $c = children
	var $d = diags
	var $s = initial + 1
	var $ids = []
	var $end = first.end
	var $done = False
	while !$done {
		look = if (multiline) sig(ctx, $s) else $s
		t = tok(ctx, look)
		if t.kind == StringPart {
			{ nodes: $n, children: $c, node: part_node } = emit($n, $c, StringSegment, t.start, t.end, [])
			$s = look + 1
			$ids = $ids.append(part_node)
			$end = t.end
		} else if t.kind == InterpolationStart {
			{ pos: value_state, nodes: $n, children: $c, diags: $d, node: value_node } = expression(ctx, look + 1, $n, $c, $d, 0, depth + 1, False)
			after = sig(ctx, value_state)
			close = tok(ctx, after)
			closed = close.kind == InterpolationEnd
			if !closed {
				$d = diag($d, UnclosedDelimiter, close.start, close.end)
			}
			validated = if (closed) after + 1 else after
			end = if (closed) close.end else nend($n, value_node)
			{ nodes: $n, children: $c, node: interpolation_node } = emit1($n, $c, Interpolation, t.start, end, value_node)
			$s = validated
			$ids = $ids.append(interpolation_node)
			$end = end
			if !closed {
				$done = True
			}
		} else if multiline and t.kind == MultilineStringStart {
			$s = look + 1
			$end = t.end
		} else if !multiline and t.kind == StringEnd {
			$s = look + 1
			$end = t.end
			$done = True
		} else {
			if !multiline {
				$d = diag($d, UnclosedString, first.start, $end)
			}
			$s = look
			$done = True
		}
	}
	n1 = $n
	c1 = $c
	d1 = $d
	{ nodes: n2, children: c2, node: literal_node } = emit(n1, c1, if (multiline) MultilineStringLiteral else StringLiteral, first.start, $end, $ids)
	literal_suffix(ctx, $s, n2, c2, d1, literal_node, multiline)
}

record_starts_block : Ctx, U64 -> Bool
record_starts_block = |ctx, s| {
	var $pos = s + 2
	var $nesting = 0.U64
	var $found = False
	var $done = False
	while !$done {
		t = source_token(ctx, $pos)
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
		} else if $nesting == 0 and t.kind == LowerIdent and is_text(ctx, significant_position(ctx.tokens, $pos + 1), Equals) {
			$found = True
			$done = True
		}
		$pos = $pos + 1
	}
	$found
}

# ---- Platform headers ---------------------------------------------------------

parse_platform_header : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
parse_platform_header = |ctx, initial, nodes, children, diags, depth| {
	start = tok(ctx, initial).start
	{ pos: name_state, nodes: n1, children: c1, diags: d1, node: name_node } = platform_header_string(ctx, sig(ctx, initial + 1), nodes, children, diags, True)
	{ pos: requires_start, diags: d2 } = platform_header_keyword(ctx, name_state, d1, "requires")
	{ pos: requires_state, nodes: n2, children: c2, diags: d3, node: requires_node } = platform_requires(ctx, requires_start, n1, c1, d2, depth + 1)
	{ pos: exposed_start, diags: d4 } = platform_header_keyword(ctx, requires_state, d3, "exposes")
	{ pos: exposed_state, nodes: n3, children: c3, diags: d5, ids: exposed_ids, end: _, comma: _ } = exposed_collection(ctx, exposed_start, n2, c2, d4)
	{ pos: packages_start, diags: d6 } = platform_header_keyword(ctx, exposed_state, d5, "packages")
	packages_opening = tok(ctx, packages_start)
	d7 = if (packages_opening.kind != OpenCurly) diag(d6, UnexpectedToken, packages_opening.start, packages_opening.end) else d6
	{ pos: packages_state, nodes: n4, children: c4, diags: d8, node: packages_node } = dependency_record(ctx, packages_start, n3, c3, d7, False, depth + 1)
	package_info = nget(n4, packages_node)
	var $d = d8
	var $index = 0.U64
	while $index < package_info.children_len {
		field_id = c4.get(package_info.children_start + $index) ?? packages_node
		field = n4.get(field_id) ?? package_info
		if field.children_len > 1 {
			value_id = c4.get(field.children_start + 1) ?? field_id
			value = n4.get(value_id) ?? field
			if value.kind == PlatformReference {
				$d = diag($d, UnexpectedToken, value.start, value.end)
			}
		}
		$index = $index + 1
	}
	d9 = $d
	{ pos: provides_start, diags: d10 } = platform_header_keyword(ctx, packages_state, d9, "provides")
	{ pos: provides_state, nodes: n5, children: c5, diags: d11, node: provides_node } = platform_symbol_map(ctx, provides_start, n4, c4, d10, PlatformProvides, depth + 1)
	s1 = sig(ctx, provides_state)
	ids1 = [name_node, requires_node].concat(exposed_ids).append(packages_node).append(provides_node)
	{ pos: s2, nodes: n6, children: c6, diags: d12, ids: ids2 } = if is_text(ctx, s1, Hosted) {
		{ pos: hosted_state, nodes: na, children: ca, diags: da, node: hosted_node } = platform_symbol_map(ctx, sig(ctx, s1 + 1), n5, c5, d11, PlatformHosted, depth + 1)
		{ pos: sig(ctx, hosted_state), nodes: na, children: ca, diags: da, ids: ids1.append(hosted_node) }
	} else {
		{ pos: s1, nodes: n5, children: c5, diags: d11, ids: ids1 }
	}
	{ pos: s3, nodes: n7, children: c7, diags: d13, ids: ids3 } = if is_text(ctx, s2, Targets) {
		{ pos: targets_start, diags: da } = platform_header_keyword(ctx, s2 + 1, d12, ":")
		{ pos: targets_state, nodes: nb, children: cb, diags: db, node: targets_node } = platform_targets(ctx, targets_start, n6, c6, da, depth + 1)
		{ pos: targets_state, nodes: nb, children: cb, diags: db, ids: ids2.append(targets_node) }
	} else {
		{ pos: s2, nodes: n6, children: c6, diags: d12, ids: ids2 }
	}
	end = ids3.fold(start, |furthest, id| U64.max(furthest, nend(n7, id)))
	finish(s3, n7, c7, d13, Header, start, end, ids3)
}

platform_header_keyword : Ctx, U64, List(RocSyntax.Diagnostic), Str -> { pos : U64, diags : List(RocSyntax.Diagnostic) }
platform_header_keyword = |ctx, initial, diags, text| {
	s = sig(ctx, initial)
	if spell(ctx, s, text) {
		{ pos: sig(ctx, s + 1), diags }
	} else {
		t = tok(ctx, s)
		{ pos: s, diags: diag(diags, UnexpectedToken, t.start, t.end) }
	}
}

platform_header_string : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool -> R
platform_header_string = |ctx, initial, nodes, children, diags, plain| {
	s = sig(ctx, initial)
	opening = tok(ctx, s)
	if opening.kind != StringStart {
		finish(s + if (opening.kind == EndOfFile) 0 else 1, nodes, children, diag(diags, UnexpectedToken, opening.start, opening.end), ErrorNode, opening.start, opening.end, [])
	} else {
		var $s = s + 1
		var $end = opening.end
		var $parts = 0.U64
		var $plain = True
		while tok(ctx, $s).kind != StringEnd and tok(ctx, $s).kind != EndOfFile {
			t = tok(ctx, $s)
			$plain = $plain and t.kind == StringPart
			$parts = $parts + 1
			$end = t.end
			$s = $s + 1
		}
		parts_end = $end
		parts_pos = $s
		single_part = $plain and $parts <= 1
		{ nodes: n1, children: c1, ids } = if parts_end > opening.end {
			{ nodes: na, children: ca, node: part_node } = emit(nodes, children, StringSegment, opening.end, parts_end, [])
			{ nodes: na, children: ca, ids: [part_node] }
		} else {
			{ nodes, children, ids: [] }
		}
		closing = tok(ctx, parts_pos)
		closed = closing.kind == StringEnd
		end = if (closed) closing.end else parts_end
		pos = if (closed) parts_pos + 1 else parts_pos
		d1 = if (closed) diags else diag(diags, UnclosedString, opening.start, parts_end)
		d2 = if (plain and !single_part) diag(d1, UnexpectedToken, opening.start, end) else d1
		finish(pos, n1, c1, d2, StringLiteral, opening.start, end, ids)
	}
}

platform_requires : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
platform_requires = |ctx, initial, nodes, children, diags, depth| {
	opening = tok(ctx, initial)
	d1 = if (opening.kind != OpenCurly) diag(diags, UnexpectedToken, opening.start, opening.end) else diags
	d2 = if (depth >= 128) diag(d1, ResourceLimit, opening.start, opening.end) else d1
	s0 = if (opening.kind == OpenCurly) initial + 1 else initial
	initial_look = sig(ctx, s0)
	empty = tok(ctx, initial_look).kind == CloseCurly
	after_empty = sig(ctx, initial_look + 1)
	reopened = tok(ctx, after_empty).kind == OpenCurly
	var $s = if (!empty) s0 else if (reopened) after_empty + 1 else after_empty
	var $ids = []
	var $end = if (empty) tok(ctx, initial_look).end else opening.end
	var $done = opening.kind != OpenCurly or depth >= 128 or (empty and !reopened)
	var $n = nodes
	var $c = children
	var $d = d2
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$end = t.end
			$s = look + 1
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseSquare or t.kind == CloseRound {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = t.start
			$done = True
		} else {
			before = look
			var $entry_ids = []
			$s = look
			if t.kind == OpenSquare {
				$s = sig(ctx, $s + 1)
				var $aliases_done = False
				while !$aliases_done {
					alias = tok(ctx, $s)
					if alias.kind == CloseSquare {
						$s = $s + 1
						$aliases_done = True
					} else if alias.kind == EndOfFile or alias.kind == CloseCurly or alias.kind == CloseRound {
						$d = diag($d, UnclosedDelimiter, alias.start, alias.end)
						$aliases_done = True
					} else {
						if !(alias.kind == UpperIdent and byte(ctx.source, alias.start) != '@') {
							$d = diag($d, UnexpectedToken, alias.start, alias.end)
						}
						alias_state = $s + 1
						{ nodes: $n, children: $c, node: alias_node } = emit($n, $c, TypeName, alias.start, alias.end, [])
						{ pos: rigid_state, diags: $d } = platform_header_keyword(ctx, alias_state, $d, ":")
						rigid = tok(ctx, rigid_state)
						if !(rigid.kind == LowerIdent and byte(ctx.source, rigid.start) != '_') {
							$d = diag($d, UnexpectedToken, rigid.start, rigid.end)
						}
						rigid_built = rigid_state + if (rigid.kind == EndOfFile) 0 else 1
						{ nodes: $n, children: $c, node: rigid_node } = emit($n, $c, TypeVariable, rigid.start, rigid.end, [])
						{ nodes: $n, children: $c, node: built_node } = emit2($n, $c, RequiredType, alias.start, rigid.end, alias_node, rigid_node)
						$s = sig(ctx, rigid_built)
						$entry_ids = $entry_ids.append(built_node)
						if is_text(ctx, $s, Comma) {
							$s = sig(ctx, $s + 1)
						} else if tok(ctx, $s).kind != CloseSquare {
							bad = tok(ctx, $s)
							$d = diag($d, UnexpectedToken, bad.start, bad.end)
							$aliases_done = True
						}
					}
				}
				{ pos: for_state, diags: $d } = platform_header_keyword(ctx, $s, $d, "for")
				$s = for_state
			}
			name_look = sig(ctx, $s)
			name = tok(ctx, name_look)
			if !(name.kind == LowerIdent and byte(ctx.source, name.start) != '_') {
				$d = diag($d, UnexpectedToken, name.start, name.end)
			}
			named_state = name_look + if (name.kind == EndOfFile) 0 else 1
			{ nodes: $n, children: $c, node: named_node } = emit($n, $c, Identifier, name.start, name.end, [])
			{ pos: type_start, diags: $d } = platform_header_keyword(ctx, named_state, $d, ":")
			{ pos: type_state, nodes: $n, children: $c, diags: $d, node: type_node } = type_expression(ctx, type_start, $n, $c, $d, FullType, depth + 1)
			$entry_ids = $entry_ids.append(named_node).append(type_node)
			entry_end = $entry_ids.fold(t.end, |furthest, id| U64.max(furthest, nend($n, id)))
			{ nodes: $n, children: $c, node: entry_node } = emit($n, $c, RequiredValue, t.start, entry_end, $entry_ids)
			$s = sig(ctx, type_state)
			$ids = $ids.append(entry_node)
			$end = entry_end
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseCurly {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node } = emit($n, $c, PlatformRequires, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node }
}

platform_symbol_map : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), RocSyntax.NodeKind, U64 -> R
platform_symbol_map = |ctx, initial, nodes, children, diags, kind, depth| {
	opening = tok(ctx, initial)
	d1 = if (opening.kind != OpenCurly) diag(diags, UnexpectedToken, opening.start, opening.end) else diags
	var $s = if (opening.kind == OpenCurly) initial + 1 else initial
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenCurly or depth >= 128
	var $n = nodes
	var $c = children
	var $d = if (depth >= 128) diag(d1, ResourceLimit, opening.start, opening.end) else d1
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = t.start
			$done = True
		} else {
			before = look
			{ pos: symbol_state, nodes: $n, children: $c, diags: $d, node: symbol_node } = platform_header_string(ctx, look, $n, $c, $d, True)
			{ pos: target_state, diags: $d } = platform_header_keyword(ctx, symbol_state, $d, ":")
			target = tok(ctx, target_state)
			var $target_state = target_state
			var $target_end = target.end
			if target.kind == UpperIdent and byte(ctx.source, target.start) != '@' {
				$target_state = target_state + 1
				var $path_done = False
				var $lower = False
				while !$path_done {
					dot = tok(ctx, $target_state)
					name_state = $target_state + 1
					name = tok(ctx, name_state)
					if spelled1(ctx.source, dot, '.') and dot.start == $target_end and name.start == dot.end and (name.kind == UpperIdent or name.kind == LowerIdent) and (is_lower(byte(ctx.source, name.start)) or is_upper(byte(ctx.source, name.start))) {
						$target_end = name.end
						$target_state = name_state + 1
						if name.kind == LowerIdent {
							$lower = True
							$path_done = True
						}
					} else {
						$path_done = True
					}
				}
				if !$lower {
					$d = diag($d, UnexpectedToken, target.start, $target_end)
				}
			} else if target.kind == LowerIdent and byte(ctx.source, target.start) != '_' {
				$target_state = target_state + 1
			} else {
				$d = diag($d, UnexpectedToken, target.start, target.end)
				$target_state = target_state + if (target.kind == EndOfFile) 0 else 1
			}
			{ nodes: $n, children: $c, node: named_node } = emit($n, $c, Identifier, target.start, $target_end, [])
			entry_end = U64.max(nend($n, symbol_node), $target_end)
			{ nodes: $n, children: $c, node: entry_node } = emit2($n, $c, SymbolMapping, t.start, entry_end, symbol_node, named_node)
			$s = sig(ctx, $target_state)
			$ids = $ids.append(entry_node)
			$end = entry_end
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseCurly {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node } = emit($n, $c, kind, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node }
}

platform_targets : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
platform_targets = |ctx, initial, nodes, children, diags, depth| {
	opening = tok(ctx, initial)
	d1 = if (opening.kind != OpenCurly) diag(diags, UnexpectedToken, opening.start, opening.end) else diags
	var $s = if (opening.kind == OpenCurly) initial + 1 else initial
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenCurly or depth >= 128
	var $n = nodes
	var $c = children
	var $d = if (depth >= 128) diag(d1, ResourceLimit, opening.start, opening.end) else d1
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = t.start
			$done = True
		} else {
			before = look
			valid_name = t.kind == LowerIdent and byte(ctx.source, t.start) != '_'
			if !valid_name {
				$d = diag($d, UnexpectedToken, t.start, t.end)
			}
			{ nodes: $n, children: $c, node: named_node } = emit($n, $c, Identifier, t.start, t.end, [])
			{ pos: value_state, diags: $d } = platform_header_keyword(ctx, look + 1, $d, ":")
			if spell(ctx, look, "inputs_dir") and tok(ctx, value_state).kind == StringStart {
				{ pos: path_state, nodes: $n, children: $c, diags: $d, node: path_node } = platform_header_string(ctx, value_state, $n, $c, $d, False)
				path_end = nend($n, path_node)
				{ nodes: $n, children: $c, node: entry_node } = emit1($n, $c, TargetInputsDirectory, t.start, path_end, path_node)
				$s = sig(ctx, path_state)
				$ids = $ids.append(entry_node)
				$end = nend($n, entry_node)
			} else if tok(ctx, value_state).kind == OpenCurly {
				{ pos: config_state, nodes: $n, children: $c, diags: $d, node: config_node } = platform_target_config(ctx, value_state, $n, $c, $d, depth + 1)
				config_end = nend($n, config_node)
				{ nodes: $n, children: $c, node: entry_node } = emit2($n, $c, TargetEntry, t.start, config_end, named_node, config_node)
				$s = sig(ctx, config_state)
				$ids = $ids.append(entry_node)
				$end = nend($n, entry_node)
			} else {
				bad = tok(ctx, value_state)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$s = value_state
				$done = True
			}
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node } = emit($n, $c, PlatformTargets, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node }
}

platform_target_config : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), U64 -> R
platform_target_config = |ctx, initial, nodes, children, diags, depth| {
	opening = tok(ctx, initial)
	var $s = initial + 1
	var $ids = []
	var $end = opening.end
	var $done = depth >= 128
	var $n = nodes
	var $c = children
	var $d = if (depth >= 128) diag(diags, ResourceLimit, opening.start, opening.end) else diags
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseCurly {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = t.start
			$done = True
		} else {
			before = look
			if !(t.kind == LowerIdent and byte(ctx.source, t.start) != '_') {
				$d = diag($d, UnexpectedToken, t.start, t.end)
			}
			{ nodes: $n, children: $c, node: name_node } = emit($n, $c, Identifier, t.start, t.end, [])
			after = sig(ctx, look + 1)
			var $field_ids = [name_node]
			$s = after
			$end = t.end
			if tok(ctx, after).kind != CloseCurly and !is_text(ctx, after, Comma) {
				{ pos: value_state, diags: $d } = platform_header_keyword(ctx, after, $d, ":")
				files = spell(ctx, look, "inputs")
				{ pos: built_state, nodes: $n, children: $c, diags: $d, node: built_node } = if (files) platform_target_list(ctx, value_state, $n, $c, $d, True, depth + 1) else platform_target_value(ctx, value_state, $n, $c, $d, False, depth + 1)
				$s = built_state
				$field_ids = $field_ids.append(built_node)
				$end = nend($n, built_node)
			}
			{ nodes: $n, children: $c, node: field_node } = emit($n, $c, TargetField, t.start, $end, $field_ids)
			$s = sig(ctx, $s)
			$ids = $ids.append(field_node)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseCurly {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node } = emit($n, $c, TargetConfig, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node }
}

platform_target_list : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
platform_target_list = |ctx, initial, nodes, children, diags, files, depth| {
	opening = tok(ctx, initial)
	d1 = if (opening.kind != OpenSquare) diag(diags, UnexpectedToken, opening.start, opening.end) else diags
	var $s = if (opening.kind == OpenSquare) initial + 1 else initial
	var $ids = []
	var $end = opening.end
	var $done = opening.kind != OpenSquare or depth >= 128
	var $n = nodes
	var $c = children
	var $d = if (depth >= 128) diag(d1, ResourceLimit, opening.start, opening.end) else d1
	while !$done {
		look = sig(ctx, $s)
		t = tok(ctx, look)
		if t.kind == CloseSquare {
			$s = look + 1
			$end = t.end
			$done = True
		} else if t.kind == EndOfFile or t.kind == CloseCurly or t.kind == CloseRound {
			$d = diag($d, UnclosedDelimiter, t.start, t.end)
			$s = look
			$end = t.start
			$done = True
		} else {
			before = look
			{ pos: item_state, nodes: $n, children: $c, diags: $d, node: item_node } = platform_target_value(ctx, look, $n, $c, $d, files, depth + 1)
			$ids = $ids.append(item_node)
			$s = sig(ctx, item_state)
			$end = nend($n, item_node)
			if is_text(ctx, $s, Comma) {
				$s = $s + 1
			} else if tok(ctx, $s).kind != CloseSquare {
				bad = tok(ctx, $s)
				$d = diag($d, UnexpectedToken, bad.start, bad.end)
				$done = True
			}
			if $s <= before {
				$s = before + 1
			}
		}
	}
	{ nodes: n1, children: c1, node } = emit($n, $c, TargetList, opening.start, $end, $ids)
	{ pos: $s, nodes: n1, children: c1, diags: $d, node }
}

platform_target_value : Ctx, U64, List(RocSyntax.Node), List(U64), List(RocSyntax.Diagnostic), Bool, U64 -> R
platform_target_value = |ctx, initial, nodes, children, diags, file, depth| {
	s = sig(ctx, initial)
	t = tok(ctx, s)
	if depth >= 128 {
		finish(s, nodes, children, diag(diags, ResourceLimit, t.start, t.end), ErrorNode, t.start, t.start, [])
	} else if t.kind == StringStart {
		{ pos: string_state, nodes: n1, children: c1, diags: d1, node: string_node } = platform_header_string(ctx, s, nodes, children, diags, False)
		if file {
			string_end = nend(n1, string_node)
			finish1(string_state, n1, c1, d1, TargetFile, t.start, string_end, string_node)
		} else {
			{ pos: string_state, nodes: n1, children: c1, diags: d1, node: string_node }
		}
	} else if !file and t.kind == OpenSquare {
		platform_target_list(ctx, s, nodes, children, diags, False, depth + 1)
	} else if (t.kind == LowerIdent and byte(ctx.source, t.start) != '_') or (file and is_text(ctx, s, App)) or (!file and t.kind == UpperIdent and byte(ctx.source, t.start) != '@') {
		{ nodes: n1, children: c1, node: name_node } = emit(nodes, children, Identifier, t.start, t.end, [])
		if (file) finish1(s + 1, n1, c1, diags, TargetFile, t.start, t.end, name_node) else { pos: s + 1, nodes: n1, children: c1, diags, node: name_node }
	} else if !file and t.kind == Number {
		raw = ctx.source.sublist({ start: t.start, len: t.end - t.start })
		is_int = raw.any(|b| b == 'x' or b == 'o' or b == 'b') or !raw.any(|b| b == '.' or b == 'e' or b == 'E')
		finish(s + 1, nodes, children, if (is_int) diags else diag(diags, UnexpectedToken, t.start, t.end), NumberLiteral, t.start, t.end, [])
	} else {
		finish(s + if (t.kind == EndOfFile) 0 else 1, nodes, children, diag(diags, UnexpectedToken, t.start, t.end), ErrorNode, t.start, t.end, [])
	}
}
