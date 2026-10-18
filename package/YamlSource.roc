import YamlScalar

## Lossless byte-oriented YAML lexical analysis. Offsets are half-open byte spans.
YamlSource :: [].{
	Span : { start : U64, end : U64 }
	TokenKind : [Whitespace, Comment, PlainScalar, SingleQuotedScalar, DoubleQuotedScalar, MappingValue, SequenceEntry, ExplicitKey, FlowSequenceStart, FlowSequenceEnd, FlowMappingStart, FlowMappingEnd, FlowEntry, BlockScalarHeader, BlockScalarContent, Anchor, Alias, Tag, Directive, DocumentStart, DocumentEnd, Invalid]
	ScalarCategory : [Text, Boolean, Null, Number]
	Token : { kind : TokenKind, start : U64, end : U64, role : [Key, Value, Other], scalar : ScalarCategory }
	DiagnosticCode : [UnclosedQuote, InvalidEscape, UnclosedCollection, UnexpectedDelimiter, InvalidIndentation, InvalidBlockHeader, InvalidCharacter, InvalidUtf8, ResourceLimit]
	Diagnostic : { code : DiagnosticCode, start : U64, end : U64 }
	Status : [Complete, Recovered, Limited]
	Tokenized : { source : List(U8), tokens : List(Token), diagnostics : List(Diagnostic), status : Status }
	HighlightCategory : [Trivia, CommentText, PropertyName, StringText, NumericLiteral, BooleanLiteral, NullLiteral, Symbol, Name, TypeName, InvalidText]

	tokenize_str : Str -> Tokenized
	tokenize_str = |source| tokenize_bytes(source.to_utf8())

	tokenize_bytes : List(U8) -> Tokenized
	tokenize_bytes = |source| {
		var $tokens = []
		var $diagnostics = []
		var $at = 0
		var $line_start = 0
		var $indent = 0
		var $candidate = source.len()
		var $explicit_key = False
		var $stack = []
		var $pending_block = False
		var $block_indent = 0
		var $block_explicit = 0
		var $block_root = False
		var $mapping_indent = 0
		var $sequence_indent = 0
		var $limited = False
		while $at < source.len() {
			start = $at
			byte = at(source, start)
			var $kind = PlainScalar
			var $role = Other
			var $scalar = Text
			if $pending_block and start == $line_start {
				end = block_end(source, start, $block_indent, $block_explicit, $block_root)
				$pending_block = False
				if end > start {
					$at = end
					$kind = BlockScalarContent
					$role = Value
					$line_start = end
				}
			}
			if $at == start {
				if byte == 239 and at(source, start + 1) == 187 and at(source, start + 2) == 191 and start == $line_start {
					$kind = Whitespace
					$at = start + 3
					$line_start = $at
				} else if is_space(byte) or is_break(byte) {
					$kind = Whitespace
					$at = start + 1
					if is_break(byte) {
						if byte == '\r' and at(source, $at) == '\n' {
							$at = $at + 1
						}
						$line_start = $at
						$indent = 0
						$mapping_indent = 0
						$sequence_indent = 0
						if $stack.is_empty() {
							$candidate = source.len()
						}
					} else {
						if start == $line_start or start == $line_start + $indent {
							if byte == '\t' and $stack.is_empty() {
								$diagnostics = $diagnostics.append({ code: InvalidIndentation, start, end: start + 1 })
							}
							$indent = $indent + 1
						}
						while $at < source.len() and is_space(at(source, $at)) {
							if $at == $line_start + $indent {
								if at(source, $at) == '\t' and $stack.is_empty() {
									$diagnostics = $diagnostics.append({ code: InvalidIndentation, start: $at, end: $at + 1 })
								}
								$indent = $indent + 1
							}
							$at = $at + 1
						}
					}
				} else if byte == '#' {
					$kind = Comment
					$at = line_end(source, start)
				} else if byte == '\'' or byte == '"' {
					$kind = if byte == '\'' SingleQuotedScalar else DoubleQuotedScalar
					$role = if $explicit_key Key else Value
					$explicit_key = False
					$at = start + 1
					var $closed = False
					while $at < source.len() and !$closed {
						current = at(source, $at)
						if current == byte {
							if byte == '\'' and at(source, $at + 1) == '\'' {
								$at = $at + 2
							} else {
								$at = $at + 1
								$closed = True
							}
						} else if byte == '"' and current == '\\' {
							escaped = at(source, $at + 1)
							width = if escaped == 'x' 2 else if escaped == 'u' 4 else if escaped == 'U' 8 else 0
							var $valid = False
							if width > 0 {
								$valid = $at + 2 + width <= source.len()
								var $digit = 0
								var $codepoint = 0
								while $digit < width and $at + 2 + $digit < source.len() {
									if !is_hex(at(source, $at + 2 + $digit)) {
										$valid = False
									}
									$codepoint = $codepoint * 16 + hex_value(at(source, $at + 2 + $digit))
									$digit = $digit + 1
								}
								if $codepoint > 1114111 or ($codepoint >= 55296 and $codepoint <= 57343) {
									$valid = False
								}
							} else {
								$valid = List.contains(['0', 'a', 'b', 't', 'n', 'v', 'f', 'r', 'e', ' ', '"', '/', '\\', 'N', '_', 'L', 'P', '\t', '\r', '\n'], escaped) and $at + 1 < source.len()
							}
							if !$valid {
								$diagnostics = $diagnostics.append({ code: InvalidEscape, start: $at, end: U64.min($at + 2 + width, source.len()) })
							}
							$at = U64.min($at + 2 + (if $valid width else 0), source.len())
						} else {
							$at = $at + 1
						}
					}
					if !$closed {
						$diagnostics = $diagnostics.append({ code: UnclosedQuote, start, end: $at })
					}
					$candidate = $tokens.len()
				} else if (byte == '-' or byte == '.') and start == $line_start and at(source, start + 1) == byte and at(source, start + 2) == byte and is_separation(source, start + 3) {
					$kind = if byte == '-' DocumentStart else DocumentEnd
					$at = start + 3
					$candidate = source.len()
				} else if byte == '%' and start == $line_start {
					$kind = Directive
					$at = directive_end(source, start)
				} else if byte == '[' or byte == '{' {
					$kind = if byte == '[' FlowSequenceStart else FlowMappingStart
					$at = start + 1
					$candidate = source.len()
					if $stack.len() < 256 {
						$stack = $stack.append({ close: if byte == '[' ']' else '}', start })
					} else {
						$limited = True
						$diagnostics = $diagnostics.append({ code: ResourceLimit, start, end: source.len() })
						$kind = Invalid
						$at = source.len()
					}
				} else if byte == ']' or byte == '}' {
					$kind = if byte == ']' FlowSequenceEnd else FlowMappingEnd
					$at = start + 1
					match $stack.last() {
						Ok(top) if top.close == byte => {
							$stack = $stack.drop_last(1)
						}
						_ => {
							$diagnostics = $diagnostics.append({ code: UnexpectedDelimiter, start, end: $at })
						}
					}
					$candidate = source.len()
				} else if byte == ',' and !$stack.is_empty() {
					$kind = FlowEntry
					$at = start + 1
					$candidate = source.len()
				} else if byte == ':' and (is_separation(source, start + 1) or (!$stack.is_empty() and (is_flow(at(source, start + 1)) or candidate_quoted($tokens, $candidate)))) {
					$kind = MappingValue
					$at = start + 1
					match $tokens.get($candidate) {
						Ok(token) => {
							if token.start >= $line_start {
								$mapping_indent = token.start - $line_start
							}
							$tokens = $tokens.set($candidate, { ..token, role: Key }) ?? []
						}
						Err(_) => {}
					}
					$candidate = source.len()
				} else if (byte == '-' or byte == '?') and is_separation(source, start + 1) {
					$kind = if byte == '-' SequenceEntry else ExplicitKey
					if byte == '-' and $stack.is_empty() {
						$sequence_indent = start - $line_start
					}
					$explicit_key = byte == '?'
					$at = start + 1
					$candidate = source.len()
				} else if byte == '&' or byte == '*' or byte == '!' {
					$kind = if byte == '&' Anchor else if byte == '*' Alias else Tag
					$at = start + 1
					if byte == '!' and at(source, $at) == '<' {
						$at = $at + 1
						while $at < source.len() and at(source, $at) != '>' and !is_break(at(source, $at)) {
							if is_space(at(source, $at)) {
								$diagnostics = $diagnostics.append({ code: InvalidCharacter, start: $at, end: $at + 1 })
							}
							$at = $at + 1
						}
						if at(source, $at) == '>' and $at < source.len() {
							$at = $at + 1
						} else {
							$diagnostics = $diagnostics.append({ code: UnclosedCollection, start, end: $at })
						}
					} else {
						while $at < source.len() and !is_separation(source, $at) and !is_flow(at(source, $at)) {
							$at = $at + 1
						}
					}
				} else if (byte == '|' or byte == '>') and $stack.is_empty() {
					$kind = BlockScalarHeader
					$role = Value
					$at = start + 1
					var $chomp = False
					var $explicit = 0
					var $bad = False
					while $at < source.len() and !is_separation(source, $at) {
						part = at(source, $at)
						if part == '+' or part == '-' {
							if $chomp {
								$bad = True
							}
							$chomp = True
						} else if part >= '1' and part <= '9' {
							if $explicit != 0 {
								$bad = True
							}
							$explicit = part.to_u64() - 48
						} else {
							$bad = True
						}
						$at = $at + 1
					}
					if $bad {
						$diagnostics = $diagnostics.append({ code: InvalidBlockHeader, start, end: $at })
					}
					$pending_block = True
					$block_indent = U64.max(U64.max($indent, $mapping_indent), $sequence_indent)
					$block_explicit = $explicit
					$block_root = start == $line_start or inline_document_root(source, $line_start, start)
				} else if (byte < 32 and byte != '\t') or byte == 127 or byte == '@' or byte == '`' {
					$kind = Invalid
					$at = start + 1
					if byte == '@' or byte == '`' {
						$diagnostics = $diagnostics.append({ code: InvalidCharacter, start, end: $at })
					}
				} else {
					$kind = PlainScalar
					$role = if $explicit_key Key else Value
					$explicit_key = False
					$at = plain_end(source, start, $indent, !$stack.is_empty(), start == $line_start)
					if $at == start {
						$at = start + 1
					}
					$scalar = match Str.from_utf8(source.sublist({ start, len: $at - start })) {
						Ok(text) => YamlScalar.classify(text)
						Err(_) => Text
					}
					$candidate = $tokens.len()
				}
			}
			$tokens = $tokens.append({ kind: $kind, start, end: $at, role: $role, scalar: $scalar })
			# Quoted and plain scalars can consume physical line breaks.
			var $scan = start
			while $scan < $at {
				if is_break(at(source, $scan)) {
					$line_start = $scan + 1
					$mapping_indent = 0
					$sequence_indent = 0
					$indent = 0
					if at(source, $scan) == '\r' and at(source, $scan + 1) == '\n' {
						$line_start = $scan + 2
					}
				}
				if $line_start > start and $scan == $line_start + $indent and at(source, $scan) == ' ' {
					$indent = $indent + 1
				}
				$scan = $scan + 1
			}
		}
		for frame in $stack {
			$diagnostics = $diagnostics.append({ code: UnclosedCollection, start: frame.start, end: source.len() })
		}
		$diagnostics = utf8_diagnostics(source, $diagnostics)
		{ source, tokens: $tokens, diagnostics: $diagnostics, status: if $limited Limited else if $diagnostics.is_empty() Complete else Recovered }
	}

	highlight_category : Token -> HighlightCategory
	highlight_category = |token| {
		if token.role == Key {
			PropertyName
		} else {
			match token.kind {
				Whitespace => Trivia
				Comment => CommentText
				PlainScalar => match token.scalar {
					Boolean => BooleanLiteral
					Null => NullLiteral
					Number => NumericLiteral
					Text => StringText
				}
				SingleQuotedScalar | DoubleQuotedScalar | BlockScalarContent => StringText
				Anchor | Alias => Name
				Tag => TypeName
				Invalid => InvalidText
				_ => Symbol
			}
		}
	}
}

at : List(U8), U64 -> U8
at = |bytes, index| bytes.get(index) ?? 0

is_space : U8 -> Bool
is_space = |byte| byte == ' ' or byte == '\t'

is_break : U8 -> Bool
is_break = |byte| byte == '\r' or byte == '\n'

is_separation : List(U8), U64 -> Bool
is_separation = |bytes, index| index >= bytes.len() or is_space(at(bytes, index)) or is_break(at(bytes, index))

is_flow : U8 -> Bool
is_flow = |byte| byte == '[' or byte == ']' or byte == '{' or byte == '}' or byte == ','

is_hex : U8 -> Bool
is_hex = |byte| (byte >= '0' and byte <= '9') or (byte >= 'a' and byte <= 'f') or (byte >= 'A' and byte <= 'F')

line_end : List(U8), U64 -> U64
line_end = |bytes, start| {
	var $at = start
	while $at < bytes.len() and !is_break(at(bytes, $at)) {
		$at = $at + 1
	}
	$at
}

candidate_quoted : List(YamlSource.Token), U64 -> Bool
candidate_quoted = |tokens, index| {
	match tokens.get(index) {
		Ok(token) => token.kind == SingleQuotedScalar or token.kind == DoubleQuotedScalar
		Err(_) => False
	}
}

plain_end : List(U8), U64, U64, Bool, Bool -> U64
plain_end = |bytes, start, indent, flow, root| {
	var $at = start
	var $end = start
	var $done = False
	while $at < bytes.len() and !$done {
		byte = at(bytes, $at)
		if (flow and is_flow(byte)) or (byte == ':' and (is_separation(bytes, $at + 1) or (flow and is_flow(at(bytes, $at + 1))))) or (byte == '#' and ($at == start or is_space(at(bytes, $at - 1)))) or (byte < 32 and !is_space(byte) and !is_break(byte)) or byte == 127 {
			$done = True
		} else if is_break(byte) {
			var $next = $at + 1
			if byte == '\r' and at(bytes, $next) == '\n' {
				$next = $next + 1
			}
			var $spaces = 0
			while $next < bytes.len() and at(bytes, $next) == ' ' {
				$spaces = $spaces + 1
				$next = $next + 1
			}
			while $next < bytes.len() and is_break(at(bytes, $next)) {
				previous = at(bytes, $next)
				$next = $next + 1
				if previous == '\r' and at(bytes, $next) == '\n' {
					$next = $next + 1
				}
				$spaces = 0
				while $next < bytes.len() and at(bytes, $next) == ' ' {
					$spaces = $spaces + 1
					$next = $next + 1
				}
			}
			if $next >= bytes.len() or (!flow and ((if root $spaces < indent else $spaces <= indent) or line_has_mapping(bytes, $next) or ($spaces == 0 and document_marker(bytes, $next)))) or at(bytes, $next) == '#' or is_flow(at(bytes, $next)) or ((at(bytes, $next) == '-' or at(bytes, $next) == '?') and is_separation(bytes, $next + 1)) {
				$done = True
			} else {
				$at = $next
			}
		} else {
			$at = $at + 1
			if !is_space(byte) {
				$end = $at
			}
		}
	}
	$end
}

block_end : List(U8), U64, U64, U64, Bool -> U64
block_end = |bytes, start, parent_indent, explicit, root| {
	var $at = start
	var $required = if explicit > 0 parent_indent + explicit - (if root 1 else 0) else 0
	var $detected = explicit > 0
	var $done = False
	while $at < bytes.len() and !$done {
		var $text = $at
		while $text < bytes.len() and at(bytes, $text) == ' ' {
			$text = $text + 1
		}
		end = line_end(bytes, $text)
		blank = $text == end
		indentation = $text - $at
		if !blank and ((indentation == 0 and at(bytes, $text) != '%' and document_marker(bytes, $text)) or ($detected and indentation < $required) or (!$detected and !root and indentation <= parent_indent)) {
			$done = True
		} else {
			if !blank and !$detected {
				$detected = True
				$required = indentation
			}
			$at = end
			if $at < bytes.len() {
				$at = $at + 1
				if at(bytes, end) == '\r' and at(bytes, $at) == '\n' {
					$at = $at + 1
				}
			}
		}
	}
	$at
}

utf8_diagnostics : List(U8), List(YamlSource.Diagnostic) -> List(YamlSource.Diagnostic)
utf8_diagnostics = |bytes, diagnostics| {
	var $out = diagnostics
	var $at = 0
	while $at < bytes.len() {
		byte = at(bytes, $at)
		if byte < 128 {
			if (byte < 32 and byte != 9 and byte != 10 and byte != 13) or byte == 127 {
				$out = $out.append({ code: InvalidCharacter, start: $at, end: $at + 1 })
			}
			$at = $at + 1
		} else {
			width = if byte >= 194 and byte <= 223 2 else if byte >= 224 and byte <= 239 3 else if byte >= 240 and byte <= 244 4 else 1
			var $valid = width > 1 and $at + width <= bytes.len()
			var $part = 1
			while $part < width and $at + $part < bytes.len() {
				value = at(bytes, $at + $part)
				if value < 128 or value > 191 {
					$valid = False
				}
				$part = $part + 1
			}
			second = at(bytes, $at + 1)
			if (byte == 224 and second < 160) or (byte == 237 and second > 159) or (byte == 240 and second < 144) or (byte == 244 and second > 143) {
				$valid = False
			}
			if $valid {
				if (byte == 194 and second >= 128 and second <= 159 and second != 133) or (byte == 239 and second == 191 and (at(bytes, $at + 2) == 190 or at(bytes, $at + 2) == 191)) {
					$out = $out.append({ code: InvalidCharacter, start: $at, end: $at + width })
				}
				$at = $at + width
			} else {
				$out = $out.append({ code: InvalidUtf8, start: $at, end: $at + 1 })
				$at = $at + 1
			}
		}
	}
	$out
}

# A continuation line cannot introduce a new block mapping entry.
line_has_mapping : List(U8), U64 -> Bool
line_has_mapping = |bytes, start| {
	var $at = start
	var $found = False
	while $at < bytes.len() and !is_break(at(bytes, $at)) and !$found {
		if at(bytes, $at) == ':' and is_separation(bytes, $at + 1) {
			$found = True
		}
		$at = $at + 1
	}
	$found
}

expect {
	result = YamlSource.tokenize_str("title: hello\n  world\nnext: 42\n")
	result.tokens.map(|token| token.kind) == [PlainScalar, MappingValue, Whitespace, PlainScalar, Whitespace, PlainScalar, MappingValue, Whitespace, PlainScalar, Whitespace]
}
expect {
	result = YamlSource.tokenize_str("{\"key\":true, url: https://host/a#frag}")
	result.tokens.get(1).map_ok(|token| token.role) == Ok(Key)
}
expect {
	result = YamlSource.tokenize_str("body: |2-\r\n  one\r\n  two\r\nnext: x\r\n")
	result.tokens.keep_if(|token| token.kind == BlockScalarContent).map(|token| result.source.sublist({ start: token.start, len: token.end - token.start })) == ["  one\r\n  two\r\n".to_utf8()]
}
expect {
	result = YamlSource.tokenize_bytes([255, ':', ' ', 0, 10])
	result.status == Recovered and result.tokens.last().map_ok(|token| token.end) == Ok(5)
}

expect {
	result = YamlSource.tokenize_str("- run: |2\n    command\n- next\n")
	result.tokens.keep_if(|token| token.kind == BlockScalarContent).map(|token| result.source.sublist({ start: token.start, len: token.end - token.start })) == ["    command\n".to_utf8()]
}
expect {
	result = YamlSource.tokenize_str("values: [\n\ttrue, false\n]\n")
	result.status == Complete
}
expect {
	result = YamlSource.tokenize_bytes(['a', ':', ' ', '\"', 1, '\"'])
	result.diagnostics.map(|problem| problem.code) == [InvalidCharacter]
}

hex_value : U8 -> U64
hex_value = |byte| {
	if byte >= '0' and byte <= '9' {
		byte.to_u64() - 48
	} else if byte >= 'a' and byte <= 'f' {
		byte.to_u64() - 87
	} else if byte >= 'A' and byte <= 'F' {
		byte.to_u64() - 55
	} else {
		0
	}
}
expect {
	result = YamlSource.tokenize_str("a: \"\\uD800\"\nb: \"\\U00110000\"\n")
	result.diagnostics.map(|problem| problem.code) == [InvalidEscape, InvalidEscape]
}

document_marker : List(U8), U64 -> Bool
document_marker = |bytes, index| {
	byte = at(bytes, index)
	((byte == '-' or byte == '.') and at(bytes, index + 1) == byte and at(bytes, index + 2) == byte and is_separation(bytes, index + 3)) or byte == '%'
}
expect {
	result = YamlSource.tokenize_str("first\n\nsecond\n---\nnext\n")
	result.tokens.map(|token| token.kind) == [PlainScalar, Whitespace, DocumentStart, Whitespace, PlainScalar, Whitespace]
}

directive_end : List(U8), U64 -> U64
directive_end = |bytes, start| {
	var $at = start
	var $end = start
	var $done = False
	while $at < bytes.len() and !is_break(at(bytes, $at)) and !$done {
		byte = at(bytes, $at)
		if byte == '#' and $at > start and is_space(at(bytes, $at - 1)) {
			$done = True
		} else {
			$at = $at + 1
			if !is_space(byte) {
				$end = $at
			}
		}
	}
	$end
}

inline_document_root : List(U8), U64, U64 -> Bool
inline_document_root = |bytes, line_start, start| {
	if at(bytes, line_start) == '-' and at(bytes, line_start + 1) == '-' and at(bytes, line_start + 2) == '-' and is_separation(bytes, line_start + 3) {
		var $at = line_start + 3
		while $at < start and is_space(at(bytes, $at)) {
			$at = $at + 1
		}
		$at == start
	} else {
		False
	}
}
expect {
	result = YamlSource.tokenize_str("--- |\n%!PS-Adobe-2.0\n---\nnext\n")
	result.tokens.keep_if(|token| token.kind == BlockScalarContent).map(|token| result.source.sublist({ start: token.start, len: token.end - token.start })) == ["%!PS-Adobe-2.0\n".to_utf8()]
}

expect {
	result = YamlSource.tokenize_str("- - |2\n    text\n  - next\n")
	result.tokens.keep_if(|token| token.kind == BlockScalarContent).map(|token| result.source.sublist({ start: token.start, len: token.end - token.start })) == ["    text\n".to_utf8()]
}
