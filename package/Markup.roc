import Xml
import XmlNs
import Literal

## Shared source, namespace trees and diagnostics for HTML, SVG and MathML.
## Trees use XML expanded names. Source retains the exact original bytes;
## decoded trees are deliberately separate from this lossless representation.
Markup :: [].{

	## Parsing syntax. HTML uses browser naming and void-element conventions.
	Syntax : [XmlSyntax, HtmlSyntax]

	## A half-open UTF-8 byte interval in the original input.
	Span : { start : U64, end : U64 }

	## Lexical source regions, including incomplete markup.
	Token : { kind : [Text, Tag, Comment, Declaration, ProcessingInstruction, Invalid], span : Span }

	## Lossless bytes and a partition into lexical regions.
	Source := { bytes : List(U8), tokens : List(Token) }.{

		## Retain arbitrary bytes, including malformed UTF-8 and incomplete tags.
		from_bytes : List(U8) -> Source
		from_bytes = |bytes| {
			var $tokens = []
			var $pos = 0
			while $pos < bytes.len() {
				start = $pos
				var $kind = Text
				if bytes.get($pos) == Ok('<') {
					$kind = Tag
					var $quote = 0.U8
					$pos = $pos + 1
					if bytes.get($pos) == Ok('!') {
						$kind = Declaration
					}
					if bytes.get($pos) == Ok('?') {
						$kind = ProcessingInstruction
					}
					comment = bytes.get($pos) == Ok('!') and bytes.get($pos + 1) == Ok('-') and bytes.get($pos + 2) == Ok('-')
					if comment {
						$kind = Comment
					}
					var $done = False
					while $pos < bytes.len() and !$done {
						byte = bytes.get($pos) ?? 0
						if comment {
							$done = byte == '>' and $pos >= start + 6 and bytes.get($pos - 1) == Ok('-') and bytes.get($pos - 2) == Ok('-')
						} else if $quote != 0 {
							if byte == $quote {
								$quote = 0
							}
						} else if byte == '"' or byte == '\'' {
							$quote = byte
						} else {
							$done = byte == '>'
						}
						$pos = $pos + 1
					}
					if !$done {
						$kind = Invalid
					}
				} else {
					while $pos < bytes.len() and bytes.get($pos) != Ok('<') {
						$pos = $pos + 1
					}
				}
				$tokens = $tokens.append({ kind: $kind, span: { start, end: $pos } })
			}
			Source.{ bytes, tokens: $tokens }
		}

		## Retain UTF-8 source without normalization.
		from_str : Str -> Source
		from_str = |text| from_bytes(text.to_utf8())

		## The exact bytes of a source token.
		slice : Source, Span -> List(U8)
		slice = |source, span| source.bytes.drop_first(span.start).take_first(span.end - span.start)
	}

	## Structured reason for rejection; presentation is separate.
	Problem : [SyntaxError(Str), NamespaceError(XmlNs.Problem), WrongRoot({ expected : Str, actual : Str }), UnknownElement(Str), UnknownAttribute({ element : Str, attribute : Str }), InvalidValue({ attribute : Str, value : Str, expected : Str }), InvalidChildren({ element : Str, expected : Str }), Unsupported(Str), LimitExceeded(Xml.Limit), SourceTooLarge(U64)]

	## One-based lines and UTF-8 byte columns refer to authored source.
	Error : { problem : Problem, line : U64, column : U64 }

	## Configurable validation and resource limits, strict by default.
	Props := { syntax : Syntax ?? XmlSyntax, allow_unknown_elements : Bool ?? False, allow_unknown_attributes : Bool ?? False, check_values : Bool ?? True, max_source_bytes : U64 ?? 16_777_216, max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_attributes : U64 ?? 1024 }

	## Original source and decoded namespace-aware structure.
	Parsed : { source : Source, document : XmlNs }

	## SVG namespace URI.
	svg_namespace = "http://www.w3.org/2000/svg"

	## MathML namespace URI.
	mathml_namespace = "http://www.w3.org/1998/Math/MathML"

	## HTML namespace URI.
	html_namespace = "http://www.w3.org/1999/xhtml"

	## Parse XML without changing the existing XmlNs API.
	parse_xml : Str, Props -> Try(Parsed, [InvalidMarkup(Error)])
	parse_xml = |text, props| {
		if text.count_utf8_bytes() > props.max_source_bytes {
			return Err(InvalidMarkup({ problem: SourceTooLarge(props.max_source_bytes), line: 1, column: 1 }))
		}
		match XmlNs.parse(text, { max_depth: props.max_depth, max_elements: props.max_elements, max_attributes: props.max_attributes }) {
			Ok(document) => Ok({ source: Source.from_str(text), document })
			Err(InvalidXml(e)) => Err(InvalidMarkup({ problem: SyntaxError(e.message), line: e.line, column: e.column }))
			Err(InvalidNamespace(e)) => Err(InvalidMarkup({ problem: NamespaceError(e.problem), line: e.line, column: e.column }))
			Err(LimitExceeded(e)) => Err(InvalidMarkup({ problem: LimitExceeded(e.limit), line: e.line, column: e.column }))
		}
	}

	## Render a structured failure for a compiler quotation diagnostic.
	quoted_error : Str, Error -> [BadQuotedBytes(Str)]
	quoted_error = |format, error| Literal.error({ format, line: error.line, column: error.column, message: Str.inspect(error.problem) })
}

expect {
	source = Markup.Source.from_str("<a x='>'><!-- > -->&amp;</a>\r\n<broken")
	Str.from_utf8(source.tokens.fold([], |out, token| out.concat(source.slice(token.span)))) == Ok("<a x='>'><!-- > -->&amp;</a>\r\n<broken")
}
expect Markup.Source.from_bytes([255, '<']).bytes == [255, '<']
