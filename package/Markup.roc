import Xml
import XmlNs
import Literal
import MarkupHtmlError

## Shared source, namespace trees and diagnostics for HTML, SVG and MathML.
## Trees use XML expanded names. Source retains the exact original bytes;
## decoded trees are deliberately separate from this lossless representation.
Markup :: [].{

	## Parsing syntax. HTML uses browser naming and void-element conventions.
	Syntax : [XmlSyntax, HtmlSyntax]

	## A half-open UTF-8 byte interval in the original input.
	Span : { start : U64, end : U64 }

	## Lexical source regions, including incomplete markup.
	Token : { kind : [Text, Tag, Comment, CData, Declaration, ProcessingInstruction, Invalid], span : Span }

	## Lossless bytes and a partition into lexical regions.
	Source := { bytes : List(U8), tokens : List(Token) }.{

		## Compare the retained source and lexical ranges.
		is_eq : _

		## Hash the retained source and lexical ranges.
		to_hash : _

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
					comment = bytes_at(bytes, start, "<!--")
					cdata = bytes_at(bytes, start, "<![CDATA[")
					pi = bytes_at(bytes, start, "<?")
					if comment {
						$kind = Comment
					}
					if cdata {
						$kind = CData
					}
					var $done = False
					while $pos < bytes.len() and !$done {
						byte = bytes.get($pos) ?? 0
						if comment {
							$done = byte == '>' and $pos >= start + 6 and bytes.get($pos - 1) == Ok('-') and bytes.get($pos - 2) == Ok('-')
						} else if cdata {
							$done = byte == '>' and $pos >= start + 11 and bytes.get($pos - 1) == Ok(']') and bytes.get($pos - 2) == Ok(']')
						} else if pi {
							$done = byte == '>' and bytes.get($pos - 1) == Ok('?')
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

		## Retain token regions produced by a syntax-specific parser. Regions must
		## partition the supplied bytes; partition_valid checks this invariant.
		from_tokens : List(U8), List(Token) -> Source
		from_tokens = |bytes, tokens| Source.{ bytes, tokens }

		## Whether token ranges cover every byte once, in order, without empty
		## regions. An empty input has an empty token list.
		partition_valid : Source -> Bool
		partition_valid = |source| {
			var $end = 0
			for token in source.tokens {
				if token.span.start != $end or token.span.end <= token.span.start or token.span.end > source.bytes.len() {
					return False
				}
				$end = token.span.end
			}
			$end == source.bytes.len()
		}

		## Retain UTF-8 source without normalization.
		from_str : Str -> Source
		from_str = |source_text| from_bytes(source_text.to_utf8())

		## The exact bytes of a source token.
		slice : Source, Span -> List(U8)
		slice = |source, span| {
			start = if (span.start < source.bytes.len()) span.start else source.bytes.len()
			end = if span.end > start {
				if (span.end < source.bytes.len()) span.end else source.bytes.len()
			} else start
			source.bytes.drop_first(start).take_first(end - start)
		}
	}

	## Typed attribute grammars shared by validators and interpolation slots.
	ValueGrammar : [Number, NonnegativeNumber, Length, NonnegativeLength, NumberList(U64), LengthList, PathData, TransformList, Keyword(List(Str)), TokenList, Color, Paint, AspectRatio, Language(Str)]

	## A structural repair forbidden by exact authored HTML parsing.
	StructureReason : [ImpliedElement(Str), MissingEndTag(Str), UnexpectedEndTag(Str), NonVoidSelfClosing(Str), ReparentedContent, IgnoredMarkup, ReconstructedContent, ChangedAttributes]

	## Structured reason for rejection. Presentation is separate.
	Problem : [NonCanonicalName({ authored : Str, expected : Str, span : Span }), HtmlStructure({ reason : StructureReason, span : Span }), SyntaxError(Str), HtmlSyntax(MarkupHtmlError.Code), NamespaceError(XmlNs.Problem), WrongRoot({ expected : Str, actual : Str }), UnknownElement(Str), UnknownAttribute({ element : Str, attribute : Str }), InvalidValue({ attribute : Str, value : Str, expected : ValueGrammar }), InvalidChildren({ element : Str, expected : Str }), Unsupported(Str), LimitExceeded(Xml.Limit), SourceTooLarge(U64), ResourceLimit(Str), Interpolation({ index : U64, reason : Str }), BindingCount({ expected : U64, actual : U64 })]

	## One-based lines and UTF-8 byte columns refer to authored source.
	Error : { problem : Problem, line : U64, column : U64 }

	## Configurable validation and resource limits, strict by default.
	## exact_structure rejects repaired HTML nesting before derived projection.
	## strict_foreign_spelling requires authored SVG/MathML names to match
	## their canonical DOM spelling in HTML syntax, including closing tags.
	## XML work counts deterministic source-byte traversals and reader events;
	## it is a progress ceiling rather than a count of machine instructions.
	Props := { syntax : Syntax ?? XmlSyntax, exact_structure : Bool ?? False, strict_foreign_spelling : Bool ?? False, allow_unknown_elements : Bool ?? False, allow_unknown_attributes : Bool ?? False, check_values : Bool ?? True, max_source_bytes : U64 ?? 16_777_216, max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_nodes : U64 ?? 4_194_304, max_slots : U64 ?? 65_536, max_attributes : U64 ?? 1024, recovery : [Strict, Browser] ?? Strict, max_diagnostics : U64 ?? 1024, max_work_per_byte : U64 ?? 256 }

	## Saturating progress budget proportional to source bytes (one for empty input).
	work_budget : Props, U64 -> U64
	work_budget = |props, bytes| {
		units = if (bytes == 0) 1 else bytes
		if props.max_work_per_byte > 18_446_744_073_709_551_615 // units {
			18_446_744_073_709_551_615
		} else {
			props.max_work_per_byte * units
		}
	}

	## Expanded names share the existing namespace-aware XML contract.
	Name : XmlNs.Name

	## Attribute spelling/value and its authored byte range. Namespace declarations
	## are represented in the XMLNS namespace, preserving declaration order.
	Attribute : { name : Name, value : Str, span : Span }

	## Authored bytes, an implied insertion point, or a projected copy whose
	## source still identifies the original authored span.
	Origin : [Authored(Span), Implied(U64), Cloned({ source : Span, at : U64 })]

	## A flat tree node. Child indices refer to the containing tree's node list.
	Node : {
		kind : [Element(Name), Text(Str), Comment(Str), Doctype({ name : Str, public_id : Str, system_id : Str }), ProcessingInstruction({ target : Str, data : Str })],
		attributes : List(Attribute),
		children : List(U64),
		origin : Origin,
	}

	## Indexed nodes with document/fragment roots in authored order.
	Tree : { nodes : List(Node), roots : List(U64) }

	## Exact input, namespace-aware structure, and accepted recovery diagnostics.
	Parsed : { source : Source, tree : Tree, diagnostics : List(Error) }

	## SVG namespace URI.
	svg_namespace = "http://www.w3.org/2000/svg"

	## MathML namespace URI.
	mathml_namespace = "http://www.w3.org/1998/Math/MathML"

	## HTML namespace URI.
	html_namespace = "http://www.w3.org/1999/xhtml"

	## Parse XML without changing the existing XmlNs API.
	parse_xml : Str, Props -> Try(Parsed, [InvalidMarkup(Error)])
	parse_xml = |source_text, props| parse_xml_source(source_text, "", False, props)

	## Parse multiple XML fragment roots in an implicit default namespace.
	## Explicit xmlns declarations override this context normally. Locations
	## refer to authored bytes, excluding the internal fragment context.
	parse_xml_fragment : Str, Str, Props -> Try(Parsed, [InvalidMarkup(Error)])
	parse_xml_fragment = |source_text, namespace, props| parse_xml_source(source_text, namespace, True, props)

	## Decode one XML character-data region, including references and CDATA.
	decode_xml_text : Str -> Try(Str, [InvalidMarkup(Error)])
	decode_xml_text = |source_text| {
		match Xml.parse_str("<_>${source_text}</_>") {
			Ok(document) => Ok(document.root.text())
			Err(InvalidXml(error)) => Err(InvalidMarkup({ problem: SyntaxError(error.message), line: error.line, column: error.column }))
		}
	}

	## Locate a byte offset using the common one-based UTF-8 position convention.
	position : Source, U64 -> { line : U64, column : U64 }
	position = |source, offset| {
		end = if (offset < source.bytes.len()) offset else source.bytes.len()
		var $index = 0
		var $line = 1
		var $column = 1
		while $index < end {
			byte = source.bytes.get($index) ?? 0
			if byte == '\r' {
				$line = $line + 1
				$column = 1
				if $index + 1 < end and source.bytes.get($index + 1) == Ok('\n') {
					$index = $index + 1
				}
			} else if byte == '\n' {
				$line = $line + 1
				$column = 1
			} else {
				$column = $column + 1
			}
			$index = $index + 1
		}
		{ line: $line, column: $column }
	}

	## Markup whitespace is ASCII, including HTML's form feed, not Unicode spaces.
	whitespace : Str -> Bool
	whitespace = |source_text| {
		bytes = source_text.to_utf8()
		var $index = 0
		while $index < bytes.len() {
			byte = bytes.get($index) ?? 0
			if !space(byte) and byte != 12 {
				return False
			}
			$index = $index + 1
		}
		True
	}

	## The only element root, ignoring comments and processing instructions.
	single_root : Tree -> Try(U64, [RootCount(U64)])
	single_root = |tree| {
		roots = tree.roots.keep_if(
			|index| match tree.nodes.get(index) {
				Ok({ kind: Comment(_), .. }) | Ok({ kind: ProcessingInstruction(_), .. }) | Ok({ kind: Doctype(_), .. }) => False
				Ok({ kind: Text(node_text), .. }) => !whitespace(node_text)
				Ok(_) => True
				Err(_) => False
			},
		)
		if roots.len() == 1 {
			index = roots.first() ?? 0
			match tree.nodes.get(index) {
				Ok({ kind: Element(_), .. }) => Ok(index)
				_ => Err(RootCount(1))
			}
		} else {
			Err(RootCount(roots.len()))
		}
	}

	## All descendant character data in document order, without recursive calls.
	text : Tree, U64 -> Str
	text = |tree, root| {
		var $pending = [root]
		var $out = []
		while !$pending.is_empty() {
			index = $pending.last() ?? 0
			$pending = $pending.drop_last(1)
			match tree.nodes.get(index) {
				Ok({ kind: Text(value), .. }) => {
					$out = $out.concat(value.to_utf8())
				}
				Ok(node) => {
					var $i = node.children.len()
					while $i > 0 {
						$i = $i - 1
						$pending = $pending.append(node.children.get($i) ?? 0)
					}
				}
				Err(_) => {}
			}
		}
		Str.from_utf8_lossy($out)
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

bytes_at : List(U8), U64, Str -> Bool
bytes_at = |bytes, start, text| bytes.drop_first(start).take_first(text.count_utf8_bytes()) == text.to_utf8()

space : U8 -> Bool
space = |byte| byte == ' ' or byte == '\t' or byte == '\r' or byte == '\n'

StartInfo : { name : Markup.Name, bindings : List(XmlNs.Binding), attributes : List(XmlNs.Attribute) }

# Reader validation establishes XML/namespace well-formedness before any
# lexical source attachment. The fragment context is internal and never
# appears in retained source, tree roots, or authored diagnostics.
parse_xml_source : Str, Str, Bool, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
parse_xml_source = |text, namespace, fragment, props| {
	if props.syntax != XmlSyntax {
		return Err(InvalidMarkup({ problem: Unsupported("HTML syntax requires Html parsing"), line: 1, column: 1 }))
	}
	if text.count_utf8_bytes() > props.max_source_bytes {
		return Err(InvalidMarkup({ problem: SourceTooLarge(props.max_source_bytes), line: 1, column: 1 }))
	}
	prefix = if fragment {
		"<_markup_fragment xmlns='${escape_namespace(namespace)}'>"
	} else ""
	input = if fragment {
		"${prefix}${text}</_markup_fragment>"
	} else text
	ceiling = text.count_utf8_bytes() + 1
	depth = (if (props.max_depth < ceiling) props.max_depth else ceiling) + if (fragment) 1 else 0
	elements = (if (props.max_elements < ceiling) props.max_elements else ceiling) + if (fragment) 1 else 0
	# Reader calls and retained lexical work total at most 6n + 3 units.
	# The conservative fast-path threshold leaves custom small budgets on the
	# event-counted reader path, preserving their exact failure positions.
	if props.max_work_per_byte >= 16 {
		maximum_attributes = if props.max_attributes == 0 and fragment {
			1
		} else props.max_attributes
		if depth > 0 and elements > 0 and Xml.SourceLex.closes_document(input, maximum_attributes) {
			match parse_xml_direct(text, input, prefix.count_utf8_bytes(), depth, elements, fragment, props) {
				Ok(parsed) => return Ok(parsed)
				Err(Fallback) => {}
			}
		}
		return parse_xml_fold(text, input, prefix.count_utf8_bytes(), depth, elements, fragment, props)
	}
	opened = XmlNs.reader(input, { max_depth: depth, max_elements: elements, max_attributes: if (props.max_attributes == 0 and fragment) 1 else props.max_attributes }).map_err(|error| located_xml_error(error, text, prefix.count_utf8_bytes()))?
	budget = Markup.work_budget(props, text.count_utf8_bytes())
	var $work = text.count_utf8_bytes()
	if $work > budget {
		return Err(InvalidMarkup({ problem: ResourceLimit("parser work"), line: 1, column: 1 }))
	}
	var $reader = opened
	var $starts = []
	var $done = False
	var $context = fragment
	while !$done {
		if $work >= budget {
			return Err(InvalidMarkup({ problem: ResourceLimit("parser work"), line: 1, column: 1 }))
		}
		$work = $work + 1
		match $reader.next() {
			Event(Start(start), rest) => {
				if $context {
					$context = False
				} else {
					$starts = $starts.append(start)
				}
				$reader = rest
			}
			Event(_, rest) => {
				$reader = rest
			}
			Done => {
				$done = True
			}
			Failed(error) => return Err(located_xml_error(error, text, prefix.count_utf8_bytes()))
		}
	}
	construct_xml_tree(Markup.Source.from_str(text), $starts, fragment, props, budget - $work)
}

escape_namespace : Str -> Str
escape_namespace = |namespace| Str.join_with(Str.join_with(namespace.split_on("&"), "&amp;").split_on("'"), "&apos;")

located_xml_error : XmlNs.ReadError, Str, U64 -> [InvalidMarkup(Markup.Error)]
located_xml_error = |error, text, prefix| {
	(problem, line, original_column) = match error {
		InvalidXml(e) => (SyntaxError(e.message), e.line, e.column)
		InvalidNamespace(e) => (NamespaceError(e.problem), e.line, e.column)
		LimitExceeded(e) => (LimitExceeded(e.limit), e.line, e.column)
	}
	column = if line == 1 {
		if original_column > prefix {
			original_column - prefix
		} else 1
	} else original_column
	last = Literal.position(text, text.count_utf8_bytes())
	at = if line > last.line or (line == last.line and column > last.column) {
		last
	} else { line, column }
	InvalidMarkup({ problem, line: at.line, column: at.column })
}

construct_xml_tree : Markup.Source, List(StartInfo), Bool, Markup.Props, U64 -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
construct_xml_tree = |source, starts, fragment, props, budget| {
	var $work = 0
	var $nodes = []
	var $roots = []
	var $stack = []
	var $start_index = 0
	var $token_index = 0
	while $token_index < source.tokens.len() {
		token = source.tokens.get($token_index) ?? { kind: Invalid, span: { start: 0, end: 0 } }
		$token_index = $token_index + 1
		bytes = source.slice(token.span)
		cost = bytes.len() * 2 + 1
		if cost > budget - $work {
			return Err(error_at(source, token.span.start, ResourceLimit("parser work")))
		}
		$work = $work + cost
		var $new_node = Err(Missing)
		match token.kind {
			Tag => {
				if bytes.get(1) == Ok('/') {
					match $stack.last() {
						Ok(index) => {
							$stack = $stack.drop_last(1)
							node = $nodes.get(index) ?? empty_node
							start = match node.origin {
								Authored(span) => span.start
								Implied(at) => at
								Cloned(copy) => copy.source.start
							}
							$nodes = $nodes.set(index, { ..node, origin: Authored({ start, end: token.span.end }) }) ?? []
						}
						Err(_) => {}
					}
				} else {
					match starts.get($start_index) {
						Ok(start) => {
							$start_index = $start_index + 1
							attrs = source_attributes(source, token.span, start)
							if attrs.len() > props.max_attributes {
								return Err(error_at(source, token.span.start, LimitExceeded(Attributes)))
							}
							$new_node = Ok({ kind: Element(start.name), attributes: attrs, children: [], origin: Authored(token.span) })
						}
						Err(_) => return Err(error_at(source, token.span.start, SyntaxError("missing XML start event")))
					}
				}
			}
			Text | CData => {
				if fragment or !$stack.is_empty() {
					# The reader has already checked this region. Plain character
					# data and CDATA only need line-ending normalization.
					value = if token.kind == CData {
						normalize_lines(Str.from_utf8_lossy(bytes.drop_first(9).drop_last(3)))
					} else if bytes.contains('&') {
						Markup.decode_xml_text(Str.from_utf8_lossy(bytes))?
					} else {
						normalize_lines(Str.from_utf8_lossy(bytes))
					}
					if value != "" {
						$new_node = Ok({ kind: Text(value), attributes: [], children: [], origin: Authored(token.span) })
					}
				}
			}
			Comment => {
				$new_node = Ok({ kind: Comment(normalize_lines(Str.from_utf8_lossy(bytes.drop_first(4).drop_last(3)))), attributes: [], children: [], origin: Authored(token.span) })
			}
			ProcessingInstruction => {
				inside = bytes.drop_first(2).drop_last(2)
				var $split = 0
				while $split < inside.len() and !space(inside.get($split) ?? 0) {
					$split = $split + 1
				}
				target = Str.from_utf8_lossy(inside.take_first($split))
				if target != "xml" {
					var $data = $split
					while $data < inside.len() and space(inside.get($data) ?? 0) {
						$data = $data + 1
					}
					$new_node = Ok({ kind: ProcessingInstruction({ target, data: normalize_lines(Str.from_utf8_lossy(inside.drop_first($data))) }), attributes: [], children: [], origin: Authored(token.span) })
				}
			}
			Declaration | Invalid => {}
		}
		# XML coalesces adjacent character data, including CDATA. Comments and
		# processing instructions remain explicit boundaries in this tree.
		match $new_node {
			Ok({ kind: Text(value), .. }) => {
				last = match $stack.last() {
					Ok(parent) => ($nodes.get(parent) ?? empty_node).children.last()
					Err(_) => $roots.last()
				}
				match last {
					Ok(index) => match $nodes.get(index) {
						Ok({ kind: Text(previous), origin: Authored(span), .. }) => {
							$nodes = $nodes.set(index, empty_node) ?? []
							$nodes = $nodes.set(index, { kind: Text(Str.concat(previous, value)), attributes: [], children: [], origin: Authored({ start: span.start, end: token.span.end }) }) ?? []
							$new_node = Err(Missing)
						}
						_ => {}
					}
					Err(_) => {}
				}
			}
			_ => {}
		}
		match $new_node {
			Ok(new_node) => {
				index = $nodes.len()
				if index >= props.max_nodes {
					return Err(error_at(source, token.span.start, ResourceLimit("nodes")))
				}
				$nodes = $nodes.append(new_node)
				match $stack.last() {
					Ok(parent) => {
						# Move the parent out so appending a child can reuse its list.
						moved = $nodes.replace(parent, empty_node) ?? { list: [], prev: empty_node }
						node = moved.prev
						$nodes = moved.list.set(parent, { ..node, children: node.children.append(index) }) ?? []
					}
					Err(_) => {
						$roots = $roots.append(index)
					}
				}
				if token.kind == Tag and bytes.get(bytes.len() - 2) != Ok('/') {
					$stack = $stack.append(index)
				}
			}
			Err(_) => {}
		}
	}
	Ok({ source, tree: { nodes: $nodes, roots: $roots }, diagnostics: [] })
}

empty_node : Markup.Node
empty_node = { kind: Text(""), attributes: [], children: [], origin: Implied(0) }

error_at : Markup.Source, U64, Markup.Problem -> [InvalidMarkup(Markup.Error)]
error_at = |source, offset, problem| {
	at = Markup.position(source, offset)
	InvalidMarkup({ problem, line: at.line, column: at.column })
}

normalize_lines : Str -> Str
normalize_lines = |text| {
	if text.contains("\r") {
		Str.join_with(Str.join_with(text.split_on("\r\n"), "\n").split_on("\r"), "\n")
	} else text
}

# Attribute ranges come from the source, while values and expanded names come
# from the independently validated reader event. This retains declaration order.
source_attributes : Markup.Source, Markup.Span, StartInfo -> List(Markup.Attribute)
source_attributes = |source, span, start| {
	bytes = source.bytes
	var $pos = span.start + 1
	while $pos < span.end and !space(bytes.get($pos) ?? 0) and bytes.get($pos) != Ok('/') and bytes.get($pos) != Ok('>') {
		$pos = $pos + 1
	}
	var $attribute_index = 0
	var $binding_index = 0
	var $attrs = []
	while $pos < span.end {
		while $pos < span.end and space(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if bytes.get($pos) == Ok('/') or bytes.get($pos) == Ok('>') {
			break
		}
		begin = $pos
		while $pos < span.end and !space(bytes.get($pos) ?? 0) and bytes.get($pos) != Ok('=') {
			$pos = $pos + 1
		}
		name_end = $pos
		declaration = name_end >= begin + 5 and bytes.get(begin) == Ok('x') and bytes.get(begin + 1) == Ok('m') and bytes.get(begin + 2) == Ok('l') and bytes.get(begin + 3) == Ok('n') and bytes.get(begin + 4) == Ok('s') and (name_end == begin + 5 or bytes.get(begin + 5) == Ok(':'))
		while $pos < span.end and bytes.get($pos) != Ok('=') {
			$pos = $pos + 1
		}
		$pos = $pos + 1
		while $pos < span.end and space(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		quote = bytes.get($pos) ?? 0
		$pos = $pos + 1
		while $pos < span.end and bytes.get($pos) != Ok(quote) {
			$pos = $pos + 1
		}
		$pos = $pos + 1
		if declaration {
			match start.bindings.get($binding_index) {
				Ok(binding) => {
					$binding_index = $binding_index + 1
					value = match binding.namespace {
						Uri(uri) => uri
						NoNamespace => ""
					}
					(local, prefix) = match binding.prefix {
						Default => ("xmlns", Unprefixed)
						Prefix(p) => (p, Prefix("xmlns"))
					}
					$attrs = $attrs.append({ name: { namespace: Uri("http://www.w3.org/2000/xmlns/"), local, prefix }, value, span: { start: begin, end: $pos } })
				}
				Err(_) => {}
			}
		} else {
			match start.attributes.get($attribute_index) {
				Ok(attribute) => {
					$attribute_index = $attribute_index + 1
					$attrs = $attrs.append({ name: attribute.name, value: attribute.value, span: { start: begin, end: $pos } })
				}
				Err(_) => {}
			}
		}
	}
	$attrs
}

expect {
	parsed = Markup.parse_xml("<?p q?><svg xmlns='http://www.w3.org/2000/svg' x='a&amp;b'><!--hi--><![CDATA[<x>]]><g/></svg>", {})?
	root = Markup.single_root(parsed.tree)?
	node = parsed.tree.nodes.get(root)?
	name_ok = match node.kind {
		Element(name) => name.namespace == Uri(Markup.svg_namespace) and name.local == "svg"
		_ => False
	}
	name_ok and node.attributes.len() == 2 and node.children.len() == 3 and Markup.text(parsed.tree, root) == "<x>" and node.origin == Authored({ start: 7, end: 93 })
}
expect {
	input = "a<g xmlns='urn:x' a='1'/><!--x--><path/>b"
	parsed = Markup.parse_xml_fragment(input, Markup.svg_namespace, {})?
	roots = parsed.tree.roots
	first_element = parsed.tree.nodes.get(roots.get(1)?)?
	last_element = parsed.tree.nodes.get(roots.get(3)?)?
	roots.len() == 5 and first_element.kind == Element({ namespace: Uri("urn:x"), local: "g", prefix: Unprefixed }) and last_element.kind == Element({ namespace: Uri(Markup.svg_namespace), local: "path", prefix: Unprefixed })
}
expect Markup.parse_xml_fragment("", Markup.svg_namespace, {}).map_ok(|parsed| parsed.tree) == Ok({ nodes: [], roots: [] })
expect Markup.parse_xml_fragment("<a><b/></a>", "", { max_depth: 1 }).is_err()
expect Markup.parse_xml_fragment("<a/>", "", { max_depth: 1, max_elements: 1, max_attributes: 0 }).is_ok()
expect Markup.parse_xml_fragment("<a x='1'/>", "", { max_attributes: 0 }).is_err()
expect Markup.parse_xml_fragment("<p:a/>", "", {}) == Err(InvalidMarkup({ problem: NamespaceError(UndeclaredPrefix("p")), line: 1, column: 1 }))
expect {
	actual = Markup.parse_xml_fragment("<a>\n<b></a>", "", {}).map_err(|InvalidMarkup(error)| (error.line, error.column))
	actual == Err((2, 4))
}
expect Markup.parse_xml("<a/>", { syntax: HtmlSyntax }).is_err()
expect Markup.parse_xml("<!DOCTYPE a><a/>", {}).is_err()
expect {
	input = "<![CDATA[<a>]]><?p a>b?><!----><a x='>'/>tail"
	source = Markup.Source.from_str(input)
	source.tokens.map(|token| token.kind) == [CData, ProcessingInstruction, Comment, Tag, Text] and Str.from_utf8(source.tokens.fold([], |out, token| out.concat(source.slice(token.span)))) == Ok(input)
}
expect {
	parsed = Markup.parse_xml("<x xmlns:p='urn:p' a='v' p:a='w'/>", {})?
	root = parsed.tree.nodes.get(Markup.single_root(parsed.tree)?)?
	root.attributes.map(|a| a.name.local) == ["p", "a", "a"] and root.attributes.map(|a| Str.from_utf8_lossy(parsed.source.slice(a.span))) == ["xmlns:p='urn:p'", "a='v'", "p:a='w'"]
}

expect Markup.parse_xml_fragment("<path/>oops", Markup.svg_namespace, {}).map_ok(|parsed| Markup.single_root(parsed.tree)) == Ok(Err(RootCount(2)))
expect Markup.parse_xml_fragment(" <!--c--><path/><?p q?> ", Markup.svg_namespace, {}).map_ok(|parsed| Markup.single_root(parsed.tree).is_ok()) == Ok(True)

expect Markup.parse_xml("<a><!--c-->text<?p q?></a>", { max_nodes: 3 }).is_err()
expect Markup.parse_xml("<a><!--c-->text<?p q?></a>", { max_nodes: 4 }).is_ok()
expect Markup.parse_xml_fragment("", "", { max_nodes: 0 }).is_ok()
expect Markup.parse_xml_fragment("<!--c-->", "", { max_nodes: 0 }).is_err()
expect Markup.parse_xml_fragment("<a/>", "", { max_depth: 18_446_744_073_709_551_615, max_elements: 18_446_744_073_709_551_615 }).is_ok()

expect {
	parsed = Markup.parse_xml_fragment("<text a=\"x\t y\">a\r\nb<![CDATA[<&]]><!-- comment -->c</text>", Markup.svg_namespace, {})?
	root = parsed.tree.nodes.get(Markup.single_root(parsed.tree)?)?
	root.children.map(|index| parsed.tree.nodes.get(index).map_ok(|node| node.kind)) == [Ok(Text("a\nb<&")), Ok(Comment(" comment ")), Ok(Text("c"))]
}

expect {
	input = "<x>é\rα<![CDATA[&amp;\r\nβ]]>&#13;&amp;</x>"
	parsed = Markup.parse_xml(input, {})?
	root = parsed.tree.nodes.get(Markup.single_root(parsed.tree)?)?
	child = parsed.tree.nodes.get(root.children.first()?)?
	child.kind == Text("é\nα&amp;\nβ\r&") and child.origin == Authored({ start: 3, end: input.count_utf8_bytes() - 4 })
}

expect Markup.Source.from_str("abc").slice({ start: 9, end: 1 }) == []
expect Markup.Source.from_str("abc").slice({ start: 1, end: 9 }) == ['b', 'c']
expect Markup.parse_xml_fragment("<path/>\u(A0)", Markup.svg_namespace, {}).map_ok(|parsed| Markup.single_root(parsed.tree)) == Ok(Err(RootCount(2)))

expect Markup.parse_xml("<a/>", { max_work_per_byte: 0 }).is_err()
expect Markup.parse_xml_fragment("", "", { max_work_per_byte: 0 }).is_err()
expect Markup.parse_xml_fragment("<a/>", "", { max_work_per_byte: 8 }).is_ok()
expect Markup.parse_xml_fragment("<a/>", "", { max_work_per_byte: 18_446_744_073_709_551_615 }).is_ok()

expect Markup.position(Markup.Source.from_bytes([255, 13, 10, 65]), 3) == { line: 2, column: 1 }
expect Markup.position(Markup.Source.from_bytes([255, 13, 10, 65]), 999) == { line: 2, column: 2 }

expect Markup.Source.from_tokens([], []).partition_valid()
expect Markup.Source.from_tokens(['a'], [{ kind: Text, span: { start: 0, end: 1 } }]).partition_valid()
expect !Markup.Source.from_tokens(['a'], []).partition_valid()
expect !Markup.Source.from_tokens(['a'], [{ kind: Text, span: { start: 0, end: 0 } }]).partition_valid()
expect !Markup.Source.from_tokens(['a'], [{ kind: Text, span: { start: 1, end: 2 } }]).partition_valid()

expect {
	input = "<é a='A' xmlns:p='urn:p' p:b='B' xmlns='urn:default' c='C' xmlns:q='urn:q' q:d='D'/>"
	parsed = Markup.parse_xml(input, {})?
	attrs = parsed.tree.nodes.first()?.attributes
	attrs.map(|a| (a.name.local, a.value, Str.from_utf8_lossy(parsed.source.slice(a.span)))) == [
		("a", "A", "a='A'"),
		("p", "urn:p", "xmlns:p='urn:p'"),
		("b", "B", "p:b='B'"),
		("xmlns", "urn:default", "xmlns='urn:default'"),
		("c", "C", "c='C'"),
		("q", "urn:q", "xmlns:q='urn:q'"),
		("d", "D", "q:d='D'"),
	]
}
expect {
	parsed = Markup.parse_xml("<x xmlnsfoo='a' xmlns:p='urn:p' p:q='b'/>", {})?
	parsed.tree.nodes.first()?.attributes.map(|a| (a.name.local, a.name.namespace)) == [("xmlnsfoo", NoNamespace), ("p", Uri("http://www.w3.org/2000/xmlns/")), ("q", Uri("urn:p"))]
}

SourceBuild : {
	nodes : List(Markup.Node),
	roots : List(U64),
	stack : List(U64),
	tokens : List(Markup.Token),
	failure : [NoFailure, AttributeFailure(U64), NodeFailure(U64)],
}

parse_xml_fold : Str, Str, U64, U64, U64, Bool, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
parse_xml_fold = |text, input, prefix, depth, elements, fragment, props| {
	initial = { nodes: [], roots: [], stack: [], tokens: [], failure: NoFailure }
	end = prefix + text.count_utf8_bytes()
	state = XmlNs.fold_source(input, { max_depth: depth, max_elements: elements, max_attributes: if (props.max_attributes == 0 and fragment) 1 else props.max_attributes }, initial, |before, event| fold_xml_event(before, event, prefix, end, props)).map_err(|error| located_xml_error(error, text, prefix))?
	source = Markup.Source.from_tokens(text.to_utf8(), state.tokens)
	match state.failure {
		AttributeFailure(offset) => Err(error_at(source, offset, LimitExceeded(Attributes)))
		NodeFailure(offset) => Err(error_at(source, offset, ResourceLimit("nodes")))
		NoFailure => Ok({ source, tree: { nodes: state.nodes, roots: state.roots }, diagnostics: [] })
	}
}

fold_xml_event : SourceBuild, XmlNs.SourceEvent, U64, U64, Markup.Props -> SourceBuild
fold_xml_event = |state, event, prefix, source_end, props| {
	(original_span, kind, push) = match event {
		Start(tag) => (tag.span, Tag, !tag.empty)
		End(end_span) => (end_span, Tag, False)
		Text(text) => (text.span, Text, False)
		CData(text) => (text.span, CData, False)
		Comment(comment) => (comment.span, Comment, False)
		ProcessingInstruction(pi) => (pi.span, ProcessingInstruction, False)
		Declaration(span) => (span, ProcessingInstruction, False)
		Gap(span) => (span, Text, False)
	}
	if original_span.start < prefix or original_span.end > source_end {
		return state
	}
	span = { start: original_span.start - prefix, end: original_span.end - prefix }
	var $tokens = state.tokens
	previous = $tokens.last() ?? { kind: Invalid, span: { start: 0, end: 0 } }
	if kind == Text and previous.kind == Text and previous.span.end == span.start {
		$tokens = $tokens.set($tokens.len() - 1, { kind: Text, span: { start: previous.span.start, end: span.end } }) ?? []
	} else {
		$tokens = $tokens.append({ kind, span })
	}
	next = { ..state, tokens: $tokens }
	if next.failure != NoFailure {
		return next
	}
	var $new_node = Err(Missing)
	match event {
		Start(tag) => {
			attrs = shifted_source_attributes(tag.attributes, prefix)
			if attrs.len() > props.max_attributes {
				return { ..next, failure: AttributeFailure(span.start) }
			}
			$new_node = Ok({ kind: Element(tag.name), attributes: attrs, children: [], origin: Authored(span) })
		}
		End(_) => {
			match next.stack.last() {
				Ok(index) => {
					node = next.nodes.get(index) ?? empty_node
					start = match node.origin {
						Authored(origin) => origin.start
						Implied(at) => at
						Cloned(copy) => copy.source.start
					}
					return { ..next, stack: next.stack.drop_last(1), nodes: next.nodes.set(index, { ..node, origin: Authored({ start, end: span.end }) }) ?? [] }
				}
				Err(_) => return next
			}
		}
		Text(text) | CData(text) => {
			if text.value != "" {
				$new_node = Ok({ kind: Text(text.value), attributes: [], children: [], origin: Authored(span) })
			}
		}
		Comment(comment) => {
			$new_node = Ok({ kind: Comment(comment.value), attributes: [], children: [], origin: Authored(span) })
		}
		ProcessingInstruction(pi) => {
			$new_node = Ok({ kind: ProcessingInstruction({ target: pi.target, data: pi.data }), attributes: [], children: [], origin: Authored(span) })
		}
		Declaration(_) | Gap(_) => {}
	}
	match $new_node {
		Ok({ kind: Text(value), .. }) => {
			last = match next.stack.last() {
				Ok(parent) => (next.nodes.get(parent) ?? empty_node).children.last()
				Err(_) => next.roots.last()
			}
			match last {
				Ok(index) => match next.nodes.get(index) {
					Ok({ kind: Text(before), origin: Authored(origin), .. }) => {
						moved = next.nodes.replace(index, empty_node) ?? { list: [], prev: empty_node }
						return { ..next, nodes: moved.list.set(index, { kind: Text(Str.concat(before, value)), attributes: [], children: [], origin: Authored({ start: origin.start, end: span.end }) }) ?? [] }
					}
					_ => {}
				}
				Err(_) => {}
			}
		}
		_ => {}
	}
	match $new_node {
		Err(_) => next
		Ok(node) => {
			index = next.nodes.len()
			if index >= props.max_nodes {
				return { ..next, failure: NodeFailure(span.start) }
			}
			var $nodes = next.nodes.append(node)
			var $roots = next.roots
			match next.stack.last() {
				Ok(parent) => {
					moved = $nodes.replace(parent, empty_node) ?? { list: [], prev: empty_node }
					previous_node = moved.prev
					$nodes = moved.list.set(parent, { ..previous_node, children: previous_node.children.append(index) }) ?? []
				}
				Err(_) => {
					$roots = $roots.append(index)
				}
			}
			stack = if (push) next.stack.append(index) else next.stack
			{ ..next, nodes: $nodes, roots: $roots, stack }
		}
	}
}

expect {
	["<", "</", "</nootg", "</a>", "<a/>", "<!bad>", "<?xml?>", "<a/>tail", "<a/><!--bad", "<a>x<!--bad-- ></a>", "<?xml version='1.0'?><a/>", "\u(FEFF)\r\n<!--p--><a>t<![CDATA[c]]>&amp;</a><?p d?>"].all(
		|input| Markup.parse_xml(input, {}) == Markup.parse_xml(input, { max_work_per_byte: 15 }),
	)
}

expect {
	input = "<a x='1'><p:q/></a>"
	Markup.parse_xml(input, { max_nodes: 0 }) == Markup.parse_xml(input, { max_nodes: 0, max_work_per_byte: 15 }) and Markup.parse_xml_fragment(input, "", { max_attributes: 0 }) == Markup.parse_xml_fragment(input, "", { max_attributes: 0, max_work_per_byte: 15 })
}

expect {
	["", "a", "ab", "&", "<", "</", " ", "\r\n", "é", "<!--c--><x xmlns:p='urn:p' a='1' p:b='2'><![CDATA[&amp;]]></x>"].all(
		|input| Markup.parse_xml_fragment(input, "urn:context", {}) == Markup.parse_xml_fragment(input, "urn:context", { max_work_per_byte: 15 }),
	)
}

shifted_source_attributes : List(XmlNs.SourceAttribute), U64 -> List(Markup.Attribute)
shifted_source_attributes = |attributes, prefix| {
	# Keep the fresh indexed output: a same-layout map makes the event
	# callback borrow its entire arena state on the supported compiler.
	var $out = List.with_capacity(attributes.len())
	var $index = 0.U64
	while $index < attributes.len() {
		attribute = attributes.get($index) ?? crash "invalid attribute index"
		$out = $out.append({ name: attribute.name, value: attribute.value, span: { start: attribute.span.start - prefix, end: attribute.span.end - prefix } })
		$index = $index + 1
	}
	$out
}

parse_xml_direct : Str, Str, U64, U64, U64, Bool, Markup.Props -> Try(Markup.Parsed, [Fallback])
parse_xml_direct = |text, input, prefix, maximum_depth, maximum_elements, fragment, props| {
	bytes = input.to_utf8()
	maximum_attributes = if props.max_attributes == 0 and fragment {
		1
	} else props.max_attributes
	source_end = prefix + text.count_utf8_bytes()
	var $pos = 0.U64
	while $pos < bytes.len() and space(bytes.get($pos) ?? 0) {
		$pos = $pos + 1
	}
	root_start = $pos
	if bytes.get(root_start) != Ok('<') or bytes.get(root_start + 1) == Ok('!') or bytes.get(root_start + 1) == Ok('?') or bytes.get(root_start + 1) == Ok('/') or !Xml.SourceLex.allows(maximum_depth, maximum_elements, 0, 0) {
		return Err(Fallback)
	}
	root_tag = Xml.SourceLex.read_start(input, bytes, root_start, maximum_attributes)?
	root_name_end = root_start + 1 + root_tag.name.count_utf8_bytes()

	# Markers can cause harmless false positives, but make late unsupported
	# scope/comment/PI constructs fall back before allocating a partial arena.
	tail = input.drop_first_bytes(root_tag.pos) ?? ""
	if tail.contains("xmlns") or tail.contains("<!") or tail.contains("<?") {
		return Err(Fallback)
	}
	resolved_root = XmlNs.StableSourceScope.begin(root_tag.name, root_tag.attributes, root_tag.attribute_spans)?
	stable_scope = resolved_root.scope
	var $nodes = []
	var $roots = []
	var $stack = []
	var $tokens = []
	if !fragment {
		if root_start > 0 {
			$tokens = $tokens.append({ kind: Text, span: { start: 0, end: root_start } })
		}
		if props.max_nodes == 0 or resolved_root.attributes.len() > props.max_attributes {
			return Err(Fallback)
		}
		root_span = { start: root_start, end: root_tag.pos }
		$tokens = $tokens.append({ kind: Tag, span: root_span })
		$nodes = $nodes.append({ kind: Element(resolved_root.name), attributes: shifted_source_attributes(resolved_root.attributes, 0), children: [], origin: Authored(root_span) })
		$roots = $roots.append(0)
		if !root_tag.empty {
			$stack = $stack.append(0)
		}
	}
	var $open = if root_tag.empty {
		[]
	} else [{ start: root_start + 1, end: root_name_end }]
	var $elements = 1.U64
	$pos = root_tag.pos
	while $pos < bytes.len() {
		start = $pos
		leading = bytes.get(start) ?? 0
		following = bytes.get(start + 1) ?? 0
		var $new_node = Err(Missing)
		var $push = False
		var $kind = Text
		if $open.is_empty() {
			if !space(leading) {
				return Err(Fallback)
			}
			while $pos < bytes.len() and space(bytes.get($pos) ?? 0) {
				$pos = $pos + 1
			}
		} else if leading == '<' and following == '/' {
			closed_tag = Xml.SourceLex.read_end(input, bytes, start, $open.last() ?? { start: 0, end: 0 })?
			$pos = closed_tag.pos
			$open = $open.drop_last(1)
			$kind = Tag
			if start >= prefix and $pos <= source_end {
				match $stack.last() {
					Ok(index) => {
						begin = match ($nodes.get(index) ?? empty_node).origin {
							Authored(span) => span.start
							_ => return Err(Fallback)
						}
						end = $pos - prefix
						$nodes = $nodes.update(index, |previous_node| { ..previous_node, origin: Authored({ start: begin, end }) }) ?? []
						$stack = $stack.drop_last(1)
					}
					Err(_) => return Err(Fallback)
				}
			}
		} else if leading == '<' {
			if following == '!' or following == '?' or !Xml.SourceLex.allows(maximum_depth, maximum_elements, $open.len(), $elements) {
				return Err(Fallback)
			}
			tag = stable_scope.read_tag(input, bytes, start, maximum_attributes, prefix)?
			name_end = tag.name_end
			resolved = { name: tag.name, attributes: tag.attributes }
			$pos = tag.pos
			$elements = $elements + 1
			if resolved.attributes.len() > props.max_attributes {
				return Err(Fallback)
			}
			if !tag.empty {
				$open = $open.append({ start: start + 1, end: name_end })
				$push = True
			}
			$kind = Tag
			$new_node = Ok({ kind: Element(resolved.name), attributes: resolved.attributes, children: [], origin: Authored({ start: start - prefix, end: $pos - prefix }) })
		} else {
			parsed_text = Xml.SourceLex.read_text(input, bytes, start)?
			$pos = parsed_text.pos
			if parsed_text.val != "" {
				$new_node = Ok({ kind: Text(parsed_text.val), attributes: [], children: [], origin: Authored({ start: start - prefix, end: $pos - prefix }) })
			}
		}
		if start >= prefix and $pos <= source_end {
			span = { start: start - prefix, end: $pos - prefix }
			previous = $tokens.last() ?? { kind: Invalid, span: { start: 0, end: 0 } }
			if $kind == Text and previous.kind == Text and previous.span.end == span.start {
				$tokens = $tokens.set($tokens.len() - 1, { kind: Text, span: { start: previous.span.start, end: span.end } }) ?? []
			} else {
				$tokens = $tokens.append({ kind: $kind, span })
			}
			match $new_node {
				Err(_) => {}
				Ok(node) => {
					index = $nodes.len()
					if index >= props.max_nodes {
						return Err(Fallback)
					}
					$nodes = $nodes.append(node)
					match $stack.last() {
						Ok(parent) => {
							$nodes = $nodes.update(parent, |previous_node| { ..previous_node, children: previous_node.children.append(index) }) ?? []
						}
						Err(_) => {
							$roots = $roots.append(index)
						}
					}
					if $push {
						$stack = $stack.append(index)
					}
				}
			}
		}
	}
	if !$open.is_empty() {
		return Err(Fallback)
	}
	Ok({ source: Markup.Source.from_tokens(text.to_utf8(), $tokens), tree: { nodes: $nodes, roots: $roots }, diagnostics: [] })
}

expect {
	["<r><n/></bad>", "<r><n></bad></r>", "<r><n a='bad></r>", "<r><n>&#0;</n></r>", "<r><n>\u(0001)</n></r>", "<é:r xmlns:é='urn:é'><é:n/></é:r   > \r\n", "<r><r></r>"].all(
		|input| Markup.parse_xml(input, {}) == Markup.parse_xml(input, { max_work_per_byte: 15 }),
	)
}
expect {
	["<n></bad>", "<n a='bad>", "<n>&#0;</n>", "text<n/>tail"].all(
		|input| Markup.parse_xml_fragment(input, "urn:context", {}) == Markup.parse_xml_fragment(input, "urn:context", { max_work_per_byte: 15 }),
	)
}
