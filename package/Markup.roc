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

		## Retain UTF-8 source without normalization.
		from_str : Str -> Source
		from_str = |text| from_bytes(text.to_utf8())

		## The exact bytes of a source token.
		slice : Source, Span -> List(U8)
		slice = |source, span| source.bytes.drop_first(span.start).take_first(span.end - span.start)
	}

	## Typed attribute grammars shared by validators and interpolation slots.
	ValueGrammar : [Number, NonnegativeNumber, Length, NonnegativeLength, NumberList(U64), LengthList, PathData, TransformList, Keyword(List(Str)), TokenList, Color, Paint, AspectRatio, Language(Str)]

	## Structured reason for rejection
	## Presentation is separate.
	Problem : [SyntaxError(Str), NamespaceError(XmlNs.Problem), WrongRoot({ expected : Str, actual : Str }), UnknownElement(Str), UnknownAttribute({ element : Str, attribute : Str }), InvalidValue({ attribute : Str, value : Str, expected : ValueGrammar }), InvalidChildren({ element : Str, expected : Str }), Unsupported(Str), LimitExceeded(Xml.Limit), SourceTooLarge(U64), ResourceLimit(Str), Interpolation({ index : U64, reason : Str }), BindingCount({ expected : U64, actual : U64 })]

	## One-based lines and UTF-8 byte columns refer to authored source.
	Error : { problem : Problem, line : U64, column : U64 }

	## Configurable validation and resource limits, strict by default.
	Props := { syntax : Syntax ?? XmlSyntax, allow_unknown_elements : Bool ?? False, allow_unknown_attributes : Bool ?? False, check_values : Bool ?? True, max_source_bytes : U64 ?? 16_777_216, max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_attributes : U64 ?? 1024, recovery : [Strict, Browser] ?? Strict, max_diagnostics : U64 ?? 1024, max_work_per_byte : U64 ?? 256 }

	## Expanded names share the existing namespace-aware XML contract.
	Name : XmlNs.Name

	## Attribute spelling/value and its authored byte range. Namespace declarations
	## are represented in the XMLNS namespace, preserving declaration order.
	Attribute : { name : Name, value : Str, span : Span }

	## Authored bytes, or the insertion point of a node implied by tree construction.
	Origin : [Authored(Span), Implied(U64)]

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
	parse_xml = |text, props| parse_xml_source(text, "", False, props)

	## Parse multiple XML fragment roots in an implicit default namespace.
	## Explicit xmlns declarations override this context normally. Locations
	## refer to authored bytes, excluding the internal fragment context.
	parse_xml_fragment : Str, Str, Props -> Try(Parsed, [InvalidMarkup(Error)])
	parse_xml_fragment = |text, namespace, props| parse_xml_source(text, namespace, True, props)

	## Decode one XML character-data region, including references and CDATA.
	decode_xml_text : Str -> Try(Str, [InvalidMarkup(Error)])
	decode_xml_text = |text| {
		match Xml.parse_str("<_>${text}</_>") {
			Ok(document) => Ok(document.root.text())
			Err(InvalidXml(error)) => Err(InvalidMarkup({ problem: SyntaxError(error.message), line: error.line, column: error.column }))
		}
	}

	## Locate a byte offset using the common one-based UTF-8 position convention.
	position : Source, U64 -> { line : U64, column : U64 }
	position = |source, offset| Literal.position(Str.from_utf8_lossy(source.bytes), offset)

	## The only element root, ignoring comments and processing instructions.
	single_root : Tree -> Try(U64, [RootCount(U64)])
	single_root = |tree| {
		roots = tree.roots.keep_if(
			|index| match tree.nodes.get(index) {
				Ok({ kind: Element(_), .. }) => True
				_ => False
			},
		)
		if roots.len() == 1 {
			Ok(roots.first() ?? 0)
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
	depth = props.max_depth + if (fragment) 1 else 0
	elements = props.max_elements + if (fragment) 1 else 0
	opened = XmlNs.reader(input, { max_depth: depth, max_elements: elements, max_attributes: if (props.max_attributes == 0 and fragment) 1 else props.max_attributes }).map_err(|error| located_xml_error(error, text, prefix.count_utf8_bytes()))?
	var $reader = opened
	var $starts = []
	var $done = False
	var $context = fragment
	while !$done {
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
	construct_xml_tree(Markup.Source.from_str(text), $starts, fragment, props)
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

construct_xml_tree : Markup.Source, List(StartInfo), Bool, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
construct_xml_tree = |source, starts, fragment, props| {
	var $nodes = []
	var $roots = []
	var $stack = []
	var $start_index = 0
	for token in source.tokens {
		bytes = source.slice(token.span)
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
					value = Markup.decode_xml_text(Str.from_utf8_lossy(bytes))?
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
		match $new_node {
			Ok(new_node) => {
				index = $nodes.len()
				$nodes = $nodes.append(new_node)
				match $stack.last() {
					Ok(parent) => {
						node = $nodes.get(parent) ?? empty_node
						$nodes = $nodes.set(parent, { ..node, children: node.children.append(index) }) ?? []
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
normalize_lines = |text| Str.join_with(Str.join_with(text.split_on("\r\n"), "\n").split_on("\r"), "\n")

# Attribute ranges come from the source, while values and expanded names come
# from the independently validated reader event. This retains declaration order.
source_attributes : Markup.Source, Markup.Span, StartInfo -> List(Markup.Attribute)
source_attributes = |source, span, start| {
	bytes = source.bytes
	var $pos = span.start + 1
	while $pos < span.end and !space(bytes.get($pos) ?? 0) and !['/', '>'].contains(bytes.get($pos) ?? 0) {
		$pos = $pos + 1
	}
	var $attrs = []
	while $pos < span.end {
		while $pos < span.end and space(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if ['/', '>'].contains(bytes.get($pos) ?? 0) {
			break
		}
		begin = $pos
		while $pos < span.end and !space(bytes.get($pos) ?? 0) and bytes.get($pos) != Ok('=') {
			$pos = $pos + 1
		}
		spelling = Str.from_utf8_lossy(bytes.drop_first(begin).take_first($pos - begin))
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
		var $attr = Err(Missing)
		if spelling == "xmlns" or spelling.starts_with("xmlns:") {
			prefix = if (spelling == "xmlns") Default else Prefix(Str.from_utf8_lossy(spelling.to_utf8().drop_first(6)))
			match start.bindings.find_first(|binding| binding.prefix == prefix) {
				Ok(binding) => {
					value = match binding.namespace {
						Uri(uri) => uri
						NoNamespace => ""
					}
					local = if (spelling == "xmlns") "xmlns" else Str.from_utf8_lossy(spelling.to_utf8().drop_first(6))
					$attr = Ok({ name: { namespace: Uri("http://www.w3.org/2000/xmlns/"), local, prefix: if (spelling == "xmlns") Unprefixed else Prefix("xmlns") }, value, span: { start: begin, end: $pos } })
				}
				Err(_) => {}
			}
		} else {
			match start.attributes.find_first(|attr| qualified_name(attr.name) == spelling) {
				Ok(attr) => {
					$attr = Ok({ name: attr.name, value: attr.value, span: { start: begin, end: $pos } })
				}
				Err(_) => {}
			}
		}
		match $attr {
			Ok(attr) => {
				$attrs = $attrs.append(attr)
			}
			Err(_) => {}
		}
	}
	$attrs
}

qualified_name : Markup.Name -> Str
qualified_name = |name| match name.prefix {
	Unprefixed => name.local
	Prefix(prefix) => "${prefix}:${name.local}"
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
