import Xml

## XML with namespaces resolved, following
## [Namespaces in XML 1.0 (Third Edition)](https://www.w3.org/TR/xml-names/).
##
## Every element and attribute name is split into an optional prefix and a
## local part, and the prefix is resolved to the namespace bound to it in
## scope. A binding (an `xmlns` or `xmlns:p` attribute) covers its element
## and that element's descendants, and an inner binding shadows an outer
## one. Unprefixed elements are in the default namespace, if one is in
## scope; unprefixed attributes are in no namespace. The `xml` prefix is
## always bound to [XmlNs.xml_namespace].
##
## Names are resolved while the document is read, so only one tree is ever
## built. Declarations are kept in each element's `bindings`, in source
## order, and every name keeps its prefix, so a tree can be written back
## with the prefixes and declarations it was read with.
##
## ```roc
## expect {
##     doc = XmlNs.parse_str("<feed xmlns='http://www.w3.org/2005/Atom'><title>News</title></feed>")?
##     atom = Uri("http://www.w3.org/2005/Atom")
##     doc.root.children_named(atom, "title").map(|title| title.text()) == ["News"]
## }
## ```
##
## Namespace names are compared as strings, as the specification requires,
## and a namespace problem is reported with the location of the start tag.
## Problems are reported in document order: a namespace problem in a start
## tag comes before a well-formedness error later in the document.
XmlNs := {
	declaration : Try(Xml.Declaration, [Missing]),
	root : XmlNs.Node,
}.{

	## Compare two documents structurally (declaration and tree).
	is_eq : _

	## Hash a document, so documents can be `Dict` keys and `Set` members.
	to_hash : _

	## The namespace a name is in. `xmlns=""` puts unprefixed elements in
	## `NoNamespace`, and unprefixed attributes are always in it.
	Namespace : [Uri(Str), NoNamespace]

	## A resolved name: its namespace, its local part, and the prefix it was
	## written with.
	##
	## ```roc
	## expect XmlNs.parse_str("<s:svg xmlns:s='http://www.w3.org/2000/svg'/>").map_ok(|doc| doc.root.name()) == Ok(Ok({ namespace: Uri("http://www.w3.org/2000/svg"), local: "svg", prefix: Prefix("s") }))
	## ```
	Name : { namespace : Namespace, local : Str, prefix : [Prefix(Str), Unprefixed] }

	## A namespace declaration: `xmlns:p="u"` binds `Prefix("p")`, and
	## `xmlns="u"` binds `Default` (to `NoNamespace` for `xmlns=""`).
	Binding : { prefix : [Prefix(Str), Default], namespace : Namespace }

	## An attribute other than a namespace declaration, with its resolved
	## name and its decoded value.
	Attribute : { name : Name, value : Str }

	## An element or text node of a namespaced document.
	##
	## An `Element` holds its resolved `name`, the namespace declarations it
	## makes in `bindings` (in source order), its other `attributes` in source
	## order, and its `children` in document order. `Text` is as in
	## [Xml.Node].
	Node := [
		Element({ name : Name, bindings : List(Binding), attributes : List(Attribute), children : List(Node) }),
		Text(Str),
	].{

		## Compare two nodes structurally.
		is_eq : _

		## Hash a node, so nodes can be `Dict` keys and `Set` members.
		to_hash : _

		## The resolved name of an element, or `Err(NotAnElement)` for text.
		name : Node -> Try(Name, [NotAnElement])
		name = |node| {
			match node {
				Element(element) => Ok(element.name)
				Text(_) => Err(NotAnElement)
			}
		}

		## The value of the element's attribute with this namespace and local
		## name, or `Err(Missing)` when there is none or the node is text.
		##
		## ```roc
		## expect {
		##     root = XmlNs.parse_str("<a xmlns:x='urn:x' x:id='1' id='2'/>")?.root
		##     root.attribute(Uri("urn:x"), "id") == Ok("1") and root.attribute(NoNamespace, "id") == Ok("2")
		## }
		## ```
		attribute : Node, Namespace, Str -> Try(Str, [Missing])
		attribute = |node, namespace, local| {
			match node {
				Element(element) =>
					match element.attributes.find_first(|attr| attr.name.local == local and attr.name.namespace == namespace) {
						Ok(attr) => Ok(attr.value)
						Err(_) => Err(Missing)
					}

				Text(_) => Err(Missing)
			}
		}

		## The element's direct child elements with this namespace and local
		## name, in document order. Empty for a text node.
		children_named : Node, Namespace, Str -> List(Node)
		children_named = |node, namespace, local| {
			match node {
				Element(element) =>
					element.children.keep_if(
						|child| {
							match child {
								Element(child_element) => child_element.name.local == local and child_element.name.namespace == namespace
								Text(_) => False
							}
						},
					)

				Text(_) => []
			}
		}

		## All character data in the node and its descendants, concatenated in
		## document order.
		text : Node -> Str
		text = |node| {
			# An explicit work list keeps deep trees from exhausting the call stack.
			var $out = []
			var $pending = [node]
			while !$pending.is_empty() {
				match $pending.last() {
					Ok(Text(str)) => {
						$out = $out.concat(str.to_utf8())
						$pending = $pending.drop_last(1)
					}

					Ok(Element(element)) => {
						$pending = $pending.drop_last(1)
						var $index = element.children.len()
						while $index > 0 {
							$index = $index - 1
							match element.children.get($index) {
								Ok(child) => {
									$pending = $pending.append(child)
								}

								Err(_) => {}
							}
						}
					}

					Err(_) => {
						$pending = []
					}
				}
			}
			Str.from_utf8($out) ?? ""
		}
	}

	## A rule of Namespaces in XML 1.0 that a document breaks.
	##
	## - `UndeclaredPrefix(p)`: an element or attribute name uses a prefix
	##   with no binding in scope.
	## - `ReservedPrefix(p)`: `xmlns` is declared or used as an element
	##   prefix, or `xml` is bound to a namespace other than its own.
	## - `ReservedNamespace(u)`: the `xml` or `xmlns` namespace is bound to
	##   another prefix or as the default.
	## - `EmptyNamespace(p)`: `xmlns:p=""`, which XML 1.0 does not allow.
	## - `NotAQName(name)`: more than one colon, or an empty or malformed
	##   prefix or local part.
	## - `DuplicateAttribute(name)`: two attributes with the same namespace and
	##   local name, such as `a:x` and `b:x` with `a` and `b` bound to the
	##   same namespace.
	Problem : [
		UndeclaredPrefix(Str),
		ReservedPrefix(Str),
		ReservedNamespace(Str),
		EmptyNamespace(Str),
		NotAQName(Str),
		DuplicateAttribute(Name),
	]

	## A namespace problem and the location of the start tag it is in.
	NamespaceProblem : { problem : Problem, line : U64, column : U64 }

	## Why reading stopped: an [Xml.ReadError], or a namespace problem.
	ReadError : [
		InvalidXml(Xml.Error),
		LimitExceeded({ limit : Xml.Limit, max : U64, line : U64, column : U64 }),
		InvalidNamespace(NamespaceProblem),
	]

	## One step through a namespaced document, as [Xml.Event] with resolved
	## names and an element's namespace declarations in `bindings`.
	Event : [
		Start({ name : Name, bindings : List(Binding), attributes : List(Attribute) }),
		End(Name),
		Text(Str),
	]

	## A pull reader that resolves namespaces as it reads, as [Xml.Reader]
	## does for names as written. Keep only the latest reader and `return`
	## from the loop's `Done` arm.
	Reader :: {
		inner : Xml.Reader,
		scope : Scope,
		last_start : [Started({ name : Name, bindings : List(Binding), attributes : List(Attribute) }), NotStarted],
	}.{

		## The next event, `Done` at the end of the document, or `Failed`.
		next : Reader -> [Event(XmlNs.Event, Reader), Done, Failed(XmlNs.ReadError)]
		next = |reader| {
			inner = reader.inner
			scope = reader.scope
			match inner.next() {
				Event(Start(tag), rest) =>
					match resolve_start(scope, tag.name, tag.attributes) {
						Ok(resolved) => {
							start = { name: resolved.name, bindings: resolved.bindings, attributes: resolved.attributes }
							Event(Start(start), { inner: rest, scope: resolved.scope, last_start: Started(start) })
						}

						Err(problem) => {
							at = rest.location()
							Failed(InvalidNamespace({ problem, line: at.line, column: at.column }))
						}
					}

				Event(End(_), rest) => {
					name = scope.names.last() ?? { namespace: NoNamespace, local: "", prefix: Unprefixed }
					Event(End(name), { inner: rest, scope: pop_scope(scope), last_start: NotStarted })
				}

				Event(Text(text), rest) => Event(Text(text), { inner: rest, scope, last_start: NotStarted })
				Done => Done
				Failed(error) => Failed(widen(error))
			}
		}

		## Directly after a `Start` event, that element and its subtree, and
		## the reader after its end tag. `NotAfterStart` otherwise.
		element : Reader -> [Element(XmlNs.Node, Reader), Failed(XmlNs.ReadError), NotAfterStart]
		element = |reader| {
			match reader.last_start {
				NotStarted => NotAfterStart
				Started(start) => build_element(reader, start)
			}
		}

		## The document's XML declaration, or `Err(Missing)` when it has none.
		declaration : Reader -> Try(Xml.Declaration, [Missing])
		declaration = |reader| reader.inner.declaration()

		## Where the most recent event starts, as [Xml.Reader] reports it.
		location : Reader -> { line : U64, column : U64 }
		location = |reader| reader.inner.location()
	}

	## A [XmlNs.Reader] positioned before the root element. The limits of
	## `props` apply, and `namespaces` is always set.
	reader : Str, Xml.ReaderProps -> Try(Reader, ReadError)
	reader = |input, props| {
		namespaced = { max_depth: props.max_depth, max_elements: props.max_elements, max_attributes: props.max_attributes, namespaces: True }
		match Xml.reader(input, namespaced) {
			Ok(inner) => Ok({ inner, scope: empty_scope, last_start: NotStarted })
			Err(error) => Err(widen(error))
		}
	}

	## An expanded source attribute, including namespace declarations.
	SourceAttribute : { name : Name, value : Str, span : Xml.SourceSpan }

	## Authored events with expanded names and ordered attribute source ranges.
	SourceEvent : [
		Start({ name : Name, attributes : List(SourceAttribute), span : Xml.SourceSpan, empty : Bool }),
		End(Xml.SourceSpan),
		Text({ value : Str, span : Xml.SourceSpan }),
		CData({ value : Str, span : Xml.SourceSpan }),
		Comment({ value : Str, span : Xml.SourceSpan }),
		ProcessingInstruction({ target : Str, data : Str, span : Xml.SourceSpan }),
		Declaration(Xml.SourceSpan),
		Gap(Xml.SourceSpan),
	]

	## Fold validated lexical source events while resolving namespace scope.
	fold_source : Str, Xml.ReaderProps, state, (state, SourceEvent -> state) -> Try(state, ReadError)
	fold_source = |input, props, initial, on_event| {
		namespaced = { max_depth: props.max_depth, max_elements: props.max_elements, max_attributes: props.max_attributes, namespaces: True }
		folded = Xml.fold_source(
			input,
			namespaced,
			{ bound: { entries: [], active: Dict.empty() }, marks: [], value: initial },
			|state, event| {
				match event {
					Start(tag) => {
						mark = state.bound.entries.len()
						resolved = resolve_source_start(state.bound, tag.name, tag.attributes, tag.attribute_spans)?
						value = on_event(state.value, Start({ name: resolved.name, attributes: resolved.attributes, span: tag.span, empty: tag.empty }))
						bound = if (tag.empty and resolved.bound.entries.len() > mark) unbind_to(resolved.bound, mark) else resolved.bound
						marks = if (tag.empty) state.marks else state.marks.append(mark)
						Ok({ bound, marks, value })
					}
					End(span) => {
						mark = state.marks.last() ?? 0
						bound = if (state.bound.entries.len() > mark) unbind_to(state.bound, mark) else state.bound
						Ok({ bound, marks: state.marks.drop_last(1), value: on_event(state.value, End(span)) })
					}
					Text(text) => Ok({ ..state, value: on_event(state.value, Text(text)) })
					CData(text) => Ok({ ..state, value: on_event(state.value, CData(text)) })
					Comment(comment) => Ok({ ..state, value: on_event(state.value, Comment(comment)) })
					ProcessingInstruction(pi) => Ok({ ..state, value: on_event(state.value, ProcessingInstruction(pi)) })
					Declaration(span) => Ok({ ..state, value: on_event(state.value, Declaration(span)) })
					Gap(span) => Ok({ ..state, value: on_event(state.value, Gap(span)) })
				}
			},
		)
		match folded {
			Folded(state) => Ok(state.value)
			Failed(error) => Err(widen(error))
			Rejected(problem, at) => Err(InvalidNamespace({ problem, line: at.line, column: at.column }))
		}
	}

	## Parse one complete namespaced document within the limits of `props`.
	##
	## ```roc
	## expect XmlNs.parse("<p:a/>", {}) == Err(InvalidNamespace({ problem: UndeclaredPrefix("p"), line: 1, column: 1 }))
	## ```
	parse : Str, Xml.ReaderProps -> Try(XmlNs, ReadError)
	parse = |input, props| {
		opened = XmlNs.reader(input, props)?
		declaration = opened.declaration()
		match opened.next() {
			Event(Start(_), after_start) =>
				match after_start.element() {
					Element(root, rest) =>
						match rest.next() {
							Done => Ok({ declaration, root })
							Failed(error) => Err(error)
							_ => Err(InvalidXml({ line: 1, column: 1, message: "internal error: an event after the root element" }))
						}

					Failed(error) => Err(error)
					NotAfterStart => Err(InvalidXml({ line: 1, column: 1, message: "internal error: no root element" }))
				}

			Failed(error) => Err(error)
			_ => Err(InvalidXml({ line: 1, column: 1, message: "internal error: no root element" }))
		}
	}

	## Parse one complete namespaced document, with no limits. Use
	## [XmlNs.parse] for untrusted input.
	##
	## ```roc
	## expect {
	##     doc = XmlNs.parse_str("<a xmlns='urn:a' xmlns:b='urn:b'><b:c/></a>")?
	##     doc.root.children_named(Uri("urn:b"), "c").len() == 1
	## }
	## ```
	parse_str : Str -> Try(XmlNs, [InvalidXml(Xml.Error), InvalidNamespace(NamespaceProblem)])
	parse_str = |input| {
		size = input.count_utf8_bytes()
		match XmlNs.parse(input, { max_depth: size, max_elements: size, max_attributes: size }) {
			Ok(doc) => Ok(doc)
			Err(InvalidXml(error)) => Err(InvalidXml(error))
			Err(InvalidNamespace(problem)) => Err(InvalidNamespace(problem))
			Err(LimitExceeded(exceeded)) => Err(InvalidXml({ line: exceeded.line, column: exceeded.column, message: "limit exceeded" }))
		}
	}

	## How [XmlNs.render] writes a document.
	##
	## - `declaration`: `Plain` writes `<?xml version="1.0" encoding="UTF-8"?>`
	##   and a line feed, `Standalone` also declares `standalone="yes"`, and
	##   `Omitted` writes no declaration.
	## - `prefixes`: `AsRead` writes each element's own `bindings` and each
	##   name's own prefix. `Table(entries)` writes each name with its
	##   namespace's prefix from the table, ignoring the tree's prefixes and
	##   bindings, and declares on the root, in table order, every namespace
	##   the document uses. An empty prefix makes a namespace the default for
	##   elements, declared with `xmlns="u"` on the first element that needs it
	##   (the root, when it is in that namespace), and `xmlns=""` or `xmlns="u"`
	##   is written wherever the default must change. The `xml` namespace is
	##   always written with `xml`.
	## - `attribute_order`: `AsGiven` keeps each element's attributes in order,
	##   and `SortedBy(compare)` sorts them.
	RenderProps := {
		declaration : [Plain, Standalone, Omitted] ?? Plain,
		prefixes : [AsRead, Table(List({ prefix : Str, namespace : Str }))] ?? AsRead,
		attribute_order : [AsGiven, SortedBy((Name, Name -> [Before, Same, After]))] ?? AsGiven,
	}

	## Why a tree cannot be written as namespace-well-formed XML 1.0.
	##
	## - `InvalidName(name)`: a prefix or local name that is not an NCName, or
	##   a binding of a reserved prefix, a table that gives one prefix two
	##   namespaces, or `Prefix(p)` bound to `NoNamespace`.
	## - `InvalidChar(code)`: a character XML 1.0 does not allow.
	## - `UnboundNamespace(namespace)`: a name whose prefix is not bound to its
	##   namespace where it is used, or a namespace missing from the table.
	##   `""` stands for no namespace, when a default namespace is in scope.
	## - `DuplicateAttribute(name)`: two attributes with the same namespace
	##   and local name.
	RenderProblem : [InvalidName(Str), InvalidChar(U32), UnboundNamespace(Str), DuplicateAttribute(Str)]

	## Write a document as XML 1.0 text. The output is a function of the
	## tree and `props` alone, and [XmlNs.parse_str] reads it back as the
	## same tree, up to prefixes and bindings with `Table`.
	##
	## Text escapes `&` and `<`, `>` after `]]`, and a carriage return as
	## `&#xD;`. Attribute values are double-quoted and also escape `"`, tab
	## and line feed, so they survive attribute value normalisation. A
	## failure names the problem and the path of written element names from
	## the root to the element it is in.
	##
	## ```roc
	## expect {
	##     doc = XmlNs.parse_str("<p:a xmlns:p='urn:p' k='1 &amp; 2'>x &lt; y</p:a>")?
	##     XmlNs.render(doc, { declaration: Omitted }) == Ok("<p:a xmlns:p=\"urn:p\" k=\"1 &amp; 2\">x &lt; y</p:a>")
	## }
	##
	## expect {
	##     doc = XmlNs.parse_str("<a xmlns='urn:w'><b/></a>")?
	##     XmlNs.render(doc, { declaration: Standalone, prefixes: Table([{ prefix: "w", namespace: "urn:w" }]) }) == Ok("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<w:a xmlns:w=\"urn:w\"><w:b/></w:a>")
	## }
	## ```
	render : XmlNs, RenderProps -> Try(Str, [Unrenderable({ problem : RenderProblem, path : List(Str) })])
	render = |doc, props| {
		start =
			match props.declaration {
				Plain => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n".to_utf8()
				Standalone => "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n".to_utf8()
				Omitted => []
			}
		order = props.attribute_order
		written =
			match props.prefixes {
				AsRead => render_as_read(start, doc.root, order)
				Table(entries) => render_with_table(start, doc.root, order, entries)
			}
		written.map_ok(|bytes| Str.from_utf8(bytes) ?? "")
	}

	## The namespace the `xml` prefix is always bound to.
	xml_namespace : Str
	xml_namespace = "http://www.w3.org/XML/1998/namespace"

	## The namespace of `xmlns` declarations, which no prefix may be bound to.
	xmlns_namespace : Str
	xmlns_namespace = "http://www.w3.org/2000/xmlns/"
}

## The bindings in scope. `entries` holds every binding of the open
## elements in document order, `frames` where each open element's own
## bindings start, and `active` the index of the innermost binding of each
## prefix ("" for the default namespace). Each entry remembers the binding
## it shadows, so leaving an element restores the outer ones.
Scope : {
	entries : List({ prefix : Str, namespace : XmlNs.Namespace, previous : U64 }),
	active : Dict(Str, U64),
	frames : List(U64),
	names : List(XmlNs.Name),
}

Resolved : {
	scope : Scope,
	name : XmlNs.Name,
	bindings : List(XmlNs.Binding),
	attributes : List(XmlNs.Attribute),
}

empty_scope : Scope
empty_scope = { entries: [], active: Dict.empty(), frames: [], names: [] }

# No shadowed binding.
none : U64
none = 18_446_744_073_709_551_615

widen : Xml.ReadError -> XmlNs.ReadError
widen = |error| {
	match error {
		InvalidXml(problem) => InvalidXml(problem)
		LimitExceeded(exceeded) => LimitExceeded(exceeded)
	}
}

# Declares the element's bindings, then resolves its name and attributes.
resolve_start : Scope, Str, List(Xml.Attribute) -> Try(Resolved, XmlNs.Problem)
resolve_start = |scope, name, attributes| {
	var $entries = scope.entries
	var $active = scope.active
	frames = scope.frames.append($entries.len())
	var $bindings = []
	var $attribute_index = 0
	while $attribute_index < attributes.len() {
		attribute = attributes.get($attribute_index) ?? { name: "", value: "" }
		$attribute_index = $attribute_index + 1
		binding = declaration(attribute)?
		match binding {
			Ok(declared) => {
				key = match declared.prefix {
					Prefix(prefix) => prefix
					Default => ""
				}
				previous = $active.get(key) ?? none
				$active = $active.insert(key, $entries.len())
				$entries = $entries.append({ prefix: key, namespace: declared.namespace, previous })
				$bindings = $bindings.append(declared)
			}

			Err(NotADeclaration) => {}
		}
	}
	resolved = resolve_names($entries, $active, name, attributes, False).map_err(problem_of_names)?
	names = scope.names.append(resolved.name)
	Ok({ scope: { entries: $entries, active: $active, frames, names }, name: resolved.name, bindings: $bindings, attributes: resolved.attributes })
}

# Resolves an element's name and its attributes other than declarations,
# with the bindings in scope. The bindings are separate arguments that are
# only read, so a lookup touches no reference counts.
# With `stop`, a declaration among the attributes fails with `Declares`
# instead of being skipped, for a caller whose bindings cannot change.
resolve_names : List(Entry), Dict(Str, U64), Str, List(Xml.Attribute), Bool -> Try({ name : XmlNs.Name, attributes : List(XmlNs.Attribute) }, [Declares, Invalid(XmlNs.Problem)])
resolve_names = |entries, active, name, attributes, stop| {
	# A declaration can bind the element's own prefix, so look for one first.
	if stop and has_declaration(attributes) {
		return Err(Declares)
	}
	element_name = split_qname(name).map_err(|problem| Invalid(problem))?
	if element_name.prefix == "xmlns" {
		return Err(Invalid(ReservedPrefix("xmlns")))
	}
	resolved_name = resolve(element_name, entries, active, True).map_err(|problem| Invalid(problem))?
	if attributes.is_empty() {
		return Ok({ name: resolved_name, attributes: [] })
	}
	var $resolved = List.with_capacity(attributes.len())
	var $prefixed = 0
	var $attribute_index = 0
	while $attribute_index < attributes.len() {
		attribute = attributes.get($attribute_index) ?? { name: "", value: "" }
		$attribute_index = $attribute_index + 1
		# With `stop` there are none to skip.
		if stop or !is_declaration(attribute.name) {
			attribute_name = split_qname(attribute.name).map_err(|problem| Invalid(problem))?
			if attribute_name.prefix != "" {
				$prefixed = $prefixed + 1
			}
			$resolved = $resolved.append({ name: resolve(attribute_name, entries, active, False).map_err(|problem| Invalid(problem))?, value: attribute.value })
		}
	}
	if $prefixed > 1 {
		check_unique($resolved).map_err(|problem| Invalid(problem))?
	}
	Ok({ name: resolved_name, attributes: $resolved })
}

# The problem of a resolution that cannot stop at a declaration.
problem_of_names : [Declares, Invalid(XmlNs.Problem)] -> XmlNs.Problem
problem_of_names = |error| {
	match error {
		Invalid(problem) => problem
		Declares => crash "resolve_names stopped at a declaration it was told to skip"
	}
}

Entry : { prefix : Str, namespace : XmlNs.Namespace, previous : U64 }

has_declaration : List(Xml.Attribute) -> Bool
has_declaration = |attributes| attributes.any(|attribute| is_declaration(attribute.name))

# Drops the bindings from `mark` on, restoring the ones they shadowed.
unbind : List(Entry), Dict(Str, U64), U64 -> { entries : List(Entry), active : Dict(Str, U64) }
unbind = |entries, active, mark| {
	var $entries = entries
	var $active = active
	while $entries.len() > mark {
		match $entries.last() {
			Ok(entry) => {
				$active = if (entry.previous == none) $active.remove(entry.prefix) else $active.insert(entry.prefix, entry.previous)
			}

			Err(_) => {}
		}
		$entries = $entries.drop_last(1)
	}
	{ entries: $entries, active: $active }
}

# Leaves the innermost element: its bindings go out of scope, and the ones
# they shadowed are restored.
pop_scope : Scope -> Scope
pop_scope = |scope| {
	first = scope.frames.last() ?? 0
	frames = scope.frames.drop_last(1)
	names = scope.names.drop_last(1)
	var $entries = scope.entries
	var $active = scope.active
	while $entries.len() > first {
		match $entries.last() {
			Ok(entry) => {
				$active = if (entry.previous == none) $active.remove(entry.prefix) else $active.insert(entry.prefix, entry.previous)
			}

			Err(_) => {}
		}
		$entries = $entries.drop_last(1)
	}
	{ entries: $entries, active: $active, frames, names }
}

is_declaration : Str -> Bool
is_declaration = |name| name.starts_with("xmlns") and (name.count_utf8_bytes() == 5 or name.to_utf8().get(5) == Ok(':'))

# The binding an attribute declares, with the rules for reserved prefixes
# and namespaces (https://www.w3.org/TR/xml-names/#xmlReserved).
declaration : Xml.Attribute -> Try(Try(XmlNs.Binding, [NotADeclaration]), XmlNs.Problem)
declaration = |attribute| {
	value = attribute.value
	if attribute.name == "xmlns" {
		if value == XmlNs.xml_namespace or value == XmlNs.xmlns_namespace {
			return Err(ReservedNamespace(value))
		}
		return Ok(Ok({ prefix: Default, namespace: if (value == "") NoNamespace else Uri(value) }))
	}
	if !attribute.name.starts_with("xmlns:") {
		return Ok(Err(NotADeclaration))
	}
	qname = split_qname(attribute.name)?
	prefix = qname.local
	if prefix == "xmlns" {
		Err(ReservedPrefix("xmlns"))
	} else if prefix == "xml" {
		if (value == XmlNs.xml_namespace) Ok(Ok({ prefix: Prefix(prefix), namespace: Uri(value) })) else Err(ReservedPrefix("xml"))
	} else if value == XmlNs.xml_namespace or value == XmlNs.xmlns_namespace {
		Err(ReservedNamespace(value))
	} else if value == "" {
		Err(EmptyNamespace(prefix))
	} else {
		Ok(Ok({ prefix: Prefix(prefix), namespace: Uri(value) }))
	}
}

# A name split at its colon; `prefix` is "" for an unprefixed name. A QName
# has at most one colon, and both parts are NCNames
# (https://www.w3.org/TR/xml-names/#ns-qualnames). The input is already an
# XML Name, so each part only needs a check of its first character.
split_qname : Str -> Try({ prefix : Str, local : Str, written : Str }, XmlNs.Problem)
split_qname = |name| {
	bytes = name.to_utf8()
	var $colon = none
	var $index = 0
	while $index < bytes.len() {
		if bytes.get($index) == Ok(':') {
			if $colon != none {
				return Err(NotAQName(name))
			}
			$colon = $index
		}
		$index = $index + 1
	}
	if $colon == none {
		return Ok({ prefix: "", local: name, written: name })
	}
	if $colon == 0 or $colon + 1 == bytes.len() or !starts_ncname(bytes, $colon + 1) {
		return Err(NotAQName(name))
	}
	prefix = name.drop_last_bytes(bytes.len() - $colon) ?? ""
	local = name.drop_first_bytes($colon + 1) ?? ""
	Ok({ prefix, local, written: name })
}

# Whether the name character at `pos` may start an NCName: anything but a
# digit, '-', '.', U+00B7, a combining mark U+0300-U+036F, or U+203F-U+2040.
starts_ncname : List(U8), U64 -> Bool
starts_ncname = |bytes, pos| {
	byte = bytes.get(pos) ?? 0
	if byte < 0x80 {
		!(byte >= '0' and byte <= '9') and byte != '-' and byte != '.'
	} else {
		second = bytes.get(pos + 1) ?? 0
		third = bytes.get(pos + 2) ?? 0
		middle_dot = byte == 0xC2 and second == 0xB7
		combining = (byte == 0xCC) or (byte == 0xCD and second < 0xB0)
		tie = byte == 0xE2 and second == 0x80 and third == 0xBF or (byte == 0xE2 and second == 0x81 and third == 0x80)
		!middle_dot and !combining and !tie
	}
}

resolve : { prefix : Str, local : Str, written : Str }, List({ prefix : Str, namespace : XmlNs.Namespace, previous : U64 }), Dict(Str, U64), Bool -> Try(XmlNs.Name, XmlNs.Problem)
resolve = |qname, entries, active, is_element| {
	if qname.prefix == "" {
		namespace =
			if is_element {
				match active.get("") {
					Ok(index) => entries.get(index).map_ok(|entry| entry.namespace) ?? NoNamespace
					Err(_) => NoNamespace
				}
			} else {
				NoNamespace
			}
		return Ok({ namespace, local: qname.local, prefix: Unprefixed })
	}
	match active.get(qname.prefix) {
		Ok(index) =>
			match entries.get(index) {
				Ok(entry) => Ok({ namespace: entry.namespace, local: qname.local, prefix: Prefix(qname.prefix) })
				Err(_) => Err(UndeclaredPrefix(qname.prefix))
			}

		Err(_) =>
			if qname.prefix == "xml" {
				Ok({ namespace: Uri(XmlNs.xml_namespace), local: qname.local, prefix: Prefix("xml") })
			} else {
				Err(UndeclaredPrefix(qname.prefix))
			}
	}
}

# No two attributes may share a namespace and local name
# (https://www.w3.org/TR/xml-names/#uniqAttrs). Few attributes are compared
# pairwise; many are checked with a set, so the check stays linear.
check_unique : List(XmlNs.Attribute) -> Try({}, XmlNs.Problem)
check_unique = |attributes| {
	if attributes.len() <= 16 {
		var $index = 0
		while $index < attributes.len() {
			match attributes.get($index) {
				Ok(first) => {
					var $other = $index + 1
					while $other < attributes.len() {
						match attributes.get($other) {
							Ok(second) =>
								if first.name.local == second.name.local and first.name.namespace == second.name.namespace {
									return Err(DuplicateAttribute(second.name))
								}

							Err(_) => {}
						}
						$other = $other + 1
					}
				}

				Err(_) => {}
			}
			$index = $index + 1
		}
		Ok({})
	} else {
		var $seen = Set.empty()
		for attribute in attributes {
			# U+0000 is not an XML character, so it cannot occur in either part.
			key = match attribute.name.namespace {
				Uri(uri) => "${uri}\u(0)${attribute.name.local}"
				NoNamespace => "\u(0)${attribute.name.local}"
			}
			if $seen.contains(key) {
				return Err(DuplicateAttribute(attribute.name))
			}
			$seen = $seen.insert(key)
		}
		Ok({})
	}
}

# Builds the element whose start was the reader's most recent event with
# Xml.Reader.element_with, so the subtree is read at the speed of the plain
# tree parser, with the bindings as the context. Bindings are declared and
# restored only at elements that declare some, so the common element costs a
# lookup per name and nothing more.
build_element : XmlNs.Reader, { name : XmlNs.Name, bindings : List(XmlNs.Binding), attributes : List(XmlNs.Attribute) } -> [Element(XmlNs.Node, XmlNs.Reader), Failed(XmlNs.ReadError), NotAfterStart]
build_element = |reader, start| {
	scope = reader.scope
	outer_frames = scope.frames.drop_last(1)
	outer_names = scope.names.drop_last(1)
	mark = scope.frames.last() ?? 0
	entries = scope.entries
	active = scope.active
	# Most subtrees declare nothing: an OOXML part declares every namespace
	# on its root. They are built with the bindings in force when the subtree
	# started and no context at all, which carrying the bindings through every
	# start and end would cost a fifth of the parse. The first declaration
	# stops that build, and the subtree is built again with the bindings as
	# the context, so a document pays at most one extra partial pass.
	fast = reader.inner.element_with({
		context: {},
		first: start,
		start: |{}, name, attributes| {
			names = resolve_names(entries, active, name, attributes, True)?
			Ok({ context: {}, open: { name: names.name, bindings: [], attributes: names.attributes } })
		},
		text: |text| Text(text),
		end: |{}, open, children| {
			node : XmlNs.Node
			node = Element({ name: open.name, bindings: open.bindings, attributes: open.attributes, children })
			{ context: {}, node }
		},
	})
	match fast {
		Element(node, _, rest) => {
			# The element's own declarations go out of scope after it.
			bound = if (entries.len() > mark) unbind(entries, active, mark) else { entries, active }
			Element(node, { inner: rest, scope: { entries: bound.entries, active: bound.active, frames: outer_frames, names: outer_names }, last_start: NotStarted })
		}

		Rejected(Declares, _) => build_scoped(reader, start)
		Rejected(Invalid(problem), at) => Failed(InvalidNamespace({ problem, line: at.line, column: at.column }))
		Failed(error) => Failed(widen(error))
		NotAfterStart => NotAfterStart
	}
}

# `build_element` for a subtree that declares namespaces: the bindings in
# scope are the context, declared and restored as elements start and end.
build_scoped : XmlNs.Reader, { name : XmlNs.Name, bindings : List(XmlNs.Binding), attributes : List(XmlNs.Attribute) } -> [Element(XmlNs.Node, XmlNs.Reader), Failed(XmlNs.ReadError), NotAfterStart]
build_scoped = |reader, start| {
	scope = reader.scope
	outer_frames = scope.frames.drop_last(1)
	outer_names = scope.names.drop_last(1)
	mark = scope.frames.last() ?? 0
	built = reader.inner.element_with({
		context: { entries: scope.entries, active: scope.active },
		first: { name: start.name, bindings: start.bindings, attributes: start.attributes, mark },
		start: |bound, name, attributes| {
			at_mark = bound.entries.len()
			if has_declaration(attributes) {
				declared = resolve_start({ entries: bound.entries, active: bound.active, frames: [], names: [] }, name, attributes)?
				Ok({ context: { entries: declared.scope.entries, active: declared.scope.active }, open: { name: declared.name, bindings: declared.bindings, attributes: declared.attributes, mark: at_mark } })
			} else {
				names = resolve_names(bound.entries, bound.active, name, attributes, False).map_err(problem_of_names)?
				Ok({ context: bound, open: { name: names.name, bindings: [], attributes: names.attributes, mark: at_mark } })
			}
		},
		text: |text| Text(text),
		end: |bound, open, children| {
			node : XmlNs.Node
			node = Element({ name: open.name, bindings: open.bindings, attributes: open.attributes, children })
			context = if (bound.entries.len() > open.mark) unbind(bound.entries, bound.active, open.mark) else bound
			{ context, node }
		},
	})
	match built {
		Element(node, bound, rest) => Element(node, { inner: rest, scope: { entries: bound.entries, active: bound.active, frames: outer_frames, names: outer_names }, last_start: NotStarted })
		Failed(error) => Failed(widen(error))
		Rejected(problem, at) => Failed(InvalidNamespace({ problem, line: at.line, column: at.column }))
		NotAfterStart => NotAfterStart
	}
}

# Writing

RenderError : [Unrenderable({ problem : XmlNs.RenderProblem, path : List(Str) })]

AttributeOrder : [AsGiven, SortedBy((XmlNs.Name, XmlNs.Name -> [Before, Same, After]))]

# Bindings in scope while writing: as `Scope`, without names and frames.
Bound : {
	entries : List({ prefix : Str, namespace : XmlNs.Namespace, previous : U64 }),
	active : Dict(Str, U64),
}

Work : [Enter(XmlNs.Node), Leave({ written : Str, mark : U64 })]

unrenderable : XmlNs.RenderProblem, List(Str) -> Try(_, RenderError)
unrenderable = |problem, path| Err(Unrenderable({ problem, path }))

bind : Bound, Str, XmlNs.Namespace -> Bound
bind = |bound, key, namespace| {
	previous = bound.active.get(key) ?? none
	index = bound.entries.len()
	{ entries: bound.entries.append({ prefix: key, namespace, previous }), active: bound.active.insert(key, index) }
}

unbind_to : Bound, U64 -> Bound
unbind_to = |bound, len| {
	var $entries = bound.entries
	var $active = bound.active
	while $entries.len() > len {
		match $entries.last() {
			Ok(entry) => {
				$active = if (entry.previous == none) $active.remove(entry.prefix) else $active.insert(entry.prefix, entry.previous)
			}

			Err(_) => {}
		}
		$entries = $entries.drop_last(1)
	}
	{ entries: $entries, active: $active }
}

# The namespace bound to a prefix ("" for the default) where it is used.
bound_namespace : Bound, Str -> XmlNs.Namespace
bound_namespace = |bound, key| {
	match bound.active.get(key) {
		Ok(index) => bound.entries.get(index).map_ok(|entry| entry.namespace) ?? NoNamespace
		Err(_) => if (key == "xml") Uri(XmlNs.xml_namespace) else NoNamespace
	}
}

namespace_str : XmlNs.Namespace -> Str
namespace_str = |namespace| {
	match namespace {
		Uri(uri) => uri
		NoNamespace => ""
	}
}

ordered : List(XmlNs.Attribute), AttributeOrder -> List(XmlNs.Attribute)
ordered = |attributes, order| {
	match order {
		AsGiven => attributes
		SortedBy(compare) => attributes.sort_with(|a, b| compare(a.name, b.name))
	}
}

# Each element's own prefixes and bindings, checked against the scope.
render_as_read : List(U8), XmlNs.Node, AttributeOrder -> Try(List(U8), RenderError)
render_as_read = |start, root, order| {
	var $out = start
	var $bound = { entries: [], active: Dict.empty() }
	var $path = []
	var $work = [Enter(root)]
	while !$work.is_empty() {
		item = $work.last() ?? Leave({ written: "", mark: 0 })
		$work = $work.drop_last(1)
		match item {
			Leave(leaving) => {
				$out = $out.append('<').append('/').concat(leaving.written.to_utf8()).append('>')
				$bound = unbind_to($bound, leaving.mark)
				$path = $path.drop_last(1)
			}

			Enter(Text(text)) => {
				$out = escape_text($out, text).map_err(|problem| Unrenderable({ problem, path: $path }))?
			}

			Enter(Element(element)) => {
				written = written_name(element.name.prefix, element.name.local)
				$path = $path.append(written)
				check_name(element.name.prefix, element.name.local, $path)?
				mark = $bound.entries.len()
				$out = $out.append('<').concat(written.to_utf8())
				for binding in element.bindings {
					declared = check_binding(binding, $path)?
					$bound = bind($bound, declared.key, binding.namespace)
					$out = write_attribute($out, declared.attribute, namespace_str(binding.namespace)).map_err(|problem| Unrenderable({ problem, path: $path }))?
				}
				key = match element.name.prefix {
					Prefix(prefix) => prefix
					Unprefixed => ""
				}
				if bound_namespace($bound, key) != element.name.namespace {
					return unrenderable(UnboundNamespace(namespace_str(element.name.namespace)), $path)
				}
				attributes = ordered(element.attributes, order)
				if attributes.len() > 1 {
					check_unique(attributes).map_err(|_| Unrenderable({ problem: DuplicateAttribute(written_name_of(attributes)), path: $path }))?
				}
				for attribute in attributes {
					check_name(attribute.name.prefix, attribute.name.local, $path)?
					in_scope =
						match attribute.name.prefix {
							Prefix(prefix) => bound_namespace($bound, prefix) == attribute.name.namespace and attribute.name.namespace != NoNamespace
							Unprefixed => attribute.name.namespace == NoNamespace
						}
					if !in_scope {
						return unrenderable(UnboundNamespace(namespace_str(attribute.name.namespace)), $path)
					}
					$out = write_attribute($out, written_name(attribute.name.prefix, attribute.name.local), attribute.value).map_err(|problem| Unrenderable({ problem, path: $path }))?
				}
				if element.children.is_empty() {
					$out = $out.append('/').append('>')
					$bound = unbind_to($bound, mark)
					$path = $path.drop_last(1)
				} else {
					$out = $out.append('>')
					$work = $work.append(Leave({ written, mark }))
					var $index = element.children.len()
					while $index > 0 {
						$index = $index - 1
						match element.children.get($index) {
							Ok(child) => {
								$work = $work.append(Enter(child))
							}

							Err(_) => {}
						}
					}
				}
			}
		}
	}
	Ok($out)
}

# Prefixes from a table, declared on the root. `$default` is the default
# namespace in effect, changed with `xmlns` where an unprefixed element needs
# another; `$defaults` remembers it for each open element.
render_with_table : List(U8), XmlNs.Node, AttributeOrder, List({ prefix : Str, namespace : Str }) -> Try(List(U8), RenderError)
render_with_table = |start, root, order, entries| {
	root_path = match root {
		Element(element) => [written_name(element.name.prefix, element.name.local)]
		Text(_) => []
	}
	var $prefix_of = Dict.empty()
	var $namespace_of = Dict.empty()
	for entry in entries {
		attribute = if (entry.prefix == "") "xmlns" else "xmlns:${entry.prefix}"
		reserved = entry.prefix == "xmlns" or entry.namespace == XmlNs.xmlns_namespace or ((entry.prefix == "xml") != (entry.namespace == XmlNs.xml_namespace)) or entry.namespace == ""
		if reserved or (entry.prefix != "" and !is_ncname(entry.prefix)) {
			return unrenderable(InvalidName(attribute), root_path)
		}
		match $namespace_of.get(entry.prefix) {
			Ok(other) =>
				if other != entry.namespace {
					return unrenderable(InvalidName(attribute), root_path)
				}

			Err(_) => {
				$namespace_of = $namespace_of.insert(entry.prefix, entry.namespace)
			}
		}
		if !$prefix_of.contains(entry.namespace) {
			$prefix_of = $prefix_of.insert(entry.namespace, entry.prefix)
		}
	}
	used = used_namespaces(root)
	var $out = start
	# The default namespace is declared where an element first needs it, as
	# the root may be in no namespace.
	var $declarations = []
	for entry in entries {
		if used.contains(entry.namespace) and $prefix_of.get(entry.namespace) == Ok(entry.prefix) and entry.prefix != "xml" and entry.prefix != "" {
			$declarations = $declarations.append({ name: "xmlns:${entry.prefix}", value: entry.namespace })
		}
	}
	var $default = NoNamespace
	var $defaults = []
	var $path = []
	var $work = [Enter(root)]
	var $first = True
	while !$work.is_empty() {
		item = $work.last() ?? Leave({ written: "", mark: 0 })
		$work = $work.drop_last(1)
		match item {
			Leave(leaving) => {
				$out = $out.append('<').append('/').concat(leaving.written.to_utf8()).append('>')
				$default = $defaults.last() ?? NoNamespace
				$defaults = $defaults.drop_last(1)
				$path = $path.drop_last(1)
			}

			Enter(Text(text)) => {
				$out = escape_text($out, text).map_err(|problem| Unrenderable({ problem, path: $path }))?
			}

			Enter(Element(element)) => {
				prefix = table_prefix($prefix_of, element.name.namespace, True)
				$path = $path.append(written_name(element.name.prefix, element.name.local))
				local_ok = is_ncname(element.name.local)
				if !local_ok {
					return unrenderable(InvalidName(element.name.local), $path)
				}
				written =
					match prefix {
						Ok("") => element.name.local
						Ok(p) => "${p}:${element.name.local}"
						Err(_) => return unrenderable(UnboundNamespace(namespace_str(element.name.namespace)), $path)
					}
				$out = $out.append('<').concat(written.to_utf8())
				outer_default = $default
				if prefix == Ok("") and $default != element.name.namespace {
					$out = write_attribute($out, "xmlns", namespace_str(element.name.namespace)).map_err(|problem| Unrenderable({ problem, path: $path }))?
					$default = element.name.namespace
				}
				if $first {
					for declared in $declarations {
						$out = write_attribute($out, declared.name, declared.value).map_err(|problem| Unrenderable({ problem, path: $path }))?
					}
					$first = False
				}
				attributes = ordered(element.attributes, order)
				if attributes.len() > 1 {
					check_unique(attributes).map_err(|_| Unrenderable({ problem: DuplicateAttribute(written_name_of(attributes)), path: $path }))?
				}
				for attribute in attributes {
					if !is_ncname(attribute.name.local) {
						return unrenderable(InvalidName(attribute.name.local), $path)
					}
					attribute_name =
						match table_prefix($prefix_of, attribute.name.namespace, False) {
							Ok("") => attribute.name.local
							Ok(p) => "${p}:${attribute.name.local}"
							Err(_) => return unrenderable(UnboundNamespace(namespace_str(attribute.name.namespace)), $path)
						}
					$out = write_attribute($out, attribute_name, attribute.value).map_err(|problem| Unrenderable({ problem, path: $path }))?
				}
				if element.children.is_empty() {
					$out = $out.append('/').append('>')
					$default = outer_default
					$path = $path.drop_last(1)
				} else {
					$out = $out.append('>')
					$defaults = $defaults.append(outer_default)
					$work = $work.append(Leave({ written, mark: 0 }))
					var $index = element.children.len()
					while $index > 0 {
						$index = $index - 1
						match element.children.get($index) {
							Ok(child) => {
								$work = $work.append(Enter(child))
							}

							Err(_) => {}
						}
					}
				}
			}
		}
	}
	Ok($out)
}

# The prefix a namespace is written with: "" for an unprefixed element name
# (no namespace, or the table's default) or an attribute in no namespace.
table_prefix : Dict(Str, Str), XmlNs.Namespace, Bool -> Try(Str, [Unbound])
table_prefix = |prefix_of, namespace, is_element| {
	match namespace {
		NoNamespace => Ok("")
		Uri(uri) =>
			if uri == XmlNs.xml_namespace {
				Ok("xml")
			} else {
				match prefix_of.get(uri) {
					Ok("") => if (is_element) Ok("") else Err(Unbound)
					Ok(prefix) => Ok(prefix)
					Err(_) => Err(Unbound)
				}
			}
	}
}

# Every namespace an element or attribute name of the tree is in.
used_namespaces : XmlNs.Node -> Set(Str)
used_namespaces = |root| {
	var $used = Set.empty()
	var $pending = [root]
	while !$pending.is_empty() {
		match $pending.last() {
			Ok(Element(element)) => {
				$pending = $pending.drop_last(1)
				$used = match element.name.namespace {
					Uri(uri) => $used.insert(uri)
					NoNamespace => $used
				}
				for attribute in element.attributes {
					$used = match attribute.name.namespace {
						Uri(uri) => $used.insert(uri)
						NoNamespace => $used
					}
				}
				$pending = $pending.concat(element.children)
			}

			Ok(Text(_)) => {
				$pending = $pending.drop_last(1)
			}

			Err(_) => {
				$pending = []
			}
		}
	}
	$used
}

written_name : [Prefix(Str), Unprefixed], Str -> Str
written_name = |prefix, local| {
	match prefix {
		Prefix(p) => "${p}:${local}"
		Unprefixed => local
	}
}

# The written name of the second attribute that duplicates an earlier one.
written_name_of : List(XmlNs.Attribute) -> Str
written_name_of = |attributes| {
	match check_unique(attributes) {
		Err(DuplicateAttribute(duplicate)) => written_name(duplicate.prefix, duplicate.local)
		_ => ""
	}
}

check_name : [Prefix(Str), Unprefixed], Str, List(Str) -> Try({}, RenderError)
check_name = |prefix, local, path| {
	match prefix {
		Prefix(p) if !is_ncname(p) => unrenderable(InvalidName(p), path)
		_ => if (is_ncname(local)) Ok({}) else unrenderable(InvalidName(local), path)
	}
}

# A binding's attribute name and scope key, with the reserved-prefix rules.
check_binding : XmlNs.Binding, List(Str) -> Try({ key : Str, attribute : Str }, RenderError)
check_binding = |binding, path| {
	match binding.prefix {
		Default =>
			if binding.namespace == Uri(XmlNs.xml_namespace) or binding.namespace == Uri(XmlNs.xmlns_namespace) {
				unrenderable(InvalidName("xmlns"), path)
			} else {
				Ok({ key: "", attribute: "xmlns" })
			}

		Prefix(prefix) => {
			attribute = "xmlns:${prefix}"
			reserved = prefix == "xmlns" or binding.namespace == NoNamespace or binding.namespace == Uri(XmlNs.xmlns_namespace) or ((prefix == "xml") != (binding.namespace == Uri(XmlNs.xml_namespace)))
			if !is_ncname(prefix) or reserved {
				unrenderable(InvalidName(attribute), path)
			} else {
				Ok({ key: prefix, attribute })
			}
		}
	}
}

write_attribute : List(U8), Str, Str -> Try(List(U8), XmlNs.RenderProblem)
write_attribute = |out, attribute_name, value| {
	escaped = escape_value(out.append(' ').concat(attribute_name.to_utf8()).append('=').append('"'), value)?
	Ok(escaped.append('"'))
}

# See https://www.w3.org/TR/xml/#syntax. Runs that need no escaping are
# copied whole.
escape_text : List(U8), Str -> Try(List(U8), XmlNs.RenderProblem)
escape_text = |out, text| {
	bytes = text.to_utf8()
	var $out = out
	var $run = 0
	var $pos = 0
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		replacement =
			if byte == '&' {
				Ok("&amp;")
			} else if byte == '<' {
				Ok("&lt;")
			} else if byte == '>' and $pos >= 2 and bytes.get($pos - 1) == Ok(']') and bytes.get($pos - 2) == Ok(']') {
				Ok("&gt;")
			} else if byte == '\r' {
				Ok("&#xD;")
			} else {
				Err(Kept)
			}
		match replacement {
			Ok(escaped) => {
				$out = $out.concat(bytes.sublist({ start: $run, len: $pos - $run })).concat(escaped.to_utf8())
				$pos = $pos + 1
				$run = $pos
			}

			Err(Kept) => {
				$pos = check_scalar(bytes, $pos)?
			}
		}
	}
	Ok($out.concat(bytes.sublist({ start: $run, len: $pos - $run })))
}

# See https://www.w3.org/TR/xml/#AVNormalize: whitespace characters are
# written as references, so normalisation keeps them.
escape_value : List(U8), Str -> Try(List(U8), XmlNs.RenderProblem)
escape_value = |out, value| {
	bytes = value.to_utf8()
	var $out = out
	var $run = 0
	var $pos = 0
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		replacement =
			if byte == '&' {
				Ok("&amp;")
			} else if byte == '<' {
				Ok("&lt;")
			} else if byte == '"' {
				Ok("&quot;")
			} else if byte == '\t' {
				Ok("&#x9;")
			} else if byte == '\n' {
				Ok("&#xA;")
			} else if byte == '\r' {
				Ok("&#xD;")
			} else {
				Err(Kept)
			}
		match replacement {
			Ok(escaped) => {
				$out = $out.concat(bytes.sublist({ start: $run, len: $pos - $run })).concat(escaped.to_utf8())
				$pos = $pos + 1
				$run = $pos
			}

			Err(Kept) => {
				$pos = check_scalar(bytes, $pos)?
			}
		}
	}
	Ok($out.concat(bytes.sublist({ start: $run, len: $pos - $run })))
}

# The position after the character at `pos` of valid UTF-8, if XML 1.0
# allows it (https://www.w3.org/TR/xml/#NT-Char).
check_scalar : List(U8), U64 -> Try(U64, XmlNs.RenderProblem)
check_scalar = |bytes, pos| {
	scalar = scalar_at(bytes, pos)
	c = scalar.code
	allowed = c == 0x9 or c == 0xA or c == 0xD or (c >= 0x20 and c <= 0xD7FF) or (c >= 0xE000 and c <= 0xFFFD) or c >= 0x10000
	if (allowed) Ok(pos + scalar.len) else Err(InvalidChar(c))
}

# The scalar starting at `pos` of valid UTF-8.
scalar_at : List(U8), U64 -> { code : U32, len : U64 }
scalar_at = |bytes, pos| {
	b0 = U8.to_u32(bytes.get(pos) ?? 0)
	cont = |offset| U8.to_u32((bytes.get(pos + offset) ?? 0x80) - 0x80)
	if b0 < 0x80 {
		{ code: b0, len: 1 }
	} else if b0 < 0xE0 {
		{ code: (b0 - 0xC0) * 64 + cont(1), len: 2 }
	} else if b0 < 0xF0 {
		{ code: (b0 - 0xE0) * 4096 + cont(1) * 64 + cont(2), len: 3 }
	} else {
		{ code: (b0 - 0xF0) * 262144 + cont(1) * 4096 + cont(2) * 64 + cont(3), len: 4 }
	}
}

# See https://www.w3.org/TR/xml-names/#NT-NCName
is_ncname : Str -> Bool
is_ncname = |text| {
	bytes = text.to_utf8()
	if bytes.is_empty() {
		return False
	}
	var $pos = 0
	while $pos < bytes.len() {
		scalar = scalar_at(bytes, $pos)
		valid = if ($pos == 0) is_name_start(scalar.code) else is_name_start(scalar.code) or is_name_rest(scalar.code)
		if !valid {
			return False
		}
		$pos = $pos + scalar.len
	}
	True
}

# See https://www.w3.org/TR/xml/#NT-NameStartChar, without ':'.
is_name_start : U32 -> Bool
is_name_start = |c| {
	(c >= 'a' and c <= 'z')
		or (c >= 'A' and c <= 'Z')
		or c == '_'
		or (c >= 0xC0 and c <= 0xD6)
		or (c >= 0xD8 and c <= 0xF6)
		or (c >= 0xF8 and c <= 0x2FF)
		or (c >= 0x370 and c <= 0x37D)
		or (c >= 0x37F and c <= 0x1FFF)
		or (c >= 0x200C and c <= 0x200D)
		or (c >= 0x2070 and c <= 0x218F)
		or (c >= 0x2C00 and c <= 0x2FEF)
		or (c >= 0x3001 and c <= 0xD7FF)
		or (c >= 0xF900 and c <= 0xFDCF)
		or (c >= 0xFDF0 and c <= 0xFFFD)
		or (c >= 0x10000 and c <= 0xEFFFF)
}

# See https://www.w3.org/TR/xml/#NT-NameChar: the characters a name may have
# after its first.
is_name_rest : U32 -> Bool
is_name_rest = |c| c == '-' or c == '.' or (c >= '0' and c <= '9') or c == 0xB7 or (c >= 0x300 and c <= 0x36F) or (c >= 0x203F and c <= 0x2040)

problem_of : Str -> Try(XmlNs.Problem, [Parsed, OtherError])
problem_of = |input| {
	match XmlNs.parse_str(input) {
		Ok(_) => Err(Parsed)
		Err(InvalidNamespace(problem)) => Ok(problem.problem)
		Err(InvalidXml(_)) => Err(OtherError)
	}
}

root_of : Str -> Try(XmlNs.Node, [InvalidXml(Xml.Error), InvalidNamespace(XmlNs.NamespaceProblem)])
root_of = |input| XmlNs.parse_str(input).map_ok(|doc| doc.root)

name : Str, Str -> XmlNs.Name
name = |uri, local| { namespace: Uri(uri), local, prefix: Unprefixed }

# Scope: a binding covers its element and descendants, an inner binding
# shadows an outer one, and xmlns="" removes the default namespace.
expect {
	match root_of("<a xmlns='urn:1' xmlns:p='urn:p'><b xmlns='urn:2'><p:c/><d xmlns=''/></b><e/></a>") {
		Ok(Element(a)) =>
			match a.children {
				[Element(b), Element(e)] =>
					match b.children {
						[Element(c), Element(d)] =>
							a.name == name("urn:1", "a")
								and a.bindings == [{ prefix: Default, namespace: Uri("urn:1") }, { prefix: Prefix("p"), namespace: Uri("urn:p") }]
								and b.name == name("urn:2", "b")
								and c.name == { namespace: Uri("urn:p"), local: "c", prefix: Prefix("p") }
								and d.name == { namespace: NoNamespace, local: "d", prefix: Unprefixed }
								and d.bindings == [{ prefix: Default, namespace: NoNamespace }]
								and e.name == name("urn:1", "e")

						_ => False
					}

				_ => False
			}

		_ => False
	}
}

# A prefix goes out of scope after its element.
expect problem_of("<a><b xmlns:p='urn:p'><p:c/></b><p:d/></a>") == Ok(UndeclaredPrefix("p"))
expect problem_of("<a xmlns:p='urn:p'><p:b><p:c xmlns:p='urn:q'/></p:b></a>") == Err(Parsed)

# Unprefixed attributes are in no namespace, whatever the default.
expect {
	match root_of("<a xmlns='urn:d' xmlns:x='urn:x' id='1' x:id='2'/>") {
		Ok(Element(a)) => a.attributes == [{ name: { namespace: NoNamespace, local: "id", prefix: Unprefixed }, value: "1" }, { name: { namespace: Uri("urn:x"), local: "id", prefix: Prefix("x") }, value: "2" }]
		_ => False
	}
}

# The xml prefix is always bound, and may be declared only to its namespace.
expect root_of("<a xml:lang='en'/>").map_ok(|root| root.attribute(Uri(XmlNs.xml_namespace), "lang")) == Ok(Ok("en"))
expect problem_of("<a xmlns:xml='http://www.w3.org/XML/1998/namespace'/>") == Err(Parsed)
expect problem_of("<a xmlns:xml='urn:other'/>") == Ok(ReservedPrefix("xml"))
expect problem_of("<a xmlns:x='http://www.w3.org/XML/1998/namespace'/>") == Ok(ReservedNamespace("http://www.w3.org/XML/1998/namespace"))
expect problem_of("<a xmlns='http://www.w3.org/XML/1998/namespace'/>") == Ok(ReservedNamespace("http://www.w3.org/XML/1998/namespace"))

# The xmlns prefix and namespace are never declared, and xmlns is not an
# element prefix.
expect problem_of("<a xmlns:xmlns='urn:x'/>") == Ok(ReservedPrefix("xmlns"))
expect problem_of("<a xmlns:x='http://www.w3.org/2000/xmlns/'/>") == Ok(ReservedNamespace("http://www.w3.org/2000/xmlns/"))
expect problem_of("<xmlns:a/>") == Ok(ReservedPrefix("xmlns"))

# XML 1.0 does not allow undeclaring a prefix.
expect problem_of("<a xmlns:p=''/>") == Ok(EmptyNamespace("p"))

# Undeclared prefixes on elements and attributes, located at their tag.
expect XmlNs.parse_str("<a>\n  <p:b/></a>") == Err(InvalidNamespace({ problem: UndeclaredPrefix("p"), line: 2, column: 3 }))
expect problem_of("<a p:x='1'/>") == Ok(UndeclaredPrefix("p"))

# Duplicate expanded attribute names, including with many attributes.
expect problem_of("<a xmlns:a='urn:x' xmlns:b='urn:x' a:x='1' b:x='2'/>") == Ok(DuplicateAttribute({ namespace: Uri("urn:x"), local: "x", prefix: Prefix("b") }))
expect problem_of("<a xmlns:a='urn:x' xmlns:b='urn:y' a:x='1' b:x='2'/>") == Err(Parsed)
expect problem_of("<a xmlns:a='urn:x' x='1' a:x='2'/>") == Err(Parsed)
expect {
	many = Str.join_with(List.repeat(0, 40).map_with_index(|_, index| " a:n${index.to_str()}='v'"), "")
	problem_of("<e xmlns:a='urn:x' xmlns:b='urn:x'${many} b:n39='w'/>") == Ok(DuplicateAttribute({ namespace: Uri("urn:x"), local: "n39", prefix: Prefix("b") })) and problem_of("<e xmlns:a='urn:x'${many}/>") == Err(Parsed)
}

# Malformed names: more than one colon, an empty part, a part that is not
# an NCName, and a colon in a processing instruction target.
expect problem_of("<a:b:c/>") == Ok(NotAQName("a:b:c"))
expect problem_of("<:a/>") == Ok(NotAQName(":a"))
expect problem_of("<a: xmlns:a='urn:a'/>") == Ok(NotAQName("a:"))
expect problem_of("<a xmlns:a='urn:a'><a:1b/></a>") == Ok(NotAQName("a:1b"))
expect problem_of("<a xmlns:a='urn:a' a:-x='1'/>") == Ok(NotAQName("a:-x"))
expect problem_of("<a xmlns:a:b='urn:a'/>") == Ok(NotAQName("xmlns:a:b"))
expect problem_of("<a><?p:i?></a>") == Err(OtherError)

# Namespace names are compared as strings.
expect problem_of("<a xmlns:a='urn:X' xmlns:b='urn:x' a:x='1' b:x='2'/>") == Err(Parsed)

# Text, CDATA and references read as in Xml, and the tree has Xml's shape.
expect {
	input = "<?xml version='1.0'?><f:a xmlns:f='urn:f'>x &amp; <![CDATA[y]]><f:b k='v'/></f:a>"
	match (XmlNs.parse_str(input), Xml.parse_str(input)) {
		(Ok(doc), Ok(xml)) => doc.root.text() == xml.root.text() and doc.declaration == xml.declaration and doc.root.children_named(Uri("urn:f"), "b").len() == 1
		_ => False
	}
}

# The reader reports resolved names, and `element` builds the subtree with
# the bindings in scope.
expect {
	match XmlNs.reader("<a xmlns='urn:a'><b><c/></b></a>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(a), after_a) =>
					match after_a.next() {
						Event(Start(b), after_b) =>
							match after_b.element() {
								Element(Element(subtree), rest) =>
									a.name == name("urn:a", "a")
										and b.name == name("urn:a", "b")
										and subtree.children == [Element({ name: name("urn:a", "c"), bindings: [], attributes: [], children: [] })]
										and (
											match rest.next() {
												Event(End(end), done) => end == name("urn:a", "a") and (
													match done.next() {
														Done => True
														_ => False
													}
												)
												_ => False
											}
										)

								_ => False
							}

						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}

# Limits apply as in Xml.
expect XmlNs.parse("<a><b/></a>", { max_depth: 1 }) == Err(LimitExceeded({ limit: Depth, max: 1, line: 1, column: 4 }))

# Doc examples.
expect {
	match XmlNs.parse_str("<feed xmlns='http://www.w3.org/2005/Atom'><title>News</title></feed>") {
		Ok(doc) => doc.root.children_named(Uri("http://www.w3.org/2005/Atom"), "title").map(|title| title.text()) == ["News"]
		Err(_) => False
	}
}
expect XmlNs.parse_str("<s:svg xmlns:s='http://www.w3.org/2000/svg'/>").map_ok(|doc| doc.root.name()) == Ok(Ok({ namespace: Uri("http://www.w3.org/2000/svg"), local: "svg", prefix: Prefix("s") }))
expect {
	match XmlNs.parse_str("<a xmlns:x='urn:x' x:id='1' id='2'/>") {
		Ok(doc) => doc.root.attribute(Uri("urn:x"), "id") == Ok("1") and doc.root.attribute(NoNamespace, "id") == Ok("2")
		Err(_) => False
	}
}
expect XmlNs.parse("<p:a/>", {}) == Err(InvalidNamespace({ problem: UndeclaredPrefix("p"), line: 1, column: 1 }))
expect {
	match XmlNs.parse_str("<a xmlns='urn:a' xmlns:b='urn:b'><b:c/></a>") {
		Ok(doc) => doc.root.children_named(Uri("urn:b"), "c").len() == 1
		Err(_) => False
	}
}

# Deep documents resolve without exhausting the call stack.
expect {
	depth = 20000
	input = Str.concat("<a xmlns='urn:a'>", Str.concat(Str.repeat("<p:a xmlns:p='urn:p'>", depth), Str.concat(Str.repeat("</p:a>", depth), "</a>")))
	XmlNs.parse_str(input).is_ok()
}

render_omitted : Str -> Try(Str, [Parse, Render(XmlNs.RenderProblem)])
render_omitted = |input| {
	match XmlNs.parse_str(input) {
		Ok(doc) => XmlNs.render(doc, { declaration: Omitted }).map_err(|Unrenderable(failure)| Render(failure.problem))
		Err(_) => Err(Parse)
	}
}

element : XmlNs.Name, List(XmlNs.Binding), List(XmlNs.Attribute), List(XmlNs.Node) -> XmlNs.Node
element = |element_name, bindings, attributes, children| Element({ name: element_name, bindings, attributes, children })

plain : Str -> XmlNs.Name
plain = |local| { namespace: NoNamespace, local, prefix: Unprefixed }

render_tree : XmlNs.Node, XmlNs.RenderProps -> Try(Str, [Unrenderable({ problem : XmlNs.RenderProblem, path : List(Str) })])
render_tree = |root, props| XmlNs.render({ declaration: Err(Missing), root }, props)

# Escaping: `&` and `<` everywhere, `>` after `]]`, `"` and whitespace in
# attribute values, and a carriage return in text.
expect render_tree(element(plain("a"), [], [{ name: plain("k"), value: "<&\"'>\t\n\r" }], [Text("a<b&c]]>d>\r]>")]), { declaration: Omitted }) == Ok("<a k=\"&lt;&amp;&quot;'>&#x9;&#xA;&#xD;\">a&lt;b&amp;c]]&gt;d>&#xD;]></a>")
expect render_omitted("<a k='x&#9;y&#10;z'>&#13;é&#x1F600;</a>") == Ok("<a k=\"x&#x9;y&#xA;z\">&#xD;é😀</a>")

# Declarations.
expect render_tree(element(plain("a"), [], [], []), {}) == Ok("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<a/>")
expect render_tree(element(plain("a"), [], [], []), { declaration: Standalone }) == Ok("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<a/>")

# AsRead writes the tree's own prefixes and bindings.
expect render_omitted("<r xmlns='urn:d' xmlns:p='urn:p'><p:a p:k='1' k='2'/><b xmlns=''/>t</r>") == Ok("<r xmlns=\"urn:d\" xmlns:p=\"urn:p\"><p:a p:k=\"1\" k=\"2\"/><b xmlns=\"\"/>t</r>")

# Characters and names XML 1.0 does not allow are refused, with a path.
expect render_tree(element(plain("a"), [], [], [element(plain("b"), [], [], [Text("x\u(1)")])]), { declaration: Omitted }) == Err(Unrenderable({ problem: InvalidChar(1), path: ["a", "b"] }))
expect render_tree(element(plain("a"), [], [{ name: plain("k"), value: "\u(FFFE)" }], []), {}) == Err(Unrenderable({ problem: InvalidChar(0xFFFE), path: ["a"] }))
expect render_tree(element(plain("1a"), [], [], []), {}) == Err(Unrenderable({ problem: InvalidName("1a"), path: ["1a"] }))
expect render_tree(element(plain("a:b"), [], [], []), {}) == Err(Unrenderable({ problem: InvalidName("a:b"), path: ["a:b"] }))
expect render_tree(element(plain("a"), [{ prefix: Prefix("xmlns"), namespace: Uri("urn:x") }], [], []), {}) == Err(Unrenderable({ problem: InvalidName("xmlns:xmlns"), path: ["a"] }))
expect render_tree(element(plain("a"), [{ prefix: Prefix("p"), namespace: NoNamespace }], [], []), {}) == Err(Unrenderable({ problem: InvalidName("xmlns:p"), path: ["a"] }))

# A prefix must be bound to its name's namespace where it is used.
expect render_tree(element({ namespace: Uri("urn:p"), local: "a", prefix: Prefix("p") }, [], [], []), {}) == Err(Unrenderable({ problem: UnboundNamespace("urn:p"), path: ["p:a"] }))
expect render_tree(element(plain("a"), [{ prefix: Default, namespace: Uri("urn:d") }], [], []), {}) == Err(Unrenderable({ problem: UnboundNamespace(""), path: ["a"] }))
expect render_tree(element(plain("a"), [{ prefix: Default, namespace: Uri("urn:d") }], [{ name: { namespace: Uri("urn:d"), local: "k", prefix: Unprefixed }, value: "" }], []), {}) == Err(Unrenderable({ problem: UnboundNamespace(""), path: ["a"] }))
expect render_tree(element(plain("a"), [], [{ name: { namespace: Uri(XmlNs.xml_namespace), local: "lang", prefix: Prefix("xml") }, value: "en" }], []), { declaration: Omitted }) == Ok("<a xml:lang=\"en\"/>")

# Duplicate expanded attribute names are refused.
expect render_tree(element(plain("a"), [{ prefix: Prefix("p"), namespace: Uri("urn:x") }, { prefix: Prefix("q"), namespace: Uri("urn:x") }], [{ name: { namespace: Uri("urn:x"), local: "k", prefix: Prefix("p") }, value: "" }, { name: { namespace: Uri("urn:x"), local: "k", prefix: Prefix("q") }, value: "" }], []), {}) == Err(Unrenderable({ problem: DuplicateAttribute("q:k"), path: ["a"] }))

# A table names every namespace once, on the root, in table order, with no
# unused declarations, and switches the default where needed.
expect {
	match XmlNs.parse_str("<x:doc xmlns:x='urn:w' xmlns:r='urn:r'><x:body><p xmlns='' r:id='1'/><x:p/></x:body></x:doc>") {
		Ok(doc) =>
			XmlNs.render(doc, { declaration: Omitted, prefixes: Table([{ prefix: "unused", namespace: "urn:unused" }, { prefix: "", namespace: "urn:w" }, { prefix: "rel", namespace: "urn:r" }]) })
				== Ok("<doc xmlns=\"urn:w\" xmlns:rel=\"urn:r\"><body><p xmlns=\"\" rel:id=\"1\"/><p/></body></doc>")

		Err(_) => False
	}
}
expect {
	match XmlNs.parse_str("<a xmlns='urn:a'/>") {
		Ok(doc) => XmlNs.render(doc, { prefixes: Table([]) }) == Err(Unrenderable({ problem: UnboundNamespace("urn:a"), path: ["a"] }))
		Err(_) => False
	}
}
expect {
	match XmlNs.parse_str("<a xmlns='urn:a'/>") {
		Ok(doc) => XmlNs.render(doc, { prefixes: Table([{ prefix: "p", namespace: "urn:a" }, { prefix: "p", namespace: "urn:b" }]) }) == Err(Unrenderable({ problem: InvalidName("xmlns:p"), path: ["a"] }))
		Err(_) => False
	}
}

# Attributes can be sorted.
by_first_byte : Str, Str -> [Before, Same, After]
by_first_byte = |a, b| {
	x = a.to_utf8().first() ?? 0
	y = b.to_utf8().first() ?? 0
	if (x < y) Before else if (x > y) After else Same
}
expect render_omitted("<a z='1' y='2' x='3'/>") == Ok("<a z=\"1\" y=\"2\" x=\"3\"/>")
expect {
	match XmlNs.parse_str("<a z='1' y='2' x='3'/>") {
		Ok(doc) => XmlNs.render(doc, { declaration: Omitted, attribute_order: SortedBy(|a, b| by_first_byte(a.local, b.local)) }) == Ok("<a x=\"3\" y=\"2\" z=\"1\"/>")
		Err(_) => False
	}
}

# Rendering a parsed document and parsing it again gives the same tree.
expect {
	inputs = [
		"<r xmlns='urn:d' xmlns:p='urn:p'><p:a p:k='1&#10;2' k='&quot;'/><b xmlns=''>x]]&gt;y&#13;</b>t<![CDATA[<c>]]></r>",
		"<?xml version='1.0' standalone='yes'?><w:document xmlns:w='urn:w' xml:space='preserve'><w:body><w:p><w:r><w:t>Hi</w:t></w:r></w:p></w:body></w:document>",
	]
	inputs.all(
		|input| {
			match XmlNs.parse_str(input) {
				Ok(doc) =>
					match XmlNs.render(doc, {}) {
						Ok(text) => XmlNs.parse_str(text).map_ok(|again| again.root) == Ok(doc.root) and XmlNs.render(doc, {}) == Ok(text)
						Err(_) => False
					}

				Err(_) => False
			}
		},
	)
}

# Deep trees are written without exhausting the call stack.
expect {
	depth = 20000
	input = Str.concat(Str.repeat("<a>", depth), Str.repeat("</a>", depth))
	render_omitted(input) == Ok(Str.concat(Str.repeat("<a>", depth - 1), Str.concat("<a/>", Str.repeat("</a>", depth - 1))))
}

# Doc examples: render.
expect render_omitted("<p:a xmlns:p='urn:p' k='1 &amp; 2'>x &lt; y</p:a>") == Ok("<p:a xmlns:p=\"urn:p\" k=\"1 &amp; 2\">x &lt; y</p:a>")
expect {
	match XmlNs.parse_str("<a xmlns='urn:w'><b/></a>") {
		Ok(doc) => XmlNs.render(doc, { declaration: Standalone, prefixes: Table([{ prefix: "w", namespace: "urn:w" }]) }) == Ok("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<w:a xmlns:w=\"urn:w\"><w:b/></w:a>")
		Err(_) => False
	}
}

# With a table, a root in no namespace leaves the default to the first
# element in it.
expect render_tree(element(plain("r"), [], [], [element(name("urn:a", "a"), [], [], [element(name("urn:a", "b"), [], [], [])])]), { declaration: Omitted, prefixes: Table([{ prefix: "", namespace: "urn:a" }]) }) == Ok("<r><a xmlns=\"urn:a\"><b/></a></r>")

# Many bindings shadow and restore like a few do.
expect {
	outer = Str.join_with(List.repeat(0, 6).map_with_index(|_, index| " xmlns:p${index.to_str()}='urn:${index.to_str()}'"), "")
	inner = Str.join_with(List.repeat(0, 6).map_with_index(|_, index| " xmlns:q${index.to_str()}='urn:q${index.to_str()}'"), "")
	input = "<r${outer}><a${inner} xmlns:p3='urn:shadow'><p3:x q5:k='v'/></a><p3:y/><b xmlns:q0='urn:b'><q0:z/></b></r>"
	match XmlNs.parse_str(input) {
		Ok(doc) =>
			match doc.root {
				Element(r) =>
					match r.children {
						[Element(a), Element(y), Element(b)] =>
							match (a.children, b.children) {
								([Element(x)], [Element(z)]) =>
									x.name.namespace == Uri("urn:shadow")
										and x.attributes.map(|attribute| attribute.name.namespace) == [Uri("urn:q5")]
										and y.name.namespace == Uri("urn:3")
										and z.name.namespace == Uri("urn:b")

								_ => False
							}

						_ => False
					}

				Text(_) => False
			}

		Err(_) => False
	}
}
expect problem_of("<r xmlns:a='urn:a' xmlns:b='urn:b' xmlns:c='urn:c' xmlns:d='urn:d' xmlns:e='urn:e' xmlns:f='urn:f' xmlns:g='urn:g' xmlns:h='urn:h'><x xmlns:i='urn:i'/><i:y/></r>") == Ok(UndeclaredPrefix("i"))

resolve_source_start : Bound, Str, List(Xml.Attribute), List(Xml.SourceSpan) -> Try({ bound : Bound, name : XmlNs.Name, attributes : List(XmlNs.SourceAttribute) }, XmlNs.Problem)
resolve_source_start = |bound, written, attributes, spans| {
	if has_declaration(attributes) {
		resolved = resolve_start({ entries: bound.entries, active: bound.active, frames: [], names: [] }, written, attributes)?
		ordered_attributes = ordered_source_attributes(attributes, spans, resolved.attributes, resolved.bindings)
		Ok({ bound: { entries: resolved.scope.entries, active: resolved.scope.active }, name: resolved.name, attributes: ordered_attributes })
	} else {
		element_name = split_qname(written)?
		if element_name.prefix == "xmlns" {
			return Err(ReservedPrefix("xmlns"))
		}
		resolved_name = resolve(element_name, bound.entries, bound.active, True)?
		var $resolved = List.with_capacity(attributes.len())
		var $index = 0
		var $prefixed = 0
		while $index < attributes.len() {
			attribute = attributes.get($index) ?? { name: "", value: "" }
			span = spans.get($index) ?? { start: 0, end: 0 }
			$index = $index + 1
			attribute_name = split_qname(attribute.name)?
			if attribute_name.prefix != "" {
				$prefixed = $prefixed + 1
			}
			$resolved = $resolved.append({ name: resolve(attribute_name, bound.entries, bound.active, False)?, value: attribute.value, span })
		}
		if $prefixed > 1 {
			check_unique($resolved.map(|attribute| { name: attribute.name, value: attribute.value }))?
		}
		Ok({ bound, name: resolved_name, attributes: $resolved })
	}
}

ordered_source_attributes : List(Xml.Attribute), List(Xml.SourceSpan), List(XmlNs.Attribute), List(XmlNs.Binding) -> List(XmlNs.SourceAttribute)
ordered_source_attributes = |raw, spans, attributes, bindings| {
	var $ordered = List.with_capacity(raw.len())
	var $index = 0
	var $attribute_index = 0
	var $binding_index = 0
	while $index < raw.len() {
		attribute = raw.get($index) ?? { name: "", value: "" }
		span = spans.get($index) ?? { start: 0, end: 0 }
		$index = $index + 1
		if is_declaration(attribute.name) {
			match bindings.get($binding_index) {
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
					$ordered = $ordered.append({ name: { namespace: Uri(XmlNs.xmlns_namespace), local, prefix }, value, span })
				}
				Err(_) => {}
			}
		} else {
			match attributes.get($attribute_index) {
				Ok(resolved) => {
					$attribute_index = $attribute_index + 1
					$ordered = $ordered.append({ name: resolved.name, value: resolved.value, span })
				}
				Err(_) => {}
			}
		}
	}
	$ordered
}
