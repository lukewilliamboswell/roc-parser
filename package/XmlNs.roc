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
	for attribute in attributes {
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
	element_name = split_qname(name)?
	if element_name.prefix == "xmlns" {
		return Err(ReservedPrefix("xmlns"))
	}
	resolved_name = resolve(element_name, $entries, $active, True)?
	var $resolved = []
	var $prefixed = 0
	for attribute in attributes {
		if !is_declaration(attribute.name) {
			attribute_name = split_qname(attribute.name)?
			if attribute_name.prefix != "" {
				$prefixed = $prefixed + 1
			}
			$resolved = $resolved.append({ name: resolve(attribute_name, $entries, $active, False)?, value: attribute.value })
		}
	}
	if $prefixed > 1 {
		check_unique($resolved)?
	}
	names = scope.names.append(resolved_name)
	Ok({ scope: { entries: $entries, active: $active, frames, names }, name: resolved_name, bindings: $bindings, attributes: $resolved })
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
is_declaration = |name| name == "xmlns" or name.starts_with("xmlns:")

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

Open : {
	name : XmlNs.Name,
	bindings : List(XmlNs.Binding),
	attributes : List(XmlNs.Attribute),
	first_child : U64,
}

# Builds the element whose start was the reader's most recent event, as
# Xml builds its tree: children of every open element in one flat list.
build_element : XmlNs.Reader, { name : XmlNs.Name, bindings : List(XmlNs.Binding), attributes : List(XmlNs.Attribute) } -> [Element(XmlNs.Node, XmlNs.Reader), Failed(XmlNs.ReadError), NotAfterStart]
build_element = |reader, start| {
	var $reader = reader
	var $open : List(Open)
	var $open = []
	var $current = start
	var $first_child = 0
	var $nodes = []
	while True {
		match $reader.next() {
			Event(Start(tag), rest) => {
				$reader = rest
				$open = $open.append({ name: $current.name, bindings: $current.bindings, attributes: $current.attributes, first_child: $first_child })
				$current = tag
				$first_child = $nodes.len()
			}

			Event(Text(text), rest) => {
				$reader = rest
				$nodes = $nodes.append(Text(text))
			}

			Event(End(_), rest) => {
				node = Element({ name: $current.name, bindings: $current.bindings, attributes: $current.attributes, children: copy_from($nodes, $first_child) })
				match $open.last() {
					Err(_) => return Element(node, rest)
					Ok(parent) => {
						$reader = rest
						$open = $open.drop_last(1)
						$nodes = truncate($nodes, $first_child).append(node)
						$current = { name: parent.name, bindings: parent.bindings, attributes: parent.attributes }
						$first_child = parent.first_child
					}
				}
			}

			Done => return Failed(InvalidXml({ line: 1, column: 1, message: "internal error: the document ended inside an element" }))
			Failed(error) => return Failed(error)
		}
	}
}

copy_from : List(XmlNs.Node), U64 -> List(XmlNs.Node)
copy_from = |nodes, start| {
	var $out = List.with_capacity(nodes.len() - start)
	var $index = start
	while $index < nodes.len() {
		match nodes.get($index) {
			Ok(node) => {
				$out = $out.append(node)
			}

			Err(_) => {}
		}
		$index = $index + 1
	}
	$out
}

truncate : List(XmlNs.Node), U64 -> List(XmlNs.Node)
truncate = |nodes, len| {
	var $out = nodes
	while $out.len() > len {
		$out = $out.drop_last(1)
	}
	$out
}

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
