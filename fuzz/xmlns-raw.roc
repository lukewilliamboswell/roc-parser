app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import parser.XmlNs

## XmlNs on arbitrary UTF-8:
## - when Xml, processing namespaces, rejects the document, XmlNs fails with
##   the same error or with a namespace problem located no later;
## - otherwise its tree has Xml's shape: every prefix and local part joins
##   back to the name as written, `xmlns` attributes become the bindings in
##   source order and the others keep their order, and the text is the same;
## - each name's namespace is the one an independent walk of the bindings in
##   scope gives;
## - writing the tree with its own prefixes and reading it back gives the
##   same tree, and writing it twice gives the same text;
## - writing it with a table of fresh prefixes and reading it back gives the
##   same tree up to prefixes and bindings.

written : XmlNs.Name -> Str
written = |name| {
	match name.prefix {
		Prefix(prefix) => "${prefix}:${name.local}"
		Unprefixed => name.local
	}
}

is_declaration : Str -> Bool
is_declaration = |name| name == "xmlns" or name.starts_with("xmlns:")

## The namespace bound to `prefix` ("" for the default) in `scope`, innermost
## binding last.
lookup : List({ prefix : Str, namespace : XmlNs.Namespace }), Str -> Try(XmlNs.Namespace, [Unbound])
lookup = |scope, prefix| {
	var $index = scope.len()
	while $index > 0 {
		$index = $index - 1
		match scope.get($index) {
			Ok(binding) if binding.prefix == prefix => return Ok(binding.namespace)
			_ => {}
		}
	}
	if prefix == "xml" {
		Ok(Uri("http://www.w3.org/XML/1998/namespace"))
	} else if prefix == "" {
		Ok(NoNamespace)
	} else {
		Err(Unbound)
	}
}

## Compares the namespaced tree with the plain one, element by element.
check_shape : XmlNs.Node, Xml.Node -> {}
check_shape = |ns_root, plain_root| {
	var $pending = [{ ns: ns_root, plain: plain_root, scope: [] }]
	while !$pending.is_empty() {
		item = $pending.last() ?? { ns: Text(""), plain: Text(""), scope: [] }
		$pending = $pending.drop_last(1)
		match (item.ns, item.plain) {
			(Text(a), Text(b)) =>
				if a != b {
					crash "text differs: ${Str.inspect(a)} and ${Str.inspect(b)}"
				}

			(Element(ns), Element(plain)) => {
				if written(ns.name) != plain.name {
					crash "${Str.inspect(ns.name)} does not join back to ${plain.name}"
				}
				declarations = plain.attributes.keep_if(|attribute| is_declaration(attribute.name))
				others = plain.attributes.keep_if(|attribute| !is_declaration(attribute.name))
				expected_bindings = declarations.map(
					|attribute| {
						prefix = if (attribute.name == "xmlns") Default else Prefix(Str.from_utf8(attribute.name.to_utf8().drop_first(6)) ?? "")
						{ prefix, namespace: if (attribute.value == "") NoNamespace else Uri(attribute.value) }
					},
				)
				if ns.bindings != expected_bindings {
					crash "bindings ${Str.inspect(ns.bindings)} are not the declarations ${Str.inspect(declarations)}"
				}
				if ns.attributes.map(|attribute| { name: written(attribute.name), value: attribute.value }) != others {
					crash "attributes ${Str.inspect(ns.attributes)} differ from ${Str.inspect(others)}"
				}
				scope = item.scope.concat(
					ns.bindings.map(
						|binding| {
							prefix = match binding.prefix {
								Prefix(p) => p
								Default => ""
							}
							{ prefix, namespace: binding.namespace }
						},
					),
				)
				element_prefix = match ns.name.prefix {
					Prefix(p) => p
					Unprefixed => ""
				}
				if lookup(scope, element_prefix) != Ok(ns.name.namespace) {
					crash "${Str.inspect(ns.name)} has the wrong namespace"
				}
				for attribute in ns.attributes {
					expected =
						match attribute.name.prefix {
							Prefix(p) => lookup(scope, p)
							Unprefixed => Ok(NoNamespace)
						}
					if expected != Ok(attribute.name.namespace) {
						crash "attribute ${Str.inspect(attribute.name)} has the wrong namespace"
					}
				}
				if ns.children.len() != plain.children.len() {
					crash "${plain.name} has a different number of children"
				}
				var $index = 0
				while $index < ns.children.len() {
					match (ns.children.get($index), plain.children.get($index)) {
						(Ok(a), Ok(b)) => {
							$pending = $pending.append({ ns: a, plain: b, scope })
						}

						_ => {}
					}
					$index = $index + 1
				}
			}

			_ => crash "a text node and an element are in the same place"
		}
	}
}

## The tree with prefixes and bindings removed, for comparisons up to them.
unprefixed : XmlNs.Node -> XmlNs.Node
unprefixed = |root| {
	strip = |name| { ..name, prefix: Unprefixed }
	# Rebuilt bottom-up with an explicit stack, as trees may be deep.
	var $work = [Visit(root)]
	var $built = []
	while !$work.is_empty() {
		item = $work.last() ?? Visit(Text(""))
		$work = $work.drop_last(1)
		match item {
			Visit(Text(text)) => {
				$built = $built.append(XmlNs.Node.Text(text))
			}

			Visit(Element(element)) => {
				$work = $work.append(Build(element))
				for child in element.children {
					$work = $work.append(Visit(child))
				}
			}

			Build(element) => {
				count = element.children.len()
				# Children were visited last to first.
				var $children = []
				var $index = $built.len()
				while $index > $built.len() - count {
					$index = $index - 1
					match $built.get($index) {
						Ok(child) => {
							$children = $children.append(child)
						}

						Err(_) => {}
					}
				}
				children = $children
				$built = $built.take_first($built.len() - count)
				$built = $built.append(Element({ name: strip(element.name), bindings: [], attributes: element.attributes.map(|attribute| { ..attribute, name: strip(attribute.name) }), children }))
			}
		}
	}
	$built.first() ?? root
}

namespaces_of : XmlNs.Node -> List(Str)
namespaces_of = |root| {
	var $found = []
	var $pending = [root]
	while !$pending.is_empty() {
		node = $pending.last() ?? Text("")
		$pending = $pending.drop_last(1)
		match node {
			Element(element) => {
				names = element.attributes.map(|attribute| attribute.name).append(element.name)
				for name in names {
					match name.namespace {
						Uri(uri) if !$found.contains(uri) and uri != XmlNs.xml_namespace => {
							$found = $found.append(uri)
						}

						_ => {}
					}
				}
				$pending = $pending.concat(element.children)
			}

			Text(_) => {}
		}
	}
	$found
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	input =
		match Str.from_utf8(bytes) {
			Ok(text) => text
			Err(_) => return Fuzz.reject
		}
	plain = Xml.parse(input, { namespaces: True })
	namespaced = XmlNs.parse_str(input)
	match (plain, namespaced) {
		(Err(InvalidXml(error)), Err(InvalidXml(other))) =>
			if error != other {
				crash "XmlNs reports ${Str.inspect(other)}, Xml ${Str.inspect(error)}"
			}

		(Ok(_), Err(InvalidNamespace(_))) => {}
		# Problems are reported in document order, so a namespace problem can
		# come before a later well-formedness error.
		(Err(InvalidXml(error)), Err(InvalidNamespace(problem))) =>
			if problem.line > error.line or (problem.line == error.line and problem.column > error.column) {
				crash "a namespace problem at ${Str.inspect(problem)} was reported for a document malformed earlier, at ${Str.inspect(error)}"
			}

		(Ok(xml), Ok(doc)) => {
			if doc.declaration != xml.declaration {
				crash "the declarations differ"
			}
			check_shape(doc.root, xml.root)
			check_render(doc)
		}

		_ => crash "Xml and XmlNs disagree: ${Str.inspect(plain)} and ${Str.inspect(namespaced)}"
	}
	Fuzz.keep
}

check_render : XmlNs -> {}
check_render = |doc| {
	match XmlNs.render(doc, {}) {
		Ok(text) => {
			if XmlNs.parse_str(text).map_ok(|again| again.root) != Ok(doc.root) {
				crash "writing with the tree's own prefixes does not read back:\n${text}"
			}
			if XmlNs.render(doc, {}) != Ok(text) {
				crash "writing twice gives different text"
			}
		}

		Err(Unrenderable(failure)) => crash "a parsed tree cannot be written: ${Str.inspect(failure)}"
	}
	table = namespaces_of(doc.root).map_with_index(|namespace, index| { prefix: "p${index.to_str()}", namespace })
	match XmlNs.render(doc, { declaration: Omitted, prefixes: Table(table) }) {
		Ok(text) =>
			match XmlNs.parse_str(text) {
				Ok(again) =>
					if unprefixed(again.root) != unprefixed(doc.root) {
						crash "writing with a table does not read back up to prefixes:\n${text}"
					}

				Err(error) => crash "writing with a table gives a document XmlNs rejects: ${Str.inspect(error)}\n${text}"
			}

		Err(Unrenderable(failure)) => crash "a parsed tree cannot be written with a table: ${Str.inspect(failure)}"
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "xmlns-raw",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
