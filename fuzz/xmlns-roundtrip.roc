app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.5.0/9WT86CuwM7ZJBSs8iTd1jLyrUFTTiGp5hcJE63TwswGE.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.XmlNs

## Writing namespaced trees: fuzzer bytes choose a tree (names from a few
## namespaces, text and attribute values full of characters that need
## escaping, and sometimes a name, character or attribute that cannot be
## written) and a prefix table, which may make one namespace the default.
##
## - XmlNs.render refuses exactly the trees with something it cannot write.
## - A tree it writes reads back as the same tree, up to prefixes and
##   bindings; written again with its own prefixes, it reads back exactly.

Cur : { bytes : List(U8), pos : U64 }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

namespaces : List(XmlNs.Namespace)
namespaces = [NoNamespace, Uri("urn:a"), Uri("urn:b"), Uri("http://example.org/é?q=1&r")]

locals : List(Str)
locals = ["a", "b", "é", "_x", "p-q", "x.1", "日本", "w"]

## Characters and runs that need escaping or are ordinary.
pieces : List(Str)
pieces = ["x", "&", "<", ">", "]]>", "\r", "\r\n", "\n", "\t", "\"", "'", "é", "😀", " ", "&amp;", "]]"]

Built : { node : XmlNs.Node, cur : Cur, valid : Bool }

gen_text : Cur -> { text : Str, cur : Cur, valid : Bool }
gen_text = |start| {
	count = pick(start, 4)
	var $cur = count.cur
	var $text = ""
	var $valid = True
	var $index = 0
	while $index <= count.n {
		piece = pick($cur, 40)
		$cur = piece.cur
		if piece.n == 39 {
			$text = Str.concat($text, "\u(1)")
			$valid = False
		} else {
			$text = Str.concat($text, pieces.get(piece.n % pieces.len()) ?? "x")
		}
		$index = $index + 1
	}
	{ text: $text, cur: $cur, valid: $valid }
}

gen_name : Cur -> { name : XmlNs.Name, cur : Cur, valid : Bool }
gen_name = |start| {
	ns = pick(start, namespaces.len())
	local = pick(ns.cur, 33)
	invalid = local.n == 32
	name = if (invalid) "1a" else locals.get(local.n % locals.len()) ?? "a"
	{ name: { namespace: namespaces.get(ns.n) ?? NoNamespace, local: name, prefix: Unprefixed }, cur: local.cur, valid: !invalid }
}

gen_element : Cur, U64 -> Built
gen_element = |start, depth| {
	element_name = gen_name(start)
	var $cur = element_name.cur
	var $valid = element_name.valid
	attribute_count = pick($cur, 4)
	$cur = attribute_count.cur
	var $attributes = []
	var $index = 0
	while $index < attribute_count.n {
		attribute_name = gen_name($cur)
		value = gen_text(attribute_name.cur)
		$cur = value.cur
		# An attribute may also be in the xml namespace.
		with_xml = pick($cur, 8)
		$cur = with_xml.cur
		name = if (with_xml.n == 0) { ..attribute_name.name, namespace: Uri(XmlNs.xml_namespace) } else attribute_name.name
		if $attributes.any(|attribute| attribute.name.namespace == name.namespace and attribute.name.local == name.local) {
			$valid = False
		}
		$valid = $valid and attribute_name.valid and value.valid
		$attributes = $attributes.append({ name, value: value.text })
		$index = $index + 1
	}
	child_count = if (depth >= 4) { n: 0, cur: $cur } else pick($cur, 4)
	$cur = child_count.cur
	var $children = []
	var $last_text = False
	$index = 0
	while $index < child_count.n {
		kind = pick($cur, 3)
		$cur = kind.cur
		if kind.n == 0 and !$last_text {
			text = gen_text($cur)
			$cur = text.cur
			$valid = $valid and text.valid
			$children = $children.append(Text(text.text))
			$last_text = True
		} else {
			child = gen_element($cur, depth + 1)
			$cur = child.cur
			$valid = $valid and child.valid
			$children = $children.append(child.node)
			$last_text = False
		}
		$index = $index + 1
	}
	{ node: Element({ name: element_name.name, bindings: [], attributes: $attributes, children: $children }), cur: $cur, valid: $valid }
}

## A prefix table for the generated namespaces: fresh prefixes, one of
## which may be the default.
gen_table : Cur -> { table : List({ prefix : Str, namespace : Str }), uses_default : Try(Str, [None]) }
gen_table = |start| {
	default = pick(start, 4)
	var $table = []
	var $default = Err(None)
	var $index = 0
	for namespace in namespaces {
		match namespace {
			Uri(uri) => {
				prefix = if ($index + 1 == default.n) "" else "n${$index.to_str()}"
				if prefix == "" {
					$default = Ok(uri)
				}
				$table = $table.append({ prefix, namespace: uri })
				$index = $index + 1
			}

			NoNamespace => {}
		}
	}
	{ table: $table, uses_default: $default }
}

## Whether an attribute of the tree is in the table's default namespace,
## which only elements can use.
uses_default_for_attribute : XmlNs.Node, Str -> Bool
uses_default_for_attribute = |root, uri| {
	var $pending = [root]
	while !$pending.is_empty() {
		node = $pending.last() ?? Text("")
		$pending = $pending.drop_last(1)
		match node {
			Element(element) => {
				if element.attributes.any(|attribute| attribute.name.namespace == Uri(uri)) {
					return True
				}
				$pending = $pending.concat(element.children)
			}

			Text(_) => {}
		}
	}
	False
}

unprefixed : XmlNs.Node -> XmlNs.Node
unprefixed = |node| {
	match node {
		Text(text) => Text(text)
		Element(element) => {
			strip = |name| { ..name, prefix: Unprefixed }
			Element({ name: strip(element.name), bindings: [], attributes: element.attributes.map(|attribute| { ..attribute, name: strip(attribute.name) }), children: element.children.map(unprefixed) })
		}
	}
}

## The tree XML reports for a generated one: line endings in text and
## attribute values arrive as written, since render escapes carriage returns.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	start = { bytes, pos: 0 }
	built = gen_element(start, 0)
	table = gen_table(built.cur)
	writable =
		built.valid
			and (
				match table.uses_default {
					Ok(uri) => !uses_default_for_attribute(built.node, uri)
					Err(None) => True
				}
			)
	doc = { declaration: Err(Missing), root: built.node }
	match XmlNs.render(doc, { prefixes: Table(table.table) }) {
		Err(Unrenderable(failure)) =>
			if writable {
				crash "a writable tree was refused: ${Str.inspect(failure)}\n${Str.inspect(built.node)}"
			}

		Ok(text) => {
			if !writable {
				crash "a tree that cannot be written was written:\n${text}"
			}
			match XmlNs.parse_str(text) {
				Ok(again) => {
					if unprefixed(again.root) != built.node {
						crash "the written tree reads back differently:\n${text}\n${Str.inspect(again.root)}\n${Str.inspect(built.node)}"
					}
					match XmlNs.render(again, {}) {
						Ok(own) =>
							if XmlNs.parse_str(own).map_ok(|third| third.root) != Ok(again.root) {
								crash "written with its own prefixes, the tree reads back differently:\n${own}"
							}

						Err(Unrenderable(failure)) => crash "a parsed tree cannot be written with its own prefixes: ${Str.inspect(failure)}"
					}
				}

				Err(error) => crash "the written text does not parse: ${Str.inspect(error)}\n${text}"
			}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	examples: [],
	name: "xmlns-roundtrip",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(gen_element({ bytes, pos: 0 }, 0).node),
})
