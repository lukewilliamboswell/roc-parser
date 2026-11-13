import parser.Markup

## Invariants independent of the scanner: every edge is a valid forward tree edge,
## roots and children are owned once, and source token spans partition the input.
MarkupCheck :: [].{
	check_source : Markup.Source -> {}
	check_source = |source| {
		var $cursor = 0
		for token in source.tokens {
			if token.span.start != $cursor or token.span.end <= token.span.start or token.span.end > source.bytes.len() {
				crash "lossless tokens do not partition the source"
			}
			$cursor = token.span.end
		}
		if $cursor != source.bytes.len() {
			crash "lossless token partition omits source bytes"
		}
	}

	check : Markup.Parsed, Str -> {}
	check = |parsed, input| {
		if parsed.source.bytes != input.to_utf8() {
			crash "parsing changed the retained source"
		}
		check_source(parsed.source)
		var $seen = Set.empty()
		var $pending = parsed.tree.roots
		while !$pending.is_empty() {
			index = $pending.last() ?? 0
			$pending = $pending.drop_last(1)
			if $seen.contains(index) {
				crash "a tree node is reachable more than once"
			}
			$seen = $seen.insert(index)
			node = parsed.tree.nodes.get(index) ?? crash "a tree edge lies outside the node arena"
			for child in node.children {
				if child >= parsed.tree.nodes.len() {
					crash "a child lies outside the node arena"
				}
				$pending = $pending.append(child)
			}
			match node.origin {
				Authored(span) => if span.start > span.end or span.end > input.count_utf8_bytes() {
					crash "authored span lies outside input"
				}
				Implied(offset) => if offset > input.count_utf8_bytes() {
					crash "implied origin lies outside input"
				}
				Cloned(origin) => if origin.source.start > origin.source.end or origin.source.end > input.count_utf8_bytes() or origin.at > input.count_utf8_bytes() {
					crash "cloned provenance lies outside source"
				}
			}
		}
	}

	## Independently described semantic trees for generated-input comparisons.
	Model := [Element({ namespace : Str, local : Str, attributes : List((Str, Str, Str)), children : List(Model) }), Text(Str), Comment(Str), Other].{
		is_eq : _
	}
	model : Markup.Tree, U64 -> Model
	model = |tree, index| {
		node = tree.nodes.get(index) ?? crash "invalid tree edge"
		match node.kind {
			Element(name) => {
				ns = |namespace| match namespace {
					Uri(uri) => uri
					NoNamespace => ""
				}
				Element({ namespace: ns(name.namespace), local: name.local, attributes: node.attributes.keep_if(|attribute| attribute.name.namespace != Uri("http://www.w3.org/2000/xmlns/") and attribute.name.local != "xmlns").map(|attribute| (ns(attribute.name.namespace), attribute.name.local, attribute.value)), children: node.children.map(|child| model(tree, child)) })
			}
			Text(text) => Text(text)
			Comment(text) => Comment(text)
			_ => Other
		}
	}
}
