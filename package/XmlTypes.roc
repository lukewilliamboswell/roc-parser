# Keep these backing types in a separate module until public aliases also work
# with exposed-field validation (https://github.com/roc-lang/roc/issues/13023).
XmlTypes :: [].{
	Attribute : { name : Str, value : Str }

	Node := [
		Element({ name : Str, attributes : List(XmlTypes.Attribute), children : List(Node) }),
		Text(Str),
	].{

		## Compare two XML nodes structurally.
		is_eq : _

		## Hash an XML node, so nodes can be `Dict` keys and `Set` members.
		to_hash : _

		## The name of an element, or `Err(NotAnElement)` for a text node.
		##
		## ```roc
		## expect Xml.parse_str("<svg:rect/>", {}).map_ok(|xml| xml.root.name()) == Ok(Ok("svg:rect"))
		## ```
		name : Node -> Try(Str, [NotAnElement])
		name = |node| {
			match node {
				Element(element) => Ok(element.name)
				Text(_) => Err(NotAnElement)
			}
		}

		## The value of the element's attribute with this exact name, or
		## `Err(Missing)` when there is none or the node is text.
		##
		## ```roc
		## expect {
		##     root = Xml.parse_str("<a href='/x'/>", {})?.root
		##     root.attribute("href") == Ok("/x") and root.attribute("title") == Err(Missing)
		## }
		## ```
		attribute : Node, Str -> Try(Str, [Missing])
		attribute = |node, attribute_name| {
			match node {
				Element(element) =>
					match element.attributes.find_first(|attr| attr.name == attribute_name) {
						Ok(attr) => Ok(attr.value)
						Err(_) => Err(Missing)
					}

				Text(_) => Err(Missing)
			}
		}

		## The element's direct child elements with this exact name, in
		## document order. Empty for a text node.
		##
		## ```roc
		## expect {
		##     root = Xml.parse_str("<list><item>1</item><other/><item>2</item></list>", {})?.root
		##     root.children_named("item").map(|item| item.text()) == ["1", "2"]
		## }
		## ```
		children_named : Node, Str -> List(Node)
		children_named = |node, child_name| {
			match node {
				Element(element) =>
					element.children.keep_if(
						|child| {
							match child {
								Element(child_element) => child_element.name == child_name
								Text(_) => False
							}
						},
					)

				Text(_) => []
			}
		}

		## All character data in the node and its descendants, concatenated in
		## document order (like the DOM's `textContent`).
		##
		## ```roc
		## expect Xml.parse_str("<p>Hello, <b>world</b>!</p>", {}).map_ok(|xml| xml.root.text()) == Ok("Hello, world!")
		## ```
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

}
