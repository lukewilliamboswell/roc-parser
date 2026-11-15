# TODO(https://github.com/roc-lang/roc/issues/13077): move Node and its methods
# back under XmlNs once nested derived methods stop producing duplicate warnings.
# Keep these backing types in a separate module until public aliases also work
# with exposed-field validation (https://github.com/roc-lang/roc/issues/13023).
XmlNsTypes :: [].{
	Namespace : [Uri(Str), NoNamespace]
	Name : { namespace : Namespace, local : Str, prefix : [Prefix(Str), Unprefixed] }
	Binding : { prefix : [Prefix(Str), Default], namespace : Namespace }
	Attribute : { name : Name, value : Str }

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

}
