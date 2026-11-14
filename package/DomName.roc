import XmlNs

## Validate computed DOM names using the [DOM Standard](https://dom.spec.whatwg.org/#validate).
## Names retain namespace, prefix and local spelling independently, using the
## same representation as [XmlNs.Name] and [Markup.Name]. These rules do not
## restrict names to a known HTML, SVG or MathML vocabulary.
##
## DOM construction accepts names that XML source cannot represent, such as
## the SVG element `a=b`. Use the XML parsers to check XML source instead.
DomName :: [].{

	## An explicit resolved identity. A colon in an unprefixed local name
	## remains part of that local name.
	Name : XmlNs.Name

	## An absent namespace or its URI. Namespace-aware construction normalizes
	## `Uri("")` to `NoNamespace`.
	Namespace : XmlNs.Namespace

	## The DOM operation's local-name validation context.
	Context : [Element, Attribute]

	## DOM exception categories, returned in the specification's validation order.
	Problem : [InvalidCharacterError, NamespaceError]

	## Validate a namespace and qualified spelling for `createElementNS`,
	## `createAttributeNS` or `setAttributeNS`. Split at the first colon only;
	## the local part can itself contain colons.
	##
	## ```roc
	## expect DomName.validate_and_extract(Uri("http://www.w3.org/2000/svg"), "s:a=b", Element) == Ok({ namespace: Uri("http://www.w3.org/2000/svg"), prefix: Prefix("s"), local: "a=b" })
	## ```
	validate_and_extract : Namespace, Str, Context -> Try(Name, Problem)
	validate_and_extract = |namespace, qualified_name, context| {
		ns = if (namespace == Uri("")) NoNamespace else namespace
		parts = match qualified_name.split_first(":") {
			Ok(split) => {
				if !valid_namespace_prefix(split.before) {
					return Err(InvalidCharacterError)
				}
				{ prefix: Prefix(split.before), local: split.after }
			}
			Err(_) => { prefix: Unprefixed, local: qualified_name }
		}
		valid = match context {
			Element => valid_element_local_name(parts.local)
			Attribute => valid_attribute_local_name(parts.local)
		}
		if !valid {
			return Err(InvalidCharacterError)
		}
		if parts.prefix != Unprefixed and ns == NoNamespace {
			return Err(NamespaceError)
		}
		if parts.prefix == Prefix("xml") and ns != Uri(XmlNs.xml_namespace) {
			return Err(NamespaceError)
		}
		xmlns = qualified_name == "xmlns" or parts.prefix == Prefix("xmlns")
		if xmlns and ns != Uri(XmlNs.xmlns_namespace) {
			return Err(NamespaceError)
		}
		if ns == Uri(XmlNs.xmlns_namespace) and !xmlns {
			return Err(NamespaceError)
		}
		Ok({ namespace: ns, prefix: parts.prefix, local: parts.local })
	}

	## Format an explicit identity. This does not validate it or infer a prefix
	## from a colon in `local`. The result alone cannot recover its identity:
	## both unprefixed `h:div` and prefix `h` plus local `div` spell `h:div`.
	qualified : Name -> Str
	qualified = |name| match name.prefix {
		Unprefixed => name.local
		Prefix(prefix) => "${prefix}:${name.local}"
	}

	## Name produced by plain `createElement` in an HTML document: HTML
	## namespace, ASCII-lowercased local spelling, and no prefix. A colon is
	## retained in the local name rather than treated as a namespace separator.
	##
	## ```roc
	## expect DomName.html_element("H:DIV") == Ok({ namespace: Uri("http://www.w3.org/1999/xhtml"), prefix: Unprefixed, local: "h:div" })
	## ```
	html_element : Str -> Try(Name, [InvalidCharacterError])
	html_element = |local| {
		name = unqualified_element(local)?
		Ok({ ..name, namespace: Uri("http://www.w3.org/1999/xhtml"), local: lower_ascii(local) })
	}

	## Plain `createElement` name for an XML document without an XHTML content
	## type: no namespace or prefix, with case and any colon retained.
	unqualified_element : Str -> Try(Name, [InvalidCharacterError])
	unqualified_element = |local| {
		if valid_element_local_name(local) {
			Ok({ namespace: NoNamespace, prefix: Unprefixed, local })
		} else {
			Err(InvalidCharacterError)
		}
	}

	## Plain `createAttribute` name for an XML document: no namespace or
	## prefix, with case and any colon retained. For HTML-document attribute
	## creation, ASCII-lowercase the input first. Namespace-aware operations
	## use `validate_and_extract` instead.
	unqualified_attribute : Str -> Try(Name, [InvalidCharacterError])
	unqualified_attribute = |local| {
		if valid_attribute_local_name(local) {
			Ok({ namespace: NoNamespace, prefix: Unprefixed, local })
		} else {
			Err(InvalidCharacterError)
		}
	}

	## Whether a string is a valid DOM namespace prefix, independent of any
	## namespace binding. This is broader than an XML NCName.
	valid_namespace_prefix : Str -> Bool
	valid_namespace_prefix = |text| !text.is_empty() and !text.to_utf8().any(forbidden_prefix_byte)

	## Whether a string is a valid DOM attribute local name. Colons, quotes
	## and leading digits are permitted; an equals sign is not.
	valid_attribute_local_name : Str -> Bool
	valid_attribute_local_name = |text| !text.is_empty() and !text.to_utf8().any(|byte| forbidden_prefix_byte(byte) or byte == '=')

	## Whether a string is a valid DOM element local name. Names starting
	## with an ASCII letter have the DOM's broader HTML-compatible rules.
	valid_element_local_name : Str -> Bool
	valid_element_local_name = |text| {
		bytes = text.to_utf8()
		first = bytes.first() ?? 0
		if ascii_alpha(first) {
			return !bytes.any(forbidden_prefix_byte)
		}
		if first != ':' and first != '_' and first < 128 {
			return False
		}
		# Str is valid UTF-8. Every non-ASCII scalar is permitted, so checking
		# its individual bytes needs no additional Unicode decoder.
		!bytes.any(|byte| byte < 128 and !ascii_alpha(byte) and !(byte >= '0' and byte <= '9') and byte != '-' and byte != '.' and byte != ':' and byte != '_')
	}
}

forbidden_prefix_byte : U8 -> Bool
forbidden_prefix_byte = |byte| byte == 0 or byte == 9 or byte == 10 or byte == 12 or byte == 13 or byte == 32 or byte == '/' or byte == '>'

ascii_alpha : U8 -> Bool
ascii_alpha = |byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z')

lower_ascii : Str -> Str
lower_ascii = |text| {
	bytes = text.to_utf8().map(|byte| if (byte >= 'A' and byte <= 'Z') byte + 32 else byte)
	Str.from_utf8(bytes) ?? text
}

expect DomName.html_element("H:DIV") == Ok({ namespace: Uri("http://www.w3.org/1999/xhtml"), prefix: Unprefixed, local: "h:div" })
expect DomName.validate_and_extract(Uri("http://www.w3.org/1999/xhtml"), "h:div", Element) == Ok({ namespace: Uri("http://www.w3.org/1999/xhtml"), prefix: Prefix("h"), local: "div" })
expect DomName.unqualified_element("H:DIV") == Ok({ namespace: NoNamespace, prefix: Unprefixed, local: "H:DIV" })
expect DomName.unqualified_attribute("xlink:href") == Ok({ namespace: NoNamespace, prefix: Unprefixed, local: "xlink:href" })
expect DomName.validate_and_extract(Uri("http://www.w3.org/1999/xlink"), "xlink:href", Attribute) == Ok({ namespace: Uri("http://www.w3.org/1999/xlink"), prefix: Prefix("xlink"), local: "href" })
expect DomName.qualified({ namespace: NoNamespace, prefix: Unprefixed, local: "xlink:href" }) == "xlink:href"
expect DomName.qualified({ namespace: Uri("urn:x"), prefix: Prefix("xlink"), local: "href" }) == "xlink:href"
expect DomName.validate_and_extract(Uri("urn:new"), "FutureThing", Element) == Ok({ namespace: Uri("urn:new"), prefix: Unprefixed, local: "FutureThing" })
expect DomName.validate_and_extract(Uri(""), "A", Element) == Ok({ namespace: NoNamespace, prefix: Unprefixed, local: "A" })
expect DomName.validate_and_extract(Uri(""), "p:A", Element) == Err(NamespaceError)
expect DomName.validate_and_extract(NoNamespace, "p:1", Element) == Err(InvalidCharacterError)
expect DomName.validate_and_extract(Uri("urn:x"), "p:1", Attribute) == Ok({ namespace: Uri("urn:x"), prefix: Prefix("p"), local: "1" })
expect DomName.validate_and_extract(Uri("urn:x"), "p:a:b", Element) == Ok({ namespace: Uri("urn:x"), prefix: Prefix("p"), local: "a:b" })
expect DomName.validate_and_extract(Uri("urn:x"), "p::", Element) == Ok({ namespace: Uri("urn:x"), prefix: Prefix("p"), local: ":" })
expect DomName.validate_and_extract(Uri("urn:x"), ":a", Element) == Err(InvalidCharacterError)
expect DomName.validate_and_extract(Uri("urn:x"), "p:", Attribute) == Err(InvalidCharacterError)
expect DomName.validate_and_extract(Uri("urn:x"), "=p:a=b", Element) == Ok({ namespace: Uri("urn:x"), prefix: Prefix("=p"), local: "a=b" })
expect DomName.validate_and_extract(Uri("urn:x"), "xml:a", Element) == Err(NamespaceError)
expect DomName.validate_and_extract(Uri(XmlNs.xml_namespace), "xml:a", Element) == Ok({ namespace: Uri(XmlNs.xml_namespace), prefix: Prefix("xml"), local: "a" })
expect DomName.validate_and_extract(Uri(XmlNs.xml_namespace), "other:a", Element) == Ok({ namespace: Uri(XmlNs.xml_namespace), prefix: Prefix("other"), local: "a" })
expect DomName.validate_and_extract(NoNamespace, "xmlns", Attribute) == Err(NamespaceError)
expect DomName.unqualified_attribute("xmlns") == Ok({ namespace: NoNamespace, prefix: Unprefixed, local: "xmlns" })
expect DomName.validate_and_extract(Uri(XmlNs.xmlns_namespace), "xmlns", Attribute) == Ok({ namespace: Uri(XmlNs.xmlns_namespace), prefix: Unprefixed, local: "xmlns" })
expect DomName.validate_and_extract(Uri(XmlNs.xmlns_namespace), "xmlns:p", Attribute) == Ok({ namespace: Uri(XmlNs.xmlns_namespace), prefix: Prefix("xmlns"), local: "p" })
expect DomName.validate_and_extract(Uri(XmlNs.xmlns_namespace), "p:a", Element) == Err(NamespaceError)
expect DomName.validate_and_extract(Uri("urn:x"), "XML:a", Element) == Ok({ namespace: Uri("urn:x"), prefix: Prefix("XML"), local: "a" })
expect DomName.valid_element_local_name("a=b") and !DomName.valid_attribute_local_name("a=b")
expect DomName.valid_element_local_name("a<\"'`=b") and DomName.valid_attribute_local_name("1<\"'`:b")
expect !DomName.valid_element_local_name("1a") and !DomName.valid_element_local_name("_a=b") and !DomName.valid_element_local_name("é=b")
expect DomName.valid_element_local_name("é:a-1._😀") and DomName.valid_element_local_name("a\u(0001)")
expect !DomName.valid_element_local_name("") and !DomName.valid_attribute_local_name("") and !DomName.valid_namespace_prefix("")
expect ["\u(0000)", "\t", "\n", "\u(000c)", "\r", " ", "/", ">"].all(|text| !DomName.valid_element_local_name("a${text}b") and !DomName.valid_attribute_local_name("a${text}b") and !DomName.valid_namespace_prefix("a${text}b"))
expect DomName.valid_namespace_prefix("=:'\"<\u(007f)") and DomName.valid_attribute_local_name("\u(000b)")
expect DomName.validate_and_extract(Uri("urn:x"), "xml:a/b", Element) == Err(InvalidCharacterError)
expect DomName.html_element("É:DIV").map_ok(|name| name.local) == Ok("É:div")
expect {
	DomName.validate_and_extract(Uri("http://www.w3.org/2000/svg"), "a=b", Element).is_ok() and XmlNs.parse_str("<a=b/>").is_err() and XmlNs.parse_str("<p:a:b xmlns:p='urn:x'/>").is_err()
}

expect DomName.valid_element_local_name("\u(80)\u(FFFE)\u(10FFFF)") and DomName.valid_attribute_local_name("\u(FFFE)")
expect !DomName.valid_element_local_name("\u(7F)") and DomName.valid_element_local_name("a\u(7F)")
