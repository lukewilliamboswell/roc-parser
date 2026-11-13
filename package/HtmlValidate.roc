import Markup
import MarkupValidate
import HtmlTokenizer
import DomName
import MarkupVocabulary
import Literal

## HTML authoring checks, separate from browser tree recovery. Embedded CSS and
## JavaScript remain source text rather than claims of validated programs.
HtmlValidate :: [].{
	validate : Markup.Parsed, Markup.Props, List(MarkupValidate.Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	validate = |parsed, props, holes| {
		var $diagnostics = parsed.diagnostics
		hole_positions = holes.fold(Dict.empty(), |dict, hole| dict.insert((hole.node, hole.attribute), True))
		var $pending = parsed.tree.roots.map(|index| { index, parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup({ problem: Unsupported("invalid tree index"), line: 1, column: 1 }))
			match node.kind {
				Element(name) if name.namespace == Uri(Markup.html_namespace) => {
					custom = custom_element(name.local)
					if !elements(name.local) and !custom {
						if !props.allow_unknown_elements {
							return fail(parsed, node, UnknownElement(name.local))
						}
						offset = match node.origin {
							Authored(span) => span.start
							Implied(at) => at
							Cloned(info) => info.source.start
						}
						at = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
						if $diagnostics.len() >= props.max_diagnostics {
							return fail(parsed, node, ResourceLimit("max_diagnostics"))
						}
						$diagnostics = $diagnostics.append({ problem: UnknownElement(name.local), line: at.line, column: at.column })
					}
					var $at = 0
					for attr in node.attributes {
						known = match attr.name.namespace {
							NoNamespace => allowed(name.local, attr.name.local) or custom
							Uri("http://www.w3.org/2000/xmlns/") => True
							Uri("http://www.w3.org/XML/1998/namespace") => (
								match attr.name.local {
									"lang" | "space" | "base" | "id" => True
									_ => False
								}
							)
							_ => False
						}
						if !known {
							if !props.allow_unknown_attributes {
								return attr_fail(parsed, attr, UnknownAttribute({ element: name.local, attribute: attr.name.local }))
							}
							at = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), attr.span.start)
							if $diagnostics.len() >= props.max_diagnostics {
								return attr_fail(parsed, attr, ResourceLimit("max_diagnostics"))
							}
							$diagnostics = $diagnostics.append({ problem: UnknownAttribute({ element: name.local, attribute: attr.name.local }), line: at.line, column: at.column })
						}
						if props.check_values and known and attr.name.namespace == NoNamespace and !hole_positions.contains((item.index, $at)) {
							expected = grammar(name.local, attr.name.local)
							if !valid(attr.value, expected) {
								return attr_fail(parsed, attr, InvalidValue({ attribute: attr.name.local, value: attr.value, expected }))
							}
						}
						$at = $at + 1
					}
					if name.local == "a" and item.anchor {
						return fail(parsed, node, InvalidChildren({ element: "a", expected: "no descendant a element" }))
					}
					if item.interactive and (interactive(node, name.local) or has_attribute(node, "tabindex")) {
						return fail(parsed, node, InvalidChildren({ element: "button", expected: "no nested interactive content" }))
					}
					if item.in_option and (
						interactive(node, name.local) or (
							match name.local {
								"datalist" | "object" => True
								_ => False
							}
						) or has_attribute(node, "tabindex")
					) {
						return fail(parsed, node, InvalidChildren({ element: "option", expected: "non-interactive option content without tabindex" }))
					}
					if name.local == "selectedcontent" and !item.selected_context {
						return fail(parsed, node, InvalidChildren({ element: "selectedcontent", expected: "descendant of a button that is a child of select" }))
					}
					check_children(parsed, node, name.local, item.in_datalist)?
					allowed_parent = match name.local {
						"li" => (
							match item.parent {
								"ul" | "ol" | "menu" => True
								_ => False
							}
						)
						"dt" | "dd" => item.parent == "dl" or item.parent == "div"
						"tr" => (
							match item.parent {
								"table" | "thead" | "tbody" | "tfoot" => True
								_ => False
							}
						)
						"td" | "th" => item.parent == "tr"
						"thead" | "tbody" | "tfoot" | "caption" | "colgroup" => item.parent == "table"
						"col" => item.parent == "colgroup"
						"option" => item.option_context
						"optgroup" => item.option_context
						"summary" => item.parent == "details"
						"legend" => (
							match item.parent {
								"fieldset" | "optgroup" => True
								_ => False
							}
						)
						"source" => (
							match item.parent {
								"audio" | "video" | "picture" => True
								_ => False
							}
						)
						"track" => (
							match item.parent {
								"audio" | "video" => True
								_ => False
							}
						)
						_ => True
					}
					# A fragment has a declared context, and standalone row/option
					# fragments are legal in that context. Parent checks apply
					# only where an authored parent is available.
					if item.parent != "" and !allowed_parent {
						return fail(parsed, node, InvalidChildren({ element: item.parent, expected: "allowed parent for ${name.local}" }))
					}
					if name.local == "img" and !has_attribute(node, "alt") and !(
						match value(node, "role") {
							"presentation" | "none" => True
							_ => False
						}
					) {
						return fail(parsed, node, InvalidChildren({ element: "img", expected: "alt attribute (or explicit presentation role)" }))
					}
					if name.local == "input" and HtmlTokenizer.lower(value(node, "type")) == "image" and (!has_attribute(node, "src") or !has_attribute(node, "alt")) {
						return fail(parsed, node, InvalidChildren({ element: "input", expected: "image input requires src and alt" }))
					}
					for child in node.children {
						$pending = $pending.append({
							index: child,
							parent: name.local,
							interactive: item.interactive or name.local == "button",
							anchor: item.anchor or name.local == "a",
							option_context: item.option_context or (
								match name.local {
									"select" | "optgroup" | "datalist" => True
									_ => False
								}
							),
							in_option: item.in_option or name.local == "option",
							in_datalist: item.in_datalist or name.local == "datalist",
							selected_context: item.selected_context or (name.local == "button" and item.parent == "select"),
						})
					}
				}
				Element(_) => {
					for child in node.children {
						$pending = $pending.append({ ..item, index: child, parent: "" })
					}
				}
				_ => {}
			}
		}
		Ok({ ..parsed, diagnostics: $diagnostics })
	}
	element_known : Str -> Bool
	element_known = elements
	custom_element_name : Str -> Bool
	custom_element_name = custom_element
	boolean_attribute : Str, Str -> Bool
	boolean_attribute = |element, attr| allowed(element, attr) and (booleans(attr) or (element == "iframe" and attr == "credentialless")) and ((attr != "playsinline" and attr != "disablepictureinpicture") or element == "video")
	attribute_known : Str, Markup.Name -> Bool
	attribute_known = |element, name| match name.namespace {
		NoNamespace => allowed(element, name.local)
		Uri("http://www.w3.org/2000/xmlns/") => True
		Uri("http://www.w3.org/XML/1998/namespace") => ["lang", "space", "base", "id"].contains(name.local)
		_ => False
	}

	## Known finite keyword values for an applicable HTML attribute. Metadata
	## does not change the ordinary author's value validation or supply defaults.
	enumerated_values : Str, Str -> Try(List(Str), [NotEnumerated])
	enumerated_values = |element, attr| {
		if !allowed(element, attr) or boolean_attribute(element, attr) or booleans(attr) or (attr == "scope" and element != "th") {
			return Err(NotEnumerated)
		}
		match MarkupVocabulary.aria_enumerated_values(attr) {
			Ok(values) => return Ok(values)
			Err(_) => {}
		}
		extra = match attr {
			"autocapitalize" => Ok(["off", "none", "on", "sentences", "words", "characters"])
			"autocorrect" => Ok(["", "on", "off"])
			"enterkeyhint" => Ok(["enter", "done", "go", "next", "previous", "search", "send"])
			"inputmode" => Ok(["none", "text", "tel", "url", "email", "numeric", "decimal", "search"])
			"popover" => Ok(["", "auto", "manual", "hint"])
			"spellcheck" | "writingsuggestions" => Ok(["", "true", "false"])
			"virtualkeyboardpolicy" => Ok(["auto", "manual"])
			"referrerpolicy" => Ok(["", "no-referrer", "no-referrer-when-downgrade", "same-origin", "origin", "strict-origin", "origin-when-cross-origin", "strict-origin-when-cross-origin", "unsafe-url"])
			"shadowrootmode" if element == "template" => Ok(["open", "closed"])
			"closedby" if element == "dialog" => Ok(["any", "closerequest", "none"])
			"popovertargetaction" if element == "button" or element == "input" => Ok(["toggle", "show", "hide"])
			"autocomplete" if element == "form" => Ok(["on", "off"])
			"capture" if element == "input" => Ok(["user", "environment"])
			"shape" if element == "area" => Ok(["default", "rect", "circle", "poly"])
			_ => Err(NotEnumerated)
		}
		match extra {
			Ok(values) => Ok(values)
			Err(_) => match grammar(element, attr) {
				Keyword(values) => Ok(values)
				_ => Err(NotEnumerated)
			}
		}
	}

	grammar : Str, Str -> Markup.ValueGrammar
	grammar = |element, attr| {
		if !allowed(element, attr) {
			return Language("extension attribute")
		}
		if booleans(attr) {
			return Keyword(["", attr])
		}
		match attr {
			"hidden" => Keyword(["", "hidden", "until-found"])
			"contenteditable" => Keyword(["", "true", "false", "plaintext-only"])
			"draggable" | "spellcheck" | "translate" => Keyword(if (attr == "translate") ["", "yes", "no"] else ["true", "false"])
			"dir" => Keyword(["ltr", "rtl", "auto"])
			"loading" => Keyword(["lazy", "eager"])
			"decoding" => Keyword(["sync", "async", "auto"])
			"crossorigin" => Keyword(["", "anonymous", "use-credentials"])
			"fetchpriority" => Keyword(["high", "low", "auto"])
			"preload" => Keyword(["", "none", "metadata", "auto"])
			"wrap" => Keyword(["soft", "hard"])
			"method" | "formmethod" => Keyword(["get", "post", "dialog"])
			"enctype" | "formenctype" => Keyword(["application/x-www-form-urlencoded", "multipart/form-data", "text/plain"])
			"type" if element == "input" => Keyword(["hidden", "text", "search", "tel", "url", "email", "password", "date", "month", "week", "time", "datetime-local", "number", "range", "color", "checkbox", "radio", "file", "submit", "image", "reset", "button"])
			"type" if element == "button" => Keyword(["submit", "reset", "button"])
			"type" if element == "ol" => Keyword(["1", "a", "A", "i", "I"])
			"scope" => Keyword(["row", "col", "rowgroup", "colgroup"])
			"kind" if element == "track" => Keyword(["subtitles", "captions", "descriptions", "chapters", "metadata"])
			"aria-hidden" | "aria-expanded" | "aria-selected" | "aria-disabled" | "aria-multiline" | "aria-modal" | "aria-required" | "aria-readonly" => Keyword(["true", "false"])
			"aria-checked" | "aria-pressed" => Keyword(["true", "false", "mixed", "undefined"])
			"aria-live" => Keyword(["off", "polite", "assertive"])
			"width" | "height" | "cols" | "rows" | "colspan" | "rowspan" | "size" | "maxlength" | "minlength" | "span" => NonnegativeNumber
			"tabindex" | "start" => Number
			"class" | "rel" | "headers" | "sandbox" | "aria-labelledby" | "aria-describedby" => TokenList
			_ => Language("HTML attribute")
		}
	}
}

valid = |text, expected| match expected {
	Keyword(values) => values.any(|v| HtmlTokenizer.lower(text) == HtmlTokenizer.lower(v))
	Number => I64.from_str(text).is_ok()
	NonnegativeNumber => U64.from_str(text).is_ok()
	_ => True
}

custom_element = |name| DomName.valid_element_local_name(name) and name.contains("-") and name.to_utf8().first().map_ok(|b| b >= 'a' and b <= 'z') == Ok(True) and !name.to_utf8().any(|b| b >= 'A' and b <= 'Z') and !(
	match name {
		"annotation-xml" | "color-profile" | "font-face" | "font-face-src" | "font-face-uri" | "font-face-format" | "font-face-name" | "missing-glyph" => True
		_ => False
	}
)

allowed = |element, attr| global(attr) or (attr.starts_with("data-") and attr.count_utf8_bytes() > 5 and !attr.contains(":")) or aria(attr) or events(attr) or (element == "input" and attr == "onsearch") or attributes(element, attr)

has_attribute = |node, key| node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == key)

value = |node, key| node.attributes.find_first(|a| a.name.namespace == NoNamespace and a.name.local == key).map_ok(|a| a.value) ?? ""

fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(s) => s.start
		Implied(o) => o
		Cloned(info) => info.source.start
	}
	p = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
	Err(InvalidMarkup({ problem, line: p.line, column: p.column }))
}

attr_fail = |parsed, attr, problem| {
	p = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), attr.span.start)
	Err(InvalidMarkup({ problem, line: p.line, column: p.column }))
}

elements = |name| match name {
	"a" | "abbr" | "address" | "area" | "article" | "aside" | "audio" | "b" | "base" | "bdi" | "bdo" | "blockquote" | "body" | "br" | "button" | "canvas" | "caption" | "cite" | "code" | "col" | "colgroup" | "data" | "datalist" | "dd" | "del" | "details" | "dfn" | "dialog" | "div" | "dl" | "dt" | "em" | "embed" | "fieldset" | "figcaption" | "figure" | "footer" | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "head" | "header" | "hgroup" | "hr" | "html" | "i" | "iframe" | "img" | "input" | "ins" | "kbd" | "label" | "legend" | "li" | "link" | "main" | "map" | "mark" | "menu" | "meta" | "meter" | "nav" | "noscript" | "object" | "ol" | "optgroup" | "option" | "output" | "p" | "picture" | "pre" | "progress" | "q" | "rp" | "rt" | "ruby" | "s" | "samp" | "script" | "search" | "section" | "select" | "selectedcontent" | "slot" | "small" | "source" | "span" | "strong" | "style" | "sub" | "summary" | "sup" | "table" | "tbody" | "td" | "template" | "textarea" | "tfoot" | "th" | "thead" | "time" | "title" | "tr" | "track" | "u" | "ul" | "var" | "video" | "wbr" => True
	_ => False
}

global = |name| match name {
	"accesskey" | "autocapitalize" | "autocorrect" | "autofocus" | "class" | "contenteditable" | "dir" | "draggable" | "enterkeyhint" | "exportparts" | "hidden" | "id" | "inert" | "inputmode" | "is" | "itemid" | "itemprop" | "itemref" | "itemscope" | "itemtype" | "lang" | "nonce" | "part" | "popover" | "role" | "slot" | "spellcheck" | "style" | "tabindex" | "title" | "translate" | "virtualkeyboardpolicy" | "writingsuggestions" | "xmlns" => True
	_ => False
}

booleans = |name| match name {
	"allowfullscreen" | "async" | "autofocus" | "autoplay" | "checked" | "controls" | "default" | "defer" | "disabled" | "disablepictureinpicture" | "disableremoteplayback" | "formnovalidate" | "inert" | "ismap" | "itemscope" | "loop" | "multiple" | "muted" | "nomodule" | "novalidate" | "open" | "playsinline" | "readonly" | "required" | "reversed" | "selected" | "shadowrootclonable" | "shadowrootdelegatesfocus" | "shadowrootserializable" => True
	_ => False
}

aria = MarkupVocabulary.aria_name

events = MarkupVocabulary.html_event_name

attributes = |element, attr| match element {
	"a" | "area" => match attr {
		"alt" | "coords" | "shape" | "download" | "href" | "hreflang" | "ping" | "referrerpolicy" | "rel" | "target" | "type" => True
		_ => False
	}
	"audio" | "video" => match attr {
		"autoplay" | "controls" | "controlslist" | "crossorigin" | "disableremoteplayback" | "disablepictureinpicture" | "height" | "loop" | "muted" | "playsinline" | "poster" | "preload" | "src" | "width" => True
		_ => False
	}
	"base" => match attr {
		"href" | "target" => True
		_ => False
	}
	"blockquote" | "q" => match attr {
		"cite" => True
		_ => False
	}
	"button" => match attr {
		"command" | "commandfor" | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "name" | "popovertarget" | "popovertargetaction" | "type" | "value" => True
		_ => False
	}
	"canvas" => match attr {
		"height" | "width" => True
		_ => False
	}
	"col" | "colgroup" => match attr {
		"span" => True
		_ => False
	}
	"data" => match attr {
		"value" => True
		_ => False
	}
	"del" | "ins" => match attr {
		"cite" | "datetime" => True
		_ => False
	}
	"details" => match attr {
		"name" | "open" => True
		_ => False
	}
	"dialog" => match attr {
		"closedby" | "open" => True
		_ => False
	}
	"embed" => match attr {
		"height" | "src" | "type" | "width" => True
		_ => False
	}
	"fieldset" => match attr {
		"disabled" | "form" | "name" => True
		_ => False
	}
	"form" => match attr {
		"accept-charset" | "action" | "autocomplete" | "enctype" | "method" | "name" | "novalidate" | "rel" | "target" => True
		_ => False
	}
	"html" => match attr {
		"xmlns" => True
		_ => False
	}
	"iframe" => match attr {
		"allow" | "allowfullscreen" | "credentialless" | "csp" | "height" | "loading" | "name" | "referrerpolicy" | "sandbox" | "src" | "srcdoc" | "width" => True
		_ => False
	}
	"img" => match attr {
		"alt" | "attributionsrc" | "crossorigin" | "decoding" | "elementtiming" | "fetchpriority" | "height" | "ismap" | "loading" | "referrerpolicy" | "sizes" | "src" | "srcset" | "usemap" | "width" => True
		_ => False
	}
	"input" => match attr {
		"accept" | "alt" | "autocomplete" | "capture" | "checked" | "dirname" | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "height" | "list" | "max" | "maxlength" | "min" | "minlength" | "multiple" | "name" | "pattern" | "placeholder" | "popovertarget" | "popovertargetaction" | "readonly" | "required" | "size" | "src" | "step" | "type" | "value" | "width" => True
		_ => False
	}
	"label" => match attr {
		"for" | "form" => True
		_ => False
	}
	"li" => match attr {
		"value" => True
		_ => False
	}
	"link" => match attr {
		"as" | "blocking" | "color" | "crossorigin" | "disabled" | "fetchpriority" | "href" | "hreflang" | "imagesizes" | "imagesrcset" | "integrity" | "media" | "referrerpolicy" | "rel" | "sizes" | "type" => True
		_ => False
	}
	"map" => match attr {
		"name" => True
		_ => False
	}
	"meta" => match attr {
		"charset" | "content" | "http-equiv" | "media" | "name" => True
		_ => False
	}
	"meter" => match attr {
		"form" | "high" | "low" | "max" | "min" | "optimum" | "value" => True
		_ => False
	}
	"object" => match attr {
		"data" | "form" | "height" | "name" | "type" | "usemap" | "width" => True
		_ => False
	}
	"ol" => match attr {
		"reversed" | "start" | "type" => True
		_ => False
	}
	"optgroup" => match attr {
		"disabled" | "label" => True
		_ => False
	}
	"option" => match attr {
		"disabled" | "label" | "selected" | "value" => True
		_ => False
	}
	"output" => match attr {
		"for" | "form" | "name" => True
		_ => False
	}
	"progress" => match attr {
		"max" | "value" => True
		_ => False
	}
	"script" => match attr {
		"async" | "attributionsrc" | "blocking" | "crossorigin" | "defer" | "fetchpriority" | "integrity" | "nomodule" | "nonce" | "referrerpolicy" | "src" | "type" => True
		_ => False
	}
	"select" => match attr {
		"autocomplete" | "disabled" | "form" | "multiple" | "name" | "required" | "size" => True
		_ => False
	}
	"slot" => match attr {
		"name" => True
		_ => False
	}
	"source" => match attr {
		"height" | "media" | "sizes" | "src" | "srcset" | "type" | "width" => True
		_ => False
	}
	"style" => match attr {
		"blocking" | "media" | "nonce" | "title" => True
		_ => False
	}
	"td" | "th" => match attr {
		"abbr" | "colspan" | "headers" | "rowspan" | "scope" => True
		_ => False
	}
	"template" => match attr {
		"shadowrootclonable" | "shadowrootdelegatesfocus" | "shadowrootmode" | "shadowrootserializable" => True
		_ => False
	}
	"textarea" => match attr {
		"autocomplete" | "cols" | "dirname" | "disabled" | "form" | "maxlength" | "minlength" | "name" | "placeholder" | "readonly" | "required" | "rows" | "wrap" => True
		_ => False
	}
	"time" => match attr {
		"datetime" => True
		_ => False
	}
	"track" => match attr {
		"default" | "kind" | "label" | "src" | "srclang" => True
		_ => False
	}
	_ => False
}

interactive = |node, name| (
	match name {
		"button" | "details" | "embed" | "iframe" | "label" | "select" | "textarea" => True
		_ => False
	}
) or (name == "input" and HtmlTokenizer.lower(value(node, "type")) != "hidden") or (name == "a" and has_attribute(node, "href")) or (
	(
		match name {
			"audio" | "video" => True
			_ => False
		}
	) and has_attribute(node, "controls")
) or (name == "img" and has_attribute(node, "usemap"))

check_children : Markup.Parsed, Markup.Node, Str, Bool -> Try({}, [InvalidMarkup(Markup.Error)])
check_children = |parsed, node, name, in_datalist| {
	allowed_children = match name {
		"ul" | "ol" | "menu" => ["li", "script", "template"]
		"dl" => ["dt", "dd", "div", "script", "template"]
		"table" => ["caption", "colgroup", "thead", "tbody", "tfoot", "tr", "script", "template"]
		"thead" | "tbody" | "tfoot" => ["tr", "script", "template"]
		"tr" => ["td", "th", "script", "template"]
		"colgroup" => ["col", "script", "template"]
		"select" => ["button", "option", "optgroup", "hr", "div", "script", "template", "noscript"]
		"optgroup" => ["legend", "option", "div", "script", "template", "noscript"]
		_ => []
	}
	nothing = void_elements(name) or name == "selectedcontent" or (name == "option" and has_attribute(node, "label") and has_attribute(node, "value"))
	text_only = name == "option" and (has_attribute(node, "label") or in_datalist)
	for index in node.children {
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported("invalid child node"))
		derived = match child.origin {
			Cloned(_) => True
			_ => False
		}
		match child.kind {
			Element(n) => {
				if !(name == "selectedcontent" and derived) and (nothing or text_only or (!allowed_children.is_empty() and (n.namespace != Uri(Markup.html_namespace) or !allowed_children.contains(n.local)))) {
					return fail(parsed, child, InvalidChildren({ element: name, expected: if (nothing) "no authored child content" else Str.join_with(allowed_children, ", ") }))
				}
			}
			Text(text) if !Markup.whitespace(text) => {
				if !(name == "selectedcontent" and derived) and (nothing or !allowed_children.is_empty()) {
					return fail(parsed, child, InvalidChildren({ element: name, expected: "element-only content" }))
				}
			}
			_ => {}
		}
	}
	Ok({})
}

void_elements = |name| match name {
	"area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "param" | "source" | "track" | "wbr" => True
	_ => False
}

# Metadata covers current names without tightening the ordinary value grammar.
expect HtmlValidate.boolean_attribute("iframe", "credentialless")
expect !HtmlValidate.boolean_attribute("div", "credentialless")
expect HtmlValidate.boolean_attribute("video", "playsinline") and HtmlValidate.boolean_attribute("video", "disablepictureinpicture")
expect !HtmlValidate.boolean_attribute("audio", "playsinline") and !HtmlValidate.boolean_attribute("audio", "disablepictureinpicture")
expect HtmlValidate.enumerated_values("audio", "playsinline") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("div", "popover") == Ok(["", "auto", "manual", "hint"])
expect HtmlValidate.enumerated_values("div", "inputmode").map_ok(|values| values.contains("decimal")) == Ok(True)
expect HtmlValidate.enumerated_values("template", "shadowrootmode") == Ok(["open", "closed"])
expect HtmlValidate.enumerated_values("div", "shadowrootmode") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("form", "autocomplete") == Ok(["on", "off"])
expect HtmlValidate.enumerated_values("input", "autocomplete") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("input", "capture") == Ok(["user", "environment"])
expect HtmlValidate.enumerated_values("dialog", "closedby") == Ok(["any", "closerequest", "none"])
expect HtmlValidate.enumerated_values("div", "aria-hidden") == Ok(["true", "false", "undefined"])
expect HtmlValidate.grammar("div", "inputmode") == Language("HTML attribute")
expect HtmlValidate.custom_element_name("x-a=b") and HtmlValidate.custom_element_name("x:a-b") and HtmlValidate.custom_element_name("x-<")
expect HtmlValidate.custom_element_name("x-\u(80)") and HtmlValidate.custom_element_name("x-\u(F0000)")
expect !HtmlValidate.custom_element_name("x-a>b") and !HtmlValidate.custom_element_name("x-a b") and !HtmlValidate.custom_element_name("x-\u(0)")
expect !HtmlValidate.custom_element_name("x-Ab") and !HtmlValidate.custom_element_name("font-face") and !HtmlValidate.custom_element_name("nohyphen")

expect HtmlValidate.enumerated_values("td", "scope") == Err(NotEnumerated) and HtmlValidate.enumerated_values("th", "scope") == Ok(["row", "col", "rowgroup", "colgroup"])
expect HtmlValidate.grammar("iframe", "credentialless") == Language("HTML attribute")
