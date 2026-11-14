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
					known_element = elements(name.local)
					custom = !known_element and custom_element(name.local)
					if !known_element and !custom {
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
					ancestry = context_of(item)
					attributes = if (ancestry.interactive or ancestry.in_option or name.local == "option") summarize_attributes(node.attributes) else empty_attributes
					match context_problem(ancestry, name.local, attributes) {
						Ok(_) => {}
						Err(problem) => return fail(parsed, node, problem)
					}
					check_children_with_attributes(parsed, node, name.local, item.in_datalist, attributes)?
					# Standalone fragment roots have no authored parent.
					if item.parent != "" and !parent_allowed(ancestry, name.local) {
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
					next_context = descend(ancestry, name)
					for child in node.children {
						$pending = $pending.append(context_frame(child, next_context))
					}
				}
				Element(name) => {
					next_context = descend(context_of(item), name)
					for child in node.children {
						$pending = $pending.append(context_frame(child, next_context))
					}
				}
				_ => {}
			}
		}
		Ok({ ..parsed, diagnostics: $diagnostics })
	}

	# Internal finite authoring state shared with constructor-context queries.
	ContextState : { parent : Str, interactive : Bool, anchor : Bool, option_context : Bool, in_option : Bool, in_datalist : Bool, selected_context : Bool }
	empty_context : ContextState
	empty_context = { parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False }
	AttributeSummary : { href : Bool, tabindex : Bool, hidden_input : Bool, controls : Bool, usemap : Bool, label : Bool, value : Bool, html_encoding : Bool, font_breakout : Bool }
	empty_attributes : AttributeSummary
	empty_attributes = { href: False, tabindex: False, hidden_input: False, controls: False, usemap: False, label: False, value: False, html_encoding: False, font_breakout: False }
	summarize_attributes : List({ name : Markup.Name, value : Str, ..extra }) -> AttributeSummary
	summarize_attributes = |attributes| {
		var $seen_type = False
		var $seen_encoding = False
		var $summary = empty_attributes
		for attribute in attributes {
			if attribute.name.namespace == NoNamespace {
				match attribute.name.local {
					"href" => {
						$summary = { ..$summary, href: True }
					}
					"tabindex" => {
						$summary = { ..$summary, tabindex: True }
					}
					"type" if !$seen_type => {
						$seen_type = True
						$summary = { ..$summary, hidden_input: HtmlTokenizer.lower(attribute.value) == "hidden" }
					}
					"controls" => {
						$summary = { ..$summary, controls: True }
					}
					"usemap" => {
						$summary = { ..$summary, usemap: True }
					}
					"label" => {
						$summary = { ..$summary, label: True }
					}
					"value" => {
						$summary = { ..$summary, value: True }
					}
					"encoding" if !$seen_encoding => {
						$seen_encoding = True
						$summary = { ..$summary, html_encoding: ["text/html", "application/xhtml+xml"].contains(HtmlTokenizer.lower(attribute.value)) }
					}
					"color" | "face" | "size" => {
						$summary = { ..$summary, font_breakout: True }
					}
					_ => {}
				}
			}
		}
		$summary
	}
	context_problem : ContextState, Str, AttributeSummary -> Try({}, [InvalidChildren({ element : Str, expected : Str })])
	context_problem = |context, element, attributes| {
		if element == "a" and context.anchor {
			return Err(InvalidChildren({ element: "a", expected: "no descendant a element" }))
		}
		if context.interactive and (interactive_summary(attributes, element) or attributes.tabindex) {
			return Err(InvalidChildren({ element: "button", expected: "no nested interactive content" }))
		}
		if context.in_option and (interactive_summary(attributes, element) or ["datalist", "object"].contains(element) or attributes.tabindex) {
			return Err(InvalidChildren({ element: "option", expected: "non-interactive option content without tabindex" }))
		}
		if element == "selectedcontent" and !context.selected_context {
			return Err(InvalidChildren({ element: "selectedcontent", expected: "descendant of a button that is a child of select" }))
		}
		Ok({})
	}
	descend : ContextState, Markup.Name -> ContextState
	descend = |context, element| if element.namespace == Uri(Markup.html_namespace) {
		{ parent: element.local, interactive: context.interactive or element.local == "button", anchor: context.anchor or element.local == "a", option_context: context.option_context or ["select", "optgroup", "datalist"].contains(element.local), in_option: context.in_option or element.local == "option", in_datalist: context.in_datalist or element.local == "datalist", selected_context: context.selected_context or (element.local == "button" and context.parent == "select") }
	} else { ..context, parent: "" }
	parent_allowed : ContextState, Str -> Bool
	parent_allowed = |context, element| match element {
		"li" => ["ul", "ol", "menu"].contains(context.parent)
		"dt" | "dd" => context.parent == "dl" or context.parent == "div"
		"tr" => ["table", "thead", "tbody", "tfoot"].contains(context.parent)
		"td" | "th" => context.parent == "tr"
		"thead" | "tbody" | "tfoot" | "caption" | "colgroup" => context.parent == "table"
		"col" => context.parent == "colgroup"
		"option" | "optgroup" => context.option_context
		"summary" => context.parent == "details"
		"legend" => context.parent == "fieldset" or context.parent == "optgroup"
		"source" => ["audio", "video", "picture"].contains(context.parent)
		"track" => context.parent == "audio" or context.parent == "video"
		_ => True
	}
	ChildRule : [AnyChildren, NoChildren, TextOnly, ElementNames(List(Str))]
	child_rule : Str, AttributeSummary, Bool -> ChildRule
	child_rule = |element, attributes, in_datalist| {
		if void_elements(element) or element == "selectedcontent" or (element == "option" and attributes.label and attributes.value) {
			return NoChildren
		}
		if element == "option" and (attributes.label or in_datalist) {
			return TextOnly
		}
		match element {
			"ul" | "ol" | "menu" => ElementNames(["li", "script", "template"])
			"dl" => ElementNames(["dt", "dd", "div", "script", "template"])
			"table" => ElementNames(["caption", "colgroup", "thead", "tbody", "tfoot", "tr", "script", "template"])
			"thead" | "tbody" | "tfoot" => ElementNames(["tr", "script", "template"])
			"tr" => ElementNames(["td", "th", "script", "template"])
			"colgroup" => ElementNames(["col", "script", "template"])
			"select" => ElementNames(["button", "option", "optgroup", "hr", "div", "script", "template", "noscript"])
			"optgroup" => ElementNames(["legend", "option", "div", "script", "template", "noscript"])
			_ => AnyChildren
		}
	}
	child_expected : ChildRule, Markup.Name -> Try({}, [Rejected(Str)])
	child_expected = |rule, element| match rule {
		AnyChildren => Ok({})
		NoChildren => Err(Rejected("no authored child content"))
		TextOnly => Err(Rejected(""))
		ElementNames(names) => if (element.namespace == Uri(Markup.html_namespace) and names.contains(element.local)) Ok({}) else Err(Rejected(Str.join_with(names, ", ")))
	}
	text_expected : ChildRule, Str -> Try({}, [Rejected(Str)])
	text_expected = |rule, text| if (Markup.whitespace(text)) Ok({}) else match rule {
		NoChildren | ElementNames(_) => Err(Rejected("element-only content"))
		_ => Ok({})
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

allowed = |element, attr| global(attr) or attributes(element, attr) or (attr.starts_with("data-") and attr.count_utf8_bytes() > 5 and !attr.contains(":")) or aria(attr) or events(attr) or (element == "input" and attr == "onsearch")

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

elements = MarkupVocabulary.html_element_name

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

interactive_summary = |attribute_facts, name| ["button", "details", "embed", "iframe", "label", "select", "textarea"].contains(name) or (name == "input" and !attribute_facts.hidden_input) or (name == "a" and attribute_facts.href) or (["audio", "video"].contains(name) and attribute_facts.controls) or (name == "img" and attribute_facts.usemap)

context_of = |item| { parent: item.parent, interactive: item.interactive, anchor: item.anchor, option_context: item.option_context, in_option: item.in_option, in_datalist: item.in_datalist, selected_context: item.selected_context }

check_children : Markup.Parsed, Markup.Node, Str, Bool -> Try({}, [InvalidMarkup(Markup.Error)])
check_children = |parsed, node, name, in_datalist| check_children_with_attributes(parsed, node, name, in_datalist, if (name == "option") HtmlValidate.summarize_attributes(node.attributes) else HtmlValidate.empty_attributes)

check_children_with_attributes = |parsed, node, name, in_datalist, attribute_facts| {
	rule = HtmlValidate.child_rule(name, attribute_facts, in_datalist)
	if rule == AnyChildren {
		node_count = parsed.tree.nodes.len()
		if node.children.any(|index| index >= node_count) {
			return fail(parsed, node, Unsupported("invalid child node"))
		}
		return Ok({})
	}
	for index in node.children {
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported("invalid child node"))
		derived = match child.origin {
			Cloned(_) => True
			_ => False
		}
		if !(name == "selectedcontent" and derived) {
			match child.kind {
				Element(n) => match HtmlValidate.child_expected(rule, n) {
					Ok(_) => {}
					Err(Rejected(expected)) => return fail(parsed, child, InvalidChildren({ element: name, expected }))
				}
				Text(text) => match HtmlValidate.text_expected(rule, text) {
					Ok(_) => {}
					Err(Rejected(expected)) => return fail(parsed, child, InvalidChildren({ element: name, expected }))
				}
				_ => {}
			}
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

expect {
	parent : Markup.Node
	parent = { kind: Element({ local: "p", namespace: Uri(Markup.html_namespace), prefix: Unprefixed }), attributes: [], children: [1], origin: Authored({ start: 0, end: 8 }) }
	child : Markup.Node
	child = { kind: Text("x"), attributes: [], children: [], origin: Authored({ start: 3, end: 4 }) }
	parsed : Markup.Parsed
	parsed = { source: Markup.Source.from_bytes("<p>x</p>".to_utf8()), tree: { nodes: [parent, child], roots: [0] }, diagnostics: [] }
	invalid = { ..parent, children: [1, 2] }
	check_children(parsed, parent, "p", False) == Ok({}) and check_children(parsed, invalid, "p", False) == Err(InvalidMarkup({ problem: Unsupported("invalid child node"), line: 1, column: 1 }))
}

context_frame : U64, HtmlValidate.ContextState -> _
context_frame = |index, context| { index, parent: context.parent, interactive: context.interactive, anchor: context.anchor, option_context: context.option_context, in_option: context.in_option, in_datalist: context.in_datalist, selected_context: context.selected_context }
