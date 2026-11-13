import Markup
import MarkupValidate
import HtmlTokenizer
import Literal

## HTML authoring checks, separate from browser tree recovery. Embedded CSS and
## JavaScript remain source text rather than claims of validated programs.
HtmlValidate :: [].{
	validate : Markup.Parsed, Markup.Props, List(MarkupValidate.Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	validate = |parsed, props, holes| {
		var $diagnostics = parsed.diagnostics
		hole_positions = holes.fold(Dict.empty(), |dict, hole| dict.insert((hole.node, hole.attribute), True))
		var $pending = parsed.tree.roots.map(|index| { index, parent: "", interactive: False, anchor: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, parent: "", interactive: False, anchor: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup({ problem: Unsupported("invalid tree index"), line: 1, column: 1 }))
			match node.kind {
				Element(name) if name.namespace == Uri(Markup.html_namespace) => {
					custom = custom_element(name.local)
					if !elements.contains(name.local) and !custom {
						if !props.allow_unknown_elements {
							return fail(parsed, node, UnknownElement(name.local))
						}
						offset = match node.origin {
							Authored(span) => span.start
							Implied(at) => at
						}
						at = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
						if $diagnostics.len() >= props.max_diagnostics {
							return fail(parsed, node, ResourceLimit("max_diagnostics"))
						}
						$diagnostics = $diagnostics.append({ problem: UnknownElement(name.local), line: at.line, column: at.column })
					}
					var $at = 0
					for attr in node.attributes {
						known = allowed(name.local, attr.name.local) or custom
						if attr.name.namespace != NoNamespace and attr.name.namespace != Uri("http://www.w3.org/2000/xmlns/") and attr.name.namespace != Uri("http://www.w3.org/XML/1998/namespace") and !props.allow_unknown_attributes {
							return attr_fail(parsed, attr, UnknownAttribute({ element: name.local, attribute: attr.name.local }))
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
						if props.check_values and known and !hole_positions.contains((item.index, $at)) {
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
					if item.interactive and interactive(node, name.local) {
						return fail(parsed, node, InvalidChildren({ element: "button", expected: "no nested interactive content" }))
					}
					allowed_parent = match name.local {
						"li" => ["ul", "ol", "menu"].contains(item.parent)
						"dt" | "dd" => item.parent == "dl" or item.parent == "div"
						"tr" => ["thead", "tbody", "tfoot"].contains(item.parent)
						"td" | "th" => item.parent == "tr"
						"thead" | "tbody" | "tfoot" | "caption" | "colgroup" => item.parent == "table"
						"col" => item.parent == "colgroup"
						"option" => ["select", "optgroup", "datalist"].contains(item.parent)
						"optgroup" => item.parent == "select"
						"summary" => item.parent == "details"
						"legend" => item.parent == "fieldset"
						"source" => ["audio", "video", "picture"].contains(item.parent)
						"track" => ["audio", "video"].contains(item.parent)
						_ => True
					}
					# A fragment has a declared context, and standalone row/option
					# fragments are legal in that context. Parent checks apply
					# only where an authored parent is available.
					if item.parent != "" and !allowed_parent {
						return fail(parsed, node, InvalidChildren({ element: item.parent, expected: "allowed parent for ${name.local}" }))
					}
					if name.local == "img" and !has_attribute(node, "alt") and !has_attribute(node, "role") {
						return fail(parsed, node, InvalidChildren({ element: "img", expected: "alt attribute (or explicit presentation role)" }))
					}
					if name.local == "input" and value(node, "type") == "image" and (!has_attribute(node, "src") or !has_attribute(node, "alt")) {
						return fail(parsed, node, InvalidChildren({ element: "input", expected: "image input requires src and alt" }))
					}
					for child in node.children {
						$pending = $pending.append({ index: child, parent: name.local, interactive: item.interactive or name.local == "button", anchor: item.anchor or name.local == "a" })
					}
				}
				Element(_) => {
					for child in node.children {
						$pending = $pending.append({ index: child, parent: "", interactive: False, anchor: False })
					}
				}
				_ => {}
			}
		}
		Ok({ ..parsed, diagnostics: $diagnostics })
	}
	grammar : Str, Str -> Markup.ValueGrammar
	grammar = |element, attr| {
		if booleans.contains(attr) {
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

custom_element = |name| name.contains("-") and name.to_utf8().first().map_ok(|b| b >= 'a' and b <= 'z') == Ok(True) and name.to_utf8().all(|b| (b >= 'a' and b <= 'z') or (b >= '0' and b <= '9') or ['-', '.', '_'].contains(b) or b >= 128) and !["annotation-xml", "color-profile", "font-face", "font-face-src", "font-face-uri", "font-face-format", "font-face-name", "missing-glyph"].contains(name)

allowed = |element, attr| global.contains(attr) or (attr.starts_with("data-") and attr.count_utf8_bytes() > 5 and !attr.contains(":")) or aria.contains(attr) or events.contains(attr) or attributes(element).contains(attr)

has_attribute = |node, key| node.attributes.any(|a| a.name.local == key)

value = |node, key| node.attributes.find_first(|a| a.name.local == key).map_ok(|a| a.value) ?? ""

fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(s) => s.start
		Implied(o) => o
	}
	p = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
	Err(InvalidMarkup({ problem, line: p.line, column: p.column }))
}

attr_fail = |parsed, attr, problem| {
	p = Literal.position(Str.from_utf8_lossy(parsed.source.bytes), attr.span.start)
	Err(InvalidMarkup({ problem, line: p.line, column: p.column }))
}

elements = "a abbr address area article aside audio b base bdi bdo blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hgroup hr html i iframe img input ins kbd label legend li link main map mark menu meta meter nav noscript object ol optgroup option output p picture pre progress q rp rt ruby s samp script search section select selectedcontent slot small source span strong style sub summary sup table tbody td template textarea tfoot th thead time title tr track u ul var video wbr".split_on(" ")

global = "accesskey autocapitalize autocorrect autofocus class contenteditable dir draggable enterkeyhint exportparts hidden id inert inputmode is itemid itemprop itemref itemscope itemtype lang nonce part popover role slot spellcheck style tabindex title translate virtualkeyboardpolicy writingsuggestions xmlns".split_on(" ")

booleans = "allowfullscreen async autofocus autoplay checked controls default defer disabled disablepictureinpicture disableremoteplayback formnovalidate inert ismap itemscope loop multiple muted nomodule novalidate open playsinline readonly required reversed selected shadowrootclonable shadowrootdelegatesfocus shadowrootserializable".split_on(" ")

aria = "aria-activedescendant aria-atomic aria-autocomplete aria-braillelabel aria-brailleroledescription aria-busy aria-checked aria-colcount aria-colindex aria-colindextext aria-colspan aria-controls aria-current aria-describedby aria-description aria-details aria-disabled aria-dropeffect aria-errormessage aria-expanded aria-flowto aria-grabbed aria-haspopup aria-hidden aria-invalid aria-keyshortcuts aria-label aria-labelledby aria-level aria-live aria-modal aria-multiline aria-multiselectable aria-orientation aria-owns aria-placeholder aria-posinset aria-pressed aria-readonly aria-relevant aria-required aria-roledescription aria-rowcount aria-rowindex aria-rowindextext aria-rowspan aria-selected aria-setsize aria-sort aria-valuemax aria-valuemin aria-valuenow aria-valuetext".split_on(" ")

events = "onabort onafterprint onanimationcancel onanimationend onanimationiteration onanimationstart onauxclick onbeforeinput onbeforematch onbeforeprint onbeforetoggle onbeforeunload onblur oncancel oncanplay oncanplaythrough onchange onclick onclose oncontextlost oncontextmenu oncontextrestored oncopy oncuechange oncut ondblclick ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus onfocusin onfocusout onformdata ongotpointercapture onhashchange oninput oninvalid onkeydown onkeypress onkeyup onlanguagechange onload onloadeddata onloadedmetadata onloadstart onlostpointercapture onmessage onmessageerror onmousedown onmouseenter onmouseleave onmousemove onmouseout onmouseover onmouseup onoffline ononline onpagehide onpageshow onpaste onpause onplay onplaying onpointercancel onpointerdown onpointerenter onpointerleave onpointermove onpointerout onpointerover onpointerrawupdate onpointerup onpopstate onprogress onratechange onrejectionhandled onreset onresize onscroll onscrollend onsecuritypolicyviolation onseeked onseeking onselect onselectionchange onselectstart onslotchange onstalled onstorage onsubmit onsuspend ontimeupdate ontoggle ontouchcancel ontouchend ontouchmove ontouchstart ontransitioncancel ontransitionend ontransitionrun ontransitionstart onunhandledrejection onunload onvolumechange onwaiting onwheel".split_on(" ")

attributes = |element| {
	match element {
		"a" | "area" => "alt coords shape download href hreflang ping referrerpolicy rel target type"
		"audio" | "video" => "autoplay controls controlslist crossorigin disableremoteplayback disablepictureinpicture height loop muted playsinline poster preload src width"
		"base" => "href target"
		"blockquote" | "q" => "cite"
		"button" => "command commandfor disabled form formaction formenctype formmethod formnovalidate formtarget name popovertarget popovertargetaction type value"
		"canvas" => "height width"
		"col" | "colgroup" => "span"
		"data" => "value"
		"del" | "ins" => "cite datetime"
		"details" => "name open"
		"dialog" => "closedby open"
		"embed" => "height src type width"
		"fieldset" => "disabled form name"
		"form" => "accept-charset action autocomplete enctype method name novalidate rel target"
		"html" => "xmlns"
		"iframe" => "allow allowfullscreen credentialless csp height loading name referrerpolicy sandbox src srcdoc width"
		"img" => "alt attributionsrc crossorigin decoding elementtiming fetchpriority height ismap loading referrerpolicy sizes src srcset usemap width"
		"input" => "accept alt autocomplete capture checked dirname disabled form formaction formenctype formmethod formnovalidate formtarget height list max maxlength min minlength multiple name pattern placeholder popovertarget popovertargetaction readonly required size src step type value width"
		"label" => "for form"
		"li" => "value"
		"link" => "as blocking color crossorigin disabled fetchpriority href hreflang imagesizes imagesrcset integrity media referrerpolicy rel sizes type"
		"map" => "name"
		"meta" => "charset content http-equiv media name"
		"meter" => "form high low max min optimum value"
		"object" => "data form height name type usemap width"
		"ol" => "reversed start type"
		"optgroup" => "disabled label"
		"option" => "disabled label selected value"
		"output" => "for form name"
		"progress" => "max value"
		"script" => "async attributionsrc blocking crossorigin defer fetchpriority integrity nomodule nonce referrerpolicy src type"
		"select" => "autocomplete disabled form multiple name required size"
		"slot" => "name"
		"source" => "height media sizes src srcset type width"
		"style" => "blocking media nonce title"
		"td" | "th" => "abbr colspan headers rowspan scope"
		"template" => "shadowrootclonable shadowrootdelegatesfocus shadowrootmode shadowrootserializable"
		"textarea" => "autocomplete cols dirname disabled form maxlength minlength name placeholder readonly required rows wrap"
		"time" => "datetime"
		"track" => "default kind label src srclang"
		_ => ""
	}.split_on(" ")
}

interactive = |node, name| ["button", "details", "embed", "iframe", "label", "select", "textarea"].contains(name) or (name == "input" and HtmlTokenizer.lower(value(node, "type")) != "hidden") or (name == "a" and has_attribute(node, "href")) or (["audio", "video"].contains(name) and has_attribute(node, "controls")) or (name == "img" and has_attribute(node, "usemap"))
