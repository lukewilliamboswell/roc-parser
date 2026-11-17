import MarkupSyntaxFacts
import DomNameCore
import MarkupGrammar

## Shared primitive vocabulary rules used by names and validators.
HtmlNameRules :: [].{
	NameParts : { namespace : [Uri(Str), NoNamespace], local : Str, prefix : [Prefix(Str), Unprefixed] }
	element_known : Str -> Bool
	element_known = elements
	custom_element_name : Str -> Bool
	custom_element_name = custom_element
	boolean_attribute : Str, Str -> Bool
	boolean_attribute = |element, attr| allowed(element, attr) and (booleans(attr) or (element == "iframe" and attr == "credentialless")) and ((attr != "playsinline" and attr != "disablepictureinpicture") or element == "video")
	enumerated_values : Str, Str -> Try(List(Str), [NotEnumerated])
	enumerated_values = |element, attr| {
		if !allowed(element, attr) or boolean_attribute(element, attr) or booleans(attr) or (attr == "scope" and element != "th") {
			return Err(NotEnumerated)
		}
		match MarkupSyntaxFacts.aria_enumerated_values(attr) {
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
	grammar : Str, Str -> MarkupGrammar.ValueGrammar
	grammar = |element, attr| {
		if !allowed(element, attr) {
			return Language(ExtensionAttribute)
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
			_ => Language(HTMLAttribute)
		}
	}
	custom_element : _
	custom_element = |name| DomNameCore.valid_element_local_name(name) and name.contains("-") and name.to_utf8().first().map_ok(|b| b >= 'a' and b <= 'z') == Ok(True) and !name.to_utf8().any(|b| b >= 'A' and b <= 'Z') and !(
		match name {
			"annotation-xml" | "color-profile" | "font-face" | "font-face-src" | "font-face-uri" | "font-face-format" | "font-face-name" | "missing-glyph" => True
			_ => False
		}
	)
	allowed : _
	allowed = |element, attr| global(attr) or attributes(element, attr) or (attr.starts_with("data-") and attr.count_utf8_bytes() > 5 and !attr.contains(":")) or aria(attr) or events(attr) or (element == "input" and attr == "onsearch")
	elements : _
	elements = MarkupSyntaxFacts.html_element_name
	global : _
	global = |name| match name {
		"accesskey" | "autocapitalize" | "autocorrect" | "autofocus" | "class" | "contenteditable" | "dir" | "draggable" | "enterkeyhint" | "exportparts" | "hidden" | "id" | "inert" | "inputmode" | "is" | "itemid" | "itemprop" | "itemref" | "itemscope" | "itemtype" | "lang" | "nonce" | "part" | "popover" | "role" | "slot" | "spellcheck" | "style" | "tabindex" | "title" | "translate" | "virtualkeyboardpolicy" | "writingsuggestions" | "xmlns" => True
		_ => False
	}
	booleans : _
	booleans = |name| match name {
		"allowfullscreen" | "async" | "autofocus" | "autoplay" | "checked" | "controls" | "default" | "defer" | "disabled" | "disablepictureinpicture" | "disableremoteplayback" | "formnovalidate" | "inert" | "ismap" | "itemscope" | "loop" | "multiple" | "muted" | "nomodule" | "novalidate" | "open" | "playsinline" | "readonly" | "required" | "reversed" | "selected" | "shadowrootclonable" | "shadowrootdelegatesfocus" | "shadowrootserializable" => True
		_ => False
	}
	aria : _
	aria = MarkupSyntaxFacts.aria_name
	events : _
	events = MarkupSyntaxFacts.html_event_name
	attributes : _
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
}
