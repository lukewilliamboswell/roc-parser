import Markup
import MarkupNameTables

# Shared names used by authoring checks and public consumer facts.
MarkupVocabulary :: [].{
	TextBehavior : [Data, Rcdata, RawText, ScriptData, Plaintext]
	text_behavior : Str, Bool -> TextBehavior
	text_behavior = |local, scripting| match local {
		"title" | "textarea" => Rcdata
		"script" => ScriptData
		"plaintext" => Plaintext
		"style" | "xmp" | "iframe" | "noembed" | "noframes" => RawText
		"noscript" if scripting => RawText
		_ => Data
	}
	html_void : Str -> Bool
	html_void = |name| match name {
		"area" | "base" | "basefont" | "bgsound" | "br" | "col" | "embed" | "frame" | "hr" | "img" | "input" | "keygen" | "link" | "meta" | "param" | "source" | "track" | "wbr" => True
		_ => False
	}
	# Canonical authoring vocabularies and their generated membership matches.
	html_element_names : List(Str)
	html_element_names = MarkupNameTables.html_names
	svg_element_names : List(Str)
	svg_element_names = MarkupNameTables.svg_names
	mathml_element_names : List(Str)
	mathml_element_names = MarkupNameTables.mathml_names
	aria_attribute_names : List(Str)
	aria_attribute_names = MarkupNameTables.aria_names
	html_element_name : Str -> Bool
	html_element_name = MarkupNameTables.html_name
	svg_element_name : Str -> Bool
	svg_element_name = MarkupNameTables.svg_name
	mathml_element_name : Str -> Bool
	mathml_element_name = MarkupNameTables.mathml_name
	aria_name : Str -> Bool
	aria_name = MarkupNameTables.aria_name

	# Only the token immediately after an emitted HTML start tag is eligible.
	drops_initial_newline : Str, Str -> Bool
	drops_initial_newline = |namespace, local| namespace == Markup.html_namespace and (local == "pre" or local == "listing" or local == "textarea")

	# Finite single-value ARIA states, shared across HTML, SVG and MathML.
	# Token-list values such as aria-relevant and aria-dropeffect are not enums.
	aria_enumerated_values : Str -> Try(List(Str), [NotEnumerated])
	aria_enumerated_values = |name| match name {
		"aria-atomic" | "aria-busy" | "aria-disabled" | "aria-modal" | "aria-multiline" | "aria-multiselectable" | "aria-readonly" | "aria-required" => Ok(["true", "false"])
		"aria-expanded" | "aria-grabbed" | "aria-hidden" | "aria-selected" => Ok(["true", "false", "undefined"])
		"aria-checked" | "aria-pressed" => Ok(["true", "false", "mixed", "undefined"])
		"aria-autocomplete" => Ok(["none", "inline", "list", "both"])
		"aria-current" => Ok(["false", "true", "page", "step", "location", "date", "time"])
		"aria-haspopup" => Ok(["false", "true", "menu", "listbox", "tree", "grid", "dialog"])
		"aria-invalid" => Ok(["false", "true", "grammar", "spelling"])
		"aria-live" => Ok(["off", "polite", "assertive"])
		"aria-orientation" => Ok(["horizontal", "vertical", "undefined"])
		"aria-sort" => Ok(["ascending", "descending", "none", "other"])
		_ => Err(NotEnumerated)
	}

	global_event_name : Str -> Bool
	global_event_name = |name| match name {
		"onabort" | "onanimationcancel" | "onanimationend" | "onanimationiteration" | "onanimationstart" | "onauxclick" | "onbeforeinput" | "onbeforematch" | "onbeforetoggle" | "onblur" | "oncancel" | "oncanplay" | "oncanplaythrough" | "onchange" | "onclick" | "onclose" | "oncommand" | "oncontextlost" | "oncontextmenu" | "oncontextrestored" | "oncopy" | "oncuechange" | "oncut" | "ondblclick" | "ondrag" | "ondragend" | "ondragenter" | "ondragleave" | "ondragover" | "ondragstart" | "ondrop" | "ondurationchange" | "onemptied" | "onended" | "onerror" | "onfocus" | "onfocusin" | "onfocusout" | "onformdata" | "ongotpointercapture" | "oninput" | "oninvalid" | "onkeydown" | "onkeypress" | "onkeyup" | "onload" | "onloadeddata" | "onloadedmetadata" | "onloadstart" | "onlostpointercapture" | "onmousedown" | "onmouseenter" | "onmouseleave" | "onmousemove" | "onmouseout" | "onmouseover" | "onmouseup" | "onpaste" | "onpause" | "onplay" | "onplaying" | "onpointercancel" | "onpointerdown" | "onpointerenter" | "onpointerleave" | "onpointermove" | "onpointerout" | "onpointerover" | "onpointerrawupdate" | "onpointerup" | "onprogress" | "onratechange" | "onreset" | "onresize" | "onscroll" | "onscrollend" | "onsecuritypolicyviolation" | "onseeked" | "onseeking" | "onselect" | "onselectionchange" | "onselectstart" | "onslotchange" | "onstalled" | "onsubmit" | "onsuspend" | "ontimeupdate" | "ontoggle" | "ontouchcancel" | "ontouchend" | "ontouchmove" | "ontouchstart" | "ontransitioncancel" | "ontransitionend" | "ontransitionrun" | "ontransitionstart" | "onvolumechange" | "onwaiting" | "onwebkitanimationend" | "onwebkitanimationiteration" | "onwebkitanimationstart" | "onwebkittransitionend" | "onwheel" => True
		_ => False
	}
	window_event_name : Str -> Bool
	window_event_name = |name| match name {
		"onafterprint" | "onbeforeprint" | "onbeforeunload" | "onhashchange" | "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" | "ononline" | "onpagehide" | "onpagereveal" | "onpageshow" | "onpageswap" | "onpopstate" | "onrejectionhandled" | "onstorage" | "onunhandledrejection" | "onunload" => True
		_ => False
	}
	legacy_svg_event_name : Str -> Bool
	legacy_svg_event_name = |name| match name {
		"onactivate" | "onbegin" | "onend" | "onrepeat" | "onunload" | "onzoom" => True
		_ => False
	}

	html_event_name : Str -> Bool
	html_event_name = |name| global_event_name(name) or window_event_name(name)
	svg_event_name : Str -> Bool
	svg_event_name = |name| global_event_name(name) or legacy_svg_event_name(name)
	event_attribute : Markup.Name, Markup.Name -> Bool
	event_attribute = |element, attribute| {
		if attribute.namespace != NoNamespace {
			return False
		}
		match element.namespace {
			Uri("http://www.w3.org/1999/xhtml") => global_event_name(attribute.local) or ((element.local == "body" or element.local == "frameset") and window_event_name(attribute.local)) or (element.local == "input" and attribute.local == "onsearch")
			Uri("http://www.w3.org/2000/svg") => global_event_name(attribute.local) or (
				match attribute.local {
					"onbegin" | "onend" | "onrepeat" => ["animate", "animateMotion", "animateTransform", "set"].contains(element.local)
					"onunload" => element.local == "svg"
					_ => False
				}
			)
			Uri("http://www.w3.org/1998/Math/MathML") => global_event_name(attribute.local)
			_ => False
		}
	}
}
