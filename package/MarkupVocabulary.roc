import Markup

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
	aria_name : Str -> Bool
	aria_name = |name| match name {
		"aria-activedescendant" | "aria-atomic" | "aria-autocomplete" | "aria-braillelabel" | "aria-brailleroledescription" | "aria-busy" | "aria-checked" | "aria-colcount" | "aria-colindex" | "aria-colindextext" | "aria-colspan" | "aria-controls" | "aria-current" | "aria-describedby" | "aria-description" | "aria-details" | "aria-disabled" | "aria-dropeffect" | "aria-errormessage" | "aria-expanded" | "aria-flowto" | "aria-grabbed" | "aria-haspopup" | "aria-hidden" | "aria-invalid" | "aria-keyshortcuts" | "aria-label" | "aria-labelledby" | "aria-level" | "aria-live" | "aria-modal" | "aria-multiline" | "aria-multiselectable" | "aria-orientation" | "aria-owns" | "aria-placeholder" | "aria-posinset" | "aria-pressed" | "aria-readonly" | "aria-relevant" | "aria-required" | "aria-roledescription" | "aria-rowcount" | "aria-rowindex" | "aria-rowindextext" | "aria-rowspan" | "aria-selected" | "aria-setsize" | "aria-sort" | "aria-valuemax" | "aria-valuemin" | "aria-valuenow" | "aria-valuetext" => True
		_ => False
	}
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
