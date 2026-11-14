import Markup
import MarkupVocabulary
import HtmlTokenizer as Tok

# Shared tree-construction predicates. No source reader or DOM arena is kept here.
HtmlContextRules :: [].{
	NamespaceClass : [HtmlNamespace, SvgNamespace, MathNamespace, OtherNamespace]
	html_whitespace = |text| {
		bytes = text.to_utf8()
		var $at = 0
		while $at < bytes.len() {
			if !Tok.space(bytes.get($at) ?? 0) {
				return False
			}
			$at = $at + 1
		}
		True
	}

	Scope : [DefaultScope, TableScope, ListScope, ButtonScope]
	namespace_class = |uri| if uri == Markup.html_namespace {
		HtmlNamespace
	} else if uri == Markup.svg_namespace {
		SvgNamespace
	} else if uri == Markup.mathml_namespace {
		MathNamespace
	} else {
		OtherNamespace
	}
	html_special = |n| (
		match n {
			"address" | "applet" | "area" | "article" | "aside" | "base" | "basefont" | "bgsound" | "blockquote" | "body" | "br" | "button" | "caption" | "center" | "col" | "colgroup" | "dd" | "details" | "dir" | "div" | "dl" | "dt" | "embed" | "fieldset" | "figcaption" | "figure" | "footer" | "form" | "frame" | "frameset" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "head" | "header" | "hgroup" | "hr" | "html" | "iframe" | "img" | "input" | "li" | "link" | "listing" | "main" | "marquee" | "menu" | "meta" | "nav" | "noembed" | "noframes" | "noscript" | "object" | "ol" | "p" | "param" | "plaintext" | "pre" | "script" | "search" | "section" | "select" | "source" | "style" | "summary" | "table" | "tbody" | "td" | "template" | "textarea" | "tfoot" | "th" | "thead" | "title" | "tr" | "track" | "ul" | "wbr" | "xmp" => True
			_ => False
		}
	)

	formatting = |n| (
		match n {
			"a" | "b" | "big" | "code" | "em" | "font" | "i" | "nobr" | "s" | "small" | "strike" | "strong" | "tt" | "u" => True
			_ => False
		}
	)

	block_start = |name| match name {
		"address" | "article" | "aside" | "blockquote" | "center" | "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" | "figcaption" | "figure" | "footer" | "header" | "hgroup" | "main" | "menu" | "nav" | "ol" | "p" | "search" | "section" | "summary" | "ul" => True
		_ => False
	}
	heading = |name| ["h1", "h2", "h3", "h4", "h5", "h6"].contains(name)
	closes_p = |name| block_start(name) or heading(name) or ["pre", "listing", "form", "li", "dd", "dt", "plaintext", "table", "hr", "xmp"].contains(name)
	svg_integration = |name| ["foreignObject", "desc", "title"].contains(name)
	math_text_integration = |name| ["mi", "mo", "mn", "ms", "mtext"].contains(name)
	math_scope_boundary = |name| math_text_integration(name) or name == "annotation-xml"
	special = |namespace, name| match namespace {
		HtmlNamespace => html_special(name)
		MathNamespace => math_scope_boundary(name)
		SvgNamespace => svg_integration(name)
		OtherNamespace => False
	}
	scope_boundary = |namespace, name, kind| {
		if namespace == HtmlNamespace {
			if ["html", "table", "template"].contains(name) {
				return True
			}
			if kind != TableScope and ["applet", "caption", "td", "th", "marquee", "object", "select"].contains(name) {
				return True
			}
			if kind == ListScope and ["ol", "ul"].contains(name) {
				return True
			}
			return kind == ButtonScope and name == "button"
		}
		kind != TableScope and ((namespace == MathNamespace and math_scope_boundary(name)) or (namespace == SvgNamespace and svg_integration(name)))
	}
	implied_end_element = |name| ["dd", "dt", "li", "optgroup", "option", "p", "rb", "rp", "rt", "rtc"].contains(name)
	list_scan_stops = |is_special, name| is_special and !["address", "div", "p"].contains(name)
	foreign_breakout = |name, font_attributes| match name {
		"b" | "big" | "blockquote" | "body" | "br" | "center" | "code" | "dd" | "div" | "dl" | "dt" | "em" | "embed" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "head" | "hr" | "i" | "img" | "li" | "listing" | "menu" | "meta" | "nobr" | "ol" | "p" | "pre" | "ruby" | "s" | "small" | "span" | "strong" | "strike" | "sub" | "sup" | "table" | "tt" | "u" | "ul" | "var" => True
		"font" => font_attributes
		_ => False
	}
	# One bit per queried scope target; descent updates these in constant space.
	Scopes : { p : Bool, button : Bool, nobr : Bool, select : Bool, option : Bool, optgroup : Bool, li : Bool, definition_item : Bool, form : Bool }
	empty_scopes : Scopes
	empty_scopes = { p: False, button: False, nobr: False, select: False, option: False, optgroup: False, li: False, definition_item: False, form: False }
	descend : Scopes, NamespaceClass, Str -> Scopes
	descend = |scopes, namespace, name| {
		html = namespace == HtmlNamespace
		stop_default = scope_boundary(namespace, name, DefaultScope)
		stop_list = list_scan_stops(special(namespace, name), name)
		{ p: (html and name == "p") or (scopes.p and !scope_boundary(namespace, name, ButtonScope)), button: (html and name == "button") or (scopes.button and !stop_default), nobr: (html and name == "nobr") or (scopes.nobr and !stop_default), select: (html and name == "select") or (scopes.select and !stop_default), option: (html and name == "option") or (scopes.option and !stop_default), optgroup: (html and name == "optgroup") or (scopes.optgroup and !stop_default), li: (html and name == "li") or (scopes.li and !stop_list), definition_item: (html and ["dd", "dt"].contains(name)) or (scopes.definition_item and !stop_list), form: scopes.form or (html and name == "form") }
	}
}
