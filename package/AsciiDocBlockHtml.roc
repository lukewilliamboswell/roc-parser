## HTML helpers shared by the document renderer. Inline fields marked with
## __rendered-source / __rendered-title have already passed through document
## state, preserving footnote numbering and cross-reference labels.
AsciiDocBlockHtml :: [].{
	Attribute : { name : Str, value : Str }
	Description : { term : Str, text : Str, body : Str }

	paragraph : Str, List(Attribute) -> Str
	paragraph = |source, attrs| {
		html = match lookup_attr(attrs, "__rendered-source") {
			Ok(prepared) => prepared
			Err(_) => AsciiDocInline.render_with(source, attrs, { subs: if has_attr(attrs, "subs") attr(attrs, "subs") else "normal" })
		}
		if attr(attrs, "options").split_on(",").contains("hardbreaks") or has_attr(attrs, "hardbreaks-option") or has_attr(attrs, "hardbreaks") {
			replace(html, "\n", "<br>\n")
		} else html
	}

	verbatim : Str, List(Attribute) -> Str
	verbatim = |source, attrs| {
		raw_subs = attr(attrs, "subs")
		modified = raw_subs.starts_with("+") or raw_subs.starts_with("-")
		subs = if !has_attr(attrs, "subs") "specialcharacters" else if modified "specialcharacters,${raw_subs}" else raw_subs
		var $callouts = !has_attr(attrs, "subs") or modified
		for spec in raw_subs.split_on(",") {
			if spec == "callouts" or spec == "+callouts" {
				$callouts = True
			}
				else if spec == "-callouts" or spec == "none" {
					$callouts = False
				}
		}
		text = trim_blank_lines(source)
		if !$callouts or !text.contains("<") {
			return if subs == "specialcharacters" escape_verbatim(text) else AsciiDocInline.render_with(text, attrs, { subs: subs })
		}
		var $prefix = "ROCASCIIDOCCALLOUT"
		while text.contains($prefix) {
			$prefix = "${$prefix}X"
		}
		var $rest = text
		var $plain = ""
		var $replacements = []
		var $auto = 1.U64
		while $rest != "" {
			escaped = $rest.starts_with("\\<")
			candidate = if escaped drop($rest, 1) else $rest
			html_comment = candidate.starts_with("<!--")
			if candidate.starts_with("<") {
				parts = drop(candidate, if html_comment 4 else 1).split_on(if html_comment "-->" else ">")
				number = parts.first() ?? ""
				length = number.to_utf8().len() + (if html_comment 7 else 2)
				if parts.len() > 1 and (number == "." or (U64.from_str(number).is_ok() and number != "")) and callout_tail(drop(candidate, length)) {
					marker = "${$prefix}${$replacements.len().to_str()}END"
					n = if number == "." $auto.to_str() else number
					if number == "." and !escaped {
						$auto = $auto + 1
					}
					callout = if escaped AsciiDocInline.render_with("<${number}>", attrs, { subs: subs }) else "<b class=\"conum\">(${n})</b>"
					replacement = if html_comment "${AsciiDocInline.render_with("<!--", attrs, { subs: subs })}${callout}${AsciiDocInline.render_with("-->", attrs, { subs: subs })}" else callout
					$replacements = $replacements.append({ marker, replacement })
					$plain = "${$plain}${marker}"
					$rest = drop($rest, length + (if escaped 1 else 0))
				} else {
					$plain = "${$plain}${take($rest, scalar_width($rest))}"
					$rest = drop($rest, scalar_width($rest))
				}
			} else {
				$plain = "${$plain}${take($rest, scalar_width($rest))}"
				$rest = drop($rest, scalar_width($rest))
			}
		}
		var $html = AsciiDocInline.render_with($plain, attrs, { subs: subs })
		for item in $replacements {
			$html = replace($html, item.marker, item.replacement)
		}
		$html
	}

	description : Str, Str, Str, List(Attribute), List(Description) -> Str
	description = |style, id, title, attrs, items| {
		idattr = if id == "" "" else " id=\"${escape(id)}\""
		title_html = if title == "" "" else "<div class=\"title\">${title}</div>\n"
		role = if attr(attrs, "role") == "" "" else " ${attr(attrs, "role")}"
		if style == "qanda" {
			content = Str.join_with(items.map(|item| "<li>\n<p><em>${item.term}</em></p>${if item.text == "" "" else "\n<p>${item.text}</p>"}${if item.body == "" "" else "\n${item.body}"}\n</li>"), "\n")
			return "<div${idattr} class=\"qlist qanda${role}\">\n${title_html}<ol>\n${content}\n</ol>\n</div>"
		}
		if style == "horizontal" {
			content = Str.join_with(items.map(|item| "<tr>\n<td class=\"hdlist1\">\n${item.term}\n</td>\n<td class=\"hdlist2\">${if item.text == "" "" else "\n<p>${item.text}</p>"}${if item.body == "" "" else "\n${item.body}"}\n</td>\n</tr>"), "\n")
			return "<div${idattr} class=\"hdlist${role}\">\n${title_html}<table>\n${content}\n</table>\n</div>"
		}
		content = Str.join_with(items.map(|item| "<dt class=\"hdlist1\">${item.term}</dt>${if item.text == "" and item.body == "" "" else "\n<dd>\n${if item.text == "" "" else "<p>${item.text}</p>\n"}${if item.body == "" "" else "${item.body}\n"}</dd>"}"), "\n")
		"<div${idattr} class=\"dlist${role}\">\n${title_html}<dl>\n${content}\n</dl>\n</div>"
	}

	list_wrapper : Str, Str, U64, Str, Str, List(Attribute), Str, Bool -> Str
	list_wrapper = |context, style, level, id, title, attrs, body, checklist| {
		tag = if context == "ulist" "ul" else "ol"
		resolved = if style != "" style else match level {
			2 => "loweralpha"
			3 => "lowerroman"
			4 => "upperalpha"
			5 => "upperroman"
			_ => "arabic"
		}
		extra = if context == "olist" " ${resolved}" else if context == "colist" " arabic" else if style == "bibliography" " bibliography" else if checklist " checklist" else ""
		roles = if attr(attrs, "role") == "" "" else " ${attr(attrs, "role")}"
		idattr = if id == "" "" else " id=\"${escape(id)}\""
		title_html = if title == "" "" else "<div class=\"title\">${title}</div>\n"
		tagattrs = if context == "olist" " class=\"${resolved}\"${if has_attr(attrs, "__implicit-list-style") "" else list_type(resolved)}${if attr(attrs, "start") == "" "" else " start=\"${escape(attr(attrs, "start"))}\""}" else if style == "bibliography" " class=\"bibliography\"" else if checklist " class=\"checklist\"" else ""
		"<div${idattr} class=\"${context}${extra}${roles}\">\n${title_html}<${tag}${tagattrs}>\n${body}\n</${tag}>\n</div>"
	}
}
import AsciiDocInline

attr : List(AsciiDocBlockHtml.Attribute), Str -> Str
attr = |attrs, name| lookup_attr(attrs, name) ?? ""

lookup_attr : List(AsciiDocBlockHtml.Attribute), Str -> Try(Str, [Missing])
lookup_attr = |attrs, name| {
	var $position = attrs.len()
	while $position > 0 {
		$position = $position - 1
		match attrs.get($position) {
			Ok(found) if found.name == name => return Ok(found.value)
			_ => {}
		}
	}
	Err(Missing)
}

has_attr : List(AsciiDocBlockHtml.Attribute), Str -> Bool
has_attr = |attrs, name| lookup_attr(attrs, name).is_ok()

replace : Str, Str, Str -> Str
replace = |s, old, new| s.replace_each(old, new)

escape : Str -> Str
escape = |s| replace(replace(replace(replace(s, "&", "&amp;"), "<", "&lt;"), ">", "&gt;"), "\"", "&quot;")

drop : Str, U64 -> Str
drop = |s, n| Str.from_utf8_lossy(s.to_utf8().drop_first(n))

take : Str, U64 -> Str
take = |s, n| Str.from_utf8_lossy(s.to_utf8().take_first(n))

trim_blank_lines : Str -> Str
trim_blank_lines = |source| {
	bytes = source.to_utf8()
	first = bytes.first() ?? 0
	last = bytes.last() ?? 0
	if first > 32 and first < 127 and last > 32 and last < 127 {
		return source
	}
	var $lines = source.split_on("\n")
	while !$lines.is_empty() and ($lines.first() ?? "").trim() == "" {
		$lines = $lines.drop_first(1)
	}
	while !$lines.is_empty() and ($lines.last() ?? "").trim() == "" {
		$lines = $lines.drop_last(1)
	}
	Str.join_with($lines, "\n")
}

list_type : Str -> Str
list_type = |style| match style {
	"loweralpha" => " type=\"a\""
	"upperalpha" => " type=\"A\""
	"lowerroman" => " type=\"i\""
	"upperroman" => " type=\"I\""
	_ => ""
}

expect AsciiDocBlockHtml.verbatim("\ncode <1>\n", []) == "code <b class=\"conum\">(1)</b>"
expect AsciiDocBlockHtml.verbatim("code \\<1>", []) == "code &lt;1&gt;"
expect AsciiDocBlockHtml.verbatim("a <.>\nb <.>", []) == "a <b class=\"conum\">(1)</b>\nb <b class=\"conum\">(2)</b>"
expect AsciiDocBlockHtml.verbatim("*bold*", [{ name: "subs", value: "+quotes" }]) == "<strong>bold</strong>"
expect AsciiDocBlockHtml.verbatim("<x> *bold*", [{ name: "subs", value: "none" }]) == "<x> *bold*"
expect AsciiDocBlockHtml.paragraph("*raw*", [{ name: "subs", value: "none" }]) == "*raw*"
expect AsciiDocBlockHtml.paragraph("raw", [{ name: "__rendered-source", value: "<strong>prepared</strong>" }]) == "<strong>prepared</strong>"
expect AsciiDocBlockHtml.description("", "", "", [], [{ term: "A", text: "", body: "" }]) == "<div class=\"dlist\">\n<dl>\n<dt class=\"hdlist1\">A</dt>\n</dl>\n</div>"

scalar_width : Str -> U64
scalar_width = |s| {
	b = s.to_utf8().first() ?? 0
	if b < 128 1 else if b < 224 2 else if b < 240 3 else 4
}
expect AsciiDocBlockHtml.verbatim("α🦊 <1>", []) == "α🦊 <b class=\"conum\">(1)</b>"

callout_tail : Str -> Bool
callout_tail = |source| {
	var $rest = source.split_on("\n").first() ?? ""
	while $rest != "" {
		while $rest.starts_with(" ") {
			$rest = drop($rest, 1)
		}
		if $rest == "" {
			return True
		}
		if $rest.starts_with("\\") {
			$rest = drop($rest, 1)
		}
		if !$rest.starts_with("<") {
			return False
		}
		parts = drop($rest, 1).split_on(">")
		n = parts.first() ?? ""
		if parts.len() < 2 or !(n == "." or U64.from_str(n).is_ok()) {
			return False
		}
		$rest = drop($rest, n.to_utf8().len() + 2)
	}
	True
}
expect AsciiDocBlockHtml.verbatim("x <1> suffix", []) == "x &lt;1&gt; suffix"
expect AsciiDocBlockHtml.verbatim("x <1> foo <2>", []) == "x &lt;1&gt; foo <b class=\"conum\">(2)</b>"
expect AsciiDocBlockHtml.verbatim("x <1> <2>", []) == "x <b class=\"conum\">(1)</b> <b class=\"conum\">(2)</b>"
expect AsciiDocBlockHtml.verbatim("x \\<1> suffix", []) == "x \\&lt;1&gt; suffix"
expect AsciiDocBlockHtml.verbatim("x <!--1-->", []) == "x &lt;!--<b class=\"conum\">(1)</b>--&gt;"

# Verbatim's default substitutions only escape the three special HTML bytes.
# Avoid building a general inline pipeline for every plain listing block.
escape_verbatim : Str -> Str
escape_verbatim = |text| text.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;")
expect AsciiDocBlockHtml.verbatim("plain 'quoted' \\source & <tag>", []) == "plain 'quoted' \\source &amp; &lt;tag&gt;"

expect {
	source = "\\< \\> \\& \\\\ \\* \\' \\\" café 東京 🦊"
	AsciiDocBlockHtml.verbatim(source, [{ name: "subs", value: "specialcharacters" }]) == AsciiDocInline.render_with(source, [], { subs: "specialcharacters" })
}
