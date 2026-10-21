## HTML helpers shared by the document renderer. Inline fields marked with
## __rendered-source / __rendered-title have already passed through document
## state, preserving footnote numbering and cross-reference labels.
AsciiDocBlockHtml :: [].{
	Attribute : { name : Str, value : Str }
	Description : { term : Str, text : Str, body : Str }

	paragraph : Str, List(Attribute) -> Str
	paragraph = |source, attrs| paragraph_indexed(source, legacy_view(attrs))

	paragraph_indexed : Str, AsciiDocAttributes.View -> Str
	paragraph_indexed = |source, attrs| {
		html = match lookup_attr(attrs, "__rendered-source") {
			Ok(prepared) => prepared
			Err(_) => render_inline(source, attrs, { subs: if (has_attr(attrs, "subs")) attr(attrs, "subs") else "normal" })
		}
		if attr(attrs, "options").split_on(",").contains("hardbreaks") or has_attr(attrs, "hardbreaks-option") or has_attr(attrs, "hardbreaks") {
			replace(html, "\n", "<br>\n")
		} else html
	}

	verbatim : Str, List(Attribute) -> Str
	verbatim = |source, attrs| verbatim_indexed(source, legacy_view(attrs))

	verbatim_indexed : Str, AsciiDocAttributes.View -> Str
	verbatim_indexed = |source, attrs| {
		specified = lookup_attr(attrs, "subs")
		raw_subs = specified ?? ""
		modified = raw_subs.starts_with("+") or raw_subs.starts_with("-")
		subs = if (specified.is_err()) "specialcharacters" else if (modified) "specialcharacters,${raw_subs}" else raw_subs
		var $callouts = specified.is_err() or modified
		for spec in raw_subs.split_on(",") {
			if spec == "callouts" or spec == "+callouts" {
				$callouts = True
			} else if spec == "-callouts" or spec == "none" {
				$callouts = False
			}
		}
		text = trim_blank_lines(source)
		if !$callouts or !text.contains("<") or !text.contains(">") {
			return render_verbatim_text(text, attrs, subs)
		}
		bytes = text.to_utf8()
		scanned = callout_candidates(bytes)
		accepted = accepted_callouts(bytes, scanned.tokens, scanned.line_ends)
		if accepted.is_empty() {
			return render_verbatim_text(text, attrs, subs)
		}
		direct = subs == "specialcharacters"
		prefix = if (direct) "" else callout_prefix(bytes)
		var $chunks = []
		var $replacements = []
		var $auto = 1.U64
		var $copied = 0.U64
		var $index = accepted.len()
		# accepted is reverse source order; number and protect in source order.
		while $index > 0 {
			$index = $index - 1
			token = accepted.get($index) ?? empty_callout
			n = if (token.label == ".") $auto.to_str() else token.label
			if token.label == "." and !token.escaped {
				$auto = $auto + 1
			}
			callout = if (token.escaped) (if (direct) escape_verbatim("<${token.label}>") else render_inline("<${token.label}>", attrs, { subs: subs })) else "<b class=\"conum\">(${n})</b>"
			replacement = if token.comment {
				if (direct) "&lt;!--${callout}--&gt;" else "${render_inline("<!--", attrs, { subs: subs })}${callout}${render_inline("-->", attrs, { subs: subs })}"
			} else callout
			marker = if (direct) replacement else "${prefix}${$replacements.len().to_str()}END"
			if !direct {
				$replacements = $replacements.append(replacement)
			}
			literal = callout_slice(bytes, $copied, token.start - $copied)
			$chunks = $chunks.append(if (direct) escape_verbatim(literal) else literal).append(marker)
			$copied = token.end
		}
		tail = callout_slice(bytes, $copied, bytes.len() - $copied)
		$chunks = $chunks.append(if (direct) escape_verbatim(tail) else tail)
		if direct {
			# Escaping is independent per character; insert HTML directly between
			# escaped source spans without allocating/restoring placeholder text.
			return Str.join_with($chunks, "")
		}
		# Custom substitutions see the WHOLE protected text, preserving formatting
		# that spans one or more callout slots.
		html = render_verbatim_text(Str.join_with($chunks, ""), attrs, subs)
		restore_callouts(html, prefix, $replacements)
	}

	description : Str, Str, Str, List(Attribute), List(Description) -> Str
	description = |style, id, title, attrs, items| description_indexed(style, id, title, legacy_view(attrs), items)

	description_indexed : Str, Str, Str, AsciiDocAttributes.View, List(Description) -> Str
	description_indexed = |style, id, title, attrs, items| {
		idattr = if (id == "") "" else " id=\"${escape(id)}\""
		title_html = if (title == "") "" else "<div class=\"title\">${title}</div>\n"
		role = if (attr(attrs, "role") == "") "" else " ${attr(attrs, "role")}"
		if style == "qanda" {
			content = Str.join_with(items.map(|item| "<li>\n<p><em>${item.term}</em></p>${if (item.text == "") "" else "\n<p>${item.text}</p>"}${if (item.body == "") "" else "\n${item.body}"}\n</li>"), "\n")
			return "<div${idattr} class=\"qlist qanda${role}\">\n${title_html}<ol>\n${content}\n</ol>\n</div>"
		}
		if style == "horizontal" {
			content = Str.join_with(items.map(|item| "<tr>\n<td class=\"hdlist1\">\n${item.term}\n</td>\n<td class=\"hdlist2\">${if (item.text == "") "" else "\n<p>${item.text}</p>"}${if (item.body == "") "" else "\n${item.body}"}\n</td>\n</tr>"), "\n")
			return "<div${idattr} class=\"hdlist${role}\">\n${title_html}<table>\n${content}\n</table>\n</div>"
		}
		content = Str.join_with(items.map(|item| "<dt class=\"hdlist1\">${item.term}</dt>${if (item.text == "" and item.body == "") "" else "\n<dd>\n${if (item.text == "") "" else "<p>${item.text}</p>\n"}${if (item.body == "") "" else "${item.body}\n"}</dd>"}"), "\n")
		"<div${idattr} class=\"dlist${role}\">\n${title_html}<dl>\n${content}\n</dl>\n</div>"
	}

	list_wrapper : Str, Str, U64, Str, Str, List(Attribute), Str, Bool -> Str
	list_wrapper = |context, style, level, id, title, attrs, body, checklist| list_wrapper_indexed(context, style, level, id, title, legacy_view(attrs), body, checklist)

	list_wrapper_indexed : Str, Str, U64, Str, Str, AsciiDocAttributes.View, Str, Bool -> Str
	list_wrapper_indexed = |context, style, level, id, title, attrs, body, checklist| {
		tag = if (context == "ulist") "ul" else "ol"
		resolved = if (style != "") style else match level {
			2 => "loweralpha"
			3 => "lowerroman"
			4 => "upperalpha"
			5 => "upperroman"
			_ => "arabic"
		}
		extra = if (context == "olist") " ${resolved}" else if (context == "colist") " arabic" else if (style == "bibliography") " bibliography" else if (checklist) " checklist" else ""
		roles = if (attr(attrs, "role") == "") "" else " ${attr(attrs, "role")}"
		idattr = if (id == "") "" else " id=\"${escape(id)}\""
		title_html = if (title == "") "" else "<div class=\"title\">${title}</div>\n"
		tagattrs = if (context == "olist") " class=\"${resolved}\"${if (has_attr(attrs, "__implicit-list-style")) "" else list_type(resolved)}${if (attr(attrs, "start") == "") "" else " start=\"${escape(attr(attrs, "start"))}\""}" else if (style == "bibliography") " class=\"bibliography\"" else if (checklist) " class=\"checklist\"" else ""
		"<div${idattr} class=\"${context}${extra}${roles}\">\n${title_html}<${tag}${tagattrs}>\n${body}\n</${tag}>\n</div>"
	}
}
import AsciiDocInline
import AsciiDocAttributes
import AsciiDocSource

attr : AsciiDocAttributes.View, Str -> Str
attr = |attrs, name| lookup_attr(attrs, name) ?? ""

lookup_attr : AsciiDocAttributes.View, Str -> Try(Str, [Missing])
lookup_attr = |attrs, name| AsciiDocAttributes.lookup_last(attrs, name)

has_attr : AsciiDocAttributes.View, Str -> Bool
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
	if (b < 128) 1 else if (b < 224) 2 else if (b < 240) 3 else 4
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

legacy_view : List(AsciiDocBlockHtml.Attribute) -> AsciiDocAttributes.View
legacy_view = |attrs| { indexed: False, semantic: attrs, first: AsciiDocSource.from_dict(Dict.empty()), last: AsciiDocSource.from_dict(Dict.empty()), key: "", generation: 0, prefix: "", before: [], after: [] }

render_inline : Str, AsciiDocAttributes.View, { subs : Str } -> Str
render_inline = |source, attrs, options| {
	if attrs.indexed {
		AsciiDocInline.render_source_state(source, attrs.semantic, options, AsciiDocInline.initial_state, { first: attrs.first, token: attrs.key, generation: attrs.generation, prefix: attrs.prefix, before: attrs.before, after: attrs.after }).html
	} else {
		AsciiDocInline.render_with(source, attrs.semantic, options)
	}
}

# Byte positions always coincide with ASCII marker boundaries, hence UTF8
# scalar boundaries. No helper copies an unbounded suffix per input scalar.
CalloutToken : { start : U64, end : U64, start_line : U64, end_line : U64, label : Str, escaped : Bool, comment : Bool }

empty_callout : CalloutToken
empty_callout = { start: 0, end: 0, start_line: 0, end_line: 0, label: "", escaped: False, comment: False }

callout_at : List(U8), U64 -> U8
callout_at = |bytes, index| bytes.get(index) ?? 0

callout_slice : List(U8), U64, U64 -> Str
callout_slice = |bytes, start, len| Str.from_utf8_lossy(bytes.sublist({ start, len }))

render_verbatim_text : Str, AsciiDocAttributes.View, Str -> Str
render_verbatim_text = |text, attrs, subs| if (subs == "specialcharacters") escape_verbatim(text) else render_inline(text, attrs, { subs: subs })

callout_candidates : List(U8) -> { tokens : List(CalloutToken), line_ends : List(U64) }
callout_candidates = |bytes| {
	var $tokens = []
	var $line_ends = []
	var $line = 0.U64
	var $open = 0.U64
	var $open_line = 0.U64
	var $has_open = False
	var $i = 0.U64
	while $i < bytes.len() {
		byte = callout_at(bytes, $i)
		if byte == '<' {
			# A numeric label cannot contain '<'. An earlier open before this
			# one can therefore never be a recognized normal/comment callout.
			$open = $i
			$open_line = $line
			$has_open = True
		} else if byte == '>' and $has_open {
			comment = callout_at(bytes, $open + 1) == '!' and callout_at(bytes, $open + 2) == '-' and callout_at(bytes, $open + 3) == '-'
			start = $open + (if (comment) 4 else 1)
			closed = !comment or ($i >= $open + 6 and callout_at(bytes, $i - 1) == '-' and callout_at(bytes, $i - 2) == '-')
			end = if (comment and $i >= 2) $i - 2 else $i
			if closed and end >= start {
				label = callout_slice(bytes, start, end - start)
				# Preserve the compiler's exact numeric grammar; no new decimal
				# character filter substitutes for U64.from_str.
				if label == "." or (label != "" and U64.from_str(label).is_ok()) {
					escaped = $open > 0 and callout_at(bytes, $open - 1) == '\\'
					$tokens = $tokens.append({ start: if (escaped) $open - 1 else $open, end: $i + 1, start_line: $open_line, end_line: $line, label, escaped, comment })
				}
			}
			# If a comment encountered an earlier plain '>', that byte is an
			# invalid numeric payload too, so later '-->' cannot rescue it.
			$has_open = False
		}
		if byte == '\n' {
			$line_ends = $line_ends.append($i)
			$line = $line + 1
		}
		$i = $i + 1
	}
	{ tokens: $tokens, line_ends: $line_ends.append(bytes.len()) }
}

accepted_callouts : List(U8), List(CalloutToken), List(U64) -> List(CalloutToken)
accepted_callouts = |bytes, tokens, line_ends| {
	var $accepted = []
	var $line = line_ends.len()
	var $boundary = 0.U64
	var $i = tokens.len()
	while $i > 0 {
		$i = $i - 1
		token = tokens.get($i) ?? empty_callout
		if token.end_line != $line {
			$line = token.end_line
			$boundary = line_ends.get($line) ?? bytes.len()
			while $boundary > 0 and callout_at(bytes, $boundary - 1) == ' ' {
				$boundary = $boundary - 1
			}
		}
		if token.end == $boundary {
			$accepted = $accepted.append(token)
			# callout_tail allows ONLY ASCII spaces and optionally ONE slash
			# before each ordinary marker. Comments cannot extend that tail.
			if !token.comment and token.start_line == token.end_line {
				$boundary = token.start
				while $boundary > 0 and callout_at(bytes, $boundary - 1) == ' ' {
					$boundary = $boundary - 1
				}
			}
		}
	}
	$accepted
}

callout_matches : List(U8), List(U8), U64 -> Bool
callout_matches = |bytes, needle, start| {
	if start > bytes.len() or needle.len() > bytes.len() - start {
		return False
	}
	var $i = 0.U64
	while $i < needle.len() {
		if callout_at(bytes, start + $i) != callout_at(needle, $i) {
			return False
		}
		$i = $i + 1
	}
	True
}

# Exactly the old shortest base+X* absent from the source. The base has no
# self-overlap, and a run of X cannot contain another base occurrence.
callout_prefix : List(U8) -> Str
callout_prefix = |bytes| {
	base = "ROCASCIIDOCCALLOUT"
	needle = base.to_utf8()
	var $seen = False
	var $longest = 0.U64
	var $i = 0.U64
	while $i < bytes.len() {
		if callout_at(bytes, $i) == 'R' and callout_matches(bytes, needle, $i) {
			$seen = True
			var $j = $i + needle.len()
			while $j < bytes.len() and callout_at(bytes, $j) == 'X' {
				$j = $j + 1
			}
			run = $j - $i - needle.len()
			$longest = if (run > $longest) run else $longest
			$i = $j
		} else {
			$i = $i + 1
		}
	}
	if (!$seen) base else base.concat(Str.from_utf8_lossy(List.repeat('X', $longest + 1)))
}

# Restore canonical known slots in one pass. Unknown/malformed/noncanonical
# indices remain exact literal text, matching the old exact-string replaces.
restore_callouts : Str, Str, List(Str) -> Str
restore_callouts = |html, prefix, replacements| {
	bytes = html.to_utf8()
	needle = prefix.to_utf8()
	var $parts = []
	var $copied = 0.U64
	var $i = 0.U64
	while $i < bytes.len() {
		if callout_at(bytes, $i) == 'R' and callout_matches(bytes, needle, $i) {
			start = $i + needle.len()
			var $end = start
			while $end < bytes.len() and callout_at(bytes, $end) >= '0' and callout_at(bytes, $end) <= '9' {
				$end = $end + 1
			}
			if $end > start and callout_at(bytes, $end) == 'E' and callout_at(bytes, $end + 1) == 'N' and callout_at(bytes, $end + 2) == 'D' {
				index_text = callout_slice(bytes, start, $end - start)
				match U64.from_str(index_text) {
					Ok(index) if index.to_str() == index_text => match replacements.get(index) {
						Ok(replacement) => {
							$parts = $parts.append(callout_slice(bytes, $copied, $i - $copied)).append(replacement)
							$i = $end + 3
							$copied = $i
						}
						Err(_) => {
							$i = $end + 3
						}
					}
					_ => {
						$i = $end + 3
					}
				}
			} else {
				$i = $end
			}
		} else {
			$i = $i + 1
		}
	}
	$parts = $parts.append(callout_slice(bytes, $copied, bytes.len() - $copied))
	Str.join_with($parts, "")
}

# Default fragments preserve escaped comments and dot labels while only real
# automatic callouts advance numbering, including adjacent Unicode text.
expect AsciiDocBlockHtml.verbatim("α \\<.>\nβ <!--.-->\nγ \\<!--2-->\nδ <.>", []) == "α &lt;.&gt;\nβ &lt;!--<b class=\"conum\">(1)</b>--&gt;\nγ &lt;!--&lt;2&gt;--&gt;\nδ <b class=\"conum\">(2)</b>"

# Modified quote substitutions keep their existing general inline fragment
# handling and whole-text formatting, rather than using the default shortcut.
expect AsciiDocBlockHtml.verbatim("*bold* <!--1-->\n\\<2>", [{ name: "subs", value: "+quotes" }]) == "<strong>bold</strong> &lt;!--<b class=\"conum\">(1)</b>--&gt;\n&lt;2&gt;"

# Explicit attribute-only/custom callouts do not escape comment wrappers or
# escaped markers when specialcharacters is absent.
expect AsciiDocBlockHtml.verbatim("{word} <!--1-->\n\\<2>", [{ name: "subs", value: "attributes,callouts" }, { name: "word", value: "Roc" }]) == "Roc <!--<b class=\"conum\">(1)</b>-->\n<2>"
