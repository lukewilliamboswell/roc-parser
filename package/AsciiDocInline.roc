import unicode.GeneralCategory
import unicode.Scalar

## Inline substitutions for AsciiDoc HTML5 output. Attributes are supplied by
## the document processor, keeping this module independent of filesystem IO.
AsciiDocInline :: [].{
	render : Str, List({ name : Str, value : Str }) -> Str
	render = |source, attrs| render_with(source, attrs, { subs: "normal" })

	## Substitution groups: normal, verbatim, none, specialcharacters, quotes,
	## attributes, replacements, macros and post_replacements
	## +/- modifiers.
	render_with : Str, List({ name : Str, value : Str }), { subs : Str } -> Str
	render_with = |source, attrs, options| {
		render_state(source, attrs, options, initial_state).html
	}

	## Apply definition-time substitutions: ordinary values use header
	## specialcharacters and attributes; a whole-value pass macro chooses its
	## own substitution list. Values are bounded as in the reference engine.
	attribute_value : Str, List({ name : Str, value : Str }) -> Str
	attribute_value = |source, attrs| {
		value = if source.starts_with("pass:") and source.ends_with("]") {
			match find(source, "[", 5) {
				Ok(open) => {
					subs = slice(source, 5, open - 5)
					body = slice(source, open + 1, source.to_utf8().len() - open - 2)
					if subs == "" body else render_with(body, attrs, { subs: subs })
				}
				Err(_) => render_with(escape(source), attrs, { subs: "attributes" })
			}
		} else if !source.contains("{") {
			escape(source)
		} else {
			render_with(escape(source), attrs, { subs: "attributes" })
		}
		bounded_attribute(value)
	}

	State : { emitted_footnotes : List(U64), footnotes : List({ name : Str, text : Str }), note_index : Dict(Str, U64), indexed_note_count : U64, refs : List({ id : Str, text : Str }), ref_index : Dict(Str, Str), indexed_ref_count : U64, attributes : Dict(Str, Str), removed : Dict(Str, Bool), snapshot : List({ name : Str, value : Str }), snapshot_index : Dict(Str, Str), tag_prefix : Str }

	## Start a new document; reuse the returned state between its blocks.
	initial_state : State
	initial_state = { emitted_footnotes: [], footnotes: [], note_index: Dict.empty(), indexed_note_count: 0, refs: [], ref_index: Dict.empty(), indexed_ref_count: 0, attributes: Dict.empty(), removed: Dict.empty(), snapshot: [], snapshot_index: Dict.empty(), tag_prefix: "\u(1)" }

	## Replace the reference registry and build its lookup index once.
	with_refs : State, List({ id : Str, text : Str }) -> State
	with_refs = |state, refs| {
		index = refs.fold(Dict.empty(), |found, ref| if found.contains(ref.id) found else found.insert(ref.id, ref.text))
		{ ..state, refs, ref_index: index, indexed_ref_count: refs.len() }
	}

	## Carry footnote numbering and references across blocks in document order.
	render_state : Str, List({ name : Str, value : Str }), { subs : Str }, State -> { html : Str, state : State }
	render_state = |input, attrs, options, state| {
		# The common pipeline retains its fused scanner. Explicit selections
		# use the engine's ordered substitution list, including modifiers.
		if options.subs == "normal" or options.subs == "n" {
			return render_state_fixed(input, attrs, options, state, False)
		}
		# Header attributes follow special characters, so inserted attribute
		# values must not be escaped again. This pair needs one state pass.
		if options.subs == "specialcharacters,attributes" {
			return render_state_fixed(escape(input), attrs, { subs: "attributes" }, state, True)
		}
		stages = substitution_stages(options.subs)
		if stages == normal_stages {
			return render_state_fixed(input, attrs, { subs: "normal" }, state, False)
		}
		render_ordered(input, attrs, stages, state)
	}
}

render_state_fixed : Str, List({ name : Str, value : Str }), { subs : Str }, AsciiDocInline.State, Bool -> { html : Str, state : AsciiDocInline.State }
render_state_fixed = |input, attrs, options, state, prepared| {
	prior_notes = state.footnotes.len()
	indexed = if state.indexed_ref_count == state.refs.len() state else AsciiDocInline.with_refs(state, state.refs)
	original = { ..(if options.subs == "normal" or options.subs == "n" normal else groups(options.subs)), ref_index: indexed.ref_index }
	refreshed = refresh_state(attrs, indexed)
	if !needs_substitutions(input, original) {
		return { html: input, state: refreshed }
	}
	quote_groups = if prepared { ..original, macros: False } else original
	before = if (original.quotes or original.macros) and !original.attr_quotes and quote_needed(input, quote_groups) quote_state(input, attrs, quote_groups, refreshed) else { source: input, prefix: "", tags: [], deferred: [] }
	mutation = if original.attributes mutate_attributes(before.source, attrs, original, refreshed, before.prefix, before.tags) else { source: before.source, state: refreshed, tags: before.tags, prefix: before.prefix }
	# Plain attribute/counter results need no second attribute merge or scan.
	# Preserve protected quote/passthrough slots and deferred stateful macros.
	if !original.attr_quotes and mutation.tags.is_empty() and before.deferred.is_empty() and !needs_substitutions(mutation.source, { ..original, quotes: False, attributes: False }) {
		return { html: mutation.source, state: mutation.state }
	}
	quotes = if original.quotes and original.attr_quotes quote_state(mutation.source, attrs, original, mutation.state) else { source: mutation.source, prefix: mutation.prefix, tags: mutation.tags, deferred: before.deferred }
	# Footnotes also use protected slots, including notes inserted by attributes.
	quoted = if original.macros and quotes.prefix == "" and quotes.source.contains("footnote:") { ..quotes, prefix: attribute_delimiter(quotes.source, attrs, "") } else quotes
	source = quoted.source
	g = { ..original, quotes: False, attributes: False, passes_done: True }
	reference_attrs = (if g.macros and macro_candidate(source) effective_attributes(attrs, mutation.state) else attrs).concat(if source.contains("mailto:") tag_attributes(quoted.prefix, quoted.tags) else [])
	if !g.macros {
		return finish_render(scan(source, reference_attrs, g, 0), attrs, mutation.state, quoted, prior_notes)
	}
	if !source.contains("footnote:") {
		return finish_render(scan(source, reference_attrs, g, 0), attrs, mutation.state, quoted, prior_notes)
	}
	indexed_notes = with_note_index(mutation.state)
	var $notes = indexed_notes.footnotes
	var $names = indexed_notes.note_index
	# Release both growing collections from the old record before updates.
	base_state = { ..indexed_notes, footnotes: [], note_index: Dict.empty() }
	notes_start = $notes.len()
	var $render_tags = quoted.tags
	var $prepared = ""
	var $pos = 0
	bytes = source.to_utf8()
	marker = "footnote:".to_utf8()
	while $pos < bytes.len() {
		escaped = $pos > 0 and (bytes.get($pos - 1) ?? 0) == '\\'
		var $handled = False
		if !escaped and bytes.drop_first($pos).starts_with(marker) {
			var $open = $pos + marker.len()
			while $open < bytes.len() and (bytes.get($open) ?? 0) != '[' and !space(bytes.get($open) ?? 0) {
				$open = $open + 1
			}
			if (bytes.get($open) ?? 0) == '[' {
				match footnote_end_at(source, bytes, $open, reference_attrs, g) {
					Ok(end) => {
						name = slice(source, $pos + marker.len(), $open - $pos - marker.len())
						text = slice(source, $open + 1, end - $open - 1)
						existing = $names.get(name) ?? 0
						reference = text == "" or existing > 0
						number = if reference existing else $notes.len() + 1
						if number > 0 {
							if !reference {
								$notes = $notes.append({ name, text: scan(text, reference_attrs, g, 0) })
								if name != "" {
									$names = $names.insert(name, number)
								}
							}
							n = number.to_str()
							id = if name == "" "" else " id=\"_footnote_${escape(name)}\""
							html = if reference "<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_${n}\" title=\"View footnote.\">${n}</a>]</sup>" else "<sup class=\"footnote\"${id}>[<a id=\"_footnoteref_${n}\" class=\"footnote\" href=\"#_footnotedef_${n}\" title=\"View footnote.\">${n}</a>]</sup>"
							$prepared = $prepared.concat(token(quoted.prefix, $render_tags.len()))
							$render_tags = $render_tags.append(html)
						} else {
							$prepared = $prepared.concat(token(quoted.prefix, $render_tags.len()))
							$render_tags = $render_tags.append("<sup class=\"footnoteref red\" title=\"Unresolved footnote reference.\">[${escape(name)}]</sup>")
						}
						$pos = end + 1
						$handled = True
					}
					Err(_) => {}
				}
			}
		}
		if !$handled {
			end = find_bytes(bytes, marker, $pos + 1) ?? bytes.len()
			$prepared = $prepared.concat(slice(source, $pos, end - $pos))
			$pos = end
		}
	}
	finish_render(scan($prepared, reference_attrs, g, 0), attrs, { ..base_state, footnotes: $notes, note_index: $names, indexed_note_count: $notes.len() }, { ..quoted, tags: $render_tags }, notes_start)
}

# Build an index for caller-supplied notes once; ordinary document renders
# carry and update it across blocks, preserving the first definition's number.
with_note_index : AsciiDocInline.State -> AsciiDocInline.State
with_note_index = |state| {
	if state.indexed_note_count == state.footnotes.len() {
		return state
	}
	var $names = Dict.empty()
	var $number = 0.U64
	for note in state.footnotes {
		$number = $number + 1
		if note.name != "" and !$names.contains(note.name) {
			$names = $names.insert(note.name, $number)
		}
	}
	{ ..state, note_index: $names, indexed_note_count: state.footnotes.len() }
}

Groups : { special : Bool, quotes : Bool, attributes : Bool, replacements : Bool, macros : Bool, post : Bool, passes_done : Bool, attr_quotes : Bool, ref_index : Dict(Str, Str) }

normal : Groups
normal = { special: True, quotes: True, attributes: True, replacements: True, macros: True, post: True, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }

empty : Groups
empty = { special: False, quotes: False, attributes: False, replacements: False, macros: False, post: False, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }

groups : Str -> Groups
groups = |spec| {
	var $g = if spec.starts_with("+") or spec.starts_with("-") normal else empty
	for raw in spec.split_on(",") {
		name = raw.trim()
		enabled = !name.starts_with("-")
		key = if name.starts_with("+") or name.starts_with("-") drop(name, 1) else name
		$g = match key {
			"normal" => normal
			"n" => normal
			"none" => empty
			"v" => { ..empty, special: True }
			"verbatim" => { special: True, quotes: False, attributes: False, replacements: False, macros: False, post: False, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }
			"specialcharacters" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"specialchars" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"q" => { special: $g.special, quotes: enabled, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"c" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"quotes" => { special: $g.special, quotes: enabled, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"a" => { ..$g, attributes: enabled }
			"r" => { ..$g, replacements: enabled }
			"m" => { ..$g, macros: enabled }
			"p" => { ..$g, post: enabled }
			"attributes" => { special: $g.special, quotes: $g.quotes, attributes: enabled, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"replacements" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: enabled, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"macros" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: enabled, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"post_replacements" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: enabled, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			_ => $g
		}
	}
	ordering = spec.split_on(",").map(|part| part.trim())
	var $seen_attributes = False
	var $attr_quotes = False
	for part in ordering {
		if part == "attributes" or part == "+attributes" or part == "a" {
			$seen_attributes = True
		}
		if $seen_attributes and (part == "quotes" or part == "+quotes" or part == "q") {
			$attr_quotes = True
		}
	}
	{ ..$g, attr_quotes: $attr_quotes }
}

drop : Str, U64 -> Str
drop = |s, n| Str.from_utf8_lossy(s.to_utf8().drop_first(n))

take : Str, U64 -> Str
take = |s, n| Str.from_utf8_lossy(s.to_utf8().take_first(n))

slice : Str, U64, U64 -> Str
slice = |s, start, len| Str.from_utf8_lossy(s.to_utf8().sublist({ start, len }))

find : Str, Str, U64 -> Try(U64, [Missing])
find = |s, needle, start| find_bytes(s.to_utf8(), needle.to_utf8(), start)

space : U8 -> Bool
space = |c| c == ' ' or c == '\n' or c == '\t' or c == '\r'

word : U8 -> Bool
word = |c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_'

lookup : List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
lookup = |attrs, key| {
	for attr in attrs {
		if attr.name == key {
			return Ok(attr.value)
		}
	}
	match key {
		"blank" => Ok("")
		"sp" => Ok(" ")
		"nbsp" => Ok("&#160;")
		"zwsp" => Ok("&#8203;")
		"apos" => Ok("&#39;")
		"quot" => Ok("&#34;")
		"amp" => Ok("&")
		"lt" => Ok("<")
		"gt" => Ok(">")
		"plus" => Ok("&#43;")
		"vbar" => Ok("|")
		"brvbar" => Ok("&#166;")
		"cpp" => Ok("C&#43;&#43;")
		"empty" => Ok("")
		"backslash" => Ok("\\")
		"backtick" => Ok("`")
		"asterisk" => Ok("*")
		"two-colons" => Ok("::")
		"two-semicolons" => Ok(";;")
		"startsb" => Ok("[")
		"endsb" => Ok("]")
		"deg" => Ok("&#176;")
		"wj" => Ok("&#8288;")
		_ => Err(Missing)
	}
}

escape : Str -> Str
escape = |s| s.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;")

repl : Str, Str, Str -> Str
repl = |s, before, after| Str.join_with(s.split_on(before), after)

scan : Str, List({ name : Str, value : Str }), Groups, U64 -> Str
scan = |input, incoming_attrs, options, depth| {
	if depth > 24 {
		return escape(input)
	}
	quoted = if options.quotes quote_all(input, incoming_attrs, options, depth) else { source: input, prefix: "", tags: [], deferred: [] }
	source = quoted.source
	attrs = incoming_attrs.concat(if source.contains("mailto:") tag_attributes(quoted.prefix, quoted.tags) else [])
	g = { ..options, quotes: False, passes_done: options.passes_done or options.quotes }
	bytes = source.to_utf8()
	has_email = bytes.contains('@')
	has_colon = bytes.contains(':')
	var $i = 0
	var $out = ""
	while $i < bytes.len() {
		simple = fixed_replacement(bytes, $i, g)
		if simple.used > 0 {
			$out = $out.concat(simple.html)
			$i = $i + simple.used
		} else if !syntax_candidate(bytes, $i, g, has_email, has_colon) {
			var $end = $i + 1
			while $end < bytes.len() and !syntax_candidate(bytes, $end, g, has_email, has_colon) {
				$end = $end + 1
			}
			$out = $out.concat(slice(source, $i, $end - $i))
			$i = $end
		} else {
			for _ in [0] {
				rest = drop(source, $i)
				c = bytes.get($i) ?? 0
				next = bytes.get($i + 1) ?? 0
				prev = if $i == 0 0 else bytes.get($i - 1) ?? 0
				if c == '\\' and $i + 1 < bytes.len() and (g.quotes or g.macros or g.attributes) {
					escaped_macro = parse_macro(drop(rest, 1), attrs, g, depth)
					if escaped_macro.used > 0 {
						$out = $out.concat(escape(slice(rest, 1, escaped_macro.used)))
						$i = $i + escaped_macro.used + 1
						break
					}
					if drop(rest, 1).starts_with("pass:") {
						match find(rest, "]", 1) {
							Ok(end) => {
								$out = $out.concat(escape(slice(rest, 1, end)))
								$i = $i + end + 1
								break
							}
							Err(_) => {}
						}
					}
				}
				if g.attributes and c == '{' {
					match find(source, "}", $i + 1) {
						Ok(end) => {
							key = slice(source, $i + 1, end - $i - 1).with_ascii_lowercased()
							match lookup(attrs, key) {
								Ok(value) => {
									$out = $out.concat(if lookup([], key).is_ok() value else scan(value, attrs, { special: False, quotes: g.attr_quotes, attributes: False, replacements: g.replacements, macros: g.macros, post: g.post, passes_done: g.passes_done, attr_quotes: g.attr_quotes, ref_index: g.ref_index }, depth + 1))
									$i = end + 1
									break
								}
								Err(_) => {}
							}
						}
						Err(_) => {}
					}
				}
				if g.macros and !word(prev) and prev != ':' and prev != '/' and prev != '>' and (word(c)) {
					email = email_link(rest)
					if email.used > 0 {
						$out = $out.concat(email.html)
						$i = $i + email.used
						break
					}
				}
				if !g.passes_done and g.macros and (rest.starts_with("+++") or rest.starts_with("++") or rest.starts_with("pass:")) {
					marker = if rest.starts_with("+++") "+++" else if rest.starts_with("++") "++" else "pass:"
					if marker == "pass:" {
						match find(source, "[", $i + 5) {
							Ok(open) => match bracket_end(source, open) {
								Ok(end) => {
									sub = slice(source, $i + 5, open - $i - 5)
									$out = $out.concat(scan(slice(source, open + 1, end - open - 1), attrs, groups(sub), depth + 1))
									$i = end + 1
									break
								}
								Err(_) => {}
							}
							Err(_) => {}
						}
					} else {
						n = marker.to_utf8().len()
						match find(source, marker, $i + n) {
							Ok(end) => {
								content = slice(source, $i + n, end - $i - n)
								$out = $out.concat(if n == 3 content else escape(content))
								$i = end + n
								break
							}
							Err(_) => {}
						}
					}
				}
				if g.macros and c == '<' and (drop(rest, 1).starts_with("https://") or drop(rest, 1).starts_with("http://")) {
					match find(rest, ">", 1) {
						Ok(end) => {
							$out = $out.concat(scan(slice(rest, 1, end - 1), attrs, g, depth + 1))
							$i = $i + end + 1
							break
						}
						Err(_) => {}
					}
				}
				if g.macros {
					macro = if prev == '<' and (rest.starts_with("https://") or rest.starts_with("http://")) { used: 0, html: "" } else parse_macro(rest, attrs, g, depth)
					if macro.used > 0 {
						$out = $out.concat(macro.html)
						$i = $i + macro.used
						break
					}
				}
				if g.post and rest == " +" {
					$out = $out.concat("<br>")
					$i = $i + 2
					break
				}
				if g.post and rest.starts_with(" +\n") {
					$out = $out.concat("<br>\n")
					$i = $i + 3
					break
				}
				if g.replacements and rest.starts_with("&amp;") {
					match find(rest, ";", 5) {
						Ok(end) if end > 5 and end < 40 => {
							entity = slice(rest, 5, end - 5)
							if g.replacements and entity.to_utf8().all(|b| word(b) or b == '#') {
								$out = $out.concat("&${entity};")
								$i = $i + end + 1
								break
							}
						}
						_ => {}
					}
				}
				if g.replacements {
					var $used = 0
					for rule in [{ source: "(C)", target: "&#169;" }, { source: "(R)", target: "&#174;" }, { source: "(TM)", target: "&#8482;" }, { source: "...", target: "&#8230;&#8203;" }, { source: "->", target: "&#8594;" }, { source: "<-", target: "&#8592;" }, { source: "=>", target: "&#8658;" }, { source: "<=", target: "&#8656;" }] {
						if rest.starts_with(rule.source) {
							$out = $out.concat(rule.target)
							$used = rule.source.to_utf8().len()
							break
						}
					}
					if $used > 0 {
						$i = $i + $used
						break
					}
					if rest.starts_with(" -- ") or rest.starts_with(" --\n") {
						$out = $out.concat("&#8201;&#8212;&#8201;")
						$i = $i + 4
						break
					}
					if rest.starts_with("--") and word(prev) and word(bytes.get($i + 2) ?? 0) {
						$out = $out.concat("&#8212;&#8203;")
						$i = $i + 2
						break
					}
					if c == '\'' and word(prev) and word(next) {
						$out = $out.concat("&#8217;")
						$i = $i + 1
						break
					}
				}
				if g.special {
					if c == '<' {
						$out = $out.concat("&lt;")
						$i = $i + 1
						break
					}
					if c == '>' {
						$out = $out.concat("&gt;")
						$i = $i + 1
						break
					}
					if c == '&' {
						match find(source, ";", $i + 1) {
							Ok(end) if end - $i < 32 => {
								entity = slice(source, $i + 1, end - $i - 1)
								if g.replacements and entity.to_utf8().all(|b| word(b) or b == '#') {
									$out = $out.concat(slice(source, $i, end - $i + 1))
									$i = end + 1
									break
								}
							}
							_ => {}
						}
						$out = $out.concat("&amp;")
						$i = $i + 1
						break
					}
				}
				n = if c < 128 1 else if c < 224 2 else if c < 240 3 else 4
				var $end = $i + n
				while $end < bytes.len() {
					b = bytes.get($end) ?? 0
					before = bytes.get($end - 1) ?? 0
					if " \n=\\{[<&>+.'\"-(".to_utf8().contains(b) or (g.macros and word(b) and !word(before)) {
						break
					}
					$end = $end + 1
				}
				$out = $out.concat(slice(source, $i, $end - $i))
				$i = $end
			}
		}
	}
	restore_tags($out, quoted.prefix, quoted.tags)
}

Macro : { used : U64, html : Str }

parse_macro : Str, List({ name : Str, value : Str }), Groups, U64 -> Macro
parse_macro = |source, attrs, g, depth| {
	none = { used: 0, html: "" }
	if source.starts_with("(((") or source.starts_with("((") {
		n = if source.starts_with("(((") 3 else 2
		match find(source, if n == 3 ")))" else "))", n) {
			Ok(end) => {
				return { used: end + n, html: if n == 3 "" else scan(slice(source, n, end - n), attrs, g, depth + 1) }
			}
			Err(_) => {}
		}
	}
	if source.starts_with("[[[") {
		match find(source, "]]]", 3) {
			Ok(end) if !take(source, end).contains("\n") => {
				id = slice(source, 3, end - 3).split_on(",").first() ?? ""
				return {
					used: end + 3,
					html: if (lookup(attrs, "style") ?? "") == "bibliography" or lookup(attrs, "bibliography").is_ok() {
						label = if slice(source, 3, end - 3).contains(",") Str.join_with(slice(source, 3, end - 3).split_on(",").drop_first(1), ",").trim() else id
						"<a id=\"${escape(id)}\"></a>[${label}]"
					} else "[<a id=\"${escape(id)}\"></a>]",
				}
			}
			_ => {}
		}
	}
	xref_open = if source.starts_with("&lt;&lt;") 8 else if g.special and source.starts_with("<<") 2 else 0
	if xref_open > 0 {
		match find(source, if xref_open == 8 "&gt;&gt;" else ">>", xref_open) {
			Ok(end) => {
				parts = slice(source, xref_open, end - xref_open).split_on(",")
				target = parts.first() ?? ""
				text = if parts.len() > 1 Str.join_with(parts.drop_first(1), ",").trim() else reference_label(attrs, g, target) ?? "[${target}]"
				return { used: end + xref_open, html: "<a href=\"#${if g.special escape_entities(target) else target}\">${scan(text, attrs, if parts.len() > 1 g else { ..normal, ref_index: g.ref_index }, depth + 1)}</a>" }
			}
			_ => {}
		}
	}
	if source.starts_with("[[") {
		match find(source, "]]", 2) {
			Ok(end) if !take(source, end).contains("\n") => {
				id = (slice(source, 2, end - 2).split_on(",").first() ?? "").trim()
				return { used: end + 2, html: "<a id=\"${escape(id)}\"></a>" }
			}
			_ => {}
		}
	}
	var $prefix = ""
	for prefix in ["indexterm:", "indexterm2:", "https://", "http://", "ftp://", "irc://", "link:", "mailto:", "image:", "xref:", "anchor:", "footnote:", "footnoteref:", "kbd:", "btn:", "menu:", "icon:"] {
		if source.starts_with(prefix) {
			$prefix = prefix
			break
		}
	}
	if $prefix == "" {
		return none
	}
	if !$prefix.ends_with("//") and drop(source, $prefix.to_utf8().len()).starts_with(":") {
		return none
	}
	is_url = $prefix.ends_with("//")
	if is_url and lookup(attrs, "__suppress-url-label").is_ok() {
		return none
	}
	match find(source, "[", $prefix.to_utf8().len()) {
		Ok(open) if !take(source, open).to_utf8().any(space) => {
			match first_bracket_end(source, open) {
				Ok(end) => {
					target = if is_url take(source, open) else slice(source, $prefix.to_utf8().len(), open - $prefix.to_utf8().len())
					if is_url and target == $prefix {
						return none
					}
					label = repl(slice(source, open + 1, end - open - 1), "\\]", "]")
					parts = if (($prefix == "link:" or is_url or $prefix == "xref:") and !label.contains("=")) or ($prefix == "mailto:" and !label.contains(",")) [label] else macro_parts(label)
					positional = parts
					first_raw = (positional.first() ?? "").trim()
					first = if named_item(first_raw) and ($prefix != "mailto:" or label.contains(",")) "" else if $prefix == "image:" or label.contains("=") or ($prefix == "mailto:" and label.contains(",")) unquote_attribute(first_raw) else first_raw
					text = if first == "" escape_entities(target) else scan(first, if is_url attrs.append({ name: "__suppress-url-label", value: "" }) else attrs, g, depth + 1)
					html = match $prefix {
						"indexterm:" => ""
						"indexterm2:" => scan(label, attrs, g, depth + 1)
						"image:" => {
							default_alt = repl(repl((target.split_on("/").last() ?? target), "_", " "), "-", " ").split_on(".").first() ?? ""
							alt = named(parts, "alt") ?? if first == "" or first.contains("=") default_alt else first
							width_value = named(parts, "width") ?? if positional.len() > 1 and !named_item(positional.get(1) ?? "") unquote_attribute((positional.get(1) ?? "").trim()) else ""
							height_value = named(parts, "height") ?? if positional.len() > 2 and !named_item(positional.get(2) ?? "") unquote_attribute((positional.get(2) ?? "").trim()) else ""
							width = if width_value == "" "" else " width=\"${escape_entities(width_value)}\""
							height = if height_value == "" "" else " height=\"${escape_entities(height_value)}\""
							title = match named(parts, "title") {
								Ok(value) => " title=\"${html_attribute(value)}\""
								Err(_) => ""
							}
							image_dir = lookup(attrs, "imagesdir") ?? ""
							src = if image_dir == "" or target.contains("://") or target.starts_with("/") target else "${image_dir}/${target}"
							image = "<img src=\"${html_attribute(src)}\" alt=\"${html_attribute(alt)}\"${width}${height}${title}>"
							content = match named(parts, "link") {
								Ok(link) => "<a class=\"image\" href=\"${html_attribute(link)}\">${image}</a>"
								Err(_) => image
							}
							"<span class=\"image${
								match named(parts, "role") {
									Ok(role) => " ${html_attribute(role)}"
									Err(_) => ""
								}
							}\">${content}</span>"
						}
						"xref:" => {
							href = if target.contains(".adoc") repl(target, ".adoc", ".html") else if target.contains("#") or path_has_extension(target) target else "#${target}"
							display = if first == "" {
								if href.starts_with("#") {
									id = drop(href, 1)
									match reference_label(attrs, g, id) {
										Ok(ref_label) => scan(ref_label, attrs, { ..normal, ref_index: g.ref_index }, depth + 1)
										Err(_) => "[${id}]"
									}
								} else href.split_on("#").first() ?? href
							} else if label.contains(",%") scan(label, attrs, g, depth + 1) else text
							"<a href=\"${escape(href)}\"${
								match named(parts, "role") {
									Ok(role) => " class=\"${html_attribute(role)}\""
									Err(_) => ""
								}
							}>${display}</a>"
						}
						"anchor:" => "<a id=\"${escape_entities(target)}\"></a>"
						"mailto:" => {
							subject = match positional.get(1) {
								Ok(value) if !named_item(value) => Ok(value)
								_ => Err(Missing)
							}
							body = match positional.get(2) {
								Ok(value) if !named_item(value) => Ok(value)
								_ => Err(Missing)
							}
							query = match subject {
								Ok(value) => "?subject=${uri_component(restore_tag_attributes(scan(unquote_attribute(value.trim()), attrs, { ..empty, special: g.special, replacements: g.replacements }, depth + 1), attrs))}${
									match body {
										Ok(body_value) => "&amp;body=${uri_component(restore_tag_attributes(scan(unquote_attribute(body_value.trim()), attrs, { ..empty, special: g.special, replacements: g.replacements }, depth + 1), attrs))}"
										Err(_) => ""
									}
								}"
								Err(_) => ""
							}
							"<a href=\"mailto:${escape_entities(target)}${query}\"${link_attributes(if label.contains(",") parts else [], False, label.ends_with("^"))}>${text}</a>"
						}
						"kbd:" => {
							if lookup(attrs, "experimental").is_err() {
								return none
							}
							keys = if label.ends_with("++") repl(take(label, label.to_utf8().len() - 2), "+", ",").split_on(",").append("+") else repl(label, "+", ",").split_on(",")
							if keys.len() == 1 "<kbd>${escape(label)}</kbd>" else "<span class=\"keyseq\">${Str.join_with(keys.map(|key| "<kbd>${escape(key.trim())}</kbd>"), "+")}</span>"
						}
						"btn:" => {
							if lookup(attrs, "experimental").is_err() {
								return none
							}
							"<b class=\"button\">${escape(label)}</b>"
						}
						"menu:" => {
							if lookup(attrs, "experimental").is_err() {
								return none
							}
							items = label.split_on(">").map(|item| item.trim())
							arrow = "&#160;<b class=\"caret\">&#8250;</b> "
							if label == "" "<b class=\"menuref\">${escape_entities(target)}</b>" else {
								middle = if items.len() > 1 Str.join_with(items.take_first(items.len() - 1).map(|item| "<b class=\"submenu\">${escape(item)}</b>"), arrow).concat(arrow) else ""
								"<span class=\"menuseq\"><b class=\"menu\">${escape_entities(target)}</b>${arrow}${middle}<b class=\"menuitem\">${escape(items.last() ?? "")}</b></span>"
							}
						}
						"footnote:" => {
							if label == "" {
								"<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
							} else {
								id = if target == "" "" else " id=\"_footnote_${escape_entities(target)}\""
								"<sup class=\"footnote\"${id}>[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
							}
						}
						"footnoteref:" => "<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
						"icon:" => "<span class=\"icon\">[${escape_entities(target)}&#93;</span>"
						_ => {
							href = repl(escape_entities(target), " ", "%20")
							attr_text = link_attributes(parts, first == "", label.ends_with("^"))
							display = if label.ends_with("^") take(text, text.to_utf8().len() - 1) else if label.contains(",%") scan(label, attrs, g, depth + 1) else text
							"<a href=\"${href}\"${attr_text}>${display}</a>"
						}
					}
					return { used: end + 1, html }
				}
				Err(_) => {}
			}
		}
		_ => {}
	}
	if is_url {
		var $end = 0
		for b in source.to_utf8() {
			if b < 32 or space(b) or b == '<' or b == '>' or b == '[' or b == ']' {
				break
			}
			$end = $end + 1
		}
		while $end > 0 and ".,;:!?)".to_utf8().contains(source.to_utf8().get($end - 1) ?? 0) {
			$end = $end - 1
		}
		target = take(source, $end)
		if target == $prefix {
			return none
		}
		rendered_target = if g.special escape_entities(target) else target
		return { used: $end, html: "<a href=\"${rendered_target}\" class=\"bare\">${rendered_target}</a>" }
	}
	none
}

expect AsciiDocInline.render("*bold* _em_ `code` #mark# ^up^ ~down~", []) == "<strong>bold</strong> <em>em</em> <code>code</code> <mark>mark</mark> <sup>up</sup> <sub>down</sub>"
expect AsciiDocInline.render("a**b**c a*b*c", []) == "a<strong>b</strong>c a*b*c"
expect AsciiDocInline.render("*one _two_*", []) == "<strong>one <em>two</em></strong>"
expect AsciiDocInline.render("α < & &copy;", []) == "α &lt; &amp; &copy;"
expect AsciiDocInline.render("{name}", [{ name: "name", value: "*Roc*" }]) == "*Roc*"
expect AsciiDocInline.render("+++<i>x</i>+++ ++<b>++", []) == "<i>x</i> &lt;b&gt;"
expect AsciiDocInline.render("https://example.org[Roc]", []) == "<a href=\"https://example.org\">Roc</a>"
expect AsciiDocInline.render_with("*x* & <", [], { subs: "none" }) == "*x* & <"

# Oracle case 0
expect AsciiDocInline.render("A *bold* word", []) == "A <strong>bold</strong> word"

# Oracle case 1
expect AsciiDocInline.render("A _quiet_ word", []) == "A <em>quiet</em> word"

# Oracle case 2
expect AsciiDocInline.render("Type `code` now", []) == "Type <code>code</code> now"

# Oracle case 3
expect AsciiDocInline.render("pre**bold**post", []) == "pre<strong>bold</strong>post"

# Oracle case 4
expect AsciiDocInline.render("A *_nested_* word", []) == "A <strong><em>nested</em></strong> word"

# Oracle case 5
expect AsciiDocInline.render("x^2^", []) == "x<sup>2</sup>"

# Oracle case 6
expect AsciiDocInline.render("H~2~O", []) == "H<sub>2</sub>O"

# Oracle case 7
expect AsciiDocInline.render("A #marked# word", []) == "A <mark>marked</mark> word"

# Oracle case 8
expect AsciiDocInline.render("A \\*literal* word", []) == "A *literal* word"

# Oracle case 9
expect AsciiDocInline.render("A +<tag>+ word", []) == "A &lt;tag&gt; word"

# Oracle case 10
expect AsciiDocInline.render("pass:[<kbd>K</kbd>]", []) == "<kbd>K</kbd>"

# Oracle case 11
expect AsciiDocInline.render("First +\nSecond", []) == "First<br>\nSecond"

# Oracle case 12
expect AsciiDocInline.render("A & B < C", []) == "A &amp; B &lt; C"

# Oracle case 13
expect AsciiDocInline.render("https://example.org[Example]", []) == "<a href=\"https://example.org\">Example</a>"

# Oracle case 14
expect AsciiDocInline.render("link:guide.html[Guide]", []) == "<a href=\"guide.html\">Guide</a>"

# Oracle case 15
expect AsciiDocInline.render("mailto:a@example.org[Mail]", []) == "<a href=\"mailto:a@example.org\">Mail</a>"

# Oracle case 16
expect AsciiDocInline.render("A image:fox.png[Fox] here", []) == "A <span class=\"image\"><img src=\"fox.png\" alt=\"Fox\"></span> here"

# Oracle case 17
expect AsciiDocInline.render("xref:other.adoc[Other]", []) == "<a href=\"other.html\">Other</a>"

# Oracle case 18
expect AsciiDocInline.render("A footnote:[A note.]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Oracle case 19
expect AsciiDocInline.render("Copyright (C), trademark (TM), registered (R).", []) == "Copyright &#169;, trademark &#8482;, registered &#174;."

# Oracle case 20
expect AsciiDocInline.render("a*b*c a_b_c a`b`c", []) == "a*b*c a_b_c a`b`c"

# Oracle case 21
expect AsciiDocInline.render("**bold _italic_**", []) == "<strong>bold <em>italic</em></strong>"

# Oracle case 22
expect AsciiDocInline.render("x _a *b* c_", []) == "x <em>a <strong>b</strong> c</em>"

# Oracle case 23
expect AsciiDocInline.render("a &copy; &#34; &#x1f600; &bad; &", []) == "a &copy; &#34; &#x1f600; &bad; &amp;"

# Oracle case 24
expect AsciiDocInline.render("`a < b & c`", []) == "<code>a &lt; b &amp; c</code>"

# Oracle case 25
expect AsciiDocInline.render("a +\nb", []) == "a<br>\nb"

# Oracle case 26
expect AsciiDocInline.render("\\*literal*", []) == "*literal*"

# Oracle case 27
expect AsciiDocInline.render("\\{name}", []) == "{name}"

# Oracle case 28
expect AsciiDocInline.render("don't can't isn't", []) == "don&#8217;t can&#8217;t isn&#8217;t"

# Oracle case 29
expect AsciiDocInline.render("one -- two and a--b", []) == "one&#8201;&#8212;&#8201;two and a&#8212;&#8203;b"

# Oracle case 30
expect AsciiDocInline.render("\"`hello`\" and '`world`'", []) == "&#8220;hello&#8221; and &#8216;world&#8217;"

# Oracle case 31
expect AsciiDocInline.render("link:guide.html[Go]", []) == "<a href=\"guide.html\">Go</a>"

# Oracle case 32
expect AsciiDocInline.render("mailto:hi@example.org[Email]", []) == "<a href=\"mailto:hi@example.org\">Email</a>"

# Oracle case 33
expect AsciiDocInline.render("mailto:hi@example.org[]", []) == "<a href=\"mailto:hi@example.org\">hi@example.org</a>"

# Oracle case 34
expect AsciiDocInline.render("https://example.org", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Oracle case 35
expect AsciiDocInline.render("https://example.org[]", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Oracle case 36
expect AsciiDocInline.render("xref:guide.adoc[Guide]", []) == "<a href=\"guide.html\">Guide</a>"

# Oracle case 37
expect AsciiDocInline.render("<<unknown>>", []) == "<a href=\"#unknown\">[unknown]</a>"

# Oracle case 38
expect AsciiDocInline.render("[[here]]text", []) == "<a id=\"here\"></a>text"

# Oracle case 39
expect AsciiDocInline.render("image:pic.png[Alt,20,30]", []) == "<span class=\"image\"><img src=\"pic.png\" alt=\"Alt\" width=\"20\" height=\"30\"></span>"

# Oracle case 40
expect AsciiDocInline.render("pass:quotes[*bold*]", []) == "<strong>bold</strong>"

# Oracle case 41
expect AsciiDocInline.render("+<x>+", []) == "&lt;x&gt;"

# Oracle case 42
expect AsciiDocInline.render("footnote:[One note.]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Oracle case 43
expect AsciiDocInline.render("text footnote:a[One note.] again footnote:a[]", []) == "text <sup class=\"footnote\" id=\"_footnote_a\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> again <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

expect {
	first = AsciiDocInline.render_state("A footnote:first[One *note*.]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("Another footnote:[Two.] and footnote:first[]", [], { subs: "normal" }, first.state)
	first.state.footnotes == [{ name: "first", text: "One <strong>note</strong>." }] and second.state.footnotes.len() == 2 and second.html.contains("_footnotedef_2") and second.html.contains("_footnotedef_1")
}
expect AsciiDocInline.render_state("<<intro>>", [], { subs: "normal" }, { ..AsciiDocInline.initial_state, refs: [{ id: "intro", text: "Introduction" }] }).html == "<a href=\"#intro\">Introduction</a>"

# Expanded oracle case 0
expect AsciiDocInline.render("[.red]#red#", []) == "<span class=\"red\">red</span>"

# Expanded oracle case 1
expect AsciiDocInline.render("[.red]**red**", []) == "<strong class=\"red\">red</strong>"

# Expanded oracle case 2
expect AsciiDocInline.render("[#foo]#text#", []) == "<span id=\"foo\">text</span>"

# Expanded oracle case 3
expect AsciiDocInline.render("[.a.b]#two roles#", []) == "<span class=\"a b\">two roles</span>"

# Expanded oracle case 4
expect AsciiDocInline.render("[role=\"red\"]#text#", []) == "<span class=\"role=\"red\"\">text</span>"

# Expanded oracle case 5
expect AsciiDocInline.render("[.line-through]#strike#", []) == "<span class=\"line-through\">strike</span>"

# Expanded oracle case 6
expect AsciiDocInline.render("[.big]*bold*", []) == "<strong class=\"big\">bold</strong>"

# Expanded oracle case 7
expect AsciiDocInline.render("a^b c^", []) == "a^b c^"

# Expanded oracle case 8
expect AsciiDocInline.render("a~b c~", []) == "a~b c~"

# Expanded oracle case 9
expect AsciiDocInline.render("``double code``", []) == "<code>double code</code>"

# Expanded oracle case 10
expect AsciiDocInline.render("_a_ _b_", []) == "<em>a</em> <em>b</em>"

# Expanded oracle case 11
expect AsciiDocInline.render("**a** **b**", []) == "<strong>a</strong> <strong>b</strong>"

# Expanded oracle case 12
expect AsciiDocInline.render("foo__bar__baz", []) == "foo<em>bar</em>baz"

# Expanded oracle case 13
expect AsciiDocInline.render("foo##bar##baz", []) == "foo<mark>bar</mark>baz"

# Expanded oracle case 14
expect AsciiDocInline.render("\"`double`\" '`single`'", []) == "&#8220;double&#8221; &#8216;single&#8217;"

# Expanded oracle case 15
expect AsciiDocInline.render("\\https://example.org", []) == "https://example.org"

# Expanded oracle case 16
expect AsciiDocInline.render("\\link:a[Text]", []) == "link:a[Text]"

# Expanded oracle case 17
expect AsciiDocInline.render("\\image:a.png[]", []) == "image:a.png[]"

# Expanded oracle case 18
expect AsciiDocInline.render("\\pass:[<b>]", []) == "pass:[&lt;b&gt;]"

# Expanded oracle case 19
expect AsciiDocInline.render("https://example.org.", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>."

# Expanded oracle case 20
expect AsciiDocInline.render("(https://example.org)", []) == "(<a href=\"https://example.org\" class=\"bare\">https://example.org</a>)"

# Expanded oracle case 21
expect AsciiDocInline.render("https://example.org/a(b)", []) == "<a href=\"https://example.org/a(b\" class=\"bare\">https://example.org/a(b</a>)"

# Expanded oracle case 22
expect AsciiDocInline.render("https://example.org?q=a&b=c", []) == "<a href=\"https://example.org?q=a&amp;b=c\" class=\"bare\">https://example.org?q=a&amp;b=c</a>"

# Expanded oracle case 23
expect AsciiDocInline.render("<https://example.org>", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Expanded oracle case 24
expect AsciiDocInline.render("https://example.org[Link^]", []) == "<a href=\"https://example.org\" target=\"_blank\" rel=\"noopener\">Link</a>"

# Expanded oracle case 25
expect AsciiDocInline.render("https://example.org[\"Link, with comma\"]", []) == "<a href=\"https://example.org\">\"Link, with comma\"</a>"

# Expanded oracle case 26
expect AsciiDocInline.render("link:some%20file.html[Some File]", []) == "<a href=\"some%20file.html\">Some File</a>"

# Expanded oracle case 27
expect AsciiDocInline.render("link:some file.html[Some]", []) == "link:some file.html[Some]"

# Expanded oracle case 28
expect AsciiDocInline.render("mailto:a@b.com[Email,subject=\"Hello\",body=\"Body text\"]", []) == "<a href=\"mailto:a@b.com\">Email</a>"

# Expanded oracle case 29
expect AsciiDocInline.render("a@b.com", []) == "<a href=\"mailto:a@b.com\">a@b.com</a>"

# Expanded oracle case 30
expect AsciiDocInline.render("xref:guide.adoc#anchor[Link]", []) == "<a href=\"guide.html#anchor\">Link</a>"

# Expanded oracle case 31
expect AsciiDocInline.render("xref:guide.adoc[]", []) == "<a href=\"guide.html\">guide.html</a>"

# Expanded oracle case 32
expect AsciiDocInline.render("<<id,Some *text*>>", []) == "<a href=\"#id\">Some <strong>text</strong></a>"

# Expanded oracle case 33
expect AsciiDocInline.render("[[id,Ref Text]]text", []) == "<a id=\"id\"></a>text"

# Expanded oracle case 34
expect AsciiDocInline.render("anchor:id[Ref Text]", []) == "<a id=\"id\"></a>"

# Expanded oracle case 35
expect AsciiDocInline.render("image:foo-bar_baz.svg[]", []) == "<span class=\"image\"><img src=\"foo-bar_baz.svg\" alt=\"foo bar baz\"></span>"

# Expanded oracle case 36
expect AsciiDocInline.render("image:cat.jpg[\"A cat, sleeping\",120,80]", []) == "<span class=\"image\"><img src=\"cat.jpg\" alt=\"A cat, sleeping\" width=\"120\" height=\"80\"></span>"

# Expanded oracle case 37
expect AsciiDocInline.render("image:foo.png[alt=Foo,width=100]", []) == "<span class=\"image\"><img src=\"foo.png\" alt=\"Foo\" width=\"100\"></span>"

# Expanded oracle case 38
expect AsciiDocInline.render("image:foo.png[Foo,link=https://example.org]", []) == "<span class=\"image\"><a class=\"image\" href=\"https://example.org\"><img src=\"foo.png\" alt=\"Foo\"></a></span>"

# Expanded oracle case 39
expect AsciiDocInline.render("image:foo.png[Foo,title=Hi]", []) == "<span class=\"image\"><img src=\"foo.png\" alt=\"Foo\" title=\"Hi\"></span>"

# Expanded oracle case 40
expect AsciiDocInline.render("icon:heart[]", []) == "<span class=\"icon\">[heart&#93;</span>"

# Expanded oracle case 41
expect AsciiDocInline.render("footnote:a[Note.] footnote:a[]", []) == "<sup class=\"footnote\" id=\"_footnote_a\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Expanded oracle case 42
expect AsciiDocInline.render("footnote:[A.] footnote:[B.]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_2\" class=\"footnote\" href=\"#_footnotedef_2\" title=\"View footnote.\">2</a>]</sup>"

# Expanded oracle case 43
expect AsciiDocInline.render("pass:attributes[<b>{foo}</b>]", []) == "<b>{foo}</b>"

# Expanded oracle case 44
expect AsciiDocInline.render("pass:q[*bold*]", []) == "<strong>bold</strong>"

# Expanded oracle case 45
expect AsciiDocInline.render("pass:c[<b>]", []) == "&lt;b&gt;"

# Expanded oracle case 46
expect AsciiDocInline.render("++a & b++", []) == "a &amp; b"

# Expanded oracle case 47
expect AsciiDocInline.render("+++a & b+++", []) == "a & b"

# Expanded oracle case 48
expect AsciiDocInline.render("+a *b*+", []) == "a *b*"

# Expanded oracle case 49
expect AsciiDocInline.render("a +\nb +\nc", []) == "a<br>\nb<br>\nc"

# Expanded oracle case 50
expect AsciiDocInline.render("a +", []) == "a<br>"

# Expanded oracle case 51
expect AsciiDocInline.render("(c) (r) (tm)", []) == "(c) (r) (tm)"

# Expanded oracle case 52
expect AsciiDocInline.render("(C) (R) (TM)", []) == "&#169; &#174; &#8482;"

# Expanded oracle case 53
expect AsciiDocInline.render("a -> b <- c => d <= e", []) == "a &#8594; b &#8592; c &#8658; d &#8656; e"

# Expanded oracle case 55
expect AsciiDocInline.render("a -- b", []) == "a&#8201;&#8212;&#8201;b"

# Expanded oracle case 56
expect AsciiDocInline.render("--begin", []) == "--begin"

# Expanded oracle case 57
expect AsciiDocInline.render("begin--", []) == "begin--"

# Expanded oracle case 58
expect AsciiDocInline.render("a--b", []) == "a&#8212;&#8203;b"

# Expanded oracle case 59
expect AsciiDocInline.render("a --\nb", []) == "a&#8201;&#8212;&#8201;b"

# Expanded oracle case 60
expect AsciiDocInline.render("`+x+`", []) == "<code>x</code>"

# Expanded oracle case 61
expect AsciiDocInline.render("`a *b*`", []) == "<code>a <strong>b</strong></code>"

# Expanded oracle case 62
expect AsciiDocInline.render("`a _b_`", []) == "<code>a <em>b</em></code>"

# Expanded oracle case 63
expect AsciiDocInline.render("*a\nb*", []) == "<strong>a\nb</strong>"

# Expanded oracle case 64
expect AsciiDocInline.render("foo\\_bar_", []) == "foo_bar_"

# Expanded oracle case 65
expect AsciiDocInline.render("*a*word", []) == "*a*word"

# Expanded oracle case 66
expect AsciiDocInline.render("*a*!", []) == "<strong>a</strong>!"

# Expanded oracle case 67
expect AsciiDocInline.render("a*one*", []) == "a*one*"

# Expanded oracle case 68
expect AsciiDocInline.render("[.red]_red_", []) == "<em class=\"red\">red</em>"

# Expanded oracle case 69
expect AsciiDocInline.render("kbd:[Ctrl+C]", []) == "kbd:[Ctrl+C]"

# Expanded oracle case 70
expect AsciiDocInline.render("btn:[OK]", []) == "btn:[OK]"

# Expanded oracle case 71
expect AsciiDocInline.render("menu:File[Open]", []) == "menu:File[Open]"

# Expanded oracle case 72
expect AsciiDocInline.render("{nbsp} {lt} {amp} {plus} {vbar} {cpp}", []) == "&#160; < & &#43; | C&#43;&#43;"

macro_parts : Str -> List(Str)
macro_parts = |incoming_source| {
	source = repl(repl(incoming_source, "\r\n", " "), "\n", " ")
	bytes = source.to_utf8()
	var $parts = []
	var $quote = 0.U8
	var $start = 0
	var $pos = 0
	var $closed = False
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		previous = if $pos == 0 0 else bytes.get($pos - 1) ?? 0
		if $closed and (b == ',' or space(b)) {
			$start = $pos + 1
			if b == ',' {
				$closed = False
			}
		} else if $quote != 0 and b == $quote and previous != '\\' {
			$quote = 0
			$parts = $parts.append(slice(source, $start, $pos - $start + 1))
			$start = $pos + 1
			$closed = True
		} else {
			$closed = False
			if $quote == 0 and (b == '\'' or b == '"') and ($pos == $start or previous == '=') {
				$quote = b
			}
			if b == ',' and $quote == 0 {
				$parts = $parts.append(slice(source, $start, $pos - $start))
				$start = $pos + 1
			}
		}
		$pos = $pos + 1
	}
	if $start < bytes.len() or !$closed {
		$parts = $parts.append(drop(source, $start))
	}
	$parts
}

named : List(Str), Str -> Try(Str, [Missing])
named = |parts, name| {
	for part in parts {
		p = part.trim()
		if p.starts_with("${name}=") {
			return Ok(unquote_attribute(drop(p, name.to_utf8().len() + 1).trim()))
		}
	}
	Err(Missing)
}

email_link : Str -> Macro
email_link = |source| {
	var $end = 0
	for b in source.to_utf8() {
		if !(word(b) or b == '@' or b == '.' or b == '-' or b == '+') {
			break
		}
		$end = $end + 1
	}
	while $end > 0 and (source.to_utf8().get($end - 1) ?? 0) == '.' {
		$end = $end - 1
	}
	email = take(source, $end)
	parts = email.split_on("@")
	if parts.len() == 2 and (parts.get(1) ?? "").contains(".") and (parts.first() ?? "") != "" {
		{ used: $end, html: "<a href=\"mailto:${escape(email)}\">${escape(email)}</a>" }
	} else {
		{ used: 0, html: "" }
	}
}

expect AsciiDocInline.render("link:outside.adoc[role=include]", []) == "<a href=\"outside.adoc\" class=\"bare include\">outside.adoc</a>"
expect AsciiDocInline.render("{x}", [{ name: "x", value: "*Roc* &lt;b&gt; &amp; &amp;copy; &amp;madeup;" }]) == "*Roc* &lt;b&gt; &amp; &copy; &madeup;"

# Inline interaction oracle case 0
expect AsciiDocInline.render("Test *a* end.", []) == "Test <strong>a</strong> end."

# Inline interaction oracle case 1
expect AsciiDocInline.render("Test *a b* end.", []) == "Test <strong>a b</strong> end."

# Inline interaction oracle case 2
expect AsciiDocInline.render("Test *a\nb* end.", []) == "Test <strong>a\nb</strong> end."

# Inline interaction oracle case 3
expect AsciiDocInline.render("Test *a *bold* z* end.", []) == "Test <strong>a *bold</strong> z* end."

# Inline interaction oracle case 4
expect AsciiDocInline.render("Test *<x> & y* end.", []) == "Test <strong>&lt;x&gt; &amp; y</strong> end."

# Inline interaction oracle case 5
expect AsciiDocInline.render("Test **a** end.", []) == "Test <strong>a</strong> end."

# Inline interaction oracle case 6
expect AsciiDocInline.render("Test **a b** end.", []) == "Test <strong>a b</strong> end."

# Inline interaction oracle case 7
expect AsciiDocInline.render("Test **a\nb** end.", []) == "Test <strong>a\nb</strong> end."

# Inline interaction oracle case 8
expect AsciiDocInline.render("Test **a *bold* z** end.", []) == "Test <strong>a <strong>bold</strong> z</strong> end."

# Inline interaction oracle case 9
expect AsciiDocInline.render("Test **<x> & y** end.", []) == "Test <strong>&lt;x&gt; &amp; y</strong> end."

# Inline interaction oracle case 10
expect AsciiDocInline.render("Test _a_ end.", []) == "Test <em>a</em> end."

# Inline interaction oracle case 11
expect AsciiDocInline.render("Test _a b_ end.", []) == "Test <em>a b</em> end."

# Inline interaction oracle case 12
expect AsciiDocInline.render("Test _a\nb_ end.", []) == "Test <em>a\nb</em> end."

# Inline interaction oracle case 13
expect AsciiDocInline.render("Test _a *bold* z_ end.", []) == "Test <em>a <strong>bold</strong> z</em> end."

# Inline interaction oracle case 14
expect AsciiDocInline.render("Test _<x> & y_ end.", []) == "Test <em>&lt;x&gt; &amp; y</em> end."

# Inline interaction oracle case 15
expect AsciiDocInline.render("Test __a__ end.", []) == "Test <em>a</em> end."

# Inline interaction oracle case 16
expect AsciiDocInline.render("Test __a b__ end.", []) == "Test <em>a b</em> end."

# Inline interaction oracle case 17
expect AsciiDocInline.render("Test __a\nb__ end.", []) == "Test <em>a\nb</em> end."

# Inline interaction oracle case 18
expect AsciiDocInline.render("Test __a *bold* z__ end.", []) == "Test <em>a <strong>bold</strong> z</em> end."

# Inline interaction oracle case 19
expect AsciiDocInline.render("Test __<x> & y__ end.", []) == "Test <em>&lt;x&gt; &amp; y</em> end."

# Inline interaction oracle case 20
expect AsciiDocInline.render("Test `a` end.", []) == "Test <code>a</code> end."

# Inline interaction oracle case 21
expect AsciiDocInline.render("Test `a b` end.", []) == "Test <code>a b</code> end."

# Inline interaction oracle case 22
expect AsciiDocInline.render("Test `a\nb` end.", []) == "Test <code>a\nb</code> end."

# Inline interaction oracle case 23
expect AsciiDocInline.render("Test `a *bold* z` end.", []) == "Test <code>a <strong>bold</strong> z</code> end."

# Inline interaction oracle case 24
expect AsciiDocInline.render("Test `<x> & y` end.", []) == "Test <code>&lt;x&gt; &amp; y</code> end."

# Inline interaction oracle case 25
expect AsciiDocInline.render("Test ``a`` end.", []) == "Test <code>a</code> end."

# Inline interaction oracle case 26
expect AsciiDocInline.render("Test ``a b`` end.", []) == "Test <code>a b</code> end."

# Inline interaction oracle case 27
expect AsciiDocInline.render("Test ``a\nb`` end.", []) == "Test <code>a\nb</code> end."

# Inline interaction oracle case 28
expect AsciiDocInline.render("Test ``a *bold* z`` end.", []) == "Test <code>a <strong>bold</strong> z</code> end."

# Inline interaction oracle case 29
expect AsciiDocInline.render("Test ``<x> & y`` end.", []) == "Test <code>&lt;x&gt; &amp; y</code> end."

# Inline interaction oracle case 30
expect AsciiDocInline.render("Test #a# end.", []) == "Test <mark>a</mark> end."

# Inline interaction oracle case 31
expect AsciiDocInline.render("Test #a b# end.", []) == "Test <mark>a b</mark> end."

# Inline interaction oracle case 32
expect AsciiDocInline.render("Test #a\nb# end.", []) == "Test <mark>a\nb</mark> end."

# Inline interaction oracle case 33
expect AsciiDocInline.render("Test #a *bold* z# end.", []) == "Test <mark>a <strong>bold</strong> z</mark> end."

# Inline interaction oracle case 34
expect AsciiDocInline.render("Test #<x> & y# end.", []) == "Test <mark>&lt;x&gt; &amp; y</mark> end."

# Inline interaction oracle case 35
expect AsciiDocInline.render("Test ##a## end.", []) == "Test <mark>a</mark> end."

# Inline interaction oracle case 36
expect AsciiDocInline.render("Test ##a b## end.", []) == "Test <mark>a b</mark> end."

# Inline interaction oracle case 37
expect AsciiDocInline.render("Test ##a\nb## end.", []) == "Test <mark>a\nb</mark> end."

# Inline interaction oracle case 38
expect AsciiDocInline.render("Test ##a *bold* z## end.", []) == "Test <mark>a <strong>bold</strong> z</mark> end."

# Inline interaction oracle case 39
expect AsciiDocInline.render("Test ##<x> & y## end.", []) == "Test <mark>&lt;x&gt; &amp; y</mark> end."

# Inline interaction oracle case 40
expect AsciiDocInline.render("Test ^a^ end.", []) == "Test <sup>a</sup> end."

# Inline interaction oracle case 41
expect AsciiDocInline.render("Test ^a b^ end.", []) == "Test ^a b^ end."

# Inline interaction oracle case 42
expect AsciiDocInline.render("Test ^a\nb^ end.", []) == "Test ^a\nb^ end."

# Inline interaction oracle case 43
expect AsciiDocInline.render("Test ^a *bold* z^ end.", []) == "Test ^a <strong>bold</strong> z^ end."

# Inline interaction oracle case 44
expect AsciiDocInline.render("Test ^<x> & y^ end.", []) == "Test ^&lt;x&gt; &amp; y^ end."

# Inline interaction oracle case 45
expect AsciiDocInline.render("Test ~a~ end.", []) == "Test <sub>a</sub> end."

# Inline interaction oracle case 46
expect AsciiDocInline.render("Test ~a b~ end.", []) == "Test ~a b~ end."

# Inline interaction oracle case 47
expect AsciiDocInline.render("Test ~a\nb~ end.", []) == "Test ~a\nb~ end."

# Inline interaction oracle case 48
expect AsciiDocInline.render("Test ~a *bold* z~ end.", []) == "Test ~a <strong>bold</strong> z~ end."

# Inline interaction oracle case 49
expect AsciiDocInline.render("Test ~<x> & y~ end.", []) == "Test ~&lt;x&gt; &amp; y~ end."

# Inline interaction oracle case 50
expect AsciiDocInline.render("Test +a+ end.", []) == "Test a end."

# Inline interaction oracle case 51
expect AsciiDocInline.render("Test +a b+ end.", []) == "Test a b end."

# Inline interaction oracle case 52
expect AsciiDocInline.render("Test +a\nb+ end.", []) == "Test a\nb end."

# Inline interaction oracle case 53
expect AsciiDocInline.render("Test +a *bold* z+ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 54
expect AsciiDocInline.render("Test +<x> & y+ end.", []) == "Test &lt;x&gt; &amp; y end."

# Inline interaction oracle case 55
expect AsciiDocInline.render("Test ++a++ end.", []) == "Test a end."

# Inline interaction oracle case 56
expect AsciiDocInline.render("Test ++a b++ end.", []) == "Test a b end."

# Inline interaction oracle case 57
expect AsciiDocInline.render("Test ++a\nb++ end.", []) == "Test a\nb end."

# Inline interaction oracle case 58
expect AsciiDocInline.render("Test ++a *bold* z++ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 59
expect AsciiDocInline.render("Test ++<x> & y++ end.", []) == "Test &lt;x&gt; &amp; y end."

# Inline interaction oracle case 60
expect AsciiDocInline.render("Test +++a+++ end.", []) == "Test a end."

# Inline interaction oracle case 61
expect AsciiDocInline.render("Test +++a b+++ end.", []) == "Test a b end."

# Inline interaction oracle case 62
expect AsciiDocInline.render("Test +++a\nb+++ end.", []) == "Test a\nb end."

# Inline interaction oracle case 63
expect AsciiDocInline.render("Test +++a *bold* z+++ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 64
expect AsciiDocInline.render("Test +++<x> & y+++ end.", []) == "Test <x> & y end."

# Inline interaction oracle case 65
expect AsciiDocInline.render("A footnote:[A *bold* note and https://example.org[link].]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Inline interaction oracle case 66
expect AsciiDocInline.render("footnote:[one] footnote:[two] footnote:[three]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_2\" class=\"footnote\" href=\"#_footnotedef_2\" title=\"View footnote.\">2</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_3\" class=\"footnote\" href=\"#_footnotedef_3\" title=\"View footnote.\">3</a>]</sup>"

# Inline interaction oracle case 67
expect AsciiDocInline.render("footnote:n[One.] footnote:n[Two.] footnote:n[]", []) == "<sup class=\"footnote\" id=\"_footnote_n\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Inline interaction oracle case 68
expect AsciiDocInline.render("footnote:missing[]", []) == "<sup class=\"footnoteref red\" title=\"Unresolved footnote reference.\">[missing]</sup>"

# Inline interaction oracle case 69
expect AsciiDocInline.render("pass:[footnote:[raw]]", []) == "footnote:[raw]"

# Inline interaction oracle case 70
expect AsciiDocInline.render("+++footnote:[raw]+++", []) == "footnote:[raw]"

# Inline interaction oracle case 71
expect AsciiDocInline.render("+++*a*+++ and *b*", []) == "*a* and <strong>b</strong>"

# Inline interaction oracle case 72
expect AsciiDocInline.render("[.small]`code`", []) == "<code class=\"small\">code</code>"

# Inline interaction oracle case 73
expect AsciiDocInline.render("[foo]#bar#", []) == "<span class=\"foo\">bar</span>"

# Inline interaction oracle case 74
expect AsciiDocInline.render("[red]##bar##", []) == "<span class=\"red\">bar</span>"

# Inline interaction oracle case 75
expect AsciiDocInline.render("[%option]#bar#", []) == "<span class=\"%option\">bar</span>"

# Inline interaction oracle case 76
expect AsciiDocInline.render("[#id.red.big]#bar#", []) == "<span id=\"id\" class=\"red big\">bar</span>"

# Inline interaction oracle case 77
expect AsciiDocInline.render("A [[[ref]]] reference.", []) == "A [<a id=\"ref\"></a>] reference."

# Inline interaction oracle case 78
expect AsciiDocInline.render("indexterm:[sunset]", []) == ""

# Inline interaction oracle case 79
expect AsciiDocInline.render("indexterm2:[sunset]", []) == "sunset"

# Inline interaction oracle case 80
expect AsciiDocInline.render("((sunset))", []) == "sunset"

# Inline interaction oracle case 81
expect AsciiDocInline.render("(((sunset)))", []) == ""

# Inline interaction oracle case 82
expect AsciiDocInline.render("http://example.org[path,role=external]", []) == "<a href=\"http://example.org\" class=\"external\">path</a>"

# Inline interaction oracle case 83
expect AsciiDocInline.render("https://example.org[Link,window=_blank]", []) == "<a href=\"https://example.org\" target=\"_blank\" rel=\"noopener\">Link</a>"

# Inline interaction oracle case 84
expect AsciiDocInline.render("https://example.org[Link,%nofollow]", []) == "<a href=\"https://example.org\">Link,%nofollow</a>"

# Inline interaction oracle case 85
expect AsciiDocInline.render("link:path[Link,role=red]", []) == "<a href=\"path\" class=\"red\">Link</a>"

# Inline interaction oracle case 86
expect AsciiDocInline.render("xref:guide.adoc#part[]", []) == "<a href=\"guide.html#part\">guide.html</a>"

# Inline interaction oracle case 87
expect AsciiDocInline.render("xref:#part[]", []) == "<a href=\"#part\">[part]</a>"

# Inline interaction oracle case 88
expect AsciiDocInline.render("&apos; &quot; &#123;", []) == "&apos; &quot; &#123;"

# Inline interaction oracle case 89
expect AsciiDocInline.render("{empty}{sp}{zwsp}{wj}{deg}", []) == " &#8203;&#8288;&#176;"

# Inline interaction oracle case 90
expect AsciiDocInline.render("http://example.org/path?q=a&amp;b=c", []) == "<a href=\"http://example.org/path?q=a&amp;b=c\" class=\"bare\">http://example.org/path?q=a&amp;b=c</a>"

expect AsciiDocInline.render("kbd:[Ctrl+C]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>C</kbd></span>"

expect AsciiDocInline.render("kbd:[Ctrl,Alt,Del]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd></span>"

expect AsciiDocInline.render("kbd:[Ctrl++]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>+</kbd></span>"

expect AsciiDocInline.render("kbd:[Enter]", [{ name: "experimental", value: "" }]) == "<kbd>Enter</kbd>"

expect AsciiDocInline.render("btn:[Save]", [{ name: "experimental", value: "" }]) == "<b class=\"button\">Save</b>"

expect AsciiDocInline.render("menu:File[New > Project]", [{ name: "experimental", value: "" }]) == "<span class=\"menuseq\"><b class=\"menu\">File</b>&#160;<b class=\"caret\">&#8250;</b> <b class=\"submenu\">New</b>&#160;<b class=\"caret\">&#8250;</b> <b class=\"menuitem\">Project</b></span>"

expect AsciiDocInline.render("menu:View[]", [{ name: "experimental", value: "" }]) == "<b class=\"menuref\">View</b>"

# Existing character references survive the specialcharacter substitution.
escape_entities : Str -> Str
escape_entities = |source| {
	bytes = source.to_utf8()
	var $out = []
	var $pos = 0
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '&' {
			var $end = $pos + 1
			while $end < bytes.len() and $end - $pos < 32 and (word(bytes.get($end) ?? 0) or (bytes.get($end) ?? 0) == '#') {
				$end = $end + 1
			}
			if $end > $pos + 1 and (bytes.get($end) ?? 0) == ';' {
				$out = $out.concat(bytes.sublist({ start: $pos, len: $end - $pos + 1 }))
				$pos = $end
			} else {
				$out = $out.concat("&amp;".to_utf8())
			}
		} else if b == '<' {
			$out = $out.concat("&lt;".to_utf8())
		}
			else if b == '>' {
				$out = $out.concat("&gt;".to_utf8())
			}
				else {
					$out = $out.append(b)
				}
		$pos = $pos + 1
	}
	Str.from_utf8_lossy($out)
}

bracket_end : Str, U64 -> Try(U64, [Missing])
bracket_end = |source, open| {
	bytes = source.to_utf8()
	var $depth = 1
	var $pos = open + 1
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '[' {
			$depth = $depth + 1
		}
		if b == ']' {
			$depth = $depth - 1
		}
		if $depth == 0 {
			return Ok($pos)
		}
		$pos = $pos + 1
	}
	Err(Missing)
}

expect AsciiDocInline.render_with("{who}", [{ name: "who", value: "*Roc*" }], { subs: "attributes,quotes" }) == "<strong>Roc</strong>"
expect AsciiDocInline.render_with("{who}", [{ name: "who", value: "*Roc*" }], { subs: "quotes,attributes" }) == "*Roc*"

QuoteStage : { open : Str, close : Str, tag : Str, unconstrained : Bool, no_space : Bool }

QuoteMatch : { used : U64, body : Str, role : Str, escaped : Bool, escaped_role : Bool }

# Generated tags use a per-input delimiter that cannot collide with source or
# attributes. Keeping tags separate lets later quote passes see the surrounding
# text without reinterpreting generated HTML attribute values.
token : Str, U64 -> Str
token = |prefix, index| "${prefix}${index.to_str()}${prefix}"

pass_needed : Str, Groups -> Bool
pass_needed = |input, g| g.macros and (input.contains("+") or input.contains("pass:") or input.contains("stem:") or input.contains("latexmath:") or input.contains("asciimath:") or input.contains("$$"))

quote_needed : Str, Groups -> Bool
quote_needed = |input, g| {
	quoted = g.quotes and (input.contains("*") or input.contains("_") or input.contains("`") or input.contains("#") or input.contains("^") or input.contains("~"))
	quoted or pass_needed(input, g)
}

# Formatting reads attribute values only to reserve a private delimiter.
# Carry that delimiter across dynamic writes rather than materialize every
# stored counter. Passthrough substitution still receives the full context.
quote_state : Str, List({ name : Str, value : Str }), Groups, AsciiDocInline.State -> QuoteResult
quote_state = |input, attrs, g, state| {
	if pass_needed(input, g) {
		quote_all(input, effective_attributes(attrs, state), g, 0)
	} else {
		quote_prefixed(input, attrs, g, state.tag_prefix)
	}
}

macro_candidate : Str -> Bool
macro_candidate = |source| source.contains(":") or source.contains("@") or source.contains("+") or source.contains("[") or source.contains("$") or source.contains("((") or source.contains("<<") or source.contains("&lt;&lt;")

quote_all : Str, List({ name : Str, value : Str }), Groups, U64 -> QuoteResult
quote_all = |input, attrs, g, _depth| quote_prefixed(input, attrs, g, "\u(1)")

quote_prefixed : Str, List({ name : Str, value : Str }), Groups, Str -> QuoteResult
quote_prefixed = |input, attrs, g, prefix| {
	# Most attribute references and URLs need no private quote slots.
	if !quote_needed(input, g) {
		return { source: input, prefix: "", tags: [], deferred: [] }
	}
	var $prefix = prefix
	while input.contains($prefix) or attrs.any(|attr| attr.value.contains($prefix)) {
		$prefix = $prefix.concat("\u(1)")
	}
	var $tags = []
	var $deferred = []
	var $text = input
	# Unconstrained passthroughs are extracted before single-plus spans. A
	# single-plus span may enclose an already extracted passthrough slot.
	for single in [False, True] {
		needed = g.macros and ($text.contains("+") or (!single and ($text.contains("pass:") or $text.contains("stem:") or $text.contains("latexmath:") or $text.contains("asciimath:") or $text.contains("$$"))))
		if needed {
			pass_source = $text
			$text = ""
			var $pos = 0
			bytes = pass_source.to_utf8()
			while $pos < bytes.len() {
				rest = drop(pass_source, $pos)
				pass = if g.macros pass_match(rest, if $pos == 0 0 else bytes.get($pos - 1) ?? 0, attrs, single, word_before(bytes, $pos)) else { used: 0, body: "", role: "", escaped: False, escaped_role: False }
				if pass.used > 0 {
					if pass.escaped {
						$text = $text.concat(pass.body)
					} else {
						body = pass.body
						html = if pass.role == "" or pass.escaped_role body else "<span${quote_attributes(pass.role)}>${body}</span>"
						$text = $text.concat(if pass.escaped_role "[${pass.role}]${token($prefix, $tags.len())}" else token($prefix, $tags.len()))
						match deferred_pass(rest) {
							Ok(deferred) if !single => {
								$deferred = $deferred.append({ slot: $tags.len(), source: deferred.source, subs: deferred.subs })
							}
							_ => {}
						}
						$tags = $tags.append(html)
					}
					$pos = $pos + pass.used
				} else {
					c = bytes.get($pos) ?? 0
					n = if c < 128 1 else if c < 224 2 else if c < 240 3 else 4
					var $end = $pos + n
					while $end < bytes.len() and !possible_pass(bytes, $end, single) {
						$end = $end + 1
					}
					$text = $text.concat(slice(pass_source, $pos, $end - $pos))
					$pos = $end
				}
			}
		}
	}
	stages = [
		{ open: "**", close: "**", tag: "strong", unconstrained: True, no_space: False },
		{ open: "*", close: "*", tag: "strong", unconstrained: False, no_space: False },
		{ open: "\"`", close: "`\"", tag: "double", unconstrained: False, no_space: False },
		{ open: "'`", close: "`'", tag: "single", unconstrained: False, no_space: False },
		{ open: "``", close: "``", tag: "code", unconstrained: True, no_space: False },
		{ open: "`", close: "`", tag: "code", unconstrained: False, no_space: False },
		{ open: "__", close: "__", tag: "em", unconstrained: True, no_space: False },
		{ open: "_", close: "_", tag: "em", unconstrained: False, no_space: False },
		{ open: "##", close: "##", tag: "mark", unconstrained: True, no_space: False },
		{ open: "#", close: "#", tag: "mark", unconstrained: False, no_space: False },
		{ open: "^", close: "^", tag: "sup", unconstrained: True, no_space: True },
		{ open: "~", close: "~", tag: "sub", unconstrained: True, no_space: True },
	]
	if g.quotes {
		for stage in stages {
			if $text.contains(stage.open) {
				source = $text
				bs = source.to_utf8()
				marker_open = stage.open.to_utf8()
				marker_close = stage.close.to_utf8()
				first_marker = marker_open.first() ?? 0
				closers = quote_closers(bs, stage, marker_close)
				var $next = ""
				var $i = 0
				var $boundary = bs.len() + 1
				while $i < bs.len() {
					matched = if !stage.unconstrained and $i == $boundary { used: 0, body: "", role: "", escaped: False, escaped_role: False } else quote_match(source, bs, $i, stage, marker_open, marker_close, closers, g.special)
					if matched.used > 0 {
						if matched.escaped {
							$next = $next.concat(matched.body)
						} else {
							tag = if stage.tag == "mark" and matched.role != "" "span" else stage.tag
							opening = if tag == "double" "&#8220;" else if tag == "single" "&#8216;" else "<${tag}${quote_attributes(if matched.escaped_role "" else matched.role)}>"
							closing = if tag == "double" "&#8221;" else if tag == "single" "&#8217;" else "</${tag}>"
							before = token($prefix, $tags.len())
							after = token($prefix, $tags.len() + 1)
							$tags = $tags.append(opening).append(closing)
							$next = $next.concat("${if matched.escaped_role "[${matched.role}]" else ""}${before}${matched.body}${after}")
						}
						$i = $i + matched.used
						$boundary = $i
					} else {
						c = bs.get($i) ?? 0
						n = if c < 128 1 else if c < 224 2 else if c < 240 3 else 4
						var $end = $i + n
						while $end < bs.len() and (bs.get($end) ?? 0) != first_marker and (bs.get($end) ?? 0) != '[' and (bs.get($end) ?? 0) != '\\' {
							$end = $end + 1
						}
						$next = $next.concat(slice(source, $i, $end - $i))
						$i = $end
					}
				}
				$text = $next
			}
		}
	}
	{ source: $text, prefix: $prefix, tags: $tags, deferred: $deferred }
}

quote_match : Str, List(U8), U64, QuoteStage, List(U8), List(U8), List(U64), Bool -> QuoteMatch
quote_match = |source, bs, position, stage, opening, closing, closers, special| {
	none = { used: 0, body: "", role: "", escaped: False, escaped_role: False }
	first = bs.get(position) ?? 0
	if first != (opening.first() ?? 0) and first != '[' and first != '\\' {
		return none
	}
	escaped = first == '\\'
	var $offset = if escaped 1 else 0
	var $role = ""
	if (bs.get(position + $offset) ?? 0) == '[' {
		match find_bytes(bs, [93], position + $offset + 1) {
			Ok(end) => {
				$role = slice(source, position + $offset + 1, end - position - $offset - 1)
				$offset = end + 1 - position
			}
			Err(_) => return none
		}
	}
	if !bs.drop_first(position + $offset).starts_with(opening) {
		return none
	}
	previous = if position == 0 or escaped 0 else bs.get(position - 1) ?? 0
	if !stage.unconstrained {
		if (!escaped and word_before(bs, position)) or (special and (previous == '<' or previous == '>' or previous == '&')) or previous == ';' or previous == ':' or previous == '}' or (stage.tag == "mark" and previous == '&') or (stage.tag == "code" and (previous == '\'' or previous == '"' or previous == '`')) {
			return none
		}
	}
	start = position + $offset + opening.len()
	if start >= bs.len() or ((!stage.unconstrained or stage.no_space) and space(bs.get(start) ?? 0)) {
		return none
	}
	# All invalid closing boundaries were filtered once for the whole pass.
	# Binary search avoids rescanning a suffix for every unmatched opener.
	var $low = 0
	var $high = closers.len()
	while $low < $high {
		middle = $low + (($high - $low) // 2)
		if (closers.get(middle) ?? 0) <= start {
			$low = middle + 1
		} else {
			$high = middle
		}
	}
	match closers.get($low) {
		Err(_) => none
		Ok(end) => {
			if stage.no_space and bs.sublist({ start, len: end - start }).any(space) {
				return none
			}
			used = end + closing.len() - position
			escaped_role = escaped and $role != "" and !stage.unconstrained
			content = slice(source, start, end - start)
			{ used, body: if escaped and !escaped_role slice(source, position + 1, used - 1) else content, role: $role, escaped: escaped and !escaped_role, escaped_role }
		}
	}
}

quote_attributes : Str -> Str
quote_attributes = |role| {
	if role == "" {
		return ""
	}
	id = if role.starts_with("#") drop(role, 1).split_on(".").first() ?? "" else ""
	classes = if role.starts_with("#") Str.join_with(role.split_on(".").drop_first(1), " ") else if role.starts_with(".") Str.join_with(drop(role, 1).split_on("."), " ") else role
	"${if id == "" "" else " id=\"${id}\""}${if classes == "" "" else " class=\"${classes}\""}"
}

pass_match : Str, U8, List({ name : Str, value : Str }), Bool, Bool -> QuoteMatch
pass_match = |source, prev, attrs, single, prev_word| {
	none = { used: 0, body: "", role: "", escaped: False, escaped_role: False }
	escaped = source.starts_with("\\")
	var $offset = if escaped 1 else 0
	var $role = ""
	if drop(source, $offset).starts_with("[") {
		match find(source, "]", $offset + 1) {
			Ok(end) => {
				$role = slice(source, $offset + 1, end - $offset - 1)
				$offset = end + 1
			}
			Err(_) => return none
		}
	}

	rest = drop(source, $offset)
	var $math_prefix = ""
	for prefix in ["stem:", "latexmath:", "asciimath:"] {
		if rest.starts_with(prefix) {
			$math_prefix = prefix
			break
		}
	}
	if !single and $math_prefix != "" {
		match find(rest, "[", $math_prefix.to_utf8().len()) {
			Ok(open) => match first_bracket_end(rest, open) {
				Ok(end) => {
					used = $offset + end + 1
					if escaped {
						return { used, body: drop(take(source, used), 1), role: "", escaped: True, escaped_role: False }
					}
					subs = slice(rest, $math_prefix.to_utf8().len(), open - $math_prefix.to_utf8().len())
					raw = repl(slice(rest, open + 1, end - open - 1), "\\]", "]")
					dialect = if $math_prefix == "stem:" lookup(attrs, "stem") ?? "" else $math_prefix
					latex = dialect == "latexmath:" or dialect == "latexmath" or dialect == "latex" or dialect == "tex"
					content = if latex and raw.starts_with("$") and raw.ends_with("$") slice(raw, 1, raw.to_utf8().len() - 2) else raw
					html = AsciiDocInline.render_with(content, attrs, { subs: if subs == "" "specialcharacters" else subs })
					body = if latex "\\(${html}\\)" else "\\$${html}\\$"
					return { used, body, role: "", escaped: False, escaped_role: False }
				}
				Err(_) => return none
			}
			Err(_) => return none
		}
	}
	if !single and rest.starts_with("pass:") {
		match find(rest, "[", 5) {
			Ok(open) => match first_bracket_end(rest, open) {
				Ok(end) => {
					used = $offset + end + 1
					if escaped {
						return { ..none, used, body: take(source, used), escaped: True }
					}
					subs = slice(rest, 5, open - 5)
					content = repl(slice(rest, open + 1, end - open - 1), "\\]", "]")
					return { used, body: AsciiDocInline.render_with(content, attrs, { subs: subs }), role: "", escaped: False, escaped_role: False }
				}
				Err(_) => return none
			}
			Err(_) => return none
		}
	}
	choices = if single {
		if rest.starts_with("+") [1] else []
	} else if rest.starts_with("+++") [3, 2] else if rest.starts_with("++") or rest.starts_with("$$") [2] else []
	for n in choices {
		if !(n == 1 and (prev_word or prev == ';' or prev == ':' or prev == '\\')) {
			marker = take(rest, n)
			var $search = n
			var $done = False
			while $search <= rest.to_utf8().len() and !$done {
				match find(rest, marker, $search) {
					Ok(end) => {
						content = slice(rest, n, end - n)
						if n > 1 or (end > n and !space(content.to_utf8().first() ?? 0) and !space(content.to_utf8().last() ?? 0) and !word_at(rest.to_utf8(), end + n)) {
							used = $offset + end + n
							body = if escaped and $role == "" drop(take(source, used), 1) else if n == 3 content else escape(content)
							return { used, body, role: $role, escaped: escaped and $role == "", escaped_role: escaped and $role != "" }
						}
						$search = end + n
					}
					Err(_) => {
						$done = True
					}
				}
			}
		}
	}
	none
}

restore_tags : Str, Str, List(Str) -> Str
restore_tags = |source, prefix, tags| restore_tags_depth(source, prefix, tags, 0)

restore_tags_depth : Str, Str, List(Str), U64 -> Str
restore_tags_depth = |source, prefix, tags, depth| {
	if depth > 64 {
		return source
	}
	if prefix == "" or tags.is_empty() {
		return source
	}
	var $out = ""
	var $pos = 0
	bytes = source.to_utf8()
	prefix_len = prefix.to_utf8().len()
	while $pos < bytes.len() {
		match find(source, prefix, $pos) {
			Ok(start) => {
				$out = $out.concat(slice(source, $pos, start - $pos))
				match find(source, prefix, start + prefix_len) {
					Ok(end) => {
						index = U64.from_str(slice(source, start + prefix_len, end - start - prefix_len))
						match index {
							Ok(value) => {
								$out = $out.concat(
									match tags.get(value) {
										Ok(tag) => restore_tags_depth(tag, prefix, tags, depth + 1)
										Err(_) => slice(source, start, end + prefix_len - start)
									},
								)
							}
							Err(_) => {
								$out = $out.concat(slice(source, start, end + prefix_len - start))
							}
						}
						$pos = end + prefix_len
					}
					Err(_) => {
						$out = $out.concat(drop(source, start))
						$pos = bytes.len()
					}
				}
			}
			Err(_) => {
				$out = $out.concat(drop(source, $pos))
				$pos = bytes.len()
			}
		}
	}
	$out
}

expect AsciiDocInline.render("\\^alpha beta^", []) == "\\^alpha beta^"

expect AsciiDocInline.render("\\*alpha beta*_.", []) == "\\*alpha beta*_."

expect AsciiDocInline.render("\\**α β***", []) == "<strong>*α β</strong>**"

expect AsciiDocInline.render("[.red]++x *bold* y++!", []) == "<span class=\"red\">x *bold* y</span>!"

expect AsciiDocInline.render("[.red]+++alpha+++).", []) == "<span class=\"red\">alpha</span>)."

expect AsciiDocInline.render("[.red]_α β__.", []) == "<em class=\"red\">α β_</em>."

expect AsciiDocInline.render("\\##x_y##.", []) == "<mark>#x_y</mark>#."

expect AsciiDocInline.render("\\+alpha+_.", []) == "\\+alpha+_."

expect AsciiDocInline.render("[.red]^_em_^x.", []) == "<sup class=\"red\"><em>em</em></sup>x."

expect AsciiDocInline.render("\\``alpha beta``).", []) == "<code>`alpha beta`</code>)."

expect AsciiDocInline.render("___em___ end.", []) == "<em><em>em</em></em> end."

expect AsciiDocInline.render("\\**a\\nb**).", []) == "<strong>*a\\nb</strong>*)."

expect AsciiDocInline.render("\\__alpha__*.", []) == "<em>_alpha_</em>*."

expect AsciiDocInline.render("[.red]_x_y_*.", []) == "<em class=\"red\">x_y</em>*."

expect AsciiDocInline.render("\\*x *bold* y*x.", []) == "*x *bold* y*x."

expect AsciiDocInline.render("\\++x *bold* y++ end.", []) == "+x *bold* y+ end."

expect AsciiDocInline.render("\\+++x *bold* y+++ end.", []) == "++x *bold* y++ end."

expect AsciiDocInline.render("[[[ref,Label]]] A book", []) == "[<a id=\"ref\"></a>] A book"

expect AsciiDocInline.render("A >*bold* and <_em_ and &#62;*bold*.", []) == "A &gt;*bold* and &lt;_em_ and &#62;*bold*."

expect AsciiDocInline.render("Test ** a **.", []) == "Test <strong> a </strong>."

expect AsciiDocInline.render("__ a __", []) == "<em> a </em>"

expect AsciiDocInline.render("[[[ref,Label]]] A book", [{ name: "style", value: "bibliography" }]) == "<a id=\"ref\"></a>[Label] A book"
expect AsciiDocInline.render("A *bold* \u(1) and _em_.", []) == "A <strong>bold</strong> \u(1) and <em>em</em>."

expect AsciiDocInline.render("A stem:[x + y] formula.", []) == "A \\$x + y\\$ formula."
expect AsciiDocInline.render("A latexmath:[x^2 + y_1] formula.", []) == "A \\(x^2 + y_1\\) formula."
expect AsciiDocInline.render("A stem:[x^2] formula.", [{ name: "stem", value: "tex" }]) == "A \\(x^2\\) formula."
expect AsciiDocInline.render("A latexmath:[$x$] formula.", []) == "A \\(x\\) formula."
expect AsciiDocInline.render("A stem:[a < b & c] formula.", []) == "A \\$a &lt; b &amp; c\\$ formula."
expect AsciiDocInline.render("A latexmath:q[*x*] formula.", []) == "A \\(<strong>x</strong>\\) formula."

expect AsciiDocInline.render("Hello {NAME}.", [{ name: "name", value: "Roc" }]) == "Hello Roc."

# Snapshot revisions allow a source definition to reset a dynamic attribute,
# including a definition whose text equals its previous definition.
refresh_state : List({ name : Str, value : Str }), AsciiDocInline.State -> AsciiDocInline.State
refresh_state = |attrs, state| {
	# With no overrides there is nothing to reconcile. Record the source for
	# a later first counter/set, without comparing or indexing this snapshot.
	if state.attributes.is_empty() and state.removed.is_empty() {
		return { ..state, snapshot: attrs, snapshot_index: Dict.empty() }
	}
	if attrs == state.snapshot {
		return state
	}
	next = source_attribute_index(attrs)
	previous = if state.snapshot_index.is_empty() source_attribute_index(state.snapshot) else state.snapshot_index
	snapshot = state.snapshot
	var $attributes = state.attributes
	var $removed = state.removed
	base = { ..state, attributes: Dict.empty(), removed: Dict.empty() }
	# Only names occurring in one of the source snapshots can have changed.
	# Never rescan every dynamic counter when unrelated metadata changes.
	for source in [snapshot, attrs] {
		for attr in source {
			name = if attr.name.starts_with("__revision:") drop(attr.name, 11) else attr.name
			if next.get(name) != previous.get(name) or next.get("__revision:${name}") != previous.get("__revision:${name}") {
				$attributes = $attributes.remove(name)
				$removed = $removed.remove(name)
			}
		}
	}
	{ ..base, attributes: $attributes, removed: $removed, snapshot: attrs, snapshot_index: next }
}

# Source attributes use their first matching binding, just like direct().
source_attribute_index : List({ name : Str, value : Str }) -> Dict(Str, Str)
source_attribute_index = |attrs| {
	var $index = Dict.empty()
	for attr in attrs {
		if !$index.contains(attr.name) {
			$index = $index.insert(attr.name, attr.value)
		}
	}
	$index
}

direct : List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
direct = |attrs, name| {
	for attr in attrs {
		if attr.name == name {
			return Ok(attr.value)
		}
	}
	Err(Missing)
}

effective_attributes : List({ name : Str, value : Str }), AsciiDocInline.State -> List({ name : Str, value : Str })
effective_attributes = |attrs, state| merged_attributes(attrs, state.attributes, state.removed)

merged_attributes : List({ name : Str, value : Str }), Dict(Str, Str), Dict(Str, Bool) -> List({ name : Str, value : Str })
merged_attributes = |attrs, attributes, removed| {
	if attributes.is_empty() and removed.is_empty() {
		return attrs
	}
	attributes.to_list().map(|(name, value)| { name, value }).concat(attrs.keep_if(|attr| !removed.contains(attr.name) and !attributes.contains(attr.name)))
}

# Dynamic values override the source snapshot, then removed values remain
# hidden. Built-in attribute defaults are applied by the ordinary lookup.
dynamic_value : List({ name : Str, value : Str }), Dict(Str, Str), Dict(Str, Bool), Str -> Try(Str, [Missing])
dynamic_value = |attrs, attributes, removed, name| {
	match attributes.get(name) {
		Ok(value) => Ok(value)
		Err(_) => if removed.contains(name) Err(Missing) else direct(attrs, name)
	}
}

# A longer delimiter stays safe for every value that was previously stored.
# Removing or redefining attributes therefore never requires a dictionary scan.
safe_prefix : Str, Str -> Str
safe_prefix = |value, prefix| {
	var $prefix = prefix
	while value.contains($prefix) {
		$prefix = $prefix.concat("\u(1)")
	}
	$prefix
}

attribute_delimiter : Str, List({ name : Str, value : Str }), Str -> Str
attribute_delimiter = |source, attrs, prefix| {
	if prefix != "" {
		return prefix
	}
	var $delimiter = "\u(1)"
	while source.contains($delimiter) or attrs.any(|attr| attr.value.contains($delimiter)) {
		$delimiter = $delimiter.concat("\u(1)")
	}
	$delimiter
}

mutate_attributes : Str, List({ name : Str, value : Str }), Groups, AsciiDocInline.State, Str, List(Str) -> { source : Str, state : AsciiDocInline.State, tags : List(Str), prefix : Str }
mutate_attributes = |source, attrs, g, state, prefix, tags| {
	if !source.contains("{") {
		return { source, state, tags, prefix }
	}
	# Reserve a collision-free delimiter only if an insertion needs a tag.
	var $delimiter = prefix
	var $attributes = state.attributes
	var $removed = state.removed
	var $tag_prefix = state.tag_prefix
	var $state = { ..state, attributes: Dict.empty(), removed: Dict.empty() }
	var $tags = tags
	var $lines = []
	for line in source.split_on("\n") {
		bytes = line.to_utf8()
		var $out = ""
		var $pos = 0
		var $drop_line = False
		var $drop_empty = False
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			# Escaped references are protected from this stage, then unescaped
			# by the ordinary scanner.
			if byte == '\\' and (bytes.get($pos + 1) ?? 0) == '{' {
				match find_bytes(bytes, "}".to_utf8(), $pos + 2) {
					Ok(end) => {
						$delimiter = attribute_delimiter(source, attrs, $delimiter)
						$out = $out.concat(token($delimiter, $tags.len()))
						$tags = $tags.append(slice(line, $pos + 1, end - $pos))
						$pos = end + 1
					}
					Err(_) => {
						$out = $out.concat(drop(line, $pos))
						$pos = bytes.len()
					}
				}
			} else if byte == '{' {
				match find_bytes(bytes, "}".to_utf8(), $pos + 1) {
					Ok(end) => {
						key = slice(line, $pos + 1, end - $pos - 1)
						if key.contains("{") and !key.starts_with("set:") and !key.starts_with("counter:") and !key.starts_with("counter2:") {
							$out = $out.concat("{")
							$pos = $pos + 1
						} else {
							parts = key.split_on(":")
							kind = parts.first() ?? ""
							name = (parts.get(1) ?? "").with_ascii_lowercased()
							if (kind == "counter" or kind == "counter2") and name != "" {
								current = dynamic_value(attrs, $attributes, $removed, name)
								value = match current {
									Ok(text) if text != "" => successor(text)
									_ => if parts.len() > 2 Str.join_with(parts.drop_first(2), ":") else "1"
								}
								$tag_prefix = safe_prefix(value, $tag_prefix)
								$attributes = $attributes.insert(name, value)
								$removed = $removed.remove(name)
								if kind == "counter" {
									$out = $out.concat(value)
								} else {
									$drop_empty = True
								}
							} else if kind == "set" and name != "" {
								if name.ends_with("!") {
									unset = take(name, name.to_utf8().len() - 1)
									$attributes = $attributes.remove(unset)
									$removed = $removed.insert(unset, True)
									$drop_line = True
								} else {
									raw = Str.join_with(parts.drop_first(2), ":")
									value = scan(restore_tags(raw, prefix, tags), [], { ..empty, special: g.special }, 0)
									$tag_prefix = safe_prefix(value, $tag_prefix)
									$attributes = $attributes.insert(name, value)
									$removed = $removed.remove(name)
									$drop_empty = True
								}
							} else {
								normalized = key.with_ascii_lowercased()
								resolved = match dynamic_value(attrs, $attributes, $removed, normalized) {
									Ok(value) => Ok(value)
									Err(_) => lookup([], normalized)
								}
								match resolved {
									Ok(value) => {
										if g.attr_quotes or !value.to_utf8().any(|b| b == '<' or b == '>' or b == '&' or b == '+') {
											$out = $out.concat(value)
										} else {
											subs = Str.join_with((if g.replacements ["replacements"] else []).concat(if g.macros ["macros"] else []).concat(if g.post ["post_replacements"] else []), ",")
											var $rendered_html = ""
											if !value.contains("footnote:") {
												effective = if g.macros and macro_candidate(value) merged_attributes(attrs, $attributes, $removed) else attrs
												$rendered_html = scan(value, effective, { ..g, quotes: False, attributes: False, special: False, passes_done: True }, 0)
											} else {
												shared = { ..$state, attributes: $attributes, removed: $removed, tag_prefix: $tag_prefix }
												$attributes = Dict.empty()
												$removed = Dict.empty()
												rendered = AsciiDocInline.render_state(value, attrs, { subs: subs }, shared)
												$rendered_html = rendered.html
												$attributes = rendered.state.attributes
												$removed = rendered.state.removed
												$tag_prefix = rendered.state.tag_prefix
												$state = { ..rendered.state, attributes: Dict.empty(), removed: Dict.empty() }
											}
											if $rendered_html == value and !value.to_utf8().any(|b| b == '<' or b == '>' or b == '&' or b == '+') {
												$out = $out.concat(value)
											} else {
												$delimiter = attribute_delimiter(source, attrs, $delimiter)
												$out = $out.concat(token($delimiter, $tags.len()))
												$tags = $tags.append($rendered_html)
											}
										}
									}
									Err(_) => {
										$out = $out.concat(slice(line, $pos, end - $pos + 1))
									}
								}
							}
							$pos = end + 1
						}
					}
					Err(_) => {
						$out = $out.concat(drop(line, $pos))
						$pos = bytes.len()
					}
				}
			} else {
				var $end = $pos + 1
				while $end < bytes.len() and (bytes.get($end) ?? 0) != '{' and (bytes.get($end) ?? 0) != '\\' {
					$end = $end + 1
				}
				$out = $out.concat(slice(line, $pos, $end - $pos))
				$pos = $end
			}
		}
		if !$drop_line and !($drop_empty and $out.trim() == "") {
			$lines = $lines.append($out)
		}
	}
	# Return the delimiter alongside source by retaining one identity slot when
	# no quote stage supplied it. The caller restores this same tag registry.
	{ source: Str.join_with($lines, "\n"), state: { ..$state, attributes: $attributes, removed: $removed, tag_prefix: $tag_prefix }, tags: $tags, prefix: $delimiter }
}

normalize_number : Str -> Try(Str, [Missing])
normalize_number = |source| {
	text = source.trim()
	negative = text.starts_with("-")
	digits = if negative or text.starts_with("+") drop(text, 1) else text
	if digits == "" or !digits.to_utf8().all(|b| b >= '0' and b <= '9') {
		return Err(Missing)
	}
	var $start = 0
	while $start + 1 < digits.to_utf8().len() and (digits.to_utf8().get($start) ?? 0) == '0' {
		$start = $start + 1
	}
	number = drop(digits, $start)
	Ok(if negative and number != "0" "-${number}" else number)
}

successor : Str -> Str
successor = |source| {
	match normalize_number(source) {
		Ok(number) if number == source => {
			negative = number.starts_with("-")
			var $digits = (if negative drop(number, 1) else number).to_utf8()
			var $pos = $digits.len()
			var $carry = True
			while $pos > 0 and $carry {
				$pos = $pos - 1
				b = $digits.get($pos) ?? '0'
				value = if negative {
					if b == '0' '9' else b - 1
				} else if b == '9' '0' else b + 1
				$carry = if negative b == '0' else b == '9'
				$digits = $digits.set($pos, value) ?? $digits
			}
			if negative {
				normalize_number("-${Str.from_utf8_lossy($digits)}") ?? "0"
			}
				else {
					if $carry "1${Str.from_utf8_lossy($digits)}" else Str.from_utf8_lossy($digits)
				}
		}
		_ => string_successor(source)
	}
}

string_successor : Str -> Str
string_successor = |source| {
	var $bytes = source.to_utf8()
	if $bytes.is_empty() {
		return ""
	}
	var $pos = $bytes.len()
	var $carry = True
	var $first = $bytes.len()
	var $prefix = '1'
	while $pos > 0 and $carry {
		$pos = $pos - 1
		b = $bytes.get($pos) ?? 0
		if (b >= '0' and b <= '9') or (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') {
			$first = $pos
			$prefix = if b >= 'a' and b <= 'z' 'a' else if b >= 'A' and b <= 'Z' 'A' else '1'
			wrap = b == '9' or b == 'z' or b == 'Z'
			replacement = if b == '9' '0' else if b == 'z' 'a' else if b == 'Z' 'A' else b + 1
			$bytes = $bytes.set($pos, replacement) ?? $bytes
			$carry = wrap
		}
	}
	if $first == $bytes.len() {
		last = ($bytes.last() ?? 0)
		$bytes = $bytes.set($bytes.len() - 1, if last == 255 0 else last + 1) ?? $bytes
	} else if $carry {
		$bytes = $bytes.take_first($first).append($prefix).concat($bytes.drop_first($first))
	}
	Str.from_utf8_lossy($bytes)
}

expect AsciiDocInline.render("{counter:n} {counter:n} {counter:n}", []) == "1 2 3"

expect AsciiDocInline.render("{counter:n:5} {counter:n} {counter:n:10} {counter:n}", []) == "5 6 7 8"

expect AsciiDocInline.render("{counter:n:A} {counter:n} {counter:n}", []) == "A B C"

expect AsciiDocInline.render("{counter:n:Z} {counter:n} {counter:n}", []) == "Z AA AB"

expect AsciiDocInline.render("{counter:n:a} {counter:n} {counter:n}", []) == "a b c"

expect AsciiDocInline.render("{counter:n:z} {counter:n} {counter:n}", []) == "z aa ab"

expect AsciiDocInline.render("{counter:n:foo} {counter:n}", []) == "foo fop"

expect AsciiDocInline.render("{counter:n:-2} {counter:n}", []) == "-2 -1"

expect AsciiDocInline.render("{counter:n:1.5} {counter:n}", []) == "1.5 1.6"

expect AsciiDocInline.render("before{counter2:n}after {counter:n}", []) == "beforeafter 2"

expect AsciiDocInline.render("{set:name:}{name}after", []) == "after"

expect AsciiDocInline.render("{set:x:*bold*}{x}", []) == "*bold*"

expect AsciiDocInline.render("{set:x:**bold**}{x}", []) == "&lt;strong&gt;bold&lt;/strong&gt;"

expect AsciiDocInline.render("{set:x:<b> &}{x}", []) == "&lt;b&gt; &amp;"

expect AsciiDocInline.render("{set:a:{b}}{a}", []) == "}{b"

expect AsciiDocInline.render("{set:n:4}{counter:n}", []) == "5"

expect AsciiDocInline.render("\\{counter:n} {counter:n}", []) == "{counter:n} 1"

expect AsciiDocInline.render("{counter:n:999999999999999999999} {counter:n}", []) == "999999999999999999999 1000000000000000000000"

expect AsciiDocInline.render("{counter:n:-999999999999999999999} {counter:n}", []) == "-999999999999999999999 -999999999999999999998"

expect AsciiDocInline.render("{counter:n:005} {counter:n}", []) == "005 006"

expect AsciiDocInline.render("{counter:n:9.9} {counter:n}", []) == "9.9 10.0"

expect AsciiDocInline.render("{set:x:footnote:[Note.]}{x}", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

expect AsciiDocInline.render("A footnote:[Note {counter:n}.] and {n}", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> and 1"

expect AsciiDocInline.render("*{counter:n}* and {counter:n}", []) == "<strong>1</strong> and 2"

expect AsciiDocInline.render("{counter2:n}\nthen {counter:n}", []) == "then 2"

expect AsciiDocInline.render("{set:x!}\nNext {x}", []) == "Next {x}"

expect {
	first = AsciiDocInline.render_state("{counter:n} {counter:n}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{counter:n} {n}", [], { subs: "normal" }, first.state)
	first.html == "1 2" and second.html == "3 3"
}
expect {
	first = AsciiDocInline.render_state("{counter:n}", [{ name: "n", value: "1" }, { name: "__revision:n", value: "1" }], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{counter:n}", [{ name: "n", value: "1" }, { name: "__revision:n", value: "2" }], { subs: "normal" }, first.state)
	first.html == "2" and second.html == "2"
}
expect {
	first = AsciiDocInline.render_state("{set:x:new}{x}", [{ name: "x", value: "old" }], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{x}", [{ name: "x", value: "old" }], { subs: "normal" }, first.state)
	third = AsciiDocInline.render_state("{set:x!}", [{ name: "x", value: "old" }], { subs: "normal" }, second.state)
	fourth = AsciiDocInline.render_state("{x}", [{ name: "x", value: "old" }], { subs: "normal" }, third.state)
	first.html == "new" and second.html == "new" and third.html == "" and fourth.html == "{x}"
}
expect AsciiDocInline.render_state("<<custom>>", [], { subs: "normal" }, { ..AsciiDocInline.initial_state, refs: [{ id: "custom", text: "A *bold* title" }] }).html == "<a href=\"#custom\">A <strong>bold</strong> title</a>"

needs_substitutions : Str, Groups -> Bool
needs_substitutions = |source, g| {
	if g.special and (source.contains("&") or source.contains("<") or source.contains(">")) {
		return True
	}
	if g.quotes and (source.contains("*") or source.contains("_") or source.contains("`") or source.contains("#") or source.contains("^") or source.contains("~")) {
		return True
	}
	if g.attributes and source.contains("{") {
		return True
	}
	if g.macros and (source.contains(":") or source.contains("@") or source.contains("+") or source.contains("[") or source.contains("$")) {
		return True
	}
	if g.macros and ((source.contains("((") and source.contains("))")) or source.contains("<<")) {
		return True
	}
	if g.replacements and (source.contains("'") or source.contains("&amp;") or source.contains("...") or source.contains("->") or source.contains("<-") or source.contains("=>") or source.contains("<=") or source.contains("(C)") or source.contains("(R)") or source.contains("(TM)") or (source.contains("--") and (source.contains("\\--") or has_dash_replacement(source.to_utf8())))) {
		return True
	}
	if (g.quotes or g.attributes or g.macros) and source.contains("\\") {
		return True
	}
	g.post and source.contains(" +")
}

possible_pass : List(U8), U64, Bool -> Bool
possible_pass = |bytes, pos, single| {
	b = bytes.get(pos) ?? 0
	if b == '+' or b == '[' or b == '\\' {
		return True
	}
	if single {
		return False
	}
	if b == '$' {
		return True
	}
	if b == 'p' {
		return (bytes.get(pos + 1) ?? 0) == 'a' and (bytes.get(pos + 2) ?? 0) == 's' and (bytes.get(pos + 3) ?? 0) == 's' and (bytes.get(pos + 4) ?? 0) == ':'
	}
	if b == 's' {
		return (bytes.get(pos + 1) ?? 0) == 't' and (bytes.get(pos + 2) ?? 0) == 'e' and (bytes.get(pos + 3) ?? 0) == 'm' and (bytes.get(pos + 4) ?? 0) == ':'
	}
	if b == 'l' {
		return bytes.drop_first(pos).starts_with([108, 97, 116, 101, 120, 109, 97, 116, 104, 58])
	}
	if b == 'a' {
		return bytes.drop_first(pos).starts_with([97, 115, 99, 105, 105, 109, 97, 116, 104, 58])
	}
	False
}

# Source suffix strings are materialized only at actual substitution sites.
# Most input is copied in a single contiguous UTF-8 slice.
syntax_candidate : List(U8), U64, Groups, Bool, Bool -> Bool
syntax_candidate = |bytes, pos, g, has_email, has_colon| {
	b = bytes.get(pos) ?? 0
	next = bytes.get(pos + 1) ?? 0
	prev = if pos == 0 0 else bytes.get(pos - 1) ?? 0
	if b == '\\' and (g.macros or g.attributes or g.quotes or (g.replacements and (next == '-' or next == '.'))) {
		return True
	}
	if g.special and (b == '&' or b == '<' or b == '>') {
		return True
	}
	if g.attributes and b == '{' {
		return True
	}
	if g.post and b == ' ' and next == '+' {
		return True
	}
	if g.replacements {
		if (b == '&' and bytes.drop_first(pos).starts_with("&amp;".to_utf8())) or (b == '(' and (bytes.drop_first(pos).starts_with("(C)".to_utf8()) or bytes.drop_first(pos).starts_with("(R)".to_utf8()) or bytes.drop_first(pos).starts_with("(TM)".to_utf8()))) {
			return True
		}
		if (b == '.' and next == '.' and (bytes.get(pos + 2) ?? 0) == '.') or (b == '-' and (next == '-' or next == '>')) or (b == '<' and (next == '-' or next == '=')) or (b == '=' and next == '>') or ((b == ' ' or b == '\n') and next == '-' and (bytes.get(pos + 2) ?? 0) == '-') or (b == '\'' and word(prev) and word(next)) {
			return True
		}
	}
	if g.macros {
		if (b == '&' and bytes.drop_first(pos).starts_with([38, 108, 116, 59, 38, 108, 116, 59])) or (b == '<' and next == '<') or (b == '[' and next == '[') or (b == '(' and next == '(') or b == '+' or b == '$' {
			return True
		}
		if has_colon and macro_name_at(bytes, pos) {
			return True
		}
		if has_email and word(b) and !word(prev) {
			var $end = pos
			var $at = False
			while $end < bytes.len() {
				c = bytes.get($end) ?? 0
				if !(word(c) or c == '@' or c == '.' or c == '-' or c == '+') {
					break
				}
				if c == '@' {
					$at = True
				}
				$end = $end + 1
			}
			if $at {
				return True
			}
		}
	}
	False
}

expect AsciiDocInline.render_with("Text *raw* pass:q[*bold*] stem:[x^2].", [], { subs: "macros" }) == "Text *raw* <strong>bold</strong> \\$x^2\\$."

QuoteResult : { source : Str, prefix : Str, tags : List(Str), deferred : List({ slot : U64, source : Str, subs : Str }) }

# Pass substitutions are restored after the enclosing text's attribute and
# macro stages. They share the same counters and footnote registry.
finish_render : Str, List({ name : Str, value : Str }), AsciiDocInline.State, QuoteResult, U64 -> { html : Str, state : AsciiDocInline.State }
finish_render = |html, attrs, state, quoted, notes_start| {
	var $state = state
	var $tags = quoted.tags
	for pass in quoted.deferred {
		rendered = AsciiDocInline.render_state(pass.source, attrs, { subs: pass.subs }, $state)
		$state = rendered.state
		$tags = $tags.set(pass.slot, rendered.html) ?? $tags
	}
	var $notes = $state.footnotes
	$state = { ..$state, footnotes: [] }
	var $index = notes_start
	while $index < $notes.len() {
		match $notes.get($index) {
			Ok(note) => {
				if quoted.prefix != "" and note.text.contains(quoted.prefix) {
					# The loop bounds guarantee a valid index. Do not retain
					# an alias of the old list for an unreachable fallback.
					$notes = $notes.set($index, { name: note.name, text: restore_tags(note.text, quoted.prefix, $tags) }) ?? []
				}
			}
			Err(_) => {}
		}
		$index = $index + 1
	}
	{ html: restore_tags(html, quoted.prefix, $tags), state: { ..$state, footnotes: $notes } }
}

deferred_pass : Str -> Try({ source : Str, subs : Str }, [Missing])
deferred_pass = |source| {
	if !source.starts_with("pass:") {
		return Err(Missing)
	}
	open = find(source, "[", 5)?
	end = first_bracket_end(source, open)?
	subs = slice(source, 5, open - 5)
	if groups(subs).attributes or groups(subs).macros {
		Ok({ source: slice(source, open + 1, end - open - 1), subs })
	} else {
		Err(Missing)
	}
}

expect AsciiDocInline.render("A {scheme}://example.org link.", [{ name: "scheme", value: "https" }]) == "A <a href=\"https://example.org\" class=\"bare\">https://example.org</a> link."
expect AsciiDocInline.render("{scheme}://example.org[Link]", [{ name: "scheme", value: "https" }]) == "<a href=\"https://example.org\">Link</a>"
expect AsciiDocInline.render("A {x} end.", [{ name: "x", value: "++*text*++" }]) == "A ++*text*++ end."
expect AsciiDocInline.render("pass:a[{counter:n}] {counter:n}", []) == "2 1"
expect AsciiDocInline.render("{counter:n} pass:a[{counter:n}]", []) == "1 2"
expect AsciiDocInline.render("pass:a[{set:x:new}] {x}", []) == " {x}"
expect AsciiDocInline.render("pass:a[{set:x:new}] {x}", [{ name: "x", value: "old" }]) == " old"

first_bracket_end : Str, U64 -> Try(U64, [Missing])
first_bracket_end = |source, open| {
	bytes = source.to_utf8()
	var $pos = open + 1
	while $pos < bytes.len() {
		if (bytes.get($pos) ?? 0) == ']' and (bytes.get($pos - 1) ?? 0) != '\\' {
			return Ok($pos)
		}
		$pos = $pos + 1
	}
	Err(Missing)
}
expect AsciiDocInline.render("+pass:a[{counter:n}]+ {counter:n}", []) == "2 1"
expect AsciiDocInline.render("A pass:a[{counter:n}] +pass:a[{counter:n}]+ {counter:n}", []) == "A 2 3 1"
expect AsciiDocInline.render("pass:m[footnote:[A.]] footnote:[B.]", []) == "footnote:[A.] <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

bounded_attribute : Str -> Str
bounded_attribute = |source| {
	if source.to_utf8().len() <= 4096 {
		return source
	}
	var $bytes = source.to_utf8().take_first(4096)
	while !$bytes.is_empty() {
		match Str.from_utf8($bytes) {
			Ok(value) => return value
			Err(_) => {
				$bytes = $bytes.take_first($bytes.len() - 1)
			}
		}
	}
	""
}

expect AsciiDocInline.attribute_value("*Roc* <b> & &copy;", []) == "*Roc* &lt;b&gt; &amp; &amp;copy;"
expect AsciiDocInline.attribute_value("pass:q[*Roc*]", []) == "<strong>Roc</strong>"
expect AsciiDocInline.attribute_value("pass:[<b>raw</b>]", []) == "<b>raw</b>"
expect AsciiDocInline.attribute_value("pass:a[{name}]", [{ name: "name", value: "Roc" }]) == "Roc"
expect AsciiDocInline.attribute_value("prefix {name}", [{ name: "name", value: "Roc" }]) == "prefix Roc"
expect AsciiDocInline.render_with("&copy; &#123; <b>", [], { subs: "specialcharacters" }) == "&amp;copy; &amp;#123; &lt;b&gt;"

expect AsciiDocInline.render("image::/cat.png[]*", []) == "image::/cat.png[]*"
expect AsciiDocInline.render("image::cat.png\nimage::cat.png[]", []) == "image::cat.png\nimage::cat.png[]"
expect AsciiDocInline.render("image::cat.png[alt=Cat,wi****\n=80]", []) == "image::cat.png[alt=Cat,wi*<strong>*</strong>\n=80]"

expect AsciiDocInline.attribute_value("${Str.join_with(List.repeat("a", 4095), "")}é", []).to_utf8().len() == 4095

expect AsciiDocInline.render("{src}/docs/a.roc[Source]", [{ name: "src", value: "https://example.org/repo" }]) == "<a href=\"https://example.org/repo/docs/a.roc\">Source</a>"
expect AsciiDocInline.render("link:{src}/docs/a.roc[Source]", [{ name: "src", value: "https://example.org/repo" }]) == "<a href=\"https://example.org/repo/docs/a.roc\">Source</a>"
expect AsciiDocInline.render("`http://` and `https://`.", []) == "<code>http://</code> and <code>https://</code>."
expect AsciiDocInline.render("`https://example.org`.", []) == "<code><a href=\"https://example.org\" class=\"bare\">https://example.org</a></code>."

expect AsciiDocInline.render("`\\\\`", []) == "<code>\\\\</code>"
expect AsciiDocInline.render("Hello {NAM {NAME}.", [{ name: "name", value: "Roc" }]) == "Hello {NAM Roc."
expect AsciiDocInline.render("Test _____ end.", []) == "Test <em>_</em> end."
expect AsciiDocInline.render("pass:a[{set:x:new}] { {x}", [{ name: "x", value: "old" }]) == " { old"

find_bytes : List(U8), List(U8), U64 -> Try(U64, [Missing])
find_bytes = |bytes, needle, start| {
	if start > bytes.len() or needle.len() > bytes.len() - start {
		return Err(Missing)
	}
	if needle.is_empty() {
		return Ok(start)
	}
	first = needle.get(0) ?? 0
	last = bytes.len() - needle.len()
	var $pos = start
	while $pos <= last {
		if (bytes.get($pos) ?? 0) == first {
			var $matched = 1.U64
			while $matched < needle.len() and bytes.get($pos + $matched) == needle.get($matched) {
				$matched = $matched + 1
			}
			if $matched == needle.len() {
				return Ok($pos)
			}
		}
		$pos = $pos + 1
	}
	Err(Missing)
}

expect find("abc", "", 0) == Ok(0)
expect find("abc", "", 3) == Ok(3)
expect find("abc", "", 4) == Err(Missing)
expect find("abc", "bc", 1) == Ok(1)
expect find("abc", "bc", 2) == Err(Missing)
expect find("aaabaaab", "aab", 0) == Ok(1)
expect find("aaabaaab", "aab", 2) == Ok(5)
expect find("café café", "é", 4) == Ok(9)
expect find("abc", "abcd", 0) == Err(Missing)

# Inspect only the few bytes of a possible macro name. Avoid allocating a
# byte list for every macro spelling at every ordinary character position.
macro_name_at : List(U8), U64 -> Bool
macro_name_at = |bytes, pos| {
	first = bytes.get(pos) ?? 0
	second = bytes.get(pos + 1) ?? 0
	rest = bytes.drop_first(pos)
	match first {
		'h' if second == 't' => rest.starts_with([104, 116, 116, 112, 58, 47, 47]) or rest.starts_with([104, 116, 116, 112, 115, 58, 47, 47])
		'f' if second == 't' or second == 'o' => rest.starts_with([102, 116, 112, 58, 47, 47]) or rest.starts_with([102, 111, 111, 116, 110, 111, 116, 101, 58]) or rest.starts_with([102, 111, 111, 116, 110, 111, 116, 101, 114, 101, 102, 58])
		'i' if second == 'r' or second == 'm' or second == 'n' or second == 'c' => rest.starts_with([105, 114, 99, 58, 47, 47]) or rest.starts_with([105, 109, 97, 103, 101, 58]) or rest.starts_with([105, 110, 100, 101, 120, 116, 101, 114, 109, 58]) or rest.starts_with([105, 110, 100, 101, 120, 116, 101, 114, 109, 50, 58]) or rest.starts_with([105, 99, 111, 110, 58])
		'l' if second == 'i' => rest.starts_with([108, 105, 110, 107, 58])
		'm' if second == 'a' or second == 'e' => rest.starts_with([109, 97, 105, 108, 116, 111, 58]) or rest.starts_with([109, 101, 110, 117, 58])
		'x' if second == 'r' => rest.starts_with([120, 114, 101, 102, 58])
		'a' if second == 'n' => rest.starts_with([97, 110, 99, 104, 111, 114, 58])
		'k' if second == 'b' => rest.starts_with([107, 98, 100, 58])
		'b' if second == 't' => rest.starts_with([98, 116, 110, 58])
		'p' if second == 'a' => rest.starts_with([112, 97, 115, 115, 58])
		_ => False
	}
}

has_dash_replacement : List(U8) -> Bool
has_dash_replacement = |bytes| {
	var $pos = 0
	while $pos + 1 < bytes.len() {
		if (bytes.get($pos) ?? 0) == '-' and (bytes.get($pos + 1) ?? 0) == '-' {
			before = if $pos == 0 0 else bytes.get($pos - 1) ?? 0
			after = bytes.get($pos + 2) ?? 0
			if (word(before) and word(after)) or (($pos == 0 or space(before)) and ($pos + 2 == bytes.len() or space(after))) {
				return True
			}
		}
		$pos = $pos + 1
	}
	False
}

fixed_replacement : List(U8), U64, Groups -> Macro
fixed_replacement = |bytes, pos, g| {
	none = { used: 0, html: "" }
	b = bytes.get(pos) ?? 0
	next = bytes.get(pos + 1) ?? 0
	third = bytes.get(pos + 2) ?? 0
	prev = if pos == 0 0 else bytes.get(pos - 1) ?? 0
	if g.post and b == ' ' and next == '+' {
		if third == '\n' {
			return { used: 3, html: "<br>\n" }
		}
		if pos + 2 == bytes.len() {
			return { used: 2, html: "<br>" }
		}
	}
	if !g.replacements {
		return none
	}
	if b == '\\' and next == '.' and third == '.' and (bytes.get(pos + 3) ?? 0) == '.' {
		return { used: 4, html: "..." }
	}
	if b == '\\' and next == '-' and third == '-' and (bytes.get(pos + 3) ?? 0) != '-' {
		return { used: 3, html: "--" }
	}
	if b == '.' and next == '.' and third == '.' {
		return { used: 3, html: "&#8230;&#8203;" }
	}
	if b == '-' and next == '>' {
		return { used: 2, html: "&#8594;" }
	}
	if b == '<' and next == '-' {
		return { used: 2, html: "&#8592;" }
	}
	if b == '=' and next == '>' {
		return { used: 2, html: "&#8658;" }
	}
	if b == '<' and next == '=' {
		return { used: 2, html: "&#8656;" }
	}
	if b == '\'' and word(prev) and word(next) {
		return { used: 1, html: "&#8217;" }
	}
	if b == '(' and third == ')' {
		if next == 'C' {
			return { used: 3, html: "&#169;" }
		}
		if next == 'R' {
			return { used: 3, html: "&#174;" }
		}
	}
	if b == '(' and next == 'T' and third == 'M' and (bytes.get(pos + 3) ?? 0) == ')' {
		return { used: 4, html: "&#8482;" }
	}
	if b == '-' and next == '-' {
		if word(prev) and word(third) {
			return { used: 2, html: "&#8212;&#8203;" }
		}
		if pos == 0 and (space(third) or pos + 2 == bytes.len()) {
			return { used: if pos + 2 == bytes.len() 2 else 3, html: "&#8201;&#8212;&#8201;" }
		}
	}
	if (b == ' ' or b == '\n') and next == '-' and third == '-' {
		after = bytes.get(pos + 3) ?? 0
		if space(after) or pos + 3 == bytes.len() {
			return { used: if pos + 3 == bytes.len() 3 else 4, html: "&#8201;&#8212;&#8201;" }
		}
	}
	none
}

# Retained mutation expectations remain active across the scanner optimization.
expect AsciiDocInline.render("Test !+++", []) == "Test !+"
expect AsciiDocInline.render("Test [.red]+++<b>+++<b>+++.", []) == "Test <span class=\"red\"><b></span>&lt;b&gt;+."
expect AsciiDocInline.render("A footnote:[A note. footnote:[A note.]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
expect AsciiDocInline.render("Test (+. ++x_y+++!.", []) == "Test (. x_y!."
expect AsciiDocInline.render("See <https://examp====\n.", []) == "See &lt;https://examp====\n."
expect AsciiDocInline.render("Test !**x *bold* [[target]]\ny**_.", []) == "Test !<strong>x <strong>bold</strong> <a id=\"target\"></a>\ny</strong>_."
expect AsciiDocInline.render("Test \\++alpha+", []) == "Test ++alpha+"
expect AsciiDocInline.render("One +++<b>raw<e +++<b>raw</b>+++ end.", []) == "One <b>raw<e &lt;b&gt;raw&lt;/b&gt;+ end."
expect AsciiDocInline.render("B*bold*Definition", []) == "B*bold*Definition"
expect AsciiDocInline.render("****", []) == "<strong>*</strong>*"

expect AsciiDocInline.render("--", []) == "&#8201;&#8212;&#8201;"
expect AsciiDocInline.render("----", []) == "----"
expect AsciiDocInline.render("a\n--\nb", []) == "a&#8201;&#8212;&#8201;b"
expect AsciiDocInline.render("Prefix ((((((( suffix", []) == "Prefix ((((((( suffix"

quote_closers : List(U8), QuoteStage, List(U8) -> List(U64)
quote_closers = |bytes, stage, closing| {
	var $positions = []
	var $pos = 0
	first = closing.first() ?? 0
	while $pos + closing.len() <= bytes.len() {
		if (bytes.get($pos) ?? 0) == first and bytes.drop_first($pos).starts_with(closing) {
			after = bytes.get($pos + closing.len()) ?? 0
			before = if $pos == 0 0 else bytes.get($pos - 1) ?? 0
			if stage.unconstrained or (!space(before) and !word_at(bytes, $pos + closing.len()) and !(stage.tag == "code" and (after == '"' or after == '\'' or after == '`'))) {
				$positions = $positions.append($pos)
			}
		}
		$pos = $pos + 1
	}
	$positions
}

# Ruby's Unicode Word class includes letters, marks, numbers and connector
# punctuation. Emoji and other symbols form valid constrained-span boundaries.
word_at : List(U8), U64 -> Bool
word_at = |bytes, pos| {
	if pos >= bytes.len() {
		return False
	}
	first = bytes.get(pos) ?? 0
	if first < 128 {
		return word(first)
	}
	width = if first < 224 2 else if first < 240 3 else 4
	var $scalar = if width == 2 first.to_u32() - 192 else if width == 3 first.to_u32() - 224 else first.to_u32() - 240
	var $offset = 1
	while $offset < width {
		$scalar = $scalar * 64 + ((bytes.get(pos + $offset) ?? 0).to_u32() % 64)
		$offset = $offset + 1
	}
	match unicode_category($scalar) {
		Ok(Lu) | Ok(Ll) | Ok(Lt) | Ok(Lm) | Ok(Lo) | Ok(Mn) | Ok(Mc) | Ok(Me) | Ok(Nd) | Ok(Nl) | Ok(No) | Ok(Pc) => True
		_ => False
	}
}

word_before : List(U8), U64 -> Bool
word_before = |bytes, pos| {
	if pos == 0 {
		return False
	}
	var $start = pos - 1
	b = bytes.get($start) ?? 0
	if b < 128 {
		return word(b)
	}
	while $start > 0 and pos - $start < 4 and (bytes.get($start) ?? 0) >= 128 and (bytes.get($start) ?? 0) < 192 {
		$start = $start - 1
	}
	word_at(bytes, $start)
}

expect AsciiDocInline.render("Test (😀+a & b+.", []) == "Test (😀a &amp; b."
expect AsciiDocInline.render("😀*bold*😀", []) == "😀<strong>bold</strong>😀"
expect AsciiDocInline.render("α*bold*β", []) == "α*bold*β"
expect AsciiDocInline.render("😀_em_😀", []) == "😀<em>em</em>😀"

unicode_category : U32 -> Try(GeneralCategory.Value, [InvalidScalar])
unicode_category = |scalar| Scalar.from_u32(scalar).map_ok(GeneralCategory.of_scalar)

normal_stages : List(Str)
normal_stages = ["specialcharacters", "quotes", "attributes", "replacements", "macros", "post_replacements"]

substitution_stages : Str -> List(Str)
substitution_stages = |spec| {
	var $stages = []
	var $started = False
	for raw in spec.split_on(",") {
		name = repl(raw.trim(), " ", "")
		append = name.starts_with("+")
		remove = name.starts_with("-")
		prepend = !append and !remove and name.ends_with("+")
		key = if append or remove drop(name, 1) else if prepend take(name, name.to_utf8().len() - 1) else name
		if !$started {
			$stages = if append or remove or prepend normal_stages else []
			$started = True
		}
		resolved = match key {
			"normal" | "n" => normal_stages
			"none" => []
			"verbatim" | "v" | "specialcharacters" | "specialchars" | "c" => ["specialcharacters"]
			"quotes" | "q" => ["quotes"]
			"attributes" | "a" => ["attributes"]
			"replacements" | "r" => ["replacements"]
			"macros" | "m" => ["macros"]
			"post_replacements" | "p" => ["post_replacements"]
			_ => []
		}
		$stages = if remove $stages.keep_if(|stage| !resolved.contains(stage)) else if prepend resolved.concat($stages) else $stages.concat(resolved)
	}
	var $unique = []
	for stage in $stages {
		if !$unique.contains(stage) {
			$unique = $unique.append(stage)
		}
	}
	$unique
}

render_ordered : Str, List({ name : Str, value : Str }), List(Str), AsciiDocInline.State -> { html : Str, state : AsciiDocInline.State }
render_ordered = |input, attrs, stages, state| {
	prior_notes = state.footnotes.len()
	indexed = if state.indexed_ref_count == state.refs.len() state else AsciiDocInline.with_refs(state, state.refs)
	refreshed = refresh_state(attrs, indexed)
	extracted = if stages.contains("macros") and quote_needed(input, { ..empty, macros: True }) quote_state(input, attrs, { ..empty, macros: True, ref_index: refreshed.ref_index }, refreshed) else { source: input, prefix: "", tags: [], deferred: [] }
	var $text = extracted.source
	var $state = refreshed
	for stage in stages {
		rendered = render_state_fixed($text, attrs, { subs: stage }, $state, True)
		$text = rendered.html
		$state = rendered.state
	}
	finish_render($text, attrs, $state, extracted, prior_notes)
}

expect AsciiDocInline.render_with("*<b>*", [], { subs: "quotes,specialcharacters" }) == "&lt;strong&gt;&lt;b&gt;&lt;/strong&gt;"
expect AsciiDocInline.render_with("...", [], { subs: "replacements,specialcharacters" }) == "&amp;#8230;&amp;#8203;"
expect AsciiDocInline.render_with("*word*", [], { subs: "quotes,quotes" }) == "<strong>word</strong>"
expect AsciiDocInline.render_with("*word*", [], { subs: "quotes,-quotes" }) == "*word*"
expect AsciiDocInline.render_with("*word*", [], { subs: "none,+quotes" }) == "<strong>word</strong>"
expect AsciiDocInline.render("*a**b*", []) == "<strong>a</strong>*b*"
expect AsciiDocInline.render("#a##b#", []) == "<mark>a</mark>#b#"
expect AsciiDocInline.render("Test \\##x_y##._y##.", []) == "Test <mark>#x_y</mark>#._y##."
expect AsciiDocInline.render("Test \\[source,roc]\n`α β``.", []) == "Test \\[source,roc]\n<code>α β`</code>."
expect AsciiDocInline.render("Visit https://example.org[Example,endif::[]\nrole=external].", []) == "Visit <a href=\"https://example.org\">Example,endif::[</a>\nrole=external]."
expect AsciiDocInline.render("[[first\nsecond]]", []) == "[[first\nsecond]]"
expect AsciiDocInline.render("\\[plain]", []) == "\\[plain]"
expect AsciiDocInline.render("\\<b>", []) == "\\&lt;b&gt;"

named_item : Str -> Bool
named_item = |part| {
	text = part.trim()
	!text.starts_with("\"") and !text.starts_with("'") and text.contains("=")
}

unquote_attribute : Str -> Str
unquote_attribute = |text| {
	n = text.to_utf8().len()
	if n >= 2 and ((text.starts_with("\"") and text.ends_with("\"")) or (text.starts_with("'") and text.ends_with("'"))) {
		repl(repl(slice(text, 1, n - 2), "\\\"", "\""), "\\'", "'")
	} else text
}

html_attribute : Str -> Str
html_attribute = |text| repl(escape_entities(text), "\"", "&quot;")

link_attributes : List(Str), Bool, Bool -> Str
link_attributes = |parts, bare, new_window| {
	id = named(parts, "id") ?? ""
	role = named(parts, "role") ?? ""
	title = named(parts, "title") ?? ""
	classes = if bare and role != "" "bare ${role}" else if bare "bare" else role
	window = named(parts, "window") ?? if new_window "_blank" else ""
	"${if id == "" "" else " id=\"${html_attribute(id)}\""}${if classes == "" "" else " class=\"${html_attribute(classes)}\""}${if title == "" "" else " title=\"${html_attribute(title)}\""}${if window == "" "" else " target=\"${html_attribute(window)}\"${if window == "_blank" " rel=\"noopener\"" else ""}"}"
}

uri_component : Str -> Str
uri_component = |source| {
	hex = "0123456789ABCDEF".to_utf8()
	var $out = []
	for byte in source.to_utf8() {
		if (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '-' or byte == '_' or byte == '.' or byte == '~' {
			$out = $out.append(byte)
		} else {
			$out = $out.append('%').append(hex.get((byte // 16).to_u64()) ?? 0).append(hex.get((byte % 16).to_u64()) ?? 0)
		}
	}
	Str.from_utf8_lossy($out)
}

expect AsciiDocInline.render("https://example.test[role=external]", []) == "<a href=\"https://example.test\" class=\"bare external\">https://example.test</a>"

expect AsciiDocInline.render("link:doc.html[id=topic]", []) == "<a href=\"doc.html\" id=\"topic\" class=\"bare\">doc.html</a>"

expect AsciiDocInline.render("https://example.test[\"a=b\"]", []) == "<a href=\"https://example.test\">a=b</a>"

expect AsciiDocInline.render("https://example.test[A, B]", []) == "<a href=\"https://example.test\">A, B</a>"

expect AsciiDocInline.render("https://example.test[A, B,role=x]", []) == "<a href=\"https://example.test\" class=\"x\">A</a>"

expect AsciiDocInline.render("image:cat.png[alt=\"A, B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A, B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt='A, B']", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A, B\"></span>"

expect AsciiDocInline.render("image:cat.png[\"A & B\",20,30]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\" width=\"20\" height=\"30\"></span>"

expect AsciiDocInline.render("https://example.test[Example,role='red big']", []) == "<a href=\"https://example.test\" class=\"red big\">Example</a>"

expect AsciiDocInline.render("https://example.test[Example,id=topic,role=x]", []) == "<a href=\"https://example.test\" id=\"topic\" class=\"x\">Example</a>"

expect AsciiDocInline.render("mailto:a@example.test[Name,Subject,Body]", []) == "<a href=\"mailto:a@example.test?subject=Subject&amp;body=Body\">Name</a>"

expect AsciiDocInline.render("link:doc.html[]", []) == "<a href=\"doc.html\" class=\"bare\">doc.html</a>"

expect AsciiDocInline.render("https://example.test[window=\"_blank\"]", []) == "<a href=\"https://example.test\" class=\"bare\" target=\"_blank\" rel=\"noopener\">https://example.test</a>"

expect AsciiDocInline.render("https://example.test[Example,window=_self]", []) == "<a href=\"https://example.test\" target=\"_self\">Example</a>"

expect AsciiDocInline.render("https://example.test[role=x,Label]", []) == "<a href=\"https://example.test\" class=\"bare x\">https://example.test</a>"

expect AsciiDocInline.render("https://example.test[\"a,b=c\",role=x]", []) == "<a href=\"https://example.test\" class=\"x\">a,b=c</a>"

expect AsciiDocInline.render("image:cat.png[\"A \\\"quote\\\" B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &quot;quote&quot; B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt='A \"quote\" B']", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &quot;quote&quot; B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt=Pic,20,30]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"Pic\" width=\"20\" height=\"30\"></span>"

expect AsciiDocInline.render("mailto:a@example.test[Name,A topic,Line one & two]", []) == "<a href=\"mailto:a@example.test?subject=A%20topic&amp;body=Line%20one%20%26amp%3B%20two\">Name</a>"

expect AsciiDocInline.render("mailto:a@example.test[\"Full name\",\"Roc & λ\",\"Body + more\"]", []) == "<a href=\"mailto:a@example.test?subject=Roc%20%26amp%3B%20%CE%BB&amp;body=Body%20%2B%20more\">Full name</a>"

expect AsciiDocInline.render("mailto:a@example.test[role=x]", []) == "<a href=\"mailto:a@example.test\">role=x</a>"

expect AsciiDocInline.render("mailto:a@example.test[Name,id=mail,role=contact,window=_blank]", []) == "<a href=\"mailto:a@example.test\" id=\"mail\" class=\"contact\" target=\"_blank\" rel=\"noopener\">Name</a>"

expect AsciiDocInline.render("xref:topic[role=ref]", []) == "<a href=\"#topic\" class=\"ref\">[topic]</a>"

expect AsciiDocInline.render("xref:topic[Text,id=ref,role=ref]", []) == "<a href=\"#topic\" class=\"ref\">Text</a>"

expect AsciiDocInline.render("https://example.test[Example,title=Help]", []) == "<a href=\"https://example.test\" title=\"Help\">Example</a>"

expect AsciiDocInline.render("image:cat.png[Cat,title=\"Helpful image\",role=thumb,id=photo]", []) == "<span class=\"image thumb\"><img src=\"cat.png\" alt=\"Cat\" title=\"Helpful image\"></span>"

tag_attributes : Str, List(Str) -> List({ name : Str, value : Str })
tag_attributes = |prefix, tags| {
	var $index = 0
	var $attrs = []
	for value in tags {
		$attrs = $attrs.append({ name: "__inline-tag:${token(prefix, $index)}", value })
		$index = $index + 1
	}
	$attrs
}

restore_tag_attributes : Str, List({ name : Str, value : Str }) -> Str
restore_tag_attributes = |input, attrs| {
	var $text = input
	var $depth = 0
	while $depth < 64 {
		previous = $text
		for attr in attrs {
			if attr.name.starts_with("__inline-tag:") {
				$text = repl($text, drop(attr.name, 13), attr.value)
			}
		}
		if $text == previous {
			break
		}
		$depth = $depth + 1
	}
	$text
}

expect AsciiDocInline.render("mailto:a@example.test[Name,*bold* topic,_body_]", []) == "<a href=\"mailto:a@example.test?subject=%3Cstrong%3Ebold%3C%2Fstrong%3E%20topic&amp;body=%3Cem%3Ebody%3C%2Fem%3E\">Name</a>"

expect AsciiDocInline.render("image:cat.png[*Bold* alt]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"<strong>Bold</strong> alt\"></span>"

expect AsciiDocInline.render("https://example.test[Name,role=*red*]", []) == "<a href=\"https://example.test\" class=\"<strong>red</strong>\">Name</a>"

expect AsciiDocInline.render("image:cat.png[alt=\"A &amp; B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\"></span>"

expect AsciiDocInline.render_with("image:cat.png[A & B]", [], { subs: "specialcharacters,macros" }) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\"></span>"

reference_label : List({ name : Str, value : Str }), Groups, Str -> Try(Str, [Missing])
reference_label = |attrs, g, id| {
	match lookup(attrs, "ref:${id}") {
		Ok(text) => Ok(text)
		Err(_) => match g.ref_index.get(id) {
			Ok(text) => Ok(text)
			Err(_) => Err(Missing)
		}
	}
}
expect {
	refs = [{ id: "first", text: "First" }, { id: "second", text: "Second" }]
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, refs)
	first = AsciiDocInline.render_state("See <<second>>", [], { subs: "normal" }, state)
	next = AsciiDocInline.render_state("xref:first[]", [], { subs: "normal" }, first.state)
	first.html == "See <a href=\"#second\">Second</a>" and next.html == "<a href=\"#first\">First</a>"
}
expect {
	initial = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "same", text: "Old" }])
	replaced = AsciiDocInline.with_refs(initial, [{ id: "same", text: "New" }])
	AsciiDocInline.render_state("<<same>>", [], { subs: "normal" }, replaced).html == "<a href=\"#same\">New</a>"
}

expect {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "ref", text: "A *formatted* title" }])
	result = AsciiDocInline.render_state("xref:ref[]", [], { subs: "specialcharacters,macros" }, state)
	result.html == "<a href=\"#ref\">A <strong>formatted</strong> title</a>"
}
expect {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "ref", text: "Registry" }])
	AsciiDocInline.render_state("<<ref>>", [{ name: "ref:ref", value: "Explicit" }], { subs: "normal" }, state).html == "<a href=\"#ref\">Explicit</a>"
}

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exates\"]\n*strong* <raw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,post_replacements,macros,replacements,specialcharacters,attributes" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://exates\"\" class=\"bare\"&gt;https://exates\"&lt;/a&gt;]\n&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;label&lt;/strong&gt;&lt;/a&gt; *attribute*&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exampl|label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "attributes,replacements,macros,quotes,post_replacements,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://exampl|label*\" class=\"bare\"&gt;https://exampl|label*&lt;/a&gt;] &lt;strong&gt;attribute&lt;/strong&gt;&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exaue} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,replacements,post_replacements,attributes,specialcharacters,macros" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; <a href=\"https://exaue}&lt;br&gt\" class=\"bare\">https://exaue}&lt;br&gt</a>;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("mailto:a@example.test[Name,", [], { subs: "normal" }) == "mailto:a@example.test[Name,"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attri&lt;&lt;target&gt;&gt;nts,post_replacements\"]" }], { subs: "normal" }) == "<strong>strong</strong> &lt;raw&gt; &copy; &#8230;&#8203; <a href=\"https://example.test\"><strong>label</strong></a> *attri<a href=\"#target\">[target]</a>nts,post_replacements\"]<br>\nnext <strong>passed</strong>"

expect AsciiDocInline.render("mailto:a@example.test[Name,", []) == "mailto:a@example.test[Name,"

expect AsciiDocInline.render("https://example.test]", []) == "<a href=\"https://example.test\" class=\"bare\">https://example.test</a>]"

# Find a footnote's closing bracket over the original bytes. Only actual
# nested macros require a bounded substring; ordinary text never copies a
# source suffix or validates the remainder of the paragraph again.
footnote_end_at : Str, List(U8), U64, List({ name : Str, value : Str }), Groups -> Try(U64, [Missing])
footnote_end_at = |source, bytes, open, attrs, g| {
	var $pos = open + 1
	note = "footnote:".to_utf8()
	note_ref = "footnoteref:".to_utf8()
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '\\' and (bytes.get($pos + 1) ?? 0) == ']' {
			$pos = $pos + 2
		}
			else if b == ']' {
				return Ok($pos)
			}
				else {
					nested = if macro_name_at(bytes, $pos) and !bytes.drop_first($pos).starts_with(note) and !bytes.drop_first($pos).starts_with(note_ref) {
						var $end = $pos
						while $end < bytes.len() {
							c = bytes.get($end) ?? 0
							if c == '\\' and (bytes.get($end + 1) ?? 0) == ']' {
								$end = $end + 2
							}
								else if c == ']' {
									break
								}
									else {
										$end = $end + 1
									}
						}
						parse_macro(slice(source, $pos, $end - $pos + if $end < bytes.len() 1 else 0), attrs, g, 0).used
					} else 0
					$pos = $pos + if nested > 0 nested else 1
				}
	}
	Err(Missing)
}

expect {
	first = AsciiDocInline.render_state("footnote:[literal \u(1)0\u(1)]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("*bold* footnote:[next]", [], { subs: "normal" }, first.state)
	(second.state.footnotes.first() ?? { name: "", text: "" }).text == "literal \u(1)0\u(1)"
}

expect {
	var $source = ""
	var $i = 0.U64
	while $i < 128 {
		$source = $source.concat("word footnote:[*note*] ")
		$i = $i + 1
	}
	result = AsciiDocInline.render_state($source, [], { subs: "normal" }, AsciiDocInline.initial_state)
	result.state.footnotes.len() == 128 and result.state.footnotes.all(|note| note.text == "<strong>note</strong>") and result.html.contains("_footnoteref_128")
}

path_has_extension : Str -> Bool
path_has_extension = |target| {
	basename = (target.split_on("#").first() ?? target).split_on("/").last() ?? ""
	if basename == "." or basename == ".." {
		return False
	}
	parts = basename.split_on(".")
	parts.len() > 1 and !(parts.len() == 2 and parts.first() == Ok(""))
}

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://example.test[*labelw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,attributes,post_replacements,macros,replacements,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;labelw&gt; &amp;copy; &amp;#8230;&amp;#8203; https://example.test[*label&lt;/strong&gt;&lt;/a&gt; *attribute*&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("xref:other.a<<target>>doc[Other]", [], { subs: "normal" }) == "<a href=\"other.a&lt;&lt;target&gt;&gt;doc\">Other</a>"

expect AsciiDocInline.render_with("mailto:a@example.test[N\r\ne,Subject,Body]", [], { subs: "normal" }) == "<a href=\"mailto:a@example.test?subject=Subject&amp;body=Body\">N e</a>"

expect AsciiDocInline.render_with("image:cat.png[alt=\"A &amp; age:cat.png[alt=\"A &amp; B\"]", [], { subs: "normal" }) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; age:cat.png[alt=\" width=\"A &amp; B\"\"></span>"

expect AsciiDocInline.render_with("*strong* <raw> &c<<target>>opy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "attributes,post_replacements,replacements,macros,quotes,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;c&lt;&lt;target&gt;&gt;opy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;label&lt;/strong&gt;&lt;/a&gt; &lt;strong&gt;attribute&lt;/strong&gt;&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render("--\\--\nRoc", []) == "----\nRoc"
expect AsciiDocInline.render("\\...", []) == "..."
expect AsciiDocInline.render("\\....", []) == "...."
expect AsciiDocInline.render_with("\\--", [], { subs: "replacements" }) == "--"

expect [2, 3, 4, 5].all(
	|n| {
		run = Str.join_with(List.repeat("-", n), "")
		escaped = "\\${run}"
		expected = if n == 2 run else escaped
		AsciiDocInline.render(escaped, []) == expected and AsciiDocInline.render("A${escaped}B", []) == "A${expected}B" and AsciiDocInline.render("--${escaped}", []) == "--${expected}"
	},
)

expect {
	result = AsciiDocInline.render_state("{note}", [{ name: "note", value: "footnote:[A note.]" }], { subs: "normal" }, AsciiDocInline.initial_state)
	result.state.footnotes == [{ name: "", text: "A note." }] and result.html.contains("_footnoteref_1")
}

expect {
	var $state = AsciiDocInline.initial_state
	var $correct = True
	var $number = 0.U64
	while $number < 128 {
		rendered = AsciiDocInline.render_state("{counter:key${$number.to_str()}}", [], { subs: "normal" }, $state)
		$correct = $correct and rendered.html == "1"
		$state = rendered.state
		$number = $number + 1
	}
	final = AsciiDocInline.render_state("{counter:key0} {counter:key127} {key64}", [{ name: "role", value: "unrelated" }], { subs: "normal" }, $state)
	$correct and final.html == "2 2 1"
}

expect AsciiDocInline.render("*bold* {mark}", [{ name: "mark", value: "\u(1)" }]) == "<strong>bold</strong> \u(1)"
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {mark}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{counter:key:\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {key}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{set:mark!}", [], { subs: "normal" }, first.state)
	AsciiDocInline.render_state("*bold* {counter:key_0}", [], { subs: "normal" }, second.state).html == "<strong>bold</strong> 1"
}
expect {
	first = AsciiDocInline.render_state("pass:a[{set:mark:\u(1)}]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {mark}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)}footnote:note[{mark}]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("*bold* footnote:note[] {mark}", [], { subs: "normal" }, first.state)
	second.state.footnotes == [{ name: "note", text: "\u(1)" }] and second.html.contains("<strong>bold</strong>") and second.html.contains("_footnotedef_1") and second.html.ends_with(" \u(1)")
}

expect {
	attrs = [{ name: "n", value: "4" }, { name: "n", value: "99" }, { name: "__revision:n", value: "1" }]
	plain = AsciiDocInline.render_state("plain", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, plain.state)
	redefined = [{ name: "n", value: "4" }, { name: "n", value: "99" }, { name: "__revision:n", value: "2" }]
	reset = AsciiDocInline.render_state("{counter:n}", redefined, { subs: "normal" }, first.state)
	first.html == "5" and reset.html == "5"
}
expect {
	attrs = [{ name: "n", value: "4" }, { name: "__revision:n", value: "1" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	next = AsciiDocInline.render_state("{counter:n}", attrs.append({ name: "role", value: "metadata" }), { subs: "normal" }, first.state)
	next.html == "6"
}
expect {
	plain = AsciiDocInline.render_state("plain", [{ name: "n", value: "4" }], { subs: "normal" }, AsciiDocInline.initial_state)
	attrs = [{ name: "n", value: "8" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, plain.state)
	second = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, first.state)
	first.html == "9" and second.html == "10"
}
