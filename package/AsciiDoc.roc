## Pure AsciiDoc block parsing and HTML rendering.
AsciiDoc := { title : Str, blocks : List(AsciiDoc.Block) }.{

	## Reject active include directives that were left unexpanded.
	Lints := { unexpanded_includes : Bool ?? True }

	## Source and pure preprocessing options for checked conversion.
	## Includes are supplied explicitly; validation never reads files.
	ValidationProps := {
		source : Str,
		name : Str ?? "<literal>",
		lints : Lints ?? {},
		includes : List({ path : Str, source : Str }) ?? [],
		attributes : List(Attribute) ?? [],
		max_depth : U64 ?? 16,
		expand_includes : Bool ?? False,
	}

	## Parse once, reporting native failures before enabled lint failures.
	validate : ValidationProps -> Try(Document, [BadQuotedBytes(Str)])
	validate = |props| {
		parsed = parse_document_metadata(props.source, { includes: props.includes, attributes: props.attributes, max_depth: props.max_depth, expand_includes: props.expand_includes }).map_err(
			|error| {
				match error {
					InvalidAsciiDoc(detail) => Quotation.error({ name: props.name, format: "AsciiDoc", message: detail.message, line: detail.line })
					Unsupported(detail) => Quotation.error({ name: props.name, format: "AsciiDoc", message: "unsupported: ${detail.message}", line: detail.line })
				}
			},
		)?
		match parsed.unexpanded_includes.first() {
			Ok(error) if props.lints.unexpanded_includes => Err(Quotation.error({ name: if (error.path == "") props.name else error.path, format: "AsciiDoc", message: error.message, line: error.line }))
			_ => Ok(parsed.value)
		}
	}

	## A quoted document uses the default pure preprocessing and lint rules.
	from_quote : Str -> Try(AsciiDoc, [BadQuotedBytes(Str)])
	from_quote = |source| AsciiDoc.validate({ source: source }).map_ok(|document| AsciiDoc.(document))

	## Parsed title and source-preserving blocks.
	Document : { title : Str, blocks : List(Block) }

	## Attribute name and its string value.
	Attribute : { name : Str, value : Str }

	## Immutable ordered bindings shared by parsed block snapshots.
	Attributes : AsciiDocAttributes.Attributes

	## Construct independent ordered bindings for a manually assembled block.
	attributes : List(Attribute) -> Attributes
	attributes = AsciiDocAttributes.flat

	## Return all bindings as an ordered list for inspection or replacement.
	materialize_attributes : Attributes -> List(Attribute)
	materialize_attributes = AsciiDocAttributes.materialize

	## Render inline text using a block's shared attribute bindings.
	render_inline_attributes : Str, Attributes -> Str
	render_inline_attributes = |source, attrs| if (source == "") "" else render_cached_inline(source, attrs, AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view, "normal", AsciiDocInline.initial_state).html

	## A source block; Rich retains metadata and nested block contexts.
	Block := [Paragraph(Str), Section({ level : U64, title : Str, blocks : List(Block) }), Listing(Str), Literal(Str), ThematicBreak, Rich({ context : Str, source : Str, title : Str, style : Str, id : Str, attrs : Attributes, blocks : List(Block), level : U64 })].{

		## Compare source blocks, including metadata and nested content.
		is_eq : _
	}

	## A located parse error: UTF-8 byte offset and one-based line.
	Error : { offset : U64, line : U64, message : Str }

	## Virtual sources and initial attributes for bounded preprocessing.
	Options : { includes : List({ path : Str, source : Str }), attributes : List(Attribute), max_depth : U64, expand_includes : Bool }

	## Default attributes, includes disabled and include depth limited to 16.
	default_options : Options
	default_options = AsciiDocPreprocess.defaults

	## Parse a UTF-8 document with default preprocessing options.
	parse_str : Str -> Try(Document, [Unsupported(Error), InvalidAsciiDoc(Error)])
	parse_str = |source| parse_with(source, AsciiDocPreprocess.defaults)

	## Parse using only caller-supplied include sources; never reads files.
	parse_with : Str, Options -> Try(Document, [Unsupported(Error), InvalidAsciiDoc(Error)])
	parse_with = |source, options| parse_document_metadata(source, options).map_ok(|parsed| parsed.value)

	## Render inline substitutions with caller-supplied attributes.
	render_inline : Str, List(Attribute) -> Str
	render_inline = |source, attrs| AsciiDocInline.render(source, attrs)

	## Render an embedded HTML document with shared IDs and footnotes.
	render : Document -> Str
	render = |document| render_document(document)

	## The lines an `include::` directive selects from its source, as returned
	## by [AsciiDoc.select_include].
	##
	## - `text`: the selected lines after any `indent=`, joined with LF and with
	##   no final LF. As in include expansion, a CR before each LF and a leading
	##   byte order mark are removed.
	## - `lines`: the one-based source line number of each selected line, in
	##   order. A final LF ends the last line rather than starting a new one,
	##   and an empty source is a single empty line.
	## - `problems`: each [AsciiDoc.IncludeProblem] found, once, in the order
	##   found. An empty list means the selection is clean.
	##
	## ```roc
	## expect AsciiDoc.select_include("one\ntwo\nthree\n", "lines=2..3") == { text: "two\nthree", lines: [2, 3], problems: [] }
	## ```
	IncludeSelection : { text : Str, lines : List(U64), problems : List(IncludeProblem) }

	## Something in an include's selection that Asciidoctor would warn about,
	## or would select differently. Selection itself stays as lenient as
	## Asciidoctor's, so a strict caller rejects any problem and a lenient
	## caller ignores them.
	##
	## Tag markers are lines containing `tag::NAME[]` or `end::NAME[]`. They are
	## checked only when the attributes select by `tag=` or `tags=` without
	## `lines=`, and only for the tags the selector names, positively or with
	## `!`, or for every tag when it uses `*` or `!*`. These are the tags
	## Asciidoctor tracks. Markers of other tags are still removed from the
	## selection.
	##
	## - `MissingTag(name)`: `tag=` or `tags=` requests `name` positively (not
	##   `*`, `**` or `!name`), but no `tag::name[]` marker opens it. Asciidoctor
	##   warns that the tag is not found.
	## - `UnclosedTag(name)`: a `tag::name[]` marker has no `end::name[]` before
	##   the source ends, which Asciidoctor warns about, or before
	##   `tag::name[]` appears again. Asciidoctor nests a re-opened tag
	##   silently, but this selection treats both as one region that the first
	##   `end::name[]` closes, so the two can select different lines.
	## - `UnmatchedEnd(name)`: an `end::name[]` marker while `name` is not
	##   open, or while a checked tag opened after `name` is still open, so the
	##   two regions cross instead of nesting. The marker still closes `name`.
	##   Asciidoctor warns of an unexpected or mismatched end tag when the
	##   selector names `name`. For a tag only `*` or `!*` checks, it ignores
	##   such an end silently, and warns later if that leaves the tag unclosed.
	## - `InvalidLines(entry)`: an entry of `lines=`, without surrounding
	##   spaces, that is not `N`, `N..M` with N ≤ M, `N..` or `N..-1`, where N
	##   and M are plain decimal numbers and N is at least 1; or one whose first
	##   line is past the end of the source. Asciidoctor reads these entries
	##   without a warning: `4abc` selects line 4 and `+2` line 2, a reversed
	##   range such as `4..2` selects nothing (so a list of only reversed ranges
	##   selects every line), and a start of 0 or less selects nothing at all.
	##   A range that only partly runs past the end is not a problem.
	## - `EmptySelection`: no lines are selected.
	##
	## When `lines=` is given, `tag=` and `tags=` are ignored, as in
	## Asciidoctor, and report nothing.
	##
	## ```roc
	## expect AsciiDoc.select_include("a\n// tag::x[]\nb\n", "tag=x").problems == [UnclosedTag("x")]
	## ```
	IncludeProblem : [MissingTag(Str), UnclosedTag(Str), UnmatchedEnd(Str), InvalidLines(Str), EmptySelection]

	## Select the lines of `source` that an include directive with this
	## attribute list selects, such as `lines=1..4,indent=0` or `tag=example`.
	## `include::` expansion calls the same code, so a tool that expands
	## includes itself selects exactly the lines this package would.
	##
	## The attributes it reads are `lines`, `tag`, `tags` and `indent`; others,
	## such as `leveloffset`, are ignored. `lines=` takes precedence over `tag=`
	## and `tags=`, and `tag=` over `tags=`. Attribute references such as
	## `{range}` are not expanded.
	##
	## - `lines=`: entries separated by `,`, or by `;` when there is no comma,
	##   each a line `N`, a range `N..M`, or an open range `N..` or `N..-1`.
	## - `tag=` names one tag and `tags=` several, separated likewise. `!name`
	##   excludes a tag, `*` selects the lines inside any tag and `**` every
	##   line. Either way, the marker lines themselves are left out.
	## - `indent=N`: removes the leading spaces that every nonblank selected
	##   line shares, then indents every line by N spaces. `indent=0` dedents.
	##
	## ```roc
	## expect AsciiDoc.select_include("  a\n  // tag::x[]\n    b\n  // end::x[]\n", "tag=x,indent=0") == { text: "b", lines: [3], problems: [] }
	## ```
	select_include : Str, Str -> IncludeSelection
	select_include = AsciiDocPreprocess.select_include
}

import Literal as Quotation
import AsciiDocInline
import AsciiDocPreprocess
import unicode.Case
import unicode.GeneralCategory
import unicode.Scalar
import AsciiDocTable
import AsciiDocMedia
import AsciiDocBlockHtml
import AsciiDocAttributes
import AsciiDocSource

# Parser sidecar metadata is kept outside the public document and parse APIs.
parse_document_metadata : Str, AsciiDoc.Options -> Try({ value : AsciiDoc.Document, unexpanded_includes : List(AsciiDocPreprocess.Error) }, [Unsupported(AsciiDoc.Error), InvalidAsciiDoc(AsciiDoc.Error)])
parse_document_metadata = |source, options| {
	if source.to_utf8().len() > 262144 {
		return Err(InvalidAsciiDoc({ offset: 0, line: 1, message: "AsciiDoc source exceeds 262144-byte limit" }))
	}
	checked = match AsciiDocPreprocess.run_scoped_checked(source, options) {
		Ok(value) => value
		Err(PreprocessError(error)) => return Err(InvalidAsciiDoc({ offset: error.offset, line: error.line, message: error.message }))
	}
	value = parse_processed(checked.processed)?
	Ok({ value, unexpanded_includes: checked.unexpanded_includes })
}

parse_processed : AsciiDocPreprocess.ScopedResult -> Try(AsciiDoc.Document, [Unsupported(AsciiDoc.Error), InvalidAsciiDoc(AsciiDoc.Error)])
parse_processed = |processed| {
	structure = structural_lines(processed.lines)
	validate_nesting(structure)?
	validate_list_nesting(structure)?
	source_lines = processed.lines.map(|line| { text: trim_end(line.text), attrs: line.attributes })
	var $lines = normalize_headings(source_lines)
	var $header_ready = False
	while !$header_ready and !$lines.is_empty() {
		first = ($lines.first() ?? empty_line).text
		if first == "" or (first.starts_with("//") and delimiter(first) != "comment") {
			$lines = $lines.drop_first(1)
		}
		else if delimiter(first) == "comment" {
			$lines = take_delimited($lines.drop_first(1), first).rest
		}
		else {
			$header_ready = True
		}
	}
	var $candidate = $lines
	var $discrete = False
	var $document_id = ""
	var $document_reftext = ""
	while !$candidate.is_empty() and is_block_metadata(($candidate.first() ?? empty_line).text) {
		metadata_line = ($candidate.first() ?? empty_line).text
		metadata = parse_metadata(metadata_line)
		if metadata_line.starts_with("[[") and metadata_line.ends_with("]]") {
			parts = split_attributes(Str.from_utf8_lossy(metadata_line.to_utf8().drop_first(2).drop_last(2)))
			$document_id = parts.first() ?? ""
			if parts.len() > 1 {
				$document_reftext = unquote(parts.get(1) ?? "")
			}
		} else if attribute(metadata, "id") != "" {
			$document_id = attribute(metadata, "id")
		}
		if has_attr(metadata, "reftext") {
			$document_reftext = attribute(metadata, "reftext")
		}
		if attribute(metadata, "1") == "discrete" {
			$discrete = True
		}
		$candidate = $candidate.drop_first(1)
		while !$candidate.is_empty() and (($candidate.first() ?? empty_line).text == "" or ($candidate.first() ?? empty_line).text.starts_with("//")) {
			$candidate = $candidate.drop_first(1)
		}
	}
	if !$discrete and ($candidate.first() ?? empty_line).text.starts_with("= ") {
		$lines = $candidate
	}
	lines = $lines
	match lines {
		[first, .. as rest] if first.text.starts_with("= ") and numeric_prefix(attribute(first.attrs, "leveloffset")) == 0 => {
			var $body = rest
			var $metadata_lines = 0.U64
			while !$body.is_empty() and ($body.first() ?? empty_line).text != "" and $metadata_lines < 2 {
				candidate = ($body.first() ?? empty_line).text
				if candidate.starts_with("//") {
					$body = $body.drop_first(1)
				}
				else if $metadata_lines == 1 and candidate.trim().starts_with(":") {
					break
				}
				else {
					$body = $body.drop_first(1)
					$metadata_lines = $metadata_lines + 1
				}
			}
			raw_title = drop_text(first.text, 2).trim()
			header_attrs = if ($body.is_empty()) processed.attributes else ($body.first() ?? empty_line).attrs
			parsed = document_reference(parse_blocks($body, 98).blocks, $document_id, if ($document_reftext == "") raw_title else $document_reftext)
			if raw_title.contains("{") or raw_title.contains("footnote:") or raw_title.contains("<<") or raw_title.contains("xref:") or has_attr(header_attrs, "doctitle") {
				stored = store_header(parsed, raw_title, first.attrs, header_attrs)
				stored_attrs = (render_node(stored.first() ?? empty_item)).attrs
				refs = if (header_ref_independent(stored_attrs)) [] else identify_blocks(stored, { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False }).state.refs
				header = prepare_header(stored_attrs, refs)
				Ok({ title: header.html, blocks: if (parsed.is_empty() and header.state.footnotes.is_empty()) [] else stored })
			} else {
				Ok({ title: AsciiDoc.render_inline_attributes(raw_title, header_attrs), blocks: parsed })
			}
		}
		_ => Ok({ title: "", blocks: parse_blocks(lines, 98).blocks })
	}
}

Line : { text : Str, attrs : AsciiDoc.Attributes }

# Empty list item used when constructing source projections.
empty_item : AsciiDoc.Block
empty_item = Rich({ context: "list_item", source: "", title: "", style: "", id: "", attrs: AsciiDocAttributes.flat([]), blocks: [], level: 0 })

empty_line : Line
empty_line = { text: "", attrs: AsciiDocAttributes.flat([]) }

drop_text : Str, U64 -> Str
drop_text = |text, count| Str.from_utf8_lossy(text.to_utf8().drop_first(count))

trim_end : Str -> Str
trim_end = |text| {
	bytes = text.to_utf8()
	var $n = bytes.len()
	while $n > 0 and [32, 9, 13, 11, 12, 0].contains(bytes.get($n - 1) ?? 0) {
		$n = $n - 1
	}
	if ($n == bytes.len()) text else Str.from_utf8_lossy(bytes.take_first($n))
}

make_lines : Str -> List(Line)
make_lines = |source| {
	s = if (source.starts_with("﻿")) drop_text(source, 3) else source
	s.split_on("\n").map(|text| { text: trim_end(text), attrs: AsciiDocAttributes.flat([]) })
}

rich : Str, Str, Str, Str, Str, AsciiDoc.Attributes, List(AsciiDoc.Block), U64 -> AsciiDoc.Block
rich = |context, source, title, style, id, attrs, blocks, level| Rich({ context, source, title, style, id, attrs, blocks, level })

heading : Str -> { level : U64, title : Str }
heading = |text| {
	if !text.starts_with("=") {
		return { level: 0, title: "" }
	}
	bytes = text.to_utf8()
	var $n = 0
	while bytes.get($n) == Ok('=') {
		$n = $n + 1
	}
	if $n >= 1 and $n <= 6 and bytes.get($n) == Ok(' ') {
		{ level: $n - 1, title: clean_heading(drop_text(text, $n + 1), $n) }
	} else {
		{ level: 0, title: "" }
	}
}

attribute : AsciiDoc.Attributes, Str -> Str
attribute = |attrs, name| lookup_attribute(attrs, name) ?? ""

lookup_attribute : AsciiDoc.Attributes, Str -> Try(Str, [Missing])
lookup_attribute = AsciiDocAttributes.lookup_attributes

parse_metadata : Str -> AsciiDoc.Attributes
parse_metadata = |text| {
	inner = drop_text(text, 1).to_utf8().drop_last(1) |> Str.from_utf8_lossy
	var $attrs = AsciiDocAttributes.flat([])
	var $position = 0.U64
	for part in split_attributes(inner) {
		$position = $position + 1
		value = part.trim()
		bits = value.split_on("=")
		match bits {
			[name, .. as rest] if !rest.is_empty() => {
				value_text = Str.join_with(rest, "=").trim()
				if value_text != "None" {
					$attrs = $attrs.append({ name: name.trim(), value: unquote(value_text) })
				}
			}
			_ => {
				$attrs = $attrs.concat(parse_compact(value, $position))
			}
		}
	}
	$attrs
}

list_marker : Str -> { kind : Str, level : U64, text : Str, term : Str }
list_marker = |text| {
	if text.starts_with(" ") or text.starts_with("\t") {
		return list_marker(text.trim())
	}
	bytes = text.to_utf8()
	var $n = 0
	first = bytes.first() ?? 0
	while bytes.get($n) == Ok(first) and [42, 46].contains(first) {
		$n = $n + 1
	}
	if $n > 0 and (bytes.get($n) == Ok(' ') or bytes.get($n) == Ok(9)) {
		return { kind: if (first == '*') "ulist" else "olist", level: $n, text: drop_text(text, $n + 1).trim(), term: "" }
	}
	if bytes.len() > 3 and ((first >= 65 and first <= 90) or (first >= 97 and first <= 122)) and bytes.get(1) == Ok(46) and bytes.get(2) == Ok(32) {
		return { kind: "olist", level: 1, text: drop_text(text, 3).trim(), term: "" }
	}
	if text.starts_with("- ") {
		return { kind: "ulist", level: 1, text: drop_text(text, 2), term: "" }
	}
	var $digits = 0
	while (bytes.get($digits) ?? 0) >= '0' and (bytes.get($digits) ?? 255) <= '9' {
		$digits = $digits + 1
	}
	if $digits > 0 and bytes.get($digits) == Ok('.') and bytes.get($digits + 1) == Ok(' ') {
		return { kind: "olist", level: 1, text: drop_text(text, $digits + 2), term: "" }
	}
	if text.starts_with("<") {
		match text.split_on("> ") {
			[marker, body] => if marker == "<." or (U64.from_str(drop_text(marker, 1)).is_ok()) {
				return { kind: "colist", level: 1, text: body, term: "" }
			}
			_ => {}
		}
	}
	if !text.contains("::") and !text.contains(";;") {
		return { kind: "", level: 0, text: "", term: "" }
	}
	var $colon = 1.U64
	while $colon + 1 < bytes.len() {
		first_colon = bytes.get($colon) ?? 0
		var $count = 0.U64
		if [58, 59].contains(first_colon) {
			while $colon + $count < bytes.len() and bytes.get($colon + $count) == Ok(first_colon) {
				$count = $count + 1
			}
			if $count >= 2 and $count <= (if (first_colon == 59) 2 else 4) and ($colon + $count == bytes.len() or bytes.get($colon + $count) == Ok(32) or bytes.get($colon + $count) == Ok(9)) {
				return { kind: "dlist", level: $count - 1, text: drop_text(text, $colon + $count).trim(), term: Str.from_utf8_lossy(bytes.take_first($colon)) }
			}
		}
		$colon = $colon + 1
	}

	{ kind: "", level: 0, text: "", term: "" }
}

delimiter : Str -> Str
delimiter = |text| {
	if text == "--" {
		return "open"
	}
	if text.count_utf8_bytes() < 4 {
		return ""
	}
	bytes = text.to_utf8()
	first = bytes.first() ?? 0
	context = match first {
		124 | 44 | 58 | 33 => "table"
		45 => "listing"
		46 => "literal"
		61 => "example"
		42 => "sidebar"
		95 => "quote"
		43 => "pass"
		47 => "comment"
		_ => ""
	}
	if context == "" {
		return ""
	}
	if context == "table" {
		if (bytes.drop_first(1).all(|b| b == 61)) context else ""
	} else if (bytes.all(|b| b == first)) context else ""
}

take_delimited : List(Line), Str -> { source : Str, lines : List(Line), rest : List(Line) }
take_delimited = |lines, end| {
	var $rest = lines
	var $body = []
	var $lines = []
	var $done = False
	while !$done and !$rest.is_empty() {
		line = $rest.first() ?? empty_line
		text = line.text
		$rest = $rest.drop_first(1)
		if text == end {
			$done = True
		} else {
			$body = $body.append(text)
			$lines = $lines.append(line)
		}
	}
	if !$done {
		while !$body.is_empty() and ($body.last() ?? "") == "" {
			$body = $body.drop_last(1)
			$lines = $lines.drop_last(1)
		}
	}
	{ source: Str.join_with($body, "\n"), lines: $lines, rest: $rest }
}

parse_blocks : List(Line), U64 -> { blocks : List(AsciiDoc.Block), rest : List(Line) }
parse_blocks = |lines, parent| {
	parsed = parse_blocks_cached(lines, parent, AsciiDocAttributes.empty)
	{ blocks: parsed.blocks, rest: parsed.rest }
}

parse_blocks_cached : List(Line), U64, AsciiDocAttributes.Cache -> { blocks : List(AsciiDoc.Block), rest : List(Line), cache : AsciiDocAttributes.Cache }
parse_blocks_cached = |lines, parent, cache| {
	var $attribute_cache = cache
	var $rest = lines
	var $blocks = []
	var $attrs = AsciiDocAttributes.flat([])
	var $title = ""
	var $id = ""
	var $done = False
	while !$done and !$rest.is_empty() {
		line = $rest.first() ?? empty_line
		text = line.text
		tail = $rest.drop_first(1)
		line_indexed = AsciiDocAttributes.indexed_attributes(line.attrs, $attribute_cache)
		$attribute_cache = line_indexed.cache
		h = adjusted_heading_cached(text, line.attrs, line_indexed.view)
		if parent < 98 and h.title != "" and h.level <= parent {
			$done = True
		}
		else if text == "" or (text.starts_with("//") and delimiter(text) != "comment") {
			$rest = tail
		}
		else if parent < 98 and (text.starts_with("[") or text.starts_with(".")) and following_heading($rest).title != "" and following_heading($rest).level <= parent {
			$done = True
		}
		else if text.starts_with("[[") and text.ends_with("]]") {
			target = Str.from_utf8_lossy(text.to_utf8().drop_first(2).drop_last(2))
			parts = split_attributes(target)
			$id = (parts.first() ?? "").trim()
			if parts.len() > 1 {
				$attrs = $attrs.append({ name: "reftext", value: unquote(Str.join_with(parts.drop_first(1), ",")) })
			}
			$rest = tail
		}
		else if is_block_metadata(text) {
			$attrs = $attrs.concat(parse_metadata(text))
			$rest = tail
		}
		else if is_block_title(text) and delimiter(text) == "" and list_marker(text).kind == "" {
			$title = drop_text(text, 1)
			$rest = tail
		}
		else if text.starts_with("> ") or text == ">" {
			var $quote = []
			while !$rest.is_empty() and (($rest.first() ?? empty_line).text.starts_with("> ") or ($rest.first() ?? empty_line).text == ">") {
				$quote = $quote.append({ text: drop_text(($rest.first() ?? empty_line).text, 2), attrs: line.attrs })
				$rest = $rest.drop_first(1)
			}
			quoted = parse_blocks_cached($quote, parent, $attribute_cache)
			$attribute_cache = quoted.cache
			$blocks = $blocks.append(rich("quote", "", $title, "quote", $id, if ($attrs.is_empty()) line.attrs else $attrs.concat(line.attrs), quoted.blocks, 0))
			$attrs = AsciiDocAttributes.flat([])
			$title = ""
			$id = ""
		}
		else {
			# List.concat promises unique output, even when its left
			# input is empty. Reuse the immutable snapshot directly.
			$attrs = if ($attrs.is_empty()) line.attrs else $attrs.concat(line.attrs)
			indexed = AsciiDocAttributes.indexed_attributes($attrs, $attribute_cache)
			$attribute_cache = indexed.cache
			style = cached_attribute($attrs, indexed.view, "1")
			explicit_id = if ($id != "") $id else cached_attribute($attrs, indexed.view, "id")
			id = if (explicit_id != "") explicit_id else if (h.title != "") heading_id(text) else ""
			kind = delimiter(text)
			if h.title != "" and parent < 99 {
				if style == "discrete" {
					$blocks = $blocks.append(rich("floating_title", "", h.title, style, id, $attrs, [], h.level))
					$rest = tail
				}
				else {
					child_lines = if ($title == "") tail else [{ text: ".${$title}", attrs: line.attrs }].concat(tail)
					child = parse_blocks_cached(child_lines, h.level, $attribute_cache)
					$attribute_cache = child.cache
					child_blocks = if (h.level == 0 and attribute($attrs, "doctype") == "book") wrap_part_intro(child.blocks, $attrs) else child.blocks
					$blocks = $blocks.append(if (plain_attrs($attrs) and id == "") Section({ level: h.level, title: h.title, blocks: child_blocks }) else rich("section", "", h.title, style, id, $attrs, child_blocks, h.level))
					$rest = child.rest
				}
			} else if kind != "" {
				body = take_delimited(tail, text)
				if kind == "table" {
					mark = text.to_utf8().first() ?? 124
					if attribute($attrs, "format") == "" and mark == 44 {
						$attrs = $attrs.append({ name: "format", value: "csv" })
					}
					else if attribute($attrs, "format") == "" and mark == 58 {
						$attrs = $attrs.append({ name: "format", value: "dsv" })
					}
					if attribute($attrs, "separator") == "" and has_attr(line.attrs, "__table-cell-context") {
						$attrs = $attrs.append({ name: "separator", value: "!" })
					}
					if AsciiDocAttributes.source_only(line.attrs).is_err() {
						$attrs = $attrs.concat(line.attrs.map(|a| { name: "__document:${a.name}", value: a.value }))
					}
				}
				$rest = body.rest
				if kind != "comment" {
					context = if (kind == "table") "table" else if (["NOTE", "TIP", "IMPORTANT", "WARNING", "CAUTION"].contains(style)) "admonition" else if (["stem", "asciimath", "latexmath"].contains(style)) "stem" else if ["listing", "literal", "open"].contains(kind) and ["source", "listing", "literal"].contains(style) {
						if (style == "source") "listing" else style
					} else if (style == "verse") "verse" else if kind == "open" and ["source", "listing", "literal", "pass", "quote", "sidebar", "example"].contains(style) {
						if (style == "source") "listing" else style
					} else kind
					compound = ["example", "sidebar", "quote", "open", "admonition"].contains(context)
					child = if (compound) parse_blocks_cached(body.lines, 99, $attribute_cache) else { blocks: [], rest: [], cache: $attribute_cache }
					$attribute_cache = child.cache
					children = child.blocks
					if kind == "listing" and plain_attrs($attrs) and $title == "" and id == "" {
						$blocks = $blocks.append(Listing(normalize_indent_cached(body.source, $attrs, indexed.view)))
					}
					else if kind == "literal" and plain_attrs($attrs) and $title == "" and id == "" {
						$blocks = $blocks.append(Literal(normalize_indent_cached(body.source, $attrs, indexed.view)))
					}
					else {
						$blocks = $blocks.append(rich(context, if (compound) "" else normalize_indent_cached(body.source, $attrs, indexed.view), $title, style, id, $attrs, children, 0))
					}
				}
			} else if (text.to_utf8().len() >= 3 and text.to_utf8().all(|b| b == 39)) or text == "---" or text == "***" {
				$blocks = $blocks.append(ThematicBreak)
				$rest = tail
			}
			else if text.to_utf8().len() >= 3 and text.to_utf8().all(|b| b == 60) {
				$blocks = $blocks.append(rich("page_break", "", $title, style, id, $attrs, [], 0))
				$rest = tail
			}
			else if text.starts_with("toc::[") {
				$blocks = $blocks.append(rich("toc", "", $title, style, id, $attrs, [], 0))
				$rest = tail
			}
			else if list_marker(text).kind != "" {
				marker = list_marker(text)
				list = parse_list($rest, marker.kind, marker.level, [])
				inferred = implicit_list_style(text)
				list_blocks = if (style == "bibliography") list.blocks.map(bibliography_item) else list.blocks
				$blocks = $blocks.append(rich(marker.kind, "", $title, if (style == "") inferred else style, id, if (style == "" and inferred != "") $attrs.append({ name: "__implicit-list-style", value: "" }) else $attrs, list_blocks, marker.level))
				$rest = list.rest
			} else if valid_media(text) {
				context = if (text.starts_with("image::")) "image" else if (text.starts_with("video::")) "video" else "audio"
				value = drop_text(text, context.to_utf8().len() + 2)
				bits = value.split_on("[")
				target = bits.first() ?? value
				inline_attrs = if (bits.len() > 1) parse_media_metadata("[${Str.join_with(bits.drop_first(1), "[")}") else AsciiDocAttributes.flat([])
				$blocks = $blocks.append(rich(context, target, $title, style, id, $attrs.concat(inline_attrs), [], 0))
				$rest = tail
			} else {
				var $body = [text]
				opaque_paragraph = ["literal", "source", "listing", "pass", "verse"].contains(style)
				$rest = tail
				while !$rest.is_empty() and ($rest.first() ?? empty_line).text != "" and (opaque_paragraph or delimiter(($rest.first() ?? empty_line).text) == "") and (opaque_paragraph or !(is_block_metadata(($rest.first() ?? empty_line).text))) and (opaque_paragraph or ($rest.first() ?? empty_line).text != "+") {
					if opaque_paragraph or !(($rest.first() ?? empty_line).text.starts_with("//")) {
						$body = $body.append(($rest.first() ?? empty_line).text)
					}
					$rest = $rest.drop_first(1)
				}
				source = Str.join_with($body, "\n")
				var $admonition = ""
				for name in ["NOTE", "TIP", "WARNING", "IMPORTANT", "CAUTION"] {
					if source.starts_with("${name}: ") {
						$admonition = name
					}
				}
				if $admonition == "" and ["NOTE", "TIP", "WARNING", "IMPORTANT", "CAUTION"].contains(style) {
					$admonition = style
				}
				if $admonition != "" {
					$blocks = $blocks.append(rich("admonition", if (source.starts_with("${$admonition}: ")) drop_text(source, $admonition.to_utf8().len() + 2).trim() else source, $title, $admonition, id, $attrs, [], 0))
				}
				else if text.starts_with(" ") or text.starts_with("\t") {
					$blocks = $blocks.append(Literal(dedent(source)))
				}
				else if ["quote", "verse", "sidebar", "example", "pass"].contains(style) {
					$blocks = $blocks.append(rich(style, source, $title, style, id, $attrs, [], 0))
				}
				else if style == "literal" {
					$blocks = $blocks.append(rich("literal", source, $title, style, id, $attrs, [], 0))
				}
				else if style == "source" or style == "listing" {
					$blocks = $blocks.append(rich("listing", source, $title, style, id, $attrs, [], 0))
				}
				else if plain_attrs($attrs) and $title == "" and id == "" and !source.contains("{") {
					$blocks = $blocks.append(Paragraph(source))
				}
				else {
					$blocks = $blocks.append(rich("paragraph", source, $title, style, id, $attrs, [], 0))
				}
			}
			$attrs = AsciiDocAttributes.flat([])
			$title = ""
			$id = ""
		}
	}
	{ blocks: $blocks, rest: $rest, cache: $attribute_cache }
}

parse_list : List(Line), Str, U64, List(Str) -> { blocks : List(AsciiDoc.Block), rest : List(Line) }
parse_list = |lines, kind, level, ancestors| {
	var $rest = lines
	var $items = []
	var $done = False
	while !$done and !$rest.is_empty() {
		line = $rest.first() ?? empty_line
		text = line.text
		marker = list_marker(text)
		if marker.kind == kind and marker.level == level {
			$rest = $rest.drop_first(1)
			var $body = marker.text
			var $children = []
			var $item_done = False
			while !$item_done and !$rest.is_empty() {
				next = ($rest.first() ?? empty_line).text
				m = list_marker(next)
				if next == "//-" {
					$item_done = True
					$done = True
				} else if next.starts_with("//") and delimiter(next) != "comment" {
					$rest = $rest.drop_first(1)
				} else if m.kind != "" and ancestors.contains("${m.kind}:${m.level.to_str()}") {
					$item_done = True
					$done = True
				} else if m.kind != "" and (m.level != level or m.kind != kind) {
					nested = parse_list($rest, m.kind, m.level, ancestors.append("${kind}:${level.to_str()}"))
					$children = $children.append(rich(m.kind, "", "", "", "", AsciiDocAttributes.flat([]), nested.blocks, m.level))
					$rest = nested.rest
					if list_marker(($rest.first() ?? empty_line).text).kind == "" and ($rest.first() ?? empty_line).text != "+" {
						$item_done = True
						$done = True
					}
				} else if next == "+" {
					attached = take_attached($rest.drop_first(1))
					continuation = parse_blocks(attached.lines, 99)
					$children = $children.concat(continuation.blocks)
					$rest = attached.rest
				} else if kind == "dlist" and $body == "" and next.starts_with(" ") {
					$body = next.trim()
					$rest = $rest.drop_first(1)
				} else if next == "" {
					while !$rest.is_empty() and ($rest.first() ?? empty_line).text == "" {
						$rest = $rest.drop_first(1)
					}
					if ($rest.first() ?? empty_line).text.starts_with(" ") or ($rest.first() ?? empty_line).text.starts_with("\t") {
						literal = take_indented($rest)
						$children = $children.append(Literal(dedent(literal.source)))
						$rest = literal.rest
						if list_marker(($rest.first() ?? empty_line).text).kind == "" and ($rest.first() ?? empty_line).text != "+" {
							$item_done = True
							$done = True
						}
					} else if list_marker(($rest.first() ?? empty_line).text).kind == "" and ($rest.first() ?? empty_line).text != "+" {
						$item_done = True
						$done = True
					}
				} else if m.kind != "" or heading(next).level > 0 or delimiter(next) != "" {
					$item_done = True
				}
				else {
					$body = if ($body == "") next.trim() else "${$body}\n${next.trim()}"
					$rest = $rest.drop_first(1)
				}
			}
			$items = $items.append(rich("list_item", $body, marker.term, "", "", line.attrs, $children, 0))
		} else {
			$done = True
		}
	}
	{ blocks: $items, rest: $rest }
}

escape : Str -> Str
escape = |s| s.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;")

inline : Str -> Str
inline = |s| AsciiDocInline.render(s, [])

render_blocks : List(AsciiDoc.Block) -> Str
render_blocks = |blocks| Str.join_with(blocks.map(render_block), "\n")

render_block : AsciiDoc.Block -> Str
render_block = |block| {
	match block {
		Rich(n) if n.context == "\u(0)prepared-paragraph" => return "<div${if (n.id == "") "" else " id=\"${escape(n.id)}\""} class=\"paragraph${role_suffix(n.attrs)}\">\n${if (n.title == "") "" else "<div class=\"title\">${n.title}</div>\n"}<p>${n.source}</p>\n</div>"
		Rich(n) if n.context == "__document_header" => return ""
		_ => {}
	}
	match block {
		Paragraph(source) => "<div class=\"paragraph\">\n<p>${inline(source)}</p>\n</div>"
		Listing(source) => "<div class=\"listingblock\">\n<div class=\"content\">\n<pre>${callouts(source)}</pre>\n</div>\n</div>"
		Literal(source) => "<div class=\"literalblock\">\n<div class=\"content\">\n<pre>${callouts(source)}</pre>\n</div>\n</div>"
		ThematicBreak => "<hr>"
		Section({ level, title, blocks }) => render_section(level, title, "", blocks)
		Rich(node) => {
			wrapper = ["paragraph", "listing", "literal", "open", "sidebar", "example", "quote", "verse", "stem", "admonition"].contains(node.context)
			title = if (node.title == "" or !wrapper) "" else "<div class=\"title\">${rendered_title(node.title, node.attrs)}</div>\n"
			body = if (["open", "sidebar", "example", "quote", "admonition", "ulist", "olist", "colist", "list_item"].contains(node.context)) render_blocks(node.blocks) else ""
			source = if (["paragraph", "list_item", "verse", "admonition", "quote", "example", "sidebar", "open"].contains(node.context)) AsciiDocBlockHtml.paragraph_indexed(node.source, AsciiDocAttributes.indexed_attributes(node.attrs, AsciiDocAttributes.empty).view) else ""
			checklist = node.context == "ulist" and node.blocks.any(
				|b| match b {
					Rich(n) => n.source.starts_with("[x] ") or n.source.starts_with("[ ] ") or n.source.starts_with("[*] ")
					_ => False
				},
			)
			id = if (node.id == "" or !wrapper) "" else " id=\"${escape(node.id)}\""
			match node.context {
				"section" => render_section_with(node.level, node.title, node.id, node.blocks, node.attrs)
				"floating_title" => "<h${(node.level + 1).to_str()}${if (node.id == "") "" else " id=\"${escape(node.id)}\""} class=\"discrete\">${rendered_title(node.title, node.attrs)}</h${(node.level + 1).to_str()}>"
				"paragraph" => "<div${id} class=\"paragraph${role_suffix(node.attrs)}\">\n${title}<p>${source}</p>\n</div>"
				"listing" | "literal" => {
					context = node.context
					language = attribute(node.attrs, "2")
					pre = if (node.style == "source") "<pre class=\"highlight${if (attribute(node.attrs, "options").split_on(",").contains("nowrap")) " nowrap" else ""}\"><code${if (language == "") "" else " class=\"language-${language}\" data-lang=\"${language}\""}>${listing_content(node.source, node.attrs)}</code></pre>" else "<pre${if (attribute(node.attrs, "options").split_on(",").contains("nowrap")) " class=\"nowrap\"" else ""}>${listing_content(node.source, node.attrs)}</pre>"
					"<div${id} class=\"${context}block\">\n${title}<div class=\"content\">\n${pre}\n</div>\n</div>"
				}
				"open" => "<div${id} class=\"openblock${if (node.style == "partintro") " partintro" else ""}${role_suffix(node.attrs)}\">\n${title}<div class=\"content\">\n${if (node.blocks.is_empty()) source else body}\n</div>\n</div>"
				"sidebar" => "<div${id} class=\"sidebarblock\">\n<div class=\"content\">\n${title}${if (node.blocks.is_empty()) source else body}\n</div>\n</div>"
				"example" => "<div${id} class=\"exampleblock\">\n${title}<div class=\"content\">\n${if (node.blocks.is_empty()) source else body}\n</div>\n</div>"
				"quote" => "<div${id} class=\"quoteblock${role_suffix(node.attrs)}\">\n${title}<blockquote>\n${if (node.blocks.is_empty()) source else body}\n</blockquote>${attribution(node.attrs)}\n</div>"
				"verse" => "<div${id} class=\"verseblock${role_suffix(node.attrs)}\">\n${title}<pre class=\"content\">${source}</pre>${attribution(node.attrs)}\n</div>"
				"toc" => attribute(node.attrs, "__toc-html")
				"stem" => "<div${id} class=\"stemblock${role_suffix(node.attrs)}\">\n${title}<div class=\"content\">\n${if (node.style == "latexmath" or attribute(node.attrs, "stem") == "latexmath") "\\[${escape(node.source)}\\]" else "\\$${escape(node.source)}\\$"}\n</div>\n</div>"
				"pass" => node.source
				"thematic_break" => "<hr>"
				"page_break" => "<div style=\"page-break-after: always;\"></div>"
				"admonition" => "<div${id} class=\"admonitionblock ${lower(node.style)}${role_suffix(node.attrs)}\">\n<table>\n<tr>\n<td class=\"icon\">\n<div class=\"title\">${capitalize(node.style)}</div>\n</td>\n<td class=\"content\">\n${title}${if (node.blocks.is_empty()) source else body}\n</td>\n</tr>\n</table>\n</div>"
				"ulist" | "olist" | "colist" => AsciiDocBlockHtml.list_wrapper(node.context, node.style, node.level, node.id, rendered_title(node.title, node.attrs), AsciiDocAttributes.materialize(node.attrs), body, checklist)
				"list_item" => "<li>\n<p>${if (node.source.starts_with("[x] ") or node.source.starts_with("[*] ")) "&#10003; ${if (has_attr(node.attrs, "__rendered-source")) source else inline(drop_text(node.source, 4))}" else if (node.source.starts_with("[ ] ")) "&#10063; ${if (has_attr(node.attrs, "__rendered-source")) source else inline(drop_text(node.source, 4))}" else source}</p>${if (body == "") "" else "\n${body}"}\n</li>"
				"dlist" => AsciiDocBlockHtml.description(
					node.style,
					node.id,
					rendered_title(node.title, node.attrs),
					AsciiDocAttributes.materialize(node.attrs),
					node.blocks.map(
						|item| {
							n = render_node(item)
							{ term: rendered_title(n.title, n.attrs), text: rendered_source(n.source, n.attrs), body: render_blocks(n.blocks) }
						},
					),
				)
				"image" | "video" | "audio" => AsciiDocMedia.render({ context: node.context, target: node.source, title: rendered_title(node.title, node.attrs), id: node.id, attrs: AsciiDocAttributes.materialize(node.attrs) })
				"table" => if (has_attr(node.attrs, "__rendered-table")) attribute(node.attrs, "__rendered-table") else AsciiDocTable.render_with(node.source, AsciiDocAttributes.materialize(put_attr(node.attrs, "title", node.title)), |text| render_blocks(parse_blocks(make_lines(text), 0).blocks))
				_ => source
			}
		}
	}
}

render_ditem : AsciiDoc.Block -> Str
render_ditem = |item| match item {
	Rich(n) => "<dt class=\"hdlist1\">${rendered_title(n.title, n.attrs)}</dt>\n<dd>\n<p>${rendered_source(n.source, n.attrs)}</p>\n${if (n.blocks.is_empty()) "" else "${render_blocks(n.blocks)}\n"}</dd>"
	_ => ""
}

capitalize : Str -> Str
capitalize = |s| {
	bytes = lower(s).to_utf8()
	match bytes {
		[b, .. as rest] => Str.from_utf8_lossy([if (b >= 'a' and b <= 'z') b - 32 else b].concat(rest))
		[] => ""
	}
}

section_id : Str -> Str
section_id = |s| {
	var $bytes = [95.U8]
	var $emoji = 0.U64
	for byte in lower(s).to_utf8() {
		if $emoji > 0 {
			$emoji = $emoji - 1
		}
		else if byte >= 240 {
			$emoji = 3
		}
		else if byte == ' ' {
			$bytes = $bytes.append(95)
		}
		else if (byte >= 'a' and byte <= 'z') or (byte >= '0' and byte <= '9') or byte == '_' or byte >= 128 {
			$bytes = $bytes.append(byte)
		}
	}
	var $result = Str.from_utf8_lossy($bytes)
	while $result.ends_with("_") and $result != "_" {
		$result = Str.from_utf8_lossy($result.to_utf8().drop_last(1))
	}
	$result
}

render_section : U64, Str, Str, List(AsciiDoc.Block) -> Str
render_section = |level, title, id, blocks| {
	render_section_with(level, title, if (id == "") section_id(title) else id, blocks, AsciiDocAttributes.flat([]))
}

render_section_with : U64, Str, Str, List(AsciiDoc.Block), AsciiDoc.Attributes -> Str
render_section_with = |level, title, sid, blocks, attrs| {
	body = render_blocks(blocks)
	heading_title = "${attribute(attrs, "__heading-number")}${rendered_title(title, attrs)}"
	heading_link = if (sid != "" and has_attr(attrs, "sectlinks")) "<a class=\"link\" href=\"#${escape(sid)}\">${heading_title}</a>" else heading_title
	anchor = if (sid != "" and has_attr(attrs, "sectanchors")) "<a class=\"anchor\" href=\"#${escape(sid)}\"></a>" else ""
	heading_html = "<h${(level + 1).to_str()}${if (sid == "" and !has_attr(attrs, "__generated-id")) "" else " id=\"${escape(sid)}\""}${if (level == 0) " class=\"sect0\"" else ""}>${anchor}${heading_link}</h${(level + 1).to_str()}>"
	if level == 0 {
		"${heading_html}\n${body}"
	}
	else if level == 1 {
		"<div class=\"sect1\">\n${heading_html}\n<div class=\"sectionbody\">\n${body}\n</div>\n</div>"
	}
	else {
		"<div class=\"sect${level.to_str()}\">\n${heading_html}\n${body}\n</div>"
	}
}

expect AsciiDoc.parse_str("") == Ok({ title: "", blocks: [] })
expect AsciiDoc.parse_str("= Title\nAuthor\n\nText") == Ok({ title: "Title", blocks: [Paragraph("Text")] })
expect AsciiDoc.parse_str("----\nx") == Ok({ title: "", blocks: [Listing("x")] })

replace : Str, Str, Str -> Str
replace = |s, from, to| Str.join_with(s.split_on(from), to)

lower : Str -> Str
lower = |s| Str.from_utf8_lossy(s.to_utf8().map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b))

following_heading : List(Line) -> { level : U64, title : Str }
following_heading = |lines| {
	var $rest = lines
	while !$rest.is_empty() and (($rest.first() ?? empty_line).text.starts_with("[") or ($rest.first() ?? empty_line).text.starts_with(".") or ($rest.first() ?? empty_line).text.starts_with("//")) {
		$rest = $rest.drop_first(1)
	}
	line = $rest.first() ?? empty_line
	adjusted_heading(line.text, line.attrs)
}

callouts : Str -> Str
callouts = |source| {
	var $out = escape(source)
	var $auto = 1.U64
	while $out.contains("&lt;.&gt;") and $auto < 100 {
		parts = $out.split_on("&lt;.&gt;")
		$out = "${parts.first() ?? ""}<b class=\"conum\">(${$auto.to_str()})</b>${Str.join_with(parts.drop_first(1), "&lt;.&gt;")}"
		$auto = $auto + 1
	}
	var $n = 1.U64
	while $n <= 99 {
		$out = replace($out, "&lt;${$n.to_str()}&gt;", "<b class=\"conum\">(${$n.to_str()})</b>")
		$n = $n + 1
	}
	$out
}

listing_content : Str, AsciiDoc.Attributes -> Str
listing_content = |source, attrs| AsciiDocBlockHtml.verbatim_indexed(source, AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view)

split_attributes : Str -> List(Str)
split_attributes = |text| {
	bytes = text.to_utf8()
	var $parts = []
	var $part = []
	var $index = 0.U64
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		prefix = if (byte == 34 or byte == 39) Str.from_utf8_lossy($part).trim() else ""
		if (byte == 34 or byte == 39) and (prefix == "" or prefix.ends_with("=")) {
			var $closing = $index + 1
			while $closing < bytes.len() and !(bytes.get($closing) == Ok(byte) and bytes.get($closing - 1) != Ok(92)) {
				$closing = $closing + 1
			}
			if $closing < bytes.len() {
				$part = $part.concat(bytes.drop_first($index).take_first($closing - $index + 1))
				$parts = $parts.append(Str.from_utf8_lossy($part))
				$part = []
				$index = $closing + 1
				while bytes.get($index) == Ok(32) or bytes.get($index) == Ok(9) {
					$index = $index + 1
				}
				if bytes.get($index) == Ok(44) {
					$index = $index + 1
				}
			} else {
				$part = $part.append(byte)
				$index = $index + 1
			}
		} else if byte == 44 {
			$parts = $parts.append(Str.from_utf8_lossy($part))
			$part = []
			$index = $index + 1
		} else {
			$part = $part.append(byte)
			$index = $index + 1
		}
	}
	if !$part.is_empty() or $parts.is_empty() or bytes.last() == Ok(44) {
		$parts = $parts.append(Str.from_utf8_lossy($part))
	}
	$parts
}

take_attached : List(Line) -> { lines : List(Line), rest : List(Line) }
take_attached = |lines| {
	var $rest = lines
	while !$rest.is_empty() and ($rest.first() ?? empty_line).text == "" {
		$rest = $rest.drop_first(1)
	}
	var $body = []
	while !$rest.is_empty() and (($rest.first() ?? empty_line).text.starts_with("[") or ($rest.first() ?? empty_line).text.starts_with(".")) and delimiter(($rest.first() ?? empty_line).text) == "" and list_marker(($rest.first() ?? empty_line).text).kind == "" {
		$body = $body.append($rest.first() ?? empty_line)
		$rest = $rest.drop_first(1)
	}
	text = ($rest.first() ?? empty_line).text
	if delimiter(text) != "" {
		$body = $body.append($rest.first() ?? empty_line)
		$rest = $rest.drop_first(1)
		var $closed = False
		while !$closed and !$rest.is_empty() {
			line = $rest.first() ?? empty_line
			$body = $body.append(line)
			$rest = $rest.drop_first(1)
			if line.text == text {
				$closed = True
			}
		}
	} else {
		while !$rest.is_empty() and ($rest.first() ?? empty_line).text != "" and list_marker(($rest.first() ?? empty_line).text).kind == "" {
			$body = $body.append($rest.first() ?? empty_line)
			$rest = $rest.drop_first(1)
		}
	}
	{ lines: $body, rest: $rest }
}

role_suffix : AsciiDoc.Attributes -> Str
role_suffix = |attrs| if (attribute(attrs, "role") == "") "" else " ${attribute(attrs, "role")}"

attribution : AsciiDoc.Attributes -> Str
attribution = |attrs| {
	author = attribute(attrs, "2")
	cite = attribute(attrs, "3")
	if (author == "" and cite == "") "" else "\n<div class=\"attribution\">\n${if (author == "") "" else "&#8212; ${inline(author)}"}${if (cite == "") "" else "${if (author == "") "" else "<br>\n"}<cite>${inline(cite)}</cite>"}\n</div>"
}

list_style : Str, U64 -> Str
list_style = |style, level| {
	if (style != "") style else match level {
		2 => "loweralpha"
		3 => "lowerroman"
		4 => "upperalpha"
		5 => "upperroman"
		_ => "arabic"
	}
}

leading_spaces : Str -> U64
leading_spaces = |text| {
	var $n = 0.U64
	while text.to_utf8().get($n) == Ok(32) or text.to_utf8().get($n) == Ok(9) {
		$n = $n + 1
	}
	$n
}

dedent : Str -> Str
dedent = |source| {
	lines = source.split_on("\n")
	var $n = 999999.U64
	for line in lines {
		if line != "" {
			n = leading_spaces(line)
			if n < $n {
				$n = n
			}
		}
	}
	Str.join_with(lines.map(|line| drop_text(line, if ($n == 999999) 0 else $n)), "\n")
}

normalize_indent : Str, AsciiDoc.Attributes -> Str
normalize_indent = |source, attrs| normalize_indent_value(source, attribute(attrs, "indent"))

normalize_indent_cached : Str, AsciiDoc.Attributes, AsciiDocAttributes.View -> Str
normalize_indent_cached = |source, attrs, view| normalize_indent_value(source, cached_attribute(attrs, view, "indent"))

normalize_indent_value : Str, Str -> Str
normalize_indent_value = |source, indent| {
	match U64.from_str(indent) {
		Ok(n) => Str.join_with(dedent(source).split_on("\n").map(|line| "${spaces(n)}${line}"), "\n")
		Err(_) => source
	}
}

spaces : U64 -> Str
spaces = |n| {
	var $out = ""
	var $i = 0.U64
	while $i < n and $i < 1024 {
		$out = "${$out} "
		$i = $i + 1
	}
	$out
}

render_horizontal : AsciiDoc.Block -> Str
render_horizontal = |block| match block {
	Rich(n) => "<tr>\n<td class=\"hdlist1\">\n${inline(n.title)}\n</td>\n<td class=\"hdlist2\">\n<p>${inline(n.source)}</p>\n</td>\n</tr>"
	_ => ""
}

size_attrs : AsciiDoc.Attributes -> Str
size_attrs = |attrs| {
	w = if (attribute(attrs, "width") == "") attribute(attrs, "2") else attribute(attrs, "width")
	h = if (attribute(attrs, "height") == "") attribute(attrs, "3") else attribute(attrs, "height")
	"${if (w == "") "" else " width=\"${escape(w)}\""}${if (h == "") "" else " height=\"${escape(h)}\""}"
}

media_attrs : AsciiDoc.Attributes -> Str
media_attrs = |attrs| {
	options = attribute(attrs, "options").split_on(",")
	"${size_attrs(attrs)}${if (attribute(attrs, "poster") == "") "" else " poster=\"${escape(attribute(attrs, "poster"))}\""}${if (options.contains("autoplay")) " autoplay" else ""}${if (options.contains("loop")) " loop" else ""}${if (options.contains("muted")) " muted" else ""}"
}

plain_attrs : AsciiDoc.Attributes -> Bool
plain_attrs = |attrs| {
	defaults = AsciiDocPreprocess.standard
	if attrs.is_empty() {
		return True
	}
	if attrs.len() != defaults.len() {
		return False
	}
	match AsciiDocAttributes.classify_baseline(attrs, defaults) {
		Ok(standard) => standard
		Err(_) => AsciiDocAttributes.materialize(attrs) == defaults or attrs.all(|a| defaults.contains(a))
	}
}

list_type : Str -> Str
list_type = |style| match style {
	"loweralpha" => " type=\"a\""
	"upperalpha" => " type=\"A\""
	"lowerroman" => " type=\"i\""
	"upperroman" => " type=\"I\""
	_ => ""
}

RenderNode : { context : Str, source : Str, title : Str, style : Str, id : Str, attrs : AsciiDoc.Attributes, blocks : List(AsciiDoc.Block), level : U64 }

# Private rendering data keeps cached HTML and generated identity metadata
# outside the caller's immutable source attribute lists.
PreparedBlock := [Prepared({ node : RenderNode, view : AsciiDocAttributes.View, source_html : Str, title_html : Str, table_html : Str, number : Str, caption_number : Str, generated : Bool, blocks : List(PreparedBlock) })]

IdentityState : { ids : Dict(Str, Bool), refs : List({ id : Str, text : Str }), numbers : List(U64), examples : U64, figures : U64, tables : U64, toc : Bool }

render_node : AsciiDoc.Block -> RenderNode
render_node = |block| render_node_with_attributes(block, AsciiDocAttributes.flat(AsciiDocPreprocess.standard))

# Construct once at a rendering traversal boundary, then share across all
# simple variants. Rich nodes always retain their own source snapshot.
standard_attributes : {} -> AsciiDoc.Attributes
standard_attributes = |_| AsciiDocAttributes.standard_scope(AsciiDocPreprocess.standard)

render_node_with_attributes : AsciiDoc.Block, AsciiDoc.Attributes -> RenderNode
render_node_with_attributes = |block, standard| match block {
	Rich(node) => node
	Paragraph(source) => { context: "paragraph", source, title: "", style: "", id: "", attrs: standard, blocks: [], level: 0 }
	Listing(source) => { context: "listing", source, title: "", style: "", id: "", attrs: standard, blocks: [], level: 0 }
	Literal(source) => { context: "literal", source, title: "", style: "", id: "", attrs: standard, blocks: [], level: 0 }
	ThematicBreak => { context: "thematic_break", source: "", title: "", style: "", id: "", attrs: standard, blocks: [], level: 0 }
	Section({ level, title, blocks }) => { context: "section", source: "", title, style: "", id: "", attrs: standard, blocks, level }
}

has_attr : AsciiDoc.Attributes, Str -> Bool
has_attr = |attrs, name| lookup_attribute(attrs, name).is_ok()

put_attr : AsciiDoc.Attributes, Str, Str -> AsciiDoc.Attributes
put_attr = |attrs, name, value| attrs.keep_if(|a| a.name != name).append({ name, value })

rendered_source : Str, AsciiDoc.Attributes -> Str
rendered_source = |source, attrs| match lookup_attribute(attrs, "__rendered-source") {
	Ok(html) => html
	Err(_) => AsciiDocInline.render(source, AsciiDocAttributes.materialize(attrs))
}

rendered_title : Str, AsciiDoc.Attributes -> Str
rendered_title = |title, attrs| match lookup_attribute(attrs, "__rendered-title") {
	Ok(html) => html
	Err(_) => AsciiDocInline.render(title, AsciiDocAttributes.materialize(attrs))
}

identify_blocks : List(AsciiDoc.Block), IdentityState -> { blocks : List(PreparedBlock), state : IdentityState }
identify_blocks = |blocks, state| {
	result = identify_blocks_cached(blocks, state, AsciiDocAttributes.empty, standard_attributes({}))
	{ blocks: result.blocks, state: result.state }
}

identify_blocks_cached : List(AsciiDoc.Block), IdentityState, AsciiDocAttributes.Cache, AsciiDoc.Attributes -> { blocks : List(PreparedBlock), state : IdentityState, cache : AsciiDocAttributes.Cache }
identify_blocks_cached = |blocks, state, cache, standard| {
	var $attribute_cache = cache
	var $state = state
	var $blocks = []
	for block in blocks {
		node = render_node_with_attributes(block, standard)
		indexed = AsciiDocAttributes.indexed_attributes(node.attrs, $attribute_cache)
		$attribute_cache = indexed.cache
		view = indexed.view
		if node.context == "toc" {
			$state = { ..$state, toc: True }
		}
		var $number = cached_attribute(node.attrs, view, "__heading-number")
		var $caption_number = cached_attribute(node.attrs, view, "__caption-number")
		var $generated = cached_has_attribute(node.attrs, view, "__generated-id")
		var $id = node.id
		if node.context == "section" or node.context == "floating_title" {
			if $id == "" and cached_has_attribute(node.attrs, view, "sectids") {
				base = generated_id_cached(node.title, node.attrs, view)
				separator = if (cached_has_attribute(node.attrs, view, "idseparator")) cached_attribute(node.attrs, view, "idseparator") else "_"
				$id = base
				var $suffix = 2.U64
				while $state.ids.contains($id) {
					$id = "${base}${separator}${$suffix.to_str()}"
					$suffix = $suffix + 1
				}
				# Only an empty generated ID needs to be distinguished from an
				# explicitly disabled section ID by the HTML renderer.
				if $id == "" {
					$generated = True
				}
			}
			if node.context == "section" and node.level > 0 and node.level <= 6 {
				var $numbers = $state.numbers
				if cached_has_attribute(node.attrs, view, "sectnums") and node.style != "unnumbered" {
					$numbers = $numbers.set(node.level - 1, ($numbers.get(node.level - 1) ?? 0) + 1) ?? $numbers
					var $index = node.level
					while $index < $numbers.len() {
						$numbers = $numbers.set($index, 0) ?? $numbers
						$index = $index + 1
					}
					if node.level <= (U64.from_str(cached_attribute(node.attrs, view, "sectnumlevels")) ?? 3) {
						$number = "${Str.join_with($numbers.take_first(node.level).map(U64.to_str), ".")}. "
					}
				}
				$state = { ..$state, numbers: $numbers }
			}
		}
		if $id != "" {
			reftext = if (cached_has_attribute(node.attrs, view, "reftext")) cached_attribute(node.attrs, view, "reftext") else node.title
			$state = { ..$state, ids: $state.ids.insert($id, True), refs: $state.refs.append({ id: $id, text: reftext }) }
		}
		if node.title != "" {
			if node.context == "example" {
				$state = { ..$state, examples: $state.examples + 1 }
				$caption_number = $state.examples.to_str()
			}
			if node.context == "image" {
				$state = { ..$state, figures: $state.figures + 1 }
				$caption_number = $state.figures.to_str()
			}
			if node.context == "table" {
				$state = { ..$state, tables: $state.tables + 1 }
				$caption_number = $state.tables.to_str()
			}
		}
		children = identify_blocks_cached(node.blocks, $state, $attribute_cache, standard)
		$attribute_cache = children.cache
		$state = children.state
		$blocks = $blocks.append(Prepared({ node: { ..node, id: $id }, view, source_html: "", title_html: "", table_html: "", number: $number, caption_number: $caption_number, generated: $generated, blocks: children.blocks }))
	}
	{ blocks: $blocks, state: $state, cache: $attribute_cache }
}

# Ordinary words and spaces render identically for every attribute snapshot.
# Keep caching syntax-bearing titles, even if their first rendering happened
# to preserve the source, since a fresh renderer has different dynamic state.
plain_title : Str -> Bool
plain_title = |title| title.to_utf8().all(|byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == ' ' or byte >= 128)

prepare_blocks : List(PreparedBlock), AsciiDocInline.State -> { blocks : List(PreparedBlock), state : AsciiDocInline.State }
prepare_blocks = |blocks, state| prepare_blocks_with_standard(blocks, state, standard_attributes({}))

prepare_blocks_with_standard : List(PreparedBlock), AsciiDocInline.State, AsciiDoc.Attributes -> { blocks : List(PreparedBlock), state : AsciiDocInline.State }
prepare_blocks_with_standard = |blocks, state, standard| {
	var $state = state
	var $blocks = []
	for Prepared(p) in blocks {
		node = p.node
		view = p.view
		var $title_html = ""
		var $source_html = ""
		var $table_html = ""
		if node.title != "" {
			title = render_cached_inline(node.title, node.attrs, view, "normal", $state)
			$title_html = title.html
			$state = title.state
		}
		if node.title != "" {
			caption_kind = if (node.context == "example") "example" else if (node.context == "image") "figure" else if (node.context == "table") "table" else ""
			caption = if (caption_kind == "") "" else if (cached_has_attribute(node.attrs, view, "caption")) cached_attribute(node.attrs, view, "caption") else if (cached_has_attribute(node.attrs, view, "${caption_kind}-caption")) "${cached_attribute(node.attrs, view, "${caption_kind}-caption")} ${p.caption_number}. " else ""
			$title_html = "${caption}${$title_html}"
		}
		if node.source != "" and ["paragraph", "list_item", "verse", "admonition", "quote", "example", "sidebar", "open"].contains(node.context) {
			subs = if (cached_has_attribute(node.attrs, view, "subs")) cached_attribute(node.attrs, view, "subs") else "normal"
			render_source = if (node.context == "list_item" and (node.source.starts_with("[x] ") or node.source.starts_with("[*] ") or node.source.starts_with("[ ] "))) drop_text(node.source, 4) else node.source
			rendered = render_cached_inline(render_source, node.attrs, view, subs, $state)
			$state = rendered.state
			$source_html = if (cached_attribute(node.attrs, view, "options").split_on(",").contains("hardbreaks") or cached_has_attribute(node.attrs, view, "hardbreaks-option") or cached_has_attribute(node.attrs, view, "hardbreaks")) rendered.html.replace_each("\n", "<br>\n") else rendered.html
		}
		if node.context == "table" {
			inherited = table_cell_attributes(node.attrs)
			inherited_index = AsciiDocAttributes.indexed_attributes(inherited, AsciiDocAttributes.empty)
			plain_snapshot = node.attrs
			plain_index = AsciiDocAttributes.indexed_attributes(plain_snapshot, AsciiDocAttributes.empty)
			cell_nofootnotes = cached_has_attribute(node.attrs, view, "nofootnotes")
			table = AsciiDocTable.render_view_state(
				node.source,
				AsciiDocAttributes.with_after(view, [{ name: "__rendered-title", value: $title_html }, { name: "title", value: node.title }, { name: "id", value: node.id }]),
				$state,
				|text, cell_state| render_cached_inline(text, plain_snapshot, plain_index.view, "normal", cell_state),
				|text, cell_state| {
					{ dynamic_tags_possible, trusted_snapshot_token, trusted_snapshot_generation, snapshot_before, snapshot_after, emitted_footnotes: prior_emitted, footnotes, note_index, indexed_note_count, refs, ref_index, indexed_ref_count, attributes, removed, snapshot, snapshot_index, tag_prefix } = cell_state
					start = footnotes.len()
					child_state = { dynamic_tags_possible, trusted_snapshot_token, trusted_snapshot_generation, snapshot_before, snapshot_after, emitted_footnotes: [], footnotes, note_index, indexed_note_count, refs, ref_index, indexed_ref_count, attributes, removed, snapshot, snapshot_index, tag_prefix }
					lines = text.split_on("\n").map(|line| { text: line, attrs: inherited })
					parsed = parse_blocks_cached(lines, 98, inherited_index.cache)
					identified = identify_blocks_cached(parsed.blocks, { ids: refs.fold(Dict.empty(), |index, ref| index.insert(ref.id, True)), refs, numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False }, parsed.cache, standard)
					content = prepare_blocks_with_standard(identified.blocks, if (refs.len() == identified.state.refs.len()) child_state else AsciiDocInline.with_refs(child_state, identified.state.refs), standard)
					prepared_blocks = if (identified.state.toc) attach_prepared_toc(content.blocks, content.blocks) else content.blocks
					footer = footnote_footer_with(content.state, cell_nofootnotes, start)
					end = content.state.footnotes.len()
					next_state = { ..content.state, emitted_footnotes: [] }
					var $emitted = prior_emitted
					var $number = start
					while $number < end {
						$number = $number + 1
						$emitted = $emitted.append($number)
					}
					{ html: "${render_prepared_blocks(prepared_blocks)}${footer}", state: { ..next_state, emitted_footnotes: $emitted } }
				},
			)
			$state = table.state
			$table_html = table.html
		}
		children = prepare_blocks_with_standard(p.blocks, $state, standard)
		$state = children.state
		$blocks = $blocks.append(Prepared({ ..p, source_html: $source_html, title_html: $title_html, table_html: $table_html, blocks: children.blocks }))
	}
	{ blocks: $blocks, state: $state }
}

render_prepared_blocks : List(PreparedBlock) -> Str
render_prepared_blocks = |blocks| Str.join_with(blocks.map(render_prepared_block), "\n")

render_prepared_block : PreparedBlock -> Str
render_prepared_block = |Prepared(p)| {
	node = p.node
	get = |key| cached_attribute(node.attrs, p.view, key)
	role = if (get("role") == "") "" else " ${get("role")}"
	wrapper = ["paragraph", "listing", "literal", "open", "sidebar", "example", "quote", "verse", "stem", "admonition"].contains(node.context)
	title = if (node.title == "" or !wrapper) "" else "<div class=\"title\">${p.title_html}</div>\n"
	body = if (["open", "sidebar", "example", "quote", "admonition", "ulist", "olist", "colist", "list_item"].contains(node.context)) render_prepared_blocks(p.blocks) else ""
	id = if (node.id == "" or !wrapper) "" else " id=\"${escape(node.id)}\""
	match node.context {
		"__document_header" => ""
		"section" => render_prepared_section(p)
		"floating_title" => "<h${(node.level + 1).to_str()}${if (node.id == "") "" else " id=\"${escape(node.id)}\""} class=\"discrete\">${p.title_html}</h${(node.level + 1).to_str()}>"
		"paragraph" => "<div${id} class=\"paragraph${role}\">\n${title}<p>${p.source_html}</p>\n</div>"
		"listing" | "literal" => {
			language = get("2")
			content = AsciiDocBlockHtml.verbatim_indexed(node.source, p.view)
			pre = if (node.style == "source") "<pre class=\"highlight${if (get("options").split_on(",").contains("nowrap")) " nowrap" else ""}\"><code${if (language == "") "" else " class=\"language-${language}\" data-lang=\"${language}\""}>${content}</code></pre>" else "<pre${if (get("options").split_on(",").contains("nowrap")) " class=\"nowrap\"" else ""}>${content}</pre>"
			"<div${id} class=\"${node.context}block\">\n${title}<div class=\"content\">\n${pre}\n</div>\n</div>"
		}
		"open" => "<div${id} class=\"openblock${if (node.style == "partintro") " partintro" else ""}${role}\">\n${title}<div class=\"content\">\n${if (p.blocks.is_empty()) p.source_html else body}\n</div>\n</div>"
		"sidebar" => "<div${id} class=\"sidebarblock\">\n<div class=\"content\">\n${title}${if (p.blocks.is_empty()) p.source_html else body}\n</div>\n</div>"
		"example" => "<div${id} class=\"exampleblock\">\n${title}<div class=\"content\">\n${if (p.blocks.is_empty()) p.source_html else body}\n</div>\n</div>"
		"quote" => "<div${id} class=\"quoteblock${role}\">\n${title}<blockquote>\n${if (p.blocks.is_empty()) p.source_html else body}\n</blockquote>${prepared_attribution(p)}\n</div>"
		"verse" => "<div${id} class=\"verseblock${role}\">\n${title}<pre class=\"content\">${p.source_html}</pre>${prepared_attribution(p)}\n</div>"
		"toc" => p.table_html
		"stem" => "<div${id} class=\"stemblock${role}\">\n${title}<div class=\"content\">\n${if (node.style == "latexmath" or get("stem") == "latexmath") "\\[${escape(node.source)}\\]" else "\\$${escape(node.source)}\\$"}\n</div>\n</div>"
		"pass" => node.source
		"thematic_break" => "<hr>"
		"page_break" => "<div style=\"page-break-after: always;\"></div>"
		"admonition" => "<div${id} class=\"admonitionblock ${lower(node.style)}${role}\">\n<table>\n<tr>\n<td class=\"icon\">\n<div class=\"title\">${capitalize(node.style)}</div>\n</td>\n<td class=\"content\">\n${title}${if (p.blocks.is_empty()) p.source_html else body}\n</td>\n</tr>\n</table>\n</div>"
		"ulist" | "olist" | "colist" => {
			checklist = node.context == "ulist" and p.blocks.any(|Prepared(item)| item.node.source.starts_with("[x] ") or item.node.source.starts_with("[ ] ") or item.node.source.starts_with("[*] "))
			AsciiDocBlockHtml.list_wrapper_indexed(node.context, node.style, node.level, node.id, p.title_html, p.view, body, checklist)
		}
		"list_item" => "<li>\n<p>${if (node.source.starts_with("[x] ") or node.source.starts_with("[*] ")) "&#10003; " else if (node.source.starts_with("[ ] ")) "&#10063; " else ""}${p.source_html}</p>${if (body == "") "" else "\n${body}"}\n</li>"
		"dlist" => AsciiDocBlockHtml.description_indexed(node.style, node.id, p.title_html, p.view, p.blocks.map(|Prepared(item)| { term: item.title_html, text: item.source_html, body: render_prepared_blocks(item.blocks) }))
		"image" | "video" | "audio" => {
			media = { context: node.context, target: node.source, title: p.title_html, id: node.id, attrs: if (p.view.indexed) [] else AsciiDocAttributes.materialize(node.attrs) }
			if p.view.indexed {
				AsciiDocMedia.render_view(media, { first: p.view.first, before: p.view.before, after: p.view.after })
			} else {
				AsciiDocMedia.render(media)
			}
		}
		"table" => p.table_html
		_ => p.source_html
	}
}

render_prepared_section = |p| {
	node = p.node
	present = |key| cached_has_attribute(node.attrs, p.view, key)
	body = render_prepared_blocks(p.blocks)
	heading_title = "${p.number}${p.title_html}"
	heading_link = if (node.id != "" and present("sectlinks")) "<a class=\"link\" href=\"#${escape(node.id)}\">${heading_title}</a>" else heading_title
	anchor = if (node.id != "" and present("sectanchors")) "<a class=\"anchor\" href=\"#${escape(node.id)}\"></a>" else ""
	heading_html = "<h${(node.level + 1).to_str()}${if (node.id == "" and !p.generated) "" else " id=\"${escape(node.id)}\""}${if (node.level == 0) " class=\"sect0\"" else ""}>${anchor}${heading_link}</h${(node.level + 1).to_str()}>"
	if node.level == 0 {
		"${heading_html}\n${body}"
	} else if node.level == 1 {
		"<div class=\"sect1\">\n${heading_html}\n<div class=\"sectionbody\">\n${body}\n</div>\n</div>"
	} else {
		"<div class=\"sect${node.level.to_str()}\">\n${heading_html}\n${body}\n</div>"
	}
}

prepared_attribution = |p| {
	author = cached_attribute(p.node.attrs, p.view, "2")
	cite = cached_attribute(p.node.attrs, p.view, "3")
	if (author == "" and cite == "") "" else "\n<div class=\"attribution\">\n${if (author == "") "" else "&#8212; ${inline(author)}"}${if (cite == "") "" else "${if (author == "") "" else "<br>\n"}<cite>${inline(cite)}</cite>"}\n</div>"
}

stateless_verbatim_block : AsciiDoc.Block -> Bool
stateless_verbatim_block = |block| match block {
	Listing(_) | Literal(_) | ThematicBreak => True
	_ => False
}

render_stateless_verbatim_block : AsciiDoc.Block -> Str
render_stateless_verbatim_block = |block| match block {
	Listing(source) => "<div class=\"listingblock\">\n<div class=\"content\">\n<pre>${AsciiDocBlockHtml.verbatim(source, [])}</pre>\n</div>\n</div>"
	Literal(source) => "<div class=\"literalblock\">\n<div class=\"content\">\n<pre>${AsciiDocBlockHtml.verbatim(source, [])}</pre>\n</div>\n</div>"
	ThematicBreak => "<hr>"
	_ => crash "non-verbatim block in stateless rendering"
}

render_document : AsciiDoc.Document -> Str
render_document = |document| {
	if document.blocks.is_empty() {
		return ""
	}
	# These public variants have only fixed standard attributes and cannot
	# create references, captions, section numbering or shared footnotes.
	# Check the whole list before rendering so mixed documents keep one pass
	# through the existing stateful renderer.
	if document.blocks.all(stateless_verbatim_block) {
		return Str.join_with(document.blocks.map(render_stateless_verbatim_block), "\n")
	}
	first_node = render_node(document.blocks.first() ?? empty_item)
	document_id = attribute(first_node.attrs, "__document-id")
	document_refs = if (document_id == "") [] else [{ id: document_id, text: attribute(first_node.attrs, "__document-reftext") }]
	identified = identify_blocks(without_header_snapshots(document.blocks), { ids: if (document_id == "") Dict.empty() else Dict.empty().insert(document_id, True), refs: document_refs, numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False })
	header = prepare_header(first_node.attrs, identified.state.refs)
	initial = prepare_blocks(identified.blocks, header.state)
	prepared = { blocks: if (identified.state.toc) attach_prepared_toc(initial.blocks, initial.blocks) else initial.blocks, state: initial.state }
	var $split = 0.U64
	while $split < prepared.blocks.len() and (prepared_node(prepared.blocks.get($split))).context != "section" {
		$split = $split + 1
	}
	body = if (document.title != "" and $split > 0 and $split < prepared.blocks.len()) "<div id=\"preamble\">\n<div class=\"sectionbody\">\n${render_prepared_blocks(prepared.blocks.take_first($split))}\n</div>\n</div>\n${render_prepared_blocks(prepared.blocks.drop_first($split))}" else render_prepared_blocks(prepared.blocks)
	attrs = first_node.attrs
	html = if (has_attr(attrs, "toc") and attribute(attrs, "toc") != "macro" and attribute(attrs, "toc-placement") != "macro") "${prepared_toc_html(prepared.blocks, attrs, False)}\n${body}" else body
	"${html}${footnote_footer(prepared.state, attrs, 0)}"
}

# Header snapshots are needed to restore the title state, then have no role
# in body substitutions. Keep them on the caller's AST while rendering the
# first block with its original source attributes.
without_header_snapshots : List(AsciiDoc.Block) -> List(AsciiDoc.Block)
without_header_snapshots = |blocks| {
	match blocks.first() {
		Ok(Rich(node)) if has_attr(node.attrs, "__header-source") => {
			attrs = AsciiDocAttributes.without_header(node.attrs)
			[Rich({ ..node, attrs })].concat(blocks.drop_first(1))
		}
		_ => blocks
	}
}

footnote_footer : AsciiDocInline.State, AsciiDoc.Attributes, U64 -> Str
footnote_footer = |state, attrs, start| footnote_footer_with(state, has_attr(attrs, "nofootnotes"), start)

footnote_footer_with : AsciiDocInline.State, Bool, U64 -> Str
footnote_footer_with = |state, suppressed, start| {
	if suppressed {
		return ""
	}
	var $notes = []
	var $index = start
	var $cursor = first_emission_after(state.emitted_footnotes, start)
	for note in state.footnotes.drop_first(start) {
		$index = $index + 1
		while $cursor < state.emitted_footnotes.len() and (state.emitted_footnotes.get($cursor) ?? 0) < $index {
			$cursor = $cursor + 1
		}
		if state.emitted_footnotes.get($cursor) != Ok($index) {
			n = $index.to_str()
			$notes = $notes.append("<div class=\"footnote\" id=\"_footnotedef_${n}\">\n<a href=\"#_footnoteref_${n}\">${n}</a>. ${note.text}\n</div>\n")
		}
	}
	if ($notes.is_empty()) "" else "\n<div id=\"footnotes\">\n<hr>\n${Str.join_with($notes, "")}</div>"
}

# Core rendering starts with an empty emission list. Each cell appends exactly
# its newly allocated note range, making this list sorted and duplicate-free.
first_emission_after : List(U64), U64 -> U64
first_emission_after = |emitted, start| {
	if (emitted.last() ?? 0) <= start {
		return emitted.len()
	}
	var $low = 0.U64
	var $high = emitted.len()
	while $low < $high {
		middle = $low + ($high - $low) / 2
		if (emitted.get(middle) ?? 0) <= start {
			$low = middle + 1
		} else {
			$high = middle
		}
	}
	$low
}

attach_toc : List(AsciiDoc.Block), List(AsciiDoc.Block) -> List(AsciiDoc.Block)
attach_toc = |blocks, document| blocks.map(
	|block| {
		node = render_node(block)
		attrs = if (node.context == "toc") put_attr(node.attrs, "__toc-html", if (attribute(node.attrs, "toc") == "macro" or attribute(node.attrs, "toc-placement") == "macro") toc_html(document, node.attrs, True) else "<!-- toc disabled -->") else node.attrs
		Rich({ ..node, attrs, blocks: attach_toc(node.blocks, document) })
	},
)

toc_html : List(AsciiDoc.Block), AsciiDoc.Attributes, Bool -> Str
toc_html = |blocks, attrs, macro| {
	title = if (has_attr(attrs, "toc-title")) attribute(attrs, "toc-title") else "Table of Contents"
	levels = U64.from_str(attribute(attrs, "toclevels")) ?? 2
	"<div id=\"toc\" class=\"toc\">\n<div id=\"toctitle\"${if (macro) " class=\"title\"" else ""}>${AsciiDocInline.render(title, AsciiDocAttributes.materialize(attrs))}</div>\n${
		toc_list(
			blocks,
			if (
				blocks.any(
					|block| {
						n = render_node(block)
						n.context == "section" and n.level == 0
					},
				)
			) 0 else 1,
			levels,
		)
	}\n</div>"
}

toc_list : List(AsciiDoc.Block), U64, U64 -> Str
toc_list = |blocks, level, levels| {
	var $items = []
	for block in blocks {
		node = render_node(block)
		if node.context == "section" and node.level <= levels {
			children = toc_list(node.blocks, node.level + 1, levels)
			$items = $items.append("<li><a href=\"#${escape(node.id)}\">${attribute(node.attrs, "__heading-number")}${rendered_title(node.title, node.attrs)}</a>${if (children == "") "" else "\n${children}\n"}</li>")
		}
	}
	actual_level = blocks.map(render_node).find_first(|node| node.context == "section").map_ok(|node| node.level) ?? level
	if ($items.is_empty()) "" else "<ul class=\"sectlevel${actual_level.to_str()}\">\n${Str.join_with($items, "\n")}\n</ul>"
}

generated_id : Str, AsciiDoc.Attributes -> Str
generated_id = |title, attrs| generated_id_cached(title, attrs, { indexed: False, first: AsciiDocSource.from_dict(Dict.empty()), last: AsciiDocSource.from_dict(Dict.empty()), semantic: AsciiDocAttributes.materialize(attrs), key: "", generation: 0, prefix: "", before: [], after: [] })

generated_id_cached : Str, AsciiDoc.Attributes, AsciiDocAttributes.View -> Str
generated_id_cached = |title, attrs, view| {
	html = render_cached_inline(title, attrs, view, "normal", AsciiDocInline.initial_state).html
	ascii = html.to_utf8().all(|byte| byte < 128)
	lowered = if (ascii) lower(html) else match Case.to_lower(html, Case.unicode_default, Case.unlimited_limits) {
		Ok(result) => Case.result_text(result)
		Err(_) => lower(html)
	}
	var $text = if (ascii) ascii_id_text(lowered) else ""
	var $tag = False
	var $entity = False
	if !ascii {
		for located in Scalar.iter(lowered) {
			scalar = located.scalar
			n = Scalar.to_u32(scalar)
			if n == 60 {
				$tag = True
			}
			else if n == 62 and $tag {
				$tag = False
			}
			else if !$tag {
				if n == 38 {
					$entity = True
				}
				else if n == 59 and $entity {
					$entity = False
				}
				else if !$entity {
					category = GeneralCategory.short(GeneralCategory.of_scalar(scalar))
					if n == 32 or n == 45 or n == 46 or n == 95 or category.starts_with("L") or category.starts_with("M") or category.starts_with("N") or category == "Pc" {
						$text = Str.concat($text, Str.from_utf8_lossy(Scalar.to_utf8(scalar)))
					}
				}
			}
		}
	}
	prefix = if (cached_has_attribute(attrs, view, "idprefix")) cached_attribute(attrs, view, "idprefix") else "_"
	explicit_separator = cached_has_attribute(attrs, view, "idseparator")
	separator = if (explicit_separator) Str.from_utf8_lossy(cached_attribute(attrs, view, "idseparator").to_utf8().take_first(1)) else "_"
	separator_bytes = separator.to_utf8()
	separator_byte = separator_bytes.first()
	var $bytes = []
	var $last = False
	for byte in "${prefix}${$text}".to_utf8() {
		translated = byte == ' ' or (separator != "" and (byte == '.' or byte == '-' or separator_byte == Ok(byte) or (!explicit_separator and byte == '_')))
		if translated {
			if !$last {
				$bytes = $bytes.concat(separator_bytes)
			}
			$last = True
		}
		else {
			$bytes = $bytes.append(byte)
			$last = False
		}
	}
	var $out = Str.from_utf8_lossy($bytes)
	if separator != "" and $out.ends_with(separator) {
		$out = Str.from_utf8_lossy($out.to_utf8().drop_last(separator.to_utf8().len()))
	}
	if prefix == "" and separator != "" and $out.starts_with(separator) {
		$out = drop_text($out, separator.to_utf8().len())
	}
	$out
}

clean_heading : Str, U64 -> Str
clean_heading = |title, count| {
	var $title = title
	# The optional closing heading marker is ignored only when it matches.
	bytes = title.to_utf8()
	if bytes.len() > count and bytes.drop_first(bytes.len() - count).all(|b| b == 61) and bytes.get(bytes.len() - count - 1) == Ok(32) {
		$title = Str.from_utf8_lossy(bytes.take_first(bytes.len() - count - 1))
	}
	if $title.ends_with("]]") {
		parts = $title.split_on(" [[")
		if parts.len() > 1 {
			$title = Str.join_with(parts.drop_last(1), " [[")
		}
	}
	$title
}

heading_id : Str -> Str
heading_id = |text| {
	if text.ends_with("]]") {
		parts = text.split_on(" [[")
		if parts.len() > 1 {
			return Str.from_utf8_lossy((parts.last() ?? "").to_utf8().drop_last(2))
		}
	}
	""
}

adjusted_heading : Str, AsciiDoc.Attributes -> { level : U64, title : Str }
adjusted_heading = |text, attrs| adjusted_heading_cached(text, attrs, AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view)

adjusted_heading_cached : Str, AsciiDoc.Attributes, AsciiDocAttributes.View -> { level : U64, title : Str }
adjusted_heading_cached = |text, attrs, view| {
	h = heading(text)
	if h.title == "" {
		return h
	}
	offset = numeric_prefix(cached_attribute(attrs, view, "leveloffset"))
	if h.level == 0 {
		if text.starts_with("= ") and offset > 0 {
			return { level: (offset.to_u64_try() ?? 1), title: drop_text(text, 2) }
		}
		return h
	}
	level = (h.level.to_i64_try() ?? 0) + offset
	{ level: if (level < 0) 0 else if (level > 6) 6 else (level.to_u64_try() ?? 1), title: h.title }
}

normalize_headings : List(Line) -> List(Line)
normalize_headings = |lines| {
	var $rest = lines
	var $result = []
	var $opaque = ""
	var $boundary = True
	var $seen = False
	var $header = False
	while !$rest.is_empty() {
		line = $rest.first() ?? empty_line
		text = line.text
		if $opaque != "" {
			$result = $result.append(line)
			$rest = $rest.drop_first(1)
			if text == $opaque {
				$opaque = ""
				$boundary = True
			}
		} else if delimiter(text) != "" {
			$opaque = text
			$result = $result.append(line)
			$rest = $rest.drop_first(1)
			$header = False
			$seen = True
		} else {
			next = ($rest.get(1) ?? empty_line).text
			first = next.to_utf8().first() ?? 0
			special = text == "---" or text == "***" or text.to_utf8().all(|b| b == 39)
			underline = $boundary and !$header and next.to_utf8().len() >= 2 and next.to_utf8().all(|b| b == first) and [61, 45, 126, 94, 43].contains(first) and has_alphanumeric(text) and !special and !text.starts_with("[") and !text.starts_with(".")
			length = scalar_count(text)
			if underline and next.to_utf8().len() + 1 >= length and next.to_utf8().len() <= length + 1 {
				marker = match first {
					61 => "="
					45 => "=="
					126 => "==="
					94 => "===="
					_ => "====="
				}
				$result = $result.append({ text: "${marker} ${text}", attrs: line.attrs })
				$rest = $rest.drop_first(2)
				$boundary = True
				$seen = True
			} else {
				$result = $result.append(line)
				$rest = $rest.drop_first(1)
				if text == "" {
					$boundary = True
					$header = False
				}
				else if !text.starts_with("//") {
					if !$seen and text.starts_with("= ") {
						$header = True
					}
					section_boundary = $boundary and !$header and heading(text).title != ""
					if !(is_block_metadata(text)) and !(text.starts_with(".") and text.to_utf8().len() > 1) {
						$boundary = False
					}
					if special or section_boundary {
						$boundary = True
					}
					if !(is_block_metadata(text)) {
						$seen = True
					}
				}
			}
		}
	}
	$result
}

scalar_count : Str -> U64
scalar_count = |text| text.to_utf8().fold(0.U64, |n, b| if (b < 128 or b >= 192) n + 1 else n)

valid_media : Str -> Bool
valid_media = |text| {
	(text.starts_with("image::") or text.starts_with("video::") or text.starts_with("audio::")) and text.ends_with("]") and text.contains("[") and !(text.starts_with("image::[") or text.starts_with("video::[") or text.starts_with("audio::["))
}

parse_compact : Str, U64 -> AsciiDoc.Attributes
parse_compact = |value, position| {
	if position > 1 {
		return AsciiDocAttributes.flat([{ name: position.to_str(), value: unquote(value) }])
	}
	var $attrs = AsciiDocAttributes.flat([])
	var $kind = position.to_str()
	var $part = []
	var $role = ""
	for b in value.to_utf8().append(0) {
		if b == 35 or b == 46 or b == 37 or b == 0 {
			text = Str.from_utf8_lossy($part)
			if text != "" {
				if $kind == "role" {
					$role = if ($role == "") text else "${$role} ${text}"
				}
				else {
					$attrs = $attrs.append({ name: $kind, value: text })
				}
			}
			$kind = if (b == 35) "id" else if (b == 46) "role" else "options"
			$part = []
		} else {
			$part = $part.append(b)
		}
	}
	if $role != "" {
		$attrs = $attrs.append({ name: "role", value: $role })
	}
	$attrs
}

take_indented : List(Line) -> { source : Str, rest : List(Line) }
take_indented = |lines| {
	var $rest = lines
	var $body = []
	while !$rest.is_empty() and (($rest.first() ?? empty_line).text.starts_with(" ") or ($rest.first() ?? empty_line).text.starts_with("\t") or ($rest.first() ?? empty_line).text == "") {
		$body = $body.append(($rest.first() ?? empty_line).text)
		$rest = $rest.drop_first(1)
	}
	while !$body.is_empty() and ($body.last() ?? "") == "" {
		$body = $body.drop_last(1)
	}
	{ source: Str.join_with($body, "\n"), rest: $rest }
}

unquote : Str -> Str
unquote = |text| {
	s = text.trim()
	if (s.to_utf8().len() >= 2 and ((s.starts_with("\"") and s.ends_with("\"")) or (s.starts_with("'") and s.ends_with("'")))) Str.from_utf8_lossy(s.to_utf8().drop_first(1).drop_last(1)).replace_each(if (s.starts_with("\"")) "\\\"" else "\\'", if (s.starts_with("\"")) "\"" else "'") else s
}

parse_media_metadata : Str -> AsciiDoc.Attributes
parse_media_metadata = |text| {
	inner = Str.from_utf8_lossy(text.to_utf8().drop_first(1).drop_last(1))
	var $attrs = AsciiDocAttributes.flat([])
	var $position = 0.U64
	for part in split_attributes(inner) {
		$position = $position + 1
		bits = part.split_on("=")
		match bits {
			[name, .. as rest] if !rest.is_empty() => {
				value_text = Str.join_with(rest, "=").trim()
				if value_text != "None" {
					$attrs = $attrs.append({ name: name.trim(), value: unquote(value_text) })
				}
			}
			_ => {
				$attrs = $attrs.append({ name: $position.to_str(), value: unquote(part) })
				if $position == 1 and (part.trim() == "\"\"" or part.trim() == "''") {
					$attrs = $attrs.append({ name: "alt", value: "" })
				}
			}
		}
	}
	$attrs
}

# Bound structural recursion before allocating the recursive tree. Opaque
# blocks do not interpret delimiters in their contents.
validate_nesting : List(AsciiDocPreprocess.ScopedLine) -> Try({}, [InvalidAsciiDoc(AsciiDoc.Error)])
validate_nesting = |lines| {
	var $stack = []
	for line in lines {
		text = trim_end(line.text)
		top = $stack.last() ?? ""
		kind = delimiter(text)
		if top != "" and text == top {
			$stack = $stack.drop_last(1)
		}
		else if kind != "" and !["listing", "literal", "pass", "comment", "table"].contains(delimiter(top)) {
			if $stack.len() >= 64 {
				return Err(InvalidAsciiDoc({ offset: line.offset, line: line.line, message: "AsciiDoc block nesting exceeds 64-level limit" }))
			}
			$stack = $stack.append(text)
		}
	}
	Ok({})
}
expect {
	var $source = ""
	var $marker = "===="
	var $n = 0.U64
	while $n < 65 {
		$source = "${$source}${$marker}\n"
		$marker = "${$marker}="
		$n = $n + 1
	}
	match AsciiDoc.parse_str($source) {
		Err(InvalidAsciiDoc(e)) => e.line == 65 and e.message == "AsciiDoc block nesting exceeds 64-level limit"
		_ => False
	}
}
expect AsciiDoc.parse_str("----\n====\n=====\n----") == Ok({ title: "", blocks: [Listing("====\n=====")] })

validate_list_nesting : List(AsciiDocPreprocess.ScopedLine) -> Try({}, [InvalidAsciiDoc(AsciiDoc.Error)])
validate_list_nesting = |lines| {
	var $frames = []
	var $opaque = ""
	var $blank = False
	var $continuation = False
	for line in lines {
		text = trim_end(line.text)
		if $opaque != "" {
			if text == $opaque {
				$opaque = ""
			}
		} else {
			m = list_marker(text)
			if m.kind != "" {
				while !$frames.is_empty() and (($frames.last() ?? { kind: "", level: 0 }).kind == m.kind and ($frames.last() ?? { kind: "", level: 0 }).level > m.level) {
					$frames = $frames.drop_last(1)
				}
				if $frames.any(|frame| frame.kind == m.kind) {
					while !$frames.is_empty() and ($frames.last() ?? { kind: "", level: 0 }).kind != m.kind {
						$frames = $frames.drop_last(1)
					}
				}
				top = $frames.last() ?? { kind: "", level: 0 }
				if top.kind != m.kind or top.level != m.level {
					if $frames.len() >= 64 {
						return Err(InvalidAsciiDoc({ offset: line.offset, line: line.line, message: "AsciiDoc list nesting exceeds 64-level limit" }))
					}
					$frames = $frames.append({ kind: m.kind, level: m.level })
				}
			} else if delimiter(text) != "" {
				if !$continuation {
					$frames = []
				}
				if ["listing", "literal", "pass", "comment", "table"].contains(delimiter(text)) {
					$opaque = text
				}
			} else if $blank and text != "" and text != "+" and !text.starts_with(" ") and !text.starts_with("\t") and !text.starts_with("[") and !text.starts_with("//") {
				$frames = []
			}
			$blank = text == ""
			$continuation = text == "+"
		}
	}
	Ok({})
}
expect {
	var $source = ""
	var $marker = "*"
	var $n = 0.U64
	while $n < 65 {
		$source = "${$source}${$marker} item\n"
		$marker = "${$marker}*"
		$n = $n + 1
	}
	match AsciiDoc.parse_str($source) {
		Err(InvalidAsciiDoc(e)) => e.line == 65 and e.message == "AsciiDoc list nesting exceeds 64-level limit"
		_ => False
	}
}
expect AsciiDoc.parse_str("* item\n*** nested").is_ok()

implicit_list_style : Str -> Str
implicit_list_style = |text| {
	bytes = text.trim().to_utf8()
	b = bytes.first() ?? 0
	if bytes.get(1) == Ok(46) and bytes.get(2) == Ok(32) {
		if (b >= 65 and b <= 90) "upperalpha" else if (b >= 97 and b <= 122) "loweralpha" else ""
	} else ""
}

# Paragraph-style verbatim blocks consume delimiter-looking lines as text.
# Exclude those contents from structural resource accounting.
structural_lines : List(AsciiDocPreprocess.ScopedLine) -> List(AsciiDocPreprocess.ScopedLine)
structural_lines = |lines| {
	var $result = []
	var $style = ""
	var $paragraph = False
	var $opaque = ""
	for line in lines {
		text = trim_end(line.text)
		if $opaque != "" {
			$result = $result.append(line)
			if text == $opaque {
				$opaque = ""
			}
		} else if $paragraph {
			if text == "" {
				$paragraph = False
				$result = $result.append(line)
			}
		} else if is_block_metadata(text) {
			next = attribute(parse_metadata(text), "1")
			if next != "" {
				$style = next
			}
			$result = $result.append(line)
		} else if text != "" and delimiter(text) == "" and list_marker(text).kind == "" and (["literal", "source", "listing", "pass", "verse"].contains($style) or text.starts_with(" ") or text.starts_with("\t")) {
			$paragraph = True
			$style = ""
		} else {
			$result = $result.append(line)
			if ["listing", "literal", "pass", "comment", "table"].contains(delimiter(text)) {
				$opaque = text
			}
			if text != "" and !text.starts_with("//") and !text.starts_with(".") {
				$style = ""
			}
		}
	}
	$result
}
expect {
	var $source = "[literal]\nText\n"
	var $marker = "===="
	var $n = 0.U64
	while $n < 65 {
		$source = "${$source}${$marker}\n"
		$marker = "${$marker}="
		$n = $n + 1
	}
	AsciiDoc.parse_str($source).is_ok()
}

wrap_part_intro : List(AsciiDoc.Block), AsciiDoc.Attributes -> List(AsciiDoc.Block)
wrap_part_intro = |blocks, attrs| {
	var $prefix = []
	var $rest = blocks
	var $done = False
	while !$done and !$rest.is_empty() {
		block = $rest.first() ?? ThematicBreak
		section = match block {
			Section(_) => True
			Rich(n) => n.context == "section"
			_ => False
		}
		if section {
			$done = True
		} else {
			$prefix = $prefix.append(block)
			$rest = $rest.drop_first(1)
		}
	}
	if $prefix.is_empty() {
		return blocks
	}
	match $prefix {
		[Rich(n)] if n.context == "open" and n.style == "partintro" => blocks
		_ => [rich("open", "", "", "partintro", "", attrs, $prefix, 0)].concat($rest)
	}
}

bibliography_item : AsciiDoc.Block -> AsciiDoc.Block
bibliography_item = |block| match block {
	Rich(n) => rich(n.context, n.source, n.title, n.style, n.id, n.attrs.append({ name: "style", value: "bibliography" }), n.blocks, n.level)
	_ => block
}

# Nested AsciiDoc tables inherit document attributes, not the containing
# table's column specifications, style, ID or substitution metadata.
table_cell_attributes : AsciiDoc.Attributes -> AsciiDoc.Attributes
table_cell_attributes = |attrs| {
	match AsciiDocAttributes.source_only(attrs) {
		Ok(source) => return source.append({ name: "__table-cell-context", value: "" })
		Err(_) => {}
	}
	snapshot = attrs.keep_if(|a| a.name.starts_with("__document:") and a.name != "__document:\u(0)roc-source-snapshot")
	inherited = if (snapshot.is_empty()) attrs.keep_if(|a| !["cols", "format", "separator", "1", "2", "3", "style", "options", "role", "id", "title", "caption"].contains(a.name) and !a.name.starts_with("__") and a.name != "\u(0)roc-source-snapshot") else snapshot.map(|a| { name: drop_text(a.name, 11), value: a.value })
	inherited.append({ name: "__table-cell-context", value: "" })
}

document_reference : List(AsciiDoc.Block), Str, Str -> List(AsciiDoc.Block)
document_reference = |blocks, id, text| {
	if id == "" or blocks.is_empty() {
		blocks
	} else {
		first_node = render_node(blocks.first() ?? empty_item)
		[Rich({ ..first_node, attrs: put_attr(put_attr(first_node.attrs, "__document-id", id), "__document-reftext", text) })].concat(blocks.drop_first(1))
	}
}

is_block_title : Str -> Bool
is_block_title = |text| {
	bytes = text.to_utf8()
	if bytes.first() != Ok(46) {
		return False
	}
	index = if (bytes.get(1) == Ok(46)) 2 else 1
	next = bytes.get(index) ?? 46
	![46, 32, 9].contains(next)
}

has_alphanumeric : Str -> Bool
has_alphanumeric = |text| {
	var $found = False
	for located in Scalar.iter(text) {
		category = GeneralCategory.short(GeneralCategory.of_scalar(located.scalar))
		if category.starts_with("L") or category.starts_with("N") {
			$found = True
		}
	}
	$found
}

numeric_prefix : Str -> I64
numeric_prefix = |text| {
	bytes = text.trim().to_utf8()
	var $length = if ([43, 45].contains(bytes.first() ?? 0)) 1.U64 else 0.U64
	while $length < bytes.len() and (bytes.get($length) ?? 0) >= 48 and (bytes.get($length) ?? 255) <= 57 {
		$length = $length + 1
	}
	I64.from_str(replace(Str.from_utf8_lossy(bytes.take_first($length)), "+", "")) ?? 0
}

is_block_metadata : Str -> Bool
is_block_metadata = |text| {
	if !text.starts_with("[") or !text.ends_with("]") {
		return False
	}
	if text.starts_with("[[") {
		return text.ends_with("]]")
	}
	if text == "[]" {
		return True
	}
	byte = text.to_utf8().get(1) ?? 0
	if [46, 35, 37, 123, 44, 34, 39, 95].contains(byte) or (byte >= 48 and byte <= 57) or (byte >= 65 and byte <= 90) or (byte >= 97 and byte <= 122) {
		return True
	}
	if byte < 128 {
		return False
	}
	for located in Scalar.iter(drop_text(text, 1)) {
		category = GeneralCategory.short(GeneralCategory.of_scalar(located.scalar))
		return category.starts_with("L") or category.starts_with("M") or category.starts_with("N") or category == "Pc"
	}
	False
}

# The header first resolves attributes into the doctitle attribute, then its raw
# title receives normal substitutions after header metadata is in scope.
store_header : List(AsciiDoc.Block), Str, AsciiDoc.Attributes, AsciiDoc.Attributes -> List(AsciiDoc.Block)
store_header = |blocks, source, initial, final| {
	node = match blocks.first() {
		Ok(Rich(existing)) => existing
		Ok(simple) => render_node_with_attributes(simple, standard_attributes({}))
		Err(_) => render_node(rich("__document_header", "", "", "", "", AsciiDocAttributes.flat([]), [], 0))
	}
	attrs = AsciiDocAttributes.header(node.attrs, source, initial, final)
	[Rich({ ..node, attrs })].concat(blocks.drop_first(1))
}

# A narrow proof avoids identifying the whole body merely to render an ordinary
# attribute title. Unproved/manual headers retain the exact reference path.
header_ref_independent : AsciiDoc.Attributes -> Bool
header_ref_independent = |attrs| match AsciiDocAttributes.header_parts(attrs) {
	Err(_) => False
	Ok(h) => {
		initial = AsciiDocAttributes.indexed_attributes(h.initial, AsciiDocAttributes.empty).view
		final = AsciiDocAttributes.indexed_attributes(h.final, AsciiDocAttributes.empty).view
		title = AsciiDocAttributes.lookup_last(final, "doctitle") ?? h.source
		header_safe_text(h.source, initial, final) and header_safe_text(title, initial, final)
	}
}

header_safe_leaf : Str -> Bool
header_safe_leaf = |text| !text.contains("{") and !text.contains("<") and !text.contains(":") and !text.contains("&")

header_safe_text : Str, AsciiDocAttributes.View, AsciiDocAttributes.View -> Bool
header_safe_text = |text, initial, final| {
	if text.contains("<") or text.contains(":") or text.contains("&") {
		return False
	}
	bytes = text.to_utf8()
	var $i = 0.U64
	while $i < bytes.len() {
		byte = bytes.get($i) ?? 0
		if byte == '}' {
			return False
		}
		if byte == '{' {
			start = $i + 1
			$i = start
			while $i < bytes.len() and bytes.get($i) != Ok('}') {
				c = bytes.get($i) ?? 0
				if !((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '-') {
					return False
				}
				$i = $i + 1
			}
			if $i == start or $i >= bytes.len() {
				return False
			}
			name = Str.from_utf8_lossy(bytes.drop_first(start).take_first($i - start)).with_ascii_lowercased()
			# Early substitutions have no macros and discarded HTML. Missing
			# initial bindings/builtins cannot mutate or consult references.
			match AsciiDocAttributes.lookup_first(initial, name) {
				Ok(value) if !header_safe_leaf(value) => return False
				_ => {}
			}
			match AsciiDocAttributes.lookup_first(final, name) {
				Ok(value) if header_safe_leaf(value) => {}
				_ => return False
			}
		}
		$i = $i + 1
	}
	True
}

prepare_header : AsciiDoc.Attributes, List({ id : Str, text : Str }) -> { html : Str, state : AsciiDocInline.State }
prepare_header = |attrs, refs| {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, refs)
	match AsciiDocAttributes.header_parts(attrs) {
		Ok(h) => {
			initial_view = AsciiDocAttributes.indexed_attributes(h.initial, AsciiDocAttributes.empty).view
			final_view = AsciiDocAttributes.indexed_attributes(h.final, AsciiDocAttributes.empty).view
			early = if (has_attr(h.initial, "doctitle")) { html: "", state } else render_cached_inline(h.source, h.initial, initial_view, "specialcharacters,attributes", state)
			title = if (has_attr(h.final, "doctitle")) attribute(h.final, "doctitle") else h.source
			return render_cached_inline(title, h.final, final_view, "normal", early.state)
		}
		Err(_) => {}
	}
	if !has_attr(attrs, "__header-source") {
		return { html: "", state }
	}
	initial = attrs.keep_if(|a| a.name.starts_with("__header-initial:")).map(|a| { name: drop_text(a.name, 17), value: a.value })
	final = attrs.keep_if(|a| a.name.starts_with("__header-final:")).map(|a| { name: drop_text(a.name, 15), value: a.value })
	source = attribute(attrs, "__header-source")
	early = if (has_attr(initial, "doctitle")) { html: "", state } else AsciiDocInline.render_state(source, AsciiDocAttributes.materialize(initial), { subs: "specialcharacters,attributes" }, state)
	title = if (has_attr(final, "doctitle")) attribute(final, "doctitle") else source
	AsciiDocInline.render_state(title, AsciiDocAttributes.materialize(final), { subs: "normal" }, early.state)
}

expect match AsciiDoc.parse_str("= {counter:n}") {
	Ok(doc) => doc.title == "2" and doc.blocks.is_empty()
	_ => False
}
expect match AsciiDoc.parse_str("= {project} Guide\n:project: Roc") {
	Ok(doc) => doc.title == "Roc Guide" and doc.blocks.is_empty()
	_ => False
}

# ASCII headings need neither Unicode case mapping nor category lookup.
ascii_id_text : Str -> Str
ascii_id_text = |source| {
	var $out = []
	var $tag = False
	var $entity = False
	for byte in source.to_utf8() {
		if byte == 60 {
			$tag = True
		}
		else if byte == 62 and $tag {
			$tag = False
		}
		else if !$tag {
			if byte == 38 {
				$entity = True
			}
			else if byte == 59 and $entity {
				$entity = False
			}
			else if !$entity and (byte == 32 or byte == 45 or byte == 46 or byte == 95 or (byte >= 97 and byte <= 122) or (byte >= 48 and byte <= 57)) {
				$out = $out.append(byte)
			}
		}
	}
	Str.from_utf8_lossy($out)
}

expect {
	state = { ..AsciiDocInline.initial_state, footnotes: [{ name: "", text: "one" }, { name: "", text: "two" }, { name: "", text: "three" }, { name: "", text: "four" }, { name: "", text: "five" }], emitted_footnotes: [1, 3, 5] }
	footer = footnote_footer(state, AsciiDocAttributes.flat([]), 2)
	footer.contains("id=\"_footnotedef_4\"") and !footer.contains("_footnotedef_1") and !footer.contains("_footnotedef_2") and !footer.contains("_footnotedef_3") and !footer.contains("_footnotedef_5") and footnote_footer(state, AsciiDocAttributes.flat([]), 5) == "" and footnote_footer(state, AsciiDocAttributes.flat([{ name: "nofootnotes", value: "" }]), 0) == ""
}
expect first_emission_after([1, 3, 5], 0) == 0 and first_emission_after([1, 3, 5], 1) == 1 and first_emission_after([1, 3, 5], 2) == 1 and first_emission_after([1, 3, 5], 3) == 2 and first_emission_after([1, 3, 5], 5) == 3 and first_emission_after([], 0) == 0

expect {
	source = "Before footnote:[Root before].\n\n[cols=\"a\"]\n|===\n|Outer before footnote:[Outer before].\n\n[cols=\"a\"]\n!===\n!Inner footnote:[Inner].\n!===\n\nOuter after footnote:[Outer after].\n|===\n\nAfter footnote:[Root after]."
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			identified = identify_blocks(doc.blocks, { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False })
			prepared = prepare_blocks(identified.blocks, AsciiDocInline.initial_state)
			html = AsciiDoc.render(doc)
			prepared.state.emitted_footnotes == [2, 3, 4] and [1.U64, 2, 3, 4, 5].all(|n| html.split_on("id=\"_footnotedef_${n.to_str()}\"").len() == 2)
		}
		_ => False
	}
}

# Core attribute lookups choose the last binding; inline substitutions choose
# the first. Both indexes are derived once from the same immutable snapshot.
cached_attribute : AsciiDoc.Attributes, AsciiDocAttributes.View, Str -> Str
cached_attribute = |attrs, view, name| if (view.indexed) AsciiDocAttributes.lookup_last(view, name) ?? "" else attribute(attrs, name)

cached_has_attribute : AsciiDoc.Attributes, AsciiDocAttributes.View, Str -> Bool
cached_has_attribute = |attrs, view, name| if (view.indexed) AsciiDocAttributes.lookup_last(view, name).is_ok() else has_attr(attrs, name)

# A long shared source snapshot must not hide changed local block metadata.
expect {
	header = dense_test_header({})
	source = "${header}\n\n[#first.red]\n.First title\nhttps://example.test[First]\n\n[#second.blue]\n.Second title\nhttps://example.test[Second]"
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			html = AsciiDoc.render(doc)
			html.contains("id=\"first\" class=\"paragraph red\"") and html.contains("id=\"second\" class=\"paragraph blue\"") and html.contains("<div class=\"title\">First title</div>") and html.contains("<div class=\"title\">Second title</div>")
		}
		_ => False
	}
}

expect {
	header = dense_test_header({})
	source = "= Guide\n${header}\n:toc:\n\n== Start\n\n[options=hardbreaks]\nOne\nTwo footnote:[Shared]\n\nAfter"
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			html = AsciiDoc.render(doc)
			html.contains("id=\"toc\"") and html.contains("One<br>\nTwo") and html.split_on("id=\"_footnotedef_1\"").len() == 2 and !html.contains("roc-source-snapshot")
		}
		_ => False
	}
}

expect {
	header = dense_test_header({})
	match (AsciiDoc.parse_str("${header}\n:value: First\n\n{value}"), AsciiDoc.parse_str("${header}\n:value: Second\n\n{value}")) {
		(Ok(first), Ok(second)) => {
			combined = { title: "", blocks: first.blocks.concat(second.blocks) }
			html = AsciiDoc.render(combined)
			html.contains("<p>First</p>") and html.contains("<p>Second</p>")
		}
		_ => False
	}
}

expect {
	header = dense_test_header({})
	source = "${header}\n\n{counter:n}\n\n--\n{counter:n}\n\n:n: 8\n\n{counter:n}\n--\n\n{counter:n}"
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			html = AsciiDoc.render(doc)
			["<p>1</p>", "<p>2</p>", "<p>9</p>", "<p>10</p>"].all(|value| html.contains(value))
		}
		_ => False
	}
}

dense_test_header : {} -> Str
dense_test_header = |_| {
	var $lines = []
	var $n = 0.U64
	while $n < 70 {
		$lines = $lines.append(":unused${$n.to_str()}: value")
		$n = $n + 1
	}
	Str.join_with($lines, "\n")
}

render_cached_inline : Str, AsciiDoc.Attributes, AsciiDocAttributes.View, Str, AsciiDocInline.State -> { html : Str, state : AsciiDocInline.State }
render_cached_inline = |source, attrs, view, subs, state| {
	if view.indexed {
		AsciiDocInline.render_source_state(
			source,
			view.semantic,
			{ subs: subs },
			state,
			{ first: view.first, token: view.key, generation: view.generation, prefix: view.prefix, before: view.before, after: view.after },
		)
	} else {
		AsciiDocInline.render_state(
			source,
			AsciiDocAttributes.materialize(attrs),
			{ subs: subs },
			state,
		)
	}
}

# Materialize independent ordered bindings throughout a parsed document.
without_internal_caches : AsciiDoc.Document -> AsciiDoc.Document
without_internal_caches = |document| { title: document.title, blocks: clear_source_caches(document.blocks) }

clear_source_caches : List(AsciiDoc.Block) -> List(AsciiDoc.Block)
clear_source_caches = |blocks| blocks.map(
	|block| match block {
		Rich(node) => Rich({ ..node, attrs: node.attrs.keep_if(|attr| attr.name != "\u(0)roc-source-snapshot"), blocks: clear_source_caches(node.blocks) })
		Section(node) => Section({ ..node, blocks: clear_source_caches(node.blocks) })
		_ => block
	},
)

expect {
	source = "${dense_test_header({})}\n:value: Original\n\n{value}"
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			original = render_node(doc.blocks.first() ?? empty_item)
			valid = AsciiDocAttributes.indexed_attributes(original.attrs, AsciiDocAttributes.empty)
			# Editing source values must first drop the authenticated carrier.
			changed = { ..original, attrs: original.attrs.map(|a| if (a.name == "value") { name: a.name, value: "Changed" } else a) }
			invalidated = without_internal_caches({ title: doc.title, blocks: [Rich(changed)] })
			cleared = render_node(invalidated.blocks.first() ?? empty_item)
			valid.view.first.get("value") == Ok("Original") and !AsciiDocAttributes.indexed_attributes(cleared.attrs, valid.cache).view.indexed and AsciiDoc.render(invalidated).contains("<p>Changed</p>")
		}
		_ => False
	}
}

expect {
	match AsciiDoc.parse_str("[caption=Prefix]\n.Title\nBody") {
		Ok(doc) => AsciiDoc.render(doc).contains("<div class=\"title\">Title</div>")
		_ => False
	}
}

expect {
	carrier = { name: "\u(0)roc-source-snapshot", value: "1:5:value3:Doc" }
	plain = table_cell_attributes(AsciiDocAttributes.flat([{ name: "value", value: "Doc" }, carrier]))
	mapped = table_cell_attributes(AsciiDocAttributes.flat([{ name: "__document:value", value: "Doc" }, { name: "__document:\u(0)roc-source-snapshot", value: carrier.value }]))
	[plain, mapped].all(|attrs| attribute(attrs, "value") == "Doc" and !has_attr(attrs, carrier.name))
}

prepared_node = |value| match value {
	Ok(Prepared(p)) => p.node
	Err(_) => render_node(empty_item)
}

attach_prepared_toc : List(PreparedBlock), List(PreparedBlock) -> List(PreparedBlock)
attach_prepared_toc = |blocks, document| blocks.map(
	|Prepared(p)| {
		html = if p.node.context == "toc" {
			if (cached_attribute(p.node.attrs, p.view, "toc") == "macro" or cached_attribute(p.node.attrs, p.view, "toc-placement") == "macro") prepared_toc_html(document, p.node.attrs, True) else "<!-- toc disabled -->"
		} else p.table_html
		Prepared({ ..p, table_html: html, blocks: attach_prepared_toc(p.blocks, document) })
	},
)

prepared_toc_html : List(PreparedBlock), AsciiDoc.Attributes, Bool -> Str
prepared_toc_html = |blocks, attrs, macro| {
	title = if (has_attr(attrs, "toc-title")) attribute(attrs, "toc-title") else "Table of Contents"
	levels = U64.from_str(attribute(attrs, "toclevels")) ?? 2
	"<div id=\"toc\" class=\"toc\">\n<div id=\"toctitle\"${if (macro) " class=\"title\"" else ""}>${AsciiDocInline.render(title, AsciiDocAttributes.materialize(attrs))}</div>\n${prepared_toc_list(blocks, if (blocks.any(|Prepared(p)| p.node.context == "section" and p.node.level == 0)) 0 else 1, levels)}\n</div>"
}

prepared_toc_list : List(PreparedBlock), U64, U64 -> Str
prepared_toc_list = |blocks, level, levels| {
	var $items = []
	for Prepared(p) in blocks {
		if p.node.context == "section" and p.node.level <= levels {
			children = prepared_toc_list(p.blocks, p.node.level + 1, levels)
			$items = $items.append("<li><a href=\"#${escape(p.node.id)}\">${p.number}${p.title_html}</a>${if (children == "") "" else "\n${children}\n"}</li>")
		}
	}
	actual_level = blocks.find_first(|Prepared(p)| p.node.context == "section").map_ok(|Prepared(p)| p.node.level) ?? level
	if ($items.is_empty()) "" else "<ul class=\"sectlevel${actual_level.to_str()}\">\n${Str.join_with($items, "\n")}\n</ul>"
}

expect match AsciiDoc.parse_str(".{blank}\nBody") {
	Ok(doc) => AsciiDoc.render(doc).contains("<div class=\"title\"></div>")
	_ => False
}

expect match AsciiDoc.parse_str(".First\n|===\n|One\n|===\n\n.Second\n|===\n|Two\n|===") {
	Ok(doc) => AsciiDoc.render(doc).contains("<caption class=\"title\">Table 2. Second</caption>")
	_ => False
}

expect {
	source = "${dense_test_header({})}\n:imagesdir: assets\n\nimage::first.png[First,100,80,role=first]\n\nimage::second.png[Second,200,160,role=second]"
	match AsciiDoc.parse_str(source) {
		Ok(doc) => {
			first = render_node(doc.blocks.first() ?? empty_item)
			second = render_node(doc.blocks.get(1) ?? empty_item)
			bindings = AsciiDoc.materialize_attributes(first.attrs)
			html = AsciiDoc.render(doc)
			bindings.contains({ name: "imagesdir", value: "assets" }) and !bindings.any(|a| a.name == "\u(0)roc-source-snapshot") and AsciiDoc.render_inline_attributes("{imagesdir}", first.attrs) == "assets" and attribute(first.attrs, "1") == "First" and attribute(second.attrs, "1") == "Second" and html.contains("alt=\"First\" width=\"100\" height=\"80\"") and html.contains("alt=\"Second\" width=\"200\" height=\"160\"")
		}
		_ => False
	}
}

expect {
	bindings = [{ name: "label", value: "first" }, { name: "label", value: "last" }, { name: "empty", value: "" }]
	attrs = AsciiDoc.attributes(bindings)
	manual = { title: "", blocks: [Rich({ context: "paragraph", source: "{label} {empty}", title: "", style: "", id: "", attrs, blocks: [], level: 0 })] }
	AsciiDoc.materialize_attributes(attrs) == bindings and AsciiDoc.render_inline_attributes("{label}", attrs) == "first" and AsciiDoc.render(manual).contains("<p>first </p>")
}

# Sharing the immutable standard source must not share mutable Inline state or
# inherit bindings from neighboring manual Rich nodes.
expect {
	first = Paragraph("{counter:n} footnote:[First]")
	last = Paragraph("{counter:n} footnote:[Last]")
	middle = Rich({ ..render_node(Paragraph("{counter:n} footnote:[Middle]")), attrs: AsciiDoc.attributes(AsciiDocPreprocess.standard.append({ name: "n", value: "8" })) })
	mixed = { title: "", blocks: [first, middle, last] }
	legacy = { title: "", blocks: [Rich(render_node(first)), middle, Rich(render_node(last))] }
	actual = AsciiDoc.render(mixed)
	actual == AsciiDoc.render(legacy) and actual == AsciiDoc.render(mixed) and actual.contains("<p>9 ") and actual.contains("id=\"_footnotedef_3\"")
}

expect {
	simple = { title: "", blocks: [Section({ level: 1, title: "{counter:n}", blocks: [Paragraph("{counter:n}"), Paragraph("{counter:n}")] })] }
	legacy = { title: "", blocks: [Rich({ ..render_node(Section({ level: 1, title: "{counter:n}", blocks: [] })), blocks: [Rich(render_node(Paragraph("{counter:n}"))), Rich(render_node(Paragraph("{counter:n}")))] })] }
	AsciiDoc.render(simple) == AsciiDoc.render(legacy)
}

# The bounded standard scope has a fixed private key and certificate, so its
# immutable definition must stay unique and contain no control delimiter.
expect {
	bindings = AsciiDocPreprocess.standard
	unique = bindings.fold(Dict.empty(), |seen, binding| seen.insert(binding.name, True))
	attrs = standard_attributes({})
	view = AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view
	unique.len() == bindings.len() and bindings.len() <= 64 and bindings.all(|binding| !binding.value.contains("\u(1)")) and AsciiDoc.materialize_attributes(attrs) == bindings and bindings.all(|binding| AsciiDocAttributes.lookup_first(view, binding.name) == Ok(binding.value) and AsciiDocAttributes.lookup_last(view, binding.name) == Ok(binding.value)) and AsciiDocAttributes.lookup_first(view, "not-a-standard-name") == Err(Missing)
}

# Equal base-source representations must still reconcile local source and
# reserved revision overlays against live dynamic names in both directions.
expect {
	attrs = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("source-boundary", AsciiDocPreprocess.standard))
	standard = standard_attributes({})
	base_view = AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view
	standard_view = AsciiDocAttributes.indexed_attributes(standard, AsciiDocAttributes.empty).view
	local = attrs.append({ name: "__revision:n", value: "new" })
	local_view = AsciiDocAttributes.indexed_attributes(local, AsciiDocAttributes.empty).view
	first = render_cached_inline("{counter:n}", attrs, base_view, "normal", AsciiDocInline.initial_state)
	with_reserved = { ..first.state, attributes: first.state.attributes.insert("__revision:n", "live") }
	body = render_cached_inline("Body", standard, standard_view, "normal", with_reserved)
	reset = render_cached_inline("{counter:n}", local, local_view, "normal", body.state)
	returned = render_cached_inline("{counter:n}", standard, standard_view, "normal", reset.state)
	# A full-Dict hint takes the old conservative all-live fallback, avoiding
	# a different legacy List reader's reserved-name normalization behavior.
	conservative = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot(AsciiDocPreprocess.standard))
	conservative_view = AsciiDocAttributes.indexed_attributes(conservative, AsciiDocAttributes.empty).view
	conservative_local = conservative.append({ name: "__revision:n", value: "new" })
	conservative_local_view = AsciiDocAttributes.indexed_attributes(conservative_local, AsciiDocAttributes.empty).view
	conservative_first = render_cached_inline("{counter:n}", conservative, conservative_view, "normal", AsciiDocInline.initial_state)
	conservative_body = render_cached_inline("Body", conservative, conservative_view, "normal", { ..conservative_first.state, attributes: conservative_first.state.attributes.insert("__revision:n", "live") })
	conservative_reset = render_cached_inline("{counter:n}", conservative_local, conservative_local_view, "normal", conservative_body.state)
	conservative_returned = render_cached_inline("{counter:n}", conservative, conservative_view, "normal", conservative_reset.state)
	first.html == "1" and reset.html == conservative_reset.html and returned.html == conservative_returned.html and returned.state.attributes == conservative_returned.state.attributes and returned.state.removed == conservative_returned.state.removed
}

expect {
	var $parts = []
	var $i = 0.U64
	while $i < 24 {
		$parts = $parts.append("[subs=normal]\n{counter:sequence}\n\nBody")
		$i = $i + 1
	}
	source = Str.join_with($parts, "\n\n")
	options = { ..AsciiDocPreprocess.defaults, attributes: [{ name: "outfilesuffix", value: ".temporary" }, { name: "outfilesuffix", value: ".html" }] }
	match (AsciiDoc.parse_str(source), AsciiDoc.parse_with(source, options)) {
		(Ok(expected), Ok(actual)) => AsciiDoc.render(actual) == AsciiDoc.render(expected) and AsciiDoc.render(actual).contains("<p>24</p>") and actual.blocks.any(
			|block| match block {
				Paragraph("Body") => True
				_ => False
			},
		)
		_ => False
	}
}

expect {
	bindings = AsciiDocPreprocess.standard
	stored = AsciiDocAttributes.from_store(AsciiDocSource.set_parsed(AsciiDocSource.new_parsed("known-negative", bindings), "outfilesuffix", ".temporary"))
	manual = AsciiDoc.attributes(bindings)
	legacy = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot(bindings))
	opts = { ..AsciiDocPreprocess.defaults, attributes: [{ name: "outfilesuffix", value: ".temporary" }] }
	match AsciiDoc.parse_with("{outfilesuffix}", opts) {
		Ok(document) => AsciiDocAttributes.classify_baseline(stored, bindings) == Ok(False) and !plain_attrs(stored) and AsciiDocAttributes.classify_baseline(manual, bindings) == Err(Unknown) and AsciiDocAttributes.classify_baseline(legacy, bindings) == Err(Unknown) and plain_attrs(manual) and plain_attrs(legacy) and AsciiDoc.render(document).contains("<p>.temporary</p>") and document.blocks.any(
			|block| match block {
				Rich(_) => True
				_ => False
			},
		)
		_ => False
	}
}

# Typed header execution must agree with the old serialized/manual AST path,
# including both stateful title phases and the body state they establish.
expect {
	var $same = True
	for source in ["= {counter:n}\n\n{counter:n}", "= {project} Guide\n:project: Roc\n\nBody {project}", "= Header footnote:[Header note]\n\nBody footnote:[Body note]", "= <<target>> {counter:n}\n\n[#target]\n== Target\n\nBody {counter:n}", "= Title\n:doctitle: {counter:n}\n\nBody {counter:n}"] {
		match AsciiDoc.parse_str(source) {
			Ok(doc) => {
				node = render_node(doc.blocks.first() ?? empty_item)
				flattened = node.attrs.map(|a| a)
				legacy = { ..doc, blocks: [Rich({ ..node, attrs: flattened })].concat(doc.blocks.drop_first(1)) }
				refs = identify_blocks(doc.blocks, { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False }).state.refs
				typed_header = prepare_header(node.attrs, refs)
				manual_header = prepare_header(flattened, refs)
				$same = $same and typed_header.html == manual_header.html and typed_header.state.attributes == manual_header.state.attributes and typed_header.state.removed == manual_header.state.removed and typed_header.state.footnotes == manual_header.state.footnotes and AsciiDoc.render(doc) == AsciiDoc.render(legacy)
			}
			Err(_) => {
				$same = False
			}
		}
	}
	$same
}

# Identification still sees the serialized header namespace. Stripping the
# carrier before this pass would change IDs/reference labels for these names.
expect {
	source = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("header-identification", [{ name: "x", value: "Roc" }]))
	stored = store_header([rich("section", "", "{__header-final:x}", "", "", source, [], 1)], "{x}", source, source)
	node = render_node(stored.first() ?? empty_item)
	flat = [Rich({ ..node, attrs: node.attrs.map(|a| a) })]
	initial = { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False }
	typed = identify_blocks(stored, initial)
	legacy = identify_blocks(flat, initial)
	typed.state == legacy.state and (prepared_node(typed.blocks.first())).id == (prepared_node(legacy.blocks.first())).id and AsciiDocAttributes.header_parts(node.attrs).is_ok()
}

# A definition after the title is missing from the initial snapshot but safe
# in the final scope. The common referenced title must qualify.
expect {
	match AsciiDoc.parse_str("= Header {project}\n:project: Roc\n\n== Body\n\nText") {
		Ok(doc) => header_ref_independent(render_node(doc.blocks.first() ?? empty_item).attrs) and doc.title == "Header Roc"
		_ => False
	}
}

# Certificate decisions are conservative; compare selected output and state
# against full body identification, including aliases and side effects.
expect {
	var $same = True
	for source in ["= {project}\n:project: Roc\n\n== Body", "= {project}\n:project: pass:[<<target>>]\n\n[[target]]\n== Destination", "= {project}\n:project: &lt;&lt;target&gt;&gt;\n\n[[target]]\n== Destination", "= Title\n:doctitle: <<target>>\n\n[[target]]\n== Destination", "= {lt}{lt}target{gt}{gt}\n\n[[target]]\n== Destination", "= {two-colons}\n\nBody", "= pass:normal[<<target>>]\n\n[[target]]\n== Destination", "= footnote:[See <<target>>]\n\n[[target]]\n== Destination", "= xref:target[See <<other>>]\n\n[[target]]\n== Target\n\n[[other]]\n== Other", "= {counter:n} footnote:[Header]\n\nBody {counter:n} footnote:[Body]"] {
		match AsciiDoc.parse_str(source) {
			Ok(doc) => {
				attrs = render_node(doc.blocks.first() ?? empty_item).attrs
				refs = identify_blocks(doc.blocks, { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False }).state.refs
				full = prepare_header(attrs, refs)
				$same = $same and doc.title == full.html
				if header_ref_independent(attrs) {
					trial = prepare_header(attrs, [])
					$same = $same and trial.html == full.html and trial.state.attributes == full.state.attributes and trial.state.removed == full.state.removed and trial.state.footnotes == full.state.footnotes
				}
			}
			_ => {
				$same = False
			}
		}
	}
	$same
}

expect {
	initial = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("certificate-initial", []))
	final = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("certificate-final", [{ name: "project", value: "Roc" }]))
	make = |source| AsciiDocAttributes.header(final, source, initial, final)
	header_ref_independent(make("{project}")) and !header_ref_independent(make("{missing}")) and !header_ref_independent(make("{lt}")) and !header_ref_independent(make("{two-colons}")) and !header_ref_independent(make("{project")) and !header_ref_independent(make("{project}}")) and !header_ref_independent(make("{{project}}")) and !header_ref_independent(make("{project name}")) and !header_ref_independent(AsciiDoc.attributes([{ name: "__header-source", value: "Safe" }]))
}

expect {
	base = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("certificate-overlays", [{ name: "project", value: "Safe" }]))
	unsafe_first = AsciiDoc.attributes([{ name: "project", value: "<<target>>" }]).concat(base)
	unsafe_last = base.append({ name: "project", value: "<<target>>" })
	title_first = AsciiDoc.attributes([{ name: "doctitle", value: "<<target>>" }]).concat(base).append({ name: "doctitle", value: "Safe title" })
	title_last = base.append({ name: "doctitle", value: "<<target>>" })
	!header_ref_independent(AsciiDocAttributes.header(base, "{project}", base, unsafe_first)) and header_ref_independent(AsciiDocAttributes.header(base, "{project}", base, unsafe_last)) and header_ref_independent(AsciiDocAttributes.header(base, "Title", base, title_first)) and !header_ref_independent(AsciiDocAttributes.header(base, "Title", base, title_last)) and !header_ref_independent(AsciiDocAttributes.header(base, "{project}", unsafe_first, base))
}

# Simple first blocks keep the exact legacy binding order while the header
# renderer can reuse the canonical indexed standard scope. Compare complete
# blocks, both header phases, and body output against explicit old serialization.
expect {
	initial = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("simple-header-initial", AsciiDocPreprocess.standard))
	final = initial.append({ name: "project", value: "Roc" })
	source = "{counter:n} footnote:h[Header] {project}"
	var $same = True
	for first in [Paragraph("Body footnote:h[]"), Listing("literal"), Literal("literal"), ThematicBreak, Section({ level: 1, title: "Section", blocks: [Paragraph("Body")] })] {
		original = render_node(first)
		bindings = AsciiDoc.materialize_attributes(original.attrs).append({ name: "__header-source", value: source }).concat(AsciiDoc.materialize_attributes(initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(AsciiDoc.materialize_attributes(final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
		legacy = [Rich({ ..original, attrs: AsciiDoc.attributes(bindings) }), Paragraph("Tail")]
		actual = store_header([first, Paragraph("Tail")], source, initial, final)
		node = render_node(actual.first() ?? empty_item)
		new_header = prepare_header(node.attrs, [])
		old_header = prepare_header(AsciiDoc.attributes(bindings), [])
		$same = $same and AsciiDocAttributes.header_parts(node.attrs).is_ok() and AsciiDoc.materialize_attributes(node.attrs) == bindings and actual == legacy and new_header.html == old_header.html and new_header.state.attributes == old_header.state.attributes and new_header.state.removed == old_header.state.removed and new_header.state.footnotes == old_header.state.footnotes and AsciiDoc.render({ title: new_header.html, blocks: actual }) == AsciiDoc.render({ title: old_header.html, blocks: legacy })
	}
	$same
}

# Manual Rich scopes retain duplicate/reserved namespace serialization. Empty
# body carriers retain exact public bindings while using the typed empty scope.
expect {
	initial = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("manual-header-initial", [{ name: "x", value: "initial" }]))
	final = initial.append({ name: "x", value: "final" })
	body = [{ name: "x", value: "first" }, { name: "__header-final:x", value: "reserved" }, { name: "x", value: "last" }]
	manual = rich("paragraph", "{x} {__header-final:x}", "", "", "", AsciiDoc.attributes(body), [], 0)
	stored = store_header([manual], "{x}", initial, final)
	actual = render_node(stored.first() ?? empty_item)
	bindings = body.append({ name: "__header-source", value: "{x}" }).concat(AsciiDoc.materialize_attributes(initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(AsciiDoc.materialize_attributes(final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
	empty_stored = store_header([], "{counter:n}", initial, final)
	empty_node = render_node(empty_stored.first() ?? empty_item)
	empty_bindings = [{ name: "__header-source", value: "{counter:n}" }].concat(AsciiDoc.materialize_attributes(initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(AsciiDoc.materialize_attributes(final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
	empty_part = match AsciiDocAttributes.header_parts(empty_node.attrs) {
		Ok(h) => h.body.is_empty()
		Err(_) => False
	}
	legacy_empty = [Rich({ ..empty_node, attrs: AsciiDoc.attributes(empty_bindings) })]
	new_header = prepare_header(empty_node.attrs, [])
	old_header = prepare_header(AsciiDoc.attributes(empty_bindings), [])
	AsciiDocAttributes.header_parts(actual.attrs).is_err() and AsciiDoc.materialize_attributes(actual.attrs) == bindings and stored == [Rich({ ..render_node(manual), attrs: AsciiDoc.attributes(bindings) })] and empty_part and AsciiDoc.materialize_attributes(empty_node.attrs) == empty_bindings and empty_stored == legacy_empty and new_header.html == old_header.html and new_header.state.attributes == old_header.state.attributes and new_header.state.removed == old_header.state.removed and new_header.state.footnotes == old_header.state.footnotes and AsciiDoc.render({ title: new_header.html, blocks: empty_stored }) == AsciiDoc.render({ title: old_header.html, blocks: legacy_empty })
}

# Verbatim blocks keep their text literal and number automatic callouts per block.
expect {
	html = AsciiDoc.render({ title: "Provided title", blocks: [Listing("code <.>"), Literal("footnote:[literal] <.>"), ThematicBreak] })
	html == "<div class=\"listingblock\">\n<div class=\"content\">\n<pre>code <b class=\"conum\">(1)</b></pre>\n</div>\n</div>\n<div class=\"literalblock\">\n<div class=\"content\">\n<pre>footnote:[literal] <b class=\"conum\">(1)</b></pre>\n</div>\n</div>\n<hr>"
}

# Literal footnote-looking text cannot consume the following paragraph's number.
expect {
	html = AsciiDoc.render({ title: "", blocks: [Listing("footnote:[literal]"), Paragraph("Real footnote:[note]")] })
	html.contains("<pre>footnote:[literal]</pre>") and html.contains("id=\"_footnoteref_1\"") and !html.contains("id=\"_footnoteref_2\"")
}

# Validation follows active preprocessing directives rather than AST text.
expect {
	document : AsciiDoc
	document = "= Checked\n\nText"
	document.title == "Checked" and document.blocks == [Paragraph("Text")]
}
expect match AsciiDoc.validate({ source: "include::chapter.adoc[]", name: "book.adoc" }) {
	Err(BadQuotedBytes(message)) => message.contains("book.adoc:1: AsciiDoc: unexpanded_includes")
	Ok(_) => False
}
expect AsciiDoc.validate({ source: "include::chapter.adoc[]", lints: { unexpanded_includes: False } }).is_ok()
expect AsciiDoc.validate({ source: "\\include::chapter.adoc[]\n\nlink:chapter.adoc[role=include]" }).is_ok()
expect AsciiDoc.validate({ source: "ifdef::missing[]\ninclude::chapter.adoc[]\nendif::[]" }).is_ok()
expect match AsciiDoc.validate({ source: "ifdef::enabled[include::chapter.adoc[]]", attributes: [{ name: "enabled", value: "" }] }) {
	Err(BadQuotedBytes(message)) => message.contains("unexpanded_includes")
	Ok(_) => False
}
expect match AsciiDoc.validate({ source: "include::chapter.adoc[]", expand_includes: True, includes: [{ path: "chapter.adoc", source: "Expanded" }] }) {
	Ok(document) => document.blocks == [Paragraph("Expanded")]
	Err(_) => False
}
expect match AsciiDoc.validate({ source: "include::missing.adoc[]", expand_includes: True }) {
	Err(BadQuotedBytes(message)) => message.contains("virtual include not supplied") and !message.contains("unexpanded_includes")
	Ok(_) => False
}
expect {
	var $source = "include::unexpanded.adoc[]\n\n"
	var $marker = "===="
	var $depth = 0.U64
	while $depth < 65 {
		$source = "${$source}${$marker}\n"
		$marker = "${$marker}="
		$depth = $depth + 1
	}
	match AsciiDoc.validate({ source: $source }) {
		Err(BadQuotedBytes(message)) => message.contains("block nesting exceeds") and !message.contains("unexpanded_includes")
		Ok(_) => False
	}
}

expect AsciiDoc.validate({ source: "////\ninclude::missing.adoc[]\n////\n\nText" }).map_ok(|doc| doc.blocks) == Ok([Paragraph("Text")])
expect AsciiDoc.validate({ source: "/////\ninclude::missing.adoc[]\n/////\n\nText" }).map_ok(|doc| doc.blocks) == Ok([Paragraph("Text")])
expect AsciiDoc.validate({ source: "----\ninclude::missing.adoc[]\n----" }).is_err()

# Include selection. The examples in the doc comments first.
expect AsciiDoc.select_include("one\ntwo\nthree\n", "lines=2..3") == { text: "two\nthree", lines: [2, 3], problems: [] }
expect AsciiDoc.select_include("a\n// tag::x[]\nb\n", "tag=x").problems == [UnclosedTag("x")]
expect AsciiDoc.select_include("  a\n  // tag::x[]\n    b\n  // end::x[]\n", "tag=x,indent=0") == { text: "b", lines: [3], problems: [] }

# Without selectors every line is selected, and markers are ordinary text.
expect AsciiDoc.select_include("// tag::x[]\ninside\n// end::x[]", "") == { text: "// tag::x[]\ninside\n// end::x[]", lines: [1, 2, 3], problems: [] }
expect AsciiDoc.select_include("one\r\ntwo\r\n", "leveloffset=+1") == { text: "one\ntwo", lines: [1, 2], problems: [] }
expect AsciiDoc.select_include("", "") == { text: "", lines: [1], problems: [] }

# `lines=` forms. Selection keeps source order and never repeats a line.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour\n", "lines=1;3..-1") == { text: "one\nthree\nfour", lines: [1, 3, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=3..") == { text: "three\nfour", lines: [3, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"1..2,4\"") == { text: "one\ntwo\nfour", lines: [1, 2, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=4;1") == { text: "one\nfour", lines: [1, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=1..3;2..4") == { text: "one\ntwo\nthree\nfour", lines: [1, 2, 3, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"1,4,\"") == { text: "one\nfour", lines: [1, 4], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\" 2 .. 3 \"") == { text: "two\nthree", lines: [2, 3], problems: [] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=2..9") == { text: "two\nthree\nfour", lines: [2, 3, 4], problems: [] }

# An open range selects its start, then the lines after every finite entry.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"2..,4\"") == { text: "two\nfour", lines: [2, 4], problems: [] }

# An unclosed quote leaves an empty value, which selects every line.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"\"1..2,4\"") == { text: "one\ntwo\nthree\nfour", lines: [1, 2, 3, 4], problems: [] }

# Lenient `lines=` entries select what Asciidoctor selects, and are reported.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=4abc") == { text: "four", lines: [4], problems: [InvalidLines("4abc")] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"+2;1_0;3..-2\"") == { text: "two\nthree", lines: [2, 3], problems: [InvalidLines("+2"), InvalidLines("1_0"), InvalidLines("3..-2")] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=4..2") == { text: "one\ntwo\nthree\nfour", lines: [1, 2, 3, 4], problems: [InvalidLines("4..2")] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=999999999999999999999999999999999999999..999999999999999999999999999999999999998") == { text: "one\ntwo\nthree\nfour", lines: [1, 2, 3, 4], problems: [InvalidLines("999999999999999999999999999999999999999..999999999999999999999999999999999999998")] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"2;4..2\"") == { text: "two", lines: [2], problems: [InvalidLines("4..2")] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"2;9..12\"") == { text: "two", lines: [2], problems: [InvalidLines("9..12")] }

# A start of 0 or less selects nothing at all.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"0,2\"") == { text: "", lines: [], problems: [InvalidLines("0"), EmptySelection] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"-1,2\"") == { text: "", lines: [], problems: [InvalidLines("-1"), EmptySelection] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"1,,4\"") == { text: "", lines: [], problems: [InvalidLines(""), EmptySelection] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=..2") == { text: "", lines: [], problems: [InvalidLines("..2"), EmptySelection] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=\"1..2,part.adoc[lines=\"1..2,4\"") == { text: "", lines: [], problems: [InvalidLines("part.adoc[lines="), EmptySelection] }

# Entries past the end select nothing.
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=9") == { text: "", lines: [], problems: [InvalidLines("9"), EmptySelection] }
expect AsciiDoc.select_include("one\ntwo\nthree\nfour", "lines=5..") == { text: "", lines: [], problems: [InvalidLines("5.."), EmptySelection] }

# `lines=` takes precedence, and the ignored tags report nothing.
expect AsciiDoc.select_include("// tag::x[]\ninside\n// end::x[]", "lines=1..2,tag=x") == { text: "// tag::x[]\ninside", lines: [1, 2], problems: [] }
expect AsciiDoc.select_include("// tag::x[]\ninside", "tags=missing,lines=1..2") == { text: "// tag::x[]\ninside", lines: [1, 2], problems: [] }

# `tag=` and `tags=`, with wildcards and negation.
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tag=x") == { text: "inside", lines: [3], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tag=!x") == { text: "before\nafter", lines: [1, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=**") == { text: "before\ninside\nafter", lines: [1, 3, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=**;!x") == { text: "before\nafter", lines: [1, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=*") == { text: "inside", lines: [3], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=!*") == { text: "before\nafter", lines: [1, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\nx\n// tag::y[]\nxy\n// end::y[]\n// end::x[]\nafter", "tags=x;!y") == { text: "x", lines: [3], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\nx\n// tag::y[]\nxy\n// end::y[]\n// end::x[]\nafter", "tags=x;y") == { text: "x\nxy", lines: [3, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\nx\n// tag::y[]\nxy\n// end::y[]\n// end::x[]\nafter", "tag=y") == { text: "xy", lines: [5], problems: [] }
expect AsciiDoc.select_include("// tag::x[]\nx\n// end::x[]\n// tag::y[]\ny\n// end::y[]", "tags=\"x,y\"") == { text: "x\ny", lines: [2, 5], problems: [] }
expect AsciiDoc.select_include("// tag::x[]\nx\n// end::x[]\n// tag::x[]\nagain\n// end::x[]", "tag=x") == { text: "x\nagain", lines: [2, 5], problems: [] }

# `indent=N` and its combinations.
expect AsciiDoc.select_include("    one\n      two\n\n    three", "indent=0") == { text: "one\n  two\n\nthree", lines: [1, 2, 3, 4], problems: [] }
expect AsciiDoc.select_include("    one\n      two\n    three", "indent=2") == { text: "  one\n    two\n  three", lines: [1, 2, 3], problems: [] }
expect AsciiDoc.select_include("one\n    two\n      three\nfour", "lines=2..3,indent=1") == { text: " two\n   three", lines: [2, 3], problems: [] }
expect AsciiDoc.select_include("  // tag::a[]\n    alpha\n  // end::a[]\n  // tag::b[]\n      beta\n  // end::b[]", "tags=*,indent=0") == { text: "alpha\n  beta", lines: [2, 5], problems: [] }

# Each problem. Tags no selector names are not checked, as in Asciidoctor.
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=x;missing") == { text: "inside", lines: [3], problems: [MissingTag("missing")] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=**;!missing") == { text: "before\ninside\nafter", lines: [1, 3, 5], problems: [] }
expect AsciiDoc.select_include("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tag=missing") == { text: "", lines: [], problems: [MissingTag("missing"), EmptySelection] }
expect AsciiDoc.select_include("// tag::x[]\na\n// tag::y[]\nb\n// end::x[]\nc", "tag=x") == { text: "a\nb", lines: [2, 4], problems: [] }
expect AsciiDoc.select_include("// tag::x[]\na\n// tag::y[]\nb\n// end::x[]\nc", "tags=*") == { text: "a\nb\nc", lines: [2, 4, 6], problems: [UnmatchedEnd("x"), UnclosedTag("y")] }
expect AsciiDoc.select_include("// tag::x[]\na\n// tag::x[]\nb\n// end::x[]\nc\n// end::x[]\nd", "tag=x") == { text: "a\nb", lines: [2, 4], problems: [UnclosedTag("x"), UnmatchedEnd("x")] }
expect AsciiDoc.select_include("a\n// end::x[]\nb", "tag=x") == { text: "", lines: [], problems: [UnmatchedEnd("x"), MissingTag("x"), EmptySelection] }
expect AsciiDoc.select_include("// tag::y[]\na\n// tag::x[]\nb\n// end::y[]\nc\n// end::x[]\nd", "tags=x;y") == { text: "a\nb\nc", lines: [2, 4, 6], problems: [UnmatchedEnd("y")] }
expect AsciiDoc.select_include("// tag::y[]\na\n// tag::x[]\nb\n// end::y[]\nc\n// end::x[]\nd", "tag=y") == { text: "a\nb", lines: [2, 4], problems: [] }
expect AsciiDoc.select_include("a\n// tag::x[]\n// end::x[]\nb", "tag=x") == { text: "", lines: [], problems: [EmptySelection] }

# `include::` expansion selects exactly what `select_include` returns.
expect {
	part = "  intro\n  // tag::a[]\n    alpha\n  // end::a[]\n  // tag::b[]\n      beta\n  // end::b[]\n  outro\n"
	options = { ..AsciiDocPreprocess.defaults, expand_includes: True, includes: [{ path: "part.txt", source: part }] }
	["", "lines=2..3", "lines=\"1;6..\"", "lines=4abc", "lines=9", "tag=a", "tags=a;b,indent=0", "tags=**;!a,indent=2", "tags=*", "tags=!*", "tag=missing", "lines=1..3,tag=a"].all(
		|attributes| {
			selection = AsciiDoc.select_include(part, attributes)
			match AsciiDocPreprocess.run("----\ninclude::part.txt[${attributes}]\n----", options) {
				Ok(result) => {
					included = result.lines.keep_if(|line| line.path == "part.txt")
					included.map(|line| line.line) == selection.lines and Str.join_with(included.map(|line| line.text), "\n") == selection.text
				}
				Err(_) => False
			}
		},
	)
}
