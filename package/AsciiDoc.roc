## Pure AsciiDoc block parsing and HTML rendering.
AsciiDoc :: [].{

	## Parsed title and source-preserving blocks.
	Document : { title : Str, blocks : List(Block) }

	## Attribute name and its string value.
	Attribute : { name : Str, value : Str }

	## A source block; Rich retains metadata and nested block contexts.
	Block := [Paragraph(Str), Section({ level : U64, title : Str, blocks : List(Block) }), Listing(Str), Literal(Str), ThematicBreak, Rich({ context : Str, source : Str, title : Str, style : Str, id : Str, attrs : List(Attribute), blocks : List(Block), level : U64 })].{

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
	parse_with = |source, options| {
		if source.to_utf8().len() > 262144 {
			return Err(InvalidAsciiDoc({ offset: 0, line: 1, message: "AsciiDoc source exceeds 262144-byte limit" }))
		}
		processed = match AsciiDocPreprocess.run(source, options) {
			Ok(value) => value
			Err(PreprocessError(error)) => return Err(InvalidAsciiDoc({ offset: error.offset, line: error.line, message: error.message }))
		}
		structure = structural_lines(processed.lines)
		validate_nesting(structure)?
		validate_list_nesting(structure)?
		var $lines = normalize_headings(processed.lines.map(|line| { text: trim_end(line.text), attrs: line.attributes }))
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
				header_attrs = if $body.is_empty() processed.attributes else ($body.first() ?? empty_line).attrs
				parsed = document_reference(parse_blocks($body, 98).blocks, $document_id, if $document_reftext == "" raw_title else $document_reftext)
				if raw_title.contains("{") or raw_title.contains("footnote:") or raw_title.contains("<<") or raw_title.contains("xref:") or has_attr(header_attrs, "doctitle") {
					stored = store_header(parsed, raw_title, first.attrs, header_attrs)
					identified = identify_blocks(stored, { ids: Dict.empty(), refs: [], numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False })
					header = prepare_header((render_node(stored.first() ?? AsciiDoc.empty_item)).attrs, identified.state.refs)
					Ok({ title: header.html, blocks: if parsed.is_empty() and header.state.footnotes.is_empty() [] else stored })
				} else {
					Ok({ title: AsciiDocInline.render(raw_title, header_attrs), blocks: parsed })
				}
			}
			_ => Ok({ title: "", blocks: parse_blocks(lines, 98).blocks })
		}
	}

	## Empty list item used when constructing source projections.
	empty_item : Block
	empty_item = Rich({ context: "list_item", source: "", title: "", style: "", id: "", attrs: [], blocks: [], level: 0 })

	## Render inline substitutions with caller-supplied attributes.
	render_inline : Str, List(Attribute) -> Str
	render_inline = |source, attrs| AsciiDocInline.render(source, attrs)

	## Render an embedded HTML document with shared IDs and footnotes.
	render : Document -> Str
	render = |document| render_document(document)
}

import AsciiDocInline
import AsciiDocPreprocess
import unicode.Case
import unicode.GeneralCategory
import unicode.Scalar
import AsciiDocTable
import AsciiDocMedia
import AsciiDocBlockHtml

Line : { text : Str, attrs : List(AsciiDoc.Attribute) }

empty_line : Line
empty_line = { text: "", attrs: [] }

drop_text : Str, U64 -> Str
drop_text = |text, count| Str.from_utf8_lossy(text.to_utf8().drop_first(count))

trim_end : Str -> Str
trim_end = |text| {
	bytes = text.to_utf8()
	var $n = bytes.len()
	while $n > 0 and [32, 9, 13, 11, 12, 0].contains(bytes.get($n - 1) ?? 0) {
		$n = $n - 1
	}
	Str.from_utf8_lossy(bytes.take_first($n))
}

make_lines : Str -> List(Line)
make_lines = |source| {
	s = if source.starts_with("﻿") drop_text(source, 3) else source
	s.split_on("\n").map(|text| { text: trim_end(text), attrs: [] })
}

rich : Str, Str, Str, Str, Str, List(AsciiDoc.Attribute), List(AsciiDoc.Block), U64 -> AsciiDoc.Block
rich = |context, source, title, style, id, attrs, blocks, level| Rich({ context, source, title, style, id, attrs, blocks, level })

heading : Str -> { level : U64, title : Str }
heading = |text| {
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

attribute : List(AsciiDoc.Attribute), Str -> Str
attribute = |attrs, name| lookup_attribute(attrs, name) ?? ""

lookup_attribute : List(AsciiDoc.Attribute), Str -> Try(Str, [Missing])
lookup_attribute = |attrs, name| {
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

parse_metadata : Str -> List(AsciiDoc.Attribute)
parse_metadata = |text| {
	inner = drop_text(text, 1).to_utf8().drop_last(1) |> Str.from_utf8_lossy
	var $attrs = []
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
		return { kind: if first == '*' "ulist" else "olist", level: $n, text: drop_text(text, $n + 1).trim(), term: "" }
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
	var $colon = 1.U64
	while $colon + 1 < bytes.len() {
		first_colon = bytes.get($colon) ?? 0
		var $count = 0.U64
		if [58, 59].contains(first_colon) {
			while $colon + $count < bytes.len() and bytes.get($colon + $count) == Ok(first_colon) {
				$count = $count + 1
			}
			if $count >= 2 and $count <= (if first_colon == 59 2 else 4) and ($colon + $count == bytes.len() or bytes.get($colon + $count) == Ok(32) or bytes.get($colon + $count) == Ok(9)) {
				return { kind: "dlist", level: $count - 1, text: drop_text(text, $colon + $count).trim(), term: Str.from_utf8_lossy(bytes.take_first($colon)) }
			}
		}
		$colon = $colon + 1
	}

	{ kind: "", level: 0, text: "", term: "" }
}

delimiter : Str -> Str
delimiter = |text| {
	if text.to_utf8().len() >= 4 and [124, 44, 58, 33].contains(text.to_utf8().first() ?? 0) and text.to_utf8().drop_first(1).all(|b| b == 61) {
		return "table"
	}
	if text.to_utf8().len() >= 4 and text.to_utf8().all(|b| b == (text.to_utf8().first() ?? 0)) {
		match text.to_utf8().first() ?? 0 {
			45 => return "listing"
			46 => return "literal"
			61 => return "example"
			42 => return "sidebar"
			95 => return "quote"
			43 => return "pass"
			47 => return "comment"
			_ => {}
		}
	}
	match text {
		"----" => "listing"
		"...." => "literal"
		"====" => "example"
		"****" => "sidebar"
		"____" => "quote"
		"--" => "open"
		"++++" => "pass"
		"////" => "comment"
		"|===" | ",===" | ":===" | "!===" => "table"
		_ => ""
	}
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
	var $rest = lines
	var $blocks = []
	var $attrs = []
	var $title = ""
	var $id = ""
	var $done = False
	while !$done and !$rest.is_empty() {
		line = $rest.first() ?? empty_line
		text = line.text
		tail = $rest.drop_first(1)
		h = adjusted_heading(text, line.attrs)
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
								else if (text.starts_with("> ") or text == ">") {
									var $quote = []
									while !$rest.is_empty() and (($rest.first() ?? empty_line).text.starts_with("> ") or ($rest.first() ?? empty_line).text == ">") {
										$quote = $quote.append({ text: drop_text(($rest.first() ?? empty_line).text, 2), attrs: line.attrs })
										$rest = $rest.drop_first(1)
									}
									$blocks = $blocks.append(rich("quote", "", $title, "quote", $id, $attrs.concat(line.attrs), parse_blocks($quote, parent).blocks, 0))
									$attrs = []
									$title = ""
									$id = ""
								}
									else {
										$attrs = $attrs.concat(line.attrs)
										style = attribute($attrs, "1")
										id = if $id != "" $id else if attribute($attrs, "id") != "" attribute($attrs, "id") else if h.title != "" heading_id(text) else ""
										kind = delimiter(text)
										if h.title != "" and parent < 99 {
											if style == "discrete" {
												$blocks = $blocks.append(rich("floating_title", "", h.title, style, id, $attrs, [], h.level))
												$rest = tail
											}
												else {
													child_lines = if $title == "" tail else [{ text: ".${$title}", attrs: line.attrs }].concat(tail)
													child = parse_blocks(child_lines, h.level)
													child_blocks = if h.level == 0 and attribute($attrs, "doctype") == "book" wrap_part_intro(child.blocks, $attrs) else child.blocks
													$blocks = $blocks.append(if plain_attrs($attrs) and id == "" Section({ level: h.level, title: h.title, blocks: child_blocks }) else rich("section", "", h.title, style, id, $attrs, child_blocks, h.level))
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
												$attrs = $attrs.concat(line.attrs.map(|a| { name: "__document:${a.name}", value: a.value }))
											}
											$rest = body.rest
											if kind != "comment" {
												context = if kind == "table" "table" else if ["NOTE", "TIP", "IMPORTANT", "WARNING", "CAUTION"].contains(style) "admonition" else if ["stem", "asciimath", "latexmath"].contains(style) "stem" else if ["listing", "literal", "open"].contains(kind) and ["source", "listing", "literal"].contains(style) {
													if style == "source" "listing" else style
												} else if style == "verse" "verse" else if kind == "open" and ["source", "listing", "literal", "pass", "quote", "sidebar", "example"].contains(style) {
													if style == "source" "listing" else style
												} else kind
												compound = ["example", "sidebar", "quote", "open", "admonition"].contains(context)
												children = if compound parse_blocks(body.lines, 99).blocks else []
												if kind == "listing" and plain_attrs($attrs) and $title == "" and id == "" {
													$blocks = $blocks.append(Listing(normalize_indent(body.source, $attrs)))
												}
													else if kind == "literal" and plain_attrs($attrs) and $title == "" and id == "" {
														$blocks = $blocks.append(Literal(normalize_indent(body.source, $attrs)))
													}
														else {
															$blocks = $blocks.append(rich(context, if compound "" else normalize_indent(body.source, $attrs), $title, style, id, $attrs, children, 0))
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
														list_blocks = if style == "bibliography" list.blocks.map(bibliography_item) else list.blocks
														$blocks = $blocks.append(rich(marker.kind, "", $title, if style == "" inferred else style, id, if style == "" and inferred != "" $attrs.append({ name: "__implicit-list-style", value: "" }) else $attrs, list_blocks, marker.level))
														$rest = list.rest
													} else if valid_media(text) {
														context = if text.starts_with("image::") "image" else if text.starts_with("video::") "video" else "audio"
														value = drop_text(text, context.to_utf8().len() + 2)
														bits = value.split_on("[")
														target = bits.first() ?? value
														inline_attrs = if bits.len() > 1 parse_media_metadata("[${Str.join_with(bits.drop_first(1), "[")}") else []
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
															$blocks = $blocks.append(rich("admonition", if source.starts_with("${$admonition}: ") drop_text(source, $admonition.to_utf8().len() + 2).trim() else source, $title, $admonition, id, $attrs, [], 0))
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
										$attrs = []
										$title = ""
										$id = ""
									}
	}
	{ blocks: $blocks, rest: $rest }
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
					$children = $children.append(rich(m.kind, "", "", "", "", [], nested.blocks, m.level))
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
						$body = if $body == "" next.trim() else "${$body}\n${next.trim()}"
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
escape = |s| replace(replace(replace(s, "&", "&amp;"), "<", "&lt;"), ">", "&gt;")

inline : Str -> Str
inline = |s| AsciiDocInline.render(s, [])

render_blocks : List(AsciiDoc.Block) -> Str
render_blocks = |blocks| Str.join_with(blocks.map(render_block), "\n")

render_block : AsciiDoc.Block -> Str
render_block = |block| {
	match block {
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
			title = if node.title == "" or !wrapper "" else "<div class=\"title\">${rendered_title(node.title, node.attrs)}</div>\n"
			body = if ["open", "sidebar", "example", "quote", "admonition", "ulist", "olist", "colist", "list_item"].contains(node.context) render_blocks(node.blocks) else ""
			source = if ["paragraph", "list_item", "verse", "admonition", "quote", "example", "sidebar", "open"].contains(node.context) AsciiDocBlockHtml.paragraph(node.source, node.attrs) else ""
			checklist = node.context == "ulist" and node.blocks.any(
				|b| match b {
					Rich(n) => n.source.starts_with("[x] ") or n.source.starts_with("[ ] ") or n.source.starts_with("[*] ")
					_ => False
				},
			)
			id = if node.id == "" or !wrapper "" else " id=\"${escape(node.id)}\""
			match node.context {
				"section" => render_section_with(node.level, node.title, node.id, node.blocks, node.attrs)
				"floating_title" => "<h${(node.level + 1).to_str()}${if node.id == "" "" else " id=\"${escape(node.id)}\""} class=\"discrete\">${rendered_title(node.title, node.attrs)}</h${(node.level + 1).to_str()}>"
				"paragraph" => "<div${id} class=\"paragraph${role_suffix(node.attrs)}\">\n${title}<p>${source}</p>\n</div>"
				"listing" | "literal" => {
					context = node.context
					language = attribute(node.attrs, "2")
					pre = if node.style == "source" "<pre class=\"highlight${if attribute(node.attrs, "options").split_on(",").contains("nowrap") " nowrap" else ""}\"><code${if language == "" "" else " class=\"language-${language}\" data-lang=\"${language}\""}>${listing_content(node.source, node.attrs)}</code></pre>" else "<pre${if attribute(node.attrs, "options").split_on(",").contains("nowrap") " class=\"nowrap\"" else ""}>${listing_content(node.source, node.attrs)}</pre>"
					"<div${id} class=\"${context}block\">\n${title}<div class=\"content\">\n${pre}\n</div>\n</div>"
				}
				"open" => "<div${id} class=\"openblock${if node.style == "partintro" " partintro" else ""}${role_suffix(node.attrs)}\">\n${title}<div class=\"content\">\n${if node.blocks.is_empty() source else body}\n</div>\n</div>"
				"sidebar" => "<div${id} class=\"sidebarblock\">\n<div class=\"content\">\n${title}${if node.blocks.is_empty() source else body}\n</div>\n</div>"
				"example" => "<div${id} class=\"exampleblock\">\n${title}<div class=\"content\">\n${if node.blocks.is_empty() source else body}\n</div>\n</div>"
				"quote" => "<div${id} class=\"quoteblock${role_suffix(node.attrs)}\">\n${title}<blockquote>\n${if node.blocks.is_empty() source else body}\n</blockquote>${attribution(node.attrs)}\n</div>"
				"verse" => "<div${id} class=\"verseblock${role_suffix(node.attrs)}\">\n${title}<pre class=\"content\">${source}</pre>${attribution(node.attrs)}\n</div>"
				"toc" => attribute(node.attrs, "__toc-html")
				"stem" => "<div${id} class=\"stemblock${role_suffix(node.attrs)}\">\n${title}<div class=\"content\">\n${if node.style == "latexmath" or attribute(node.attrs, "stem") == "latexmath" "\\[${escape(node.source)}\\]" else "\\$${escape(node.source)}\\$"}\n</div>\n</div>"
				"pass" => node.source
				"thematic_break" => "<hr>"
				"page_break" => "<div style=\"page-break-after: always;\"></div>"
				"admonition" => "<div${id} class=\"admonitionblock ${lower(node.style)}${role_suffix(node.attrs)}\">\n<table>\n<tr>\n<td class=\"icon\">\n<div class=\"title\">${capitalize(node.style)}</div>\n</td>\n<td class=\"content\">\n${title}${if node.blocks.is_empty() source else body}\n</td>\n</tr>\n</table>\n</div>"
				"ulist" | "olist" | "colist" => AsciiDocBlockHtml.list_wrapper(node.context, node.style, node.level, node.id, rendered_title(node.title, node.attrs), node.attrs, body, checklist)
				"list_item" => "<li>\n<p>${if node.source.starts_with("[x] ") or node.source.starts_with("[*] ") "&#10003; ${if has_attr(node.attrs, "__rendered-source") source else inline(drop_text(node.source, 4))}" else if node.source.starts_with("[ ] ") "&#10063; ${if has_attr(node.attrs, "__rendered-source") source else inline(drop_text(node.source, 4))}" else source}</p>${if body == "" "" else "\n${body}"}\n</li>"
				"dlist" => AsciiDocBlockHtml.description(
					node.style,
					node.id,
					rendered_title(node.title, node.attrs),
					node.attrs,
					node.blocks.map(
						|item| {
							n = render_node(item)
							{ term: rendered_title(n.title, n.attrs), text: rendered_source(n.source, n.attrs), body: render_blocks(n.blocks) }
						},
					),
				)
				"image" | "video" | "audio" => AsciiDocMedia.render({ context: node.context, target: node.source, title: rendered_title(node.title, node.attrs), id: node.id, attrs: node.attrs })
				"table" => if has_attr(node.attrs, "__rendered-table") attribute(node.attrs, "__rendered-table") else AsciiDocTable.render_with(node.source, put_attr(node.attrs, "title", node.title), |text| render_blocks(parse_blocks(make_lines(text), 0).blocks))
				_ => source
			}
		}
	}
}

render_ditem : AsciiDoc.Block -> Str
render_ditem = |item| match item {
	Rich(n) => "<dt class=\"hdlist1\">${rendered_title(n.title, n.attrs)}</dt>\n<dd>\n<p>${rendered_source(n.source, n.attrs)}</p>\n${if n.blocks.is_empty() "" else "${render_blocks(n.blocks)}\n"}</dd>"
	_ => ""
}

capitalize : Str -> Str
capitalize = |s| {
	bytes = lower(s).to_utf8()
	match bytes {
		[b, .. as rest] => Str.from_utf8_lossy([if b >= 'a' and b <= 'z' b - 32 else b].concat(rest))
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
	render_section_with(level, title, if id == "" section_id(title) else id, blocks, [])
}

render_section_with : U64, Str, Str, List(AsciiDoc.Block), List(AsciiDoc.Attribute) -> Str
render_section_with = |level, title, sid, blocks, attrs| {
	body = render_blocks(blocks)
	heading_title = "${attribute(attrs, "__heading-number")}${rendered_title(title, attrs)}"
	heading_link = if sid != "" and has_attr(attrs, "sectlinks") "<a class=\"link\" href=\"#${escape(sid)}\">${heading_title}</a>" else heading_title
	anchor = if sid != "" and has_attr(attrs, "sectanchors") "<a class=\"anchor\" href=\"#${escape(sid)}\"></a>" else ""
	heading_html = "<h${(level + 1).to_str()}${if sid == "" and !has_attr(attrs, "__generated-id") "" else " id=\"${escape(sid)}\""}${if level == 0 " class=\"sect0\"" else ""}>${anchor}${heading_link}</h${(level + 1).to_str()}>"
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
lower = |s| Str.from_utf8_lossy(s.to_utf8().map(|b| if b >= 'A' and b <= 'Z' b + 32 else b))

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

listing_content : Str, List(AsciiDoc.Attribute) -> Str
listing_content = |source, attrs| AsciiDocBlockHtml.verbatim(source, attrs)

split_attributes : Str -> List(Str)
split_attributes = |text| {
	bytes = text.to_utf8()
	var $parts = []
	var $part = []
	var $index = 0.U64
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		prefix = if byte == 34 or byte == 39 Str.from_utf8_lossy($part).trim() else ""
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

role_suffix : List(AsciiDoc.Attribute) -> Str
role_suffix = |attrs| if attribute(attrs, "role") == "" "" else " ${attribute(attrs, "role")}"

attribution : List(AsciiDoc.Attribute) -> Str
attribution = |attrs| {
	author = attribute(attrs, "2")
	cite = attribute(attrs, "3")
	if author == "" and cite == "" "" else "\n<div class=\"attribution\">\n${if author == "" "" else "&#8212; ${inline(author)}"}${if cite == "" "" else "${if author == "" "" else "<br>\n"}<cite>${inline(cite)}</cite>"}\n</div>"
}

list_style : Str, U64 -> Str
list_style = |style, level| {
	if style != "" style else match level {
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
	Str.join_with(lines.map(|line| drop_text(line, if $n == 999999 0 else $n)), "\n")
}

normalize_indent : Str, List(AsciiDoc.Attribute) -> Str
normalize_indent = |source, attrs| {
	match U64.from_str(attribute(attrs, "indent")) {
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

size_attrs : List(AsciiDoc.Attribute) -> Str
size_attrs = |attrs| {
	w = if attribute(attrs, "width") == "" attribute(attrs, "2") else attribute(attrs, "width")
	h = if attribute(attrs, "height") == "" attribute(attrs, "3") else attribute(attrs, "height")
	"${if w == "" "" else " width=\"${escape(w)}\""}${if h == "" "" else " height=\"${escape(h)}\""}"
}

media_attrs : List(AsciiDoc.Attribute) -> Str
media_attrs = |attrs| {
	options = attribute(attrs, "options").split_on(",")
	"${size_attrs(attrs)}${if attribute(attrs, "poster") == "" "" else " poster=\"${escape(attribute(attrs, "poster"))}\""}${if options.contains("autoplay") " autoplay" else ""}${if options.contains("loop") " loop" else ""}${if options.contains("muted") " muted" else ""}"
}

plain_attrs : List(AsciiDoc.Attribute) -> Bool
plain_attrs = |attrs| {
	defaults = AsciiDocPreprocess.standard
	attrs == defaults or ((attrs.is_empty() or attrs.len() == defaults.len()) and attrs.all(|a| defaults.contains(a)))
}

list_type : Str -> Str
list_type = |style| match style {
	"loweralpha" => " type=\"a\""
	"upperalpha" => " type=\"A\""
	"lowerroman" => " type=\"i\""
	"upperroman" => " type=\"I\""
	_ => ""
}

RenderNode : { context : Str, source : Str, title : Str, style : Str, id : Str, attrs : List(AsciiDoc.Attribute), blocks : List(AsciiDoc.Block), level : U64 }

IdentityState : { ids : Dict(Str, Bool), refs : List({ id : Str, text : Str }), numbers : List(U64), examples : U64, figures : U64, tables : U64, toc : Bool }

render_node : AsciiDoc.Block -> RenderNode
render_node = |block| match block {
	Rich(node) => node
	Paragraph(source) => { context: "paragraph", source, title: "", style: "", id: "", attrs: AsciiDocPreprocess.standard, blocks: [], level: 0 }
	Listing(source) => { context: "listing", source, title: "", style: "", id: "", attrs: AsciiDocPreprocess.standard, blocks: [], level: 0 }
	Literal(source) => { context: "literal", source, title: "", style: "", id: "", attrs: AsciiDocPreprocess.standard, blocks: [], level: 0 }
	ThematicBreak => { context: "thematic_break", source: "", title: "", style: "", id: "", attrs: AsciiDocPreprocess.standard, blocks: [], level: 0 }
	Section({ level, title, blocks }) => { context: "section", source: "", title, style: "", id: "", attrs: AsciiDocPreprocess.standard, blocks, level }
}

has_attr : List(AsciiDoc.Attribute), Str -> Bool
has_attr = |attrs, name| attrs.any(|a| a.name == name)

put_attr : List(AsciiDoc.Attribute), Str, Str -> List(AsciiDoc.Attribute)
put_attr = |attrs, name, value| attrs.keep_if(|a| a.name != name).append({ name, value })

rendered_source : Str, List(AsciiDoc.Attribute) -> Str
rendered_source = |source, attrs| match lookup_attribute(attrs, "__rendered-source") {
	Ok(html) => html
	Err(_) => AsciiDocInline.render(source, attrs)
}

rendered_title : Str, List(AsciiDoc.Attribute) -> Str
rendered_title = |title, attrs| match lookup_attribute(attrs, "__rendered-title") {
	Ok(html) => html
	Err(_) => AsciiDocInline.render(title, attrs)
}

identify_blocks : List(AsciiDoc.Block), IdentityState -> { blocks : List(AsciiDoc.Block), state : IdentityState }
identify_blocks = |blocks, state| {
	var $state = state
	var $blocks = []
	for block in blocks {
		node = render_node(block)
		if node.context == "toc" {
			$state = { ..$state, toc: True }
		}
		var $attrs = node.attrs
		var $id = node.id
		if node.context == "section" or node.context == "floating_title" {
			if $id == "" and has_attr(node.attrs, "sectids") {
				base = generated_id(node.title, node.attrs)
				separator = if has_attr(node.attrs, "idseparator") attribute(node.attrs, "idseparator") else "_"
				$id = base
				$attrs = put_attr($attrs, "__generated-id", "")
				var $suffix = 2.U64
				while $state.ids.contains($id) {
					$id = "${base}${separator}${$suffix.to_str()}"
					$suffix = $suffix + 1
				}
			}
			if node.context == "section" and node.level > 0 and node.level <= 6 {
				var $numbers = $state.numbers
				if has_attr(node.attrs, "sectnums") and node.style != "unnumbered" {
					$numbers = $numbers.set(node.level - 1, ($numbers.get(node.level - 1) ?? 0) + 1) ?? $numbers
					var $index = node.level
					while $index < $numbers.len() {
						$numbers = $numbers.set($index, 0) ?? $numbers
						$index = $index + 1
					}
					if node.level <= (U64.from_str(attribute(node.attrs, "sectnumlevels")) ?? 3) {
						$attrs = put_attr($attrs, "__heading-number", "${Str.join_with($numbers.take_first(node.level).map(U64.to_str), ".")}. ")
					}
				}
				$state = { ..$state, numbers: $numbers }
			}
		}
		if $id != "" {
			reftext = if has_attr(node.attrs, "reftext") attribute(node.attrs, "reftext") else node.title
			$state = { ..$state, ids: $state.ids.insert($id, True), refs: $state.refs.append({ id: $id, text: reftext }) }
		}
		if node.title != "" {
			if node.context == "example" {
				$state = { ..$state, examples: $state.examples + 1 }
				$attrs = put_attr($attrs, "__caption-number", $state.examples.to_str())
			}
			if node.context == "image" {
				$state = { ..$state, figures: $state.figures + 1 }
				$attrs = put_attr($attrs, "__caption-number", $state.figures.to_str())
			}
			if node.context == "table" {
				$state = { ..$state, tables: $state.tables + 1 }
				$attrs = put_attr($attrs, "__caption-number", $state.tables.to_str())
			}
		}
		children = identify_blocks(node.blocks, $state)
		$state = children.state
		$blocks = $blocks.append(Rich({ ..node, id: $id, attrs: $attrs, blocks: children.blocks }))
	}
	{ blocks: $blocks, state: $state }
}

prepare_blocks : List(AsciiDoc.Block), AsciiDocInline.State -> { blocks : List(AsciiDoc.Block), state : AsciiDocInline.State }
prepare_blocks = |blocks, state| {
	var $state = state
	var $blocks = []
	for block in blocks {
		node = render_node(block)
		var $attrs = node.attrs
		title = if node.title == "" { html: "", state: $state } else AsciiDocInline.render_state(node.title, node.attrs, { subs: "normal" }, $state)
		$state = title.state
		caption_kind = if node.context == "example" "example" else if node.context == "image" "figure" else if node.context == "table" "table" else ""
		caption = if caption_kind == "" "" else if has_attr(node.attrs, "caption") attribute(node.attrs, "caption") else if caption_kind != "" and has_attr(node.attrs, "${caption_kind}-caption") "${attribute(node.attrs, "${caption_kind}-caption")} ${attribute(node.attrs, "__caption-number")}. " else ""
		if node.title != "" {
			$attrs = put_attr($attrs, "__rendered-title", "${caption}${title.html}")
		}
		if node.source != "" and ["paragraph", "list_item", "verse", "admonition", "quote", "example", "sidebar", "open"].contains(node.context) {
			subs = if has_attr(node.attrs, "subs") attribute(node.attrs, "subs") else "normal"
			render_source = if node.context == "list_item" and (node.source.starts_with("[x] ") or node.source.starts_with("[*] ") or node.source.starts_with("[ ] ")) drop_text(node.source, 4) else node.source
			rendered = AsciiDocInline.render_state(render_source, node.attrs, { subs: subs }, $state)
			$state = rendered.state
			if rendered.html != render_source or render_source.contains("{") {
				$attrs = put_attr($attrs, "__rendered-source", rendered.html)
			}
		}
		if node.context == "table" {
			table = AsciiDocTable.render_state(
				node.source,
				put_attr(put_attr($attrs, "title", node.title), "id", node.id),
				$state,
				|text, cell_state| AsciiDocInline.render_state(text, node.attrs, { subs: "normal" }, cell_state),
				|text, cell_state| {
					lines = text.split_on("\n").map(|line| { text: line, attrs: table_cell_attributes(node.attrs) })
					identified = identify_blocks(parse_blocks(lines, 98).blocks, { ids: cell_state.refs.fold(Dict.empty(), |index, ref| index.insert(ref.id, True)), refs: cell_state.refs, numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False })
					content = prepare_blocks(identified.blocks, if cell_state.refs.len() == identified.state.refs.len() cell_state else AsciiDocInline.with_refs(cell_state, identified.state.refs))
					prepared = { blocks: if identified.state.toc attach_toc(content.blocks, content.blocks) else content.blocks, state: content.state }
					footer = footnote_footer(prepared.state, node.attrs, cell_state.footnotes.len())
					var $emitted = []
					var $note_index = cell_state.footnotes.len()
					while $note_index < prepared.state.footnotes.len() {
						$note_index = $note_index + 1
						$emitted = $emitted.append($note_index)
					}
					{ html: "${render_blocks(prepared.blocks)}${footer}", state: { ..prepared.state, emitted_footnotes: prepared.state.emitted_footnotes.concat($emitted) } }
				},
			)
			$state = table.state
			$attrs = put_attr($attrs, "__rendered-table", table.html)
		}
		children = prepare_blocks(node.blocks, $state)
		$state = children.state
		$blocks = $blocks.append(Rich({ ..node, attrs: $attrs, blocks: children.blocks }))
	}
	{ blocks: $blocks, state: $state }
}

render_document : AsciiDoc.Document -> Str
render_document = |document| {
	first_node = render_node(document.blocks.first() ?? AsciiDoc.empty_item)
	document_id = attribute(first_node.attrs, "__document-id")
	document_refs = if document_id == "" [] else [{ id: document_id, text: attribute(first_node.attrs, "__document-reftext") }]
	identified = identify_blocks(document.blocks, { ids: if document_id == "" Dict.empty() else Dict.empty().insert(document_id, True), refs: document_refs, numbers: List.repeat(0, 6), examples: 0, figures: 0, tables: 0, toc: False })
	header = prepare_header(first_node.attrs, identified.state.refs)
	initial = prepare_blocks(identified.blocks, header.state)
	prepared = { blocks: if identified.state.toc attach_toc(initial.blocks, initial.blocks) else initial.blocks, state: initial.state }
	var $split = 0.U64
	while $split < prepared.blocks.len() and (render_node(prepared.blocks.get($split) ?? AsciiDoc.empty_item)).context != "section" {
		$split = $split + 1
	}
	body = if document.title != "" and $split > 0 and $split < prepared.blocks.len() "<div id=\"preamble\">\n<div class=\"sectionbody\">\n${render_blocks(prepared.blocks.take_first($split))}\n</div>\n</div>\n${render_blocks(prepared.blocks.drop_first($split))}" else render_blocks(prepared.blocks)
	attrs = (render_node(prepared.blocks.first() ?? AsciiDoc.empty_item)).attrs
	html = if has_attr(attrs, "toc") and attribute(attrs, "toc") != "macro" and attribute(attrs, "toc-placement") != "macro" "${toc_html(prepared.blocks, attrs, False)}\n${body}" else body
	"${html}${footnote_footer(prepared.state, attrs, 0)}"
}

footnote_footer : AsciiDocInline.State, List(AsciiDoc.Attribute), U64 -> Str
footnote_footer = |state, attrs, start| {
	var $notes = ""
	var $index = 0.U64
	if !has_attr(attrs, "nofootnotes") {
		for note in state.footnotes {
			$index = $index + 1
			if $index > start and !state.emitted_footnotes.contains($index) {
				n = $index.to_str()
				$notes = "${$notes}<div class=\"footnote\" id=\"_footnotedef_${n}\">\n<a href=\"#_footnoteref_${n}\">${n}</a>. ${note.text}\n</div>\n"
			}
		}
	}
	if $notes == "" "" else "\n<div id=\"footnotes\">\n<hr>\n${$notes}</div>"
}

attach_toc : List(AsciiDoc.Block), List(AsciiDoc.Block) -> List(AsciiDoc.Block)
attach_toc = |blocks, document| blocks.map(
	|block| {
		node = render_node(block)
		attrs = if node.context == "toc" put_attr(node.attrs, "__toc-html", if attribute(node.attrs, "toc") == "macro" or attribute(node.attrs, "toc-placement") == "macro" toc_html(document, node.attrs, True) else "<!-- toc disabled -->") else node.attrs
		Rich({ ..node, attrs, blocks: attach_toc(node.blocks, document) })
	},
)

toc_html : List(AsciiDoc.Block), List(AsciiDoc.Attribute), Bool -> Str
toc_html = |blocks, attrs, macro| {
	title = if has_attr(attrs, "toc-title") attribute(attrs, "toc-title") else "Table of Contents"
	levels = U64.from_str(attribute(attrs, "toclevels")) ?? 2
	"<div id=\"toc\" class=\"toc\">\n<div id=\"toctitle\"${if macro " class=\"title\"" else ""}>${AsciiDocInline.render(title, attrs)}</div>\n${
		toc_list(
			blocks,
			if blocks.any(
				|block| {
					n = render_node(block)
					n.context == "section" and n.level == 0
				},
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
			$items = $items.append("<li><a href=\"#${escape(node.id)}\">${attribute(node.attrs, "__heading-number")}${rendered_title(node.title, node.attrs)}</a>${if children == "" "" else "\n${children}\n"}</li>")
		}
	}
	actual_level = blocks.map(render_node).find_first(|node| node.context == "section").map_ok(|node| node.level) ?? level
	if $items.is_empty() "" else "<ul class=\"sectlevel${actual_level.to_str()}\">\n${Str.join_with($items, "\n")}\n</ul>"
}

generated_id : Str, List(AsciiDoc.Attribute) -> Str
generated_id = |title, attrs| {
	html = AsciiDocInline.render(title, attrs)
	ascii = html.to_utf8().all(|byte| byte < 128)
	lowered = if ascii lower(html) else match Case.to_lower(html, Case.unicode_default, Case.unlimited_limits) {
		Ok(result) => Case.result_text(result)
		Err(_) => lower(html)
	}
	var $text = if ascii ascii_id_text(lowered) else ""
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
	prefix = if has_attr(attrs, "idprefix") attribute(attrs, "idprefix") else "_"
	explicit_separator = has_attr(attrs, "idseparator")
	separator = if explicit_separator Str.from_utf8_lossy(attribute(attrs, "idseparator").to_utf8().take_first(1)) else "_"
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

adjusted_heading : Str, List(AsciiDoc.Attribute) -> { level : U64, title : Str }
adjusted_heading = |text, attrs| {
	h = heading(text)
	if h.title == "" {
		return h
	}
	offset = numeric_prefix(attribute(attrs, "leveloffset"))
	if h.level == 0 {
		if text.starts_with("= ") and offset > 0 {
			return { level: (offset.to_u64_try() ?? 1), title: drop_text(text, 2) }
		}
		return h
	}
	level = (h.level.to_i64_try() ?? 0) + offset
	{ level: if level < 0 0 else if level > 6 6 else (level.to_u64_try() ?? 1), title: h.title }
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
scalar_count = |text| text.to_utf8().fold(0.U64, |n, b| if b < 128 or b >= 192 n + 1 else n)

valid_media : Str -> Bool
valid_media = |text| {
	(text.starts_with("image::") or text.starts_with("video::") or text.starts_with("audio::")) and text.ends_with("]") and text.contains("[") and !(text.starts_with("image::[") or text.starts_with("video::[") or text.starts_with("audio::["))
}

parse_compact : Str, U64 -> List(AsciiDoc.Attribute)
parse_compact = |value, position| {
	if position > 1 {
		return [{ name: position.to_str(), value: unquote(value) }]
	}
	var $attrs = []
	var $kind = position.to_str()
	var $part = []
	var $role = ""
	for b in value.to_utf8().append(0) {
		if b == 35 or b == 46 or b == 37 or b == 0 {
			text = Str.from_utf8_lossy($part)
			if text != "" {
				if $kind == "role" {
					$role = if $role == "" text else "${$role} ${text}"
				}
					else {
						$attrs = $attrs.append({ name: $kind, value: text })
					}
			}
			$kind = if b == 35 "id" else if b == 46 "role" else "options"
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
	if s.to_utf8().len() >= 2 and ((s.starts_with("\"") and s.ends_with("\"")) or (s.starts_with("'") and s.ends_with("'"))) Str.from_utf8_lossy(s.to_utf8().drop_first(1).drop_last(1)).replace_each(if s.starts_with("\"") "\\\"" else "\\'", if s.starts_with("\"") "\"" else "'") else s
}

parse_media_metadata : Str -> List(AsciiDoc.Attribute)
parse_media_metadata = |text| {
	inner = Str.from_utf8_lossy(text.to_utf8().drop_first(1).drop_last(1))
	var $attrs = []
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
validate_nesting : List(AsciiDocPreprocess.Line) -> Try({}, [InvalidAsciiDoc(AsciiDoc.Error)])
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

validate_list_nesting : List(AsciiDocPreprocess.Line) -> Try({}, [InvalidAsciiDoc(AsciiDoc.Error)])
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
		if b >= 65 and b <= 90 "upperalpha" else if b >= 97 and b <= 122 "loweralpha" else ""
	} else ""
}

# Paragraph-style verbatim blocks consume delimiter-looking lines as text.
# Exclude those contents from structural resource accounting.
structural_lines : List(AsciiDocPreprocess.Line) -> List(AsciiDocPreprocess.Line)
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

wrap_part_intro : List(AsciiDoc.Block), List(AsciiDoc.Attribute) -> List(AsciiDoc.Block)
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
table_cell_attributes : List(AsciiDoc.Attribute) -> List(AsciiDoc.Attribute)
table_cell_attributes = |attrs| {
	snapshot = attrs.keep_if(|a| a.name.starts_with("__document:"))
	inherited = if snapshot.is_empty() attrs.keep_if(|a| !["cols", "format", "separator", "1", "2", "3", "style", "options", "role", "id", "title", "caption"].contains(a.name) and !a.name.starts_with("__")) else snapshot.map(|a| { name: drop_text(a.name, 11), value: a.value })
	inherited.append({ name: "__table-cell-context", value: "" })
}

document_reference : List(AsciiDoc.Block), Str, Str -> List(AsciiDoc.Block)
document_reference = |blocks, id, text| {
	if id == "" or blocks.is_empty() {
		blocks
	} else {
		first_node = render_node(blocks.first() ?? AsciiDoc.empty_item)
		[Rich({ ..first_node, attrs: put_attr(put_attr(first_node.attrs, "__document-id", id), "__document-reftext", text) })].concat(blocks.drop_first(1))
	}
}

is_block_title : Str -> Bool
is_block_title = |text| {
	bytes = text.to_utf8()
	if bytes.first() != Ok(46) {
		return False
	}
	index = if bytes.get(1) == Ok(46) 2 else 1
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
	var $length = if [43, 45].contains(bytes.first() ?? 0) 1.U64 else 0.U64
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
store_header : List(AsciiDoc.Block), Str, List(AsciiDoc.Attribute), List(AsciiDoc.Attribute) -> List(AsciiDoc.Block)
store_header = |blocks, source, initial, final| {
	node = render_node(blocks.first() ?? rich("__document_header", "", "", "", "", [], [], 0))
	attrs = node.attrs.append({ name: "__header-source", value: source }).concat(initial.map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(final.map(|a| { name: "__header-final:${a.name}", value: a.value }))
	[Rich({ ..node, attrs })].concat(blocks.drop_first(1))
}

prepare_header : List(AsciiDoc.Attribute), List({ id : Str, text : Str }) -> { html : Str, state : AsciiDocInline.State }
prepare_header = |attrs, refs| {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, refs)
	if !has_attr(attrs, "__header-source") {
		return { html: "", state }
	}
	initial = attrs.keep_if(|a| a.name.starts_with("__header-initial:")).map(|a| { name: drop_text(a.name, 17), value: a.value })
	final = attrs.keep_if(|a| a.name.starts_with("__header-final:")).map(|a| { name: drop_text(a.name, 15), value: a.value })
	source = attribute(attrs, "__header-source")
	early = if has_attr(initial, "doctitle") { html: "", state } else AsciiDocInline.render_state(source, initial, { subs: "specialcharacters,attributes" }, state)
	title = if has_attr(final, "doctitle") attribute(final, "doctitle") else source
	AsciiDocInline.render_state(title, final, { subs: "normal" }, early.state)
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
