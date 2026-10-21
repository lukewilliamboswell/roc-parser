import AsciiDocInline
import AsciiDocAttributes
import CSV

## Parse and render AsciiDoc PSV, CSV and DSV table contents.
AsciiDocTable :: [].{

	## A table or column attribute.
	Attribute : { name : Str, value : Str }

	## A cell with its positional span and formatting metadata.
	Cell : { text : Str, colspan : U64, rowspan : U64, style : Str, halign : Str, valign : Str }

	## Parsed cells in source order, column specifications and inferred header.
	Table : { rows : List(List(Cell)), columns : List(Column), header : Bool, footer : Bool }

	## A resolved column specification.
	Column : { width : U64, style : Str, halign : Str, valign : Str }

	## Parse table interior text using its block attributes.
	parse : Str, List(Attribute) -> Table
	parse = |source, attrs| parse_table(source, attrs)

	## Render a table. AsciiDoc cells can use a caller-provided block renderer.
	render_with : Str, List(Attribute), (Str -> Str) -> Str
	render_with = |source, attrs, render_blocks| render_table(parse_table(source, attrs), attrs, render_blocks)

	## Render a table with paragraph-only nested AsciiDoc cells.
	render : Str, List(Attribute) -> Str
	render = |source, attrs| render_table(parse_table(source, attrs), attrs, |text| "<div class=\"paragraph\"><p>${AsciiDocInline.render(text, attrs)}</p></div>")

	## Render cells in document order, carrying caller state through inline and
	## nested block content. Footnotes and mutable attributes can cross cells.
	render_state : Str, List(Attribute), state, (Str, state -> { html : Str, state : state }), (Str, state -> { html : Str, state : state }) -> { html : Str, state : state }
	render_state = |source, attrs, state, render_inline, render_blocks| render_table_state(parse_table(source, attrs), Raw(attrs), state, render_inline, render_blocks)

	## Internal renderer bridge. Table options read the immutable source view
	## and its small last-winner overlays without flattening source bindings.
	render_view_state : Str, AsciiDocAttributes.View, state, (Str, state -> { html : Str, state : state }), (Str, state -> { html : Str, state : state }) -> { html : Str, state : state }
	render_view_state = |source, view, state, render_inline, render_blocks| render_table_state(parse_environment(source, Indexed(view)), Indexed(view), state, render_inline, render_blocks)
}

Environment : [Raw(List(AsciiDocTable.Attribute)), Indexed(AsciiDocAttributes.View)]

attr : Environment, Str, Str -> Str
attr = |attrs, name, fallback| lookup_attr(attrs, name) ?? fallback

lookup_attr : Environment, Str -> Try(Str, [Missing])
lookup_attr = |attrs, name| match attrs {
	Indexed(view) => AsciiDocAttributes.lookup_last(view, name)
	Raw(values) => {
		var $position = values.len()
		while $position > 0 {
			$position = $position - 1
			match values.get($position) {
				Ok(item) if item.name == name => return Ok(item.value)
				_ => {}
			}
		}
		Err(Missing)
	}
}

render_caption : Str, Environment -> Str
render_caption = |title, attrs| match attrs {
	Raw(values) => AsciiDocInline.render(title, values)
	Indexed(view) => AsciiDocInline.render_view(title, view)
}

option : Environment, Str -> Bool
option = |attrs, name| {
	flag = "${name}-option"
	if lookup_attr(attrs, flag).is_ok() {
		return True
	}
	for item in attr(attrs, "options", "").split_on(",") {
		if item.trim() == name {
			return True
		}
	}
	False
}

column : Str -> AsciiDocTable.Column
column = |spec| {
	bytes = spec.to_utf8()
	digits = bytes.keep_if(|byte| byte >= '0' and byte <= '9')
	width = U64.from_str(Str.from_utf8_lossy(digits)) ?? 1
	style = if (spec.contains("a")) "asciidoc" else if (spec.contains("l")) "literal" else if (spec.contains("m")) "monospaced" else if (spec.contains("s")) "strong" else if (spec.contains("e")) "emphasis" else if (spec.contains("h")) "header" else ""
	halign = if (spec.starts_with(">")) "right" else if (spec.starts_with("^")) "center" else "left"
	valign = if (spec.contains(".>")) "bottom" else if (spec.contains(".^")) "middle" else "top"
	{ width, style, halign, valign }
}

cell : Str, Str -> AsciiDocTable.Cell
cell = |text, prefix| {
	spec = column(prefix)
	span = prefix.split_on("+").first() ?? ""
	parts = span.split_on(".")
	colspan = if (prefix.contains("+")) U64.from_str(parts.first() ?? "") ?? 1 else 1
	rowspan = if (prefix.contains("+")) U64.from_str(parts.get(1) ?? "") ?? 1 else 1
	{ text: trim_cell(text), colspan: if (colspan == 0) 1 else colspan, rowspan: if (rowspan == 0) 1 else rowspan, style: spec.style, halign: spec.halign, valign: spec.valign }
}

trim_cell : Str -> Str
trim_cell = |text| {
	var $bytes = text.to_utf8()
	while $bytes.first() == Ok('\n') or $bytes.first() == Ok('\r') {
		$bytes = $bytes.drop_first(1)
	}
	while ($bytes.last() ?? 255) <= 32 {
		$bytes = $bytes.drop_last(1)
	}
	Str.from_utf8_lossy($bytes)
}

parse_table : Str, List(AsciiDocTable.Attribute) -> AsciiDocTable.Table
parse_table = |source, attrs| parse_environment(source, Raw(attrs))

parse_environment : Str, Environment -> AsciiDocTable.Table
parse_environment = |source, attrs| {
	format = attr(attrs, "format", "psv")
	var $cells = []
	var $first_row_count = 0
	var $inferred_header = False
	if format == "csv" or format == "tsv" {
		separator = attr(attrs, "separator", if (format == "tsv") "\t" else ",").to_utf8().first() ?? 44
		records = if (separator == 44) CSV.parse_records(source).map_ok(|rows| rows.map(|row| row.map(Str.from_utf8_lossy))) else Ok(delimited_records(source, separator))
		match records {
			Ok(rows) => {
				$first_row_count = (rows.first() ?? []).len()
				for row in rows {
					for value in row {
						$cells = $cells.append(cell(value, ""))
					}
				}
			}
			Err(_) => {}
		}
	} else if format == "dsv" {
		separator = attr(attrs, "separator", ":").to_utf8().first() ?? ':'
		for line in source.split_on("\n") {
			if line != "" {
				var $fields = []
				var $part = []
				var $escaped = False
				for byte in line.to_utf8() {
					if $escaped {
						$part = $part.append(byte)
						$escaped = False
					}
					else if byte == '\\' {
						$escaped = True
					}
					else if byte == separator {
						$fields = $fields.append(Str.from_utf8_lossy($part))
						$part = []
					}
					else {
						$part = $part.append(byte)
					}
				}
				if $escaped {
					$part = $part.append('\\')
				}
				$fields = $fields.append(Str.from_utf8_lossy($part))
				if $first_row_count == 0 {
					$first_row_count = $fields.len()
				}
				for value in $fields {
					$cells = $cells.append(cell(value, ""))
				}
			}
		}
	} else {
		separator = if (format == "dsv") (attr(attrs, "separator", ":").to_utf8().first() ?? ':') else (attr(attrs, "separator", "|").to_utf8().first() ?? '|')
		bytes = source.to_utf8()
		var $index = 0
		var $text = []
		var $prefix = ""
		var $started = False
		var $first_line_done = False
		var $line_separators = 0
		first_row = inferred_row_boundary(source, separator)
		while $index < bytes.len() {
			byte = bytes.get($index) ?? 0
			if byte == '\\' and bytes.get($index + 1) == Ok(separator) {
				$text = $text.append(separator)
				$index = $index + 2
			} else if byte == separator {
				# Cell specifications are the last whitespace-delimited token before
				# a separator, unless separated by a newline from previous content.
				before = Str.from_utf8_lossy($text)
				suffix = before.split_on("\n").last() ?? ""
				candidate = suffix.split_on(" ").last() ?? ""
				is_spec = candidate != "" and (!$started or before.to_utf8().len() > candidate.to_utf8().len()) and candidate.to_utf8().all(|b| (b >= '0' and b <= '9') or b == '+' or b == '.' or b == '<' or b == '>' or b == '^' or b == 'a' or b == 'l' or b == 's' or b == 'e' or b == 'h' or b == 'm' or b == 'v')
				prefix = if (is_spec) candidate else ""
				content = if (is_spec) Str.from_utf8_lossy($text.drop_last(candidate.to_utf8().len())) else before
				if $started or content.trim() != "" {
					$cells = $cells.append(cell(content, $prefix))
					if !$started {
						$line_separators = $line_separators + 1
					}
				}
				$prefix = prefix
				$text = []
				$started = True
				if !$first_line_done {
					$line_separators = $line_separators + 1
				}
				$index = $index + 1
			} else {
				if byte == '\n' and !$first_line_done and $line_separators > 0 and $index == first_row.end {
					$first_line_done = True
					$first_row_count = $line_separators
					$inferred_header = first_row.header and !["asciidoc", "literal"].contains(column((attr(attrs, "cols", "").split_on(",").first() ?? "")).style)
				}
				$text = $text.append(byte)
				$index = $index + 1
			}
		}
		if $started or source.trim() != "" {
			$cells = $cells.append(cell(Str.from_utf8_lossy($text), $prefix))
		}
		if $first_row_count == 0 {
			$first_row_count = $line_separators
		}
	}
	var $columns = []
	cols = attr(attrs, "cols", "")
	if cols != "" {
		for spec in cols.split_on(",") {
			parts = spec.split_on("*")
			count = if (parts.len() > 1) U64.from_str(parts.first() ?? "") ?? 1 else 1
			body = if (parts.len() > 1) parts.last() ?? "" else spec
			var $repeat = 0
			while $repeat < count and $columns.len() < 256 {
				$columns = $columns.append(column(body))
				$repeat = $repeat + 1
			}
		}
	} else {
		var $index = 0
		inferred_columns = $cells.take_first($first_row_count).fold(0.U64, |sum, c| if (sum >= 256 or c.colspan >= 256 - sum) 256 else sum + c.colspan)
		while $index < inferred_columns and $index < 256 {
			$columns = $columns.append(column(""))
			$index = $index + 1
		}
	}
	if $columns.is_empty() {
		$columns = [column("")]
	}
	var $rows = []
	var $row = []
	var $visits = 0.U64
	var $active = []
	var $pending = []
	for current in $cells {
		$row = $row.append(current)
		$visits = if (current.colspan > $columns.len() or $visits > $columns.len() - current.colspan) $columns.len() + 1 else $visits + current.colspan
		if current.rowspan > 1 {
			$pending = $pending.append({ remaining: current.rowspan - 1, width: current.colspan })
		}
		occupied = $active.fold(0.U64, |sum, span| if (sum > $columns.len() or span.width > $columns.len()) $columns.len() + 1 else sum + span.width)
		if $visits + occupied >= $columns.len() {
			if $visits + occupied == $columns.len() {
				$rows = $rows.append($row)
			}
			$row = []
			$visits = 0
			$active = $active.keep_if(|span| span.remaining > 1).map(|span| { remaining: span.remaining - 1, width: span.width }).concat($pending)
			$pending = []
		}
	}
	# Native Asciidoctor drops the final incomplete or overflowing row.

	{ rows: $rows, columns: $columns, header: option(attrs, "header") or ($inferred_header and $cells.len() > $first_row_count and !option(attrs, "noheader")), footer: option(attrs, "footer") }
}

## A PSV row ends when a later physical line starts a new cell. Continuation
## lines (including escaped leading separators) remain part of the first row.
inferred_row_boundary : Str, U8 -> { end : U64, header : Bool }
inferred_row_boundary = |source, separator| {
	var $offset = 0
	var $started = False
	var $header = False
	var $after_first = False
	var $leading = False
	for line in source.split_on("\n") {
		if !$started {
			if line == "" {
				$leading = True
			}
			else {
				$started = True
				$after_first = True
			}
		} else if line == "" {
			if $after_first and !$leading {
				$header = True
			}
		} else {
			if starts_cell(line, separator) {
				return { end: $offset - 1, header: $header }
			}
			$header = False
			$after_first = False
		}
		$offset = $offset + line.to_utf8().len() + 1
	}
	{ end: source.to_utf8().len(), header: False }
}

starts_cell : Str, U8 -> Bool
starts_cell = |line, separator| {
	bytes = line.to_utf8()
	var $index = 0
	while $index < bytes.len() and [32, 9].contains(bytes.get($index) ?? 0) {
		$index = $index + 1
	}
	var $span_end = $index
	var $has_digits = False
	while ($span_end < bytes.len()) and (bytes.get($span_end) ?? 0) >= '0' and (bytes.get($span_end) ?? 0) <= '9' {
		$has_digits = True
		$span_end = $span_end + 1
	}
	if bytes.get($span_end) == Ok('.') {
		$span_end = $span_end + 1
		while ($span_end < bytes.len()) and (bytes.get($span_end) ?? 0) >= '0' and (bytes.get($span_end) ?? 0) <= '9' {
			$has_digits = True
			$span_end = $span_end + 1
		}
	}
	if $has_digits and ['*', '+'].contains(bytes.get($span_end) ?? 0) {
		$index = $span_end + 1
	}
	if ['<', '>', '^'].contains(bytes.get($index) ?? 0) {
		$index = $index + 1
		if bytes.get($index) == Ok('.') {
			$index = $index + 1
			if ['<', '>', '^'].contains(bytes.get($index) ?? 0) {
				$index = $index + 1
			}
		}
	} else if bytes.get($index) == Ok('.') and ['<', '>', '^'].contains(bytes.get($index + 1) ?? 0) {
		$index = $index + 2
	}
	if (bytes.get($index) ?? 0) >= 'a' and (bytes.get($index) ?? 0) <= 'z' {
		$index = $index + 1
	}
	bytes.get($index) == Ok(separator)
}

escape : Str -> Str
escape = |text| Str.replace_each(Str.replace_each(Str.replace_each(text, "&", "&amp;"), "<", "&lt;"), ">", "&gt;")

percent : U64, U64 -> Str
percent = |width, total| {
	# Asciidoctor prints four fractional digits, dropping trailing zeroes.
	scaled = if (total == 0) 1000000 else width * 1000000 / total
	whole = scaled / 10000
	var $fraction = (10000 + scaled % 10000).to_str().to_utf8().drop_first(1)
	while $fraction.last() == Ok('0') {
		$fraction = $fraction.drop_last(1)
	}
	if ($fraction.is_empty()) whole.to_str() else "${whole.to_str()}.${Str.from_utf8_lossy($fraction)}"
}

render_table : AsciiDocTable.Table, List(AsciiDocTable.Attribute), (Str -> Str) -> Str
render_table = |table, attrs, render_blocks| {
	render_table_state(table, Raw(attrs), {}, |text, state| { html: AsciiDocInline.render(text, attrs), state }, |text, state| { html: render_blocks(text), state }).html
}

render_table_state : AsciiDocTable.Table, Environment, state, (Str, state -> { html : Str, state : state }), (Str, state -> { html : Str, state : state }) -> { html : Str, state : state }
render_table_state = |table, attrs, state, render_inline, render_blocks| {
	var $state = state
	raw_frame = attr(attrs, "frame", attr(attrs, "table-frame", "all"))
	frame = if (raw_frame == "topbot") "ends" else raw_frame
	grid = attr(attrs, "grid", attr(attrs, "table-grid", "all"))
	width = attr(attrs, "width", "")
	roles = attr(attrs, "role", "")
	autowidth = option(attrs, "autowidth")
	stretch = if (autowidth and width == "") " fit-content" else if (width == "" or width == "100" or width == "100%") " stretch" else ""
	role_class = if (roles == "") "" else " ${roles}"
	width_attr = if (width == "" or stretch == " stretch") "" else " style=\"width: ${width.trim()}${if (width.ends_with("%")) "" else "%"};\""
	id = attr(attrs, "id", "")
	id_attr = if (id == "") "" else " id=\"${id}\""
	stripes = attr(attrs, "stripes", attr(attrs, "table-stripes", ""))
	float = attr(attrs, "float", "")
	extra = "${if (stripes == "") "" else " stripes-${stripes}"}${if (float == "") "" else " ${float}"}"
	var $html = "<table${id_attr} class=\"tableblock frame-${frame} grid-${grid}${stretch}${extra}${role_class}\"${width_attr}>\n"
	title = attr(attrs, "title", "")
	if title != "" {
		caption = match lookup_attr(attrs, "__rendered-title") {
			Ok(cached) => cached
			Err(_) => "Table ${attr(attrs, "__caption-number", "1")}. ${render_caption(title, attrs)}"
		}
		$html = Str.concat($html, "<caption class=\"title\">${caption}</caption>\n")
	}
	if table.rows.is_empty() {
		return { html: "${$html}</table>", state: $state }
	}
	$html = Str.concat($html, "<colgroup>\n")
	total = table.columns.fold(0.U128, |sum, col| sum + col.width.to_u128())
	var $assigned = 0.U64
	var $column_index = 0.U64
	for col in table.columns {
		$column_index = $column_index + 1
		amount = if ($column_index == table.columns.len()) 1000000 - $assigned else if (total == 0) 0 else (col.width.to_u128() * 1000000 / total).to_u64_try() ?? 0
		$assigned = $assigned + amount
		style = if (autowidth) "" else " style=\"width: ${percent(amount, 1000000)}%;\""
		$html = Str.concat($html, "<col${style}>\n")
	}
	$html = Str.concat($html, "</colgroup>\n")
	background = attr(attrs, "cellbgcolor", "")
	background_attr = if (background == "") "" else " style=\"background-color: ${background};\""
	default_column = column("")
	var $section = ""
	var $row_index = 0
	for row in table.rows {
		section = if (table.header and $row_index == 0) "thead" else if (table.footer and $row_index + 1 == table.rows.len()) "tfoot" else "tbody"
		if section != $section {
			if $section != "" {
				$html = Str.concat($html, "</${$section}>\n")
			}
			$html = Str.concat($html, "<${section}>\n")
			$section = section
		}
		$html = Str.concat($html, "<tr>\n")
		var $col_index = 0
		for current in row {
			col = table.columns.get($col_index) ?? default_column
			style = if (current.style == "") col.style else current.style
			halign = if (current.halign == "left") col.halign else current.halign
			valign = if (current.valign == "top") col.valign else current.valign
			tag = if (section == "thead" or style == "header") "th" else "td"
			spans = "${if (current.colspan > 1) " colspan=\"${current.colspan.to_str()}\"" else ""}${if (current.rowspan > 1) " rowspan=\"${current.rowspan.to_str()}\"" else ""}"
			$html = Str.concat($html, "<${tag} class=\"tableblock halign-${halign} valign-${valign}\"${spans}${background_attr}>")
			if section == "thead" {
				rendered = render_inline(current.text.trim(), $state)
				$state = rendered.state
				$html = Str.concat($html, rendered.html)
			} else if style == "asciidoc" {
				rendered = render_blocks(current.text, $state)
				$state = rendered.state
				$html = Str.concat($html, "<div class=\"content\">${rendered.html}</div>")
			} else if style == "literal" or style == "verse" {
				$html = Str.concat($html, "<div class=\"${if (style == "literal") "literal" else "verse"}\"><pre>${escape(current.text)}</pre></div>")
			} else {
				var $paragraph_index = 0.U64
				text = current.text.trim()
				for paragraph in (if (text == "") [] else text.split_on("\n\n")) {
					if $paragraph_index > 0 {
						$html = Str.concat($html, "\n")
					}
					rendered = render_inline(paragraph, $state)
					$state = rendered.state
					content = rendered.html
					styled = if (style == "strong") "<strong>${content}</strong>" else if (style == "emphasis") "<em>${content}</em>" else if (style == "monospaced") "<code>${content}</code>" else content
					$html = Str.concat($html, "<p class=\"tableblock\">${styled}</p>")
					$paragraph_index = $paragraph_index + 1
				}
			}
			$html = Str.concat($html, "</${tag}>\n")
			$col_index = $col_index + 1
		}
		$html = Str.concat($html, "</tr>\n")
		$row_index = $row_index + 1
	}
	if $section != "" {
		$html = Str.concat($html, "</${$section}>\n")
	}
	{ html: Str.concat($html, "</table>"), state: $state }
}

expect AsciiDocTable.parse("|A |B\n\n|C |D", []).rows.len() == 2
expect AsciiDocTable.parse("|A \\| B", []).rows == [[{ text: "A | B", colspan: 1, rowspan: 1, style: "", halign: "left", valign: "top" }]]
expect AsciiDocTable.parse("|A |B\n\n2+|Wide", []).rows.last().map_ok(|row| row.len()) == Ok(1)

# TSV and explicitly separated CSV use the same quote rules as CSV without
# replacing separator-looking bytes inside a field or quoted value.
delimited_records : Str, U8 -> List(List(Str))
delimited_records = |source, separator| {
	bytes = source.to_utf8()
	var $i = 0.U64
	var $quoted = False
	var $text = []
	var $row = []
	var $rows = []
	while $i < bytes.len() {
		b = bytes.get($i) ?? 0
		if $quoted {
			if b == 34 and bytes.get($i + 1) == Ok(34) {
				$text = $text.append(34)
				$i = $i + 2
			}
			else if b == 34 {
				$quoted = False
				$i = $i + 1
			}
			else {
				$text = $text.append(b)
				$i = $i + 1
			}
		} else if b == 34 and $text.is_empty() {
			$quoted = True
			$i = $i + 1
		}
		else if b == separator {
			$row = $row.append(Str.from_utf8_lossy($text))
			$text = []
			$i = $i + 1
		}
		else if b == 10 or b == 13 {
			$row = $row.append(Str.from_utf8_lossy($text))
			$text = []
			$rows = $rows.append($row)
			$row = []
			$i = $i + (if (b == 13 and bytes.get($i + 1) == Ok(10)) 2 else 1)
		} else {
			$text = $text.append(b)
			$i = $i + 1
		}
	}
	if !$text.is_empty() or !$row.is_empty() {
		$rows = $rows.append($row.append(Str.from_utf8_lossy($text)))
	}
	$rows
}
expect delimited_records("A,B\tC\nD\tE", 9) == [["A,B", "C"], ["D", "E"]]
expect delimited_records("\"A,B\"\t\"C\tD\"", 9) == [["A,B", "C\tD"]]
expect AsciiDocTable.parse(".3+|A |B .2+|C\n|D\n|E |F", [{ name: "cols", value: "s,e,m" }]).rows.map(List.len) == [3, 1, 2]

expect AsciiDocTable.parse("|A |B\n|Incomplete", []).rows.len() == 1
expect AsciiDocTable.parse(".3+|A\n|B", [{ name: "cols", value: "1" }]).rows.len() == 1

expect {
	table = parse_table("|o\n\nne |two\n|three |four", [])
	table.columns.len() == 2 and table.rows.len() == 2 and !table.header
}
expect {
	table = parse_table("|o----\nne |two\n|three |four", [])
	table.columns.len() == 2 and table.rows.len() == 2
}
expect {
	table = parse_table("|one |two\n\\|three |four", [])
	table.columns.len() == 3 and table.rows.len() == 1
}
expect {
	table = parse_table("|one\n\n |two\n|three |four", [])
	table.columns.len() == 1 and table.rows.len() == 4 and table.header
}

expect {
	html = AsciiDocTable.render("|A", [{ name: "frame", value: "none" }, { name: "frame", value: "ends" }, { name: "cellbgcolor", value: "pink" }])
	html.contains("frame-ends") and html.contains("background-color: pink;") and !html.contains("frame-none")
}
expect {
	html = AsciiDocTable.render("|A", [{ name: "title", value: "Unused *source*" }, { name: "__rendered-title", value: "" }])
	html.contains("<caption class=\"title\"></caption>") and !html.contains("Unused")
}

expect AsciiDocTable.render("|A", [{ name: "autowidth-option", value: "" }]).contains("fit-content")
expect AsciiDocTable.render("|A", [{ name: "options", value: " footer, autowidth " }]).contains("fit-content")

# Table options use their last binding, while inline captions retain the
# ordinary first-binding pipeline through the view-aware inline renderer.
expect {
	attrs = [{ name: "frame", value: "none" }, { name: "frame", value: "ends" }, { name: "title", value: "source" }, { name: "__rendered-title", value: "old" }, { name: "__rendered-title", value: "" }, { name: "options", value: "header" }, { name: "options", value: "footer,autowidth" }]
	view = AsciiDocAttributes.indexed(AsciiDocAttributes.with_snapshot(attrs), AsciiDocAttributes.empty).view
	inline = |text, count| { html: "inline:${text}", state: count + 1.U64 }
	blocks = |text, count| { html: "block:${text}", state: count + 10.U64 }
	raw = AsciiDocTable.render_state("|A |B\n|C |D", attrs, 0.U64, inline, blocks)
	shared = AsciiDocTable.render_view_state("|A |B\n|C |D", view, 0.U64, inline, blocks)
	view.indexed and raw == shared and shared.html.contains("frame-ends") and shared.html.contains("<tfoot>") and shared.html.contains("<caption class=\"title\"></caption>") and lookup_attr(Indexed(view), "__rendered-title") == Ok("") and lookup_attr(Indexed(view), "missing") == Err(Missing)
}

expect {
	cases = [
		{ source: "|A |B\n|C |D", attrs: [{ name: "options", value: "header,footer" }, { name: "cols", value: "a,s" }] },
		{ source: "A,B\nC,D", attrs: [{ name: "format", value: "csv" }, { name: "options", value: "header" }] },
		{ source: "A\tB\nC\tD", attrs: [{ name: "format", value: "tsv" }, { name: "options", value: "footer" }] },
		{ source: "A:B\nC:D", attrs: [{ name: "format", value: "dsv" }, { name: "separator", value: ":" }, { name: "cols", value: "l,m" }] },
		{ source: "", attrs: [{ name: "id", value: "old" }, { name: "id", value: "new" }, { name: "width", value: "" }] },
	]
	cases.all(
		|case| {
			view = AsciiDocAttributes.indexed(AsciiDocAttributes.with_snapshot(case.attrs), AsciiDocAttributes.empty).view
			inline = |text, count| { html: "inline:${text}", state: count + 1.U64 }
			blocks = |text, count| { html: "block:${text}", state: count + 10.U64 }
			AsciiDocTable.render_state(case.source, case.attrs, 0.U64, inline, blocks) == AsciiDocTable.render_view_state(case.source, view, 0.U64, inline, blocks)
		},
	)
}

expect {
	attrs = [{ name: "title", value: "{name}" }, { name: "name", value: "*First*" }, { name: "name", value: "*Last*" }]
	view = AsciiDocAttributes.indexed(AsciiDocAttributes.with_snapshot(attrs), AsciiDocAttributes.empty).view
	inline = |text, count| { html: text, state: count }
	blocks = |text, count| { html: text, state: count }
	AsciiDocTable.render_state("|A", attrs, 0.U64, inline, blocks) == AsciiDocTable.render_view_state("|A", view, 0.U64, inline, blocks)
}

expect {
	base = [{ name: "id", value: "source" }, { name: "title", value: "Source title" }, { name: "__rendered-title", value: "Old caption" }]
	overrides = [{ name: "id", value: "local" }, { name: "title", value: "Local title" }, { name: "__rendered-title", value: "" }]
	view = AsciiDocAttributes.with_after(AsciiDocAttributes.indexed(AsciiDocAttributes.with_snapshot(base), AsciiDocAttributes.empty).view, overrides)
	inline = |text, count| { html: text, state: count }
	blocks = |text, count| { html: text, state: count }
	raw = AsciiDocTable.render_state("|A", base.concat(overrides), 0.U64, inline, blocks)
	shared = AsciiDocTable.render_view_state("|A", view, 0.U64, inline, blocks)
	raw == shared and shared.html.contains("id=\"local\"") and shared.html.contains("<caption class=\"title\"></caption>")
}
