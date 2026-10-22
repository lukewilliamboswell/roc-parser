import AsciiDocInline
import AsciiDocAttributes
import AsciiDocSource
import unicode.Case
import unicode.GeneralCategory
import unicode.Scalar

## Pure AsciiDoc preprocessing. Included sources are supplied by the caller.
AsciiDocPreprocess :: [].{
	Attribute : { name : Str, value : Str }
	Include : { path : Str, source : Str }
	Options : { includes : List(Include), attributes : List(Attribute), max_depth : U64, expand_includes : Bool }
	Line : { text : Str, attributes : List(Attribute), path : Str, line : U64, offset : U64 }
	Error : { path : Str, line : U64, offset : U64, message : Str }
	Result : { source : Str, attributes : List(Attribute), lines : List(Line) }
	standard : List(Attribute)
	standard = standard_attributes

	defaults : Options
	defaults = { includes: [], attributes: [], max_depth: 16, expand_includes: False }

	run : Str, Options -> Try(Result, [PreprocessError(Error)])
	run = |source, options| {
		result = run_scoped(source, options)?
		Ok({ source: result.source, attributes: AsciiDocAttributes.materialize(result.attributes), lines: result.lines.map(|line| { text: line.text, path: line.path, line: line.line, offset: line.offset, attributes: AsciiDocAttributes.materialize(line.attributes) }) })
	}

	## The document parser consumes immutable scopes directly. The legacy
	## preprocessing inspection API materializes only at its explicit boundary.
	ScopedLine : { text : Str, attributes : AsciiDocAttributes.Attributes, path : Str, line : U64, offset : U64 }
	ScopedResult : { source : Str, attributes : AsciiDocAttributes.Attributes, lines : List(ScopedLine) }
	run_scoped : Str, Options -> Try(ScopedResult, [PreprocessError(Error)])
	run_scoped = |source, options| run_scoped_checked(source, options).map_ok(|result| result.processed)

	## Parse once and retain actual active include directives left unexpanded.
	## Used by document quotation validation without inspecting rendered text.
	run_scoped_checked : Str, Options -> Try({ processed : ScopedResult, unexpanded_includes : List(Error) }, [PreprocessError(Error)])
	run_scoped_checked = |source, options| {
		attributes = AsciiDocSource.new_standard_parsed_options(source_namespace(source, options), options.attributes)
		result = process(source, "", attributes, options, [], 0)?
		Ok({ processed: { source: join(result.lines.map(|line| line.text), "\n"), attributes: AsciiDocAttributes.from_store(result.attributes), lines: result.lines }, unexpanded_includes: result.unexpanded_includes })
	}

	## Expand defined attribute references, preserving unknown references and
	## escaped references for the inline substitution pass.
	substitute : Str, List(Attribute) -> Str
	substitute = |source, attributes| expand(source, attributes, 0)

	## Published as `AsciiDoc.IncludeProblem`, which documents each variant.
	IncludeProblem : [MissingTag(Str), UnclosedTag(Str), UnmatchedEnd(Str), InvalidLines(Str), EmptySelection]

	## Published as `AsciiDoc.IncludeSelection`.
	IncludeSelection : { text : Str, lines : List(U64), problems : List(IncludeProblem) }

	## Select an include's lines exactly as `include::` expansion does, since
	## both call `include_lines`.
	select_include : Str, Str -> IncludeSelection
	select_include = |source, options| select_include_with(source, options, 0)

	## `select_include` in a document whose `tabsize` is `tab_size`.
	select_include_with : Str, Str, U64 -> IncludeSelection
	select_include_with = |source, options, tab_size| {
		selected = include_lines(source, options, OtherFile, if (tab_size > max_columns) max_columns else tab_size)
		{ text: join(selected.lines.map(|line| line.text), "\n"), lines: selected.lines.map(|line| line.line), problems: selected.problems }
	}

	## A verbatim block's content as Asciidoctor's `build_block` adjusts it:
	## with the block's own `indent` attribute, `adjust_indentation` with
	## that width; without one, only tab expansion. `tabsize` is the block's
	## `tabsize` attribute or, when the block has none, the document's.
	verbatim_source : Str, Try(Str, [Missing]), Str -> Str
	verbatim_source = |source, indent, tabsize| {
		size = parse_tab_size(tabsize)
		match indent {
			Ok(width) => Str.join_with(adjust_indentation(source.split_on("\n"), reindent(width), size), "\n")
			Err(_) => if (size > 0 and source.contains("\t")) Str.join_with(source.split_on("\n").map(|line| expand_tabs(line, size)), "\n") else source
		}
	}
}

source_namespace : Str, AsciiDocPreprocess.Options -> Str
source_namespace = |source, options| {
	var $namespace = "ADOCSource1:${frame(source)}${options.max_depth.to_str()}:${if (options.expand_includes) "1" else "0"}:${options.attributes.len().to_str()}:"
	for attr in options.attributes {
		$namespace = $namespace.concat("${frame(attr.name)}${frame(attr.value)}")
	}
	$namespace = $namespace.concat("${options.includes.len().to_str()}:")
	for entry in options.includes {
		$namespace = $namespace.concat("${frame(entry.path)}${frame(entry.source)}")
	}
	$namespace
}

frame : Str -> Str
frame = |value| "${value.count_utf8_bytes().to_str()}:${value}"

condition_source : Str, AsciiDocSource.Store -> Bool
condition_source = |source, attributes| {
	if (source.contains(",")) source.split_on(",").any(|name| AsciiDocSource.lookup(attributes, lower(trim(name))).is_ok()) else source.split_on("+").all(|name| AsciiDocSource.lookup(attributes, lower(trim(name))).is_ok())
}

Context : { definition_allowed : Bool, opaque : Str, header : Bool, lint_comment : Str }

State : { lines : List(AsciiDocPreprocess.ScopedLine), attributes : AsciiDocSource.Store, context : Context, unexpanded_includes : List(AsciiDocPreprocess.Error) }

process : Str, Str, AsciiDocSource.Store, AsciiDocPreprocess.Options, List(Str), U64 -> Try(State, [PreprocessError(AsciiDocPreprocess.Error)])
process = |source, path, attributes, options, stack, depth| process_lines(split_lines(source, AsciiDocFile), path, attributes, options, stack, depth, AsciiDocFile, { definition_allowed: True, opaque: "", lint_comment: "", header: source.starts_with("= ") or source.starts_with("﻿= ") })

SourceLine : { text : Str, line : U64, offset : U64 }

## The lines of an included file before selection, as Asciidoctor reads
## them: only a CR that ends a line is removed.
source_lines : Str -> List(SourceLine)
source_lines = |source| split_lines(source, OtherFile)

## Asciidoctor removes trailing whitespace from each line of AsciiDoc source
## as it reads it, before it recognizes any directive or block. It keeps
## trailing whitespace in an included file that isn't AsciiDoc.
split_lines : Str, IncludeFile -> List(SourceLine)
split_lines = |source, file| {
	var $number = 0
	var $offset = 0
	var $lines = []
	for raw in source.split_on("\n") {
		$number = $number + 1
		bom = $number == 1 and raw.starts_with("﻿")
		text = if (bom) drop(raw, 3) else raw
		$lines = $lines.append({ text: if (file == AsciiDocFile) trim_line_end(text) else remove_cr(text), line: $number, offset: $offset + (if (bom) 3 else 0) })
		$offset = $offset + raw.to_utf8().len() + 1
	}
	if (source.ends_with("\n")) $lines.drop_last(1) else $lines
}

## Asciidoctor processes preprocessor directives only in AsciiDoc source. In
## an included file that isn't AsciiDoc, `ifdef::`, `include::` and the
## other directives, escaped or not, are ordinary text.
process_lines : List(SourceLine), Str, AsciiDocSource.Store, AsciiDocPreprocess.Options, List(Str), U64, IncludeFile, Context -> Try(State, [PreprocessError(AsciiDocPreprocess.Error)])
process_lines = |input_lines, path, attributes, options, stack, depth, file, context| {
	directives = file == AsciiDocFile
	var $attributes = attributes
	var $snapshot_ready = False
	var $snapshot_attrs = AsciiDocAttributes.flat([])

	var $lines = []
	var $unexpanded_includes = []
	var $conditions = []
	var $continuation = ""
	var $expanded_size = 0
	var $definition_allowed = context.definition_allowed
	var $opaque = context.opaque
	var $lint_comment = context.lint_comment
	var $header = context.header
	for input_line in input_lines {
		emitted_before = $lines.len()
		number = input_line.line
		offset = input_line.offset
		text = input_line.text
		var $definition_text = ""
		active = $conditions.all(|condition| condition.enabled)
		# Track comment blocks for lint metadata without changing the ordinary
		# preprocessor's directive or attribute handling. Delimiters may have
		# any number of slashes >= 4; listing/literal includes remain active.
		if active {
			if $lint_comment == text {
				$lint_comment = ""
			} else if $lint_comment == "" and $opaque == "" and text.starts_with("////") and text.to_utf8().all(|byte| byte == '/') {
				$lint_comment = text
			}
		}
		if directives and text.starts_with("endif::") and complete_directive(text) {
			closing = directive(drop(text, 7))
			target = lower(closing.target)
			opened = $conditions.last() ?? { enabled: False, target: "" }
			if closing.body == "" and !$conditions.is_empty() and (target == "" or target == opened.target) {
				$conditions = $conditions.drop_last(1)
			}
		} else if directives and (text.starts_with("ifdef::") or text.starts_with("ifndef::")) and complete_directive(text) {
			negative = text.starts_with("ifndef::")
			body = drop(text, if (negative) 8 else 7)
			parts = directive(body)
			defined = condition_source(parts.target, $attributes)
			enabled = if (negative) !defined else defined
			if parts.target != "" and parts.body == "" {
				$conditions = $conditions.append({ enabled: active and enabled, target: lower(parts.target) })
			} else if parts.target != "" and active and enabled {
				if $opaque == "" and (!is_definition(parts.body) or $definition_allowed) {
					nested = process_lines([{ text: trim_line_end(parts.body), line: number, offset }], path, $attributes, options, stack, depth, AsciiDocFile, { definition_allowed: $definition_allowed, opaque: $opaque, header: $header, lint_comment: $lint_comment })?
					$unexpanded_includes = $unexpanded_includes.concat(nested.unexpanded_includes)
					$attributes = nested.attributes
					$snapshot_ready = False
					$definition_allowed = nested.context.definition_allowed
					$opaque = nested.context.opaque
					$lint_comment = nested.context.lint_comment
					$header = nested.context.header
					for line in nested.lines {
						$lines = $lines.append(line)
					}
				} else {
					if !$snapshot_ready {
						$snapshot_attrs = AsciiDocAttributes.from_store($attributes)
						$snapshot_ready = True
					}
					$lines = $lines.append({ text: trim_line_end(parts.body), attributes: $snapshot_attrs, path, line: number, offset })
				}
			}
		} else if directives and text.starts_with("ifeval::") and complete_directive(text) {
			parsed = directive(drop(text, 8))
			expression = parsed.body
			if parsed.target == "" and ["==", "!=", "<=", ">=", "<", ">"].any(|operator| expression.contains(operator)) {
				var $enabled = False
				if active {
					if expression.contains("{") {
						$enabled = evaluate_lookup(expression, [], Stored($attributes))
					} else {
						## Literal expressions never perform attribute lookups.
						## Keep the reader index lazy for subsequent directives.
						$enabled = evaluate_with(expression, [])
					}
				}
				$conditions = $conditions.append({ enabled: $enabled, target: "" })
			}
		} else if active {
			if $continuation != "" {
				$continuation = Str.concat($continuation, trim_start(text))
				if continues($continuation) {
					$continuation = Str.concat(strip_continuation($continuation), " ")
				} else {
					$definition_text = $continuation
					$continuation = ""
				}
			} else if is_definition(text) and $definition_allowed and $opaque == "" {
				if continues(text) {
					$continuation = Str.concat(strip_continuation(text), " ")
				} else {
					$definition_text = text
				}
			} else if directives and text.starts_with("include::") and complete_directive(text) {
				parsed = directive(expand_lookup(drop(text, 9), [], Stored($attributes), 0))
				target = expand_lookup(parsed.target, [], Stored($attributes), 0)
				resolved = resolve(path, target)
				found = if (options.expand_includes) options.includes.find_first(|entry| entry.path == resolved) else Err(NotFound)
				match found {
					Ok(entry) => {
						if depth >= options.max_depth or stack.contains(resolved) {
							return Err(PreprocessError({ path, line: number, offset, message: Str.concat("include expansion limit or cycle: ", resolved) }))
						}
						if invalid_path(target) or resolved == "" or resolved == ".." or resolved.starts_with("../") {
							return Err(PreprocessError({ path, line: number, offset, message: "virtual includes prohibit absolute paths, URIs and escaping the root" }))
						}
						included = include_file(resolved)
						selected = include_lines(entry.source, parsed.body, included, parse_tab_size(AsciiDocSource.lookup($attributes, "tabsize") ?? ""))
						previous_offset = AsciiDocSource.lookup($attributes, "leveloffset")
						include_offset = option(parsed.body, "leveloffset")
						base_offset = I64.from_str((previous_offset ?? "0").replace_each("+", "")) ?? 0
						delta = I64.from_str(include_offset.replace_each("+", "")) ?? 0
						effective_offset = if (include_offset.starts_with("+") or include_offset.starts_with("-")) base_offset + delta else delta
						nested_attrs = if (include_offset == "") $attributes else AsciiDocSource.set_parsed($attributes, "leveloffset", effective_offset.to_str())
						nested = process_lines(selected.lines, resolved, nested_attrs, options, stack.append(resolved), depth + 1, included, { definition_allowed: $definition_allowed, opaque: $opaque, header: $header, lint_comment: $lint_comment })?
						$unexpanded_includes = $unexpanded_includes.concat(nested.unexpanded_includes)
						$attributes = if (include_offset == "") nested.attributes else match previous_offset {
							Ok(previous) => AsciiDocSource.set_parsed(nested.attributes, "leveloffset", previous)
							Err(_) => AsciiDocSource.unset_parsed(nested.attributes, "leveloffset")
						}
						$snapshot_ready = False
						$definition_allowed = nested.context.definition_allowed
						$opaque = nested.context.opaque
						$lint_comment = nested.context.lint_comment
						$header = nested.context.header
						shifted = nested.lines
						for line in shifted {
							$lines = $lines.append(line)
						}
					}
					Err(_) => {
						if options.expand_includes and !parsed.body.contains("opts=optional") and !parsed.body.contains("options=optional") {
							return Err(PreprocessError({ path, line: number, offset, message: Str.concat("virtual include not supplied: ", resolved) }))
						}
						if !options.expand_includes {
							if $lint_comment == "" {
								$unexpanded_includes = $unexpanded_includes.append({ path, line: number, offset, message: "unexpanded_includes: include was not expanded: ${target}" })
							}
							if !$snapshot_ready {
								$snapshot_attrs = AsciiDocAttributes.from_store($attributes)
								$snapshot_ready = True
							}
							$lines = $lines.append({ text: Str.concat(Str.concat("link:", target), "[role=include]"), attributes: $snapshot_attrs, path, line: number, offset })
						}
					}
				}
			} else {
				emitted = if (directives and (text.starts_with("\\include::") or text.starts_with("\\ifdef::") or text.starts_with("\\ifndef::") or text.starts_with("\\endif::") or text.starts_with("\\ifeval::")) and complete_directive(drop(text, 1))) drop(text, 1) else text
				if !$snapshot_ready {
					$snapshot_attrs = AsciiDocAttributes.from_store($attributes)
					$snapshot_ready = True
				}
				$lines = $lines.append({ text: emitted, attributes: $snapshot_attrs, path, line: number, offset })
				if $opaque == text {
					$opaque = ""
				}
				else if $opaque == "" and (text == "----" or text == "...." or text == "++++" or text == "////" or table_delimiter(text)) {
					$opaque = text
				}
				if text == "" {
					$header = False
				}
				if !(text == "//" or text.starts_with("// ")) or $opaque != "" {
					$definition_allowed = text == "" or heading_prefix(text) > 0 or $header or text == "----" or text == "...." or text == "++++" or text == "////" or text == "====" or text == "****" or text == "____" or text == "--" or table_delimiter(text)
				}
			}
		}
		if $definition_text != "" {
			$snapshot_ready = False
			parts = split_first(drop($definition_text, 1), ":")
			unset = parts.before.starts_with("!") or parts.before.ends_with("!")
			name = normalize_attribute_name(strip_suffix(if (parts.before.starts_with("!")) drop(parts.before, 1) else parts.before, "!"))
			# Like Asciidoctor's attribute entry pattern, keep any trailing
			# whitespace, which only a line from an included file that isn't
			# AsciiDoc still has.
			value = if (unset) "" else AsciiDocInline.attribute_value_source(trim_start(parts.after), $attributes)
			$attributes = AsciiDocSource.define_parsed($attributes, name, value, unset)
		}

		for line in $lines.drop_first(emitted_before) {
			$expanded_size = $expanded_size + line.text.to_utf8().len() + 1
		}
		if $expanded_size > 1048576 {
			return Err(PreprocessError({ path, line: number, offset, message: "maximum expanded source size exceeded (1048576 bytes)" }))
		}
	}
	Ok({ lines: $lines, attributes: $attributes, context: { definition_allowed: $definition_allowed, opaque: $opaque, header: $header, lint_comment: $lint_comment }, unexpanded_includes: $unexpanded_includes })
}

is_definition : Str -> Bool
is_definition = |source| {
	if !source.starts_with(":") or source.starts_with("::") {
		return False
	}
	tail = drop(source, 1)
	parts = split_first(tail, ":")
	tail.contains(":") and starts_word(if (parts.before.starts_with("!")) drop(parts.before, 1) else parts.before) and (parts.after == "" or parts.after.starts_with(" ") or parts.after.starts_with("\t"))
}

Definition : { attributes : List(AsciiDocPreprocess.Attribute), index : Dict(Str, U64) }

index_attributes : List(AsciiDocPreprocess.Attribute) -> Dict(Str, U64)
index_attributes = |attributes| {
	var $index = Dict.empty()
	var $position = 0
	for attribute in attributes {
		$index = $index.insert(attribute.name, $position)
		$position = $position + 1
	}
	$index
}

# Build the name index lazily, so documents without definitions retain the
# constant shared defaults. Fresh header bindings append in constant amortized
# time, without scanning or copying all earlier values and revision markers.
# Replacement/unset keeps the existing canonical remove-then-append ordering.
define_indexed : Str, List(AsciiDocPreprocess.Attribute), Dict(Str, U64), Bool -> Definition
define_indexed = |source, attributes, index, ready| {
	current = if (ready) index else index_attributes(attributes)
	parts = split_first(drop(source, 1), ":")
	unset = parts.before.starts_with("!") or parts.before.ends_with("!")
	name = normalize_attribute_name(strip_suffix(if (parts.before.starts_with("!")) drop(parts.before, 1) else parts.before, "!"))
	revision_name = Str.concat("__revision:", name)
	value = if (unset) "" else definition_value(trim(parts.after), attributes, current)
	if !current.contains(name) and !current.contains(revision_name) {
		var $updated = attributes
		var $index = current
		if !unset {
			$index = $index.insert(name, $updated.len())
			$updated = $updated.append({ name, value })
		}
		$index = $index.insert(revision_name, $updated.len())
		$updated = $updated.append({ name: revision_name, value: "1" })
		{ attributes: $updated, index: $index }
	} else {
		revision = match current.get(revision_name) {
			Ok(position) => U64.from_str((attributes.get(position) ?? { name: "", value: "0" }).value) ?? 0
			Err(_) => 0
		}
		remaining = attributes.keep_if(|attribute| attribute.name != name and attribute.name != revision_name)
		defined = if (unset) remaining else remaining.append({ name, value })
		updated = defined.append({ name: revision_name, value: (revision + 1).to_str() })
		{ attributes: updated, index: index_attributes(updated) }
	}
}

## Ordinary definition substitutions only consult explicitly referenced names.
## Select those bindings through the reader's current index, then retain the
## canonical inline escape/substitution/cap pipeline. Inserted values stay
## opaque. Stateful, escaped and pass-macro inputs retain the full environment.
definition_value : Str, List(AsciiDocPreprocess.Attribute), Dict(Str, U64) -> Str
definition_value = |source, attributes, index| {
	if source.contains("\\") or (source.starts_with("pass:") and source.ends_with("]")) {
		return AsciiDocInline.attribute_value(source, attributes)
	}
	if !source.contains("{") {
		return AsciiDocInline.attribute_value(source, [])
	}
	bytes = source.to_utf8()
	var $position = 0
	var $selected = []
	var $seen = Dict.empty()
	while $position < bytes.len() {
		if bytes.get($position) == Ok('{') {
			var $end = $position + 1
			while $end < bytes.len() and bytes.get($end) != Ok('}') {
				byte = bytes.get($end) ?? 0

				## Special characters inside a name would be escaped before
				## canonical lookup; nested and stateful names need full state.
				if byte == '{' or byte == ':' or byte == '<' or byte == '>' or byte == '&' {
					return AsciiDocInline.attribute_value(source, attributes)
				}
				$end = $end + 1
			}
			if $end == bytes.len() {
				return AsciiDocInline.attribute_value(source, attributes)
			}
			name = slice(source, $position + 1, $end - $position - 1).with_ascii_lowercased()
			if !$seen.contains(name) {
				$seen = $seen.insert(name, True)
				match index.get(name) {
					Ok(attribute_position) => match attributes.get(attribute_position) {
						Ok(attribute) => {
							$selected = $selected.append(attribute)
						}
						Err(_) => {}
					}
					Err(_) => {}
				}
			}
			$position = $end + 1
		} else {
			$position = $position + 1
		}
	}
	AsciiDocInline.attribute_value(source, $selected)
}

condition : Str, Dict(Str, U64) -> Bool
condition = |source, index| {
	if source.contains(",") {
		source.split_on(",").any(|name| index.contains(lower(trim(name))))
	} else {
		source.split_on("+").all(|name| index.contains(lower(trim(name))))
	}
}

evaluate : Str -> Bool
evaluate = |source| evaluate_with(source, [])

IntegerValue : { negative : Bool, digits : Str }

ExpressionValue : [Text(Str), Integer(IntegerValue), Floating(F64), Truth(Bool), Missing]

## Quotes classify operands before attribute expansion: "10" remains a string.
evaluate_with : Str, List(AsciiDocPreprocess.Attribute) -> Bool
evaluate_with = |source, attributes| evaluate_lookup(source, attributes, FirstMatch)

## Reader lookups borrow the existing name index; standalone substitutions
## retain first-match semantics even for caller-provided duplicate names.
AttributeLookup : [FirstMatch, Indexed(Dict(Str, U64)), Stored(AsciiDocSource.Store)]

evaluate_lookup : Str, List(AsciiDocPreprocess.Attribute), AttributeLookup -> Bool
evaluate_lookup = |source, attributes, lookup| {
	## EvalExpressionRx uses the first operator after a nonempty left operand;
	## it does not interpret quotes while finding that operator.
	text = trim(source)
	bytes = text.to_utf8()
	var $index = 1
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0
		length = if ((byte == '=' or byte == '!' or byte == '<' or byte == '>') and next == '=') 2 else if (byte == '<' or byte == '>') 1 else 0
		if length > 0 and $index + length < bytes.len() {
			operator = slice(text, $index, length)
			left = expression_value(trim(slice(text, 0, $index)), attributes, lookup)
			right = expression_value(trim(drop(text, $index + length)), attributes, lookup)
			return match (left, right) {
				(Text(a), Text(b)) => compare_strings(a, b, operator)
				(Integer(a), Integer(b)) => numeric_relation(integer_order(a, b), operator)
				(Floating(a), Floating(b)) => numeric_relation(if (a < b) -1 else if (a > b) 1 else 0, operator)
				(Integer(a), Floating(b)) => numeric_relation(integer_float_order(a, b), operator)
				(Floating(a), Integer(b)) => numeric_relation(0 - integer_float_order(b, a), operator)
				_ => if (operator == "==") left == right else if (operator == "!=") left != right else False
			}
		}
		$index = $index + 1
	}
	False
}

expression_value : Str, List(AsciiDocPreprocess.Attribute), AttributeLookup -> ExpressionValue
expression_value = |source, attributes, lookup| {
	unquoted = unquote(source)
	quoted = unquoted != source

	## Missing references drop in conditional expressions (unlike normal text).
	var $value = expand_lookup(unquoted, attributes, lookup, 0)
	bytes = $value.to_utf8()
	var $index = 0
	var $cleaned = ""
	while $index < bytes.len() {
		if bytes.get($index) == Ok('{') {
			var $end = $index + 1
			while $end < bytes.len() and bytes.get($end) != Ok('}') {
				$end = $end + 1
			}
			if $end < bytes.len() {
				$index = $end + 1
			} else {
				$cleaned = Str.concat($cleaned, slice($value, $index, bytes.len() - $index))
				$index = bytes.len()
			}
		} else {
			var $end = $index + 1
			while $end < bytes.len() and bytes.get($end) != Ok('{') {
				$end = $end + 1
			}
			$cleaned = Str.concat($cleaned, slice($value, $index, $end - $index))
			$index = $end
		}
	}
	$value = $cleaned
	if quoted {
		return Text($value)
	}
	if $value == "" {
		return Missing
	}
	if $value == "true" {
		return Truth(True)
	}
	if $value == "false" {
		return Truth(False)
	}
	if trim($value) == "" {
		return Text(" ")
	}

	## Pinned reader.rb resolve_expr_val selects Float only when a dot occurs
	## anywhere in the value; otherwise Ruby String#to_i reads a decimal prefix.
	number = trim($value)
	bytes_number = number.to_utf8()
	negative = bytes_number.first() == Ok('-')
	start = if (negative or bytes_number.first() == Ok('+')) 1 else 0
	whole = numeric_digits(bytes_number, start)
	if !$value.contains(".") {
		return Integer(canonical_integer(negative, whole.digits))
	}
	var $end = whole.end
	var $prefix = whole.digits
	var $has_fraction = False
	if bytes_number.get($end) == Ok('.') {
		fraction = numeric_digits(bytes_number, $end + 1)
		if fraction.digits != "" {
			$prefix = Str.concat(if ($prefix == "") "0" else $prefix, Str.concat(".", fraction.digits))
			$end = fraction.end
			$has_fraction = True
		}
	}

	## Ruby ignores the exponent after a decimal point with no following digit.
	if ($has_fraction or bytes_number.get($end) != Ok('.')) and $prefix != "" and (bytes_number.get($end) == Ok('e') or bytes_number.get($end) == Ok('E')) {
		exponent_start = $end + 1
		exponent_sign = bytes_number.get(exponent_start) ?? 0
		exponent = numeric_digits(bytes_number, if (exponent_sign == '+' or exponent_sign == '-') exponent_start + 1 else exponent_start)
		if exponent.digits != "" {
			$prefix = Str.concat($prefix, Str.concat(if (exponent_sign == '-') "e-" else "e", exponent.digits))
		}
	}
	if $prefix == "" {
		return Floating(0.0)
	}
	prefix = if (negative) Str.concat("-", $prefix) else $prefix

	## A syntactically valid numeric prefix can fail F64 parsing only on overflow.
	Floating(F64.from_str(prefix) ?? if (negative) 0.0 - F64.infinity else F64.infinity)
}

numeric_digits : List(U8), U64 -> { digits : Str, end : U64 }
numeric_digits = |bytes, start| {
	var $end = start
	var $digits = []
	while $end < bytes.len() {
		byte = bytes.get($end) ?? 0
		if byte >= '0' and byte <= '9' {
			$digits = $digits.append(byte)
			$end = $end + 1
		} else if byte == '_' and !$digits.is_empty() and (bytes.get($end + 1) ?? 0) >= '0' and (bytes.get($end + 1) ?? 0) <= '9' {
			$end = $end + 1
		} else {
			break
		}
	}
	{ digits: Str.from_utf8_lossy($digits), end: $end }
}

canonical_integer : Bool, Str -> IntegerValue
canonical_integer = |negative, digits| {
	bytes = digits.to_utf8()
	var $start = 0
	while $start < bytes.len() and bytes.get($start) == Ok('0') {
		$start = $start + 1
	}
	if $start == bytes.len() {
		{ negative: False, digits: "0" }
	}
	else { negative, digits: drop(digits, $start) }
}

integer_order : IntegerValue, IntegerValue -> I64
integer_order = |a, b| {
	if a.negative != b.negative {
		return if (a.negative) -1 else 1
	}
	order = if (a.digits.to_utf8().len() < b.digits.to_utf8().len()) -1 else if (a.digits.to_utf8().len() > b.digits.to_utf8().len()) 1 else lexical_order(a.digits, b.digits)
	if (a.negative) 0 - order else order
}

lexical_order : Str, Str -> I64
lexical_order = |a, b| {
	left = a.to_utf8()
	right = b.to_utf8()
	var $index = 0
	while $index < left.len() and $index < right.len() {
		x = left.get($index) ?? 0
		y = right.get($index) ?? 0
		if x != y {
			return if (x < y) -1 else 1
		}
		$index = $index + 1
	}
	if (left.len() < right.len()) -1 else if (left.len() > right.len()) 1 else 0
}

numeric_relation : I64, Str -> Bool
numeric_relation = |order, operator| match operator {
	"==" => order == 0
	"!=" => order != 0
	"<=" => order <= 0
	">=" => order >= 0
	"<" => order < 0
	_ => order > 0
}

## Compare an arbitrary integer with a Float without rounding the integer.
## Every finite binary64 has a <=309 digit integer part; decode it exactly.
integer_float_order : IntegerValue, F64 -> I64
integer_float_order = |integer, float| {
	if float.is_infinite() {
		return if (float < 0.0) 1 else -1
	}
	negative = float < 0.0
	if integer.negative != negative {
		return if (integer.negative) -1 else 1
	}
	bits = float.to_bits()
	exponent = bits.shr_wrap(52).bitwise_and(2047)
	mantissa = bits.bitwise_and(4503599627370495) + if (exponent == 0) 0 else 4503599627370496
	var $digits = "0"
	var $fraction = False
	if exponent >= 1075 {
		$digits = binary_integer_digits(mantissa, exponent - 1075)
	} else {
		shift = if (exponent == 0) 1074 else 1075 - exponent
		if shift >= 64 {
			$fraction = mantissa != 0
		}
		else {
			$digits = mantissa.shr_wrap(shift.to_u8_wrap()).to_str()
			$fraction = mantissa.bitwise_and(U64.shl_wrap(1, shift.to_u8_wrap()) - 1) != 0
		}
	}
	floor = { negative, digits: $digits }
	order = integer_order(integer, floor)
	if order == 0 and $fraction {
		if (negative) 1 else -1
	} else order
}

## Base 10^9 limbs let each multiplication consume 32 binary exponent bits.
## A limb product plus carry fits U64: (10^9 - 1) * 2^32 + (2^32 - 1).
## This constructs at most 35 limbs for binary64, without repeatedly allocating
## and reversing the complete decimal string for every exponent bit.
binary_integer_digits : U64, U64 -> Str
binary_integer_digits = |mantissa, shift| {
	base = 1000000000.U64
	var $limbs = [mantissa % base]
	if mantissa >= base {
		$limbs = $limbs.append(mantissa // base)
	}
	var $remaining = shift
	while $remaining > 0 {
		step = if ($remaining > 32) 32 else $remaining
		factor = U64.shl_wrap(1, step.to_u8_wrap())
		var $next = []
		var $carry = 0.U64
		for limb in $limbs {
			product = limb * factor + $carry
			$next = $next.append(product % base)
			$carry = product // base
		}
		while $carry > 0 {
			$next = $next.append($carry % base)
			$carry = $carry // base
		}
		$limbs = $next
		$remaining = $remaining - step
	}
	var $index = $limbs.len() - 1
	var $output = ($limbs.get($index) ?? 0).to_str()
	while $index > 0 {
		$index = $index - 1
		chunk = ($limbs.get($index) ?? 0).to_str()
		$output = Str.concat($output, slice("000000000", 0, 9 - chunk.to_utf8().len()))
		$output = Str.concat($output, chunk)
	}
	$output
}

expect binary_integer_digits(1, 0) == "1"
expect binary_integer_digits(1, 32) == "4294967296"
expect binary_integer_digits(4503599627370496, 1) == "9007199254740992"
expect binary_integer_digits(9007199254740991, 32) == "38685626227668129295630336"
expect binary_integer_digits(1, 128) == "340282366920938463463374607431768211456"

expect binary_integer_digits(1, 31) == "2147483648"

expect binary_integer_digits(1, 33) == "8589934592"

expect binary_integer_digits(1, 64) == "18446744073709551616"

expect binary_integer_digits(9007199254740991, 63) == "83076749736557232833115904412745728"

expect binary_integer_digits(4503599627370496, 75) == "170141183460469231731687303715884105728"

compare : Dec, Dec, Str -> Bool
compare = |a, b, operator| match operator {
	"==" => a == b
	"!=" => a != b
	"<=" => a <= b
	">=" => a >= b
	"<" => a < b
	_ => a > b
}

expand : Str, List(AsciiDocPreprocess.Attribute), U64 -> Str
expand = |source, attributes, depth| expand_lookup(source, attributes, FirstMatch, depth)

expand_lookup : Str, List(AsciiDocPreprocess.Attribute), AttributeLookup, U64 -> Str
expand_lookup = |source, attributes, lookup, depth| {
	if depth >= 16 {
		return source
	}
	bytes = source.to_utf8()
	var $position = 0
	var $output = ""
	while $position < bytes.len() {
		current = bytes.get($position) ?? 0
		if current == '\\' and bytes.get($position + 1) == Ok('{') {
			$output = Str.concat($output, "\\{")
			$position = $position + 2
		} else if current == '{' {
			var $end = $position + 1
			while $end < bytes.len() and bytes.get($end) != Ok('}') {
				$end = $end + 1
			}
			if $end < bytes.len() {
				name = slice(source, $position + 1, $end - $position - 1)
				normalized_name = lower(name)
				match lookup_attribute(attributes, normalized_name, lookup) {
					Ok(attribute) => {
						$output = Str.concat($output, expand_lookup(attribute.value, attributes, lookup, depth + 1))
					}
					Err(_) => {
						$output = Str.concat($output, slice(source, $position, $end - $position + 1))
					}
				}
				$position = $end + 1
			} else {
				$output = Str.concat($output, "{")
				$position = $position + 1
			}
		} else {
			var $end = $position + 1
			while $end < bytes.len() and bytes.get($end) != Ok('{') and bytes.get($end) != Ok('\\') {
				$end = $end + 1
			}
			$output = Str.concat($output, slice(source, $position, $end - $position))
			$position = $end
		}
	}
	$output
}

lookup_attribute : List(AsciiDocPreprocess.Attribute), Str, AttributeLookup -> Try(AsciiDocPreprocess.Attribute, [NotFound])
lookup_attribute = |attributes, name, lookup| match lookup {
	Stored(store) => AsciiDocSource.lookup(store, name).map_ok(|value| { name, value }).map_err(|_| NotFound)
	FirstMatch => find_attribute(attributes, name)
	Indexed(index) => match index.get(name) {
		Ok(position) => attributes.get(position).map_err(|_| NotFound)
		Err(_) => Err(NotFound)
	}
}

## Indexed lookup keeps first-definition precedence without allocating an
## iterator callback or normalizing the reference name for every candidate.
find_attribute : List(AsciiDocPreprocess.Attribute), Str -> Try(AsciiDocPreprocess.Attribute, [NotFound])
find_attribute = |attributes, name| {
	var $index = 0
	while $index < attributes.len() {
		attribute = attributes.get($index) ?? { name: "", value: "" }
		if attribute.name == name {
			return Ok(attribute)
		}
		$index = $index + 1
	}
	Err(NotFound)
}

expect AsciiDocPreprocess.substitute("{CAFÉ}", [{ name: "café", value: "first" }, { name: "café", value: "second" }]) == "first"
expect AsciiDocPreprocess.substitute("{Missing}", [{ name: "other", value: "value" }]) == "{Missing}"

directive : Str -> { target : Str, body : Str }
directive = |source| {
	parts = split_first(source, "[")
	{ target: parts.before, body: strip_suffix(parts.after, "]") }
}

split_first : Str, Str -> { before : Str, after : Str }
split_first = |source, separator| {
	pieces = source.split_on(separator)
	{ before: pieces.first() ?? source, after: join(pieces.drop_first(1), separator) }
}

join : List(Str), Str -> Str
join = |pieces, separator| {
	var $result = ""
	var $first = True
	for piece in pieces {
		if !$first {
			$result = Str.concat($result, separator)
		}
		$result = Str.concat($result, piece)
		$first = False
	}
	$result
}

slice : Str, U64, U64 -> Str
slice = |source, start, len| Str.from_utf8_lossy(source.to_utf8().sublist({ start, len }))

drop : Str, U64 -> Str
drop = |source, count| Str.from_utf8_lossy(source.to_utf8().drop_first(count))

strip_suffix : Str, Str -> Str
strip_suffix = |source, suffix| if (source.ends_with(suffix)) slice(source, 0, source.to_utf8().len() - suffix.to_utf8().len()) else source

remove_cr : Str -> Str
remove_cr = |source| strip_suffix(source, "\r")

trim : Str -> Str
trim = |source| {
	bytes = source.to_utf8()
	var $start = 0
	var $end = bytes.len()
	while $start < $end and (bytes.get($start) == Ok(' ') or bytes.get($start) == Ok('\t')) {
		$start = $start + 1
	}
	while $end > $start and (bytes.get($end - 1) == Ok(' ') or bytes.get($end - 1) == Ok('\t')) {
		$end = $end - 1
	}
	if ($start == 0 and $end == bytes.len()) source else Str.from_utf8_lossy(bytes.sublist({ start: $start, len: $end - $start }))
}

trim_start : Str -> Str
trim_start = |source| {
	bytes = source.to_utf8()
	var $start = 0
	while bytes.get($start) == Ok(' ') or bytes.get($start) == Ok('\t') {
		$start = $start + 1
	}
	if ($start == 0) source else Str.from_utf8_lossy(bytes.drop_first($start))
}

lower : Str -> Str
lower = |source| {
	bytes = source.to_utf8()
	if bytes.all(|byte| byte < 128) {
		return Str.from_utf8_lossy(bytes.map(|byte| if (byte >= 'A' and byte <= 'Z') byte + 32 else byte))
	}
	match Case.to_lower(source, Case.unicode_default, Case.unlimited_limits) {
		Ok(result) => Case.result_text(result)
		Err(_) => source
	}
}

unquote : Str -> Str
unquote = |source| if ((source.starts_with("\"") and source.ends_with("\"")) or (source.starts_with("'") and source.ends_with("'"))) (if (source.to_utf8().len() < 2) "" else slice(source, 1, source.to_utf8().len() - 2)) else source

resolve : Str, Str -> Str
resolve = |path, target| {
	parts = path.split_on("/").drop_last(1)
	combined = if (target.starts_with("/")) target else join(parts.append(target), "/")
	var $resolved = []
	for part in combined.split_on("/") {
		if part == ".." {
			if $resolved.is_empty() or $resolved.last() == Ok("..") {
				$resolved = $resolved.append("..")
			}
			else {
				$resolved = $resolved.drop_last(1)
			}
		}
		else if part != "." and part != "" {
			$resolved = $resolved.append(part)
		}
	}
	join($resolved, "/")
}

invalid_path : Str -> Bool
invalid_path = |path| path.starts_with("/") or path.starts_with("\\") or path.contains(":") or path.contains("\\")

option : Str, Str -> Str
option = |options, name| include_option(options, name) ?? ""

## An include attribute's value, read as Asciidoctor reads an attribute list:
## the last `name=value` wins, an unquoted `None` leaves the attribute unset,
## and an empty value still sets it.
include_option : Str, Str -> Try(Str, [Absent])
include_option = |options, name| {
	var $value = Err(Absent)
	for piece in split_options(options) {
		parsed = split_first(piece, "=")
		value = trim(parsed.after)
		if piece.contains("=") and trim(parsed.before) == name and value != "None" {
			$value = Ok(option_value(value))
		}
	}
	$value
}

option_value : Str -> Str
option_value = |source| {
	bytes = source.to_utf8()
	first = bytes.first() ?? 0
	if first == '\'' or first == '"' {
		var $end = 1
		while $end < bytes.len() {
			if bytes.get($end) == Ok(first) {
				return slice(source, 1, $end - 1)
			}
			$end = $end + 1
		}
	}
	source
}

IncludeLines : { lines : List(SourceLine), problems : List(AsciiDocPreprocess.IncludeProblem) }

## How Asciidoctor reads a source. In AsciiDoc, the document itself or an
## included AsciiDoc file, it removes trailing whitespace from each line
## (before `indent=`, for an include) and processes preprocessor
## directives. In any other included file it does neither.
IncludeFile : [AsciiDocFile, OtherFile]

## Asciidoctor reads a file as AsciiDoc when its name ends in one of these.
include_file : Str -> IncludeFile
include_file = |path| if ([".adoc", ".asciidoc", ".asc", ".ad", ".txt"].any(|extension| path.ends_with(extension))) AsciiDocFile else OtherFile

## Select and indent an include's lines. `include::` expansion and
## `select_include` both call this, so the two can never disagree. Selection
## stays as lenient as Asciidoctor's; problems only describe it. With
## `indent=`, tabs expand first when `tab_size`, the document's `tabsize`,
## is positive. Asciidoctor ignores a `tabsize=` in the include's own
## attribute list.
include_lines : Str, Str, IncludeFile, U64 -> IncludeLines
include_lines = |source, options, file, tab_size| {
	selected = select_lines(source, options)
	read = match file {
		AsciiDocFile => selected.lines.map(|line| { ..line, text: trim_line_end(line.text) })
		OtherFile => selected.lines
	}
	lines = match include_option(options, "indent") {
		Ok(width) => indent_lines(read, reindent(width), tab_size)
		Err(Absent) => read
	}
	{ lines, problems: if (lines.is_empty()) add_problem(selected.problems, EmptySelection) else selected.problems }
}

## Asciidoctor uses the first of `lines=`, `tag=` and `tags=` that is
## present, even with an empty value. An empty `lines=`, an empty `tag=` or
## `tag=!`, and a `tags=` that names no tag select the whole file, in which
## tag markers are ordinary text.
select_lines : Str, Str -> IncludeLines
select_lines = |source, options| {
	## Asciidoctor finds no lines in an empty file. Reading a whole file, it
	## removes a byte order mark before splitting, so one alone leaves none.
	lines = if (source == "") [] else source_lines(source)
	whole = if (source == "\u(FEFF)") [] else lines
	match include_option(options, "lines") {
		Ok(selector) => select_line_numbers(lines, selector, whole)
		Err(Absent) => {
			choices = match include_option(options, "tag") {
				Ok(tag) => single_tag(tag)
				Err(Absent) => tag_list(include_option(options, "tags") ?? "")
			}
			if (choices.is_empty()) { lines: whole, problems: [] } else select_tagged(lines, choices)
		}
	}
}

## Each problem is reported once, in the order it was found.
add_problem : List(AsciiDocPreprocess.IncludeProblem), AsciiDocPreprocess.IncludeProblem -> List(AsciiDocPreprocess.IncludeProblem)
add_problem = |problems, problem| if (problems.contains(problem)) problems else problems.append(problem)

## A tag that `tag=` or `tags=` names, and whether its lines are selected.
TagChoice : { name : Str, select : Bool }

## `tag=` names a single tag, even one containing `,` or `;`.
single_tag : Str -> List(TagChoice)
single_tag = |tag| {
	if tag == "" or tag == "!" {
		[]
	} else if tag.starts_with("!") {
		[{ name: drop(tag, 1), select: False }]
	} else {
		[{ name: tag, select: True }]
	}
}

## `tags=` separates tags as `lines=` separates entries. A tag named twice
## keeps its first position and its last choice, as in a Ruby hash.
tag_list : Str -> List(TagChoice)
tag_list = |source| {
	var $choices = []
	for entry in selectors(source) {
		if entry != "" and entry != "!" {
			choice = if (entry.starts_with("!")) { name: drop(entry, 1), select: False } else { name: entry, select: True }
			$choices = if ($choices.any(|known| known.name == choice.name)) $choices.map(|known| if (known.name == choice.name) choice else known) else $choices.append(choice)
		}
	}
	$choices
}

## Whether lines outside every open tag are selected, and how a marker of a
## tag the selector doesn't name is treated: Asciidoctor tracks every tag,
## with `Tracked(select)`, when the selector uses `*` or `!*`, or uses `!**`
## and negates the first other tag it names.
TagPlan : { base : Bool, wildcard : [Tracked(Bool), Untracked] }

tag_plan : List(TagChoice) -> TagPlan
tag_plan = |choices| {
	named = choices.keep_if(|choice| choice.name != "*" and choice.name != "**")
	star = choices.find_first(|choice| choice.name == "*")
	match choices.find_first(|choice| choice.name == "**") {
		Ok(every) => {
			wildcard = match star {
				Ok(any) => Tracked(any.select)
				Err(_) => if (!every.select and named.first().map_ok(|choice| choice.select) == Ok(False)) Tracked(True) else Untracked
			}
			{ base: every.select, wildcard }
		}
		Err(_) => match star {
			Ok(any) => if (choices.first().map_ok(|choice| choice.name) == Ok("*")) { base: !any.select, wildcard: Tracked(any.select) } else { base: False, wildcard: Tracked(any.select) }
			Err(_) => { base: !named.any(|choice| choice.select), wildcard: Untracked }
		}
	}
}

## Asciidoctor's tag selection. Open tags form a stack. A marker of a named
## tag, or of any tag while every tag is tracked, pushes the tag with
## whether its lines are selected; a tracked tag inside an unselected one is
## unselected too. The end of the innermost open tag pops it. Any other end
## of a named tag removes the innermost open copy of that tag, if there is
## one, without changing what is selected. Other markers have no effect.
## Lines outside markers are selected as the innermost open tag says, or as
## `base` says when no tag is open.
select_tagged : List(SourceLine), List(TagChoice) -> IncludeLines
select_tagged = |lines, choices| {
	plan = tag_plan(choices)
	named = choices.keep_if(|choice| choice.name != "*" and choice.name != "**")
	var $open = []
	var $opened = []
	var $output = []
	var $problems = []
	for entry in lines {
		match tag_marker(entry.text) {
			Close(name) => {
				if $open.last().map_ok(|tag| tag.name) == Ok(name) {
					$open = $open.drop_last(1)
				} else if named.any(|choice| choice.name == name) {
					$open = remove_innermost($open, name)
					$problems = add_problem($problems, UnmatchedEnd(name))
				}
			}
			Open(name) => {
				match named.find_first(|choice| choice.name == name) {
					Ok(choice) => {
						if choice.select and !$opened.contains(name) {
							$opened = $opened.append(name)
						}
						$open = $open.append(choice)
					}
					Err(_) => {
						match plan.wildcard {
							Tracked(select) => {
								$open = $open.append({ name, select: select and ($open.last().map_ok(|tag| tag.select) ?? True) })
							}
							Untracked => {}
						}
					}
				}
			}
			Text => {
				if $open.last().map_ok(|tag| tag.select) ?? plan.base {
					$output = $output.append(entry)
				}
			}
		}
	}
	for tag in $open {
		$problems = add_problem($problems, UnclosedTag(tag.name))
	}
	for choice in named {
		if choice.select and !$opened.contains(choice.name) {
			$problems = add_problem($problems, MissingTag(choice.name))
		}
	}
	{ lines: $output, problems: $problems }
}

## Remove the innermost open copy of `name`, if there is one.
remove_innermost : List(TagChoice), Str -> List(TagChoice)
remove_innermost = |open, name| {
	var $index = open.len()
	while $index > 0 {
		$index = $index - 1
		if open.get($index).map_ok(|tag| tag.name) == Ok(name) {
			return open.take_first($index).concat(open.drop_first($index + 1))
		}
	}
	open
}

TagMarker : [Open(Str), Close(Str), Text]

## Asciidoctor's `TagDirectiveRx`, `\b(?:tag|(e)nd)::(\S+?)\[\](?=$|[ \r])`:
## the first `tag::NAME[]` or `end::NAME[]` that starts a word, where NAME is
## the shortest run of non-whitespace characters after which `[]` ends the
## line or is followed by a space or CR. So `"tag::x[]"` in a string
## literal, `tag::x[]` followed by a tab and `my_tag::x[]` are not markers.
tag_marker : Str -> TagMarker
tag_marker = |line| {
	if !line.contains("::") or !line.contains("[]") {
		return Text
	}
	bytes = line.to_utf8()
	var $start = 0
	while $start + 8 <= bytes.len() {
		first = bytes.get($start) ?? 0
		opens = first == 't' and bytes.sublist({ start: $start, len: 5 }) == "tag::".to_utf8()
		closes = first == 'e' and bytes.sublist({ start: $start, len: 5 }) == "end::".to_utf8()
		if (opens or closes) and !follows_word(bytes, $start) {
			match marker_name(bytes, $start + 5) {
				Ok(name) => return if (opens) Open(name) else Close(name)
				Err(NoName) => {}
			}
		}
		$start = $start + 1
	}
	Text
}

## Whether the character before `index` is a word character, so that a
## marker there would not start a word. Ruby's `\b` counts letters, marks,
## numbers and connector punctuation as word characters.
follows_word : List(U8), U64 -> Bool
follows_word = |bytes, index| {
	if index == 0 {
		return False
	}
	var $start = index - 1
	while $start > 0 and (bytes.get($start) ?? 0) >= 128 and (bytes.get($start) ?? 0) < 192 {
		$start = $start - 1
	}
	starts_word(Str.from_utf8_lossy(bytes.sublist({ start: $start, len: index - $start })))
}

## The name of a marker whose `::` ends just before `start`: the shortest run
## of one or more non-whitespace bytes followed by `[]` and then the end of
## the line, a space or a CR.
marker_name : List(U8), U64 -> Try(Str, [NoName])
marker_name = |bytes, start| {
	var $end = start
	while $end < bytes.len() and !ruby_space(bytes.get($end) ?? 0) {
		$end = $end + 1
		if bytes.get($end) == Ok('[') and bytes.get($end + 1) == Ok(']') and ($end + 2 == bytes.len() or bytes.get($end + 2) == Ok(' ') or bytes.get($end + 2) == Ok(13)) {
			return Ok(Str.from_utf8_lossy(bytes.sublist({ start, len: $end - start })))
		}
	}
	Err(NoName)
}

## Ruby's ASCII whitespace: space, tab, LF, vertical tab, form feed and CR.
ruby_space : U8 -> Bool
ruby_space = |byte| byte == ' ' or (byte >= 9 and byte <= 13)

## Ruby's `rstrip`, which also removes NUL.
trim_line_end : Str -> Str
trim_line_end = |source| {
	bytes = source.to_utf8()
	var $end = bytes.len()
	while $end > 0 and (ruby_space(bytes.get($end - 1) ?? 1) or bytes.get($end - 1) == Ok(0)) {
		$end = $end - 1
	}
	if ($end == bytes.len()) source else slice(source, 0, $end)
}

## The native reader sorts finite selected line numbers before an infinity
## marker. A nonpositive first number blocks the cursor; an open range starts
## selecting remaining lines only after every explicit finite number is read.
## Every entry is still read after a block, so each invalid one is reported.
## When no entry selects a range, as for an empty `lines=` or only reversed
## ranges, Asciidoctor reads the `whole` file instead.
select_line_numbers : List(SourceLine), Str, List(SourceLine) -> IncludeLines
select_line_numbers = |lines, selector, whole| {
	var $ranges = []
	var $infinite = False
	var $largest = 0
	var $blocked = False
	var $problems = []
	cap = lines.len() + 1
	for range in selectors(selector) {
		pieces = split_first(range, "..")
		first = selector_integer(pieces.before)
		last = selector_integer(pieces.after)
		open = range.contains("..") and (pieces.after == "" or last.negative)
		if !range.contains("..") or open or integer_order(first, last) <= 0 {
			if first.negative or first.digits == "0" {
				$blocked = True
				$problems = add_problem($problems, InvalidLines(trim(range)))
			} else {
				start = bounded_selector_number(first, cap)
				end = if (range.contains("..") and !open) bounded_selector_number(last, cap) else start
				$ranges = $ranges.append({ start, end })
				if end > $largest {
					$largest = end
				}
				if open {
					$infinite = True
				}
				if start > lines.len() or !clean_line_selector(range) {
					$problems = add_problem($problems, InvalidLines(trim(range)))
				}
			}
		} else {
			$problems = add_problem($problems, InvalidLines(trim(range)))
		}
	}
	if $blocked {
		return { lines: [], problems: $problems }
	}
	if $ranges.is_empty() {
		return { lines: whole, problems: $problems }
	}
	{ lines: lines.keep_if(|line| ($infinite and line.line > $largest) or $ranges.any(|range| line.line >= range.start and line.line <= range.end)), problems: $problems }
}

## Asciidoctor documents `N`, `N..M`, `N..` and `N..-1`. Any other entry is
## read leniently, like Ruby's `to_i`, and reported.
clean_line_selector : Str -> Bool
clean_line_selector = |range| {
	if !range.contains("..") {
		return decimal_digits(trim(range))
	}
	pieces = split_first(range, "..")
	end = trim(pieces.after)
	decimal_digits(trim(pieces.before)) and (end == "" or end == "-1" or decimal_digits(end))
}

decimal_digits : Str -> Bool
decimal_digits = |source| {
	bytes = source.to_utf8()
	!bytes.is_empty() and bytes.all(|byte| byte >= '0' and byte <= '9')
}

selector_integer : Str -> IntegerValue
selector_integer = |source| {
	bytes = trim(source).to_utf8()
	negative = bytes.first() == Ok('-')
	start = if (negative or bytes.first() == Ok('+')) 1 else 0
	canonical_integer(negative, numeric_digits(bytes, start).digits)
}

bounded_selector_number : IntegerValue, U64 -> U64
bounded_selector_number = |integer, cap| {
	number = U64.from_str(integer.digits) ?? cap
	if (number > cap) cap else number
}

## `adjust_indentation` on an include's selected lines, keeping each line's
## number and offset.
indent_lines : List(SourceLine), Reindent, U64 -> List(SourceLine)
indent_lines = |lines, width, tab_size| {
	texts = adjust_indentation(lines.map(|line| line.text), width, tab_size)
	var $result = []
	var $index = 0
	for line in lines {
		$result = $result.append({ ..line, text: texts.get($index) ?? line.text })
		$index = $index + 1
	}
	$result
}

## What `adjust_indentation` does with the block indent. `Keep` leaves the
## leading whitespace, as a negative width does in Asciidoctor, and
## `Reindent(width)` replaces it with `width` spaces.
Reindent : [Keep, Reindent(U64)]

## Asciidoctor bounds neither an indent width nor a tab size. This package
## counts anything wider than 1024 columns as 1024.
max_columns : U64
max_columns = 1024

## An `indent` value read like Ruby's `to_i`: empty or not a number is 0,
## and a negative width keeps the indentation.
reindent : Str -> Reindent
reindent = |value| {
	width = selector_integer(value)
	if (width.negative) Keep else Reindent(bounded_columns(width))
}

## A `tabsize` value read like Ruby's `to_i`. Zero, a negative number, or
## text that doesn't start with a number gives 0, which expands no tabs.
parse_tab_size : Str -> U64
parse_tab_size = |value| {
	if value == "" {
		return 0
	}
	size = selector_integer(value)
	if (size.negative) 0 else bounded_columns(size)
}

bounded_columns : IntegerValue -> U64
bounded_columns = |integer| {
	number = U64.from_str(integer.digits) ?? max_columns
	if (number > max_columns) max_columns else number
}

## Asciidoctor's `Parser.adjust_indentation!`. When `tab_size` is positive,
## it first expands each tab to the next tab stop. Then, for
## `Reindent(width)`, it finds the block indent: the fewest leading
## whitespace characters on any nonempty line, counting a tab that is left
## like a space, or none when some nonempty line has none. Each nonempty line
## loses that many characters and gains `width` spaces, while empty lines
## stay empty. A line of only spaces or tabs is not empty, so it counts and
## gains the spaces too.
adjust_indentation : List(Str), Reindent, U64 -> List(Str)
adjust_indentation = |lines, width, tab_size| {
	expanded = if (tab_size > 0 and lines.any(|line| line.contains("\t"))) lines.map(|line| expand_tabs(line, tab_size)) else lines
	match width {
		Keep => expanded
		Reindent(columns) => {
			var $block = 18446744073709551615
			for line in expanded {
				if line != "" {
					count = leading_whitespace(line)
					if count < $block {
						$block = count
					}
				}
			}
			block = if ($block == 18446744073709551615) 0 else $block
			if block == 0 and columns == 0 {
				expanded
			} else {
				prefix = repeat(" ", columns)
				expanded.map(|line| if (line == "") line else Str.concat(prefix, drop(line, block)))
			}
		}
	}
}

## Replace each tab with the spaces that reach the next multiple of
## `tab_size` columns. As in Asciidoctor, every character (code point)
## before the tab is one column, whatever its display width.
expand_tabs : Str, U64 -> Str
expand_tabs = |line, tab_size| {
	if !line.contains("\t") {
		return line
	}
	var $output = []
	var $column = 0
	for byte in line.to_utf8() {
		if byte == '\t' {
			var $spaces = tab_size - $column % tab_size
			$column = $column + $spaces
			while $spaces > 0 {
				$output = $output.append(' ')
				$spaces = $spaces - 1
			}
		} else {
			$output = $output.append(byte)
			if byte < 128 or byte >= 192 {
				$column = $column + 1
			}
		}
	}
	Str.from_utf8_lossy($output)
}

## The leading characters Ruby's `lstrip` removes.
leading_whitespace : Str -> U64
leading_whitespace = |source| {
	var $count = 0
	bytes = source.to_utf8()
	while ruby_space(bytes.get($count) ?? 0) {
		$count = $count + 1
	}
	$count
}

heading_prefix : Str -> U64
heading_prefix = |source| {
	var $count = 0
	bytes = source.to_utf8()
	while bytes.get($count) == Ok('=') {
		$count = $count + 1
	}
	if (bytes.get($count) == Ok(' ')) $count else 0
}

repeat : Str, U64 -> Str
repeat = |source, count| {
	var $result = ""
	var $i = 0
	while $i < count {
		$result = Str.concat($result, source)
		$i = $i + 1
	}
	$result
}

expect AsciiDocPreprocess.substitute("Hi {name} \\{name}", [{ name: "name", value: "Roc" }]) == "Hi Roc \\{name}"
expect match AsciiDocPreprocess.run(":x: yes\nifdef::x[]\nIncluded\nendif::[]\nifndef::x[Hidden]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "Included"
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":a: one\n{a}\n\n:a: two\n{a}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.lines.map(|line| AsciiDocPreprocess.substitute(line.text, line.attributes)) == ["one", "", "two"]
	Err(_) => False
}
expect match AsciiDocPreprocess.run("include::part.adoc[]", { attributes: [], max_depth: 16, expand_includes: True, includes: [{ path: "part.adoc", source: "== Child\n\nText" }] }) {
	Ok(result) => result.source == "== Child\n\nText"
	Err(_) => False
}

# Regression expectations checked against Asciidoctor 2.0.23. Attribute
# definitions belong to block boundaries, unlike reader-level directives.
expect match AsciiDocPreprocess.run(":a: value\n\n{a}\n:a: later\n{a}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.lines.map(|line| AsciiDocPreprocess.substitute(line.text, line.attributes)) == ["", "value", ":a: later", "value"]
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":x: hi\n\n----\n:y: wow\n{x}\n----\n\n{y}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "\n----\n:y: wow\n{x}\n----\n\n{y}" and !result.attributes.any(|a| a.name == "y")
	Err(_) => False
}
expect match AsciiDocPreprocess.run("----\n\\ifdef::x[YES]\n\\include::x[]\n----", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "----\nifdef::x[YES]\ninclude::x[]\n----"
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":foo: one \\\n two\n\n{foo}", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{foo}", result.attributes) == "one two"
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":foo: yes\n:foo!:\nifndef::foo[absent]\n\n:!bar:\nifdef::foo[hidden]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "absent\n"
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":a:\n:b:\nifdef::a+b[both]\nifdef::a,c[either]\nifndef::a+c[missing]\nifdef::missing[]\nifdef::a[hidden]\nendif::[]\nlast", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "both\neither\nmissing\nlast"
	Err(_) => False
}
expect match AsciiDocPreprocess.run(":n: 12\nifeval::[{n} >= 10]\nyes\nendif::[]\nifeval::[\"x\" != \"y\"]\ndifferent\nendif::[]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "yes\ndifferent"
	Err(_) => False
}
expect match AsciiDocPreprocess.run("include::a.adoc[opts=optional]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "link:a.adoc[role=include]"
	Err(_) => False
}
expect match AsciiDocPreprocess.run("include::missing.adoc[]", { includes: [], attributes: [], max_depth: 16, expand_includes: True }) {
	Err(PreprocessError(error)) => error.line == 1 and error.offset == 0 and error.path == ""
	_ => False
}
expect match AsciiDocPreprocess.run("include::a.adoc[]", { includes: [{ path: "a.adoc", source: "include::a.adoc[]" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Err(PreprocessError(error)) => error.line == 1 and error.path == "a.adoc"
	_ => False
}
expect match AsciiDocPreprocess.run("include::dir/a.adoc[]", { includes: [{ path: "dir/a.adoc", source: "include::b.adoc[]" }, { path: "dir/b.adoc", source: "nested" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == "nested" and (result.lines.first() ?? { text: "", attributes: [], path: "", line: 0, offset: 0 }).path == "dir/b.adoc"
	_ => False
}
# Offsets stay internal; they locate errors in included lines.
expect include_lines("one\ntwo\nthree\nfour\n", "lines=2..3", OtherFile, 0).lines.map(|line| (line.text, line.line, line.offset)) == [("two", 2, 4), ("three", 3, 8)]
expect include_lines("\u(FEFF)  one\r\n    two\r\n", "indent=0", OtherFile, 0).lines.map(|line| (line.text, line.line, line.offset)) == [("one", 1, 3), ("  two", 2, 10)]
expect match AsciiDocPreprocess.run("include::a.adoc[leveloffset=+1]", { includes: [{ path: "a.adoc", source: "== Child" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == "== Child" and result.lines.any(|line| line.attributes.contains({ name: "leveloffset", value: "1" }))
	_ => False
}
expect match AsciiDocPreprocess.run("include::a.adoc[indent=1]", { includes: [{ path: "a.adoc", source: "    one\n      two" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == " one\n   two"
	_ => False
}
expect AsciiDocPreprocess.substitute("{outer}", [{ name: "outer", value: "hello {inner}" }, { name: "inner", value: "world" }]) == "hello world"
expect AsciiDocPreprocess.substitute("{unknown}", []) == "{unknown}"
expect match AsciiDocPreprocess.run("first\n\ninclude::missing.adoc[]", { includes: [], attributes: [], max_depth: 16, expand_includes: True }) {
	Err(PreprocessError(error)) => error.line == 3 and error.offset == 7
	_ => False
}

continues : Str -> Bool
continues = |source| source.ends_with(" \\") or source.ends_with(" +")

strip_continuation : Str -> Str
strip_continuation = |source| slice(source, 0, source.to_utf8().len() - 2)

compare_strings : Str, Str, Str -> Bool
compare_strings = |left, right, operator| {
	a = left.to_utf8()
	b = right.to_utf8()
	var $index = 0
	var $order = 0
	while $index < a.len() and $index < b.len() and $order == 0 {
		x = a.get($index) ?? 0
		y = b.get($index) ?? 0
		$order = if (x < y) -1 else if (x > y) 1 else 0
		$index = $index + 1
	}
	if $order == 0 {
		$order = if (a.len() < b.len()) -1 else if (a.len() > b.len()) 1 else 0
	}
	compare($order, 0, operator)
}

expect match AsciiDocPreprocess.run(":x: one +\n two\n\n{x}", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{x}", result.attributes) == "one two"
	_ => False
}
expect match AsciiDocPreprocess.run(":x: yes\nifdef::x[:y: inner]\n\n{y}", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{y}", result.attributes) == "inner"
	_ => False
}
expect evaluate("\"a\" < \"b\"")
expect evaluate("1.5 > 1.25")
expect evaluate("123456789012345678901234567890123456789 > 123456789012345678901234567890123456788")
expect evaluate("-123456789012345678901234567890123456789 < -123456789012345678901234567890123456788")
expect evaluate("9007199254740993 > 9007199254740992.0")
expect evaluate("9007199254740993 < 9007199254740994.0")
expect evaluate("9007199254740993 != 9007199254740993.0")
expect evaluate("9007199254740992 == 9007199254740993.0")
expect evaluate("-9007199254740993 < -9007199254740992.0")
expect evaluate("0 < 1.0e-300")
expect evaluate("0 > -1.0e-300")
expect evaluate("1.0e999 > 9999999999999999999999999999999999999999999")
expect evaluate("-1.0e999 < -9999999999999999999999999999999999999999999")
expect evaluate("1.0e-999 == 0")
expect evaluate("1_000_000 == 1000000")
expect evaluate("1_2.3_4 == 12.34")
expect evaluate("1__2.0 == 1")
expect evaluate("1.e2 == 1")
expect evaluate("1e3. == 1000")
expect evaluate(".1e2 == 10")
expect evaluate("1.7976931348623157e308 > 1000000000000000000000000000000000000000000000000000000000000")
expect evaluate("\"10\" < \"2\"")
expect !evaluate("\"10\" == 10")
expect evaluate("true != false")
expect !evaluate("true > false")
expect evaluate("abc == 0")
expect evaluate_with("\"{n}\" < \"2\"", [{ name: "n", value: "10" }])
expect evaluate_with("{missing} == {missing2}", [])

standard_attributes : List(AsciiDocPreprocess.Attribute)
standard_attributes = AsciiDocSource.standard

expect match AsciiDocPreprocess.run("include::dir/a.adoc[]", { includes: [{ path: "dir/a.adoc", source: "include::../b.adoc[]" }, { path: "b.adoc", source: "nested" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == "nested"
	_ => False
}
expect match AsciiDocPreprocess.run(":x: *Roc* <b> & café\n\n{x}", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{x}", result.attributes) == "*Roc* &lt;b&gt; &amp; café"
	_ => False
}
expect match AsciiDocPreprocess.run("ifdef::backend-html5[html]\n{docyear}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.lines.map(|line| AsciiDocPreprocess.substitute(line.text, line.attributes)) == ["html", "2000"]
	_ => False
}

expect match AsciiDocPreprocess.run("----\ninclude::part.adoc[]\n----\n\n{item}", { includes: [{ path: "part.adoc", source: ":item: hidden\n{item}" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == "----\n:item: hidden\n{item}\n----\n\n{item}" and !result.attributes.any(|a| a.name == "item")
	_ => False
}

complete_directive : Str -> Bool
complete_directive = |source| {
	if !source.contains("[") or !source.ends_with("]") {
		return False
	}
	before = split_first(source, "[").before
	if source.starts_with("include::") {
		target = drop(before, 9)
		target != "" and (target.to_utf8().first() ?? 0) > 32
	} else {
		before.to_utf8().all(|byte| byte > 32 and byte != 127)
	}
}
expect match AsciiDocPreprocess.run("ifdef::foo\ninclude::foo\nendif::foo\ntext", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "ifdef::foo\ninclude::foo\nendif::foo\ntext"
	_ => False
}
expect match AsciiDocPreprocess.run("ifeval::[1]\ntext", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "text"
	_ => False
}

expect match AsciiDocPreprocess.run("﻿:item: yes\n\n{item}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "\n{item}" and AsciiDocPreprocess.substitute("{item}", result.attributes) == "yes"
	_ => False
}

expect match AsciiDocPreprocess.run("include::part.adoc[indent=0]\n\n{item}", { includes: [{ path: "part.adoc", source: "    :item: yes" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => result.source == "\n{item}" and AsciiDocPreprocess.substitute("{item}", result.attributes) == "yes"
	_ => False
}

# Include attribute lists permit commas inside single or double quotes.
split_options : Str -> List(Str)
split_options = |source| {
	bytes = source.to_utf8()
	var $quote = 0.U8
	var $start = 0
	var $position = 0
	var $parts = []
	while $position < bytes.len() {
		byte = bytes.get($position) ?? 0
		if $quote == 0 and (byte == '\'' or byte == '"') {
			$quote = byte
		}
		else if $quote == byte {
			$quote = 0
		}
		else if $quote == 0 and byte == ',' {
			$parts = $parts.append(slice(source, $start, $position - $start))
			$start = $position + 1
		}
		$position = $position + 1
	}
	$parts.append(drop(source, $start))
}

selectors : Str -> List(Str)
selectors = |source| {
	var $parts = source.split_on(if (source.contains(",")) "," else ";")
	while !$parts.is_empty() and $parts.last() == Ok("") {
		$parts = $parts.drop_last(1)
	}
	$parts
}

expect match AsciiDocPreprocess.run(":imagesdinclude::missing.adoc[]\nir: assets\n\nimage::cat.png[]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == ":imagesdinclude::missing.adoc[]\nir: assets\n\nimage::cat.png[]"
	_ => False
}

starts_word : Str -> Bool
starts_word = |source| {
	for located in Scalar.iter(source) {
		return word_scalar(located.scalar)
	}
	False
}

word_scalar : Scalar -> Bool
word_scalar = |scalar| {
	value = Scalar.to_u32(scalar)
	if value < 128 {
		return (value >= 'a' and value <= 'z') or (value >= 'A' and value <= 'Z') or (value >= '0' and value <= '9') or value == '_'
	}
	match GeneralCategory.of_scalar(scalar) {
		Lu | Ll | Lt | Lm | Lo | Mn | Mc | Me | Nd | Nl | No | Pc => True
		_ => False
	}
}

normalize_attribute_name : Str -> Str
normalize_attribute_name = |source| {
	bytes = source.to_utf8()
	var $ascii = True
	var $canonical = True
	for byte in bytes {
		if byte >= 128 {
			$ascii = False
			break
		}
		if !((byte >= 'a' and byte <= 'z') or (byte >= '0' and byte <= '9') or byte == '_' or byte == '-') {
			$canonical = False
		}
	}
	if $ascii {
		if $canonical {
			return source
		}
		return Str.from_utf8_lossy(bytes.keep_if(|byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '_' or byte == '-').map(|byte| if (byte >= 'A' and byte <= 'Z') byte + 32 else byte))
	}
	var $bytes = []
	for located in Scalar.iter(source) {
		if word_scalar(located.scalar) or Scalar.to_u32(located.scalar) == '-' {
			$bytes = $bytes.concat(Scalar.to_utf8(located.scalar))
		}
	}
	text = Str.from_utf8_lossy($bytes)
	match Case.to_lower(text, Case.unicode_default, Case.unlimited_limits) {
		Ok(result) => Case.result_text(result)
		Err(_) => text
	}
}
expect match AsciiDocPreprocess.run(":experimen=== tal:\n\nPress kbd:[Ctrl++].", AsciiDocPreprocess.defaults) {
	Ok(result) => result.attributes.any(|attribute| attribute.name == "experimental" and attribute.value == "")
	_ => False
}
expect normalize_attribute_name("CAFÉ ・ 東京") == "café東京"
expect !is_definition(":-invalid: value")

expect match AsciiDocPreprocess.run(":a: first\n:b: second\n:a: reset\n:c: third", AsciiDocPreprocess.defaults) {
	Ok(result) => result.attributes.keep_if(|attribute| ["a", "b", "c"].contains(attribute.name)) == [{ name: "b", value: "second" }, { name: "a", value: "reset" }, { name: "c", value: "third" }] and result.attributes.any(|attribute| attribute.name == "__revision:a" and attribute.value == "2")
	_ => False
}
expect match AsciiDocPreprocess.run(":a: first\ninclude::part.adoc[]\n:b: third\n\n{b}", { includes: [{ path: "part.adoc", source: ":b: second" }], attributes: [], max_depth: 16, expand_includes: True }) {
	Ok(result) => AsciiDocPreprocess.substitute("{b}", result.attributes) == "third" and result.attributes.any(|attribute| attribute.name == "__revision:b" and attribute.value == "2")
	_ => False
}
expect AsciiDocPreprocess.substitute("{CAFÉ}", [{ name: "café", value: "yes" }]) == "yes"

expect match AsciiDocPreprocess.run("// Defined externally\nifndef::src[]\n:repo: URL\n:src: {repo}/blob/main\nendif::[]\n\n= Title\n\nBody", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{src}", result.attributes) == "URL/blob/main" and !result.source.contains(":repo:")
	_ => False
}

expect match AsciiDocPreprocess.run(":x: pass:q[*bold*]\n\n{x}", AsciiDocPreprocess.defaults) {
	Ok(result) => AsciiDocPreprocess.substitute("{x}", result.attributes) == "<strong>bold</strong>"
	_ => False
}

# Continued definitions share the reader-loop mutation path with ordinary ones.
expect {
	var $source = ""
	var $i = 0.U64
	while $i < 128 {
		$source = Str.concat($source, Str.concat(":fresh", Str.concat($i.to_str(), ": value \\\n continued\n")))
		$i = $i + 1
	}
	match AsciiDocPreprocess.run($source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.attributes.keep_if(|attribute| attribute.name.starts_with("fresh")).len() == 128 and AsciiDocPreprocess.substitute("{fresh127}", result.attributes) == "value continued"
		_ => False
	}
}

expect match AsciiDocPreprocess.run("----\n\\ifdef::x[]\n\\include::pé----", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "----\nifdef::x[]\n\\include::pé----"
	_ => False
}
expect match AsciiDocPreprocess.run("ifdef::missing[]\nhidden\nendif::different[]\nstill hidden\nendif::missing[]\nvisible", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "visible"
	_ => False
}
expect match AsciiDocPreprocess.run("ifdef::[]\nvisible\nifeval::target[1 == 1]\nalso visible", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "visible\nalso visible"
	_ => False
}
expect match AsciiDocPreprocess.run("ifdef::a + b[raw]", AsciiDocPreprocess.defaults) {
	Ok(result) => result.source == "ifdef::a + b[raw]"
	_ => False
}

expect evaluate("1.7976931348623157e308 == 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368")
expect evaluate("1.7976931348623157e308 < 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369")
expect evaluate("-1.7976931348623157e308 > -179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369")
expect evaluate("1 != 2 == 3")
expect !evaluate("1 ==")

expect evaluate("\" == \"")

expect match AsciiDocPreprocess.run(":name: one\n\n{name}\n\n:name: two\n\n{name}\n\n:name: {name}-three\n\n{name}", AsciiDocPreprocess.defaults) {
	Ok(result) => result.lines.keep_if(|line| line.text == "{name}").map(|line| AsciiDocPreprocess.substitute(line.text, line.attributes)) == ["one", "two", "two-three"] and result.attributes.any(|attr| attr.name == "__revision:name" and attr.value == "3")
	_ => False
}

# Table bodies retain attribute entries as cell source. AsciiDoc-style cells
# get their own nested reader later; ordinary cells treat these lines as text.
table_delimiter : Str -> Bool
table_delimiter = |text| text == "|===" or text == ",===" or text == ":===" or text == "!==="

expect {
	source = "|===\n|first\n\n:value: inner\n\n{value}"
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.source == source and AsciiDocPreprocess.substitute("{value}", result.attributes) == "{value}"
		_ => False
	}
}

expect {
	source = "|===\n|cell\n|===\n:value: outside\n\n{value}"
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => !result.source.contains(":value:") and AsciiDocPreprocess.substitute("{value}", result.attributes) == "outside"
		_ => False
	}
}

expect {
	source = ":flag:\n\n|===\n|before\nifdef::flag[]\n:value: inner\n|after\nendif::[]\n|==="
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.source.contains(":value: inner") and !result.source.contains("ifdef::") and AsciiDocPreprocess.substitute("{value}", result.attributes) == "{value}"
		_ => False
	}
}

expect {
	source = "|===\n|before\ninclude::row.adoc[]\n|===\n\n{value}"
	options = { ..AsciiDocPreprocess.defaults, includes: [{ path: "row.adoc", source: ":value: inner\n|included" }], expand_includes: True }
	match AsciiDocPreprocess.run(source, options) {
		Ok(result) => result.source.contains(":value: inner") and result.source.contains("|included") and !result.source.contains("include::") and AsciiDocPreprocess.substitute("{value}", result.attributes) == "{value}"
		_ => False
	}
}

expect {
	source = ":key: old\nifdef::key[:key: new]\nifeval::[\"{key}\" == \"new\"]\nfirst\nendif::[]\n\n:key!:\nifndef::key[]\nsecond\nendif::[]"
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.source == "first\n\nsecond" and !result.attributes.any(|attribute| attribute.name == "key")
		_ => False
	}
}

expect {
	source = ":key: old\nifdef::key[]\ninclude::part.adoc[]\nendif::[]\nifeval::[\"{key}\" == \"new\"]\npassed\nendif::[]"
	options = { ..AsciiDocPreprocess.defaults, includes: [{ path: "part.adoc", source: ":key: new" }], expand_includes: True }
	match AsciiDocPreprocess.run(source, options) {
		Ok(result) => result.source == "passed"
		_ => False
	}
}

expect {
	source = ":inner: yes\n:outer: {inner}\nifeval::[\"{outer}\" == \"yes\"]\npassed\nendif::[]"
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.source == "passed"
		_ => False
	}
}

expect {
	source = "ifdef::CAFÉ+other[]\nyes\nendif::[]\nifdef::missing,CAFÉ[]\nalso\nendif::[]"
	options = { ..AsciiDocPreprocess.defaults, attributes: [{ name: "café", value: "" }, { name: "other", value: "" }] }
	match AsciiDocPreprocess.run(source, options) {
		Ok(result) => result.source == "yes\nalso"
		_ => False
	}
}

expect {
	attributes = [{ name: "unused", value: "\u(1)\u(1)unused" }, { name: "prev", value: "Roc" }, { name: "empty", value: "" }, { name: "html", value: "<b>Roc</b> &amp; +" }, { name: "amp", value: "override" }, { name: "café", value: "Unicode" }, { name: "a&amp;b", value: "escaped-name" }, { name: "control", value: "\u(1)0\u(1)" }]
	index = index_attributes(attributes)
	sources = ["{prev}", "prefix {prev}{empty} < & suffix", "{html}-{prev}-{html}", "{PREV} {café} {CAFÉ}", "{amp} {lt} {missing}", "\\{prev}", "pass:q[*{prev}*]", "{counter:n}{prev}", "{set:prev:new}{prev}", "{{prev}}", "{prev", "{html}&{empty}", "{a&b}", "literal \u(1)0\u(1) {control} {html}"]
	sources.all(|source| definition_value(source, attributes, index) == AsciiDocInline.attribute_value(source, attributes))
}

expect {
	value = Str.join_with(List.repeat("é", 2050), "")
	attributes = [{ name: "long", value }]
	definition_value("prefix{long}", attributes, index_attributes(attributes)) == AsciiDocInline.attribute_value("prefix{long}", attributes)
}

expect {
	source = "ifeval::[1 == 1]\nendif::[]\n:answer: 42\nifeval::[{answer} == 42]\nindexed\nendif::[]\n\n:answer: 43\nifeval::[1 == 1]\nendif::[]\nifeval::[{answer} == 43]\nupdated\nendif::[]"
	match AsciiDocPreprocess.run(source, AsciiDocPreprocess.defaults) {
		Ok(result) => result.source == "indexed\n\nupdated"
		_ => False
	}
}

# A source snapshot is immutable after emission. Its exact canonical token is
# computed lazily once, so a long header still appends definitions linearly.
# NUL cannot occur in a legal AsciiDoc attribute name. Small snapshots retain
# the ordinary representation and avoid indexing overhead.
snapshot_attributes : List(AsciiDocPreprocess.Attribute) -> List(AsciiDocPreprocess.Attribute)
snapshot_attributes = |attrs| {
	if (attrs.len() < 64) attrs else AsciiDocAttributes.with_snapshot(attrs)
}

expect {
	options = { ..AsciiDocPreprocess.defaults, attributes: [{ name: "seed", value: "first" }, { name: "seed", value: "last" }] }
	source = "{seed}\n\n:value: one\n\n{value}\n\n:value!: \n\n:other: middle\n\n:value: two\n\n{value}"
	match AsciiDocPreprocess.run_scoped(source, options) {
		Ok(result) => {
			lines = result.lines.keep_if(|line| line.text == "{seed}" or line.text == "{value}")
			views = lines.map(|line| AsciiDocAttributes.indexed_attributes(line.attributes, AsciiDocAttributes.empty).view)
			initial = views.get(0) ?? AsciiDocAttributes.indexed_attributes(AsciiDocAttributes.flat([]), AsciiDocAttributes.empty).view
			last = views.last() ?? initial
			values = lines.map(|line| AsciiDocAttributes.materialize(line.attributes))
			(values.get(0) ?? []).any(|a| a.name == "seed" and a.value == "last") and !(values.get(0) ?? []).any(|a| a.name == "seed" and a.value == "first") and AsciiDocAttributes.lookup_first(last, "value") == Ok("two") and initial.key == last.key and last.generation > initial.generation and AsciiDocAttributes.materialize(result.attributes).keep_if(|a| ["other", "value", "__revision:value"].contains(a.name)) == [{ name: "other", value: "middle" }, { name: "value", value: "two" }, { name: "__revision:value", value: "3" }]
		}
		_ => False
	}
}

expect {
	source = "include::part.adoc[]"
	base = { ..AsciiDocPreprocess.defaults, expand_includes: True, includes: [{ path: "part.adoc", source: "Body" }] }
	changed = { ..base, includes: [{ path: "part.adoc", source: "Other" }] }
	source_namespace(source, base) == source_namespace(source, base) and source_namespace(source, base) != source_namespace(source, changed) and source_namespace(source, base) != source_namespace("include::other.adoc[]", base) and source_namespace(source, base) != source_namespace(source, { ..base, attributes: [{ name: "a", value: "bc" }] }) and source_namespace(source, { ..base, attributes: [{ name: "a", value: "bc" }] }) != source_namespace(source, { ..base, attributes: [{ name: "ab", value: "c" }] })
}
