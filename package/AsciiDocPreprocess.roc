import AsciiDocInline
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
		var $attributes = standard_attributes
		for attribute in options.attributes {
			$attributes = $attributes.keep_if(|current| current.name != attribute.name).append(attribute)
		}
		result = process(source, "", $attributes, options, [], 0)?
		Ok({ source: join(result.lines.map(|line| line.text), "\n"), attributes: result.attributes, lines: result.lines })
	}

	## Expand defined attribute references, preserving unknown references and
	## escaped references for the inline substitution pass.
	substitute : Str, List(Attribute) -> Str
	substitute = |source, attributes| expand(source, attributes, 0)
}

Context : { definition_allowed : Bool, opaque : Str, header : Bool }

State : { lines : List(AsciiDocPreprocess.Line), attributes : List(AsciiDocPreprocess.Attribute), context : Context }

process : Str, Str, List(AsciiDocPreprocess.Attribute), AsciiDocPreprocess.Options, List(Str), U64 -> Try(State, [PreprocessError(AsciiDocPreprocess.Error)])
process = |source, path, attributes, options, stack, depth| process_lines(source_lines(source), path, attributes, options, stack, depth, { definition_allowed: True, opaque: "", header: source.starts_with("= ") or source.starts_with("﻿= ") })

SourceLine : { text : Str, line : U64, offset : U64 }

source_lines : Str -> List(SourceLine)
source_lines = |source| {
	var $number = 0
	var $offset = 0
	var $lines = []
	for raw in source.split_on("\n") {
		$number = $number + 1
		bom = $number == 1 and raw.starts_with("﻿")
		$lines = $lines.append({ text: remove_cr(if bom drop(raw, 3) else raw), line: $number, offset: $offset + (if bom 3 else 0) })
		$offset = $offset + raw.to_utf8().len() + 1
	}
	if source.ends_with("\n") $lines.drop_last(1) else $lines
}

process_lines : List(SourceLine), Str, List(AsciiDocPreprocess.Attribute), AsciiDocPreprocess.Options, List(Str), U64, Context -> Try(State, [PreprocessError(AsciiDocPreprocess.Error)])
process_lines = |input_lines, path, attributes, options, stack, depth, context| {
	var $attributes = attributes
	var $attribute_index = Dict.empty()
	var $index_ready = False
	var $lines = []
	var $conditions = []
	var $continuation = ""
	var $expanded_size = 0
	var $definition_allowed = context.definition_allowed
	var $opaque = context.opaque
	var $header = context.header
	for input_line in input_lines {
		emitted_before = $lines.len()
		number = input_line.line
		offset = input_line.offset
		text = input_line.text
		var $definition_text = ""
		active = $conditions.all(|condition| condition.enabled)
		if text.starts_with("endif::") and complete_directive(text) {
			closing = directive(drop(text, 7))
			target = lower(closing.target)
			opened = $conditions.last() ?? { enabled: False, target: "" }
			if closing.body == "" and !$conditions.is_empty() and (target == "" or target == opened.target) {
				$conditions = $conditions.drop_last(1)
			}
		} else if (text.starts_with("ifdef::") or text.starts_with("ifndef::")) and complete_directive(text) {
			negative = text.starts_with("ifndef::")
			body = drop(text, if negative 8 else 7)
			parts = directive(body)
			defined = condition(parts.target, $attributes)
			enabled = if negative !defined else defined
			if parts.target != "" and parts.body == "" {
				$conditions = $conditions.append({ enabled: active and enabled, target: lower(parts.target) })
			} else if parts.target != "" and active and enabled {
				if $opaque == "" and (!is_definition(parts.body) or $definition_allowed) {
					nested = process_lines([{ text: parts.body, line: number, offset }], path, $attributes, options, stack, depth, { definition_allowed: $definition_allowed, opaque: $opaque, header: $header })?
					$attributes = nested.attributes
					$index_ready = False
					$definition_allowed = nested.context.definition_allowed
					$opaque = nested.context.opaque
					$header = nested.context.header
					for line in nested.lines {
						$lines = $lines.append(line)
					}
				} else {
					$lines = $lines.append({ text: parts.body, attributes: $attributes, path, line: number, offset })
				}
			}
		} else if text.starts_with("ifeval::") and complete_directive(text) {
			parsed = directive(drop(text, 8))
			expression = parsed.body
			if parsed.target == "" and ["==", "!=", "<=", ">=", "<", ">"].any(|operator| expression.contains(operator)) {
				$conditions = $conditions.append({ enabled: active and evaluate_with(expression, $attributes), target: "" })
			}
		} else if active {
			if $continuation != "" {
				$continuation = Str.concat($continuation, trim(text))
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
			} else if text.starts_with("include::") and complete_directive(text) {
				parsed = directive(AsciiDocPreprocess.substitute(drop(text, 9), $attributes))
				target = AsciiDocPreprocess.substitute(parsed.target, $attributes)
				resolved = resolve(path, target)
				found = if options.expand_includes options.includes.find_first(|entry| entry.path == resolved) else Err(NotFound)
				match found {
					Ok(entry) => {
						if depth >= options.max_depth or stack.contains(resolved) {
							return Err(PreprocessError({ path, line: number, offset, message: Str.concat("include expansion limit or cycle: ", resolved) }))
						}
						if invalid_path(target) or resolved == "" or resolved == ".." or resolved.starts_with("../") {
							return Err(PreprocessError({ path, line: number, offset, message: "virtual includes prohibit absolute paths, URIs and escaping the root" }))
						}
						selected = select_lines(entry.source, parsed.body)
						prepared = adjust_lines(selected.map(|line| { text: line.text, line: line.line, offset: line.offset, path: resolved, attributes: $attributes }), parsed.body)
						previous_offset = $attributes.find_first(|a| a.name == "leveloffset")
						include_offset = option(parsed.body, "leveloffset")
						base_offset = I64.from_str((previous_offset.map_ok(|a| a.value) ?? "0").replace_each("+", "")) ?? 0
						delta = I64.from_str(include_offset.replace_each("+", "")) ?? 0
						effective_offset = if include_offset.starts_with("+") or include_offset.starts_with("-") base_offset + delta else delta
						nested_attrs = if include_offset == "" $attributes else $attributes.keep_if(|a| a.name != "leveloffset").append({ name: "leveloffset", value: effective_offset.to_str() })
						nested = process_lines(prepared.map(|line| { text: line.text, line: line.line, offset: line.offset }), resolved, nested_attrs, options, stack.append(resolved), depth + 1, { definition_allowed: $definition_allowed, opaque: $opaque, header: $header })?
						$attributes = if include_offset == "" nested.attributes else match previous_offset {
							Ok(previous) => nested.attributes.keep_if(|a| a.name != "leveloffset").append(previous)
							Err(_) => nested.attributes.keep_if(|a| a.name != "leveloffset")
						}
						$index_ready = False
						$definition_allowed = nested.context.definition_allowed
						$opaque = nested.context.opaque
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
							$lines = $lines.append({ text: Str.concat(Str.concat("link:", target), "[role=include]"), attributes: $attributes, path, line: number, offset })
						}
					}
				}
			} else {
				emitted = if (text.starts_with("\\include::") or text.starts_with("\\ifdef::") or text.starts_with("\\ifndef::") or text.starts_with("\\endif::") or text.starts_with("\\ifeval::")) and complete_directive(drop(text, 1)) drop(text, 1) else text
				$lines = $lines.append({ text: emitted, attributes: $attributes, path, line: number, offset })
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
			if !$index_ready {
				$attribute_index = index_attributes($attributes)
			}
			parts = split_first(drop($definition_text, 1), ":")
			unset = parts.before.starts_with("!") or parts.before.ends_with("!")
			name = normalize_attribute_name(strip_suffix(if parts.before.starts_with("!") drop(parts.before, 1) else parts.before, "!"))
			revision_name = Str.concat("__revision:", name)
			# Keep the mutable growing containers in this loop. Returning the
			# Dict and attribute list through a helper forces backing-buffer
			# copies on the pinned compiler, despite consuming the old values.
			if !$attribute_index.contains(name) and !$attribute_index.contains(revision_name) {
				value = if unset "" else AsciiDocInline.attribute_value(trim(parts.after), $attributes)
				if !unset {
					$attribute_index = $attribute_index.insert(name, $attributes.len())
					$attributes = $attributes.append({ name, value })
				}
				$attribute_index = $attribute_index.insert(revision_name, $attributes.len())
				$attributes = $attributes.append({ name: revision_name, value: "1" })
			} else if !unset and $attributes.len() >= 2 and $attribute_index.get(name) == Ok($attributes.len() - 2) and $attribute_index.get(revision_name) == Ok($attributes.len() - 1) {
				## The most recently defined name already occupies the canonical
				## remove-then-append position. Keep its stable index and replace
				## only that final pair, retaining earlier per-line snapshots.
				value = AsciiDocInline.attribute_value(trim(parts.after), $attributes)
				revision = U64.from_str(($attributes.last() ?? { name: "", value: "0" }).value) ?? 0
				$attributes = $attributes.drop_last(2).append({ name, value }).append({ name: revision_name, value: (revision + 1).to_str() })
			} else {
				{ attributes: next_attributes, index: next_index } = define_indexed($definition_text, $attributes, $attribute_index, True)
				$attributes = next_attributes
				$attribute_index = next_index
			}
			$index_ready = True
		}
		for line in $lines.drop_first(emitted_before) {
			$expanded_size = $expanded_size + line.text.to_utf8().len() + 1
		}
		if $expanded_size > 1048576 {
			return Err(PreprocessError({ path, line: number, offset, message: "maximum expanded source size exceeded (1048576 bytes)" }))
		}
	}
	Ok({ lines: $lines, attributes: $attributes, context: { definition_allowed: $definition_allowed, opaque: $opaque, header: $header } })
}

is_definition : Str -> Bool
is_definition = |source| {
	if !source.starts_with(":") or source.starts_with("::") {
		return False
	}
	tail = drop(source, 1)
	parts = split_first(tail, ":")
	tail.contains(":") and starts_word(if parts.before.starts_with("!") drop(parts.before, 1) else parts.before) and (parts.after == "" or parts.after.starts_with(" ") or parts.after.starts_with("\t"))
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
	current = if ready index else index_attributes(attributes)
	parts = split_first(drop(source, 1), ":")
	unset = parts.before.starts_with("!") or parts.before.ends_with("!")
	name = normalize_attribute_name(strip_suffix(if parts.before.starts_with("!") drop(parts.before, 1) else parts.before, "!"))
	revision_name = Str.concat("__revision:", name)
	value = if unset "" else AsciiDocInline.attribute_value(trim(parts.after), attributes)
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
		defined = if unset remaining else remaining.append({ name, value })
		updated = defined.append({ name: revision_name, value: (revision + 1).to_str() })
		{ attributes: updated, index: index_attributes(updated) }
	}
}

condition : Str, List(AsciiDocPreprocess.Attribute) -> Bool
condition = |source, attributes| {
	if source.contains(",") {
		source.split_on(",").any(|name| attributes.any(|attribute| attribute.name == lower(trim(name))))
	} else {
		source.split_on("+").all(|name| attributes.any(|attribute| attribute.name == lower(trim(name))))
	}
}

evaluate : Str -> Bool
evaluate = |source| evaluate_with(source, [])

IntegerValue : { negative : Bool, digits : Str }

ExpressionValue : [Text(Str), Integer(IntegerValue), Floating(F64), Truth(Bool), Missing]

## Quotes classify operands before attribute expansion: "10" remains a string.
evaluate_with : Str, List(AsciiDocPreprocess.Attribute) -> Bool
evaluate_with = |source, attributes| {
	## EvalExpressionRx uses the first operator after a nonempty left operand;
	## it does not interpret quotes while finding that operator.
	text = trim(source)
	bytes = text.to_utf8()
	var $index = 1
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		next = bytes.get($index + 1) ?? 0
		length = if (byte == '=' or byte == '!' or byte == '<' or byte == '>') and next == '=' 2 else if byte == '<' or byte == '>' 1 else 0
		if length > 0 and $index + length < bytes.len() {
			operator = slice(text, $index, length)
			left = expression_value(trim(slice(text, 0, $index)), attributes)
			right = expression_value(trim(drop(text, $index + length)), attributes)
			return match (left, right) {
				(Text(a), Text(b)) => compare_strings(a, b, operator)
				(Integer(a), Integer(b)) => numeric_relation(integer_order(a, b), operator)
				(Floating(a), Floating(b)) => numeric_relation(if a < b -1 else if a > b 1 else 0, operator)
				(Integer(a), Floating(b)) => numeric_relation(integer_float_order(a, b), operator)
				(Floating(a), Integer(b)) => numeric_relation(0 - integer_float_order(b, a), operator)
				_ => if operator == "==" left == right else if operator == "!=" left != right else False
			}
		}
		$index = $index + 1
	}
	False
}

expression_value : Str, List(AsciiDocPreprocess.Attribute) -> ExpressionValue
expression_value = |source, attributes| {
	unquoted = unquote(source)
	quoted = unquoted != source

	## Missing references drop in conditional expressions (unlike normal text).
	var $value = AsciiDocPreprocess.substitute(unquoted, attributes)
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
	start = if negative or bytes_number.first() == Ok('+') 1 else 0
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
			$prefix = Str.concat(if $prefix == "" "0" else $prefix, Str.concat(".", fraction.digits))
			$end = fraction.end
			$has_fraction = True
		}
	}

	## Ruby ignores the exponent after a decimal point with no following digit.
	if ($has_fraction or bytes_number.get($end) != Ok('.')) and $prefix != "" and (bytes_number.get($end) == Ok('e') or bytes_number.get($end) == Ok('E')) {
		exponent_start = $end + 1
		exponent_sign = bytes_number.get(exponent_start) ?? 0
		exponent = numeric_digits(bytes_number, if exponent_sign == '+' or exponent_sign == '-' exponent_start + 1 else exponent_start)
		if exponent.digits != "" {
			$prefix = Str.concat($prefix, Str.concat(if exponent_sign == '-' "e-" else "e", exponent.digits))
		}
	}
	if $prefix == "" {
		return Floating(0.0)
	}
	prefix = if negative Str.concat("-", $prefix) else $prefix

	## A syntactically valid numeric prefix can fail F64 parsing only on overflow.
	Floating(F64.from_str(prefix) ?? if negative 0.0 - F64.infinity else F64.infinity)
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
		return if a.negative -1 else 1
	}
	order = if a.digits.to_utf8().len() < b.digits.to_utf8().len() -1 else if a.digits.to_utf8().len() > b.digits.to_utf8().len() 1 else lexical_order(a.digits, b.digits)
	if a.negative 0 - order else order
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
			return if x < y -1 else 1
		}
		$index = $index + 1
	}
	if left.len() < right.len() -1 else if left.len() > right.len() 1 else 0
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
		return if float < 0.0 1 else -1
	}
	negative = float < 0.0
	if integer.negative != negative {
		return if integer.negative -1 else 1
	}
	bits = float.to_bits()
	exponent = bits.shr_wrap(52).bitwise_and(2047)
	mantissa = bits.bitwise_and(4503599627370495) + if exponent == 0 0 else 4503599627370496
	var $digits = "0"
	var $fraction = False
	if exponent >= 1075 {
		$digits = mantissa.to_str()
		var $remaining = exponent - 1075
		while $remaining > 0 {
			$digits = decimal_double($digits)
			$remaining = $remaining - 1
		}
	} else {
		shift = if exponent == 0 1074 else 1075 - exponent
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
		if negative 1 else -1
	} else order
}

decimal_double : Str -> Str
decimal_double = |digits| {
	bytes = digits.to_utf8()
	var $remaining = bytes.len()
	var $carry = 0
	var $reversed = []
	while $remaining > 0 {
		$remaining = $remaining - 1
		value = (bytes.get($remaining) ?? '0') - '0'
		doubled = value * 2 + $carry
		$carry = doubled.div_trunc_by(10)
		$reversed = $reversed.append('0' + doubled - $carry * 10)
	}
	if $carry > 0 {
		$reversed = $reversed.append('0' + $carry)
	}
	var $output = []
	var $index = $reversed.len()
	while $index > 0 {
		$index = $index - 1
		$output = $output.append($reversed.get($index) ?? '0')
	}
	Str.from_utf8_lossy($output)
}

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
expand = |source, attributes, depth| {
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
				match attributes.find_first(|attribute| attribute.name == lower(name)) {
					Ok(attribute) => {
						$output = Str.concat($output, expand(attribute.value, attributes, depth + 1))
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
strip_suffix = |source, suffix| if source.ends_with(suffix) slice(source, 0, source.to_utf8().len() - suffix.to_utf8().len()) else source

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
	slice(source, $start, $end - $start)
}

lower : Str -> Str
lower = |source| {
	bytes = source.to_utf8()
	if bytes.all(|byte| byte < 128) {
		return Str.from_utf8_lossy(bytes.map(|byte| if byte >= 'A' and byte <= 'Z' byte + 32 else byte))
	}
	match Case.to_lower(source, Case.unicode_default, Case.unlimited_limits) {
		Ok(result) => Case.result_text(result)
		Err(_) => source
	}
}

unquote : Str -> Str
unquote = |source| if (source.starts_with("\"") and source.ends_with("\"")) or (source.starts_with("'") and source.ends_with("'")) (if source.to_utf8().len() < 2 "" else slice(source, 1, source.to_utf8().len() - 2)) else source

resolve : Str, Str -> Str
resolve = |path, target| {
	parts = path.split_on("/").drop_last(1)
	combined = if target.starts_with("/") target else join(parts.append(target), "/")
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
option = |options, name| {
	var $value = ""
	for piece in split_options(options) {
		parsed = split_first(piece, "=")
		if piece.contains("=") and trim(parsed.before) == name {
			$value = option_value(trim(parsed.after))
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

select_lines : Str, Str -> List(SourceLine)
select_lines = |source, options| {
	selector = option(options, "lines")
	tag = option(options, "tag")
	tags = if tag == "" option(options, "tags") else tag
	if tags == "" or selector != "" {
		return if selector == "" source_lines(source) else select_line_numbers(source_lines(source), selector)
	}
	var $output = []
	var $active_tags = []
	for entry in source_lines(source) {
		line = entry.text
		if line.contains("tag::") and line.contains("[]") {
			name = split_first(split_first(line, "tag::").after, "[]").before
			$active_tags = $active_tags.append(name)
		} else if line.contains("end::") and line.contains("[]") {
			name = split_first(split_first(line, "end::").after, "[]").before
			$active_tags = $active_tags.keep_if(|active| active != name)
		} else {
			selected_line = True
			selected_tag = select_tag(tags, $active_tags)
			if selected_line and selected_tag {
				$output = $output.append(entry)
			}
		}
	}
	$output
}

## The native reader sorts finite selected line numbers before an infinity
## marker. A nonpositive first number blocks the cursor; an open range starts
## selecting remaining lines only after every explicit finite number is read.
select_line_numbers : List(SourceLine), Str -> List(SourceLine)
select_line_numbers = |lines, selector| {
	var $ranges = []
	var $infinite = False
	var $largest = 0
	cap = lines.len() + 1
	for range in selectors(selector) {
		pieces = split_first(range, "..")
		first = selector_integer(pieces.before)
		last = selector_integer(pieces.after)
		open = range.contains("..") and (pieces.after == "" or last.negative)
		if !range.contains("..") or open or integer_order(first, last) <= 0 {
			if first.negative or first.digits == "0" {
				return []
			}
			start = bounded_selector_number(first, cap)
			end = if range.contains("..") and !open bounded_selector_number(last, cap) else start
			$ranges = $ranges.append({ start, end })
			if end > $largest {
				$largest = end
			}
			if open {
				$infinite = True
			}
		}
	}
	if $ranges.is_empty() {
		return lines
	}
	lines.keep_if(|line| ($infinite and line.line > $largest) or $ranges.any(|range| line.line >= range.start and line.line <= range.end))
}

selector_integer : Str -> IntegerValue
selector_integer = |source| {
	bytes = trim(source).to_utf8()
	negative = bytes.first() == Ok('-')
	start = if negative or bytes.first() == Ok('+') 1 else 0
	canonical_integer(negative, numeric_digits(bytes, start).digits)
}

bounded_selector_number : IntegerValue, U64 -> U64
bounded_selector_number = |integer, cap| {
	number = U64.from_str(integer.digits) ?? cap
	if number > cap cap else number
}

adjust_lines : List(AsciiDocPreprocess.Line), Str -> List(AsciiDocPreprocess.Line)
adjust_lines = |lines, options| {
	indent_text = option(options, "indent")
	indent = U64.from_str(indent_text) ?? 0
	var $minimum = 18446744073709551615
	if indent_text != "" {
		for line in lines {
			if trim(line.text) != "" {
				count = leading_spaces(line.text)
				if count < $minimum {
					$minimum = count
				}
			}
		}
	}
	lines.map(
		|line| {
			var $text = line.text
			if indent_text != "" and $minimum != 18446744073709551615 {
				count = leading_spaces($text)
				remove = if count < $minimum count else $minimum
				$text = Str.concat(repeat(" ", indent), drop($text, remove))
			}
			{ text: $text, attributes: line.attributes, path: line.path, line: line.line, offset: line.offset }
		},
	)
}

leading_spaces : Str -> U64
leading_spaces = |source| {
	var $count = 0
	bytes = source.to_utf8()
	while bytes.get($count) == Ok(' ') {
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
	if bytes.get($count) == Ok(' ') $count else 0
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
expect select_lines("one\ntwo\nthree\nfour\n", "lines=2..3").map(|line| (line.text, line.line, line.offset)) == [("two", 2, 4), ("three", 3, 8)]
expect select_lines("one\ntwo\nthree\nfour\n", "lines=1;3..-1").map(|line| line.text) == ["one", "three", "four"]
expect select_lines("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tag=x").map(|line| line.text) == ["inside"]
expect select_lines("before\n// tag::x[]\ninside\n// end::x[]\nafter", "tags=**;!x").map(|line| line.text) == ["before", "after"]
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

select_tag : Str, List(Str) -> Bool
select_tag = |selector, active_tags| {
	if selector == "" {
		return True
	}
	entries = selectors(selector)
	positives = entries.keep_if(|name| !name.starts_with("!"))
	included = positives.is_empty() or positives.any(|name| if name == "**" True else if name == "*" !active_tags.is_empty() else active_tags.contains(name))
	excluded = entries.any(|name| if !name.starts_with("!") False else if name == "!**" True else if name == "!*" !active_tags.is_empty() else active_tags.contains(drop(name, 1)))
	included and !excluded
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
		$order = if x < y -1 else if x > y 1 else 0
		$index = $index + 1
	}
	if $order == 0 {
		$order = if a.len() < b.len() -1 else if a.len() > b.len() 1 else 0
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
standard_attributes = [
	{ name: "asciidoctor", value: "" },
	{ name: "asciidoctor-version", value: "2.0.23" },
	{ name: "backend", value: "html5" },
	{ name: "backend-html5", value: "" },
	{ name: "basebackend", value: "html" },
	{ name: "basebackend-html", value: "" },
	{ name: "backend-html5-doctype-article", value: "" },
	{ name: "basebackend-html-doctype-article", value: "" },
	{ name: "doctype", value: "article" },
	{ name: "doctype-article", value: "" },
	{ name: "filetype", value: "html" },
	{ name: "filetype-html", value: "" },
	{ name: "htmlsyntax", value: "html" },
	{ name: "safe-mode-level", value: "20" },
	{ name: "safe-mode-name", value: "secure" },
	{ name: "safe-mode-secure", value: "" },
	{ name: "docdate", value: "2000-01-01" },
	{ name: "docyear", value: "2000" },
	{ name: "doctime", value: "00:00:00 UTC" },
	{ name: "docdatetime", value: "2000-01-01 00:00:00 UTC" },
	{ name: "localdate", value: "2000-01-01" },
	{ name: "localyear", value: "2000" },
	{ name: "localtime", value: "00:00:00 UTC" },
	{ name: "localdatetime", value: "2000-01-01 00:00:00 UTC" },
	{ name: "docdir", value: "" },
	{ name: "outdir", value: "." },
	{ name: "user-home", value: "." },
	{ name: "attribute-missing", value: "skip" },
	{ name: "attribute-undefined", value: "drop-line" },
	{ name: "outfilesuffix", value: ".html" },
	{ name: "sectids", value: "" },
	{ name: "prewrap", value: "" },
	{ name: "embedded", value: "" },
	{ name: "notitle", value: "" },
	{ name: "toc-placement", value: "auto" },
	{ name: "toc-title", value: "Table of Contents" },
	{ name: "stylesdir", value: "." },
	{ name: "iconsdir", value: "./images/icons" },
	{ name: "appendix-caption", value: "Appendix" },
	{ name: "appendix-refsig", value: "Appendix" },
	{ name: "chapter-refsig", value: "Chapter" },
	{ name: "part-refsig", value: "Part" },
	{ name: "section-refsig", value: "Section" },
	{ name: "example-caption", value: "Example" },
	{ name: "figure-caption", value: "Figure" },
	{ name: "table-caption", value: "Table" },
	{ name: "note-caption", value: "Note" },
	{ name: "tip-caption", value: "Tip" },
	{ name: "important-caption", value: "Important" },
	{ name: "warning-caption", value: "Warning" },
	{ name: "caution-caption", value: "Caution" },
	{ name: "untitled-label", value: "Untitled" },
	{ name: "version-label", value: "Version" },
	{ name: "last-update-label", value: "Last updated" },
]

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

expect select_lines("// tag::x[]\ninside\n// end::x[]", "").map(|line| line.text) == ["// tag::x[]", "inside", "// end::x[]"]
expect select_lines("// tag::x[]\ninside\n// end::x[]", "lines=1..2,tag=x").map(|line| line.text) == ["// tag::x[]", "inside"]
expect select_lines("before\n// tag::x[]\nx\n// tag::y[]\nxy\n// end::y[]\n// end::x[]\nafter", "tags=x;!y").map(|line| line.text) == ["x"]
expect select_lines("before\n// tag::x[]\nx\n// end::x[]\nafter", "tags=**").map(|line| line.text) == ["before", "x", "after"]
expect select_lines("before\n// tag::x[]\nx\n// end::x[]\nafter", "tags=!*").map(|line| line.text) == ["before", "after"]

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
	var $parts = source.split_on(if source.contains(",") "," else ";")
	while !$parts.is_empty() and $parts.last() == Ok("") {
		$parts = $parts.drop_last(1)
	}
	$parts
}
expect select_lines("one\ntwo\nthree\nfour", "lines=\"1..2,4\"").map(|line| line.text) == ["one", "two", "four"]
expect select_lines("// tag::x[]\nx\n// end::x[]\n// tag::y[]\ny\n// end::y[]", "tags=\"x,y\"").map(|line| line.text) == ["x", "y"]

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
	if bytes.all(|byte| byte < 128) {
		return Str.from_utf8_lossy(bytes.keep_if(|byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '_' or byte == '-').map(|byte| if byte >= 'A' and byte <= 'Z' byte + 32 else byte))
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

expect select_lines("one\ntwo\nthree\nfour", "lines=\"\"1..2,4\"").map(|line| line.text) == ["one", "two", "three", "four"]
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

expect select_lines("one\ntwo\nthree\nfour", "lines=\"1..2,part.adoc[lines=\"1..2,4\"").is_empty()
expect select_lines("one\ntwo\nthree\nfour", "lines=\"2..,4\"").map(|line| line.text) == ["two", "four"]
expect select_lines("one\ntwo\nthree\nfour", "lines=4..2").map(|line| line.text) == ["one", "two", "three", "four"]
expect select_lines("one\ntwo\nthree\nfour", "lines=\"1,4,\"").map(|line| line.text) == ["one", "four"]
expect select_lines("one\ntwo\nthree\nfour", "lines=4abc").map(|line| line.text) == ["four"]
expect select_lines("one\ntwo\nthree\nfour", "lines=999999999999999999999999999999999999999..999999999999999999999999999999999999998").len() == 4
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
