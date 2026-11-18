import fuzz.Fuzz
import parser.Parser
import parser.Xml

## Arbitrary UTF-8 must parse or fail cleanly, and:
## - errors point at a real one-based line and byte column with a native problem;
## - Xml.parser agrees with Xml.parse_str;
## - LF, CRLF, and CR line endings give the same result (XML 1.0 2.11);
## - a leading comment line gives the same tree, with errors one line lower.
##
## Inputs starting with byte 0xFF instead build a pathological document
## (deep nesting, many attributes, long text or references) whose size comes
## from the next bytes; it must parse to the expected shape within --timeout.

Parsed : Try(Xml, [InvalidXml(Xml.Error)])

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match bytes {
		[0xFF, .. as rest] => pathological(rest)
		_ =>
			match Str.from_utf8(bytes) {
				Err(_) => Fuzz.reject
				Ok(input) => {
					result = Xml.parse_str(input, {})
					check_location(bytes, result)
					check_combinator(input, result)
					if !bytes.contains('\r') {
						check_line_ends(input, result, "\r\n")
						check_line_ends(input, result, "\r")
					}
					if !Str.starts_with(input, "<?xml") and !Str.starts_with(input, "\u(FEFF)") {
						check_leading_comment(input, result)
					}
					Fuzz.keep
				}
			}
	}
}

## Line lengths in bytes, where CR LF, CR, and LF each end a line.
line_lengths : List(U8) -> List(U64)
line_lengths = |bytes| {
	var $lengths = []
	var $current = 0
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' or byte == '\r' {
			$lengths = $lengths.append($current)
			$current = 0
			$index = if (byte == '\r' and bytes.get($index + 1) == Ok('\n')) $index + 2 else $index + 1
		} else {
			$current = $current + 1
			$index = $index + 1
		}
	}
	$lengths.append($current)
}

check_location : List(U8), Parsed -> {}
check_location = |bytes, result| {
	match result {
		Ok(_) => {}
		Err(InvalidXml(error)) => {
			match error.location {
				Text(at) => if at.offset > bytes.len() or line_column(bytes, at.offset) != { line: at.line, column: at.column } {
					crash "XML diagnostic byte offset disagrees with source coordinates"
				}
				_ => {}
			}
			if source_position(error.location).line == 0 or source_position(error.location).column == 0 {
				crash "XML error locations must be one-based: ${show(result)}"
			}
			lengths = line_lengths(bytes)
			match lengths.get(source_position(error.location).line - 1) {
				Err(_) => crash "error line ${source_position(error.location).line.to_str()} is past the last line (${lengths.len().to_str()}): ${show(result)}"
				Ok(length) =>
					if source_position(error.location).column > length + 1 {
						crash "error column ${source_position(error.location).column.to_str()} is past the end of line ${source_position(error.location).line.to_str()} (${length.to_str()} bytes): ${show(result)}"
					}
			}
		}
	}
}

check_combinator : Str, Parsed -> {}
check_combinator = |input, result| {
	combinator = Xml.parser({}).parse_str(input)
	consistent =
		match (result, combinator) {
			(Ok(tree), Ok(other)) => tree == other
			(Err(InvalidXml(error)), Err(ParseError({ problem, location, .. }))) => {
				same_place = source_position(location) == source_position(error.location)
				same_place and (Err(problem) == native_failure(error.problem) or (problem == UnexpectedInput and error.problem == TrailingContent))
			}
			_ => False
		}
	if !consistent {
		crash "Xml.parser disagrees with Xml.parse_str\nparse_str: ${show(result)}\nparser: ${Str.inspect(combinator)}"
	}
}

## One-based line and byte column of a byte offset, counting `\r\n`, `\r`
## and `\n` as line ends, as Xml.Error does.
line_column : List(U8), U64 -> { line : U64, column : U64 }
line_column = |bytes, offset| {
	var $line = 1
	var $line_start = 0
	var $index = 0
	while $index < offset and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' {
			$line = $line + 1
			$line_start = $index + 1
		} else if byte == '\r' {
			if $index + 1 < offset and bytes.get($index + 1) == Ok('\n') {
				$index = $index + 1
			}
			$line = $line + 1
			$line_start = $index + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: offset - $line_start + 1 }
}

check_line_ends : Str, Parsed, Str -> {}
check_line_ends = |input, result, line_end| {
	changed_source = Str.replace_each(input, "\n", line_end)
	changed = Xml.parse_str(changed_source, {})
	check_location(changed_source.to_utf8(), changed)
	same = match (result, changed) {
		(Ok(original), Ok(other)) => original == other
		(Err(InvalidXml(original)), Err(InvalidXml(other))) => original.problem == other.problem and original.path == other.path and source_position(original.location) == source_position(other.location)
		_ => False
	}
	if !same {
		crash "line ends ${Str.inspect(line_end)} changed the result\nLF:    ${show(result)}\nother: ${show(changed)}"
	}
}

check_leading_comment : Str, Parsed -> {}
check_leading_comment = |input, result| {
	commented = Xml.parse_str("<!-- c -->\n${input}", {})
	consistent =
		match (result, commented) {
			(Ok(plain), Ok(with_comment)) => plain == with_comment
			(Err(InvalidXml(plain)), Err(InvalidXml(with_comment))) =>
				source_position(plain.location).line + 1 == source_position(with_comment.location).line and source_position(plain.location).column == source_position(with_comment.location).column and plain.problem == with_comment.problem
			_ => False
		}
	if !consistent {
		crash "a leading comment changed the result\nplain:     ${show(result)}\ncommented: ${show(commented)}"
	}
}

show : Parsed -> Str
show = |result| {
	match result {
		Ok(xml) => Str.inspect(xml)
		Err(InvalidXml(error)) => "error ${source_position(error.location).line.to_str()}:${source_position(error.location).column.to_str()} ${Str.inspect(error.problem)}"
	}
}

## Pathological inputs

size_from : List(U8) -> U64
size_from = |bytes| {
	high = U8.to_u64(bytes.get(1) ?? 0)
	low = U8.to_u64(bytes.get(2) ?? 0)
	high * 64 + low + 1
}

pathological : List(U8) -> Fuzz.Outcome
pathological = |bytes| {
	size = size_from(bytes)
	match (bytes.get(0) ?? 0) % 5 {
		0 => {
			# Deep nesting: <a><a>...x...</a></a>
			input = "${Str.repeat("<a>", size)}x${Str.repeat("</a>", size)}"
			depth = nesting_depth(Xml.parse_str(input, {}))
			if depth != Ok(size) {
				crash "deep nesting of ${size.to_str()} parsed as ${Str.inspect(depth)}"
			}
		}
		1 => {
			# Many attributes on one element.
			var $attributes = ""
			var $index = 0
			while $index < size {
				$attributes = Str.concat($attributes, " a${$index.to_str()}='${$index.to_str()}'")
				$index = $index + 1
			}
			attributes = $attributes
			match Xml.parse_str("<e${attributes}/>", {}) {
				Ok(xml) => {
					count =
						match xml.root {
							Element({ name: _, attributes: parsed, children: [] }) => parsed.len()
							_ => 0
						}
					if count != size {
						crash "${size.to_str()} attributes parsed as ${count.to_str()}"
					}
				}
				Err(_) => crash "${size.to_str()} distinct attributes were rejected"
			}
			# The same with a duplicate at the end must be rejected.
			if Xml.parse_str("<e${attributes} a0='x'/>", {}).is_ok() {
				crash "duplicate attribute after ${size.to_str()} attributes was accepted"
			}
		}
		2 => {
			# Long text made of references.
			input = "<t>${Str.repeat("&amp;&#x41;&lt;", size)}</t>"
			expected = Str.repeat("&A<", size)
			if Xml.parse_str(input, {}).map_ok(|xml| xml.root) != Ok(Element({ name: "t", attributes: [], children: [Text(expected)] })) {
				crash "${size.to_str()} references parsed wrongly"
			}
		}
		3 => {
			# Many siblings interleaved with comments and CDATA, merging into one text.
			input = "<t>${Str.repeat("a<!--c-->b<![CDATA[c]]><?p?>", size)}</t>"
			expected = Str.repeat("abc", size)
			if Xml.parse_str(input, {}).map_ok(|xml| xml.root) != Ok(Element({ name: "t", attributes: [], children: [Text(expected)] })) {
				crash "${size.to_str()} merged text pieces parsed wrongly"
			}
		}
		_ => {
			# Unclosed deep nesting must fail at the end of input, not crash.
			input = Str.repeat("<a>", size)
			match Xml.parse_str(input, {}) {
				Err(InvalidXml(error)) if source_position(error.location).line == 1 and source_position(error.location).column == Str.count_utf8_bytes(input) + 1 => {}
				other => crash "unclosed nesting of ${size.to_str()} gave ${show(other)}"
			}
		}
	}
	Fuzz.keep
}

## Depth of the single chain of `a` elements ending in Text("x"), counted
## iteratively so the check itself cannot overflow the stack.
nesting_depth : Parsed -> Try(U64, [Unexpected])
nesting_depth = |result| {
	match result {
		Err(_) => Err(Unexpected)
		Ok(xml) => {
			var $node = xml.root
			var $depth = 0
			var $done = False
			var $answer = Err(Unexpected)
			while !$done {
				match $node {
					Element({ name: "a", attributes: [], children: [child] }) => {
						$depth = $depth + 1
						$node = child
					}
					Text("x") => {
						$answer = Ok($depth)
						$done = True
					}
					_ => {
						$done = True
					}
				}
			}
			$answer
		}
	}
}

property_target = Fuzz.target_with({
	examples: [],
	name: "xml-raw",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source coordinates"
}

native_failure : [..p] -> Try({}, Parser.Problem(p))
native_failure = |problem| {
	value = Err(problem)?
	Ok(value)
}

XmlRawProperty :: [].{
	target = property_target
}
