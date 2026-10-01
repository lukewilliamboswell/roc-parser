app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Utf8
import parser.Xml

## Arbitrary UTF-8 must parse or fail cleanly, and:
## - errors point at a real one-based line and byte column with a message;
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
					result = Xml.parse_str(input)
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
			$index = if byte == '\r' and bytes.get($index + 1) == Ok('\n') $index + 2 else $index + 1
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
			if error.line == 0 or error.column == 0 {
				crash "XML error locations must be one-based: ${show(result)}"
			}
			if error.message.is_empty() {
				crash "XML errors must explain the problem"
			}
			lengths = line_lengths(bytes)
			match lengths.get(error.line - 1) {
				Err(_) => crash "error line ${error.line.to_str()} is past the last line (${lengths.len().to_str()}): ${show(result)}"
				Ok(length) =>
					if error.column > length + 1 {
						crash "error column ${error.column.to_str()} is past the end of line ${error.line.to_str()} (${length.to_str()} bytes): ${show(result)}"
					}
			}
		}
	}
}

check_combinator : Str, Parsed -> {}
check_combinator = |input, result| {
	combinator = Utf8.parse_str(Xml.parser, input)
	consistent =
		match (result, combinator) {
			(Ok(tree), Ok(other)) => tree == other
			(Err(InvalidXml(error)), Err(ParseError({ message, offset }))) => {
				location = line_column(input.to_utf8(), offset)
				same_place = location.line == error.line and location.column == error.column
				if message == "unexpected input" {
					same_place and error.message == "unexpected content after the root element"
				} else {
					same_place and message == error.message
				}
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
	changed = Xml.parse_str(Str.replace_each(input, "\n", line_end))
	if changed != result {
		crash "line ends ${Str.inspect(line_end)} changed the result\nLF:    ${show(result)}\nother: ${show(changed)}"
	}
}

check_leading_comment : Str, Parsed -> {}
check_leading_comment = |input, result| {
	commented = Xml.parse_str("<!-- c -->\n${input}")
	consistent =
		match (result, commented) {
			(Ok(plain), Ok(with_comment)) => plain == with_comment
			(Err(InvalidXml(plain)), Err(InvalidXml(with_comment))) =>
				plain.line + 1 == with_comment.line and plain.column == with_comment.column and plain.message == with_comment.message
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
		Err(InvalidXml(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
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
			depth = nesting_depth(Xml.parse_str(input))
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
			match Xml.parse_str("<e${attributes}/>") {
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
			if Xml.parse_str("<e${attributes} a0='x'/>").is_ok() {
				crash "duplicate attribute after ${size.to_str()} attributes was accepted"
			}
		}
		2 => {
			# Long text made of references.
			input = "<t>${Str.repeat("&amp;&#x41;&lt;", size)}</t>"
			expected = Str.repeat("&A<", size)
			if Xml.parse_str(input).map_ok(|xml| xml.root) != Ok(Element({ name: "t", attributes: [], children: [Text(expected)] })) {
				crash "${size.to_str()} references parsed wrongly"
			}
		}
		3 => {
			# Many siblings interleaved with comments and CDATA, merging into one text.
			input = "<t>${Str.repeat("a<!--c-->b<![CDATA[c]]><?p?>", size)}</t>"
			expected = Str.repeat("abc", size)
			if Xml.parse_str(input).map_ok(|xml| xml.root) != Ok(Element({ name: "t", attributes: [], children: [Text(expected)] })) {
				crash "${size.to_str()} merged text pieces parsed wrongly"
			}
		}
		_ => {
			# Unclosed deep nesting must fail at the end of input, not crash.
			input = Str.repeat("<a>", size)
			match Xml.parse_str(input) {
				Err(InvalidXml(error)) if error.line == 1 and error.column == Str.count_utf8_bytes(input) + 1 => {}
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

target = Fuzz.target_with({
	name: "xml-raw",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
