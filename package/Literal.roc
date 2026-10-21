## Checked source conversions for quoted values and imported text.
##
## Use [Literal.require] in a top-level declaration to make a checked imported
## file fail compilation when its contents are invalid. Dynamic inputs should
## keep their `Try` result: calling `require` at runtime can crash at runtime.
Literal :: [].{

	## Unwrap a checked conversion, or fail with its explanation.
	## A demanded top-level call runs during compilation.
	require : Try(a, [BadQuotedBytes(Str)]) -> a
	require = |result| match result {
		Ok(value) => value
		Err(BadQuotedBytes(message)) => crash message
	}

	## The available location and explanation of a checked conversion failure.
	## Zero line or column means that coordinate is unavailable.
	ErrorProps := {
		name : Str ?? "<literal>",
		format : Str,
		message : Str,
		line : U64 ?? 0,
		column : U64 ?? 0,
	}

	## Build a simple compiler quotation error without inventing coordinates.
	error : ErrorProps -> [BadQuotedBytes(Str)]
	error = |props| {
		location = if props.line == 0 {
			props.name
		} else if props.column == 0 {
			"${props.name}:${props.line.to_str()}"
		} else {
			"${props.name}:${props.line.to_str()}:${props.column.to_str()}"
		}
		BadQuotedBytes("${location}: ${props.format}: ${props.message}")
	}

	## Convert a UTF-8 byte offset to a one-based line and byte column.
	## CRLF, CR, and LF each end one line; offsets past the end are clamped.
	position : Str, U64 -> { line : U64, column : U64 }
	position = |source, offset| {
		bytes = source.to_utf8()
		end = if (offset < bytes.len()) offset else bytes.len()
		var $index = 0
		var $line = 1
		var $column = 1
		while $index < end {
			byte = bytes.get($index) ?? 0
			if byte == '\r' {
				$line = $line + 1
				$column = 1
				if $index + 1 < end and bytes.get($index + 1) == Ok('\n') {
					$index = $index + 1
				}
			} else if byte == '\n' {
				$line = $line + 1
				$column = 1
			} else {
				$column = $column + 1
			}
			$index = $index + 1
		}
		{ line: $line, column: $column }
	}
}

expect Literal.require(Ok(42)) == 42
expect Literal.error({ format: "CSV", name: "people.csv", message: "missing age" }) == BadQuotedBytes("people.csv: CSV: missing age")
expect Literal.error({ format: "YAML", message: "bad value", line: 2, column: 3 }) == BadQuotedBytes("<literal>:2:3: YAML: bad value")
expect Literal.position("a\r\nb\rc\né", 9) == { line: 4, column: 3 }
expect Literal.position("a\r\nb", 3) == { line: 2, column: 1 }
