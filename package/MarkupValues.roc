## Shared numeric microsyntax for XML-oriented SVG and MathML attributes.
## Offsets count UTF-8 bytes. No locale-dependent or non-finite numbers.
MarkupValues :: [].{

	## A parsed finite decimal and its source interval.
	Number : { value : F64, start : U64, end : U64 }

	## Scan one signed decimal, optionally with an exponent, at an exact offset.
	number : List(U8), U64 -> Try(Number, [InvalidNumber(U64)])
	number = |bytes, start| {
		var $pos = start
		if [Ok('+'), Ok('-')].contains(bytes.get($pos)) {
			$pos = $pos + 1
		}
		var $digits = 0
		while digit(bytes.get($pos)) {
			$digits = $digits + 1
			$pos = $pos + 1
		}
		if bytes.get($pos) == Ok('.') and digit(bytes.get($pos + 1)) {
			$pos = $pos + 1
			while digit(bytes.get($pos)) {
				$digits = $digits + 1
				$pos = $pos + 1
			}
		}
		if $digits == 0 {
			return Err(InvalidNumber(start))
		}
		if [Ok('e'), Ok('E')].contains(bytes.get($pos)) and (digit(bytes.get($pos + 1)) or ([Ok('+'), Ok('-')].contains(bytes.get($pos + 1)) and digit(bytes.get($pos + 2)))) {
			$pos = $pos + 1
			if [Ok('+'), Ok('-')].contains(bytes.get($pos)) {
				$pos = $pos + 1
			}
			exponent = $pos
			while digit(bytes.get($pos)) {
				$pos = $pos + 1
			}
			if $pos == exponent {
				return Err(InvalidNumber($pos))
			}
		}
		text = Str.from_utf8(bytes.sublist({ start, len: $pos - start })) ?? ""
		match F64.from_str(text) {
			Ok(value) if value.is_finite() => Ok({ value, start, end: $pos })
			_ => Err(InvalidNumber(start))
		}
	}

	## XML whitespace at an offset, without consuming punctuation.
	whitespace : List(U8), U64 -> U64
	whitespace = |bytes, start| {
		var $pos = start
		while [Ok(' '), Ok('\t'), Ok('\r'), Ok('\n'), Ok(12)].contains(bytes.get($pos)) {
			$pos = $pos + 1
		}
		$pos
	}

	## A complete comma/whitespace-separated list
	## Reject empty/trailing commas.
	numbers : Str -> Try(List(F64), [InvalidNumber(U64)])
	numbers = |text| {
		bytes = text.to_utf8()
		var $pos = whitespace(bytes, 0)
		var $values = []
		while $pos < bytes.len() {
			parsed = number(bytes, $pos)?
			$values = $values.append(parsed.value)
			$pos = whitespace(bytes, parsed.end)
			if $pos == bytes.len() {
				break
			}
			if bytes.get($pos) == Ok(',') {
				$pos = whitespace(bytes, $pos + 1)
				if $pos == bytes.len() {
					return Err(InvalidNumber($pos))
				}
			} else if $pos == parsed.end {
				return Err(InvalidNumber($pos))
			}
		}
		Ok($values)
	}

	## A complete scalar number.
	scalar : Str -> Try(F64, [InvalidNumber(U64)])
	scalar = |text| {
		bytes = text.to_utf8()
		parsed = number(bytes, whitespace(bytes, 0))?
		if whitespace(bytes, parsed.end) != bytes.len() {
			return Err(InvalidNumber(parsed.end))
		}
		Ok(parsed.value)
	}

	## Trim only CSS/XML ASCII whitespace, preserving non-breaking spaces.
	trim_space : Str -> Str
	trim_space = |text| {
		bytes = text.to_utf8()
		start = whitespace(bytes, 0)
		var $end = bytes.len()
		while $end > start and [9, 10, 12, 13, 32].contains(bytes.get($end - 1) ?? 0) {
			$end = $end - 1
		}
		Str.from_utf8(bytes.sublist({ start, len: $end - start })) ?? ""
	}

	## A CSS/SVG length with a recognized unit
	## Unitless numbers are allowed.
	length : Str -> Try({ value : F64, unit : Str }, [InvalidNumber(U64)])
	length = |text| {
		bytes = trim_space(text).to_utf8()
		parsed = number(bytes, 0)?
		unit = Str.from_utf8(bytes.drop_first(parsed.end)) ?? ""
		lower_unit = Str.from_utf8_lossy(unit.to_utf8().map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b))
		if !["", "%", "px", "em", "ex", "ch", "rem", "vw", "vh", "vmin", "vmax", "cm", "mm", "q", "in", "pt", "pc"].contains(lower_unit) {
			return Err(InvalidNumber(parsed.end))
		}
		Ok({ value: parsed.value, unit })
	}

	## Strict SVG transform-list syntax, with command arity checks.
	transforms : Str -> Try({}, [InvalidTransform({ offset : U64, command : Str })])
	transforms = |text| {
		bytes = text.to_utf8()
		var $pos = whitespace(bytes, 0)
		while $pos < bytes.len() {
			start = $pos
			while match bytes.get($pos) {
				Ok(b) => (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z')
				_ => False
			} {
				$pos = $pos + 1
			}
			command = Str.from_utf8(bytes.sublist({ start, len: $pos - start })) ?? ""
			$pos = whitespace(bytes, $pos)
			if bytes.get($pos) != Ok('(') {
				return Err(InvalidTransform({ offset: $pos, command }))
			}
			$pos = $pos + 1
			args = $pos
			while $pos < bytes.len() and bytes.get($pos) != Ok(')') {
				$pos = $pos + 1
			}
			if $pos == bytes.len() {
				return Err(InvalidTransform({ offset: $pos, command }))
			}
			values = numbers(Str.from_utf8(bytes.sublist({ start: args, len: $pos - args })) ?? "").map_err(|_| InvalidTransform({ offset: args, command }))?
			count = values.len()
			valid = match command {
				"matrix" => count == 6
				"translate" | "scale" => count == 1 or count == 2
				"rotate" => count == 1 or count == 3
				"skewX" | "skewY" => count == 1
				_ => False
			}
			if !valid {
				return Err(InvalidTransform({ offset: start, command }))
			}
			end = $pos + 1
			$pos = whitespace(bytes, end)
			if bytes.get($pos) == Ok(',') {
				$pos = whitespace(bytes, $pos + 1)
				if $pos == bytes.len() {
					return Err(InvalidTransform({ offset: $pos, command }))
				}
			} else if $pos < bytes.len() and $pos == end {
				return Err(InvalidTransform({ offset: $pos, command }))
			}
		}
		Ok({})
	}
}

digit : Try(U8, [OutOfBounds]) -> Bool
digit = |byte| match byte {
	Ok(b) => b >= '0' and b <= '9'
	_ => False
}

expect MarkupValues.scalar(" -1.25e+2 ") == Ok(-125)
expect MarkupValues.scalar("1e").is_err()
expect MarkupValues.scalar("inf").is_err()
expect MarkupValues.scalar("1e999").is_err()
expect MarkupValues.numbers("0, 1 .5 2e2") == Ok([0, 1, 0.5, 200])
expect MarkupValues.numbers("1,").is_err()
expect MarkupValues.numbers("1,,2").is_err()
expect MarkupValues.length("-2.5em") == Ok({ value: -2.5, unit: "em" })
expect MarkupValues.length("2garbage").is_err()
expect MarkupValues.transforms("translate(10 20) rotate(45,0,0) scale(2)").is_ok()
expect MarkupValues.transforms("matrix(1 0 0 1 0)").is_err()
expect MarkupValues.transforms("translate(2),").is_err()

expect MarkupValues.scalar("1.").is_err()
expect MarkupValues.length("2PX") == Ok({ value: 2, unit: "PX" })
expect MarkupValues.scalar("\u(000C)2") == Ok(2)

expect MarkupValues.length("\u(00A0)2px").is_err()
