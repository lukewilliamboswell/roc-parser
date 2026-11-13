## Shared numeric microsyntax for XML-oriented SVG and MathML attributes.
## Offsets count UTF-8 bytes. No locale-dependent or non-finite numbers.
MarkupValues :: [].{

	## A parsed finite decimal and its source interval.
	Number : { value : F64, start : U64, end : U64 }

	## Scan one signed decimal, optionally with an exponent, at an exact offset.
	number : List(U8), U64 -> Try(Number, [InvalidNumber(U64)])
	number = |bytes, start| {
		var $pos = start
		if sign(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		var $digits = 0
		while digit(bytes.get($pos)) {
			$digits = $digits + 1
			$pos = $pos + 1
		}
		integer_end = $pos
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
		if (bytes.get($pos) == Ok('e') or bytes.get($pos) == Ok('E')) and (digit(bytes.get($pos + 1)) or (sign(bytes.get($pos + 1) ?? 0) and digit(bytes.get($pos + 2)))) {
			$pos = $pos + 1
			if sign(bytes.get($pos) ?? 0) {
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
		if $pos == integer_end and $digits <= 15 {
			value = short_integer(bytes, $pos - $digits, $pos).to_f64()
			return Ok({ value: if (bytes.get(start) == Ok('-')) value.negate() else value, start, end: $pos })
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
		while space(bytes.get($pos) ?? 0) {
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
		while $end > start and space(bytes.get($end - 1) ?? 0) {
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
		if !length_unit(lower_unit) {
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

# Called only after the existing scanner proves 1 to 15 ASCII decimal digits.
# Maximum 999_999_999_999_999 fits both U64 and exact F64 integer precision.
short_integer : List(U8), U64, U64 -> U64
short_integer = |bytes, start, end| {
	var $value = 0.U64
	var $index = start
	while $index < end {
		digit_value = (bytes.get($index) ?? 48).to_u64().minus_wrap(48)
		$value = $value.times_wrap(10).plus_wrap(digit_value)
		$index = $index + 1
	}
	$value
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

sign : U8 -> Bool
sign = |byte| byte == '+' or byte == '-'

space : U8 -> Bool
space = |byte| byte == ' ' or byte == '\t' or byte == '\r' or byte == '\n' or byte == 12

length_unit : Str -> Bool
length_unit = |unit| match unit {
	"" | "%" | "px" | "em" | "ex" | "ch" | "rem" | "vw" | "vh" | "vmin" | "vmax" | "cm" | "mm" | "q" | "in" | "pt" | "pc" => True
	_ => False
}

expect MarkupValues.number("-0".to_utf8(), 0).map_ok(|parsed| parsed.value.to_bits()) == Ok(0x8000_0000_0000_0000)
expect MarkupValues.number("xx-999999999999999,".to_utf8(), 2) == Ok({ value: -999_999_999_999_999.F64, start: 2, end: 18 })
expect {
	["999999999999999", "1000000000000000", "-0.0", "1e3", "1e", "1.", ".5"].all(
		|source| match MarkupValues.number(source.to_utf8(), 0) {
			Ok(parsed) => {
				prefix = source.drop_last_bytes(source.count_utf8_bytes() - parsed.end) ?? ""
				F64.from_str(prefix).map_ok(|value| value.to_bits()) == Ok(parsed.value.to_bits())
			}
			Err(_) => False
		},
	)
}
