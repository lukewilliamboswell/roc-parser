## Shared YAML 1.2 core-schema spellings for decoding and source highlighting.
## Classification is lexical: numeric overflow remains a decoder concern.
YamlScalar :: [].{
	Category : [Text, Boolean, Null, Number]

	classify : Str -> Category
	classify = |text| {
		bytes = text.to_utf8()
		if !may_be_typed(bytes.first() ?? '~') {
			Text
		} else if is_null_text(text) {
			Null
		} else if is_true_text(text) or is_false_text(text) {
			Boolean
		} else if is_decimal_integer(bytes) or is_infinity_text(text) or is_negative_infinity_text(text) or is_nan_text(text) {
			Number
		} else {
			match bytes {
				['0', 'o', .. as digits] if !digits.is_empty() and digits.all(|b| b >= '0' and b <= '7') => Number
				['0', 'x', .. as digits] if !digits.is_empty() and digits.all(|b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')) => Number
				_ => if core_float_text(bytes).is_ok() Number else Text
			}
		}
	}

	is_null_text : Str -> Bool
	is_null_text = |text| {
		match text {
			"" | "null" | "Null" | "NULL" | "~" => True
			_ => False
		}
	}

	is_true_text : Str -> Bool
	is_true_text = |text| {
		match text {
			"true" | "True" | "TRUE" => True
			_ => False
		}
	}

	is_false_text : Str -> Bool
	is_false_text = |text| {
		match text {
			"false" | "False" | "FALSE" => True
			_ => False
		}
	}

	is_infinity_text : Str -> Bool
	is_infinity_text = |text| {
		match text {
			".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => True
			_ => False
		}
	}

	is_negative_infinity_text : Str -> Bool
	is_negative_infinity_text = |text| {
		match text {
			"-.inf" | "-.Inf" | "-.INF" => True
			_ => False
		}
	}

	is_nan_text : Str -> Bool
	is_nan_text = |text| {
		match text {
			".nan" | ".NaN" | ".NAN" => True
			_ => False
		}
	}

	may_be_typed : U8 -> Bool
	may_be_typed = |byte| {
		match byte {
			'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '+' | '-' | '.' | '~' | 'n' | 'N' | 't' | 'T' | 'f' | 'F' => True
			_ => False
		}
	}

	core_float_text : List(U8) -> Try(Str, [NotFloat])
	core_float_text = |bytes| {
		# Most plain scalars are words; reject them before slicing anything.
		if !bytes.all(is_float_byte) {
			return Err(NotFloat)
		}
		{ sign, unsigned } =
			match bytes {
				['-', .. as rest] => { sign: "-", unsigned: rest }
				['+', .. as rest] => { sign: "", unsigned: rest }
				_ => { sign: "", unsigned: bytes }
			}
		mantissa = unsigned.take_first(count_while(unsigned, |b| b != 'e' and b != 'E'))
		exponent = unsigned.drop_first(mantissa.len())
		whole = mantissa.take_first(count_while(mantissa, is_digit))
		after_whole = mantissa.drop_first(whole.len())
		{ has_point, fraction } =
			match after_whole {
				['.', .. as rest] => { has_point: True, fraction: rest }
				_ => { has_point: False, fraction: after_whole }
			}
		exponent_digits =
			match exponent {
				['e', '+', .. as rest] | ['e', '-', .. as rest] | ['E', '+', .. as rest] | ['E', '-', .. as rest] | ['e', .. as rest] | ['E', .. as rest] => rest
				_ => []
			}
		mantissa_ok = fraction.all(is_digit) and (!whole.is_empty() or (has_point and !fraction.is_empty()))
		exponent_ok = exponent.is_empty() or (!exponent_digits.is_empty() and exponent_digits.all(is_digit))
		# A plain integer is not a float; "1." and "1e3" are.
		is_float = has_point or !exponent.is_empty()

		if mantissa_ok and exponent_ok and is_float {
			whole_text = if whole.is_empty() "0" else bytes_to_str(whole)
			fraction_text = if fraction.is_empty() "0" else bytes_to_str(fraction)
			exponent_text = if exponent.is_empty() "" else bytes_to_str(exponent)
			Ok("${sign}${whole_text}.${fraction_text}${exponent_text}")
		} else {
			Err(NotFloat)
		}
	}

	is_decimal_integer : List(U8) -> Bool
	is_decimal_integer = |bytes| {
		match bytes {
			['+', .. as rest] | ['-', .. as rest] => !rest.is_empty() and all_digits(rest)
			_ => !bytes.is_empty() and all_digits(bytes)
		}
	}
}

is_float_byte : U8 -> Bool
is_float_byte = |b| (b >= '0' and b <= '9') or b == '.' or b == 'e' or b == 'E' or b == '+' or b == '-'

count_while : List(U8), (U8 -> Bool) -> U64
count_while = |bytes, keep| {
	var $count = 0
	while $count < bytes.len() and keep(bytes.get($count) ?? 0) {
		$count = $count + 1
	}
	$count
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

all_digits : List(U8) -> Bool
all_digits = |bytes| {
	var $index = 0
	len = bytes.len()
	while $index < len and is_digit(bytes.get($index) ?? 0) {
		$index = $index + 1
	}
	$index == len
}

bytes_to_str : List(U8) -> Str
bytes_to_str = |bytes| Str.from_utf8(bytes) ?? Str.from_utf8_lossy(bytes)
