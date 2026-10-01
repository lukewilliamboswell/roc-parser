import Parser

## Parsers and helpers specialized to UTF-8 byte lists and Roc `Str` values.
##
## Use these with the combinators in [Parser] whenever the input is text.
## Parsers work on bytes (`Bytes`), so `codeunit` and `digit` match single
## bytes, and `parse_str` converts a `Str` to bytes and back for you.
Utf8 :: [].{

	## UTF-8 input represented as a list of bytes.
	Bytes : List(U8)

	## Parse a whole `Str` using a [Parser].
	##
	## Fails with `ParseError({ message, offset })`, where `offset` is the byte
	## offset of the furthest failure. Input left over after the parser
	## succeeds is a failure too (see [Parser.parse]).
	##
	## ```roc
	## color : Parser(Utf8.Bytes, [Red, Green, Blue])
	## color =
	##     Parser.one_of([
	##         Parser.const(Red).skip(Utf8.string("red")),
	##         Parser.const(Green).skip(Utf8.string("green")),
	##         Parser.const(Blue).skip(Utf8.string("blue")),
	##     ])
	##
	## expect Utf8.parse_str(color, "green") == Ok(Green)
	## ```
	parse_str : Parser(Bytes, a), Str -> Try(a, [ParseError({ message : Str, offset : U64 })])
	parse_str = |parser, input| {
		parser.parse(input.to_utf8())
	}

	## Runs a parser against the start of a string, allowing the parser to consume it only partially.
	##
	## - If the parser succeeds, returns the resulting `value` and the `rest` of the string.
	##   A `rest` that starts inside a multi-byte character is rendered with
	##   U+FFFD replacement characters rather than crashing.
	## - If the parser fails, returns `Err(ParseError({ message, offset }))`.
	##
	## ```roc
	## at_sign : Parser(Utf8.Bytes, [AtSign])
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str_partial(at_sign, "@").map_ok(|r| r.value) == Ok(AtSign)
	## expect Utf8.parse_str_partial(at_sign, "$").is_err()
	## ```
	parse_str_partial : Parser(Bytes, a), Str -> Try({ value : a, rest : Str }, [ParseError({ message : Str, offset : U64 })])
	parse_str_partial = |parser, input| {
		parser
			.run(input.to_utf8())
			.map_ok(|{ value, rest: remaining }| { value, rest: str_from_utf8(remaining) })
	}

	## Runs a parser against UTF-8 bytes, requiring the parser to consume them fully.
	##
	## Fails with `ParseError({ message, offset })` like [Utf8.parse_str].
	parse_bytes : Parser(Bytes, a), Bytes -> Try(a, [ParseError({ message : Str, offset : U64 })])
	parse_bytes = |parser, input| {
		parser.parse(input)
	}

	## Runs a parser against the start of UTF-8 bytes, allowing the parser to consume them only partially.
	##
	## Returns the parsed `value` and the `rest` of the bytes, or
	## `Err(ParseError({ message, offset }))`.
	parse_bytes_partial : Parser(Bytes, a), Bytes -> Try({ value : a, rest : Bytes }, [ParseError({ message : Str, offset : U64 })])
	parse_bytes_partial = |parser, input| {
		parser.run(input)
	}

	## Match one UTF-8 code unit when it satisfies the given predicate.
	##
	## Fails on empty input or when the predicate returns `False`.
	##
	## ```roc
	## is_digit : U8 -> Bool
	## is_digit = |b| b >= '0' and b <= '9'
	##
	## expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "0") == Ok('0')
	## expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "*").is_err()
	## ```
	codeunit_satisfies : (U8 -> Bool) -> Parser(Bytes, U8)
	codeunit_satisfies = |check| {
		# Messages are built once, here, not on every failure: failures are
		# frequent (every losing alternative) and building text allocates.
		empty_message = "expected a codeunit satisfying a condition, but input was empty."
		message = "expected a codeunit satisfying a condition"
		Parser.custom(
			|input| {
				{ before: start, others: input_rest } = input.split_at(1)

				match start.get(0) {
					Err(OutOfBounds) =>
						Err(ParseError({ message: empty_message, offset: 0 }))

					Ok(start_codeunit) => {
						if check(start_codeunit) {
							Ok({ value: start_codeunit, rest: input_rest })
						} else {
							Err(ParseError({ message, offset: 0 }))
						}
					}
				}
			},
		)
	}

	## Match one exact UTF-8 code unit.
	##
	## For a character outside ASCII, which is several code units, use `string`.
	##
	## ```roc
	## at_sign : Parser(Utf8.Bytes, [AtSign])
	## at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))
	##
	## expect Utf8.parse_str(at_sign, "@") == Ok(AtSign)
	## expect Utf8.parse_str_partial(at_sign, "$").is_err()
	## ```
	codeunit : U8 -> Parser(Bytes, U8)
	codeunit = |expected_code_unit| {
		message = "expected char `${str_from_codeunit(expected_code_unit)}`"
		empty_message = "${message} but input was empty."
		Parser.custom(
			|input| {
				match input {
					[] =>
						Err(ParseError({ message: empty_message, offset: 0 }))

					[first, .. as others] if first == expected_code_unit =>
						Ok({ value: expected_code_unit, rest: others })

					_ =>
						Err(ParseError({ message, offset: 0 }))
				}
			},
		)
	}

	## Match an exact sequence of UTF-8 bytes and return them.
	utf8 : List(U8) -> Parser(Bytes, List(U8))
	utf8 = |expected_string| {
		# Implemented manually instead of a sequence of codeunits
		# because of efficiency and better error messages
		message = "expected string `${str_from_utf8(expected_string)}`"
		Parser.custom(
			|input| {
				{ before: start, others: input_rest } = input.split_at(expected_string.len())

				if start == expected_string {
					Ok({ value: expected_string, rest: input_rest })
				} else {
					Err(ParseError({ message, offset: 0 }))
				}
			},
		)
	}

	## Match the given `Str` exactly (case-sensitive) and return it.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.string("Foo"), "Foo") == Ok("Foo")
	## expect Utf8.parse_str(Utf8.string("Foo"), "Bar").is_err()
	## ```
	string : Str -> Parser(Bytes, Str)
	string = |expected_string| {
		utf8(expected_string.to_utf8())
			.map(
				|_val| {
					expected_string
				},
			)
	}

	## Match any single `U8` code unit; fails only on empty input.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.any_codeunit, "a") == Ok('a')
	## expect Utf8.parse_str(Utf8.any_codeunit, "$") == Ok('$')
	## ```
	any_codeunit : Parser(Bytes, U8)
	any_codeunit = codeunit_satisfies(|_| True)

	## Consume the rest of the input and return it as bytes; never fails.
	##
	## ```roc
	## expect {
	##     bytes = "consumes all the input".to_utf8()
	##     Utf8.rest.parse(bytes) == Ok(bytes)
	## }
	## ```
	rest : Parser(Bytes, Bytes)
	rest = Parser.custom(
		|input| {
			Ok({ value: input, rest: [] })
		},
	)

	## Consume the rest of the input as a `Str`, failing if the bytes are not valid UTF-8.
	rest_str : Parser(Bytes, Str)
	rest_str = Parser.custom(
		|field_utf8ing| {
			match Str.from_utf8(field_utf8ing) {
				Ok(string_val) =>
					Ok({ value: string_val, rest: [] })

				Err(BadUtf8(_)) =>
					Err(ParseError({ message: "Expected a string field, but its contents cannot be parsed as UTF8.", offset: 0 }))
			}
		},
	)

	## Parse one ASCII decimal digit into a `U64` from 0 through 9.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.digit, "0") == Ok(0)
	## expect Utf8.parse_str(Utf8.digit, "not a digit").is_err()
	## ```
	digit : Parser(Bytes, U64)
	digit =
		Parser.custom(
			|input| {
				match input {
					[] =>
						Err(ParseError({ message: "Expected a digit from 0-9 but input was empty.", offset: 0 }))

					[first, .. as others] if first >= '0' and first <= '9' =>
						Ok({ value: (first - '0').to_u64(), rest: others })

					_ =>
						Err(ParseError({ message: "Not a digit", offset: 0 }))
				}
			},
		)

	## Parse one or more ASCII decimal digits into a `U64`, accepting leading zeroes.
	##
	## Fails when the input does not start with a digit or the value does not fit
	## in a `U64`. Signs and decimal points are not accepted.
	##
	## ```roc
	## expect Utf8.parse_str(Utf8.digits, "0123") == Ok(123)
	## expect Utf8.parse_str(Utf8.digits, "not a digit").is_err()
	## ```
	digits : Parser(Bytes, U64)
	digits = Parser.custom(
		|input| {
			var $i = 0
			var $sum = 0
			while $i < input.len() {
				d = input.get($i) ?? 0
				if d < '0' or d > '9' {
					break
				}
				value = (d - '0').to_u64()
				if $sum > (18446744073709551615 - value) / 10 {
					return Err(ParseError({ message: "Integer is too large for U64", offset: 0 }))
				}
				$sum = $sum * 10 + value
				$i = $i + 1
			}
			if $i == 0 {
				if input.is_empty() {
					Err(ParseError({ message: "Expected a digit from 0-9 but input was empty.", offset: 0 }))
				} else {
					Err(ParseError({ message: "Not a digit", offset: 0 }))
				}
			} else {
				Ok({ value: $sum, rest: input.drop_first($i) })
			}
		},
	)

	## A set of byte values, for finding or skipping runs of bytes 16 at a time.
	##
	## Build a class once, outside any loop, and reuse it: construction derives
	## the lookup tables the vector scan uses. Any set of bytes works. A set
	## whose high-nibble rows have at most eight distinct shapes (every ASCII
	## class, and most practical ones) is scanned with two table lookups per 16
	## bytes; other sets fall back to a byte loop.
	##
	## ```roc
	## spaces = Utf8.ByteClass.from_bytes([' ', '\t'])
	## expect spaces.contains('\t')
	## expect !spaces.contains('x')
	## ```
	ByteClass :: { lo : U8x16, hi : U8x16, a : U8x16, b : U8x16, c : U8x16, members : List(Bool), kind : [Exact, Nibbles, Bytewise] }.{

		## The class holding exactly these bytes.
		from_bytes : List(U8) -> ByteClass
		from_bytes = |bytes| {
			var $members = List.repeat(False, 256)
			for b in bytes {
				$members = $members.set(b.to_u64(), True) ?? $members
			}
			build_class($members)
		}

		## The class holding every byte `b` for which `check(b)` is true.
		from_predicate : (U8 -> Bool) -> ByteClass
		from_predicate = |check| {
			var $members = List.with_capacity(256)
			var $b = 0.U64
			while $b < 256 {
				$members = $members.append(check($b.to_u8_wrap()))
				$b = $b + 1
			}
			build_class($members)
		}

		## The class holding every byte that is not in this one.
		complement : ByteClass -> ByteClass
		complement = |class| build_class(class.members.map(|m| !m))

		## Whether the byte is in the class.
		contains : ByteClass, U8 -> Bool
		contains = |class, byte| class.members.get(byte.to_u64()) ?? False
	}

	## The index of the first byte at or after `pos` that is in `class`, or
	## the length of `bytes` if there is none. Scans 16 bytes at a time.
	##
	## ```roc
	## markup = Utf8.ByteClass.from_bytes(['<', '&'])
	## expect Utf8.find_any("a&b<c".to_utf8(), 0, markup) == 1
	## expect Utf8.find_any("abc".to_utf8(), 0, markup) == 3
	## ```
	find_any : Bytes, U64, ByteClass -> U64
	find_any = |bytes, pos, class| scan_class(bytes, pos, class, True)

	## The index of the first byte at or after `pos` that is not in `class`,
	## or the length of `bytes` if every remaining byte is. Use it to skip a
	## run of name characters, digits or whitespace.
	##
	## ```roc
	## digits = Utf8.ByteClass.from_predicate(|b| b >= '0' and b <= '9')
	## expect Utf8.skip_class("123abc".to_utf8(), 0, digits) == 3
	## ```
	skip_class : Bytes, U64, ByteClass -> U64
	skip_class = |bytes, pos, class| scan_class(bytes, pos, class, False)

	## The index of the next `\n` or `\r` at or after `pos`, or the length of
	## `bytes` if there is none. Scans 16 bytes at a time.
	##
	## ```roc
	## expect Utf8.find_line_end("ab\r\ncd".to_utf8(), 0) == 2
	## ```
	find_line_end : Bytes, U64 -> U64
	find_line_end = |bytes, pos| {
		cr = U8x16.splat('\r')
		lf = U8x16.splat('\n')
		len = bytes.len()
		var $i = pos
		var $scanning = True
		while $scanning {
			match U8x16.load(bytes, $i) {
				Ok(v) => {
					mask = U8x16.to_bitmask(U8x16.bitwise_or(U8x16.eq_lanes(v, cr), U8x16.eq_lanes(v, lf)))
					if mask != 0 {
						$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
						$scanning = False
					} else {
						$i = $i + 16
					}
				}
				Err(_) => {
					$scanning = False
					while $i < len {
						c = bytes.get($i) ?? 0
						if c == '\n' or c == '\r' {
							break
						}
						$i = $i + 1
					}
				}
			}
		}
		$i
	}

	## Consume the longest run of bytes in `class`, possibly empty, and return
	## it as a slice of the input. It scans 16 bytes at a time, so prefer it to
	## `Parser.chomp_while` for runs longer than a few bytes.
	##
	## ```roc
	## name_char = Utf8.ByteClass.from_predicate(|b| (b >= 'a' and b <= 'z') or b == '-')
	## expect Utf8.parse_str_partial(Utf8.span_class(name_char), "foo-bar=1").map_ok(|r| r.rest) == Ok("=1")
	## ```
	span_class : ByteClass -> Parser(Bytes, Bytes)
	span_class = |class| Parser.custom(
		|input| {
			n = scan_class(input, 0, class, False)
			Ok({ value: input.take_first(n), rest: input.drop_first(n) })
		},
	)
}

# Derive nibble lookup tables for a byte set. Byte `b` is in the set exactly
# when `lo[b & 15] & hi[b >> 4] != 0`. Each distinct non-empty high-nibble row
# (the set of low nibbles present under that high nibble) gets its own bit, so
# the tables are exact when there are at most eight distinct rows; otherwise
# the class scans byte by byte.
build_class : List(Bool) -> Utf8.ByteClass
build_class = |members| {
	var $shapes = []
	var $hi = []
	var $h = 0.U64
	while $h < 16 {
		var $row = 0.U16
		var $l = 0.U64
		while $l < 16 {
			if members.get($h * 16 + $l) ?? False {
				$row = $row.bitwise_or(1.U16.shl_wrap($l.to_u8_wrap()))
			}
			$l = $l + 1
		}
		row = $row
		if row == 0 {
			$hi = $hi.append(0.U8)
		} else {
			match $shapes.find_first_index(|s| s == row) {
				Ok(k) => {
					$hi = $hi.append(1.U8.shl_wrap(k.to_u8_wrap()))
				}
				Err(NotFound) => {
					k = $shapes.len()
					$shapes = $shapes.append(row)
					$hi = $hi.append(if k < 8 1.U8.shl_wrap(k.to_u8_wrap()) else 0)
				}
			}
		}
		$h = $h + 1
	}
	shapes = $shapes
	var $lo = []
	var $l2 = 0.U64
	while $l2 < 16 {
		bit = 1.U16.shl_wrap($l2.to_u8_wrap())
		var $entry = 0.U8
		var $k = 0.U64
		while $k < shapes.len() and $k < 8 {
			if (shapes.get($k) ?? 0).bitwise_and(bit) != 0 {
				$entry = $entry.bitwise_or(1.U8.shl_wrap($k.to_u8_wrap()))
			}
			$k = $k + 1
		}
		$lo = $lo.append($entry)
		$l2 = $l2 + 1
	}
	lo = U8x16.from_list($lo) ?? U8x16.splat(0)
	hi = U8x16.from_list($hi) ?? U8x16.splat(0)
	# Sets of one to three bytes compare against those bytes directly, which
	# is cheaper than two table lookups. Repeat the last byte to fill all three.
	var $exact = []
	var $m = 0.U64
	while $m < 256 {
		if members.get($m) ?? False {
			$exact = $exact.append($m.to_u8_wrap())
		}
		$m = $m + 1
	}
	exact = $exact
	last = exact.last() ?? 0
	kind = if exact.len() >= 1 and exact.len() <= 3 Exact else if shapes.len() <= 8 Nibbles else Bytewise
	{
		lo,
		hi,
		a: U8x16.splat(exact.get(0) ?? last),
		b: U8x16.splat(exact.get(1) ?? last),
		c: U8x16.splat(exact.get(2) ?? last),
		members,
		kind,
	}
}

# The index of the first byte at or after `pos` whose membership in `class`
# equals `want`, or the length of `bytes`.
scan_class : Utf8.Bytes, U64, Utf8.ByteClass, Bool -> U64
scan_class = |bytes, pos, class, want| {
	len = bytes.len()
	var $i = pos
	match class.kind {
		Exact => {
			a = class.a
			b = class.b
			c = class.c
			var $scanning = True
			while $scanning {
				match U8x16.load(bytes, $i) {
					Ok(v) => {
						inside = U8x16.to_bitmask(U8x16.bitwise_or(U8x16.bitwise_or(U8x16.eq_lanes(v, a), U8x16.eq_lanes(v, b)), U8x16.eq_lanes(v, c)))
						mask = if want inside else inside.bitwise_not()
						if mask != 0 {
							$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
							$scanning = False
						} else {
							$i = $i + 16
						}
					}
					Err(_) => {
						$scanning = False
					}
				}
			}
		}
		Nibbles => {
			lo_table = class.lo
			hi_table = class.hi
			nibble = U8x16.splat(15)
			zero = U8x16.splat(0)
			var $scanning = True
			while $scanning {
				match U8x16.load(bytes, $i) {
					Ok(v) => {
						lo = U8x16.table_lookup(lo_table, U8x16.bitwise_and(v, nibble))
						hi = U8x16.table_lookup(hi_table, U8x16.shr_zf_wrap(v, 4))
						outside = U8x16.to_bitmask(U8x16.eq_lanes(U8x16.bitwise_and(lo, hi), zero))
						mask = if want outside.bitwise_not() else outside
						if mask != 0 {
							$i = $i + U16.count_trailing_zero_bits(mask).to_u64()
							$scanning = False
						} else {
							$i = $i + 16
						}
					}
					Err(_) => {
						$scanning = False
					}
				}
			}
		}
		Bytewise => {}
	}
	while $i < len {
		if (class.members.get((bytes.get($i) ?? 0).to_u64()) ?? False) == want {
			break
		}
		$i = $i + 1
	}
	$i
}

str_from_codeunit : U8 -> Str
str_from_codeunit = |cu| {
	str_from_utf8([cu])
}

## Bytes of remaining input quoted in failure messages.
excerpt_len : U64
excerpt_len = 32

## Render the start of the remaining input for a failure message.
##
## Failures are cheap and frequent (every losing branch of `alt`/`one_of`, the
## last iteration of `many`), so quoting the whole remaining input made those
## combinators quadratic in the input length. Quote a bounded prefix instead.
excerpt : Utf8.Bytes -> Str
excerpt = |bytes| {
	if bytes.len() <= excerpt_len {
		str_from_utf8(bytes)
	} else {
		Str.concat(str_from_utf8(bytes.sublist({ start: 0, len: excerpt_len })), "…")
	}
}

# Failure messages quote only a bounded prefix of the remaining input.
expect {
	long = List.repeat('b', 1000)
	match Parser.run(Utf8.codeunit('a'), long) {
		Err(ParseError({ message, offset: _ })) => message.count_utf8_bytes() < 200
		Ok(_) => False
	}
}

str_from_utf8 : Utf8.Bytes -> Str
str_from_utf8 = |bytes| {
	# Byte-oriented parsers can stop within a multibyte scalar, so diagnostics
	# and Str leftovers render bytes lossily rather than crashing.
	Str.from_utf8_lossy(bytes)
}

# Any codeunit parser accepts a lowercase ASCII byte.
expect {
	actual = Utf8.parse_str(Utf8.any_codeunit, "a")?
	actual == 'a'
}

# Any codeunit parser accepts a dollar-sign byte.
expect {
	actual = Utf8.parse_str(Utf8.any_codeunit, "\$")?
	actual == 36
}

# Any input parser consumes all bytes and returns them.
expect {
	bytes = "consumes all the input".to_utf8()
	actual = Parser.parse(Utf8.rest, bytes)?
	actual == bytes
}

# -------------------- example snippets used in docs --------------------

parse_u32 : Parser(Utf8.Bytes, U32)
parse_u32 =
	Parser.const(U64.to_u32_wrap).keep(Utf8.digits)

# Digit parsing can be mapped into a U32.
expect {
	actual = Utf8.parse_str(parse_u32, "123")?
	actual == 123.U32
}

color : Parser(Utf8.Bytes, [Red, Green, Blue])
color =
	Parser.one_of([
		Parser.const(Red).skip(Utf8.string("red")),
		Parser.const(Green).skip(Utf8.string("green")),
		Parser.const(Blue).skip(Utf8.string("blue")),
	])

# One-of parsing selects the matching color tag.
expect {
	actual = Utf8.parse_str(color, "green")?
	actual == Green
}

parse_numbers : Parser(Utf8.Bytes, List(U64))
parse_numbers = (Utf8.digits).sep_by(Utf8.codeunit(','))

# Separator parsing returns the list of parsed numbers.
expect {
	actual = Utf8.parse_str(parse_numbers, "1,2,3")?
	actual == [1, 2, 3]
}

# Exact string parsing succeeds when the input matches.
expect {
	actual = Utf8.parse_str(Utf8.string("Foo"), "Foo")?
	actual == "Foo"
}

# Exact string parsing reports non-matching input.
expect Utf8.parse_str(Utf8.string("Foo"), "Bar").is_err()

ignore_text : Parser(Utf8.Bytes, U64)
ignore_text =
	Parser.const(
		|d| {
			d
		},
	)
		.skip(Parser.chomp_until(':'))
		.skip(Utf8.codeunit(':'))
		.keep(Utf8.digits)

# Skipping a prefix can parse the numeric suffix.
expect {
	actual = Utf8.parse_str(ignore_text, "ignore preceding text:123")?
	actual == 123
}

ignore_numbers : Parser(Utf8.Bytes, Str)
ignore_numbers =
	Parser.const(
		|str| {
			str
		},
	)
		.skip(
			Parser.chomp_while(
				|b| {
					b >= '0' and b <= '9'
				},
			),
		)
		.keep(Utf8.string("TEXT"))

# Chomping digits can leave the following text parser result.
expect {
	actual = Utf8.parse_str(ignore_numbers, "0123456789876543210TEXT")?
	actual == "TEXT"
}

is_digit : U8 -> Bool
is_digit = |b| {
	b >= '0' and b <= '9'
}

# Codeunit predicates can accept digit bytes.
expect {
	actual = Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "0")?
	actual == '0'
}

# Codeunit predicates reject bytes that do not satisfy the predicate.
expect Utf8.parse_str(Utf8.codeunit_satisfies(is_digit), "*").is_err()

at_sign : Parser(Utf8.Bytes, [AtSign])
at_sign = Parser.const(AtSign).skip(Utf8.codeunit('@'))

# The at-sign parser succeeds on an at-sign byte.
expect {
	actual = Utf8.parse_str(at_sign, "@")?
	actual == AtSign
}

# Partial parsing returns the parsed at-sign tag.
expect {
	actual = Utf8.parse_str_partial(at_sign, "@")?
	actual.value == AtSign
}

# The at-sign parser rejects other bytes.
expect Utf8.parse_str_partial(at_sign, "\$").is_err()

# Partial string parsing renders a leftover that begins within a UTF-8 scalar.
expect {
	actual = Utf8.parse_str_partial(Utf8.any_codeunit, "ӿ")?
	actual.rest == "�"
}

# Complete string parsing reports a leftover as unexpected input at its byte offset.
expect Utf8.parse_str(Utf8.any_codeunit, "ӿ") == Err(ParseError({ message: "unexpected input", offset: 1 }))

Requirement : [Green(U64), Red(U64), Blue(U64)]

RequirementSet : List(Requirement)

Game : { id : U64, requirements : List(RequirementSet) }

parse_game : Str -> Try(Game, [ParsingError])
parse_game = |s| {
	green = Parser.const(|x| Green(x)).keep(Utf8.digits).skip(Utf8.string(" green"))
	red = Parser.const(|x| Red(x)).keep(Utf8.digits).skip(Utf8.string(" red"))
	blue = Parser.const(|x| Blue(x)).keep(Utf8.digits).skip(Utf8.string(" blue"))

	requirement_set : Parser(_, RequirementSet)
	requirement_set = Parser.one_of([green, red, blue]).sep_by(Utf8.string(", "))

	requirements : Parser(_, List(RequirementSet))
	requirements = requirement_set.sep_by(Utf8.string("; "))

	game : Parser(_, Game)
	game =
		Parser.const(
			|id| {
				|r| {
					{ id, requirements: r }
				}
			},
		)
			.skip(Utf8.string("Game "))
			.keep(Utf8.digits)
			.skip(Utf8.string(": "))
			.keep(requirements)

	match Utf8.parse_str(game, s) {
		Ok(g) => Ok(g)
		Err(ParseError(_)) => Err(ParsingError)
	}
}

# Game parsing extracts requirements grouped by reveal.
expect {
	actual = parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")?
	actual
		== {
			id: 1,
			requirements: [
				[Blue(3), Red(4)],
				[Red(1), Green(2), Blue(6)],
				[Green(2)],
			],
		}
}

# Single digit parsing converts ASCII zero into numeric zero.
expect {
	actual = Utf8.parse_str(Utf8.digit, "0")?
	actual == 0
}

# Single digit parsing rejects non-digit text.
expect Utf8.parse_str(Utf8.digit, "not a digit").is_err()

# Multiple digit parsing accepts leading zeroes.
expect {
	actual = Utf8.parse_str(Utf8.digits, "0123")?
	actual == 123
}

# Multiple digit parsing accepts the largest U64.
expect {
	actual = Utf8.parse_str(Utf8.digits, "18446744073709551615")?
	actual == 18446744073709551615
}

# Multiple digit parsing rejects values larger than U64 without crashing.
expect Utf8.parse_str(Utf8.digits, "18446744073709551616").is_err()

# Multiple digit parsing rejects text without a leading digit.
expect Utf8.parse_str(Utf8.digits, "not a digit").is_err()

bool_parser : Parser(Utf8.Bytes, Bool)
bool_parser =
	Parser.one_of([Utf8.string("true"), Utf8.string("false")])
		.map(
			|x| {
				x == "true"
			},
		)

# Boolean parser maps true text to True.
expect {
	actual = Utf8.parse_str(bool_parser, "true")?
	actual == True
}

# Boolean parser maps false text to False.
expect {
	actual = Utf8.parse_str(bool_parser, "false")?
	actual == False
}

# Boolean parser rejects other text.
expect Utf8.parse_str(bool_parser, "not a bool").is_err()

even : Parser(Utf8.Bytes, U64)
even =
	Utf8.digits
		.map(
			|n| {
				if n % 2 == 0 {
					Ok(n)
				} else {
					Err("odd number")
				}
			},
		)
		.flatten()

# Flattening keeps an Ok value from the mapped function.
expect Utf8.parse_str(even, "42") == Ok(42)

# Flattening turns an Err value into a parse failure with its message.
expect Utf8.parse_str(even, "7") == Err(ParseError({ message: "odd number", offset: 0 }))

# Capturing up to a delimiter returns the bytes before it.
expect {
	capture_text = Parser.const(|codeunits| codeunits).keep(Parser.chomp_until(':')).skip(Utf8.codeunit(':'))
	Utf8.parse_str(capture_text, "Roc:") == Ok(['R', 'o', 'c'])
}

# Capturing while a predicate holds returns the matching bytes.
expect {
	capture_numbers =
		Parser.const(|codeunits| codeunits)
			.keep(Parser.chomp_while(|b| b >= '0' and b <= '9'))
			.skip(Utf8.string("TEXT"))
	Utf8.parse_str(capture_numbers, "123TEXT") == Ok(['1', '2', '3'])
}

# Mapping three parsers matches applying a curried constructor three times.
expect {
	space = Utf8.codeunit(' ')
	mapped = Parser.map3(Utf8.digits.skip(space), Utf8.digits.skip(space), Utf8.digits, |x, y, z| Triple(x, y, z))
	applied =
		Parser.const(|x| |y| |z| Triple(x, y, z))
			.keep(Utf8.digits.skip(space))
			.keep(Utf8.digits.skip(space))
			.keep(Utf8.digits)
	Utf8.parse_str(mapped, "1 2 3") == Ok(Triple(1, 2, 3)) and Utf8.parse_str(applied, "1 2 3") == Ok(Triple(1, 2, 3))
}

# A length-prefixed field reads as many bytes as its digit says.
expect {
	sized : Parser(Utf8.Bytes, List(U8))
	sized = Utf8.digit.and_then(|n| Utf8.any_codeunit.many().map(|bytes| bytes.take_first(n)))
	Utf8.parse_str(sized, "2abc") == Ok(['a', 'b'])
}

# Between keeps only the delimited value.
expect {
	between_brackets = |parser| parser.between(Utf8.codeunit('['), Utf8.codeunit(']'))
	Utf8.parse_str(between_brackets(Utf8.digits), "[42]") == Ok(42)
}

# The rest of the input is read as a Str, and invalid UTF-8 fails without crashing.
expect Utf8.parse_str(Utf8.rest_str, "héllo") == Ok("héllo")
expect Utf8.parse_bytes(Utf8.rest_str, [0xFF]).is_err()

# A bad element in the middle of a repetition reports its own failure and offset.
expect {
	numbers = Utf8.digits.sep_by(Utf8.codeunit(','))
	match Utf8.parse_str(numbers, "1,2,x,4") {
		Err(ParseError({ offset, message: _ })) => offset == 4
		Ok(_) => False
	}
}

# Leftover input after a number is reported where the number stopped.
expect Utf8.parse_str(Utf8.digits, "12 ") == Err(ParseError({ message: "unexpected input", offset: 2 }))

# ParseError composes with `?` in a function that returns other errors too.
expect {
	both : Str -> Try(U64, [ParseError({ message : Str, offset : U64 }), TooBig])
	both = |text| {
		n = Utf8.parse_str(Utf8.digits, text)?
		if n > 100 Err(TooBig) else Ok(n)
	}
	both("7") == Ok(7) and both("700") == Err(TooBig) and both("x").is_err()
}

# Reference scan for the vector primitives: the first index at or after `pos`
# whose membership equals `want`.
scan_reference : List(U8), U64, (U8 -> Bool), Bool -> U64
scan_reference = |bytes, pos, member, want| {
	var $i = pos
	while $i < bytes.len() {
		if member(bytes.get($i) ?? 0) == want {
			break
		}
		$i = $i + 1
	}
	$i
}

every_byte : List(U8)
every_byte = {
	var $all = []
	var $b = 0.U64
	while $b < 256 {
		$all = $all.append($b.to_u8_wrap())
		$b = $b + 1
	}
	$all
}

# Every byte value, at several lane positions of a 16-byte block and in the
# tail, is classified like the scalar reference, for ASCII, high-byte and
# more-than-eight-row classes.
expect {
	classes = [
		|b| b == '<' or b == '&',
		|b| (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') or (b >= '0' and b <= '9') or b == '-' or b == '_' or b == '.' or b == ':' or b >= 0x80,
		|b| b == ' ' or b == '\t' or b == '\n' or b == '\r',
		|b| b % 3 == 0,
		|b| b % 16 <= b / 16,
		|b| b >= 0xC0,
	]
	classes.all(
		|member| {
			class = Utf8.ByteClass.from_predicate(member)
			every_byte.all(
				|byte| {
					other = if member('a') 0x00 else 'a'
					filler = if member(other) 0x01 else other
					run = List.repeat(byte, 37)
					[0, 5, 15, 16, 31, 40].all(
						|at| {
							input = List.repeat(filler, 41).set(at, byte) ?? []
							Utf8.find_any(input, 0, class) == scan_reference(input, 0, member, True)
								and Utf8.find_any(input, 3, class) == scan_reference(input, 3, member, True)
						},
					)
						and Utf8.skip_class(run, 0, class) == scan_reference(run, 0, member, False)
				},
			)
		},
	)
}

# Sets of up to eight distinct row shapes scan with vectors; others with bytes.
expect Utf8.ByteClass.from_predicate(|b| b % 3 == 0).kind == Nibbles
expect Utf8.ByteClass.from_predicate(|b| b % 16 <= b / 16).kind == Bytewise
expect Utf8.ByteClass.from_bytes(['<', '&']).kind == Exact

expect Utf8.find_line_end(List.repeat('a', 40).append('\n'), 0) == 40
expect Utf8.find_line_end(List.repeat('a', 40).append('\r'), 7) == 40
expect Utf8.find_line_end("abc".to_utf8(), 0) == 3
expect Utf8.find_line_end("abc".to_utf8(), 5) == 5
expect Utf8.find_any([], 0, Utf8.ByteClass.from_bytes(['x'])) == 0
expect Utf8.ByteClass.from_bytes(['a']).complement().contains('b')
expect Utf8.parse_str_partial(Utf8.span_class(Utf8.ByteClass.from_bytes(['a'])), "aaab").map_ok(|r| r.rest) == Ok("b")
