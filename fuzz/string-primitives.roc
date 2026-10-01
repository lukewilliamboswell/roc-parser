app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Parser
import parser.Utf8

## Utf8 primitive properties over arbitrary (often invalid) UTF-8:
## - digit/digits agree with a naive U128 decimal oracle and with the builtin
##   U64.from_str, including the U64 overflow boundary;
## - every exported Utf8 parser either fails with a renderable message or
##   returns a value plus a leftover that is a suffix of the input, with
##   `consumed ++ leftover == input`;
## - the Str front ends (parse_str, parse_str_partial) never crash, even when
##   a byte parser stops inside a multi-byte scalar;
## - repetition over a long input stays linear (bounded by --timeout).

Case : { input : List(U8), literal : List(U8), unit : U8, scale : U64 }

generate : List(U8) -> Case
generate = |bytes| {
	unit = bytes.get(0) ?? 0
	lit_len = U8.to_u64(bytes.get(1) ?? 0) % 5
	literal = bytes.sublist({ start: 2, len: lit_len })
	body = bytes.drop_first(2 + lit_len)
	# Mostly digits so the numeric paths reach the U64 boundary.
	input = if unit >= 128 body.map(|b| if b < 200 '0' + b % 10 else b) else body
	{ input, literal, unit, scale: U8.to_u64(bytes.get(1) ?? 0) / 5 }
}

check_suffix : Str, List(U8), List(U8), List(U8) -> {}
check_suffix = |name, input, consumed, rest| {
	if consumed.concat(rest) != input {
		crash "${name}: consumed ++ leftover != input"
	} else {
		{}
	}
}

## Run a parser and check the generic consumption invariant: the leftover is
## a suffix of the input. Returns the consumed prefix length on success.
run : Str, Parser(List(U8), a), List(U8) -> Try(U64, [Failed])
run = |name, parser, input| {
	match Parser.run(parser, input) {
		Ok({ value: _, rest: rest }) => {
			if rest.len() > input.len() or input.drop_first(input.len() - rest.len()) != rest {
				crash "${name}: leftover is not a suffix of the input"
			}
			Ok(input.len() - rest.len())
		}
		Err(ParseError({ message, offset })) => {
			_ = message.count_utf8_bytes()
			if offset > input.len() crash "${name}: failure offset is beyond the input"
			Err(Failed)
		}
	}
}

leading_digits : List(U8) -> U64
leading_digits = |input| input.fold_until(0, |n, b| if b >= '0' and b <= '9' Continue(n + 1) else Break(n))

check_numbers : List(U8) -> {}
check_numbers = |input| {
	count = leading_digits(input)
	prefix = input.sublist({ start: 0, len: count })
	oracle = prefix.fold(0.U128, |acc, b| if acc > 18446744073709551615 acc else acc * 10 + U8.to_u128(b - '0'))
	builtin = U64.from_str(Str.from_utf8_lossy(prefix))
	actual = Parser.run(Utf8.digits, input)
	match actual {
		Ok({ value: val, rest: rest }) => {
			if count == 0 or oracle > 18446744073709551615 or U64.to_u128(val) != oracle or rest != input.drop_first(count) {
				crash "digits accepted ${Str.inspect(prefix)} as ${val.to_str()}"
			}
			if builtin != Ok(val) {
				crash "digits disagrees with U64.from_str on ${Str.inspect(prefix)}"
			}
		}
		Err(_) =>
			if count > 0 and oracle <= 18446744073709551615 {
				crash "digits rejected ${Str.inspect(prefix)}"
			} else if count > 0 and builtin.is_ok() {
				crash "digits rejected a value U64.from_str accepts: ${Str.inspect(prefix)}"
			}
	}
	match Parser.run(Utf8.digit, input) {
		Ok({ value: val, rest: rest }) =>
			if count == 0 or U64.to_u128(val) != U8.to_u128((input.get(0) ?? 0) - '0') or rest != input.drop_first(1) {
				crash "digit wrong"
			}
		Err(_) => if count > 0 crash "digit rejected a digit"
	}
	{}
}

check_codeunits : Case -> {}
check_codeunits = |case| {
	input = case.input
	first = input.get(0)
	match Parser.run(Utf8.any_codeunit, input) {
		Ok({ value: val, rest: rest }) => {
			if first != Ok(val) crash "any_codeunit value"
			check_suffix("any_codeunit", input, [val], rest)
		}
		Err(_) => if first.is_ok() crash "any_codeunit failed on non-empty input"
	}
	match Parser.run(Utf8.codeunit(case.unit), input) {
		Ok({ value: val, rest: rest }) => {
			if first != Ok(case.unit) or val != case.unit crash "codeunit accepted wrong byte"
			check_suffix("codeunit", input, [val], rest)
		}
		Err(_) => if first == Ok(case.unit) crash "codeunit rejected matching byte"
	}
	match Parser.run(Utf8.codeunit_satisfies(|b| b < case.unit), input) {
		Ok({ value: val, rest: rest }) => {
			if first != Ok(val) or val >= case.unit crash "codeunit_satisfies accepted wrong byte"
			check_suffix("codeunit_satisfies", input, [val], rest)
		}
		Err(_) =>
			match first {
				Ok(b) if b < case.unit => crash "codeunit_satisfies rejected matching byte"
				_ => {}
			}
	}
	match Parser.run(Utf8.utf8(case.literal), input) {
		Ok({ value: val, rest: rest }) => {
			if val != case.literal or !input.starts_with(case.literal) crash "utf8 accepted non-prefix"
			check_suffix("utf8", input, val, rest)
		}
		Err(_) => if input.starts_with(case.literal) crash "utf8 rejected a prefix"
	}
	match Str.from_utf8(case.literal) {
		Ok(text) =>
			match Parser.run(Utf8.string(text), input) {
				Ok({ value: val, rest: rest }) => {
					if val != text or !input.starts_with(case.literal) crash "string accepted non-prefix"
					check_suffix("string", input, case.literal, rest)
				}
				Err(_) => if input.starts_with(case.literal) crash "string rejected a prefix"
			}
		Err(_) => {}
	}
	match Parser.run(Utf8.any_thing, input) {
		Ok({ value: val, rest: rest }) => check_suffix("any_thing", input, val, rest)
		Err(_) => crash "any_thing failed"
	}
	match Parser.run(Utf8.any_string, input) {
		Ok({ value: val, rest: rest }) => {
			if Str.from_utf8(input) != Ok(val) crash "any_string value"
			check_suffix("any_string", input, val.to_utf8(), rest)
		}
		Err(_) => if Str.from_utf8(input).is_ok() crash "any_string rejected valid UTF-8"
	}
	match Parser.run(Parser.chomp_until(case.unit), input) {
		Ok({ value: val, rest: rest }) => {
			if val.contains(case.unit) or rest.get(0) != Ok(case.unit) crash "chomp_until stopped at wrong byte"
			check_suffix("chomp_until", input, val, rest)
		}
		Err(_) => if input.contains(case.unit) crash "chomp_until missed the byte"
	}
	match Parser.run(Parser.chomp_while(|b| b < case.unit), input) {
		Ok({ value: val, rest: rest }) => {
			if val.any(|b| b >= case.unit) crash "chomp_while consumed a failing byte"
			match rest.get(0) {
				Ok(b) if b < case.unit => crash "chomp_while stopped early"
				_ => {}
			}
			check_suffix("chomp_while", input, val, rest)
		}
		Err(_) => crash "chomp_while failed"
	}
	_ = run("one_of", Utf8.one_of([Utf8.utf8(case.literal), Utf8.digit.map(|_| [])]), input)
	_ = run("one_of empty", Utf8.one_of([]), input)
	{}
}

## The Str front ends must never crash, whatever byte parser is used.
check_str_front_ends : Case -> {}
check_str_front_ends = |case| {
	text = Str.from_utf8_lossy(case.input)
	bytes = text.to_utf8()
	parsers = [
		Utf8.any_codeunit.map(|b| [b]),
		Utf8.codeunit(case.unit).map(|b| [b]),
		Utf8.utf8(case.literal),
		Parser.many(Utf8.any_codeunit),
		Parser.chomp_until(case.unit),
	]
	parsers.fold(
		{},
		|_, parser| {
			match Utf8.parse_str_partial(parser, text) {
				Ok({ value: val, rest: rest }) =>
					if !Str.from_utf8_lossy(val).is_empty() and rest.count_utf8_bytes() > bytes.len() * 3 {
						crash "parse_str_partial leftover grew"
					}
				Err(ParseError({ message, offset: _ })) => {
					_ = message.count_utf8_bytes()
				}
			}
			match Utf8.parse_str(parser, text) {
				Ok(_) => {}
				Err(ParseError({ message, offset })) => {
					_ = message.count_utf8_bytes()
					if offset > bytes.len() crash "parse_str failure offset is beyond the input"
				}
			}
			{}
		},
	)
	{}
}

## Repetition over a long input built by repeating the case input must stay
## linear; a quadratic step shows up as a --timeout.
check_scaling : Case -> {}
check_scaling = |case| {
	# Expensive, so only for a slice of the inputs.
	if case.input.is_empty() or case.unit % 64 != 3 or case.scale % 4 != 0 {
		{}
	} else {
		var $big = case.input
		while $big.len() < 20000 + case.scale * 1000 {
			$big = $big.concat($big)
		}
		element = Parser.alt(Utf8.codeunit(case.unit).map(|b| [b]), Utf8.one_of([Utf8.utf8(case.literal), Utf8.any_codeunit.map(|_| [])]))
		match Parser.run(Parser.many(element.map(|_| {})), $big) {
			Ok({ value: val, rest: rest }) => if val.len() == 0 and rest.len() != $big.len() crash "many consumed without values"
			Err(_) => crash "many failed"
		}
		_ = run("sep_by", Parser.sep_by(Utf8.digits, Utf8.codeunit(case.unit)), $big)
		_ = run("chomp_while", Parser.many(Parser.chomp_while(|b| b < case.unit)), $big)
		{}
	}
}

test : Case -> Fuzz.Outcome
test = |case| {
	check_numbers(case.input)
	check_codeunits(case)
	check_str_front_ends(case)
	check_scaling(case)
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "string-primitives",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |case| "unit=${case.unit.to_str()} literal=${Str.inspect(case.literal)} input=${Str.inspect(case.input)}",
})
