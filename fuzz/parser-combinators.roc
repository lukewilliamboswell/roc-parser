app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Parser
import parser.Utf8

## Combinator model property: bytes decode into a random combinator expression
## tree plus an input. The tree is turned into a real `Parser` built from the
## library's combinators and, independently, evaluated by `model`, a naive
## deterministic-PEG interpreter written straight from the documented
## semantics. Both must agree on success/failure, the produced value and the
## leftover input. Every parser produces a `List(U8)` so values compare
## directly; composite parsers encode structure with marker bytes.

Expr := [
	Lit(List(U8)),
	Text(Str),
	Cu(U8),
	Sat(U8),
	AnyCu,
	AnyThing,
	AnyString,
	Digit,
	Digits,
	Const(List(U8)),
	Fail,
	ChompWhile(U8),
	ChompUntil(U8),
	Seq(Expr, Expr),
	Apply(Expr, Expr),
	Skip(Expr, Expr),
	Map3(Expr, Expr, Expr),
	Alt(Expr, Expr),
	OneOf(List(Expr)),
	StrOneOf(List(Expr)),
	Many(Expr),
	OneOrMore(Expr),
	SepBy(Expr, Expr),
	SepBy1(Expr, Expr),
	Between(Expr, Expr, Expr),
	Maybe(Expr),
	Rev(Expr),
	EvenOnly(Expr),
	Lazy(Expr),
	Ignore(Expr),
]

Case : { expr : Expr, input : List(U8), text : Str }

Outcome : Try({ val : List(U8), rest : List(U8) }, [Fail])

## Bytes that make interesting literals and inputs: digits, separators,
## letters, and the lead/continuation bytes of a multi-byte scalar.
alphabet : List(U8)
alphabet = ['a', 'b', '0', '1', '9', ',', ';', '(', ')', ' ', 0xC3, 0xA9, 'x', '\n', 0xE2, 0x82]

pick : U8 -> U8
pick = |b| alphabet.get(U8.to_u64(b) % alphabet.len()) ?? 'a'

## A decoder over the fuzz bytes: returns the decoded value and new position.
byte : List(U8), U64 -> U8
byte = |bytes, pos| bytes.get(pos) ?? 0

short_bytes : List(U8), U64 -> (List(U8), U64)
short_bytes = |bytes, pos| {
	len = U8.to_u64(byte(bytes, pos)) % 4
	var $out = []
	var $i = 0
	while $i < len {
		$out = $out.append(pick(byte(bytes, pos + 1 + $i)))
		$i = $i + 1
	}
	($out, pos + 1 + len)
}

decode : List(U8), U64, U64 -> (Expr, U64)
decode = |bytes, pos, depth| {
	tag = byte(bytes, pos)
	next = pos + 1
	kind = if depth == 0 U8.to_u64(tag) % 13 else U8.to_u64(tag) % 30
	match kind {
		0 => {
			(lit, p) = short_bytes(bytes, next)
			(Lit(lit), p)
		}
		1 => {
			(lit, p) = short_bytes(bytes, next)
			text = Str.from_utf8_lossy(lit.keep_if(|b| b < 0x80))
			(Text(text), p)
		}
		2 => (Cu(pick(byte(bytes, next))), next + 1)
		3 => (Sat(byte(bytes, next)), next + 1)
		4 => (AnyCu, next)
		5 => (AnyThing, next)
		6 => (AnyString, next)
		7 => (Digit, next)
		8 => (Digits, next)
		9 => {
			(lit, p) = short_bytes(bytes, next)
			(Const(lit), p)
		}
		10 => (Fail, next)
		11 => (ChompWhile(byte(bytes, next)), next + 1)
		12 => (ChompUntil(pick(byte(bytes, next))), next + 1)
		13 => decode2(bytes, next, depth, |a, b| Seq(a, b))
		14 => decode2(bytes, next, depth, |a, b| Apply(a, b))
		15 => decode2(bytes, next, depth, |a, b| Skip(a, b))
		16 => {
			(a, p1) = decode(bytes, next, depth - 1)
			(b, p2) = decode(bytes, p1, depth - 1)
			(c, p3) = decode(bytes, p2, depth - 1)
			(Map3(a, b, c), p3)
		}
		17 => decode2(bytes, next, depth, |a, b| Alt(a, b))
		18 => {
			(items, p) = decode_list(bytes, next, depth)
			(OneOf(items), p)
		}
		19 => {
			(items, p) = decode_list(bytes, next, depth)
			(StrOneOf(items), p)
		}
		20 => decode1(bytes, next, depth, |a| Many(a))
		21 => decode1(bytes, next, depth, |a| OneOrMore(a))
		22 => decode2(bytes, next, depth, |a, b| SepBy(a, b))
		23 => decode2(bytes, next, depth, |a, b| SepBy1(a, b))
		24 => {
			(a, p1) = decode(bytes, next, depth - 1)
			(b, p2) = decode(bytes, p1, depth - 1)
			(c, p3) = decode(bytes, p2, depth - 1)
			(Between(a, b, c), p3)
		}
		25 => decode1(bytes, next, depth, |a| Maybe(a))
		26 => decode1(bytes, next, depth, |a| Rev(a))
		27 => decode1(bytes, next, depth, |a| EvenOnly(a))
		28 => decode1(bytes, next, depth, |a| Lazy(a))
		_ => decode1(bytes, next, depth, |a| Ignore(a))
	}
}

decode1 : List(U8), U64, U64, (Expr -> Expr) -> (Expr, U64)
decode1 = |bytes, pos, depth, wrap| {
	(a, p) = decode(bytes, pos, depth - 1)
	(wrap(a), p)
}

decode2 : List(U8), U64, U64, (Expr, Expr -> Expr) -> (Expr, U64)
decode2 = |bytes, pos, depth, wrap| {
	(a, p1) = decode(bytes, pos, depth - 1)
	(b, p2) = decode(bytes, p1, depth - 1)
	(wrap(a, b), p2)
}

decode_list : List(U8), U64, U64 -> (List(Expr), U64)
decode_list = |bytes, pos, depth| {
	count = U8.to_u64(byte(bytes, pos)) % 4
	var $items = []
	var $p = pos + 1
	var $i = 0
	while $i < count {
		(item, p) = decode(bytes, $p, depth - 1)
		$items = $items.append(item)
		$p = p
		$i = $i + 1
	}
	($items, $p)
}

generate : List(U8) -> Case
generate = |bytes| {
	depth = U8.to_u64(byte(bytes, 0)) % 5
	(expr, pos) = decode(bytes, 1, depth)
	raw = bytes.drop_first(pos)
	# Half the time map input bytes through the alphabet so literals match.
	input = if byte(bytes, 0) >= 128 raw.map(pick) else raw
	{ expr, input, text: Str.from_utf8_lossy(input) }
}

# ------------------------------------------------------------------ library

even_only : List(U8) -> Try(List(U8), Str)
even_only = |v| if v.len() % 2 == 0 Ok(v) else Err("odd length")

items : List(List(U8)), U8 -> List(U8)
items = |vals, marker| vals.fold([], |acc, v| acc.concat(v).append(marker))

build : Expr -> Parser(List(U8), List(U8))
build = |expr| {
	match expr {
		Lit(lit) => Utf8.utf8(lit)
		Text(text) => Utf8.string(text).map(|s| s.to_utf8())
		Cu(c) => Utf8.codeunit(c).map(|b| [b])
		Sat(t) => Utf8.codeunit_satisfies(|b| b < t).map(|b| [b])
		AnyCu => Utf8.any_codeunit.map(|b| [b])
		AnyThing => Utf8.any_thing
		AnyString => Utf8.any_string.map(|s| s.to_utf8())
		Digit => Utf8.digit.map(|n| n.to_str().to_utf8())
		Digits => Utf8.digits.map(|n| n.to_str().to_utf8())
		Const(lit) => Parser.const(lit)
		Fail => Parser.fail("model fail")
		ChompWhile(t) => Parser.chomp_while(|b| b < t)
		ChompUntil(c) => Parser.chomp_until(c)
		Seq(a, b) => Parser.map2(build(a), build(b), |x, y| x.concat(y))
		Apply(a, b) => Parser.const(|x| |y| x.concat(y).append('+')).apply(build(a)).apply(build(b))
		Skip(a, b) => build(a).skip(build(b))
		Map3(a, b, c) => Parser.map3(build(a), build(b), build(c), |x, y, z| x.append('|').concat(y).append('|').concat(z))
		Alt(a, b) => Parser.alt(build(a), build(b))
		OneOf(es) => Parser.one_of(es.map(build))
		StrOneOf(es) => Utf8.one_of(es.map(build))
		Many(a) => Parser.many(build(a)).map(|vs| items(vs, ';'))
		OneOrMore(a) => Parser.one_or_more(build(a)).map(|vs| items(vs, ';'))
		SepBy(a, s) => Parser.sep_by(build(a), build(s)).map(|vs| items(vs, ','))
		SepBy1(a, s) => Parser.sep_by1(build(a), build(s)).map(|vs| items(vs, ','))
		Between(a, o, c) => Parser.between(build(a), build(o), build(c))
		Maybe(a) =>
			Parser.maybe(build(a)).map(
				|r| {
					match r {
						Ok(v) => v.prepend('J')
						Err(Nothing) => ['N']
					}
				},
			)
		Rev(a) => build(a).map(|v| v.drop_first(1))
		EvenOnly(a) => build(a).map(even_only).flatten()
		Lazy(a) => Parser.lazy(|_| build(a))
		Ignore(a) => Parser.ignore(build(a)).map(|_| [])
	}
}

# -------------------------------------------------------------------- model

ok : List(U8), List(U8) -> Outcome
ok = |val, rest| Ok({ val, rest })

## Repeat `step` until it fails; documented `many` semantics.
repeat : (List(U8) -> Outcome), List(U8) -> { vals : List(List(U8)), rest : List(U8) }
repeat = |step, input| {
	var $vals = []
	var $rest = input
	var $go = Bool.True
	while $go {
		match step($rest) {
			Ok(r) => {
				if r.rest.len() == $rest.len() {
					# Non-consuming iteration: stop, discarding its value.
					$go = Bool.False
				} else {
					$vals = $vals.append(r.val)
					$rest = r.rest
				}
			}
			Err(_) => {
				$go = Bool.False
			}
		}
	}
	{ vals: $vals, rest: $rest }
}

then : Outcome, (List(U8), List(U8) -> Outcome) -> Outcome
then = |first, k| {
	match first {
		Ok(r) => k(r.val, r.rest)
		Err(e) => Err(e)
	}
}

first_success : List(Expr), List(U8) -> Outcome
first_success = |es, input| {
	es.fold(
		Err(Fail),
		|acc, e| {
			match acc {
				Ok(_) => acc
				Err(_) => model(e, input)
			}
		},
	)
}

model : Expr, List(U8) -> Outcome
model = |expr, input| {
	match expr {
		Lit(lit) => if input.starts_with(lit) ok(lit, input.drop_first(lit.len())) else Err(Fail)
		Text(text) => model(Lit(text.to_utf8()), input)
		Cu(c) =>
			match input {
				[b, .. as rest] if b == c => ok([b], rest)
				_ => Err(Fail)
			}
		Sat(t) =>
			match input {
				[b, .. as rest] if b < t => ok([b], rest)
				_ => Err(Fail)
			}
		AnyCu =>
			match input {
				[b, .. as rest] => ok([b], rest)
				_ => Err(Fail)
			}
		AnyThing => ok(input, [])
		AnyString =>
			match Str.from_utf8(input) {
				Ok(_) => ok(input, [])
				Err(_) => Err(Fail)
			}
		Digit =>
			match input {
				[b, .. as rest] if b >= '0' and b <= '9' => ok([b], rest)
				_ => Err(Fail)
			}
		Digits => {
			count = input.fold_until(0, |n, b| if b >= '0' and b <= '9' Continue(n + 1) else Break(n))
			if count == 0 {
				Err(Fail)
			} else {
				value = input.sublist({ start: 0, len: count }).fold(0.U128, |acc, b| if acc > 18446744073709551615 acc else acc * 10 + U8.to_u128(b - '0'))
				if value > 18446744073709551615 {
					Err(Fail)
				} else {
					ok(value.to_str().to_utf8(), input.drop_first(count))
				}
			}
		}
		Const(lit) => ok(lit, input)
		Fail => Err(Fail)
		ChompWhile(t) => {
			count = input.fold_until(0, |n, b| if b < t Continue(n + 1) else Break(n))
			ok(input.sublist({ start: 0, len: count }), input.drop_first(count))
		}
		ChompUntil(c) =>
			match input.find_first_index(|b| b == c) {
				Ok(i) => ok(input.sublist({ start: 0, len: i }), input.drop_first(i))
				Err(_) => Err(Fail)
			}
		Seq(a, b) => then(model(a, input), |x, r1| then(model(b, r1), |y, r2| ok(x.concat(y), r2)))
		Apply(a, b) => then(model(a, input), |x, r1| then(model(b, r1), |y, r2| ok(x.concat(y).append('+'), r2)))
		Skip(a, b) => then(model(a, input), |x, r1| then(model(b, r1), |_, r2| ok(x, r2)))
		Map3(a, b, c) =>
			then(model(a, input), |x, r1| then(model(b, r1), |y, r2| then(model(c, r2), |z, r3| ok(x.append('|').concat(y).append('|').concat(z), r3))))
		Alt(a, b) => first_success([a, b], input)
		OneOf(es) => first_success(es, input)
		StrOneOf(es) => first_success(es, input)
		Many(a) => {
			r = repeat(|i| model(a, i), input)
			ok(items(r.vals, ';'), r.rest)
		}
		OneOrMore(a) => then(model(a, input), |x, r1| {
			r = repeat(|i| model(a, i), r1)
			ok(items(r.vals.prepend(x), ';'), r.rest)
		})
		SepBy(a, s) =>
			match model(SepBy1(a, s), input) {
				Ok(r) => Ok(r)
				Err(_) => ok([], input)
			}
		SepBy1(a, s) => then(model(a, input), |x, r1| {
			r = repeat(|i| then(model(s, i), |_, ri| model(a, ri)), r1)
			ok(items(r.vals.prepend(x), ','), r.rest)
		})
		Between(a, o, c) => then(model(o, input), |_, r1| then(model(a, r1), |x, r2| then(model(c, r2), |_, r3| ok(x, r3))))
		Maybe(a) =>
			match model(a, input) {
				Ok(r) => ok(r.val.prepend('J'), r.rest)
				Err(_) => ok(['N'], input)
			}
		Rev(a) => then(model(a, input), |x, r1| ok(x.drop_first(1), r1))
		EvenOnly(a) => then(model(a, input), |x, r1| if x.len() % 2 == 0 ok(x, r1) else Err(Fail))
		Lazy(a) => model(a, input)
		Ignore(a) => then(model(a, input), |_, r1| ok([], r1))
	}
}

# --------------------------------------------------------------------- test

show_outcome : Outcome -> Str
show_outcome = |o| {
	match o {
		Ok(r) => "Ok(val=${Str.inspect(r.val)}, rest=${Str.inspect(r.rest)})"
		Err(_) => "Err(Fail)"
	}
}

test : Case -> Fuzz.Outcome
test = |case| {
	parser = build(case.expr)
	expected = model(case.expr, case.input)
	actual : Outcome
	actual =
		match Parser.parse_partial(parser, case.input) {
			Ok({ value: val, rest: rest }) => Ok({ val, rest })
			Err(ParsingFailure(msg)) => {
				# Messages must be renderable without crashing.
				_ = msg.count_utf8_bytes()
				Err(Fail)
			}
		}
	if actual != expected {
		crash "parse_partial disagrees with model\n  library: ${show_outcome(actual)}\n  model:   ${show_outcome(expected)}"
	}
	# Full-input runs agree with the partial run.
	full_ok =
		match Utf8.parse_utf8(parser, case.input) {
			Ok(v) => Ok(v)
			Err(ParsingIncomplete(rest)) => Err(Incomplete(rest))
			Err(ParsingFailure(_)) => Err(Failure)
		}
	expected_full =
		match expected {
			Ok(r) => if r.rest.is_empty() Ok(r.val) else Err(Incomplete(r.rest))
			Err(_) => Err(Failure)
		}
	if full_ok != expected_full {
		crash "parse_utf8 disagrees with parse_partial"
	}
	# Str front ends only when the input is valid UTF-8.
	match Str.from_utf8(case.input) {
		Ok(text) => {
			partial = Utf8.parse_str_partial(parser, text)
			match (partial, expected) {
				(Ok(p), Ok(r)) =>
					if p.value != r.val or p.rest != Str.from_utf8_lossy(r.rest) {
						crash "parse_str_partial disagrees with model"
					}
				(Err(_), Err(_)) => {}
				_ => crash "parse_str_partial success differs from model"
			}
			full = Utf8.parse_str(parser, text)
			match (full, expected) {
				(Ok(v), Ok(r)) => if !r.rest.is_empty() or v != r.val crash "parse_str Ok differs from model"
				(Err(ParsingIncomplete(left)), Ok(r)) => if r.rest.is_empty() or left != Str.from_utf8_lossy(r.rest) crash "parse_str leftover differs from model"
				(Err(ParsingFailure(_)), Err(_)) => {}
				_ => crash "parse_str outcome differs from model"
			}
		}
		Err(_) => {}
	}
	Fuzz.keep
}

show_expr : Expr -> Str
show_expr = |expr| {
	match expr {
		Lit(l) => "Lit(${Str.inspect(l)})"
		Text(t) => "Text(${Str.inspect(t)})"
		Cu(c) => "Cu(${c.to_str()})"
		Sat(t) => "Sat(<${t.to_str()})"
		AnyCu => "AnyCu"
		AnyThing => "AnyThing"
		AnyString => "AnyString"
		Digit => "Digit"
		Digits => "Digits"
		Const(l) => "Const(${Str.inspect(l)})"
		Fail => "Fail"
		ChompWhile(t) => "ChompWhile(<${t.to_str()})"
		ChompUntil(c) => "ChompUntil(${c.to_str()})"
		Seq(a, b) => "Seq(${show_expr(a)}, ${show_expr(b)})"
		Apply(a, b) => "Apply(${show_expr(a)}, ${show_expr(b)})"
		Skip(a, b) => "Skip(${show_expr(a)}, ${show_expr(b)})"
		Map3(a, b, c) => "Map3(${show_expr(a)}, ${show_expr(b)}, ${show_expr(c)})"
		Alt(a, b) => "Alt(${show_expr(a)}, ${show_expr(b)})"
		OneOf(es) => "OneOf([${Str.join_with(es.map(show_expr), ", ")}])"
		StrOneOf(es) => "StrOneOf([${Str.join_with(es.map(show_expr), ", ")}])"
		Many(a) => "Many(${show_expr(a)})"
		OneOrMore(a) => "OneOrMore(${show_expr(a)})"
		SepBy(a, b) => "SepBy(${show_expr(a)}, ${show_expr(b)})"
		SepBy1(a, b) => "SepBy1(${show_expr(a)}, ${show_expr(b)})"
		Between(a, b, c) => "Between(${show_expr(a)}, ${show_expr(b)}, ${show_expr(c)})"
		Maybe(a) => "Maybe(${show_expr(a)})"
		Rev(a) => "Rev(${show_expr(a)})"
		EvenOnly(a) => "EvenOnly(${show_expr(a)})"
		Lazy(a) => "Lazy(${show_expr(a)})"
		Ignore(a) => "Ignore(${show_expr(a)})"
	}
}

target = Fuzz.target_with({
	name: "parser-combinators",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |case| "${show_expr(case.expr)} on ${Str.inspect(case.input)}",
})
