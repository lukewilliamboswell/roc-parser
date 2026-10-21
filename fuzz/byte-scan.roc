app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Parser
import parser.Utf8

## The vector byte scanners agree with a byte-at-a-time reference:
## - Utf8.find_any and Utf8.skip_class, for a class drawn from the input
##   (an explicit byte set, a byte range, or a complement, which covers
##   both the vector and the scalar-only table shapes), from every start;
## - Utf8.find_line_end from every start;
## - Utf8.span_class returns exactly the skipped prefix as a slice.

Case : { members : List(U8), lo : U8, hi : U8, mode : U8, input : List(U8) }

generate : List(U8) -> Case
generate = |bytes| {
	mode = bytes.get(0) ?? 0
	count = U8.to_u64(bytes.get(1) ?? 0) % 9
	members = bytes.sublist({ start: 2, len: count })
	lo = bytes.get(2) ?? 0
	hi = bytes.get(3) ?? 0
	{ members, lo, hi, mode, input: bytes.drop_first(2 + count) }
}

member_of : Case -> (U8 -> Bool)
member_of = |case| {
	match case.mode % 4 {
		0 => |b| case.members.contains(b)
		1 => |b| b >= case.lo and b <= case.hi
		2 => |b| !case.members.contains(b)
		_ => |b| b % 16 <= b / 16 or case.members.contains(b)
	}
}

reference : List(U8), U64, (U8 -> Bool), Bool -> U64
reference = |bytes, pos, member, want| {
	var $i = pos
	while $i < bytes.len() {
		if member(bytes.get($i) ?? 0) == want {
			break
		}
		$i = $i + 1
	}
	$i
}

test : Case -> Fuzz.Outcome
test = |case| {
	member = member_of(case)
	class = Utf8.ByteClass.from_predicate(member)
	line_end = |b| b == '\n' or b == '\r'
	input = case.input
	var $pos = 0
	while $pos <= input.len() + 1 {
		if Utf8.find_any(input, $pos, class) != reference(input, $pos, member, True) {
			crash "find_any disagrees with the reference at ${$pos.to_str()}"
		}
		if Utf8.skip_class(input, $pos, class) != reference(input, $pos, member, False) {
			crash "skip_class disagrees with the reference at ${$pos.to_str()}"
		}
		if Utf8.find_line_end(input, $pos) != reference(input, $pos, line_end, True) {
			crash "find_line_end disagrees with the reference at ${$pos.to_str()}"
		}
		$pos = $pos + 1
	}
	match Utf8.parse_bytes_partial(Utf8.span_class(class), input) {
		Ok({ value, rest }) => {
			n = reference(input, 0, member, False)
			if value != input.take_first(n) or rest != input.drop_first(n) {
				crash "span_class returned the wrong split"
			}
		}
		Err(_) => crash "span_class failed"
	}
	_ = Parser.const({})
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "byte-scan",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |case| "mode=${case.mode.to_str()} members=${Str.inspect(case.members)} lo=${case.lo.to_str()} hi=${case.hi.to_str()} input=${Str.inspect(case.input)}",
})
