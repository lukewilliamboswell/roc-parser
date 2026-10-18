app [target] { fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst" }
import fuzz.Fuzz

## Mode 0 reassigns a function-level `var` from a destructured call result inside
## the branch that produces the function's value. Mode 1 is identical except the
## var is declared inside that branch. Mode 2 binds a fresh immutable name.
## All three build the same list; only allocation counts differ.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	mode = bytes.get(0) ?? 0
	measured = Fuzz.measure_allocs!(|{}| run(mode, 4096))
	crash "VARJOIN mode=${mode.to_str()} allocations=${measured.allocations.to_str()} len=${measured.value.len().to_str()} sum=${measured.value.fold(0, |a, x| a + x).to_str()}"
}

target = Fuzz.target_with!({ name: "var-join", generator: Fuzz.raw_bytes, test!, show: Str.inspect })

R : { out : List(U64), last : U64 }

push : List(U64), U64 -> R
push = |out, x| { out: out.append(x), last: x }

step0 : List(U64), U64 -> R
step0 = |out, x| {
	var $out = out
	if x % 2 == 0 {
		{ out: $out, last } = push($out, x)
		{ out: $out.append(last), last }
	} else {
		{ out: $out.append(x), last: x }
	}
}

step1 : List(U64), U64 -> R
step1 = |out, x| {
	if x % 2 == 0 {
		var $out = out
		{ out: $out, last } = push($out, x)
		{ out: $out.append(last), last }
	} else {
		{ out: out.append(x), last: x }
	}
}

step2 : List(U64), U64 -> R
step2 = |out, x| {
	if x % 2 == 0 {
		{ out: o1, last } = push(out, x)
		{ out: o1.append(last), last }
	} else {
		{ out: out.append(x), last: x }
	}
}

run : U8, U64 -> List(U64)
run = |mode, n| {
	var $out = []
	var $i = 0
	while $i < n {
		{ out, last: _ } = match mode {
			0 => step0($out, $i)
			1 => step1($out, $i)
			_ => step2($out, $i)
		}
		$out = out
		$i = $i + 1
	}
	$out
}
