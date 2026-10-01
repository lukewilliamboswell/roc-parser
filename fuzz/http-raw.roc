app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import HttpCheck

## Arbitrary bytes, including invalid UTF-8 as a network peer may send them,
## must parse or fail cleanly, and parsed messages must satisfy the invariants
## in fuzz/HttpCheck.roc. The first byte selects the parser: even for a
## request, odd for a response. Run with a large --max-input-size and a
## --timeout to catch super-linear behaviour on big heads and bodies.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match bytes {
		[] => Fuzz.reject
		[mode, .. as message] => {
			if mode % 2 == 0 HttpCheck.check_request(message) else HttpCheck.check_response(message)
			Fuzz.keep
		}
	}
}

show : List(U8) -> Str
show = |bytes| {
	match bytes {
		[mode, .. as message] => {
			digits = message.fold([], |acc, b| acc.append(hex_digit(b // 16)).append(hex_digit(b % 16)))
			kind = if mode % 2 == 0 "Q" else "S"
			"${HttpCheck.show(message)}\nhex: ${kind}${Str.from_utf8(digits) ?? ""}"
		}
		[] => "(empty)"
	}
}

hex_digit : U8 -> U8
hex_digit = |n| if n < 10 n + '0' else n - 10 + 'a'

target = Fuzz.target_with({
	name: "http-raw",
	generator: Fuzz.raw_bytes,
	test,
	show,
})
