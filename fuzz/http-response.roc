app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import HttpCheck

## Arbitrary text must parse or fail cleanly as a response, and every parsed
## response must satisfy the invariants in fuzz/HttpCheck.roc. Uses Fuzz.str,
## encoded identically in roc-fuzz 0.3.0 and 0.4.2, so the reviewed seeds in
## fuzz/seeds/http-response.json still apply.
test : Str -> Fuzz.Outcome
test = |input| {
	HttpCheck.check_response(input.to_utf8())
	Fuzz.keep
}

target = Fuzz.target_with({
	examples: [],
	name: "http-response",
	generator: Fuzz.str,
	test,
	show: |input| HttpCheck.show(input.to_utf8()),
})
