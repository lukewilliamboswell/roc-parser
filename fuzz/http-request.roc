app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import HttpCheck

## Arbitrary text must parse or fail cleanly as a request, and every parsed
## request must satisfy the invariants in fuzz/HttpCheck.roc (prefix
## consumption, field syntax, canonical re-serialization, pipelining and
## truncation). Uses Fuzz.str, encoded identically in roc-fuzz 0.3.0 and
## 0.4.2, so the reviewed seeds in fuzz/seeds/http-request.json still apply.
test : Str -> Fuzz.Outcome
test = |input| {
	HttpCheck.check_request(input.to_utf8())
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "http-request",
	generator: Fuzz.str,
	test,
	show: |input| HttpCheck.show(input.to_utf8()),
})
