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

property_target = Fuzz.target_with({
	examples: [],
	name: "http-request",
	generator: Fuzz.str,
	test,
	show: |input| HttpCheck.show(input.to_utf8()),
})

HttpRequestProperty :: [].{
	target = property_target
}
