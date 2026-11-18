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

property_target = Fuzz.target_with({
	examples: [],
	name: "http-response",
	generator: Fuzz.str,
	test,
	show: |input| HttpCheck.show(input.to_utf8()),
})

HttpResponseProperty :: [].{
	target = property_target
}
