## A rejection must name its intended diagnostic and must never be a crash.
LiteralContract := [].{
	check : I32, Str, List(Str) -> Try({}, [CompilerCrash(I32), UnexpectedAcceptance, UnexpectedRejection(I32), MissingDiagnostics(List(Str))])
	check = |code, output, expected| {
		text = normalize(output)
		if code < 0 or ["segmentation fault", "internal compiler error", "thread panic", "panic:"].any(|marker| text.contains(marker)) {
			return Err(CompilerCrash(code))
		}
		if expected.is_empty() {
			return if code == 0 Ok({}) else Err(UnexpectedRejection(code))
		}
		if code == 0 {
			return Err(UnexpectedAcceptance)
		}
		missing = expected.keep_if(|fragment| !text.contains(normalize(fragment)))
		if missing.is_empty() {
			Ok({})
		} else {
			Err(MissingDiagnostics(missing))
		}
	}
	normalize : Str -> Str
	normalize = |text| Str.join_with(Str.from_utf8_lossy(text.to_utf8().map(|byte| if byte >= 65 and byte <= 90 byte + 32 else if [9, 10, 13].contains(byte) 32 else byte)).split_on(" ").keep_if(|part| part != ""), " ")
}
expect LiteralContract.check(0, "compile time crash", ["compile time crash"]) == Err(UnexpectedAcceptance)
expect LiteralContract.check(1, "type mismatch", ["compile time crash"]).is_err()
expect {
	var $valid = True
	for index in List.repeat({}, 2000).map_with_index(|_, index| index) {
		name = "source-${index.to_str()}.yaml"
		$valid = $valid and LiteralContract.check(1, "COMPILE\nTIME CRASH ${name}", ["compile time crash", name]) == Ok({})
		$valid = $valid and LiteralContract.check(-11, "compile time crash ${name}", [name]).is_err()
	}
	$valid
}
