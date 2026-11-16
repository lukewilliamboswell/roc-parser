app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.OFX
import parser.Streaming
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |choices| {
	generated = OfxGen.generate(choices)
	text = Str.from_utf8(generated.bytes) ?? crash "generator must write UTF8"
	invalid = text.replace_each("<TRNAMT>", "<TRNAMT>bad").to_utf8()
	if OFX.parse_bytes(invalid, {}).is_ok() {
		crash "invalid amount accepted"
	}
	report = OFX.parse_report(invalid, { errors: { error_policy: ReportInvalidRecords } }) ?? crash "framed semantic errors were not recoverable"
	if report.report.rejected_records != generated.expected.statements.first().map_ok(|statement| statement.transactions.len()) ?? 0 {
		crash "wrong rejected transaction count"
	}
	broken = text.replace_each("</STMTTRN>", "</BROKEN>").to_utf8()
	outcomes = List.from_iter(OFX.parse_iter(OfxGen.chunks(broken, choices).iter(), { errors: { error_policy: SkipInvalidRecords } }))
	if outcomes.last().map_ok(|event| event.is_err()) != Ok(True) {
		crash "syntax error was recovered"
	}
	capped = OFX.parse_report(invalid, { errors: { error_policy: SkipInvalidRecords, max_errors: 0 } })
	if capped.is_ok() {
		crash "zero rejection cap accepted invalid transaction"
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-malformed", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
