import fuzz.Fuzz
import parser.OFX
import parser.Parser
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |choices| {
	generated = OfxGen.generate(choices)
	text = Str.from_utf8(generated.bytes) ?? crash "generator must write UTF8"
	invalid = text.replace_each("<TRNAMT>", "<TRNAMT>bad").to_utf8()
	if OFX.parse_bytes(invalid, {}).is_ok() {
		crash "invalid amount accepted"
	}
	report = (OFX.decoder({ errors: { policy: ReportInvalidRecords } })).collect_report([invalid].iter()) ?? crash "framed semantic errors were not recoverable"
	if report.report.rejected_records != generated.expected.statements.first().map_ok(|statement| statement.transactions.len()) ?? 0 {
		crash "wrong rejected transaction count"
	}
	broken = text.replace_each("</STMTTRN>", "</BROKEN>").to_utf8()
	outcomes = List.from_iter((OFX.decoder({ errors: { policy: SkipInvalidRecords } })).read_iter(OfxGen.chunks(broken, choices).iter()))
	if outcomes.last().map_ok(|event| event.is_err()) != Ok(True) {
		crash "syntax error was recovered"
	}
	capped = (OFX.decoder({ errors: { policy: SkipInvalidRecords, max_errors: 0 } })).collect_report([invalid].iter())
	if capped.is_ok() {
		crash "zero rejection cap accepted invalid transaction"
	}
	Fuzz.keep
}

property_target = Fuzz.target_with({ name: "ofx-malformed", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })

OfxMalformedProperty :: [].{
	target = property_target
}
