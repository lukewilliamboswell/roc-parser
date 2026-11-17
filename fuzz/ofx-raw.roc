app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.OFX
import parser.Streaming
import OfxGen

collect : List(Try(Streaming.ReadEvent(OFX.Event, OFX.Error), Streaming.ReadError(OFX.Error))) -> Try(OFX, [InvalidOfx(OFX.Error)])
collect = |events| {
	var $statements = []
	var $current = { context: { kind: Bank, currency: "", account: { bank_id: "", branch_id: "", account_id: "", account_type: "", key: "" }, start: "", end: "", transaction_id: "", status: { code: "", severity: "", message: "" }, header: { old_uid: "", new_uid: "", server_date: "", language: "", org: "", fid: "" } }, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }
	for event in events {
		match event {
			Ok(Value(StatementStart(context))) => {
				$current = { context, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }
			}
			Ok(Value(Transaction(value))) => {
				$current = { ..$current, transactions: $current.transactions.append(value) }
			}
			Ok(Value(LedgerBalance(value))) => {
				$current = { ..$current, ledger_balance: Ok(value) }
			}
			Ok(Value(AvailableBalance(value))) => {
				$current = { ..$current, available_balance: Ok(value) }
			}
			Ok(Value(StatementEnd)) => {
				$statements = $statements.append($current)
			}
			Ok(Finished(_)) => return Ok(OFX.{ statements: $statements })
			Err(ParseErr(error)) => return Err(InvalidOfx(error))
			_ => crash "unexpected fail-fast streaming protocol outcome"
		}
	}
	crash "streaming OFX omitted completion"
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	results = List.from_iter(Streaming.read_iter(OfxGen.chunks(bytes, bytes).iter(), OFX.decoder({})))
	if OFX.parse_bytes(bytes, {}) != collect(results) {
		crash "whole-document and chunked OFX models or diagnostics disagree"
	}
	for event in results {
		match event {
			Err(ParseErr(error)) | Err(ErrorLimitExceeded({ error, .. })) => {
				if error.offset > bytes.len() or error.line == 0 or error.column == 0 {
					crash "OFX diagnostic outside source"
				}
			}
			_ => {}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-raw", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
