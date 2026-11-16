import parser.OFX
import parser.Streaming

## Specification-generated bank/card models and independent wire spellings.
OfxGen :: [].{
	Input : { bytes : List(U8), expected : OFX }
	generate : List(U8) -> Input
	generate = |choices| {
		pick = |index| (choices.get(index) ?? 0).to_u64()
		card = pick(0) % 2 == 1
		sgml = pick(1) % 2 == 1
		header = { old_uid: "NONE", new_uid: "NONE", server_date: "20000101000000", language: "ENG", org: "", fid: "" }
		account = { bank_id: if (card) "" else "001", branch_id: "", account_id: "000123", account_type: if (card) "" else "CHECKING", key: "" }
		context = { kind: if (card) CreditCard else Bank, currency: "AUD", account, start: "20260101", end: "20260201", transaction_id: "0", status: { code: "0", severity: "INFO", message: "" }, header }
		var $transactions = []
		var $index = 0
		while $index < pick(2) % 8 + 1 {
			name = ["Roc", "Café", "A & B", "Euro €", "<shop>"].get(pick(3 + $index) % 5) ?? "Roc"
			$transactions = $transactions.append({ type: "DEBIT", posted: "20260103120000[-5:EST]", amount: "-${pick(8 + $index).to_str()}.00", id: "000${$index.to_str()}", name, memo: "Note", check_number: "", extensions: [] })
			$index = $index + 1
		}
		balance = { amount: "100.00", as_of: "20260201" }
		expected = OFX.{ statements: [{ context, transactions: $transactions, ledger_balance: Ok(balance), available_balance: Err(Missing) }] }
		leaf = |tag, value| "<${tag}>${value.replace_each("&", "&amp;").replace_each("<", "&lt;")}${if (sgml) "\n" else "</${tag}>"}"
		group = if (card) "CREDITCARDMSGSRSV1" else "BANKMSGSRSV1"
		wrap = if (card) "CCSTMTTRNRS" else "STMTTRNRS"
		stmt = if (card) "CCSTMTRS" else "STMTRS"
		acct = if (card) "CCACCTFROM" else "BANKACCTFROM"
		bank = if (card) "" else leaf("BANKID", "001")
		records = Str.join_with($transactions.map(|t| "<STMTTRN>${leaf("TRNTYPE", t.type)}${leaf("DTPOSTED", t.posted)}${leaf("TRNAMT", t.amount)}${leaf("FITID", t.id)}${leaf("NAME", t.name)}${leaf("MEMO", t.memo)}</STMTTRN>"), "")
		body = "<OFX><SIGNONMSGSRSV1><SONRS><STATUS>${leaf("CODE", "0")}${leaf("SEVERITY", "INFO")}</STATUS>${leaf("DTSERVER", "20000101000000")}${leaf("LANGUAGE", "ENG")}</SONRS></SIGNONMSGSRSV1><${group}><${wrap}>${leaf("TRNUID", "0")}<STATUS>${leaf("CODE", "0")}${leaf("SEVERITY", "INFO")}</STATUS><${stmt}>${leaf("CURDEF", "AUD")}<${acct}>${bank}${leaf("ACCTID", "000123")}${if (card) "" else leaf("ACCTTYPE", "CHECKING")}</${acct}><BANKTRANLIST>${leaf("DTSTART", "20260101")}${leaf("DTEND", "20260201")}${records}</BANKTRANLIST><LEDGERBAL>${leaf("BALAMT", balance.amount)}${leaf("DTASOF", balance.as_of)}</LEDGERBAL></${stmt}></${wrap}></${group}></OFX>"
		prefix = if (sgml) "OFXHEADER:100\r\nDATA:OFXSGML\r\nVERSION:102\r\nSECURITY:NONE\r\nENCODING:UTF-8\r\nCHARSET:NONE\r\nCOMPRESSION:NONE\r\nOLDFILEUID:NONE\r\nNEWFILEUID:NONE\r\n\r\n" else "<?xml version=\"1.0\" encoding=\"UTF-8\"?><?OFX OFXHEADER=\"200\" VERSION=\"203\" SECURITY=\"NONE\" OLDFILEUID=\"NONE\" NEWFILEUID=\"NONE\"?>"
		{ bytes: "${prefix}${body}".to_utf8(), expected }
	}

	## Chunk sizes and empty chunks are independently chosen by fuzz bytes.
	chunks : List(U8), List(U8) -> List(List(U8))
	chunks = |bytes, choices| {
		var $out = [[]]
		var $at = 0
		var $index = 0
		while $at < bytes.len() {
			width = (choices.get($index % (choices.len() + 1)) ?? 0).to_u64() % 31 + 1
			$out = $out.append(bytes.sublist({ start: $at, len: width.min(bytes.len() - $at) }))
			if $index % 3 == 0 {
				$out = $out.append([])
			}
			$at = $at + width.min(bytes.len() - $at)
			$index = $index + 1
		}
		$out
	}

	## A valid reader's events, consumed lazily by the actual OFX writer.
	values : Iter(Try(Streaming.ReadEvent(OFX.Event, OFX.Error), Streaming.ReadError(OFX.Error))) -> Iter(OFX.Event)
	values = |events| Iter.custom(
		events,
		Err(Unknown),
		|start| {
			var $cursor = start
			while True {
				match $cursor.next() {
					Done => return Err(NoMore)
					Skip({ rest }) => {
						$cursor = rest
					}
					One({ item: Ok(Value(value)), rest }) => return Ok((value, rest))
					One({ item: Ok(Finished(_)), .. }) => return Err(NoMore)
					_ => crash "valid OFX reader failed in writer pipeline"
				}
			}
		},
	)

	## Unwrap successful bounded output without collecting encoded documents.
	unwrap_bytes : Iter(Try(List(U8), problem)) -> Iter(List(U8))
	unwrap_bytes = |source| source.map(
		|chunk| match chunk {
			Ok(value) => value
			Err(_) => crash "valid OFX writer failed"
		},
	)
}
