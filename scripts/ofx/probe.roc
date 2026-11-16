app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.OFX
import parser.JsonValue

main! : List(OsStr) => Try({}, _)
main! = |args| {
	bytes = Stdin.read_to_end!()?
	match OFX.parse_bytes(bytes, {}) {
		Err(InvalidOfx(error)) => Stdout.line!(JsonValue.to_str(Object([{ key: "status", value: String("error") }, { key: "message", value: String(error.message) }, { key: "offset", value: Number(error.offset.to_str()) }])) ?? "{}")?
		Ok(document) => {
			mode = args.first().map_ok(OsStr.display) ?? "json"
			if mode == "xml" or mode == "sgml" {
				encoded = OFX.to_bytes(document, { syntax: if (mode == "sgml") Sgml102 else Xml203 })?
				Stdout.write!(Str.from_utf8(encoded) ?? "")?
			} else {
				value : JsonValue
				value = Object([{ key: "status", value: String("ok") }, { key: "statements", value: Array(document.statements.map(statement_json)) }])
				Stdout.line!(JsonValue.to_str(value) ?? "{}")?
			}
		}
	}
	Ok({})
}

obj : List((Str, Str)) -> JsonValue
obj = |pairs| Object(pairs.map(|pair| { key: pair.0, value: String(pair.1) }))

statement_json : OFX.Statement -> JsonValue
statement_json = |statement| {
	context = statement.context
	account = context.account
	Object([
		{ key: "kind", value: String(if (context.kind == Bank) "bank" else "credit_card") },
		{ key: "currency", value: String(context.currency) },
		{ key: "response", value: obj([("transaction_id", context.transaction_id), ("code", context.status.code), ("severity", context.status.severity), ("message", context.status.message)]) },
		{ key: "header", value: obj([("old_uid", context.header.old_uid), ("new_uid", context.header.new_uid), ("server_date", context.header.server_date), ("language", context.header.language), ("org", context.header.org), ("fid", context.header.fid)]) },
		{ key: "account", value: obj([("bank_id", account.bank_id), ("branch_id", account.branch_id), ("account_id", account.account_id), ("account_type", account.account_type), ("key", account.key)]) },
		{ key: "start", value: String(context.start) },
		{ key: "end", value: String(context.end) },
		{ key: "transactions", value: Array(statement.transactions.map(|value| obj([("type", value.type), ("posted", value.posted), ("amount", value.amount), ("id", value.id), ("name", value.name), ("memo", value.memo), ("check_number", value.check_number)]))) },
		{ key: "ledger_balance", value: balance_json(statement.ledger_balance) },
		{ key: "available_balance", value: balance_json(statement.available_balance) },
	])
}

balance_json : Try(OFX.Balance, [Missing]) -> JsonValue
balance_json = |balance| match balance {
	Ok(value) => obj([("amount", value.amount), ("as_of", value.as_of)])
	Err(_) => Null
}
