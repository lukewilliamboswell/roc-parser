app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.Stdin
import cli.Stdout
import parser.QIF
import parser.JsonValue

json : Str -> Str
json = |text| JsonValue.to_str(String(text)) ?? "null"

row_json : QIF.Transaction -> Str
row_json = |r| {
	splits = r.splits.map(|s| "{\"category\":${json(s.category)},\"memo\":${json(s.memo)},\"amount\":${json(s.amount)}}") |> Str.join_with(",")
	"{\"account\":${json(r.raw.account)},\"date\":[${r.date.year.to_str()},${r.date.month.to_str()},${r.date.day.to_str()}],\"amount\":${json(r.amount)},\"payee\":${json(r.payee)},\"memo\":${json(r.memo)},\"number\":${json(r.number)},\"category\":${json(r.category)},\"cleared\":${json(r.cleared)},\"splits\":[${splits}]}"
}

main! = |_| {
	bytes = Stdin.read_to_end!()?
	result = decode(bytes)
	match result {
		Ok((doc, rows)) => {
			written = doc.to_str() ?? ""
			Stdout.line!("{\"status\":\"ok\",\"rows\":[${Str.join_with(rows.map(row_json), ",")}],\"written\":${json(written)}}")?
		}
		Err(InvalidQif(error)) => Stdout.line!("{\"status\":\"error\",\"offset\":${error.offset.to_str()},\"message\":${json(error.message)}}")?
	}
	Ok({})
}

decode = |bytes| {
	doc = QIF.parse_bytes(bytes, {})?
	rows = doc.transactions({ date_order: DayFirst })?
	Ok((doc, rows))
}
