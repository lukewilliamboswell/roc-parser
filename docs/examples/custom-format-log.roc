app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.Utf8

# tag::types[]
Level : [Info, Warn, Error]

Time : { hour : U64, minute : U64 }

Entry : { time : Time, level : Level, message : Str }

# end::types[]

# tag::pieces[]
two_digits : Parser(Utf8.Bytes, U64)
two_digits =
	Parser.const(|tens| |ones| tens * 10 + ones)
		.keep(Utf8.digit)
		.keep(Utf8.digit)

time : Parser(Utf8.Bytes, Time)
time =
	Parser.const(|hour| |minute| { hour, minute })
		.keep(two_digits)
		.skip(Utf8.codeunit(':'))
		.keep(two_digits)
		.map(
			|t| if t.hour < 24 and t.minute < 60 {
				Ok(t)
			} else {
				Err("no such time")
			},
		)
		.flatten()

level : Parser(Utf8.Bytes, Level)
level =
	Parser.one_of([
		Parser.const(Info).skip(Utf8.string("INFO")),
		Parser.const(Warn).skip(Utf8.string("WARN")),
		Parser.const(Error).skip(Utf8.string("ERROR")),
	])

message : Parser(Utf8.Bytes, Str)
message = Utf8.rest_str

# end::pieces[]

# tag::entry[]
entry : Parser(Utf8.Bytes, Entry)
entry =
	Parser.const(|t| |l| |m| { time: t, level: l, message: m })
		.keep(time)
		.skip(Utf8.codeunit(' '))
		.keep(level)
		.skip(Utf8.codeunit(' '))
		.keep(message)
# end::entry[]

# tag::tests[]
expect Utf8.parse_str(time, "09:05") == Ok({ hour: 9, minute: 5 })
expect Utf8.parse_str(time, "24:00") == Err(ParseError({ message: "no such time", offset: 0 }))
expect Utf8.parse_str(level, "WARN") == Ok(Warn)

expect
	Utf8.parse_str(entry, "12:30 WARN disk almost full")
		== Ok({ time: { hour: 12, minute: 30 }, level: Warn, message: "disk almost full" })

expect Utf8.parse_str(entry, "12:30 DEBUG hello").is_err()
# end::tests[]

# tag::file[]
Problem : { line : U64, reason : Str }

parse_log : Str -> Try(List(Entry), Problem)
parse_log = |text| {
	var $entries = []
	var $number = 0
	for line in text.split_on("\n") {
		$number = $number + 1
		match Utf8.parse_str(entry, line) {
			_ if line.is_empty() => {}
			Ok(e) => {
				$entries = $entries.append(e)
			}
			Err(ParseError({ message: reason, offset: _ })) => return Err({ line: $number, reason })
		}
	}
	Ok($entries)
}

# end::file[]

# tag::main[]
level_name : Level -> Str
level_name = |l| {
	match l {
		Info => "info"
		Warn => "warning"
		Error => "error"
	}
}

summary : Str -> Str
summary = |text| {
	match parse_log(text) {
		Ok(entries) =>
			Str.join_with(entries.map(|e| "${level_name(e.level)} at ${e.time.hour.to_str()}h: ${e.message}"), "\n")
		Err({ line, reason }) => "line ${line.to_str()}: ${reason}"
	}
}

main! = |_args| {
	Stdout.line!(summary("08:00 INFO started\n12:30 WARN disk almost full\n"))?
	Stdout.line!(summary("08:00 INFO started\n\n25:00 ERROR late\n"))
}
# end::main[]
