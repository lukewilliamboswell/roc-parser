app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.String

# tag::types[]
Level : [Info, Warn, Error]

Time : { hour : U64, minute : U64 }

Entry : { time : Time, level : Level, message : Str }

# end::types[]

# tag::pieces[]
two_digits : Parser(String.Utf8, U64)
two_digits =
	Parser.const(|tens| |ones| tens * 10 + ones)
		.keep(String.digit)
		.keep(String.digit)

time : Parser(String.Utf8, Time)
time =
	Parser.const(|hour| |minute| { hour, minute })
		.keep(two_digits)
		.skip(String.codeunit(':'))
		.keep(two_digits)
		.map(
			|t| if t.hour < 24 and t.minute < 60 {
				Ok(t)
			} else {
				Err("no such time")
			},
		)
		.flatten()

level : Parser(String.Utf8, Level)
level =
	String.one_of([
		Parser.const(Info).skip(String.string("INFO")),
		Parser.const(Warn).skip(String.string("WARN")),
		Parser.const(Error).skip(String.string("ERROR")),
	])

message : Parser(String.Utf8, Str)
message = String.any_string

# end::pieces[]

# tag::entry[]
entry : Parser(String.Utf8, Entry)
entry =
	Parser.const(|t| |l| |m| { time: t, level: l, message: m })
		.keep(time)
		.skip(String.codeunit(' '))
		.keep(level)
		.skip(String.codeunit(' '))
		.keep(message)
# end::entry[]

# tag::tests[]
expect String.parse_str(time, "09:05") == Ok({ hour: 9, minute: 5 })
expect String.parse_str(time, "24:00") == Err(ParsingFailure("no such time"))
expect String.parse_str(level, "WARN") == Ok(Warn)

expect
	String.parse_str(entry, "12:30 WARN disk almost full")
		== Ok({ time: { hour: 12, minute: 30 }, level: Warn, message: "disk almost full" })

expect String.parse_str(entry, "12:30 DEBUG hello").is_err()
# end::tests[]

# tag::file[]
Problem : { line : U64, reason : Str }

parse_log : Str -> Try(List(Entry), Problem)
parse_log = |text| {
	var $entries = []
	var $number = 0
	for line in text.split_on("\n") {
		$number = $number + 1
		match String.parse_str(entry, line) {
			_ if line.is_empty() => {}
			Ok(e) => {
				$entries = $entries.append(e)
			}
			Err(ParsingFailure(reason)) => return Err({ line: $number, reason })
			Err(ParsingIncomplete(rest)) => return Err({ line: $number, reason: "unexpected `${rest}`" })
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
			entries.map(|e| "${level_name(e.level)} at ${e.time.hour.to_str()}h: ${e.message}")
				|> Str.join_with("\n")
		Err({ line, reason }) => "line ${line.to_str()}: ${reason}"
	}
}

main! = |_args| {
	Stdout.line!(summary("08:00 INFO started\n12:30 WARN disk almost full\n"))?
	Stdout.line!(summary("08:00 INFO started\n\n25:00 ERROR late\n"))
}
# end::main[]
