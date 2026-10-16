app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Utc
import parser.RocSyntax

## In-process optimized full parse with shared input. Input/output and the
## status's display string are outside timing. Consume child IDs as well as
## node/token spans so precedence topology remains observable.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	iterations = match args.last() {
		Ok(arg) => U64.from_str(OsStr.display(arg)) ?? 1
		Err(_) => 1
	}
	input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
	var $checksum = 0.U64
	var $successes = 0.U64
	var $status = Complete
	var $i = 0
	start = Utc.now!()
	while $i < iterations {
		parsed = RocSyntax.parse_str(input)
		$checksum = $checksum + parsed.tokens.fold(0, |sum, token| sum + token.start + token.end)
			+ parsed.nodes.fold(0, |sum, node| sum + node.start + node.end + node.children_start + node.children_len)
			+ parsed.children.fold(0, |sum, child| (sum * 31 + child) % 1000000007)
		$status = parsed.status
		if parsed.status == Complete { $successes = $successes + 1 }
		$i = $i + 1
	}
	elapsed = Utc.delta_as_nanos(Utc.now!(), start)
	Stdout.line!(Json.to_str({ iterations, elapsed_ns: elapsed, successes: $successes, checksum: $checksum, status: Str.inspect($status) }))?
	Ok({})
}
