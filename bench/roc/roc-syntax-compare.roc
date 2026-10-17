app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Monotonic
import parser.RocSyntax

Request : { id : Str, mode : Str, source : Str, benchmark_iterations : U64, benchmark_warmup : U64 }

# JSON decoding and source ownership are outside the timer. Each iteration
# constructs and releases a complete parse result. Only constant-time result
# fields are observed; the existing geometric benchmark measures full traversal.
main! : List(OsStr) => Try({}, _)
main! = |_| {
	var $running = True
	while $running {
		match Stdin.line!() {
			Err(EndOfFile) => {
				$running = False
			}
			Err(StdinErr(err)) => {
				return Err(StdinErr(err))
			}
			Ok(line) => {
				request : Try(Request, _)
				request = Json.parse(line)
				match request {
					Err(_) => Stdout.line!("{\"error\":\"invalid_request\"}")?
					Ok(item) => {
						if item.benchmark_iterations == 0 or item.benchmark_iterations > 100000 or item.benchmark_warmup > 10000 or item.source.count_utf8_bytes() > 16777216 or !["file", "expr", "statement", "header", "repl"].contains(item.mode) {
							Stdout.line!("{\"error\":\"invalid_benchmark_request\"}")?
						} else {
							input_bytes = item.source.to_utf8()
							var $warm = 0
							var $checksum = 0.U64
							while $warm < item.benchmark_warmup {
								parsed = parse(item.source, input_bytes, item.mode)
								$checksum = $checksum + parsed.nodes.len() + parsed.tokens.len() + parsed.root
								$warm = $warm + 1
							}
							var $i = 0
							var $successes = 0.U64
							var $consumed = 0.U64
							start = Monotonic.now!()
							while $i < item.benchmark_iterations {
								parsed = parse(item.source, input_bytes, item.mode)
								$checksum = $checksum + parsed.nodes.len() + parsed.tokens.len() + parsed.root
								if parsed.status == Complete {
									$successes = $successes + 1
								}
								if parsed.consumed {
									$consumed = $consumed + 1
								}
								$i = $i + 1
							}
							elapsed = Monotonic.now!() - start
							Stdout.line!(Json.to_str({ version: 1.U64, id: item.id, iterations: $i, warmup: $warm, elapsed_ns: elapsed, successes: $successes, consumed: $consumed, checksum: $checksum }))?
						}
					}
				}
			}
		}
	}
	Ok({})
}

parse : Str, List(U8), Str -> RocSyntax.Parsed
parse = |source, bytes, mode| match mode {
	"expr" => RocSyntax.parse_expr_bytes(bytes)
	"statement" => RocSyntax.parse_statement_str(source)
	"header" => RocSyntax.parse_header_str(source)
	"repl" => {
		file = RocSyntax.parse_bytes(bytes)
		root = file.nodes.get(file.root) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
		if file.status == Complete and root.children_len == 1 file else RocSyntax.parse_statement_str(source)
	}
	_ => RocSyntax.parse_bytes(bytes)
}
