import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Utc

## The timing loop shared by every format driver. It reads one document from
## stdin, parses it `iterations` times (the last argument), and prints one JSON
## line. Reading stdin and printing are outside the timed region.
##
## `parse` returns `Ok(checksum)` for a document it accepts and `Err(checksum)`
## for one it rejects; the checksum makes the result observable so the
## optimizer cannot drop the work.
Bench :: {}.{

	run! : List(OsStr), (Str -> Try(U64, U64)) => Try({}, _)
	run! = |args, parse| {
		iterations = match args.last() {
			Ok(arg) => U64.from_str(OsStr.display(arg)) ?? 1
			Err(_) => 1
		}
		input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
		repetitions = List.repeat({}, iterations)
		start = Utc.now!()
		var $checksum = 0.U64
		var $successes = 0.U64
		for _ in repetitions {
			match parse(input) {
				Ok(sum) => {
					$checksum = $checksum + sum
					$successes = $successes + 1
				}
				Err(sum) => {
					$checksum = $checksum + sum
				}
			}
		}
		elapsed = Utc.delta_as_nanos(Utc.now!(), start)
		Stdout.line!("{\"iterations\":${iterations.to_str()},\"elapsed_ns\":${elapsed.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()}}")?
		Ok({})
	}
}
