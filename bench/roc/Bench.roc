import cli.Monotonic
import cli.OsStr
import cli.Stdin
import cli.Stdout

## The timing loop shared by every format driver. It reads one document from
## stdin, parses it `iterations` times (the last argument), and prints one JSON
## line. Reading stdin and printing are outside the timed region.
##
## `parse` returns `Ok(checksum)` for a document it accepts and `Err(checksum)`
## for one it rejects; the checksum makes the result observable so the
## optimizer cannot drop the work.
Bench :: {}.{

	## Weight raw bytes directly, including byte fields that are not UTF-8.
	weighted_bytes : List(U8) -> U64
	weighted_bytes = |bytes| {
		var $index = 0.U64
		var $sum = 0.U64
		while $index < bytes.len() {
			$sum = $sum + ($index + 1) * (bytes.get($index) ?? 0).to_u64()
			$index = $index + 1
		}
		$sum
	}

	## Visit every UTF-8 byte with the same one-based weighting as the references.
	weighted_utf8 : Str -> U64
	weighted_utf8 = |text| {
		bytes = text.to_utf8()
		var $index = 0.U64
		var $sum = 0.U64
		while $index < bytes.len() {
			$sum = $sum + ($index + 1) * (bytes.get($index) ?? 0).to_u64()
			$index = $index + 1
		}
		$sum
	}

	run! : List(OsStr), (Str -> Try(U64, U64)) => Try({}, _)
	run! = |args, parse| {
		iterations = match args.last() {
			Ok(arg) => U64.from_str(OsStr.display(arg)) ?? 1
			Err(_) => 1
		}
		input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
		repetitions = List.repeat({}, iterations)
		start = Monotonic.now!()
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
		elapsed = start.elapsed!().to_nanos()
		Stdout.line!("{\"iterations\":${iterations.to_str()},\"elapsed_ns\":${elapsed.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()}}")?
		Ok({})
	}
}

expect Bench.weighted_bytes([0, 128, 255]) == 1021
