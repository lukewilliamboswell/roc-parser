app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.File
import cli.Monotonic
import cli.OsStr
import cli.Path
import cli.Stdin
import cli.Stdout
import parser.CSV

## Read a real file (argument 1), or stdin, with bounded source buffers.
## Timing includes reads. The process never preloads or retains the document.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	start = Monotonic.now!()
	result = match args.get(0) {
		Ok(path) => {
			reader = File.open_reader!(Path.from_os_str(path))?
			consume!(reader.chunks(), start)
		}
		Err(_) => consume!(Stdin.chunks(), start)
	}
	elapsed = start.elapsed!().to_nanos()
	match result {
		Ok({ checksum, records, first_item_ns }) => Stdout.line!("{\"elapsed_ns\":${elapsed.to_str()},\"records\":${records.to_str()},\"first_item_ns\":${first_item_ns.to_str()},\"checksum\":${checksum.to_str()}}")?
		Err(_) => return Err(InvalidInput)
	}
	Ok({})
}

consume! : Stream(Try(List(U8), source_error)), Monotonic => Try({ checksum : U64, records : U64, first_item_ns : U64 }, _)
consume! = |chunks, start| {
	var $first_item_ns = 0.U64
	var $seen = False
	var $sum = 0.U64
	for! event in CSV.stream_records(chunks, {}, {}) {
		match event? {
			Value(row) => {
				if !$seen {
					$first_item_ns = start.elapsed!().to_nanos()
					$seen = True
				}
				$sum = row.fold($sum + 1, |total, field| field.iter().with_index().fold(total, |weighted, pair| weighted + (pair.0 + 1) * pair.1.to_u64()))
			}
			Finished(report) => return Ok({ checksum: $sum, records: report.emitted_items, first_item_ns: $first_item_ns })
			Rejected(_) => return Err(InvalidInput)
		}
	}
	Err(MissingFinished)
}
