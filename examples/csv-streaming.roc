app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}

import cli.File
import cli.OsStr
import cli.Path
import cli.Stdin
import parser.CSV
import parser.Streaming

## Read stdin or an input file and write canonical CSV to argument 1.
## Usage: roc csv-streaming.roc output.csv [input.csv]
## Both source reads and output chunks are bounded. A parse/source failure
## stops the write; bytes for preceding complete records may already exist.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	destination = args.get(0).map_ok(Path.from_os_str).map_err(|_| MissingOutputPath)?
	match args.get(1) {
		Ok(path) => {
			source = Path.from_os_str(path)
			if source == destination {
				return Err(SameInputAndOutput)
			}
			reader = File.open_reader!(source)?
			write!(destination, reader.chunks())
		}
		Err(_) => write!(destination, Stdin.chunks())
	}
}

write! : Path, Stream(Try(List(U8), source_error)) => Try({}, _)
write! = |destination, chunks| {
	events = CSV.stream_records(chunks, {}, {})
	encoded = Streaming.encode_stream(
		events,
		|event| match event {
			Value(row) => CSV.encode_record(row, {})
			Finished(_) => Ok([])
			Rejected(_) => Err(InvalidRecord("a record was rejected"))
		},
		65_536,
	)
	File.with_writer!(destination, Truncate, {}, |writer| writer.write_stream!(encoded))
}
