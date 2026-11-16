app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import cli.Stdin
import cli.Stdout
import parser.QIF
import parser.Streaming

## Canonicalize QIF from stdin to stdout without retaining the document.
## Full success requires consuming the parser's Finished event.
values : Stream(Try(Streaming.ReadEvent(item, record_error), failure)) -> Stream(Try(item, [Source(failure), Rejected(record_error)]))
values = |source| Stream.custom(
	Reading(source),
	Err(Unknown),
	|state| match state {
		Stopped => Err(NoMore)
		Reading(start) => {
			var $source = start
			while True {
				match $source.next!() {
					Done => return Err(NoMore)
					Skip({ rest }) => {
						$source = rest
					}
					One({ item, rest }) => {
						$source = rest
						match item {
							Ok(Value(value)) => return Ok((Ok(value), Reading($source)))
							Ok(Finished(_)) => return Err(NoMore)
							Ok(Rejected(error)) => return Ok((Err(Rejected(error)), Stopped))
							Err(error) => return Ok((Err(Source(error)), Stopped))
						}
					}
				}
			}
		}
	},
)

main! = |_| {
	parsed = QIF.parse_stream(Stdin.chunks(), {})
	for! chunk in QIF.write_stream(values(parsed), {}) {
		Stdout.write_bytes!(chunk?)?
	}
	Ok({})
}
