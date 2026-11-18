app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import parser.HTTP
import Bench

## Parse every complete request in a buffered pipeline, counting all results.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(
	args,
	|input| {
		match HTTP.parse_requests(input.to_utf8(), {}) {
			Ok(requests) => Ok(requests.fold(0.U64, |sum, request| sum + request.headers.len() + request.body.len() + request.target.count_utf8_bytes()))
			Err(_) => Err(1)
		}
	},
)
