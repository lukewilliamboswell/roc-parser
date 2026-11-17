app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}

import cli.Stdin
import cli.Stdout
import parser.XmlNs
import parser.Streaming

## Count Atom entries from stdin without collecting the input or a tree.
## Names are matched by namespace, regardless of the source's chosen prefix.
main! = |_| {
	# tag::count[]
	var $entries = 0.U64
	for! result in Streaming.read_stream(Stdin.chunks(), XmlNs.decoder({ max_depth: 128 })) {
		match result? {
			Value(Start({ name: { namespace: Uri("http://www.w3.org/2005/Atom"), local: "entry", .. }, .. })) => {
				$entries = $entries + 1
			}
			Finished(_) => Stdout.line!("${$entries.to_str()} entries")?
			_ => {}
		}
	}
	# end::count[]
	Ok({})
}
