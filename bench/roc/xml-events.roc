app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Xml
import parser.Streaming
import Bench

## Consume validated events without constructing a tree. Chunked mode uses
## source slices of the requested byte width (default 4 KiB). Both modes
## coalesce adjacent text for the same checksum as xml.roc's tree traversal,
## including the text-node count.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	mode = args.first().map_ok(OsStr.display) ?? "chunked"
	width = if (args.len() >= 3) U64.from_str(args.get(1).map_ok(OsStr.display) ?? "") ?? 4096 else 4096
	Bench.run!(args, if (mode == "reader") read else if (mode == "direct") |input| direct(input, width) else |input| chunked(input, width))
}

consume : { sum : U64, text : Bool }, Xml.Event -> { sum : U64, text : Bool }
consume = |state, event| match event {
	Start(tag) => {
		attributes = tag.attributes.fold(0.U64, |sum, attr| sum + attr.name.count_utf8_bytes() + attr.value.count_utf8_bytes())
		{ sum: state.sum + tag.name.count_utf8_bytes() + attributes, text: False }
	}
	End(_) => { ..state, text: False }
	Text(text) => { sum: state.sum + text.count_utf8_bytes() + (if (state.text) 0 else 1), text: True }
}

read : Str -> Try(U64, U64)
read = |input| {
	var $reader = Xml.reader(input, {}).map_err(|_| 0.U64)?
	var $state = { sum: 0.U64, text: False }
	while True {
		match $reader.next() {
			Event(event, rest) => {
				$reader = rest
				$state = consume($state, event)
			}
			Done => return Ok($state.sum)
			Failed(_) => return Err(0)
		}
	}
}

chunked : Str, U64 -> Try(U64, U64)
chunked = |input, width| {
	if width == 0 {
		return Err(0)
	}
	bytes = input.to_utf8()
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|at| {
			if (at >= bytes.len()) Err(NoMore) else {
				len = U64.min(width, bytes.len() - at)
				Ok((bytes.sublist({ start: at, len }), at + len))
			}
		},
	)
	var $state = { sum: 0.U64, text: False }
	for result in Streaming.read_iter(chunks, Xml.decoder({})) {
		match result {
			Ok(Value(event)) => {
				$state = consume($state, event)
			}
			Ok(Finished(_)) => return Ok($state.sum)
			_ => return Err(0)
		}
	}
	Err(0)
}

## Diagnostic pure-step loop with the same chunk width and event checksum.
## It keeps decoder ownership outside the source cursor used by Streaming.read_iter.
direct : Str, U64 -> Try(U64, U64)
direct = |input, width| {
	if width == 0 {
		return Err(0)
	}
	bytes = input.to_utf8()
	var $decoder = Xml.decoder({})
	var $state = { sum: 0.U64, text: False }
	var $at = 0.U64
	var $stop = U64.min(width, bytes.len())
	var $final = False
	var $emitted = 0.U64
	while True {
		next = ($decoder).step(bytes.sublist({ start: $at, len: $stop - $at }), if ($final) Final else More)
		if next.consumed > $stop - $at {
			return Err(0)
		}
		$decoder = next.state
		$at = $at + next.consumed
		match next.outcome {
			Item(event) => {
				$state = consume($state, event)
				$emitted = $emitted + 1
			}
			NeedInput => {
				if $final or $at != $stop {
					return Err(0)
				}
				$final = $stop == bytes.len()
				$stop = $stop + U64.min(width, bytes.len() - $stop)
			}
			Done => return if ($final and $at == bytes.len() and $emitted >= 2) Ok($state.sum) else Err(0)
			_ => return Err(0)
		}
	}
}
