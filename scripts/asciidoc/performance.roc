app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Monotonic
import parser.AsciiDoc

Request : { id : Str, source : Str, includes : List({ path : Str, source : Str }), expand : Bool, iterations : U64, warmup : U64, mode : Str }

# Input decoding, options, warm-up, correctness projection and JSON encoding are
# outside the timer. Every timed iteration constructs and destroys a fresh AST.
main! : List(OsStr) => Try({}, _)
main! = |_| {
	var $running = True
	while $running {
		match Stdin.line!() {
			Err(EndOfFile) => {
				$running = False
			}
			Err(StdinErr(error)) => {
				return Err(StdinErr(error))
			}
			Ok(line) => {
				request : Try(Request, _)
				request = Json.parse(line)
				match request {
					Err(_) => Stdout.line!("{\"error\":\"invalid_request\"}")?
					Ok(item) => {
						if item.iterations > 100000 or item.warmup > 10000 or item.source.count_utf8_bytes() > 1048576 or !["parse", "render"].contains(item.mode) {
							Stdout.line!("{\"error\":\"invalid_benchmark_request\"}")?
						} else {
							options = { includes: item.includes, attributes: [], max_depth: 16, expand_includes: item.expand }
							var $i = 0
							var $checksum = 0.U64
							while $i < item.warmup {
								$checksum = $checksum + operation(item.source, options, item.mode)
								$i = $i + 1
							}
							$i = 0
							var $successes = 0.U64
							start = Monotonic.now!()
							while $i < item.iterations {
								count = operation(item.source, options, item.mode)
								$checksum = $checksum + count
								if count > 0 {
									$successes = $successes + 1
								}
								$i = $i + 1
							}
							elapsed_ns = Monotonic.now!() - start
							match AsciiDoc.parse_with(item.source, options) {
								Err(_) => Stdout.line!(Json.to_str({ error: "parse_failed", id: item.id }))?
								Ok(document) => {
									projection = "{\"title\":${json_string(document.title)},\"blocks\":${encode_blocks(document.blocks)}}"
									html = AsciiDoc.render(document)
									Stdout.line!("{\"version\":1,\"id\":${json_string(item.id)},\"iterations\":${item.iterations.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()},\"elapsed_ns\":${elapsed_ns.to_str()},\"projection\":${projection},\"html\":${json_string(html)}}")?
								}
							}
						}
					}
				}
			}
		}
	}
	Ok({})
}

operation : Str, { includes : List({ path : Str, source : Str }), attributes : List(AsciiDoc.Attribute), max_depth : U64, expand_includes : Bool }, Str -> U64
operation = |source, options, mode| match AsciiDoc.parse_with(source, options) {
	Ok(document) => if (mode == "render") AsciiDoc.render(document).count_utf8_bytes() + 1 else document.blocks.len() + document.title.count_utf8_bytes() + 1
	Err(_) => 0
}

encode_blocks : List(AsciiDoc.Block) -> Str
encode_blocks = |blocks| "[${
	Str.join_with(
		blocks.keep_if(
			|block| match block {
				Rich(n) => n.context != "__document_header"
				_ => True
			},
		).map(encode_block),
		",",
	)
}]"

encode_block : AsciiDoc.Block -> Str
encode_block = |block| {
	match block {
		Paragraph(source) => encode_leaf("paragraph", source)
		Listing(source) => encode_leaf("listing", source)
		Literal(source) => encode_leaf("literal", source)
		ThematicBreak => encode_leaf("thematic_break", "")
		Rich(node) => {
			context = node.context
			level = if (context == "section") node.level else 0
			source = if (["list_item", "image", "video", "audio", "table", "table"].contains(context)) "" else node.source
			title = if (context == "list_item") "" else AsciiDoc.render_inline(node.title, node.attrs)
			var $blocks = []
			for item in node.blocks {
				if context == "dlist" and has_definition(item) {
					$blocks = $blocks.append(AsciiDoc.empty_item)
				}
				$blocks = $blocks.append(item)
			}
			"{\"context\":${json_string(context)},\"level\":${level.to_str()},\"title\":${json_string(title)},\"source\":${json_string(source)},\"blocks\":${encode_blocks($blocks)}}"
		}
		Section({ level, title, blocks }) => "{\"context\":\"section\",\"level\":${level.to_str()},\"title\":${json_string(AsciiDoc.render_inline(title, []))},\"source\":\"\",\"blocks\":${encode_blocks(blocks)}}"
	}
}

encode_leaf : Str, Str -> Str
encode_leaf = |context, source| "{\"context\":${json_string(context)},\"level\":0,\"title\":\"\",\"source\":${json_string(source)},\"blocks\":[]}"

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, out| {
	match bytes {
		[] => out
		['"', .. as rest] => escape_json(rest, out.concat(['\\', '"']))
		['\\', .. as rest] => escape_json(rest, out.concat(['\\', '\\']))
		[byte, .. as rest] if byte < 32 => {
			hi = if (byte < 16) '0' else '1'
			lo = if (byte < 16) byte else byte - 16
			digit = if (lo < 10) lo + '0' else lo - 10 + 'a'
			escape_json(rest, out.concat(['\\', 'u', '0', '0', hi, digit]))
		}
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}

has_definition : AsciiDoc.Block -> Bool
has_definition = |block| match block {
	Rich(n) => n.source != "" or !n.blocks.is_empty()
	_ => False
}
