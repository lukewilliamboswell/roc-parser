app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import parser.Parser
import fuzz.Fuzz
import parser.HTTP

## Source chunks are constructed on demand inside the measured region, and
## events are discarded immediately. Neither source nor result is precollected.
drain : U64, Bool -> U64
drain = |n, bodies| {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| {
			if bodies and index == 0 {
				Ok(("POST / HTTP/1.1\r\nHost: a\r\nContent-Length: ${(n * 1024).to_str()}\r\n\r\n".to_utf8(), 1))
			} else if index <= n and bodies {
				Ok((Str.repeat(if (index % 2 == 0) "a" else "b", 1024).to_utf8(), index + 1))
			} else if !bodies and index < n {
				Ok(("GET /${index.to_str()} HTTP/1.1\r\nHost: a\r\n\r\n".to_utf8(), index + 1))
			} else Err(NoMore)
		},
	)
	var $count = 0.U64
	var $finished = False
	for result in (HTTP.Request.decoder({})).read_iter(chunks) {
		match result {
			Ok(Value(Body(bytes))) => {
				$count = $count + bytes.len()
			}
			Ok(Value(MessageEnd)) if !bodies => {
				$count = $count + 1
			}
			Ok(Finished(_)) => {
				$finished = True
			}
			Ok(Value(_)) => {}
			_ => crash "generated HTTP stream was rejected"
		}
	}
	if !$finished {
		crash "HTTP stream omitted source completion"
	}
	$count
}

# Long metadata and dense header lists exercise both direct complete heads
# and fragmented buffer growth. Budgets scale with retained metadata, not body.
drain_metadata : U64, Bool, Bool -> U64
drain_metadata = |n, many, fragmented| {
	wire = if many {
		"GET / HTTP/1.1\r\nHost: a\r\n${Str.repeat("X: x\r\n", n)}\r\n".to_utf8()
	} else {
		"GET / HTTP/1.1\r\nHost: a\r\nX: ${Str.repeat("x", n)}\r\n\r\n".to_utf8()
	}
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|at| {
			if (at >= wire.len()) Err(NoMore) else {
				len = if (fragmented) 1 else wire.len()
				Ok((wire.sublist({ start: at, len }), at + len))
			}
		},
	)
	var $headers = 0.U64
	for result in (HTTP.Request.decoder({ max_head_bytes: 262_144, max_line_bytes: 65_536, max_headers: 16_384 })).read_iter(chunks) {
		match result {
			Ok(Value(RequestHead(head))) => {
				$headers = $headers + head.headers.len()
			}
			Ok(_) => {}
			_ => crash "generated metadata rejected"
		}
	}
	$headers
}

test! = |bytes| {
	choice = (bytes.first() ?? 0).to_u64()
	if choice >= 8 {
		n = [256.U64, 1024, 4096, 8192].get(choice // 2 % 4) ?? 256
		many = choice % 2 == 1
		for fragmented in [False, True] {
			measured = Fuzz.measure!("http-metadata", || drain_metadata(n, many, fragmented))
			if measured.value != (if (many) n + 1 else 2) {
				crash "HTTP metadata lost headers"
			}
			held = measured.stats.peak_live_bytes.minus_saturated(measured.stats.entry_live_bytes)
			if held > 65_536 + (if (many) 256 else 8) * n {
				crash "HTTP metadata retained beyond its linear ceiling"
			}
			if measured.stats.requested_bytes > 65_536 + (if (many) 512 else 32) * n {
				crash "HTTP metadata allocation traffic exceeded its linear ceiling"
			}
		}
		return Fuzz.keep
	}
	n = [1.U64, 32, 256, 2048].get(choice // 2 % 4) ?? 1
	bodies = choice % 2 == 0
	measured = Fuzz.measure!("http-stream", || drain(n, bodies))
	if measured.value != (if (bodies) n * 1024 else n) {
		crash "HTTP stream lost body bytes or messages"
	}
	held = measured.stats.peak_live_bytes.minus_saturated(measured.stats.entry_live_bytes)
	if held > 262_144 {
		crash "HTTP stream retained completed input"
	}
	if measured.stats.requested_bytes > 65_536 + 32_768 * n {
		crash "HTTP allocation traffic exceeded its linear ceiling"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "http-stream-ceilings", examples: [Fuzz.example({ name: "small-body", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large-body", revision: "1", make: || [6.U8] }), Fuzz.example({ name: "large-pipeline", revision: "1", make: || [7.U8] }), Fuzz.example({ name: "large-header", revision: "1", make: || [14.U8] }), Fuzz.example({ name: "many-headers", revision: "1", make: || [15.U8] })], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
