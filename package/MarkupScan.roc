## Internal byte scanners. They preserve source bytes and return byte offsets.
## Checked vector loads handle full blocks; scalar tails never read past input.
## A start at or beyond the end is returned unchanged without reading input.
MarkupScan :: [].{
	HtmlData : { end : U64, normalize : Bool }

	## Find '<'; normalization is needed only for '&', CR or NUL before it.
	html_data : List(U8), U64 -> HtmlData
	html_data = |bytes, start| {
		var $pos = start
		var $normalize = False
		while $pos < bytes.len().minus_saturated(15) {
			word = U8x16.load(bytes, $pos) ?? crash "guarded vector load"
			lt = word.eq_lanes(U8x16.splat('<')).to_bitmask()
			norm = word.eq_lanes(U8x16.splat('&')).bitwise_or(word.eq_lanes(U8x16.splat('\r'))).bitwise_or(word.eq_lanes(U8x16.splat(0))).to_bitmask()
			if lt != 0 {
				lane = lt.count_trailing_zero_bits()
				prefix = (1.U16).shl_wrap(lane).minus_wrap(1)
				return { end: $pos + lane.to_u64(), normalize: $normalize or norm.bitwise_and(prefix) != 0 }
			}
			$normalize = $normalize or norm != 0
			$pos = $pos + 16
		}
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			if byte == '<' {
				break
			}
			if byte == '&' or byte == '\r' or byte == 0 {
				$normalize = True
			}
			$pos = $pos + 1
		}
		{ end: $pos, normalize: $normalize }
	}

	## Skip printable ASCII 0x20..0x7e; callers handle controls and Unicode.
	printable_ascii : List(U8), U64 -> U64
	printable_ascii = |bytes, start| {
		var $pos = start
		while $pos < bytes.len().minus_saturated(15) {
			word = U8x16.load(bytes, $pos) ?? crash "guarded vector load"
			mask = word.lt_lanes(U8x16.splat(32)).bitwise_or(word.gte_lanes(U8x16.splat(127))).to_bitmask()
			if mask != 0 {
				return $pos + mask.count_trailing_zero_bits().to_u64()
			}
			$pos = $pos + 16
		}
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			if byte < 32 or byte >= 127 {
				break
			}
			$pos = $pos + 1
		}
		$pos
	}

	## XML character data also stops before entity/tag syntax and possible `]]>`.
	xml_text : List(U8), U64 -> U64
	xml_text = |bytes, start| {
		var $pos = start
		while $pos < bytes.len().minus_saturated(15) {
			word = U8x16.load(bytes, $pos) ?? crash "guarded vector load"
			mask = word.lt_lanes(U8x16.splat(32)).bitwise_or(word.gte_lanes(U8x16.splat(127))).bitwise_or(word.eq_lanes(U8x16.splat('<'))).bitwise_or(word.eq_lanes(U8x16.splat('&'))).bitwise_or(word.eq_lanes(U8x16.splat(']'))).to_bitmask()
			if mask != 0 {
				return $pos + mask.count_trailing_zero_bits().to_u64()
			}
			$pos = $pos + 16
		}
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			if byte < 32 or byte >= 127 or byte == '<' or byte == '&' or byte == ']' {
				break
			}
			$pos = $pos + 1
		}
		$pos
	}

	## CDATA allows < and &, and stops before a possible closing `]]>`.
	cdata_text : List(U8), U64 -> U64
	cdata_text = |bytes, start| {
		var $pos = start
		while $pos < bytes.len().minus_saturated(15) {
			word = U8x16.load(bytes, $pos) ?? crash "guarded vector load"
			mask = word.lt_lanes(U8x16.splat(32)).bitwise_or(word.gte_lanes(U8x16.splat(127))).bitwise_or(word.eq_lanes(U8x16.splat(']'))).to_bitmask()
			if mask != 0 {
				return $pos + mask.count_trailing_zero_bits().to_u64()
			}
			$pos = $pos + 16
		}
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			if byte < 32 or byte >= 127 or byte == ']' {
				break
			}
			$pos = $pos + 1
		}
		$pos
	}
}

model_html : List(U8), U64 -> MarkupScan.HtmlData
model_html = |bytes, start| {
	var $end = start
	var $normalize = False
	while $end < bytes.len() {
		byte = bytes.get($end) ?? 0
		if byte == '<' {
			break
		}
		if byte == '&' or byte == '\r' or byte == 0 {
			$normalize = True
		}
		$end = $end + 1
	}
	{ end: $end, normalize: $normalize }
}

model_end : List(U8), U64, [Ascii, Xml, Cdata] -> U64
model_end = |bytes, start, mode| {
	var $end = start
	while $end < bytes.len() {
		byte = bytes.get($end) ?? 0
		if byte < 32 or byte >= 127 {
			break
		}
		if mode != Ascii and byte == ']' {
			break
		}
		if mode == Xml and (byte == '<' or byte == '&') {
			break
		}
		$end = $end + 1
	}
	$end
}

models_agree = |bytes, start| {
	MarkupScan.html_data(bytes, start) == model_html(bytes, start)
		and MarkupScan.printable_ascii(bytes, start) == model_end(bytes, start, Ascii)
		and MarkupScan.xml_text(bytes, start) == model_end(bytes, start, Xml)
		and MarkupScan.cdata_text(bytes, start) == model_end(bytes, start, Cdata)
}

# Every byte value, including invalid UTF-8, at every vector lane and tail.
expect {
	var $valid = True
	var $byte = 0.U16
	while $byte < 256 {
		var $index = 0.U64
		while $index < 33 {
			bytes = List.repeat(65.U8, 33).set($index, $byte.to_u8_wrap()) ?? []
			var $start = 0.U64
			while $start < 18 {
				$valid = $valid and models_agree(bytes, $start)
				$start = $start + 1
			}
			$index = $index + 1
		}
		$byte = $byte + 1
	}
	$valid
}

# Empty/short input, unaligned starts, all tails and out-of-range starts.
expect {
	var $valid = True
	var $len = 0.U64
	while $len < 50 {
		bytes = List.repeat(65.U8, $len)
		var $start = 0.U64
		while $start <= $len + 1 {
			$valid = $valid and models_agree(bytes, $start)
			$start = $start + 1
		}
		$valid = $valid and models_agree(bytes, U64.highest)
		$len = $len + 1
	}
	$valid
}

# Normalization after '<' must not affect the preceding text run.
expect {
	var $valid = True
	for byte in ['&', '\r', 0.U8] {
		var $lt = 0.U64
		while $lt < 34 {
			var $normal = 0.U64
			while $normal < 34 {
				base = List.repeat(65.U8, 34).set($lt, '<') ?? []
				bytes = base.set($normal, byte) ?? []
				$valid = $valid and models_agree(bytes, 0) and models_agree(bytes, 1)
				$normal = $normal + 1
			}
			$lt = $lt + 1
		}
	}
	$valid
}

CheckedScan := {}.{
	from_quote : Str -> Try(CheckedScan, [BadQuotedBytes(Str)])
	from_quote = |source| if models_agree(source.to_utf8(), 0) {
		Ok({})
	} else {
		Err(BadQuotedBytes("byte scanner disagreement"))
	}
	passed : CheckedScan -> Bool
	passed = |_| True
}

# Checked literals evaluate the same vector operations during compilation.
expect {
	before : CheckedScan
	before = "ordinary prose &amp; more < after text ]]> \r"
	after : CheckedScan
	after = "ordinary prose < after &amp; more text ]]> \u(0)"
	unicode : CheckedScan
	unicode = "a café welcomes λ and 東京 < after text"
	before.passed() and after.passed() and unicode.passed()
}
