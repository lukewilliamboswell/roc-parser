## Compact already validated JSON without changing string or number tokens.
## Sorting or reserializing through a floating-point JSON runtime can change
## large integers; preserve the attestation's original data instead.
Attestations := [].{
	compact_valid_json : Str -> Str
	compact_valid_json = |source| {
		var $inside = False
		var $escaped = False
		var $out = []
		for byte in source.to_utf8() {
			if $inside {
				$out = $out.append(byte)
				if $escaped {
					$escaped = False
				}
					else if byte == '\\' {
						$escaped = True
					}
						else if byte == '"' {
							$inside = False
						}
			} else if byte == '"' {
				$inside = True
				$out = $out.append(byte)
			} else if ![32, 9, 10, 13].contains(byte) {
				$out = $out.append(byte)
			}
		}
		Str.from_utf8_lossy($out)
	}
}

expect Attestations.compact_valid_json("{\n  \"n\": 9007199254740993123456789, \"text\": \"a b\"\n}\n") == "{\"n\":9007199254740993123456789,\"text\":\"a b\"}"
expect Attestations.compact_valid_json("{ \"text\": \"quote: \\\"; slash: \\\\; newline: \\n\" }") == "{\"text\":\"quote: \\\"; slash: \\\\; newline: \\n\"}"
expect Attestations.compact_valid_json("{ \"non-ascii\": \"café\", \"payload\": \"YQ==\" }") == "{\"non-ascii\":\"café\",\"payload\":\"YQ==\"}"
