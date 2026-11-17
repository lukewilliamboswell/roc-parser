## Native target registration; generators live in the entries themselves.
FuzzPlan := [].{

	## Allocation-measurement programs used by benchmarks, never campaign targets.
	measurement_only = ["yaml-alloc", "markup-alloc"]
	Target : { name : Str, seeds : Str, encoding : Str, dictionary : Str, size : U64, timeout : U64, examples : Bool, scheduled : Bool }
	entries : List(Target)
	entries = [
		{ name: "asciidoc-raw", seeds: "fuzz/seeds/asciidoc.json", encoding: "raw", dictionary: "fuzz/dictionaries/asciidoc.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "asciidoc-structured", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-document", seeds: "fuzz/seeds/markdown-document.json", encoding: "raw", dictionary: "fuzz/dictionaries/markdown-document.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-blocks", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-refdefs", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline", seeds: "fuzz/seeds/markdown-inline.json", encoding: "raw", dictionary: "fuzz/dictionaries/markdown-inline.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline-ast", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline-pathological", seeds: "fuzz/seeds/markdown-inline-pathological.json", encoding: "raw", dictionary: "", size: 64, timeout: 10, examples: False, scheduled: True },
		{ name: "yaml", seeds: "fuzz/seeds/yaml.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-raw", seeds: "fuzz/seeds/yaml.json", encoding: "raw", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-source", seeds: "fuzz/seeds/yaml.json", encoding: "raw", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-roundtrip", seeds: "", encoding: "", dictionary: "", size: 512, timeout: 10, examples: False, scheduled: True },
		{ name: "yaml-block", seeds: "", encoding: "", dictionary: "", size: 64, timeout: 5, examples: False, scheduled: True },
		{ name: "xml", seeds: "fuzz/seeds/xml.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-raw", seeds: "fuzz/seeds/xml.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-roundtrip", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-malformed", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-reader", seeds: "fuzz/seeds/xml.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-stream", seeds: "fuzz/seeds/xml-stream.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 2048, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-stream-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "xml-limits", seeds: "fuzz/seeds/xml-limits.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-ceilings", seeds: "fuzz/seeds/xml-ceilings.json", encoding: "raw", dictionary: "", size: 2, timeout: 20, examples: True, scheduled: True },
		{ name: "xmlns-raw", seeds: "fuzz/seeds/xmlns.json", encoding: "raw", dictionary: "fuzz/dictionaries/xmlns.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xmlns-roundtrip", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "svg-path", seeds: "", encoding: "", dictionary: "", size: 64, timeout: 10, examples: False, scheduled: False },
		{ name: "svg-raw", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "mathml-raw", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "html-raw", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-source", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "dom-construction", seeds: "", encoding: "", dictionary: "", size: 12, timeout: 10, examples: True, scheduled: False },
		{ name: "markup-structured", seeds: "", encoding: "", dictionary: "", size: 128, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-limits", seeds: "", encoding: "", dictionary: "", size: 8, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-segments", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: True, scheduled: False },
		{ name: "json-raw", seeds: "fuzz/seeds/json.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "json-options", seeds: "fuzz/seeds/json-options.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "json-roundtrip", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "json-stream", seeds: "fuzz/seeds/json.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 1024, timeout: 10, examples: True, scheduled: True },
		{ name: "json-stream-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "ofx-raw", seeds: "fuzz/seeds/ofx.json", encoding: "raw", dictionary: "fuzz/dictionaries/ofx.dict", size: 8192, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-stream", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-roundtrip", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-malformed", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "qif-raw", seeds: "fuzz/seeds/qif.json", encoding: "raw", dictionary: "fuzz/dictionaries/qif.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-stream", seeds: "fuzz/seeds/qif.json", encoding: "raw", dictionary: "fuzz/dictionaries/qif.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-roundtrip", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-malformed", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "stream-contract", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "stream-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 5, examples: True, scheduled: True },
		{ name: "csv", seeds: "fuzz/seeds/csv.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "csv-raw", seeds: "fuzz/seeds/csv.json", encoding: "raw", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-roundtrip", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-decode", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-stream-raw", seeds: "fuzz/seeds/csv.json", encoding: "raw", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 10, examples: True, scheduled: True },
		{ name: "csv-stream-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "http-request", seeds: "fuzz/seeds/http-request.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/http-request.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-response", seeds: "fuzz/seeds/http-response.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/http-response.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-raw", seeds: "fuzz/seeds/http-request.json", encoding: "raw", dictionary: "fuzz/dictionaries/http-raw.dict", size: 65536, timeout: 10, examples: False, scheduled: True },
		{ name: "http-roundtrip", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-smuggling", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-stream", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: True, scheduled: True },
		{ name: "http-stream-ceilings", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "parser-combinators", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "validate", seeds: "fuzz/seeds/validate.json", encoding: "raw", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "diagnostic-lifetimes", seeds: "", encoding: "", dictionary: "", size: 2, timeout: 20, examples: True, scheduled: False },
		{ name: "string-primitives", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "byte-scan", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "roc-syntax-raw", seeds: "fuzz/seeds/roc-syntax.json", encoding: "raw", dictionary: "fuzz/dictionaries/roc-syntax.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-structured", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-spans", seeds: "fuzz/seeds/roc-syntax.json", encoding: "raw", dictionary: "fuzz/dictionaries/roc-syntax.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-span-model", seeds: "", encoding: "", dictionary: "", size: 512, timeout: 5, examples: False, scheduled: True },
	]
	select : List(Str), Bool -> Try(List(Target), [InvalidSelection(List(Str)), UnknownTarget(Str), EmptySelection])
	select = |names, ci| {
		requested = if names.is_empty() or names == ["all"] entries.map(|target| target.name) else names
		if requested.contains("all") {
			return Err(InvalidSelection(requested))
		}
		var $chosen = []
		for name in requested {
			target = entries.find_first(|candidate| candidate.name == name).map_err(|_| UnknownTarget(name))?
			if (!ci or target.scheduled) and !$chosen.any(|item| item.name == name) {
				$chosen = $chosen.append(target)
			}
		}
		if $chosen.is_empty() {
			Err(EmptySelection)
		} else {
			Ok($chosen)
		}
	}
	encode : Str, Str -> Try(List(U8), [SeedTooLong(U64)])
	encode = |text, encoding| {
		bytes = text.to_utf8()
		if encoding != "fuzz-str" {
			return Ok(bytes)
		}
		if bytes.len() > 255 {
			return Err(SeedTooLong(bytes.len()))
		}
		Ok(bytes.append(bytes.len().to_u8_wrap()))
	}
}
expect FuzzPlan.select(["all", "csv"], False).is_err()
expect FuzzPlan.select(["csv", "csv"], False).map_ok(|items| items.len()) == Ok(1)
expect {
	var $valid = True
	for size in List.repeat({}, 256).map_with_index(|_, index| index) {
		text = Str.repeat("a", size)
		$valid = $valid and FuzzPlan.encode(text, "fuzz-str") == Ok(text.to_utf8().append(size.to_u8_wrap()))
	}
	$valid and FuzzPlan.encode(Str.repeat("a", 256), "fuzz-str").is_err()
}
