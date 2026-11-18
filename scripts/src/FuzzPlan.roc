import "../../fuzz/seeds/asciidoc.json" as asciidoc_seeds : Str
import "../../fuzz/seeds/csv.json" as csv_seeds : Str
import "../../fuzz/seeds/http-request.json" as http_request_seeds : Str
import "../../fuzz/seeds/http-response.json" as http_response_seeds : Str
import "../../fuzz/seeds/json-options.json" as json_options_seeds : Str
import "../../fuzz/seeds/json.json" as json_seeds : Str
import "../../fuzz/seeds/markdown-document.json" as markdown_document_seeds : Str
import "../../fuzz/seeds/markdown-inline-pathological.json" as markdown_inline_pathological_seeds : Str
import "../../fuzz/seeds/markdown-inline.json" as markdown_inline_seeds : Str
import "../../fuzz/seeds/markup.json" as markup_seeds : Str
import "../../fuzz/seeds/ofx.json" as ofx_seeds : Str
import "../../fuzz/seeds/qif.json" as qif_seeds : Str
import "../../fuzz/seeds/roc-syntax.json" as roc_syntax_seeds : Str
import "../../fuzz/seeds/validate.json" as validate_seeds : Str
import "../../fuzz/seeds/xml-ceilings.json" as xml_ceilings_seeds : Str
import "../../fuzz/seeds/xml-limits.json" as xml_limits_seeds : Str
import "../../fuzz/seeds/xml-stream.json" as xml_stream_seeds : Str
import "../../fuzz/seeds/xml.json" as xml_seeds : Str
import "../../fuzz/seeds/xmlns.json" as xmlns_seeds : Str
import "../../fuzz/seeds/yaml.json" as yaml_seeds : Str

## Native target registration; generators live in the entries themselves.
FuzzPlan := [].{

	## Allocation-measurement programs used by benchmarks, never campaign targets.
	measurement_only = ["yaml-alloc", "markup-alloc"]
	Target : { name : Str, family : Str, seeds : Str, encoding : Str, dictionary : Str, size : U64, timeout : U64, examples : Bool, scheduled : Bool }
	entries : List(Target)
	entries = [
		{ name: "asciidoc-raw", family: "Text", seeds: "fuzz/seeds/asciidoc.json", encoding: "raw", dictionary: "fuzz/dictionaries/asciidoc.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "asciidoc-structured", family: "Text", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-document", family: "Text", seeds: "fuzz/seeds/markdown-document.json", encoding: "raw", dictionary: "fuzz/dictionaries/markdown-document.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-blocks", family: "Text", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-refdefs", family: "Text", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline", family: "Text", seeds: "fuzz/seeds/markdown-inline.json", encoding: "raw", dictionary: "fuzz/dictionaries/markdown-inline.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline-ast", family: "Text", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "markdown-inline-pathological", family: "Text", seeds: "fuzz/seeds/markdown-inline-pathological.json", encoding: "raw", dictionary: "", size: 64, timeout: 10, examples: False, scheduled: True },
		{ name: "yaml", family: "Text", seeds: "fuzz/seeds/yaml.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-raw", family: "Text", seeds: "fuzz/seeds/yaml.json", encoding: "raw", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-source", family: "Text", seeds: "fuzz/seeds/yaml.json", encoding: "raw", dictionary: "fuzz/dictionaries/yaml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "yaml-roundtrip", family: "Text", seeds: "", encoding: "", dictionary: "", size: 512, timeout: 10, examples: False, scheduled: True },
		{ name: "yaml-block", family: "Text", seeds: "", encoding: "", dictionary: "", size: 64, timeout: 5, examples: False, scheduled: True },
		{ name: "xml", family: "Xml", seeds: "fuzz/seeds/xml.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-raw", family: "Xml", seeds: "fuzz/seeds/xml.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-roundtrip", family: "Xml", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-malformed", family: "Xml", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-reader", family: "Xml", seeds: "fuzz/seeds/xml.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-stream", family: "Xml", seeds: "fuzz/seeds/xml-stream.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 2048, timeout: 10, examples: False, scheduled: True },
		{ name: "xml-stream-ceilings", family: "Xml", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "xml-limits", family: "Xml", seeds: "fuzz/seeds/xml-limits.json", encoding: "raw", dictionary: "fuzz/dictionaries/xml.dict", size: 1024, timeout: 5, examples: False, scheduled: True },
		{ name: "xml-ceilings", family: "Xml", seeds: "fuzz/seeds/xml-ceilings.json", encoding: "raw", dictionary: "", size: 2, timeout: 20, examples: True, scheduled: True },
		{ name: "xmlns-raw", family: "Xml", seeds: "fuzz/seeds/xmlns.json", encoding: "raw", dictionary: "fuzz/dictionaries/xmlns.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "xmlns-roundtrip", family: "Xml", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "svg-path", family: "Markup", seeds: "", encoding: "", dictionary: "", size: 64, timeout: 10, examples: False, scheduled: False },
		{ name: "svg-raw", family: "Markup", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "mathml-raw", family: "Markup", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "html-raw", family: "Markup", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-source", family: "Markup", seeds: "fuzz/seeds/markup.json", encoding: "raw", dictionary: "fuzz/dictionaries/markup.dict", size: 4096, timeout: 10, examples: False, scheduled: False },
		{ name: "dom-construction", family: "Markup", seeds: "", encoding: "", dictionary: "", size: 12, timeout: 10, examples: True, scheduled: False },
		{ name: "markup-structured", family: "Markup", seeds: "", encoding: "", dictionary: "", size: 128, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-limits", family: "Markup", seeds: "", encoding: "", dictionary: "", size: 8, timeout: 10, examples: False, scheduled: False },
		{ name: "markup-segments", family: "Markup", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: True, scheduled: False },
		{ name: "json-raw", family: "Json", seeds: "fuzz/seeds/json.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "json-options", family: "Json", seeds: "fuzz/seeds/json-options.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "json-roundtrip", family: "Json", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "json-stream", family: "Json", seeds: "fuzz/seeds/json.json", encoding: "raw", dictionary: "fuzz/dictionaries/json.dict", size: 1024, timeout: 10, examples: True, scheduled: True },
		{ name: "json-stream-ceilings", family: "Json", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "ofx-raw", family: "Records", seeds: "fuzz/seeds/ofx.json", encoding: "raw", dictionary: "fuzz/dictionaries/ofx.dict", size: 8192, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-stream", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-roundtrip", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-malformed", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "ofx-ceilings", family: "Records", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "qif-raw", family: "Records", seeds: "fuzz/seeds/qif.json", encoding: "raw", dictionary: "fuzz/dictionaries/qif.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-stream", family: "Records", seeds: "fuzz/seeds/qif.json", encoding: "raw", dictionary: "fuzz/dictionaries/qif.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-roundtrip", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-malformed", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "qif-ceilings", family: "Records", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "stream-contract", family: "Records", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "stream-ceilings", family: "Records", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 5, examples: True, scheduled: True },
		{ name: "csv", family: "Records", seeds: "fuzz/seeds/csv.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "csv-raw", family: "Records", seeds: "fuzz/seeds/csv.json", encoding: "raw", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-roundtrip", family: "Records", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-decode", family: "Records", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "csv-stream-raw", family: "Records", seeds: "fuzz/seeds/csv.json", encoding: "raw", dictionary: "fuzz/dictionaries/csv.dict", size: 4096, timeout: 10, examples: True, scheduled: True },
		{ name: "csv-stream-ceilings", family: "CsvRetention", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "http-request", family: "Http", seeds: "fuzz/seeds/http-request.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/http-request.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-response", family: "Http", seeds: "fuzz/seeds/http-response.json", encoding: "fuzz-str", dictionary: "fuzz/dictionaries/http-response.dict", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-raw", family: "Http", seeds: "fuzz/seeds/http-request.json", encoding: "raw", dictionary: "fuzz/dictionaries/http-raw.dict", size: 65536, timeout: 10, examples: False, scheduled: True },
		{ name: "http-roundtrip", family: "Http", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-smuggling", family: "Http", seeds: "", encoding: "", dictionary: "", size: 4096, timeout: 10, examples: False, scheduled: True },
		{ name: "http-stream", family: "Http", seeds: "", encoding: "", dictionary: "", size: 1024, timeout: 10, examples: True, scheduled: True },
		{ name: "http-stream-ceilings", family: "Http", seeds: "", encoding: "", dictionary: "", size: 1, timeout: 20, examples: True, scheduled: True },
		{ name: "parser-combinators", family: "Core", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "validate", family: "Core", seeds: "fuzz/seeds/validate.json", encoding: "raw", dictionary: "", size: 1024, timeout: 10, examples: False, scheduled: True },
		{ name: "diagnostic-lifetimes", family: "Diagnostics", seeds: "", encoding: "", dictionary: "", size: 2, timeout: 20, examples: True, scheduled: False },
		{ name: "string-primitives", family: "Core", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "byte-scan", family: "Core", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 10, examples: False, scheduled: True },
		{ name: "roc-syntax-raw", family: "Core", seeds: "fuzz/seeds/roc-syntax.json", encoding: "raw", dictionary: "fuzz/dictionaries/roc-syntax.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-structured", family: "Core", seeds: "", encoding: "", dictionary: "", size: 256, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-spans", family: "Core", seeds: "fuzz/seeds/roc-syntax.json", encoding: "raw", dictionary: "fuzz/dictionaries/roc-syntax.dict", size: 4096, timeout: 5, examples: False, scheduled: True },
		{ name: "roc-syntax-span-model", family: "Core", seeds: "", encoding: "", dictionary: "", size: 512, timeout: 5, examples: False, scheduled: True },
	]

	## Seed JSON and encoding bounds are checked while compiling the runner.
	seed_values : Str -> List(Str)
	seed_values = |name| seed_corpora.find_first(|entry| entry.name == name).map_ok(|entry| entry.values) ?? crash "unregistered seed target: ${name}"
	select : List(Str), Bool -> Try(List(Target), [InvalidSelection(List(Str)), UnknownTarget(Str), EmptySelection])
	select = |names, ci| {
		requested = if (names.is_empty() or names == ["all"]) entries.map(|target| target.name) else names
		if requested.contains("all") {
			return Err(InvalidSelection(requested))
		}
		var $chosen = []
		for name in requested {
			members = entries.keep_if(|candidate| candidate.name == name or candidate.family == name)
			if members.is_empty() {
				return Err(UnknownTarget(name))
			}
			for target in members {
				if (!ci or target.scheduled) and !$chosen.any(|item| item.name == target.name) {
					$chosen = $chosen.append(target)
				}
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
expect FuzzPlan.select(["Xml", "xml-stream"], False).map_ok(|items| items.map(|entry| entry.name)) == FuzzPlan.select(["Xml"], False).map_ok(|items| items.map(|entry| entry.name))
expect {
	var $valid = True
	for size in List.repeat({}, 256).map_with_index(|_, index| index) {
		text = Str.repeat("a", size)
		$valid = $valid and FuzzPlan.encode(text, "fuzz-str") == Ok(text.to_utf8().append(size.to_u8_wrap()))
	}
	$valid and FuzzPlan.encode(Str.repeat("a", 256), "fuzz-str").is_err()
}

seed_sources : List({ path : Str, source : Str })
seed_sources = [
	{ path: "fuzz/seeds/asciidoc.json", source: asciidoc_seeds },
	{ path: "fuzz/seeds/csv.json", source: csv_seeds },
	{ path: "fuzz/seeds/http-request.json", source: http_request_seeds },
	{ path: "fuzz/seeds/http-response.json", source: http_response_seeds },
	{ path: "fuzz/seeds/json-options.json", source: json_options_seeds },
	{ path: "fuzz/seeds/json.json", source: json_seeds },
	{ path: "fuzz/seeds/markdown-document.json", source: markdown_document_seeds },
	{ path: "fuzz/seeds/markdown-inline-pathological.json", source: markdown_inline_pathological_seeds },
	{ path: "fuzz/seeds/markdown-inline.json", source: markdown_inline_seeds },
	{ path: "fuzz/seeds/markup.json", source: markup_seeds },
	{ path: "fuzz/seeds/ofx.json", source: ofx_seeds },
	{ path: "fuzz/seeds/qif.json", source: qif_seeds },
	{ path: "fuzz/seeds/roc-syntax.json", source: roc_syntax_seeds },
	{ path: "fuzz/seeds/validate.json", source: validate_seeds },
	{ path: "fuzz/seeds/xml-ceilings.json", source: xml_ceilings_seeds },
	{ path: "fuzz/seeds/xml-limits.json", source: xml_limits_seeds },
	{ path: "fuzz/seeds/xml-stream.json", source: xml_stream_seeds },
	{ path: "fuzz/seeds/xml.json", source: xml_seeds },
	{ path: "fuzz/seeds/xmlns.json", source: xmlns_seeds },
	{ path: "fuzz/seeds/yaml.json", source: yaml_seeds },
]

seed_corpora : List({ name : Str, values : List(Str) })
seed_corpora = FuzzPlan.entries.map(
	|target| {
		values : List(Str)
		values = if (target.seeds == "") [] else {
			source = seed_sources.find_first(|entry| entry.path == target.seeds).map_ok(|entry| entry.source) ?? crash "unregistered seed file: ${target.seeds}"
			Json.parse(source) ?? crash "${target.seeds}: expected a JSON array of seed strings"
		}
		for seed in values {
			_ = FuzzPlan.encode(seed, target.encoding) ?? crash "${target.seeds}: seed exceeds the ${target.encoding} encoding bound"
		}
		{ name: target.name, values }
	},
)
