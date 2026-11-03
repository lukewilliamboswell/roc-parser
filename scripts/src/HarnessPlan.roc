## Product-specific parser review commands. Python supplies independent oracles.
HarnessPlan := [].{
	Review : { name : Str, args : List(Str), fast : Bool }
	reviews : List(Review)
	reviews = [
		{ name: "yaml", args: ["scripts/review_yaml.py", "check", "--baseline", "scripts/yaml/known-failures.json"], fast: True },
		{ name: "yaml-robustness", args: ["scripts/review_yaml.py", "campaign", "--smoke"], fast: False },
		{ name: "markdown-inline", args: ["scripts/review_markdown.py", "check"], fast: True },
		{ name: "markdown-blocks", args: ["scripts/review_markdown_blocks.py", "check", "--baseline", "scripts/markdown/blocks/known-failures.json"], fast: True },
		{ name: "xml", args: ["scripts/review_xml.py", "check", "--baseline", "scripts/xml/known-failures.json"], fast: True },
		{ name: "json", args: ["scripts/review_json.py", "check", "--baseline", "scripts/json/known-failures.json"], fast: True },
		{ name: "json-robustness", args: ["scripts/review_json.py", "campaign", "--smoke"], fast: False },
		{ name: "csv", args: ["scripts/review_csv.py"], fast: True },
		{ name: "http", args: ["scripts/review_http.py"], fast: True },
	]

	requirements = ["scripts/http/requirements.txt", "scripts/markdown/requirements.txt", "scripts/yaml/requirements.txt"]

	select : List(Str) -> Try(List(Review), Str)
	select = |requested| {
		if requested.is_empty() {
			return Ok(reviews.keep_if(|review| review.fast))
		}
		if requested == ["all"] {
			return Ok(reviews)
		}
		var $chosen = []
		for name in requested {
			match reviews.find_first(|review| review.name == name) {
				Err(_) => return Err("unknown review: ${name}; choose from ${Str.join_with(reviews.map(|review| review.name), ", ")}, or all alone")
				Ok(review) => if !$chosen.any(|item| item.name == name) {
					$chosen = $chosen.append(review)
				}
			}
		}
		Ok($chosen)
	}

	asciidoc : Str, Str, Str, Str, Bool -> List(List(Str))
	asciidoc = |roc, generated, mutations, seed, performance| {
		probe = ".roc-parser-tmp/asciidoc/probe"
		steps = [
			["python3", "scripts/asciidoc/setup_oracle.py"],
			[roc, "build", "--no-cache", "--opt=speed", "scripts/asciidoc/probe.roc", "--output=${probe}"],
			["python3", "scripts/review_asciidoc.py", "check", "--require-complete", "--compare-html", "--probe", probe, "--generated", generated, "--mutations", mutations, "--seed", seed, "--corpus", "docs"],
		]
		if performance {
			steps.append(["python3", "scripts/check_asciidoc_performance.py", "--smoke", "--roc", roc, "--output", ".roc-parser-tmp/asciidoc/performance-smoke"])
		} else {
			steps
		}
	}
}

expect HarnessPlan.select([]).map_ok(|selected| selected.map(|review| review.name)) == Ok(["yaml", "markdown-inline", "markdown-blocks", "xml", "json", "csv", "http"])
expect HarnessPlan.select(["all"]) == Ok(HarnessPlan.reviews)
expect HarnessPlan.select(["all", "yaml"]).is_err()
expect HarnessPlan.select(["json", "yaml", "json"]).map_ok(|selected| selected.map(|review| review.name)) == Ok(["json", "yaml"])
expect HarnessPlan.select(["json", "unknown"]).is_err()
expect HarnessPlan.asciidoc("package-roc", "200", "300", "13", False).len() == 3
expect HarnessPlan.asciidoc("package-roc", "200", "300", "13", True).last() == Ok(["python3", "scripts/check_asciidoc_performance.py", "--smoke", "--roc", "package-roc", "--output", ".roc-parser-tmp/asciidoc/performance-smoke"])
