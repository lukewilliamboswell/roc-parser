## The published site serves the current manual, its API reference and a PDF.
DocsSite := [].{
	sorted : List(Str) -> List(Str)
	sorted = |names| names.sort_with(|a, b| compare_bytes(a.to_utf8(), b.to_utf8()))

	safe_site_destination : Str, Str -> Bool
	safe_site_destination = |root, destination| !destination.split_on("/").contains("..") and ((destination.starts_with("${root}/.docs-out/") and destination != "${root}/.docs-out/") or (destination.starts_with("${root}/.roc-parser-tmp/") and destination != "${root}/.roc-parser-tmp/"))

	not_found : Str -> Str
	not_found = |repository| {
		parts = repository.split_on("/")
		owner = parts.first() ?? ""
		name = parts.last() ?? ""
		root = "https://${owner}.github.io/${name}/"
		"<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>Page not found · roc-parser</title><style>body{font-family:system-ui,sans-serif;max-width:40rem;margin:4rem auto;padding:0 1rem;line-height:1.5;color:#1a1a1a;background:#fff}a{color:#6b3f8f}@media(prefers-color-scheme:dark){body{color:#ececea;background:#16181a}a{color:#c9a8ea}}</style></head><body><h1>Page not found</h1><p>This site shows the documentation for the current roc-parser release only: the <a href=\"${root}\">manual</a> and the <a href=\"${root}api/\">API reference</a>.</p><p>The documentation for an earlier release is attached to that release on the <a href=\"https://github.com/${repository}/releases\">releases page</a>, as a PDF and as a zipped static site.</p></body></html>\n"
	}
}

expect DocsSite.safe_site_destination("/workspace", "/workspace/.docs-out/release/site")
expect DocsSite.safe_site_destination("/workspace", "/workspace/.roc-parser-tmp/release/site")
expect !DocsSite.safe_site_destination("/workspace", "/workspace/package")
expect !DocsSite.safe_site_destination("/workspace", "/workspace/.docs-out/../package")
expect !DocsSite.safe_site_destination("/workspace", "/workspace/.docs-out")
expect DocsSite.not_found("owner/repository").contains("href=\"https://owner.github.io/repository/api/\"") and DocsSite.not_found("owner/repository").contains("https://github.com/owner/repository/releases")

compare_bytes = |left, right| match (left, right) {
	([], []) => Same
	([], _) => Before
	(_, []) => After
	([a, .. as aa], [b, .. as bb]) => if a < b Before else if a > b After else compare_bytes(aa, bb)
}

expect DocsSite.sorted(["index.html", "api/index.html", "404.html"]) == ["404.html", "api/index.html", "index.html"]

expect !DocsSite.safe_site_destination("/workspace", "/workspace/.docs-out/")
