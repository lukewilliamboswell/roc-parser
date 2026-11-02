## Normalize compiler HTML without changing the documented public API.
ApiDocs := [].{
	escape : Str -> Str
	escape = |text| text.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;").replace_each("\"", "&quot;").replace_each("'", "&#x27;")

	header : Str -> Str
	header = |source| {
		var $lines = []
		for line in source.split_on("\n") {
			trimmed = line.trim()
			if trimmed.starts_with("##") {
				$lines = $lines.append(trimmed.drop_prefix("##").drop_prefix(" "))
			} else if trimmed != "" {
				break
			}
		}
		Str.join_with($lines, "\n").trim()
	}

	inline : Str -> Str
	inline = |text| Str.join_with(text.split_on("`").map_with_index(|part, index| if index % 2 == 0 escape(part) else "<code>${escape(part)}</code>"), "")

	paragraph : List(Str) -> List(Str)
	paragraph = |lines| if lines.is_empty() [] else ["                <p>${inline(Str.join_with(lines, " "))}</p>"]

	## The package header uses paragraphs, backtick spans and fenced examples.
	markdown : Str -> Str
	markdown = |text| {
		var $out = []
		var $paragraph = []
		var $fence = []
		var $fenced = False
		for line in text.split_on("\n") {
			if line.trim().starts_with("```") {
				if $fenced {
					$out = $out.append("                <pre><code>${escape(Str.join_with($fence, "\n"))}</code></pre>")
					$fence = []
				} else {
					$out = $out.concat(paragraph($paragraph))
					$paragraph = []
				}
				$fenced = !$fenced
			} else if $fenced {
				$fence = $fence.append(line)
			}
				else if line.trim() != "" {
					$paragraph = $paragraph.append(line.trim())
				}
					else {
						$out = $out.concat(paragraph($paragraph))
						$paragraph = []
					}
		}
		Str.join_with($out.concat(paragraph($paragraph)), "\n")
	}

	index_body : Str, Str -> Try(Str, Str)
	index_body = |page, source| {
		doc = header(source)
		if doc == "" {
			return Err("package/main.roc has no package header documentation")
		}
		marker = "        <div class=\"index-decoration\">"
		parts = page.split_first(marker).map_err(|_| "compiler API index has no body insertion marker")?
		Ok("${parts.before}        <div class=\"module-doc\">\n${markdown(doc)}\n        </div>\n${marker}${parts.after}")
	}

	site_name : Str -> Try(Str, Str)
	site_name = |page| {
		after = page.split_first("<h1 class=\"pkg-full-name\"><a href=\"").map_err(|_| "compiler API index has no package name")?.after
		body = after.split_first("\">").map_err(|_| "compiler API index has an invalid package link")?.after
		body.split_first("</a></h1>").map_ok(|parts| parts.before).map_err(|_| "compiler API index has an invalid package heading")
	}

	name_site : Str, Str, Str -> Str
	name_site = |page, old, name| page.replace_each(">${old}</a></h1>", ">${escape(name)}</a></h1>").replace_each("<title>${old} Docs</title>", "<title>${escape(name)} Docs</title>")

	## Remove only the module nominal's compiler-generated representation.
	## Entries further down the page can legitimately use these same types.
	strip_module_type : Str, Str -> Str
	strip_module_type = |page, name| {
		match page.split_first("<h1 class=\"module-name\">${name}</h1>") {
			Err(_) => page
			Ok(heading) => {
				match heading.after.split_first("<pre class=\"entry-type-def\"><code class=\"entry-type-def-code\">") {
					Err(_) => page
					Ok(block) => {
						# Only whitespace/comments may separate this block from the heading.
						var $gap = block.before.trim()
						while $gap.starts_with("<!--") {
							match $gap.split_first("-->") {
								Ok(parts) => {
									$gap = parts.after.trim()
								}
								Err(_) => return page
							}
						}
						if $gap != "" {
							return page
						}
						match block.after.split_first("</code></pre>") {
							Ok(code) if code.before == ":= []" or code.before == "${name} :: # (opaque)" => "${heading.before}<h1 class=\"module-name\">${name}</h1>${block.before}${code.after}"
							_ => page
						}
					}
				}
			}
		}
	}
}

expect ApiDocs.header("## Parse text.\n##\n## More.\npackage []\n## ignored") == "Parse text.\n\nMore."
expect ApiDocs.header("package []\n## ignored") == ""
expect ApiDocs.markdown("Parse `Str` & bytes.\n\n```roc\nx = 1 < 2\n```\n") == "                <p>Parse <code>Str</code> &amp; bytes.</p>\n                <pre><code>x = 1 &lt; 2</code></pre>"
expect ApiDocs.index_body("before        <div class=\"index-decoration\">after", "## Introduction\npackage []").map_ok(|page| page.contains("<p>Introduction</p>")) == Ok(True)
expect ApiDocs.index_body("changed compiler structure", "## Introduction\npackage []").is_err()
expect ApiDocs.index_body("        <div class=\"index-decoration\">", "package []").is_err()
expect ApiDocs.site_name("<h1 class=\"pkg-full-name\"><a href=\"../\">package</a></h1>") == Ok("package")
expect ApiDocs.name_site("<title>package Docs</title><h1 class=\"pkg-full-name\"><a href=\"../\">package</a></h1>", "package", "roc-parser") == "<title>roc-parser Docs</title><h1 class=\"pkg-full-name\"><a href=\"../\">roc-parser</a></h1>"
expect {
	page = "<h1 class=\"module-name\">CSV</h1>\n<!--!\u(1F)--><pre class=\"entry-type-def\"><code class=\"entry-type-def-code\">CSV :: # (opaque)</code></pre>\nbody"
	ApiDocs.strip_module_type(page, "CSV") == "<h1 class=\"module-name\">CSV</h1>\n<!--!\u(1F)-->\nbody"
}
expect {
	page = "<h1 class=\"module-name\">CSV</h1><article><pre class=\"entry-type-def\"><code class=\"entry-type-def-code\">:= []</code></pre></article>"
	ApiDocs.strip_module_type(page, "CSV") == page
}
