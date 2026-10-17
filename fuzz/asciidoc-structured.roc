app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.AsciiDoc

# The model describes semantic block structure, independently of the parser's
# choice of compact or rich public AST constructors. Source text stays raw;
# separate HTML assertions check substitutions and conversion.
Model := [Node({ context : Str, title : Str, source : Str, level : U64, blocks : List(Model) })].{
	is_eq : _
}

node : Str, Str, List(Model) -> Model
node = |context, source, blocks| Node({ context, source, blocks, title: "", level: 0 })

heading : Str, U64, Str, List(Model) -> Model
heading = |context, level, title, blocks| Node({ context, level, title, blocks, source: "" })

project : AsciiDoc.Block -> Model
project = |block| match block {
	Paragraph(source) => node("paragraph", source, [])
	Listing(source) => node("listing", source, [])
	Literal(source) => node("literal", source, [])
	ThematicBreak => node("thematic_break", "", [])
	Section(s) => heading("section", s.level, s.title, s.blocks.keep_if(visible_block).map(project))
	Rich(n) => Node({ context: n.context, title: n.title, source: n.source, level: if n.context == "section" or n.context == "floating_title" n.level else 0, blocks: n.blocks.keep_if(visible_block).map(project) })
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	words = ["Alpha", "Café", "日本語", "Hello 😀"]
	var $source = "= Generated\n\n"
	var $blocks = []
	var $html = []
	for byte in bytes.take_first(48) {
		word = words.get(byte.to_u64() % 4) ?? "Alpha"
		choice = byte % 21
		match choice {
			0 => {
				$source = "${$source}${word}\n\n"
				$blocks = $blocks.append(node("paragraph", word, []))
			}
			1 => {
				$source = "${$source}----\n${word}\n* untouched {attribute}\n----\n\n"
				$blocks = $blocks.append(node("listing", "${word}\n* untouched {attribute}", []))
			}
			2 => {
				$source = "${$source}....\n  ${word}\n....\n\n"
				$blocks = $blocks.append(node("literal", "  ${word}", []))
			}
			3 => {
				$source = "${$source}// ${word}\n\n"
			}
			4 => {
				$source = "${$source}'''\n\n"
				$blocks = $blocks.append(node("thematic_break", "", []))
			}
			5 => {
				$source = "${$source}* ${word}\n* Second\n\n//-\n\n"
				$blocks = $blocks.append(node("ulist", "", [node("list_item", word, []), node("list_item", "Second", [])]))
			}
			6 => {
				$source = "${$source}. ${word}\n.. Child\n\n//-\n\n"
				$blocks = $blocks.append(node("olist", "", [node("list_item", word, [node("olist", "", [node("list_item", "Child", [])])])]))
			}
			7 => {
				$source = "${$source}Term:: ${word}\n\n//-\n\n"
				$blocks = $blocks.append(Node({ context: "dlist", source: "", title: "", level: 0, blocks: [Node({ context: "list_item", source: word, title: "Term", level: 0, blocks: [] })] }))
			}
			8 => {
				$source = "${$source}****\n${word}\n****\n\n"
				$blocks = $blocks.append(node("sidebar", "", [node("paragraph", word, [])]))
			}
			9 => {
				$source = "${$source}NOTE: ${word}\n\n"
				$blocks = $blocks.append(node("admonition", word, []))
				$html = $html.append("class=\"admonitionblock note\"")
			}
			10 => {
				body = "|${word} |Value\n\n|Next |Data"
				$source = "${$source}|===\n${body}\n|===\n\n"
				$blocks = $blocks.append(node("table", body, []))
				$html = $html.append("class=\"tableblock")
			}
			11 => {
				$source = "${$source}image::fox.png[${word}]\n\n"
				$blocks = $blocks.append(node("image", "fox.png", []))
				$html = $html.append("alt=\"${word}\"")
			}
			12 => {
				$source = "${$source}:animal: fox\n\n{animal}\n\n"
				$blocks = $blocks.append(node("paragraph", "{animal}", []))
				$html = $html.append("<p>fox</p>")
			}
			13 => {
				$source = "${$source}:feature:\n\nifdef::feature[${word}]\n\n:feature!:\n\n"
				$blocks = $blocks.append(node("paragraph", word, []))
			}
			14 => {
				$source = "${$source}include::part.adoc[]\n\n"
				$blocks = $blocks.append(node("paragraph", "Included", []))
			}
			15 => {
				$source = "${$source}++++\n<b>${word}</b>\n++++\n\n"
				$blocks = $blocks.append(node("pass", "<b>${word}</b>", []))
				$html = $html.append("<b>${word}</b>")
			}
			16 => {
				$source = "${$source}[quote,Author,Book]\n____\n${word}\n____\n\n"
				$blocks = $blocks.append(node("quote", "", [node("paragraph", word, [])]))
				$html = $html.append("<cite>Book</cite>")
			}
			17 => {
				$source = "${$source}[verse]\n____\n${word}\nLine\n____\n\n"
				$blocks = $blocks.append(node("verse", "${word}\nLine", []))
			}
			18 => {
				$source = "${$source}[discrete]\n=== ${word}\n\n"
				$blocks = $blocks.append(heading("floating_title", 2, word, []))
			}
			19 => {
				$source = "${$source}* ${word}\n+\n----\nAttached\n----\n\n//-\n\n"
				$blocks = $blocks.append(node("ulist", "", [node("list_item", word, [node("listing", "Attached", [])])]))
			}
			_ => {
				$source = "${$source}[source,roc,subs=attributes]\n----\n${word}\n----\n\n"
				$blocks = $blocks.append(node("listing", word, []))
			}
		}
	}
	$source = "${$source}== First\n\nBody\n\n=== Child\n\nNested\n\n== Last\n\nEnd\n"
	$blocks = $blocks.concat([heading("section", 1, "First", [node("paragraph", "Body", []), heading("section", 2, "Child", [node("paragraph", "Nested", [])])]), heading("section", 1, "Last", [node("paragraph", "End", [])])])
	options = { includes: [{ path: "part.adoc", source: "Included" }], attributes: [], max_depth: 16, expand_includes: True }
	document = match AsciiDoc.parse_with($source, options) {
		Ok(doc) => doc
		Err(_) => crash "Generated valid AsciiDoc rejected: ${Str.inspect($source)}"
	}
	if document.title != "Generated" or document.blocks.keep_if(visible_block).map(project) != $blocks {
		crash "AsciiDoc disagrees with independent generated tree: ${Str.inspect($source)}"
	}
	html = AsciiDoc.render(document)
	for expected in $html {
		if !html.contains(expected) {
			crash "Generated AsciiDoc conversion lost semantic HTML: ${Str.inspect(expected)} source ${Str.inspect($source)}"
		}
	}
	crlf = Str.replace_each($source, "\n", "\r\n")
	match AsciiDoc.parse_with(crlf, options) {
		Ok(doc) => if doc.title != document.title or doc.blocks.keep_if(visible_block).map(project) != $blocks or AsciiDoc.render(doc) != html {
			crash "CRLF changed generated AsciiDoc semantics"
		}
		Err(_) => crash "CRLF rejected generated AsciiDoc"
	}
	match AsciiDoc.parse_with("// Prologue\n\n${$source}", options) {
		Ok(doc) => if doc.title != document.title or doc.blocks.keep_if(visible_block).map(project) != $blocks {
			crash "Leading line comment changed generated document"
		}
		Err(_) => crash "Leading line comment rejected generated AsciiDoc"
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "asciidoc-structured", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })

visible_block : AsciiDoc.Block -> Bool
visible_block = |block| match block {
	Rich(n) => n.context != "__document_header"
	_ => True
}
