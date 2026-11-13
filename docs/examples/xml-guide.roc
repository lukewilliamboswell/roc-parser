app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.Xml
import parser.XmlNs

# tag::document[]
feed_text =
	\\<?xml version="1.0" encoding="UTF-8"?>
	\\<feed lang="en">
	\\  <!-- newest first -->
	\\  <entry id="2"><title>Fish &amp; Chips</title></entry>
	\\  <entry id="1"><title><![CDATA[<Hello>]]> &#x1F600;</title></entry>
	\\</feed>

# end::document[]

# tag::walk[]
describe_entry : Xml.Node -> Str
describe_entry = |entry| {
	id = entry.attribute("id") ?? "?"
	titles = Str.join_with(entry.children_named("title").map(|title| title.text()), "")

	"entry ${id}: ${titles}"
}

# end::walk[]

# tag::errors[]
check : Str -> Str
check = |text| {
	match Xml.parse_str(text) {
		Ok(_) => "well-formed"
		Err(InvalidXml({ line, column, message })) => "${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::errors[]

print_feed! : Str => Try({}, _)
print_feed! = |text| {
	# tag::parse[]
	match Xml.parse_str(text) {
		Ok(xml) =>
			for entry in xml.root.children_named("entry") {
				# Keep the displayed output portable across documentation fonts.
				Stdout.line!(describe_entry(entry).replace_each("\u(1F600)", "\\u(1F600)"))?
			}

		Err(InvalidXml(problem)) => Stdout.line!("invalid XML: ${problem.message}")?
	}
	# end::parse[]
	Ok({})
}

# tag::reader[]
## The titles of a feed's entries, read one entry at a time: only the entry
## being read is ever a tree.
entry_titles : Str -> Try(List(Str), Xml.ReadError)
entry_titles = |text| {
	var $reader = Xml.reader(text, {})?
	var $titles = []
	while True {
		match $reader.next() {
			Event(Start({ name: "entry", .. }), at_entry) =>
				match at_entry.element() {
					Element(entry, after) => {
						$titles = $titles.concat(entry.children_named("title").map(|title| title.text()))
						$reader = after
					}

					Failed(error) => return Err(error)
					NotAfterStart => return Ok($titles)
				}

			Event(_, rest) => {
				$reader = rest
			}

			Done => return Ok($titles)
			Failed(error) => return Err(error)
		}
	}
}

# end::reader[]

# tag::limits[]
check_untrusted : Str -> Str
check_untrusted = |text| {
	match Xml.parse(text, { max_depth: 3, max_elements: 100 }) {
		Ok(_) => "accepted"
		Err(LimitExceeded({ limit, max, line, column })) => "${line.to_str()}:${column.to_str()}: more than ${max.to_str()} (${Str.inspect(limit)})"
		Err(InvalidXml({ line, column, message })) => "${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::limits[]

# tag::namespaces[]
atom : XmlNs.Namespace
atom = Uri("http://www.w3.org/2005/Atom")

describe_atom : Str -> Str
describe_atom = |text| {
	match XmlNs.parse_str(text) {
		Ok(doc) => {
			titles = doc.root.children_named(atom, "title").map(|title| title.text())
			"${Str.inspect(doc.root.name())} ${Str.inspect(titles)}"
		}

		Err(InvalidNamespace({ problem, line, column })) => "${line.to_str()}:${column.to_str()}: ${Str.inspect(problem)}"
		Err(InvalidXml({ message, .. })) => message
	}
}

# end::namespaces[]

# tag::render[]
## Writes an Atom feed with Atom as the default namespace, whatever prefix it
## was read with.
rewrite_atom : Str -> Str
rewrite_atom = |text| {
	match XmlNs.parse_str(text) {
		Ok(doc) =>
			match XmlNs.render(doc, { declaration: Omitted, prefixes: Table([{ prefix: "", namespace: "http://www.w3.org/2005/Atom" }]) }) {
				Ok(written) => written
				Err(Unrenderable({ problem, .. })) => Str.inspect(problem)
			}

		Err(_) => "invalid"
	}
}

# end::render[]

print_tree! : Str => Try({}, _)
print_tree! = |text| {
	match Xml.parse_str(text) {
		Ok(xml) => Stdout.line!(Str.inspect(xml.root))?
		Err(_) => Stdout.line!("invalid XML")?
	}
	Ok({})
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	# tag::parse-run[]
	print_feed!(feed_text)?
	# end::parse-run[]

	# tag::normalise-run[]
	print_tree!("<p class='a\tb'>x &lt; y &#65;&#x42;\r\n<![CDATA[<raw>]]><!-- gone -->z<br/></p>")?
	# end::normalise-run[]

	# tag::errors-run[]
	Stdout.line!(check("<a>\n  <b>\n</a>"))?
	Stdout.line!(check("<a>&nbsp;</a>"))?
	Stdout.line!(check("<a x='1' x='2'/>"))?
	Stdout.line!(check("<a/><b/>"))?
	Stdout.line!(check("<!DOCTYPE html><html/>"))?
	# end::errors-run[]

	# tag::reader-run[]
	Stdout.line!(Str.inspect(entry_titles("<feed><entry><title>One</title></entry><entry><title>Two</title></entry></feed>")))?
	# end::reader-run[]

	# tag::limits-run[]
	Stdout.line!(check_untrusted("<a><b><c/></b></a>"))?
	Stdout.line!(check_untrusted("<a>\n  <b><c><d/></c></b>\n</a>"))?
	# end::limits-run[]

	# tag::namespaces-run[]
	Stdout.line!(describe_atom("<feed xmlns='http://www.w3.org/2005/Atom'><title>News</title></feed>"))?
	Stdout.line!(describe_atom("<a:feed xmlns:a='http://www.w3.org/2005/Atom'><a:title>News</a:title></a:feed>"))?
	Stdout.line!(describe_atom("<feed><x:title/></feed>"))?
	# end::namespaces-run[]

	# tag::render-run[]
	Stdout.line!(rewrite_atom("<a:feed xmlns:a='http://www.w3.org/2005/Atom'><a:title>Fish &amp; Chips</a:title></a:feed>"))?
	# end::render-run[]

	Ok({})
}

expect Xml.parse_str(feed_text).map_ok(|xml| xml.root.children_named("entry").last().map_ok(|entry| entry.children_named("title").first().map_ok(|title| title.text()))) == Ok(Ok(Ok("<Hello> \u(1F600)")))
