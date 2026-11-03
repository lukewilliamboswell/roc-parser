app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.Xml

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

	Ok({})
}

expect Xml.parse_str(feed_text).map_ok(|xml| xml.root.children_named("entry").last().map_ok(|entry| entry.children_named("title").first().map_ok(|title| title.text()))) == Ok(Ok(Ok("<Hello> \u(1F600)")))
