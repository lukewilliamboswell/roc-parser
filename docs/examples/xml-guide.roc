app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
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
## The value of the attribute called `name`, if the element has one.
attribute : List(Xml.Attribute), Str -> Try(Str, [NotFound])
attribute = |attributes, name| {
	match attributes.find_first(|attr| attr.name == name) {
		Ok(attr) => Ok(attr.value)
		Err(_) => Err(NotFound)
	}
}

## All text directly or indirectly inside a node, joined in document order.
text_of : Xml.Node -> Str
text_of = |node| {
	match node {
		Text(text) => text
		Element(_, _, children) => children.map(text_of) |> Str.join_with("")
	}
}

## The child elements of `node` called `name`.
children_named : Xml.Node, Str -> List(Xml.Node)
children_named = |node, name| {
	match node {
		Element(_, _, children) =>
			children.keep_if(
				|child| {
					match child {
						Element(child_name, _, _) => child_name == name
						Text(_) => Bool.False
					}
				},
			)

		Text(_) => []
	}
}

describe_entry : Xml.Node -> Str
describe_entry = |entry| {
	id =
		match entry {
			Element(_, attributes, _) => attribute(attributes, "id") ?? "?"
			Text(_) => "?"
		}
	titles = children_named(entry, "title").map(text_of) |> Str.join_with("")

	"entry ${id}: ${titles}"
}

# end::walk[]

# tag::errors[]
check : Str -> Str
check = |text| {
	match Xml.parse_str(text) {
		Ok(_) => "well-formed"
		Err(XmlError({ line, column, message })) => "${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::errors[]

print_feed! : Str => Try({}, _)
print_feed! = |text| {
	# tag::parse[]
	match Xml.parse_str(text) {
		Ok(xml) =>
			for entry in children_named(xml.root, "entry") {
				Stdout.line!(describe_entry(entry))?
			}

		Err(XmlError(problem)) => Stdout.line!("invalid XML: ${problem.message}")?
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
