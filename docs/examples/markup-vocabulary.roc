app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Html
import parser.Markup
import parser.MarkupFacts

# tag::names[]
svg_candidates = MarkupFacts.element_names(Uri(Markup.svg_namespace))

suggestion = svg_candidates.find_first(|name| name.with_ascii_lowercased() == "lineargradient") ?? "none"

aria_candidates = MarkupFacts.aria_attribute_names

# end::names[]

# tag::newline[]
html_name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

# These sample values contain no markup requiring escaping.
# Use the state after the tokens the serializer actually emitted.
protect_text = |state, value| if MarkupFacts.initial_newline_step(state, Characters(value)).skipped {
	"\n${value}"
} else value

desired = "\nvalue"

pre_text = protect_text(MarkupFacts.initial_newline(html_name("pre")), desired)

textarea_text = protect_text(MarkupFacts.initial_newline(html_name("textarea")), desired)

after_comment = MarkupFacts.initial_newline_step(MarkupFacts.initial_newline(html_name("pre")), Comment)

comment_text = protect_text(after_comment.state, desired)

empty_binding = MarkupFacts.initial_newline_step(MarkupFacts.initial_newline(html_name("pre")), Characters(""))

# end::newline[]

text_content : Str -> Try(Str, [InvalidMarkup(Markup.Error)])
text_content = |source| Html.parse_fragment(source, {}).map_ok(
	|fragment| fragment.parsed.tree.nodes.fold(
		"",
		|text, node| match node.kind {
			Text(value) => "${text}${value}"
			_ => text
		},
	),
)

main! = |_args| {
	Stdout.line!("SVG candidate: ${suggestion}")?
	Stdout.line!("HTML candidates include selectedcontent: ${Str.inspect(MarkupFacts.element_names(Uri(Markup.html_namespace)).contains("selectedcontent"))}")?
	Stdout.line!("ARIA candidates include aria-label: ${Str.inspect(aria_candidates.contains("aria-label"))}")?
	Stdout.line!("pre LF protected: ${Str.inspect(text_content("<pre>${pre_text}</pre>") == Ok(desired))}")?
	Stdout.line!("textarea LF protected: ${Str.inspect(text_content("<textarea>${textarea_text}</textarea>") == Ok(desired))}")?
	Stdout.line!("pre comment keeps LF: ${Str.inspect(text_content("<pre><!-- marker -->${comment_text}</pre>") == Ok(desired))}")?
	Stdout.line!("Empty binding keeps LF pending: ${Str.inspect(empty_binding.state == Pending)}")?
	Stdout.line!("RCDATA comment spelling is text: ${Str.inspect(text_content("<textarea><!-- marker -->\nvalue</textarea>") == Ok("<!-- marker -->\nvalue"))}")?
	Ok({})
}

expect suggestion == "linearGradient"
expect pre_text == "\n\nvalue" and textarea_text == "\n\nvalue" and comment_text == "\nvalue"
expect text_content("<pre>${pre_text}</pre>") == Ok(desired)
expect text_content("<textarea>${textarea_text}</textarea>") == Ok(desired)
expect text_content("<pre><!-- marker -->${comment_text}</pre>") == Ok(desired)
expect text_content("<textarea><!-- marker -->\nvalue</textarea>") == Ok("<!-- marker -->\nvalue")
expect {
	Html.parse_fragment("\nvalue", { context_element: "textarea" }).map_ok(|fragment| fragment.parsed.tree.nodes.any(|node| node.kind == Text("\nvalue"))) == Ok(True)
}
