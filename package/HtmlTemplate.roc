import Markup
import MarkupTemplate
import MarkupValidate
import HtmlValidate
import HtmlTree
import HtmlTokenizer

## Shared typed-hole pipeline for HTML syntax, including foreign fragment
## contexts. Projection runs after authored hole nodes exist; bindings fan out
## through exact node-ID provenance and never trigger source-text parsing.
HtmlTemplate :: [].{
	parse_segments : List(Str), HtmlTree.Context, Markup.Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, context, props| {
		raw_context = ["script", "style"].contains(context.name) or (props.syntax == HtmlSyntax and (["xmp", "iframe", "noembed", "noframes", "plaintext"].contains(context.name) or (context.name == "noscript" and context.scripting)))
		if segments.len() > 1 and raw_context {
			return Err(InvalidMarkup({ problem: Interpolation({ index: 0, reason: "holes are not permitted in raw-text fragment contexts" }), line: 1, column: 1 }))
		}
		unprojected = MarkupTemplate.parse_segments_with(
			segments,
			props,
			|text| if props.syntax == HtmlSyntax {
				HtmlTree.parse(text, True, { ..context, defer_projection: True }, props)
			} else {
				Markup.parse_xml_fragment(text, context.namespace, props)
			},
			|text| if props.syntax == HtmlSyntax {
				Ok(HtmlTokenizer.normalized(text.to_utf8(), True, False).text)
			} else {
				Markup.decode_xml_text(text)
			},
		)?
		var $plan = unprojected
		if props.syntax == HtmlSyntax {
			projected = HtmlTree.project_parsed(unprojected.parsed, props)?
			$plan = MarkupTemplate.fanout(unprojected, projected.parsed, projected.clones, props)?
		}
		_ = MarkupValidate.text_slots($plan)?
		holes = $plan.slots.keep_oks(
			|slot| match slot.target {
				AttributeSlot(target) => Ok(target)
				_ => Err(Skip)
			},
		)
		foreign = MarkupValidate.foreign($plan.parsed, props, holes)?
		checked = HtmlValidate.validate(foreign, props, holes)?
		slots = $plan.slots.map(
			|slot| match slot.target {
				AttributeSlot(target) => {
					node = checked.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
					match (node.kind, node.attributes.get(target.attribute)) {
						(Element(name), Ok(attribute)) => {
							expected = if name.namespace == Uri(Markup.svg_namespace) {
								MarkupValidate.grammar(True, name.local, attribute.name.local)
							} else if name.namespace == Uri(Markup.mathml_namespace) {
								MarkupValidate.grammar(False, name.local, attribute.name.local)
							} else {
								HtmlValidate.grammar(name.local, attribute.name.local)
							}
							{ ..slot, expected }
						}
						_ => slot
					}
				}
				_ => slot
			},
		)
		Ok({ ..$plan, parsed: checked, slots })
	}
}
