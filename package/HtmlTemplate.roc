import Diagnostic
import MarkupCore
import MarkupTemplate
import MarkupValidate
import HtmlValidate
import HtmlTree
import HtmlTokenizer

## Shared typed-hole pipeline for HTML syntax, including foreign fragment
## contexts. Projection runs after authored hole nodes exist; bindings fan out
## through exact node-ID provenance and never trigger source-text parsing.
HtmlTemplate :: [].{
	parse_segments : List(Str), HtmlTree.Context, MarkupCore.Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
	parse_segments = |segments, context, props| {
		raw_context = ["script", "style"].contains(context.name) or (props.syntax == HtmlSyntax and (["xmp", "iframe", "noembed", "noframes", "plaintext"].contains(context.name) or (context.name == "noscript" and context.scripting)))
		if segments.len() > 1 and raw_context {
			return Err(InvalidMarkup(Diagnostic.{ problem: Interpolation({ index: 0, reason: RawTextHole }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
		}
		unprojected = MarkupTemplate.parse_segments_placed_with(
			segments,
			props,
			|text, boundaries| if props.syntax == HtmlSyntax {
				HtmlTree.parse_placed(text, True, { ..context, defer_projection: True }, props, boundaries)
			} else {
				MarkupCore.parse_xml_fragment(text, context.namespace, props).map_ok(|parsed| { parsed, parents: [] })
			},
			|text| if props.syntax == HtmlSyntax {
				Ok(HtmlTokenizer.normalized(text.to_utf8(), True, False).text)
			} else {
				MarkupCore.decode_xml_text(text)
			},
		)?
		_ = MarkupValidate.text_slots(unprojected)?
		authored_holes = unprojected.metadata().slots.keep_oks(
			|slot| match slot.target {
				AttributeSlot(target) => Ok(target)
				_ => Err(Skip)
			},
		)
		authored_foreign = MarkupValidate.foreign(unprojected.metadata().parsed, props, authored_holes)?
		authored_checked = HtmlValidate.validate(authored_foreign, props, authored_holes)?
		var $plan = MarkupTemplate.rebuild({ ..unprojected.metadata(), parsed: authored_checked })
		if props.syntax == HtmlSyntax {
			projected = HtmlTree.project_parsed($plan.metadata().parsed, props)?
			$plan = MarkupTemplate.fanout($plan, projected.parsed, projected.clones, props)?
		}
		_ = MarkupValidate.text_slots($plan)?
		checked = $plan.metadata().parsed
		slots = $plan.metadata().slots.map(
			|slot| match slot.target {
				AttributeSlot(target) => {
					node = checked.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
					match (node.kind, node.attributes.get(target.attribute)) {
						(Element(name), Ok(attribute)) => {
							expected = if attribute.name.namespace != NoNamespace {
								Language(NamespacedAttribute)
							} else if name.namespace == Uri(MarkupCore.svg_namespace) {
								MarkupValidate.grammar(True, name.local, attribute.name.local)
							} else if name.namespace == Uri(MarkupCore.mathml_namespace) {
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
		Ok(MarkupTemplate.rebuild({ ..$plan.metadata(), parsed: checked, slots }))
	}
}

# The expected parent is authored directly, not inferred from source intervals
# or from another invocation of the same tree builder. Prefix/suffix controls
# exercise the existing text splitting path alongside empty and adjacent holes.
expect {
	["strong", "b", "em", "i", "a", "code", "s", "small", "u", "form", "span", "p"].all(
		|name| {
			["body", "section"].all(
				|context_name| {
					[0.U64, 1, 2].all(
						|text_case| {
							opening = if name == "a" {
								"<a href=\"/destination\">"
							} else Str.join_with(["<", name, ">"], "")
							prefix = Str.join_with(
								[
									"<div>",
									opening,
									if text_case == 1 {
										"before"
									} else "",
								],
								"",
							)
							suffix = Str.join_with(
								[
									if text_case == 2 {
										"after"
									} else "",
									"</",
									name,
									"></div>",
								],
								"",
							)
							segments = [prefix, "", suffix]
							plan = match HtmlTemplate.parse_segments(segments, { name: context_name }, { syntax: HtmlSyntax, exact_structure: True, strict_foreign_spelling: True }) {
								Ok(value) => value
								Err(_) => return False
							}
							metadata = plan.metadata()
							metadata.parsed.source.bytes == Str.join_with(segments, "").to_utf8() and metadata.slots.len() == 2 and metadata.slots.all(
								|slot| {
									match slot.target {
										TextSlot(target) => {
											node = match metadata.parsed.tree.nodes.get(target.node) {
												Ok(value) => value
												Err(_) => return False
											}
											parents = metadata.parsed.tree.nodes.keep_if(|candidate| candidate.children.contains(target.node))
											node.kind == Text("") and node.origin == Authored(slot.location) and slot.location.start == prefix.count_utf8_bytes() and slot.location.end == prefix.count_utf8_bytes() and parents.len() == 1 and parents.all(
												|parent| match parent.kind {
													Element(parent_name) => parent_name.local == name
													_ => False
												},
											)
										}
										_ => False
									}
								},
							)
						},
					)
				},
			)
		},
	)
}
expect {
	segments = ["<strong><em>", "", "</em>", "</strong>"]
	plan = HtmlTemplate.parse_segments(segments, {}, { syntax: HtmlSyntax, exact_structure: True })?
	metadata = plan.metadata()
	metadata.slots.map(
		|slot| match slot.target {
			TextSlot(target) => metadata.parsed.tree.nodes.keep_oks(
				|parent| {
					if !parent.children.contains(target.node) {
						return Err(NotParent)
					}
					match parent.kind {
						Element(name) => Ok(name.local)
						_ => Err(NotElement)
					}
				},
			).first() ?? ""
			_ => ""
		},
	) == ["em", "em", "strong"] and metadata.parsed.tree.roots.len() == 1
}
expect HtmlTemplate.parse_segments(["<b><i>", "</b></i>"], {}, { syntax: HtmlSyntax, exact_structure: True, recovery: Browser }).is_err()
expect HtmlTemplate.parse_segments(["<form><div></form>", "</div>"], {}, { syntax: HtmlSyntax, exact_structure: True, recovery: Browser }).is_err()

# Boundaries before the first token and at EOF remain fragment roots; boundaries
# immediately before the closing token remain children of the authored element.
expect {
	plan = HtmlTemplate.parse_segments(["", "<strong>", "</strong>", ""], {}, { syntax: HtmlSyntax, exact_structure: True })?
	metadata = plan.metadata()
	metadata.slots.map(
		|slot| match slot.target {
			TextSlot(target) => metadata.parsed.tree.nodes.keep_oks(
				|parent| {
					if !parent.children.contains(target.node) {
						return Err(NotParent)
					}
					match parent.kind {
						Element(name) => Ok(name.local)
						_ => Err(NotElement)
					}
				},
			).first() ?? "root"
			_ => "attribute"
		},
	) == ["root", "strong", "root"] and metadata.parsed.tree.roots.len() == 3 and metadata.parsed.source.bytes == "<strong></strong>".to_utf8()
}
expect {
	plan = HtmlTemplate.parse_segments(["", "", ""], {}, { syntax: HtmlSyntax, exact_structure: True })?
	metadata = plan.metadata()
	metadata.parsed.source.bytes.is_empty() and metadata.parsed.tree.roots.len() == 2 and metadata.slots.len() == 2 and metadata.slots.all(|slot| slot.location == { start: 0, end: 0 })
}
