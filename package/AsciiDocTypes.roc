import AsciiDocAttributes

# Shared node identity keeps derived methods out of the public facade scope.
AsciiDocTypes :: [].{
	Block := [Paragraph(Str), Section({ level : U64, title : Str, blocks : List(Block) }), Listing(Str), Literal(Str), ThematicBreak, Rich({ context : Str, source : Str, title : Str, style : Str, id : Str, attrs : AsciiDocAttributes.Attributes, blocks : List(Block), level : U64 })].{

		## Compare source blocks, including metadata and nested content.
		is_eq : _
	}

}
