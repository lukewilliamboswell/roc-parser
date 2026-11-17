import Diagnostic

## Own only native diagnostic metadata at failure boundaries.
DiagnosticOwnership :: [].{
	own_text : Str -> Str
	own_text = |text| {
		bytes = text.to_utf8()
		var $copy = List.with_capacity(bytes.len())
		for byte in bytes {
			$copy = $copy.append(byte)
		}
		Str.from_utf8($copy) ?? crash "owned diagnostic text changed its UTF-8 bytes"
	}

	own_path : List(Diagnostic.PathSegment) -> List(Diagnostic.PathSegment)
	own_path = |path| path.map(
		|segment| match segment {
			Record(number) => Record(number)
			Field(number) => Field(number)
			Index(number) => Index(number)
			Key(text) => Key(own_text(text))
			Source(text) => Source(own_text(text))
		},
	)
}
