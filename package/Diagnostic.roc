## Shared failure metadata. Public aliases are owned by Parser and formats.
Diagnostic(problem) := {
	problem : problem,
	location : Diagnostic.Location,
	path : List(Diagnostic.PathSegment),
}.{

	## Offsets count original bytes, except InputOffset which counts input elements.
	## Lines, columns, records and fields are one-based; indexes are zero-based.
	Location : [InputOffset(U64), ByteOffset(U64), Text({ offset : U64, line : U64, column : U64 }), LineColumn({ line : U64, column : U64 }), Unavailable(UnavailableReason)]

	UnavailableReason : [NotRetained, GeneratedValue, NotApplicable]

	## Logical context retained independently of physical source coordinates.
	PathSegment : [Record(U64), Field(U64), Index(U64), Key(Str), Source(Str)]

	## Counts can stop at the first violation without scanning additional input.
	Observed : [Exact(U64), AtLeast(U64)]

	## Text coordinates when known; binary and generated values have none.
	line_column : Location -> Try({ line : U64, column : U64 }, [NoTextPosition])
	line_column = |location| match location {
		Text(position) => Ok({ line: position.line, column: position.column })
		LineColumn(position) => Ok(position)
		_ => Err(NoTextPosition)
	}
}
