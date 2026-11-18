# Internal storage shared by document and framed-record JSON scanning.
JsonWorkspace(value) := {
	open : List(JsonWorkspace.Frame),
	items : List(value),
	members : List(JsonWorkspace.Member(value)),
	key_ats : List(U64),
}.{
	Frame : { container : Container, first : U64, key : Str, key_at : U64 }
	Container : [Top, InArray, InObject]
	Member(value) : { key : Str, value : value }

	empty : () -> JsonWorkspace(value)
	empty = || JsonWorkspace.{ open: [], items: [], members: [], key_ats: [] }
}
