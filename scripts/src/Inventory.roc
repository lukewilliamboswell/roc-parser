## Product inventory boundaries. The shared Blueprint tool validates the graph.
Inventory := [].{
	unicode_url = "https://github.com/roc-lang/unicode/releases/download/4.3.0/7HGaK8HUHMs5judroSmUoS4TjPGZ1kAjPTyPDSCWiSKM.tar.zst"

	## Refuse a changed closure until its supplier metadata has been reviewed.
	## `roc deps` reads headers, and reports a transitive child on its own line.
	known_dependencies : Str -> Bool
	known_dependencies = |report| report.split_on("\n").drop_first(1).keep_if(|line| line.trim() != "") == ["└── ${unicode_url}"]

	valid_bundle_name : Str -> Bool
	valid_bundle_name = |name| name.ends_with(".tar.zst") and name.to_utf8().all(|c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or ['-', '_', '.'].contains(c))

}

expect Inventory.known_dependencies("package/main.roc [package]\n└── ${Inventory.unicode_url}\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n└── https://example.com/new.tar.zst\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n└── ${Inventory.unicode_url}\n    └── https://example.com/transitive.tar.zst\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n")
expect Inventory.valid_bundle_name("hash.tar.zst") and !Inventory.valid_bundle_name("../hash.tar.zst") and !Inventory.valid_bundle_name("hash\noutput=bad.tar.zst")
