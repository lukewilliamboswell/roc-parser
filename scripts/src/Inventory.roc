## Product inventory boundaries. The shared Blueprint tool validates the graph.
Inventory := [].{
	unicode_url = "https://github.com/roc-lang/unicode/releases/download/4.3.0/7HGaK8HUHMs5judroSmUoS4TjPGZ1kAjPTyPDSCWiSKM.tar.zst"

	## Refuse a changed closure until its supplier metadata has been reviewed.
	## `roc deps` reads headers, and reports a transitive child on its own line.
	known_dependencies : Str -> Bool
	known_dependencies = |report| report.split_on("\n").drop_first(1).keep_if(|line| line.trim() != "") == ["└── ${unicode_url}"]

}

expect Inventory.known_dependencies("package/main.roc [package]\n└── ${Inventory.unicode_url}\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n└── https://example.com/new.tar.zst\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n└── ${Inventory.unicode_url}\n    └── https://example.com/transitive.tar.zst\n")
expect !Inventory.known_dependencies("package/main.roc [package]\n")
