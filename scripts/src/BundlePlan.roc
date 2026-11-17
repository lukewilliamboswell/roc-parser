## Module enumeration for the standalone bundle task.
BundlePlan := [].{

	## main.roc establishes dependencies; the other modules must also be bundled.
	module_names : List(Str) -> List(Str)
	module_names = |names| ["main.roc"].concat(names.keep_if(|name| name.ends_with(".roc") and name != "main.roc").sort_with(|a, b| compare_bytes(Str.to_utf8(a), Str.to_utf8(b))))
}

expect BundlePlan.module_names(["Yaml.roc", "main.roc", "CSV.roc", "README.md"]) == ["main.roc", "CSV.roc", "Yaml.roc"]

compare_bytes = |left, right| match (left, right) {
	([], []) => Same
	([], _) => Before
	(_, []) => After
	([a, .. as more_a], [b, .. as more_b]) => if a < b Before else if a > b After else compare_bytes(more_a, more_b)
}
