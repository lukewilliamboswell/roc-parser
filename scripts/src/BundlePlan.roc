## Compiler output and command construction for the package dry run.
BundlePlan := [].{

	## `roc bundle` runs inside package/, and may print a relative path.
	bundle_path : Str -> Try(Str, Str)
	bundle_path = |output| {
		paths = Str.split_on(output, "\n").keep_oks(
			|line| {
				match Str.split_first(line, "Created:") {
					Ok({ before: "", after }) if after.trim().ends_with(".tar.zst") => Ok(after.trim())
					_ => Err(NotPath)
				}
			},
		)
		match paths {
			[path] => Ok(path)
			_ => Err("expected exactly one Created: <bundle>.tar.zst line from roc bundle")
		}
	}

	## Consume only our output option; pass compiler flags through unchanged.
	options : List(Str) -> Try({ output : Str, forwarded : List(Str) }, Str)
	options = |args| parse_options(args, { output: "dist", forwarded: [] })

	## main.roc establishes dependencies; the other modules must also be bundled.
	module_names : List(Str) -> List(Str)
	module_names = |names| ["main.roc"].concat(names.keep_if(|name| name.ends_with(".roc") and name != "main.roc").sort_with(|a, b| compare_bytes(Str.to_utf8(a), Str.to_utf8(b))))
}

expect BundlePlan.bundle_path("Created: /tmp/example.tar.zst\nCompressed size: 42 bytes\n") == Ok("/tmp/example.tar.zst")
expect BundlePlan.bundle_path("Created: ../dist/hash.tar.zst\n") == Ok("../dist/hash.tar.zst")
expect BundlePlan.bundle_path("bundle complete\n").is_err()
expect BundlePlan.bundle_path("Created: a.tar.zst\nCreated: b.tar.zst\n").is_err()
expect BundlePlan.module_names(["Yaml.roc", "main.roc", "CSV.roc", "README.md"]) == ["main.roc", "CSV.roc", "Yaml.roc"]

compare_bytes = |left, right| match (left, right) {
	([], []) => Same
	([], _) => Before
	(_, []) => After
	([a, .. as more_a], [b, .. as more_b]) => if a < b Before else if a > b After else compare_bytes(more_a, more_b)
}

parse_options = |args, state| match args {
	["--output-dir", value, .. as rest] => parse_options(rest, { ..state, output: value })
	["--output-dir"] => Err("--output-dir needs a directory")
	[value, .. as rest] if value.starts_with("--output-dir=") => parse_options(rest, { ..state, output: value.drop_prefix("--output-dir=") })
	[value, .. as rest] => parse_options(rest, { ..state, forwarded: state.forwarded.append(value) })
	[] => if state.output == "" Err("output directory cannot be empty") else Ok(state)
}

expect BundlePlan.options(["--output-dir=build/release", "--some-roc-option", "value"]) == Ok({ output: "build/release", forwarded: ["--some-roc-option", "value"] })
expect BundlePlan.options(["--output-dir", "first", "--output-dir", "last"]) == Ok({ output: "last", forwarded: [] })
expect BundlePlan.options(["--output-dir"]).is_err()
expect BundlePlan.options(["--output-dir="]).is_err()
