## Repository-specific release request gates. Packaging lives in release-package.
ReleasePlan := [].{
	Options : List({ name : Str, value : Str })
	Request : { version : Str, dry_run : Bool, prerelease : Bool }

	options : List(Str) -> Try(Options, Str)
	options = |args| parse_options(args, [])

	option : Options, Str, Str -> Str
	option = |entries, name, fallback| entries.keep_if(|entry| entry.name == name).last().map_ok(|entry| entry.value) ?? fallback

	valid_version : Str -> Bool
	valid_version = |version| {
		parts = version.split_on("-")
		core = parts.first() ?? ""
		numbers = core.split_on(".")
		if numbers.len() != 3 or !numbers.all(canonical_number) {
			return False
		}
		if parts.len() == 1 {
			return True
		}
		pre = Str.join_with(parts.drop_first(1), "-")
		pre.split_on(".").all(|part| !part.is_empty() and part.to_utf8().all(|c| ascii_alnum(c) or c == '-') and (!digits(part) or canonical_number(part)))
	}

	request : Str, Str, Str, Str -> Try(Request, Str)
	request = |event, version, ref, default_branch| {
		if event != "workflow_dispatch" {
			return Ok({ version: "0.0.1-dryrun", dry_run: True, prerelease: True })
		}
		if ref != "refs/heads/${default_branch}" {
			return Err("release only from refs/heads/${default_branch}")
		}
		if !valid_version(version) or version == "0.0.0" or version.starts_with("0.0.0-") {
			return Err("invalid release version: ${version}")
		}
		Ok({ version, dry_run: False, prerelease: version.contains("-") })
	}

	## A major version cannot understate an API change; a minor/patch still
	## requires a readable comparison, even when the old compiler API is broken.
	unreadable_major : Str, Str, Str -> Bool
	unreadable_major = |version, previous, output| {
		if !valid_version(version) or !valid_version(previous) {
			return False
		}
		major = |value| U64.from_str(value.split_on(".").first() ?? "") ?? 0
		major(version) > major(previous) and (lower(output).contains("package does not compile") or lower(output).contains("cannot extract public api"))
	}

	repository : Str -> Bool
	repository = |value| match value.split_on("/") {
		[owner, name] => [owner, name].all(|part| !part.is_empty() and part.to_utf8().all(|c| ascii_alnum(c) or c == '-' or c == '_' or c == '.'))
		_ => False
	}

	outputs : Request -> Str
	outputs = |selected| "version=${selected.version}\nprerelease=${if selected.prerelease "true" else "false"}\n"
}

allowed = ["--release-version", "--default-branch", "--github-output", "--event", "--ref", "--repo"]

parse_options = |args, found| match args {
	[] => Ok(found)
	[name, .. as rest] => match name.split_first("=") {
		Ok({ before, after }) if allowed.contains(before) => parse_options(rest, found.append({ name: before, value: after }))
		_ => match rest {
			[value, .. as more] if allowed.contains(name) => parse_options(more, found.append({ name, value }))
			_ => Err("unknown option or missing value: ${name}")
		}
	}
}

ascii_alnum = |c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')

digits = |text| !text.is_empty() and text.to_utf8().all(|c| c >= '0' and c <= '9')

canonical_number = |text| digits(text) and (text == "0" or !text.starts_with("0")) and U64.from_str(text).is_ok()

expect ReleasePlan.request("pull_request", "garbage", "refs/pull/1/merge", "main") == Ok({ version: "0.0.1-dryrun", dry_run: True, prerelease: True })
expect ReleasePlan.request("workflow_dispatch", "3.0.0", "refs/heads/other", "main").is_err()
expect ReleasePlan.request("workflow_dispatch", "3.0.0-rc.1", "refs/heads/main", "main") == Ok({ version: "3.0.0-rc.1", dry_run: False, prerelease: True })
expect ["", "3.0", "03.0.0", "3.0.0-", "3.0.0-rc..1", "3.0.0-01", "3.0.0\nversion=evil"].all(|v| !ReleasePlan.valid_version(v))
expect ReleasePlan.options(["--unknown", "x"]).is_err()
expect ReleasePlan.options(["--repo"]).is_err()
expect ReleasePlan.option(ReleasePlan.options(["--repo=one/repo", "--repo", "last/repo"]) ?? [], "--repo", "") == "last/repo"
expect ReleasePlan.unreadable_major("3.0.0-rc.1", "2.0.0", "Package does not compile")
expect !ReleasePlan.unreadable_major("2.1.0", "2.0.0", "Package does not compile")
expect !ReleasePlan.unreadable_major("3.0.0", "2.0.0", "network failure")
expect ReleasePlan.repository("owner/repo") and !ReleasePlan.repository("o/r/other") and !ReleasePlan.repository("o/r\noutput=evil")

lower = |text| Str.from_utf8_lossy(text.to_utf8().map(|c| if c >= 'A' and c <= 'Z' c + 32 else c))
