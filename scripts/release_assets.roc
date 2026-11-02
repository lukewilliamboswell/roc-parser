#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.Env
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/ReleasePlan

## Compose the shared release builder with roc-parser's manual and SBOM.
## Test transferred files in place; the test operation never rebuilds them.
main! = |args| {
	result = match args.map(OsStr.display) {
		["build", .. as options] => build!(options)
		["test", .. as options] => test!(options)
		_ => Err("usage: release_assets.roc -- build|test --release-version VERSION --output-dir|--directory DIR [--repo owner/name]")
	}
	match result {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("release: ${reason}")?
			Err(Exit(1))
		}
	}
}

settings! = |args, directory_flag| {
	options = ReleasePlan.options_for(args, ["--release-version", directory_flag, "--repo", "--github-output"])?
	version = ReleasePlan.option(options, "--release-version", "")
	if !ReleasePlan.valid_version(version) or version == "0.0.0" or version.starts_with("0.0.0-") {
		return Err("invalid release version: ${version}")
	}
	repository = ReleasePlan.option(options, "--repo", optional!("GITHUB_REPOSITORY", "lukewilliamboswell/roc-parser")?)
	if !ReleasePlan.repository(repository) {
		return Err("repository must be owner/name")
	}
	given = ReleasePlan.option(options, directory_flag, "")
	if given == "" {
		return Err("${directory_flag} is required")
	}
	directory = Path.absolute!(Path.utf8(given)).map_err(|_| "cannot resolve release directory")?
	Ok({ version, repository, directory, output: ReleasePlan.option(options, "--github-output", "") })
}

build! = |args| {
	settings = settings!(args, "--output-dir")?
	root = Path.absolute!(".").map_err(|_| "cannot resolve workspace")?
	# Builds replace their output. Confine cleanup to a child of our scratch directory.
	directory_name = Path.display(settings.directory)
	if !directory_name.starts_with("${Path.display(root)}/.roc-parser-tmp/") or directory_name.split_on("/").contains("..") {
		return Err("release output must be a child directory of .roc-parser-tmp")
	}
	if Path.exists!(settings.directory).map_err(|_| "cannot inspect release output")? {
		Path.delete_all!(settings.directory).map_err(|_| "cannot clear release output")?
	}
	Path.create_all!(settings.directory).map_err(|_| "cannot create release output")?
	dist = Path.join(settings.directory, "dist")
	utility!(["build", settings.version], settings, dist)?
	# The shared tool's API archive is replaced by our combined manual/API site.
	notes = Path.join(dist, "release-notes.md")
	Path.copy!(notes, Path.join(settings.directory, "release-notes.md")).map_err(|_| "cannot place release notes")?
	tooling = Cmd.new("roc-stable").args(["version"].map(OsStr.from_str)).stdout(Capture).stderr(Inherit).run!().map_err(|_| "cannot identify repository tooling compiler")?
	if tooling.status != Exited(0) {
		return Err("cannot identify repository tooling compiler")
	}
	body_path = Path.join(settings.directory, "release-notes.md")
	body = Path.read_utf8!(body_path).map_err(|_| "cannot read generated notes")?
	Path.write_utf8!(body_path, "${body}\n## Repository tooling compiler\n\n```\n${Str.from_utf8_lossy(tooling.stdout_bytes).trim()}\n```\n").map_err(|_| "cannot record tooling compiler")?
	Path.delete!(notes).map_err(|_| "cannot remove duplicate notes")?
	run!(["python3", "scripts/release_helpers.py", "package-docs", "--release-version", settings.version, "--output-dir", Path.display(dist), "--site", Path.display(Path.join(settings.directory, "site")), "--repo", settings.repository])?
	bundle = bundle!(dist)?
	run!(["scripts/release_sbom.roc", "--", "generate", "--bundle", Path.display(bundle), "--release-version", settings.version, "--output-dir", Path.display(dist), "--repo", settings.repository])?
	verify_sbom!(settings, bundle)?
	_ = files!(settings)?
	if settings.output != "" {
		output = Path.utf8(settings.output)
		before = if Path.exists!(output).map_err(|_| "cannot inspect GitHub output")? Path.read_utf8!(output).map_err(|_| "cannot read GitHub output")? else ""
		name = Path.filename(bundle).map_ok(Path.display).map_err(|_| "bundle has no filename")?
		Path.write_utf8!(output, "${before}bundle=${name}\n").map_err(|_| "cannot write GitHub output")?
	}
	Ok({})
}

test! = |args| {
	settings = settings!(args, "--directory")?
	found = files!(settings)?
	verify_sbom!(settings, found.bundle)?
	body = Path.read_utf8!(found.notes).map_err(|_| "cannot read release notes")?
	if body.contains("{package-url}") or !body.contains("https://github.com/${settings.repository}/releases/download/${settings.version}/${found.bundle_name}") {
		return Err("release notes do not name the bundle's release URL")
	}
	listing = Cmd.new("unzip").args(["-Z1", Path.display(found.docs)].map(OsStr.from_str)).stdout(Capture).stderr(Inherit).timeout_ms(60_000).run!().map_err(|_| "cannot inspect documentation archive")?
	if listing.status != Exited(0) {
		return Err("invalid documentation archive")
	}
	names = Str.from_utf8(listing.stdout_bytes).map_err(|_| "archive names must be UTF-8")?.split_on("\n")
	prefix = "roc-parser-docs-${settings.version}"
	for required in ["index.html", "api/index.html", "404.html"] {
		if !names.contains("${prefix}/${required}") {
			return Err("documentation archive is missing ${required}")
		}
	}
	utility!(["test-archive"], settings, Path.join(settings.directory, "dist"))
}

bundle! = |dist| {
	paths = Path.list!(dist).map_err(|_| "cannot list release files")?.keep_if(|path| Path.display(path).ends_with(".tar.zst"))
	match paths {
		[path] => Ok(path)
		_ => Err("expected exactly one package bundle")
	}
}

files! = |settings| {
	dist = Path.join(settings.directory, "dist")
	bundle = bundle!(dist)?
	bundle_name = Path.filename(bundle).map_ok(Path.display).map_err(|_| "bundle has no filename")?
	expected = [bundle_name, "${bundle_name}.spdx.json", "roc-parser-examples-${settings.version}.zip", "roc-parser-docs-${settings.version}.zip", "roc-parser-${settings.version}.pdf"]
	actual = Path.list!(dist).map_err(|_| "cannot list release files")?.map(|path| Path.filename(path).map_ok(Path.display) ?? "")
	if actual.len() != expected.len() or !expected.all(|name| actual.contains(name)) {
		return Err("release files are missing or unexpected: ${Str.inspect(actual)}")
	}
	for path in expected.map(|name| Path.join(dist, name)).append(Path.join(settings.directory, "release-notes.md")) {
		if !(Path.is_file!(path).map_err(|_| "cannot inspect release file")?) or (Path.size_in_bytes!(path).map_err(|_| "cannot measure release file")? == 0) {
			return Err("missing or empty release file: ${Path.display(path)}")
		}
	}
	Ok({ bundle, bundle_name, docs: Path.join(dist, "roc-parser-docs-${settings.version}.zip"), notes: Path.join(settings.directory, "release-notes.md") })
}

verify_sbom! = |settings, bundle| run!(["scripts/release_sbom.roc", "--", "verify", "--sbom", "${Path.display(bundle)}.spdx.json", "--bundle", Path.display(bundle), "--release-version", settings.version, "--repo", settings.repository])

utility! = |args, settings, dist| Cmd.new("release-package").args(args.map(OsStr.from_str)).env("RP_REPOSITORY", OsStr.from_str(settings.repository)).env("RP_OUTPUT", OsStr.from_str(Path.display(dist))).env("RP_COMPILER", "roc").env("RP_AUTOMATION", "lukewilliamboswell/roc-automation@8454f8ed9acc026c85e31e87b132dfeeaf9f1b81").env("GITHUB_OUTPUT", "").exec_cmd!().map_err(|_| "release-package failed: ${Str.join_with(args, " ")}")

run! = |command| {
	Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot print command")?
	match command {
		[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
		[] => Err("empty command")
	}
}

optional! = |name, fallback| match Env.var_str!(OsStr.from_str(name)) {
	Ok(value) if value != "" => Ok(value)
	Ok(_) | Err(VarNotFound(_)) => Ok(fallback)
	Err(_) => Err("${name} is not UTF-8")
}
