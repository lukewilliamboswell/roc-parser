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
import src/DocsSite
import src/ReleasePlan

## Compose the shared native manual with the audited compiler API reference.
main! = |args| match package!(args.map(OsStr.display)) {
	Ok(path) => Stdout.line!("Documentation site: ${Path.display(path)}")
	Err(reason) => {
		Stderr.line!("documentation: ${reason}")?
		Err(Exit(1))
	}
}

package! = |args| {
	options = ReleasePlan.options_for(args, ["--release-version", "--output-dir", "--site", "--repo", "--roc"])?
	version = ReleasePlan.option(options, "--release-version", "unreleased")
	if version != "unreleased" and !ReleasePlan.valid_version(version) {
		return Err("invalid documentation version")
	}
	repository = ReleasePlan.option(options, "--repo", Env.var_str!("GITHUB_REPOSITORY") ?? "lukewilliamboswell/roc-parser")
	if !ReleasePlan.repository(repository) {
		return Err("repository must be owner/name")
	}
	roc = ReleasePlan.option(options, "--roc", Env.var_str!("ROC") ?? "roc")
	output = Path.absolute!(Path.utf8(ReleasePlan.option(options, "--output-dir", ".docs-out/release"))).map_err(|_| "cannot resolve documentation output")?
	site = Path.absolute!(Path.utf8(ReleasePlan.option(options, "--site", ".docs-out/release/site"))).map_err(|_| "cannot resolve site destination")?
	if output == site or Path.display(output).starts_with("${Path.display(site)}/") {
		return Err("archive output must be outside the replaced site directory")
	}
	root = Path.absolute!(".").map_err(|_| "cannot resolve workspace")?
	# A site is replaced as one directory. Keep recursive cleanup within known scratch roots.
	if !DocsSite.safe_site_destination(Path.display(root), Path.display(site)) {
		return Err("site destination must be a child of .docs-out or .roc-parser-tmp")
	}
	Path.create_all!(".roc-parser-tmp/docs").map_err(|_| "cannot create documentation workspace")?
	work = Env.create_temp_dir_in!(".roc-parser-tmp/docs", "build-").map_err(|_| "cannot create fresh documentation workspace")?
	absolute_work = Path.absolute!(work).map_err(|_| "cannot resolve documentation workspace")?
	manual = Path.join(absolute_work, "manual")
	api = Path.join(absolute_work, "api")
	run!(["scripts/build_api_docs.roc", "--", "--roc", roc, "--docs-root", Path.display(api), "--version", version], ".")?
	run!(["roc-docs", "build", "--workspace", Path.display(root), "--docs-directory", "docs", "--entrypoint", "index.adoc", "--output-directory", Path.display(manual).drop_prefix("${Path.display(root)}/"), "--title", "roc-parser", "--docs-version", version, "--pdf-filename", "roc-parser.pdf"], ".")?
	manual_site = Path.join(manual, "site")
	api_version = Path.join(api, version)
	for directory in [manual_site, api_version] {
		if !Path.is_file!(Path.join(directory, "index.html")).map_err(|_| "cannot inspect documentation index")? {
			return Err("documentation has no index.html")
		}
	}
	if Path.exists!(Path.join(manual_site, "api")).map_err(|_| "cannot inspect manual layout")? {
		return Err("the manual already has an api/ directory")
	}
	prefix = "roc-parser-docs-${version}"
	stage = Path.join(absolute_work, prefix)
	run!(["cp", "-R", Path.display(manual_site), Path.display(stage)], ".")?
	run!(["cp", "-R", Path.display(api_version), Path.display(Path.join(stage, "api"))], ".")?
	Path.write_utf8!(Path.join(stage, "404.html"), DocsSite.not_found(repository)).map_err(|_| "cannot write missing-page guidance")?
	run!(["python3", "scripts/check_manual_content.py", Path.display(stage)], ".")?
	Path.create_all!(output).map_err(|_| "cannot create documentation output")?
	Path.copy!(Path.join(manual, "roc-parser.pdf"), Path.join(output, "roc-parser-${version}.pdf")).map_err(|_| "cannot place manual PDF")?
	# Stable archive names, modes and timestamps; do not mutate the shared renderer output.
	files = DocsSite.sorted(regular_files!(stage, "")?)
	if files.is_empty() {
		return Err("documentation site is empty")
	}
	full_paths = files.map(|file| Path.display(Path.join(stage, file)))
	run!(["chmod", "0644"].concat(full_paths), ".")?
	run!(["env", "TZ=UTC", "touch", "-t", "198001010000"].concat(full_paths), ".")?
	archive = Path.join(output, "${prefix}.zip")
	if Path.exists!(archive).map_err(|_| "cannot inspect previous documentation archive")? {
		Path.delete!(archive).map_err(|_| "cannot replace documentation archive")?
	}
	listing = Str.join_with(files.map(|file| "${prefix}/${file}"), "\n")
	packed = Cmd.new("env").args(["TZ=UTC", "zip", "-q", "-X", "-D", Path.display(archive), "-@"].map(OsStr.from_str)).cwd(absolute_work).stdin(Bytes(listing.to_utf8())).run!().map_err(|_| "zip did not complete")?
	if packed.status != Exited(0) {
		return Err("documentation archive failed")
	}
	if Path.exists!(site).map_err(|_| "cannot inspect previous site")? {
		Path.delete_all!(site).map_err(|_| "cannot replace documentation site")?
	}
	parent = Path.display(site).split_last("/").map_ok(|parts| parts.before).map_err(|_| "site has no parent")?
	Path.create_all!(Path.utf8(parent)).map_err(|_| "cannot create site parent")?
	run!(["cp", "-R", Path.display(stage), Path.display(site)], ".")?
	Path.delete_all!(work).map_err(|_| "cannot remove documentation workspace")?
	Ok(site)
}

regular_files! = |root, prefix| {
	var $files = []
	for path in Path.list!(root).map_err(|_| "cannot list documentation files")? {
		name = Path.filename(path).map_ok(Path.display).map_err(|_| "documentation file has no name")?
		relative = if prefix == "" name else "${prefix}/${name}"
		match Path.type!(path).map_err(|_| "cannot inspect documentation file")? {
			IsDir => {
				$files = $files.concat(regular_files!(path, relative)?)
			}
			IsFile => {
				$files = $files.append(relative)
			}
			_ => return Err("documentation must contain regular files and directories")
		}
	}
	Ok($files)
}

run! = |command, cwd| match command {
	[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).cwd(Path.utf8(cwd)).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
	[] => Err("empty command")
}
