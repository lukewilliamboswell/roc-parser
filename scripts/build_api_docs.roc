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
import src/ApiDocs
import src/ReleasePlan

## Generate with the package compiler, normalize its HTML, then audit independently.
main! = |args| match build!(args.map(OsStr.display)) {
	Ok(path) => Stdout.line!("API documentation built and audited: ${Path.display(path)}")
	Err(reason) => {
		Stderr.line!("API documentation: ${reason}")?
		Err(Exit(1))
	}
}

build! = |args| {
	options = ReleasePlan.options_for(args.keep_if(|arg| arg != "--check"), ["--roc", "--docs-root", "--version"])?
	roc = ReleasePlan.option(options, "--roc", Env.var_str!("ROC") ?? "roc")
	root = Path.utf8(ReleasePlan.option(options, "--docs-root", ".docs-api"))
	version = ReleasePlan.option(options, "--version", "")
	checking = args.contains("--check")
	if !checking and version != "unreleased" and !ReleasePlan.valid_version(version) {
		return Err("--version must be a release version unless --check is given")
	}
	Path.create_all!(".roc-parser-tmp/api-docs").map_err(|_| "cannot create API workspace")?
	work = Env.create_temp_dir_in!(".roc-parser-tmp/api-docs", "build-").map_err(|_| "cannot create fresh API workspace")?
	generated = Path.join(work, "site")
	Cmd.new(OsStr.from_str(roc)).args(["docs", "package/main.roc", "--output=${Path.display(generated)}"].map(OsStr.from_str)).exec_cmd!().map_err(|_| "roc docs failed")?
	index = Path.join(generated, "index.html")
	index_source = Path.read_utf8!(index).map_err(|_| "cannot read compiler API index")?
	old_name = ApiDocs.site_name(index_source)?
	entry = Path.read_utf8!("package/main.roc").map_err(|_| "cannot read package entry")?
	Path.write_utf8!(index, ApiDocs.index_body(index_source, entry)?).map_err(|_| "cannot write API introduction")?
	# roc docs writes each exposed module immediately below the root.
	var $pages = [index]
	for path in Path.list!(generated).map_err(|_| "cannot list API pages")? {
		candidate = Path.join(path, "index.html")
		if Path.is_dir!(path).map_err(|_| "cannot inspect API module")? and Path.is_file!(candidate).map_err(|_| "cannot inspect API page")? {
			$pages = $pages.append(candidate)
		}
	}
	for page in $pages {
		text = Path.read_utf8!(page).map_err(|_| "cannot read API page")?
		renamed = ApiDocs.name_site(text, old_name, "roc-parser")
		module_name = Path.display(page).split_on("/").drop_last(1).last() ?? ""
		normalized = if page == index renamed else ApiDocs.normalize_module(renamed, module_name)
		Path.write_utf8!(page, normalized).map_err(|_| "cannot normalize API page")?
	}
	Cmd.new("python3").args(["scripts/check_api_docs.py", Path.display(generated)].map(OsStr.from_str)).exec_cmd!().map_err(|_| "API documentation audit failed")?
	if checking {
		Path.delete_all!(work).map_err(|_| "cannot remove successful preview")?
		Ok(Path.utf8("preview"))
	} else {
		Path.create_all!(root).map_err(|_| "cannot create API destination")?
		destination = Path.join(root, version)
		if Path.exists!(destination).map_err(|_| "cannot inspect API destination")? {
			Path.delete_all!(destination).map_err(|_| "cannot replace API version")?
		}
		Cmd.new("cp").args(["-R", Path.display(generated), Path.display(destination)].map(OsStr.from_str)).exec_cmd!().map_err(|_| "cannot place API documentation")?
		Path.delete_all!(work).map_err(|_| "cannot remove API workspace")?
		Ok(destination)
	}
}
