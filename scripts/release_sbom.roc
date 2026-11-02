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
import src/Inventory
import src/ReleasePlan
import "release/dependencies.json" as dependency_json : Str

## Bind the shared inventory generator to this package's actual source bundle.
## Verification independently recomputes the expected graph from the bundle,
## working-tree source, reviewed supplier metadata, Spec and Lock.
main! = |args| {
	result = match args.map(OsStr.display) {
		[mode, .. as rest] if mode == "generate" or mode == "verify" => inventory!(mode, rest)
		_ => Err("usage: release_sbom.roc -- generate|verify --bundle FILE --release-version VERSION [--output-dir DIR | --sbom FILE] [--repo owner/name]")
	}
	match result {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("SBOM: ${reason}")?
			Err(Exit(1))
		}
	}
}

inventory! = |mode, args| {
	options = ReleasePlan.options_for(args, ["--bundle", "--release-version", "--output-dir", "--sbom", "--repo"])?
	version = ReleasePlan.option(options, "--release-version", "")
	if !ReleasePlan.valid_version(version) {
		return Err("invalid release version")
	}
	repository = ReleasePlan.option(
		options,
		"--repo",
		match Env.var_str!("GITHUB_REPOSITORY") {
			Ok(value) if value != "" => value
			_ => "lukewilliamboswell/roc-parser"
		},
	)
	if !ReleasePlan.repository(repository) {
		return Err("repository must be owner/name")
	}
	given = ReleasePlan.option(options, "--bundle", "")
	if given == "" {
		return Err("--bundle is required")
	}
	bundle = Path.absolute!(Path.utf8(given)).map_err(|_| "cannot resolve bundle")?
	name = Path.filename(bundle).map_ok(Path.display).map_err(|_| "bundle has no filename")?
	if !Inventory.valid_bundle_name(name) {
		return Err("bundle must have a safe .tar.zst filename")
	}
	base = Path.utf8(".roc-parser-tmp/release-sbom")
	Path.create_all!(base).map_err(|_| "cannot create inventory workspace")?
	work = Env.create_temp_dir_in!(base, "${mode}-").map_err(|_| "cannot create fresh inventory workspace")?
	dependencies = capture!(["roc", "deps", "package/main.roc"])?
	if !Inventory.known_dependencies(dependencies) {
		return Err("package dependency closure changed; review scripts/release/dependencies.json before releasing")
	}
	listing = Path.join(work, "members.json")
	run!(["blueprint-sbom", "members", Path.display(bundle), Path.display(listing)])?
	parsed : Try({ archive : Str, sha256 : Str, members : List({ name : Str, size : U64, sha256 : Str }) }, _)
	parsed = Json.parse(Path.read_utf8!(listing).map_err(|_| "cannot read bundle member listing")?)
	listed = parsed.map_err(|_| "invalid bundle member listing")?
	for member in listed.members {
		# Each shipped file must be the exact source in this checkout, not an
		# opaque addition smuggled into an otherwise well-formed archive.
		digest = capture!(["openssl", "dgst", "-sha256", "package/${member.name}"])?
		found = digest.trim().split_on(" ").last() ?? ""
		if found != member.sha256 {
			return Err("bundle member differs from working-tree source: ${member.name}")
		}
	}
	if listed.members.is_empty() {
		return Err("empty package bundle")
	}
	spec_source = capture!(["blueprint", "spec"])?
	supplier : Try({ components : List(Component), relationships : List(Edge), inputs : List({ input : Str, component : Str, reason : Str }) }, _)
	supplier = Json.parse(dependency_json)
	reviewed = supplier.map_err(|_| "invalid reviewed dependency inventory")?
	member_components = listed.members.map_with_index(
		|member, index| {
			id: "source-file-${index.to_str()}",
			name: member.name,
			producer: "Organization: roc-parser contributors",
			version: "sha256-${member.sha256}",
			identity: "urn:roc-parser:source-member:${index.to_str()}:${member.sha256}",
			download: "NOASSERTION",
			sha256: member.sha256,
			license: "UPL-1.0",
			license_url: "",
			modified: "",
			target: "source",
			origin: "reviewed",
		},
	)
	member_edges = listed.members.map_with_index(|member, index| { from: "roc-parser", to: "source-file-${index.to_str()}", relation: "contains", scope: "", basis: "Archive member ${member.name}, ${member.size.to_str()} bytes, matching this checkout's source by SHA-256." })
	package_component : Component
	package_component = {
		id: "roc-parser",
		name: "roc-parser",
		version,
		producer: "Organization: roc-parser contributors",
		license: "UPL-1.0",
		license_url: "",
		identity: "urn:roc-parser:source:${listed.sha256}",
		download: "https://github.com/${repository}/releases/download/${version}/${name}",
		sha256: "",
		modified: "Source modules of the delivered bundle; tooling and separately fetched dependencies are not contained in it.",
		target: "source",
		origin: "reviewed",
	}
	empty : List(Str)
	empty = []
	composition = {
		profile: "blueprint-spdx-3.0-v1",
		components: [package_component].concat(reviewed.components).concat(member_components),
		relationships: [{ from: "Subject", to: "roc-parser", relation: "contains", scope: "", basis: "The source bundle contains this package's modules; every archive member is accounted for below." }].concat(reviewed.relationships).concat(member_edges),
		coverage: [
			{ scope: "source-members", status: "complete", reason: "Every archive member is listed by the shared tool and its SHA-256 matches the working-tree package source.", contact: "https://github.com/${repository}/issues" },
			{ scope: "package-dependencies", status: "complete", reason: "The package compiler's dependency tree matches the reviewed Unicode 4.3.0 closure. Unicode's production data inputs are recorded from its vendor manifest; conformance-only inputs are excluded.", contact: "https://github.com/${repository}/issues" },
			{ scope: "build-tooling", status: "excluded", reason: "This is a source-only deliverable. The inventory records why each Blueprint input is excluded; no build tool or test platform is shipped in the archive.", contact: "https://github.com/${repository}/issues" },
		],
		inputs: reviewed.inputs,
		imported_documents: empty,
		unavailable_sboms: empty,
	}
	composition_path = Path.join(work, "composition.json")
	Path.write_utf8!(composition_path, Json.to_str(composition)).map_err(|_| "cannot write reviewed composition")?
	spec_path = Path.join(work, "Spec")
	Path.write_utf8!(spec_path, spec_source).map_err(|_| "cannot write evaluated Spec")?
	inventory_path = Path.join(work, "inventory.json")
	run!(["blueprint-sbom", "inspect", Path.display(spec_path), "Blueprint.lock", Path.display(composition_path), "source", Path.display(inventory_path), "--members", Path.display(listing)])?
	parent_text = Path.display(bundle).split_last("/").map_ok(|parts| if (parts.before == "") "/" else parts.before).map_err(|_| "bundle has no parent")?
	parent = Path.utf8(parent_text)
	sbom = if mode == "generate" {
		output = Path.utf8(ReleasePlan.option(options, "--output-dir", Path.display(parent)))
		Path.create_all!(output).map_err(|_| "cannot create SBOM output directory")?
		Path.join(output, "${name}.spdx.json")
	} else {
		given_sbom = ReleasePlan.option(options, "--sbom", "")
		if given_sbom == "" {
			return Err("--sbom is required for verification")
		}
		path = Path.utf8(given_sbom)
		if Path.filename(path).map_ok(Path.display) != Ok("${name}.spdx.json") {
			return Err("SBOM filename does not identify its bundle")
		}
		path
	}
	if mode == "generate" {
		created = capture_with_utc!(["git", "show", "-s", "--date=format-local:%Y-%m-%dT%H:%M:%SZ", "--format=%cd", "HEAD"])?
		context = { namespace: "https://github.com/${repository}/sbom/${version}/${listed.sha256}/revision-1", revision: "1", created: created.trim(), author: "Organization: roc-parser contributors", generator: "Tool: blueprint-sbom-0.11.0" }
		subject = { filename: name, version, target: "source", sha256: listed.sha256, url: "https://github.com/${repository}/releases/download/${version}/${name}" }
		context_path = Path.join(work, "context.json")
		subject_path = Path.join(work, "subject.json")
		Path.write_utf8!(context_path, Json.to_str(context)).map_err(|_| "cannot write inventory context")?
		Path.write_utf8!(subject_path, Json.to_str(subject)).map_err(|_| "cannot write inventory subject")?
		if Path.exists!(sbom).map_err(|_| "cannot inspect old sidecar")? {
			Path.delete!(sbom).map_err(|_| "cannot replace old local sidecar")?
		}
		run!(["blueprint-sbom", "bind", Path.display(inventory_path), Path.display(subject_path), Path.display(context_path), Path.display(bundle), Path.display(sbom), "--spdx-2.3"])?
	}
	run!(["blueprint-sbom", "check", "--artifacts", Path.display(parent), "--inventory", Path.display(inventory_path), Path.display(sbom)])?
	# The generic inventory has no release version. Check the subject identity
	# against this request as well as the shared graph/digest/schema checks.
	run!(["jq", "-e", "--arg", "version", version, "--arg", "url", "https://github.com/${repository}/releases/download/${version}/${name}", "--arg", "subject", "SPDXRef-Subject", ".packages[] | select(.SPDXID == $subject) | .versionInfo == $version and .downloadLocation == $url", Path.display(sbom)])
}

capture_with_utc! = |command| match command {
	[program, .. as args] => {
		ran = Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).env("TZ", "UTC").env("NO_COLOR", "1").stdout(Capture).stderr(Capture).timeout_ms(120_000).output_limit(10_000_000).run!().map_err(|_| "${program} did not complete")?
		if ran.status != Exited(0) {
			return Err("${program} failed: ${Str.from_utf8_lossy(ran.stderr_bytes)}")
		}
		Str.from_utf8(ran.stdout_bytes).map_err(|_| "${program} output must be UTF-8")
	}
	[] => Err("empty command")
}

capture! = |command| capture_with_utc!(command)

run! = |command| {
	Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot print command")?
	match command {
		[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
		[] => Err("empty command")
	}
}

Component : { id : Str, name : Str, producer : Str, version : Str, identity : Str, download : Str, sha256 : Str, license : Str, license_url : Str, modified : Str, target : Str, origin : Str }

Edge : { from : Str, to : Str, relation : Str, scope : Str, basis : Str }
