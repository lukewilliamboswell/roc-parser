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
import "release/dependencies.json" as dependency_json : Str

## Account for the package source and reviewed Unicode inputs in the release.
## The shared workflow validates this composition and binds the SBOM sidecars.
main! = |_args| {
	match composition!() {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("SBOM composition: ${reason}")?
			Err(Exit(1))
		}
	}
}

composition! = || {
	version = Env.var_str!("RELEASE_VERSION").map_err(|_| "RELEASE_VERSION is required")?
	bundle_url = Env.var_str!("BUNDLE_URL").map_err(|_| "BUNDLE_URL is required")?
	filename = Env.var_str!("BUNDLE_PATH").map_err(|_| "BUNDLE_PATH is required")?
	bundle = Path.absolute!(Path.utf8(filename)).map_err(|_| "cannot resolve bundle")?
	output = Path.utf8(Env.var_str!("SBOM_COMPOSITION").map_err(|_| "SBOM_COMPOSITION is required")?)
	repository = "lukewilliamboswell/roc-parser"
	base = Path.utf8(".roc-parser-tmp/release-sbom")
	Path.create_all!(base).map_err(|_| "cannot create inventory workspace")?
	work = Env.create_temp_dir_in!(base, "composition-").map_err(|_| "cannot create fresh inventory workspace")?
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
		download: bundle_url,
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
			{ scope: "package-dependencies", status: "complete", reason: "The package compiler's dependency tree matches the reviewed Unicode 4.3.2 closure. Unicode's production data inputs are recorded from its vendor manifest; conformance-only inputs are excluded.", contact: "https://github.com/${repository}/issues" },
			{ scope: "build-tooling", status: "excluded", reason: "This is a source-only deliverable. The inventory records why each Blueprint input is excluded; no build tool or test platform is shipped in the archive.", contact: "https://github.com/${repository}/issues" },
		],
		inputs: reviewed.inputs,
		imported_documents: empty,
		unavailable_sboms: empty,
	}
	Path.write_utf8!(output, Json.to_str(composition)).map_err(|_| "cannot write reviewed composition")
}

capture! = |command| match command {
	[program, .. as args] => {
		ran = Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).env("NO_COLOR", "1").stdout(Capture).stderr(Capture).timeout_ms(120_000).output_limit(10_000_000).run!().map_err(|_| "${program} did not complete")?
		if ran.status != Exited(0) {
			return Err("${program} failed: ${Str.from_utf8_lossy(ran.stderr_bytes)}")
		}
		Str.from_utf8(ran.stdout_bytes).map_err(|_| "${program} output must be UTF-8")
	}
	[] => Err("empty command")
}

run! = |command| {
	Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot print command")?
	match command {
		[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
		[] => Err("empty command")
	}
}

Component : { id : Str, name : Str, producer : Str, version : Str, identity : Str, download : Str, sha256 : Str, license : Str, license_url : Str, modified : Str, target : Str, origin : Str }

Edge : { from : Str, to : Str, relation : Str, scope : Str, basis : Str }
