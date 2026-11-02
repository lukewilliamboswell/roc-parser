#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/Attestations
import src/ReleasePlan

## Join the two inert Sigstore bundles for offline verification. Validate both
## before creating output, and preserve all original JSON value tokens.
main! = |args| {
	match combine!(args.map(OsStr.display)) {
		Ok(path) => Stdout.line!(Path.display(path))
		Err(reason) => {
			Stderr.line!("attestations: ${reason}")?
			Err(Exit(1))
		}
	}
}

combine! = |args| {
	options = ReleasePlan.options_for(args, ["--provenance-bundle", "--sbom-bundle", "--output"])?
	provenance = ReleasePlan.option(options, "--provenance-bundle", "")
	sbom = ReleasePlan.option(options, "--sbom-bundle", "")
	output = ReleasePlan.option(options, "--output", "")
	if provenance == "" or sbom == "" or output == "" {
		return Err("both input bundles and --output are required")
	}
	first = validated!(Path.utf8(provenance))?
	second = validated!(Path.utf8(sbom))?
	parent = match output.split_last("/") {
		Ok({ before, .. }) => if before == "" "/" else before
		Err(_) => "."
	}
	Path.create_all!(Path.utf8(parent)).map_err(|_| "cannot create output directory")?
	path = Path.utf8(output)
	Path.write_utf8!(path, "${first}\n${second}\n").map_err(|_| "cannot write combined attestations")?
	Ok(path)
}

validated! = |path| {
	source = Path.read_utf8!(path).map_err(|_| "cannot read attestation bundle")?
	if source.starts_with("\u(FEFF)") {
		return Err("attestation JSON must not contain a byte-order mark")
	}
	checked = Cmd.new("jq").args(["-e", "--arg", "object_type", "object", "--slurp", "length == 1 and (.[0] | type) == $object_type and (.[0] | length) > 0"].map(OsStr.from_str)).stdin(Bytes(source.to_utf8())).stdout(Capture).stderr(Capture).timeout_ms(60_000).output_limit(1_000_000).run!().map_err(|_| "jq did not complete")?
	if checked.status != Exited(0) {
		return Err("attestation must be exactly one non-empty JSON object: ${Path.display(path)}")
	}
	Ok(Attestations.compact_valid_json(source))
}
