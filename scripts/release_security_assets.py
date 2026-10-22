#!/usr/bin/env python3
"""Release SBOM and attestation sidecars for the roc-parser bundle.

    release_security_assets.py sbom --bundle dist/<hash>.tar.zst --release-version 2.1.0 --output-dir dist
    release_security_assets.py verify --sbom dist/<hash>.tar.zst.spdx.json --bundle dist/<hash>.tar.zst \\
        --release-version 2.1.0
    release_security_assets.py combine-attestations --provenance-bundle P --sbom-bundle S --output O

`sbom` writes an SPDX 2.3 document for the bundle's exact bytes. It describes
the roc-parser package (the bundle's own modules) and, as runtime dependencies
that Roc fetches by URL and that the bundle does not contain, every package
`package/main.roc` names. `verify` checks a document against the bundle before
it is published. `combine-attestations` joins the provenance and SBOM
attestation bundles into one offline `.intoto.jsonl` file.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Sequence
from urllib.parse import quote


ROOT = Path(__file__).resolve().parents[1]
REPOSITORY = "lukewilliamboswell/roc-parser"
REPOSITORY_URL = f"https://github.com/{REPOSITORY}"
BUNDLE_SUFFIX = ".tar.zst"
PACKAGE_ID = "SPDXRef-Package-roc-parser"
# A dependency in the package header: `name: "https://github.com/<owner>/<repo>/releases/download/<version>/<hash>.tar.zst"`.
DEPENDENCY_RE = re.compile(
    r'(?m)^\s*([a-z][a-z0-9_]*):\s*"(https://github\.com/([^/"]+)/([^/"]+)/releases/download/([^/"]+)/([^/"]+\.tar\.zst))"'
)


class ReleaseSecurityError(Exception):
    pass


def is_semver_identifier(value: str) -> bool:
    return bool(value) and all(
        character.isascii() and (character.isalnum() or character == "-")
        for character in value
    )


def validate_semver(value: str) -> None:
    if value.count("+") > 1:
        raise ReleaseSecurityError(f"release version is not valid SemVer: {value!r}")
    version_and_prerelease, separator, build = value.partition("+")
    if separator and (
        not build or any(not is_semver_identifier(part) for part in build.split("."))
    ):
        raise ReleaseSecurityError(f"release version is not valid SemVer: {value!r}")

    core, separator, prerelease = version_and_prerelease.partition("-")
    core_parts = core.split(".")
    if len(core_parts) != 3 or any(
        not part.isascii()
        or not part.isdigit()
        or (len(part) > 1 and part.startswith("0"))
        for part in core_parts
    ):
        raise ReleaseSecurityError(f"release version is not valid SemVer: {value!r}")

    if separator:
        identifiers = prerelease.split(".")
        if any(
            not is_semver_identifier(identifier)
            or (identifier.isdigit() and len(identifier) > 1 and identifier.startswith("0"))
            for identifier in identifiers
        ):
            raise ReleaseSecurityError(f"release version is not valid SemVer: {value!r}")


def read_json(path: Path, description: str) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise ReleaseSecurityError(f"{description} is missing: {path}") from exc
    except json.JSONDecodeError as exc:
        raise ReleaseSecurityError(f"{description} is not valid JSON: {path}: {exc}") from exc


def validate_bundle(path: Path) -> Path:
    name = path.name
    if not name.endswith(BUNDLE_SUFFIX) or "#" in name or any(c in name for c in "\r\n"):
        raise ReleaseSecurityError(f"release bundle must be a <hash>{BUNDLE_SUFFIX} file: {name!r}")
    if not path.is_file():
        raise ReleaseSecurityError(f"release bundle is missing: {path}")
    return path.resolve()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def creation_time() -> str:
    source_date_epoch = os.environ.get("SOURCE_DATE_EPOCH")
    if source_date_epoch is None:
        created = datetime.now(timezone.utc)
    else:
        try:
            created = datetime.fromtimestamp(int(source_date_epoch), timezone.utc)
        except (ValueError, OverflowError) as exc:
            raise ReleaseSecurityError("SOURCE_DATE_EPOCH must be a valid integer timestamp") from exc
    return created.replace(microsecond=0).isoformat().replace("+00:00", "Z")


def package_dependencies(header: str) -> list[dict[str, str]]:
    """The released packages a package header names, by alias, in order."""
    return [
        {"alias": alias, "url": url, "owner": owner, "repo": repo, "version": version, "file": file}
        for alias, url, owner, repo, version, file in DEPENDENCY_RE.findall(header)
    ]


def release_url(release_version: str, artifact_file: str) -> str:
    return f"{REPOSITORY_URL}/releases/download/{quote(release_version, safe='.-')}/{quote(artifact_file, safe='.-')}"


def spdx_document(
    bundle_path: Path, release_version: str, dependencies: Sequence[dict[str, str]]
) -> dict[str, Any]:
    validate_semver(release_version)

    artifact_file = bundle_path.name
    sbom_file = f"{artifact_file}.spdx.json"
    packages: list[dict[str, Any]] = [
        {
            "name": "roc-parser",
            "SPDXID": PACKAGE_ID,
            "versionInfo": release_version,
            "packageFileName": artifact_file,
            "downloadLocation": release_url(release_version, artifact_file),
            "filesAnalyzed": False,
            "checksums": [{"algorithm": "SHA256", "checksumValue": sha256_file(bundle_path)}],
            "homepage": REPOSITORY_URL,
            "licenseConcluded": "UPL-1.0",
            "licenseDeclared": "UPL-1.0",
            "copyrightText": "NOASSERTION",
            "primaryPackagePurpose": "LIBRARY",
            "comment": "The Roc modules of package/ in the release bundle. Dependencies are fetched by "
                       "the Roc compiler from their own URLs and are not contained in the bundle.",
            "externalRefs": [
                {
                    "referenceCategory": "PACKAGE-MANAGER",
                    "referenceType": "purl",
                    "referenceLocator": f"pkg:github/{REPOSITORY}@{release_version}",
                }
            ],
        }
    ]
    relationships: list[dict[str, str]] = [
        {"spdxElementId": "SPDXRef-DOCUMENT", "relationshipType": "DESCRIBES", "relatedSpdxElement": PACKAGE_ID}
    ]
    for dependency in dependencies:
        identifier = f"SPDXRef-Package-{dependency['owner']}-{dependency['repo']}".replace("_", "-")
        packages.append({
            "name": f"{dependency['owner']}/{dependency['repo']}",
            "SPDXID": identifier,
            "versionInfo": dependency["version"],
            "packageFileName": dependency["file"],
            "downloadLocation": dependency["url"],
            "filesAnalyzed": False,
            "homepage": f"https://github.com/{dependency['owner']}/{dependency['repo']}",
            "licenseConcluded": "NOASSERTION",
            "licenseDeclared": "NOASSERTION",
            "copyrightText": "NOASSERTION",
            "primaryPackagePurpose": "LIBRARY",
            "comment": f"Imported by package/main.roc as `{dependency['alias']}`. Roc verifies the download "
                       "against the content hash in its file name.",
            "externalRefs": [
                {
                    "referenceCategory": "PACKAGE-MANAGER",
                    "referenceType": "purl",
                    "referenceLocator": f"pkg:github/{dependency['owner']}/{dependency['repo']}@{dependency['version']}",
                }
            ],
        })
        relationships.append(
            {"spdxElementId": PACKAGE_ID, "relationshipType": "DEPENDS_ON", "relatedSpdxElement": identifier}
        )

    return {
        "spdxVersion": "SPDX-2.3",
        "dataLicense": "CC0-1.0",
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": f"roc-parser-{release_version}",
        "documentNamespace": release_url(release_version, sbom_file),
        "creationInfo": {
            "created": creation_time(),
            "creators": ["Tool: roc-parser/scripts/release_security_assets.py"],
        },
        "packages": packages,
        "relationships": relationships,
    }


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def generate_sbom(
    *, bundle: Path, release_version: str, output_dir: Path, header: Path = ROOT / "package" / "main.roc"
) -> Path:
    bundle_path = validate_bundle(bundle)
    dependencies = package_dependencies(header.read_text(encoding="utf-8"))
    output_dir.mkdir(parents=True, exist_ok=True)
    sbom_path = output_dir.resolve() / f"{bundle_path.name}.spdx.json"
    write_json(sbom_path, spdx_document(bundle_path, release_version, dependencies))
    return sbom_path


def verify_sbom(
    *, sbom: Path, bundle: Path, release_version: str, header: Path = ROOT / "package" / "main.roc"
) -> None:
    """The document is SPDX 2.3, describes exactly this bundle's bytes under
    this version, and names every dependency of the package header."""
    bundle_path = validate_bundle(bundle)
    document = read_json(sbom, "SBOM")
    problems: list[str] = []
    if not isinstance(document, dict):
        raise ReleaseSecurityError("SBOM must be a JSON object")
    if sbom.name != f"{bundle_path.name}.spdx.json":
        problems.append(f"SBOM file name {sbom.name} does not name the bundle {bundle_path.name}")
    if document.get("spdxVersion") != "SPDX-2.3" or document.get("SPDXID") != "SPDXRef-DOCUMENT":
        problems.append("not an SPDX-2.3 document")
    packages = document.get("packages") if isinstance(document.get("packages"), list) else []
    identifiers = [package.get("SPDXID") for package in packages if isinstance(package, dict)]
    if len(identifiers) != len(packages) or len(set(identifiers)) != len(identifiers):
        problems.append("package identifiers are missing or repeated")
    by_id = {package["SPDXID"]: package for package in packages if isinstance(package, dict) and "SPDXID" in package}
    relationships = document.get("relationships") if isinstance(document.get("relationships"), list) else []
    known = set(by_id) | {"SPDXRef-DOCUMENT"}
    for relationship in relationships:
        if not isinstance(relationship, dict) or {relationship.get("spdxElementId"), relationship.get("relatedSpdxElement")} - known:
            problems.append(f"relationship refers to an unknown element: {relationship}")
    described = [r.get("relatedSpdxElement") for r in relationships
                 if isinstance(r, dict) and r.get("spdxElementId") == "SPDXRef-DOCUMENT" and r.get("relationshipType") == "DESCRIBES"]
    if described != [PACKAGE_ID]:
        problems.append(f"the document must describe exactly {PACKAGE_ID}, found {described}")
    subject = by_id.get(PACKAGE_ID, {})
    if subject.get("versionInfo") != release_version:
        problems.append(f"subject version {subject.get('versionInfo')!r} is not {release_version!r}")
    if subject.get("packageFileName") != bundle_path.name:
        problems.append(f"subject file {subject.get('packageFileName')!r} is not {bundle_path.name!r}")
    if subject.get("downloadLocation") != release_url(release_version, bundle_path.name):
        problems.append(f"subject download location {subject.get('downloadLocation')!r} is not the release URL")
    if subject.get("checksums") != [{"algorithm": "SHA256", "checksumValue": sha256_file(bundle_path)}]:
        problems.append("subject SHA-256 does not match the bundle")
    depends_on = {r.get("relatedSpdxElement") for r in relationships
                  if isinstance(r, dict) and r.get("spdxElementId") == PACKAGE_ID and r.get("relationshipType") == "DEPENDS_ON"}
    for dependency in package_dependencies(header.read_text(encoding="utf-8")):
        matches = [identifier for identifier, package in by_id.items() if package.get("downloadLocation") == dependency["url"]]
        if len(matches) != 1 or matches[0] not in depends_on:
            problems.append(f"dependency {dependency['url']} is not recorded once as a DEPENDS_ON package")
    if problems:
        raise ReleaseSecurityError(f"{sbom}: " + "; ".join(problems))


def sigstore_bundle(path: Path, description: str) -> dict[str, Any]:
    value = read_json(path, description)
    if not isinstance(value, dict) or not value:
        raise ReleaseSecurityError(f"{description} must be a non-empty JSON object: {path}")
    return value


def combine_attestations(
    *, provenance_bundle: Path, sbom_bundle: Path, output: Path
) -> Path:
    provenance = sigstore_bundle(provenance_bundle, "provenance attestation bundle")
    sbom = sigstore_bundle(sbom_bundle, "SBOM attestation bundle")
    output.parent.mkdir(parents=True, exist_ok=True)
    records = (
        json.dumps(provenance, separators=(",", ":"), sort_keys=True),
        json.dumps(sbom, separators=(",", ":"), sort_keys=True),
    )
    output.write_text("\n".join(records) + "\n", encoding="utf-8")
    return output.resolve()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subcommands = parser.add_subparsers(dest="command", required=True)

    sbom = subcommands.add_parser("sbom", help="Create an SPDX 2.3 SBOM for one bundle")
    sbom.add_argument("--bundle", type=Path, required=True)
    sbom.add_argument("--release-version", required=True)
    sbom.add_argument("--output-dir", type=Path, required=True)

    verify = subcommands.add_parser("verify", help="Check an SBOM against the bundle it describes")
    verify.add_argument("--sbom", type=Path, required=True)
    verify.add_argument("--bundle", type=Path, required=True)
    verify.add_argument("--release-version", required=True)

    combine = subcommands.add_parser(
        "combine-attestations", help="Combine provenance and SBOM Sigstore bundles"
    )
    combine.add_argument("--provenance-bundle", type=Path, required=True)
    combine.add_argument("--sbom-bundle", type=Path, required=True)
    combine.add_argument("--output", type=Path, required=True)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        if args.command == "sbom":
            print(generate_sbom(bundle=args.bundle, release_version=args.release_version, output_dir=args.output_dir))
        elif args.command == "verify":
            verify_sbom(sbom=args.sbom, bundle=args.bundle, release_version=args.release_version)
            print(f"{args.sbom} describes {args.bundle.name}")
        elif args.command == "combine-attestations":
            print(combine_attestations(
                provenance_bundle=args.provenance_bundle,
                sbom_bundle=args.sbom_bundle,
                output=args.output,
            ))
        else:  # pragma: no cover - argparse enforces the subcommand choices.
            parser.error(f"unknown command: {args.command}")
    except (OSError, ReleaseSecurityError) as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
