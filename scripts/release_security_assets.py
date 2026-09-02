#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Sequence
from urllib.parse import quote


REPOSITORY = "lukewilliamboswell/roc-parser"
REPOSITORY_URL = f"https://github.com/{REPOSITORY}"
BUNDLE_SUFFIX = ".tar.zst"
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


def validate_artifact_file(value: Any) -> str:
    if not isinstance(value, str) or not value:
        raise ReleaseSecurityError("release bundle entry has invalid artifact_file")
    if "\n" in value or "\r" in value:
        raise ReleaseSecurityError("release artifact filename contains a newline")
    if Path(value).name != value or "/" in value or "\\" in value:
        raise ReleaseSecurityError(f"release artifact filename must be a basename: {value!r}")
    if "#" in value:
        raise ReleaseSecurityError(f"release artifact filename must not contain '#': {value!r}")
    if not value.endswith(BUNDLE_SUFFIX):
        raise ReleaseSecurityError(
            f"release artifact filename must end with {BUNDLE_SUFFIX}: {value!r}"
        )
    return value


def load_single_bundle(bundle_dir: Path, release_list_file: Path) -> Path:
    release_list = read_json(release_list_file, "release bundle list")
    if not isinstance(release_list, list) or len(release_list) != 1:
        raise ReleaseSecurityError("release bundle list must contain exactly one bundle")
    entry = release_list[0]
    if not isinstance(entry, dict):
        raise ReleaseSecurityError("release bundle list entry must be an object")

    artifact_file = validate_artifact_file(entry.get("artifact_file"))
    bundle_path = (bundle_dir / artifact_file).resolve()
    if not bundle_path.is_file():
        raise ReleaseSecurityError(f"release bundle is missing: {bundle_path}")
    return bundle_path


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


def spdx_document(bundle_path: Path, release_version: str) -> dict[str, Any]:
    validate_semver(release_version)

    artifact_file = bundle_path.name
    checksum = sha256_file(bundle_path)
    encoded_version = quote(release_version, safe=".-")
    encoded_artifact = quote(artifact_file, safe=".-")
    release_url = f"{REPOSITORY_URL}/releases/download/{encoded_version}/{encoded_artifact}"
    sbom_file = f"{artifact_file}.spdx.json"
    namespace = (
        f"{REPOSITORY_URL}/releases/download/{encoded_version}/"
        f"{quote(sbom_file, safe='.-')}"
    )

    return {
        "spdxVersion": "SPDX-2.3",
        "dataLicense": "CC0-1.0",
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": f"roc-parser-{release_version}",
        "documentNamespace": namespace,
        "creationInfo": {
            "created": creation_time(),
            "creators": ["Tool: roc-parser/scripts/release_security_assets.py"],
        },
        "packages": [
            {
                "name": "roc-parser",
                "SPDXID": "SPDXRef-Package-roc-parser",
                "versionInfo": release_version,
                "packageFileName": artifact_file,
                "downloadLocation": release_url,
                "filesAnalyzed": False,
                "checksums": [
                    {
                        "algorithm": "SHA256",
                        "checksumValue": checksum,
                    }
                ],
                "homepage": REPOSITORY_URL,
                "licenseConcluded": "UPL-1.0",
                "licenseDeclared": "UPL-1.0",
                "copyrightText": "NOASSERTION",
                "primaryPackagePurpose": "LIBRARY",
                "externalRefs": [
                    {
                        "referenceCategory": "PACKAGE-MANAGER",
                        "referenceType": "purl",
                        "referenceLocator": f"pkg:github/{REPOSITORY}@{release_version}",
                    }
                ],
            }
        ],
        "relationships": [
            {
                "spdxElementId": "SPDXRef-DOCUMENT",
                "relationshipType": "DESCRIBES",
                "relatedSpdxElement": "SPDXRef-Package-roc-parser",
            }
        ],
    }


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def append_github_outputs(path: Path, values: dict[str, Path]) -> None:
    lines: list[str] = []
    for name, value in values.items():
        text = str(value)
        if "\n" in text or "\r" in text:
            raise ReleaseSecurityError(f"GitHub output {name} contains a newline")
        lines.append(f"{name}={text}\n")
    try:
        with path.open("a", encoding="utf-8") as output:
            output.writelines(lines)
    except OSError as exc:
        raise ReleaseSecurityError(f"could not write GitHub outputs to {path}: {exc}") from exc


def generate_sbom(
    *,
    bundle_dir: Path,
    release_list_file: Path,
    release_version: str,
    output_dir: Path,
    github_output: Path,
) -> tuple[Path, Path, Path]:
    bundle_path = load_single_bundle(bundle_dir, release_list_file)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_dir = output_dir.resolve()
    sbom_path = output_dir / f"{bundle_path.name}.spdx.json"
    attestation_path = output_dir / f"{bundle_path.name}.intoto.jsonl"

    write_json(sbom_path, spdx_document(bundle_path, release_version))
    append_github_outputs(
        github_output,
        {
            "bundle-path": bundle_path,
            "sbom-path": sbom_path,
            "attestation-path": attestation_path,
        },
    )
    return bundle_path, sbom_path, attestation_path


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
    parser = argparse.ArgumentParser(
        description="Create release SBOM and attestation sidecar assets."
    )
    subcommands = parser.add_subparsers(dest="command", required=True)

    sbom = subcommands.add_parser("sbom", help="Create an SPDX 2.3 SBOM for one bundle")
    sbom.add_argument("--bundle-dir", type=Path, required=True)
    sbom.add_argument("--release-list-file", type=Path, required=True)
    sbom.add_argument("--release-version", required=True)
    sbom.add_argument("--output-dir", type=Path, required=True)
    sbom.add_argument("--github-output", type=Path, required=True)

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
            generate_sbom(
                bundle_dir=args.bundle_dir,
                release_list_file=args.release_list_file,
                release_version=args.release_version,
                output_dir=args.output_dir,
                github_output=args.github_output,
            )
        elif args.command == "combine-attestations":
            combine_attestations(
                provenance_bundle=args.provenance_bundle,
                sbom_bundle=args.sbom_bundle,
                output=args.output,
            )
        else:  # pragma: no cover - argparse enforces the subcommand choices.
            parser.error(f"unknown command: {args.command}")
    except (OSError, ReleaseSecurityError) as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
