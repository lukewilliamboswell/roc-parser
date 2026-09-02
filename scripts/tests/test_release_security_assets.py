from __future__ import annotations

import hashlib
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from scripts import release_security_assets as assets  # noqa: E402


class ReleaseSecurityAssetsTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.tmp = Path(self.temporary_directory.name)
        self.bundle_dir = self.tmp / "bundles"
        self.bundle_dir.mkdir()
        self.artifact_file = "AcowExample123.tar.zst"
        self.bundle_path = self.bundle_dir / self.artifact_file
        self.bundle_bytes = b"deterministic test bundle\n"
        self.bundle_path.write_bytes(self.bundle_bytes)
        self.release_list = self.tmp / "release-bundles.json"
        self.release_list.write_text(
            json.dumps(
                [
                    {
                        "name": "default",
                        "artifact_file": self.artifact_file,
                        "source_path": f"dist/{self.artifact_file}",
                    }
                ]
            ),
            encoding="utf-8",
        )

    def test_sbom_writes_spdx_document_and_github_outputs(self) -> None:
        output_dir = self.tmp / "security-assets"
        github_output = self.tmp / "github-output"

        with patch.dict(os.environ, {"SOURCE_DATE_EPOCH": "0"}):
            bundle_path, sbom_path, attestation_path = assets.generate_sbom(
                bundle_dir=self.bundle_dir,
                release_list_file=self.release_list,
                release_version="1.2.3",
                output_dir=output_dir,
                github_output=github_output,
            )

        self.assertEqual(bundle_path, self.bundle_path.resolve())
        self.assertEqual(sbom_path.name, f"{self.artifact_file}.spdx.json")
        self.assertEqual(attestation_path.name, f"{self.artifact_file}.intoto.jsonl")
        self.assertFalse(attestation_path.exists())

        document = json.loads(sbom_path.read_text(encoding="utf-8"))
        package = document["packages"][0]
        self.assertEqual(document["spdxVersion"], "SPDX-2.3")
        self.assertEqual(document["dataLicense"], "CC0-1.0")
        self.assertEqual(document["creationInfo"]["created"], "1970-01-01T00:00:00Z")
        self.assertEqual(package["versionInfo"], "1.2.3")
        self.assertEqual(package["licenseDeclared"], "UPL-1.0")
        self.assertFalse(package["filesAnalyzed"])
        self.assertEqual(
            package["checksums"],
            [
                {
                    "algorithm": "SHA256",
                    "checksumValue": hashlib.sha256(self.bundle_bytes).hexdigest(),
                }
            ],
        )
        self.assertEqual(
            package["downloadLocation"],
            f"https://github.com/{assets.REPOSITORY}/releases/download/1.2.3/"
            f"{self.artifact_file}",
        )

        self.assertEqual(
            github_output.read_text(encoding="utf-8").splitlines(),
            [
                f"bundle-path={self.bundle_path.resolve()}",
                f"sbom-path={sbom_path}",
                f"attestation-path={attestation_path}",
            ],
        )

    def test_sbom_requires_exactly_one_bundle(self) -> None:
        self.release_list.write_text("[]\n", encoding="utf-8")
        with self.assertRaisesRegex(
            assets.ReleaseSecurityError, "must contain exactly one bundle"
        ):
            assets.generate_sbom(
                bundle_dir=self.bundle_dir,
                release_list_file=self.release_list,
                release_version="1.2.3",
                output_dir=self.tmp / "output",
                github_output=self.tmp / "github-output",
            )

    def test_sbom_rejects_invalid_release_version(self) -> None:
        for version in ("latest", "01.2.3", "1.2.3-01", "1.2.3-", "1.2.3+a..b"):
            with self.subTest(version=version):
                with self.assertRaisesRegex(assets.ReleaseSecurityError, "not valid SemVer"):
                    assets.generate_sbom(
                        bundle_dir=self.bundle_dir,
                        release_list_file=self.release_list,
                        release_version=version,
                        output_dir=self.tmp / "output",
                        github_output=self.tmp / "github-output",
                    )

    def test_semver_validation_accepts_prerelease_and_build_metadata(self) -> None:
        assets.validate_semver("1.2.3-rc.1+build.7")

    def test_combine_attestations_writes_two_compact_jsonl_records(self) -> None:
        provenance = self.tmp / "provenance.json"
        sbom = self.tmp / "sbom.json"
        output = self.tmp / "combined" / "bundle.intoto.jsonl"
        provenance.write_text('{\n  "z": 1,\n  "a": {"value": true}\n}\n', encoding="utf-8")
        sbom.write_text('{"kind": "sbom", "items": [1, 2]}\n', encoding="utf-8")

        result = assets.combine_attestations(
            provenance_bundle=provenance,
            sbom_bundle=sbom,
            output=output,
        )

        self.assertEqual(result, output.resolve())
        self.assertEqual(
            output.read_text(encoding="utf-8"),
            '{"a":{"value":true},"z":1}\n'
            '{"items":[1,2],"kind":"sbom"}\n',
        )
        records = output.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(records), 2)
        self.assertTrue(all(isinstance(json.loads(record), dict) for record in records))

    def test_combine_attestations_rejects_non_object_bundle(self) -> None:
        provenance = self.tmp / "provenance.json"
        sbom = self.tmp / "sbom.json"
        output = self.tmp / "bundle.intoto.jsonl"
        provenance.write_text("[]\n", encoding="utf-8")
        sbom.write_text('{"kind":"sbom"}\n', encoding="utf-8")

        with self.assertRaisesRegex(assets.ReleaseSecurityError, "non-empty JSON object"):
            assets.combine_attestations(
                provenance_bundle=provenance,
                sbom_bundle=sbom,
                output=output,
            )
        self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
