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

UNICODE = "https://github.com/roc-lang/unicode/releases/download/4.3.0/7HGaK8HUHMs5judroSmUoS4TjPGZ1kAjPTyPDSCWiSKM.tar.zst"
HEADER = f'package\n\t[Parser]\n\t{{\n\t\tunicode: "{UNICODE}",\n\t}}\n'


class ReleaseSecurityAssetsTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.tmp = Path(self.temporary_directory.name)
        self.artifact_file = "AcowExample123.tar.zst"
        self.bundle_path = self.tmp / self.artifact_file
        self.bundle_bytes = b"deterministic test bundle\n"
        self.bundle_path.write_bytes(self.bundle_bytes)
        self.header = self.tmp / "main.roc"
        self.header.write_text(HEADER, encoding="utf-8")

    def make_sbom(self, version: str = "1.2.3") -> Path:
        with patch.dict(os.environ, {"SOURCE_DATE_EPOCH": "0"}):
            return assets.generate_sbom(
                bundle=self.bundle_path, release_version=version, output_dir=self.tmp / "out", header=self.header
            )

    def test_sbom_describes_the_bundle_bytes_and_its_dependencies(self) -> None:
        sbom_path = self.make_sbom()
        self.assertEqual(sbom_path.name, f"{self.artifact_file}.spdx.json")

        document = json.loads(sbom_path.read_text(encoding="utf-8"))
        package, unicode = document["packages"]
        self.assertEqual(document["spdxVersion"], "SPDX-2.3")
        self.assertEqual(document["dataLicense"], "CC0-1.0")
        self.assertEqual(document["creationInfo"]["created"], "1970-01-01T00:00:00Z")
        self.assertEqual(package["versionInfo"], "1.2.3")
        self.assertEqual(package["licenseDeclared"], "UPL-1.0")
        self.assertFalse(package["filesAnalyzed"])
        self.assertEqual(
            package["checksums"],
            [{"algorithm": "SHA256", "checksumValue": hashlib.sha256(self.bundle_bytes).hexdigest()}],
        )
        self.assertEqual(
            package["downloadLocation"],
            f"https://github.com/{assets.REPOSITORY}/releases/download/1.2.3/{self.artifact_file}",
        )
        # A query for the transitive component finds it, its source and how it is used.
        self.assertEqual(unicode["name"], "roc-lang/unicode")
        self.assertEqual(unicode["versionInfo"], "4.3.0")
        self.assertEqual(unicode["downloadLocation"], UNICODE)
        self.assertIn(
            {"spdxElementId": assets.PACKAGE_ID, "relationshipType": "DEPENDS_ON",
             "relatedSpdxElement": unicode["SPDXID"]},
            document["relationships"],
        )
        assets.verify_sbom(sbom=sbom_path, bundle=self.bundle_path, release_version="1.2.3", header=self.header)

    def test_verify_rejects_another_bundle_or_version(self) -> None:
        sbom_path = self.make_sbom()
        with self.assertRaisesRegex(assets.ReleaseSecurityError, "version"):
            assets.verify_sbom(sbom=sbom_path, bundle=self.bundle_path, release_version="1.2.4", header=self.header)
        self.bundle_path.write_bytes(b"different bytes\n")
        with self.assertRaisesRegex(assets.ReleaseSecurityError, "SHA-256 does not match"):
            assets.verify_sbom(sbom=sbom_path, bundle=self.bundle_path, release_version="1.2.3", header=self.header)

    def test_verify_rejects_a_missing_dependency_or_dangling_reference(self) -> None:
        sbom_path = self.make_sbom()
        document = json.loads(sbom_path.read_text(encoding="utf-8"))
        document["packages"].pop()
        sbom_path.write_text(json.dumps(document), encoding="utf-8")
        with self.assertRaisesRegex(assets.ReleaseSecurityError, "unknown element.*DEPENDS_ON package"):
            assets.verify_sbom(sbom=sbom_path, bundle=self.bundle_path, release_version="1.2.3", header=self.header)

    def test_sbom_requires_a_bundle_file(self) -> None:
        with self.assertRaisesRegex(assets.ReleaseSecurityError, "missing"):
            assets.generate_sbom(bundle=self.tmp / "absent.tar.zst", release_version="1.2.3",
                                 output_dir=self.tmp / "out", header=self.header)
        with self.assertRaisesRegex(assets.ReleaseSecurityError, "tar.zst"):
            assets.generate_sbom(bundle=self.header, release_version="1.2.3",
                                 output_dir=self.tmp / "out", header=self.header)

    def test_sbom_rejects_invalid_release_version(self) -> None:
        for version in ("latest", "01.2.3", "1.2.3-01", "1.2.3-", "1.2.3+a..b"):
            with self.subTest(version=version):
                with self.assertRaisesRegex(assets.ReleaseSecurityError, "not valid SemVer"):
                    self.make_sbom(version)

    def test_semver_validation_accepts_prerelease_and_build_metadata(self) -> None:
        assets.validate_semver("1.2.3-rc.1+build.7")
        assets.validate_semver("0.0.1-dry-run")

    def test_the_package_header_names_its_dependency(self) -> None:
        dependencies = assets.package_dependencies((ROOT / "package" / "main.roc").read_text(encoding="utf-8"))
        self.assertEqual([dependency["repo"] for dependency in dependencies], ["unicode"])

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
