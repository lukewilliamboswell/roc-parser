"""Acceptance tests of real shared SBOM generation and independent verification."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
VERSION = "0.0.1-dryrun"
TOOLS = ("roc-stable", "roc", "blueprint", "blueprint-sbom", "openssl", "jq")


@unittest.skipUnless(all(shutil.which(tool) for tool in TOOLS), "run through Blueprint for inventory tools")
class SourceInventoryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        scratch = ROOT / ".roc-parser-tmp"
        scratch.mkdir(exist_ok=True)
        cls.build = tempfile.TemporaryDirectory(prefix="inventory-tests-", dir=scratch)
        cls.directory = Path(cls.build.name)
        cls.binary = cls.directory / "release-sbom"
        subprocess.run(["roc-stable", "build", "scripts/release_sbom.roc", f"--output={cls.binary}"],
                       cwd=ROOT, check=True, capture_output=True)
        bundles = cls.directory / "bundle"
        subprocess.run(["scripts/bundle.roc", "--", "--output-dir", str(bundles)],
                       cwd=ROOT, check=True, capture_output=True)
        cls.bundle = next(bundles.glob("*.tar.zst"))
        cls.sbom = cls.bundle.with_name(cls.bundle.name + ".spdx.json")
        generated = cls.command("generate", "--bundle", str(cls.bundle), "--output-dir", str(bundles))
        if generated.returncode:
            raise AssertionError(generated.stdout + generated.stderr)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    @classmethod
    def command(cls, mode: str, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(cls.binary), mode, "--release-version", VERSION, *args], cwd=ROOT,
                              capture_output=True, text=True, timeout=180)

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory(dir=self.directory)
        self.addCleanup(self.temp.cleanup)
        self.folder = Path(self.temp.name)
        self.bundle_copy = self.folder / self.bundle.name
        self.sbom_copy = self.folder / self.sbom.name
        shutil.copyfile(self.bundle, self.bundle_copy)
        shutil.copyfile(self.sbom, self.sbom_copy)

    def verify(self, *extra: str) -> subprocess.CompletedProcess[str]:
        return self.command("verify", "--bundle", str(self.bundle_copy), "--sbom", str(self.sbom_copy), *extra)

    def mutate(self, transform) -> None:
        document = json.loads(self.sbom_copy.read_text())
        transform(document)
        self.sbom_copy.write_text(json.dumps(document))

    def test_verified_bytes_are_unchanged(self) -> None:
        before = {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                  for path in (self.bundle_copy, self.sbom_copy)}
        result = self.verify()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(before, {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                                  for path in (self.bundle_copy, self.sbom_copy)})

    def test_subject_digest_is_bound_to_the_actual_bundle(self) -> None:
        content = bytearray(self.bundle_copy.read_bytes())
        content[len(content) // 2] ^= 1
        self.bundle_copy.write_bytes(content)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_wrong_subject_version_is_rejected(self) -> None:
        def change(document):
            subject = next(package for package in document["packages"] if package["SPDXID"] == "SPDXRef-Subject")
            subject["versionInfo"] = "9.9.9"
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_changed_dependency_identity_is_rejected(self) -> None:
        def change(document):
            dependency = next(package for package in document["packages"] if package["SPDXID"] == "SPDXRef-unicode")
            dependency["downloadLocation"] = "https://example.com/different.tar.zst"
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_known_transitive_data_can_be_queried_by_source_and_digest(self) -> None:
        document = json.loads(self.sbom_copy.read_text())
        data = next(package for package in document["packages"] if package["name"] == "EastAsianWidth.txt")
        self.assertEqual(data["downloadLocation"], "https://www.unicode.org/Public/17.0.0/ucd/EastAsianWidth.txt")
        self.assertIn({"algorithm": "SHA256", "checksumValue": "ea7ce50f3444a050333448dffef1cadd9325af55cbb764b4a2280faf52170a33"}, data["checksums"])
        self.assertEqual(data["licenseDeclared"], "Unicode-3.0")
        self.assertTrue(any(data["SPDXID"] in (edge["spdxElementId"], edge["relatedSpdxElement"])
                            and "SPDXRef-unicode" in (edge["spdxElementId"], edge["relatedSpdxElement"])
                            for edge in document["relationships"]))

    def test_removing_transitive_data_breaks_the_expected_inventory(self) -> None:
        def change(document):
            identifier = next(package["SPDXID"] for package in document["packages"] if package["name"] == "EastAsianWidth.txt")
            document["packages"] = [package for package in document["packages"] if package["SPDXID"] != identifier]
            document["relationships"] = [edge for edge in document["relationships"]
                                         if identifier not in (edge["spdxElementId"], edge["relatedSpdxElement"])]
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_wrong_subject_url_is_rejected(self) -> None:
        def change(document):
            subject = next(package for package in document["packages"] if package["SPDXID"] == "SPDXRef-Subject")
            subject["downloadLocation"] = "https://example.com/wrong.tar.zst"
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_changed_component_license_is_rejected(self) -> None:
        def change(document):
            dependency = next(package for package in document["packages"] if package["SPDXID"] == "SPDXRef-unicode")
            dependency["licenseDeclared"] = "MIT"
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_changed_dependency_relationship_is_rejected(self) -> None:
        def change(document):
            identifier = next(package["SPDXID"] for package in document["packages"] if package["name"] == "EastAsianWidth.txt")
            edge = next(edge for edge in document["relationships"] if identifier in (edge["spdxElementId"], edge["relatedSpdxElement"]))
            edge["relationshipType"] = "DEPENDS_ON" if edge["relationshipType"] != "DEPENDS_ON" else "BUILD_DEPENDENCY_OF"
        self.mutate(change)
        result = self.verify()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
