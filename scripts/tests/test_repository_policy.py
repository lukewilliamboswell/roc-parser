from __future__ import annotations

import re
import json
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_DIR = ROOT / ".github" / "workflows"
PINNED_ACTION_RE = re.compile(r"^[^\s]+@[0-9a-f]{40}$")
# The reviewed roc-automation revision every workflow uses.
ROC_AUTOMATION = "cc6d4081762c6d70bf95bf18c9abf99b5e9ade25"


def repository_files() -> list[Path]:
    # Imported oracle source and build output live in ignored scratch folders.
    # Check repository-owned files, including new untracked changes, rather
    # than third-party files generated underneath the working directory.
    result = subprocess.run(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=ROOT, check=True, capture_output=True,
    )
    return [ROOT / path.decode("utf-8") for path in result.stdout.split(b"\0") if path]


class RepositoryPolicyTests(unittest.TestCase):
    def test_runtime_exceptions_are_explicit_and_current(self) -> None:
        policy = json.loads((ROOT / "scripts/automation-policy.json").read_text())
        self.assertEqual(policy["schema_version"], 1)
        declared = []
        for group in policy["runtime_exceptions"]:
            self.assertTrue(group["runtime"])
            self.assertTrue(group["contract"])
            self.assertTrue(group["paths"])
            self.assertEqual(group["paths"], sorted(group["paths"]))
            declared.extend(group["paths"])
        actual = sorted(str(path.relative_to(ROOT)) for path in repository_files()
                        if path.is_file() and path.suffix in (".py", ".rb", ".cjs", ".js", ".mjs", ".zig"))
        self.assertEqual(sorted(declared), actual, "review and declare each independent harness or platform adapter")
        self.assertEqual(len(declared), len(set(declared)))

        nightly = sorted(str(path.relative_to(ROOT)) for path in repository_files()
                         if path.is_file() and path.suffix == ".roc" and path.parent != ROOT / "scripts"
                         and "scripts" in path.relative_to(ROOT).parts
                         and re.search(r"(?m)^app ", path.read_text()))
        self.assertEqual(policy["nightly_compiler_apps"], nightly,
                         "list each probe or compiler fixture that requires the nightly")

    def test_repo_owned_automation_lives_under_scripts(self) -> None:
        paths = repository_files()
        shell_scripts = sorted(path for path in paths if path.suffix == ".sh")
        self.assertEqual(shell_scripts, [], "repository-owned Bash scripts are not allowed")

        python_outside_scripts = sorted(
            path
            for path in paths
            if path.suffix == ".py" and "scripts" not in path.relative_to(ROOT).parts
        )
        self.assertEqual(
            python_outside_scripts,
            [],
            "repository automation must live under scripts/",
        )

    def test_roc_tooling_uses_its_own_fixed_compiler(self) -> None:
        blueprint = (ROOT / "Blueprint.roc").read_text()
        stable = re.findall(r'Fetch\("roc-stable", Recipes.roc_nightly\("([^"\n]+)"\)\)', blueprint)
        self.assertEqual(len(stable), 1)
        tasks = re.findall(r'Task\("[^"\n]+",[^\n]*', blueprint)
        for path in repository_files():
            if path.suffix != ".roc" or not path.is_file():
                continue
            text = path.read_text()
            pin = re.findall(r'(?m)^\s*roc:\s*"([^"\n]+)"', text)
            # The oracle probes are built by the floating package compiler.
            # Only executable orchestration entry points carry the tooling pin.
            if path.parent == ROOT / "scripts":
                if not re.search(r'(?m)^app ', text):
                    continue
                self.assertEqual(pin, stable, str(path))
                header = text.split("}", 1)[0]
                dependencies = re.findall(r'\b(\w+):\s*(?:platform\s*)?"([^"\n]+)"', header)
                for field, dependency in dependencies:
                    if field == "roc":
                        continue
                    self.assertRegex(dependency, r'^https://[^/]+/[^/]+/[^/]+/releases/download/[^/]+/[A-Za-z0-9_-]{43,44}\.tar\.zst$')
                    self.assertIn('"' + dependency + '"', blueprint)
                self.assertTrue(text.startswith("#!/usr/bin/env roc-stable\n"), str(path))
                self.assertTrue(path.stat().st_mode & 0o111, str(path))
                self.assertTrue(any(str(path.relative_to(ROOT)) in task for task in tasks), str(path))
            else:
                self.assertEqual(pin, [], str(path))

    def test_workflow_actions_are_pinned_to_full_commit_shas(self) -> None:
        failures: list[str] = []
        for workflow in sorted(WORKFLOW_DIR.glob("*.y*ml")):
            for line_number, line in enumerate(
                workflow.read_text(encoding="utf-8").splitlines(), start=1
            ):
                stripped = line.strip()
                if not stripped.startswith("uses:") and not stripped.startswith("- uses:"):
                    continue
                reference = stripped.split("uses:", 1)[1].split("#", 1)[0].strip()
                if reference.startswith("./"):
                    continue
                if not PINNED_ACTION_RE.fullmatch(reference):
                    failures.append(f"{workflow.name}:{line_number}: {reference}")
        self.assertEqual(failures, [], "unpinned action references: " + ", ".join(failures))

    def test_workflows_avoid_dangerous_triggers_and_inline_shell_scripts(self) -> None:
        failures: list[str] = []
        for workflow in sorted(WORKFLOW_DIR.glob("*.y*ml")):
            text = workflow.read_text(encoding="utf-8")
            for trigger in ("pull_request_target:", "workflow_run:"):
                if trigger in text:
                    failures.append(f"{workflow.name}: forbidden trigger {trigger[:-1]}")
            for line_number, line in enumerate(text.splitlines(), start=1):
                if re.match(r"^\s*run:\s*[|>]", line):
                    failures.append(f"{workflow.name}:{line_number}: multiline shell block")
        self.assertEqual(failures, [])

    def test_workflows_follow_the_roc_automation_model(self) -> None:
        # Every job states its own permissions, every roc-automation action and
        # workflow is the reviewed revision, and the compiler floats: nothing
        # pins a nightly or reads a pin file. The retired names are spelled in
        # parts, so that a search of the repository for them finds nothing.
        pin_file = "." + "roc-version"
        retired = (pin_file, "nightly" + "_validation", "roc-nightly" + ".json", "nightly-tag: nightly-")
        failures: list[str] = []
        for workflow in sorted(WORKFLOW_DIR.glob("*.y*ml")):
            text = workflow.read_text(encoding="utf-8")
            if not re.search(r"(?m)^permissions: \{\}$", text):
                failures.append(f"{workflow.name}: top-level permissions must be {{}}")
            for reference in re.findall(r"uses: (lukewilliamboswell/roc-automation/\S+)", text):
                if not reference.endswith("@" + ROC_AUTOMATION):
                    failures.append(f"{workflow.name}: {reference} is not at {ROC_AUTOMATION}")
            for name in retired:
                if name in text:
                    failures.append(f"{workflow.name}: mentions {name}")
        self.assertEqual(failures, [])
        self.assertFalse((ROOT / pin_file).exists())


if __name__ == "__main__":
    unittest.main()
