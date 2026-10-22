from __future__ import annotations

import re
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_DIR = ROOT / ".github" / "workflows"
PINNED_ACTION_RE = re.compile(r"^[^\s]+@[0-9a-f]{40}$")
# The reviewed roc-automation revision (tools-0.2.0) every workflow uses.
ROC_AUTOMATION = "8a6c914fa8400c9d1f5a3815ab06e06687d58148"


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
    def test_repo_owned_automation_is_python_under_scripts(self) -> None:
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


if __name__ == "__main__":
    unittest.main()
