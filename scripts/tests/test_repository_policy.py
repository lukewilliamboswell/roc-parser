from __future__ import annotations

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_DIR = ROOT / ".github" / "workflows"
PINNED_ACTION_RE = re.compile(r"^[^\s]+@[0-9a-f]{40}$")


class RepositoryPolicyTests(unittest.TestCase):
    def test_repo_owned_automation_is_python_under_scripts(self) -> None:
        shell_scripts = sorted(path for path in ROOT.rglob("*.sh") if ".git" not in path.parts)
        self.assertEqual(shell_scripts, [], "repository-owned Bash scripts are not allowed")

        python_outside_scripts = sorted(
            path
            for path in ROOT.rglob("*.py")
            if ".git" not in path.parts and "scripts" not in path.relative_to(ROOT).parts
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


if __name__ == "__main__":
    unittest.main()
