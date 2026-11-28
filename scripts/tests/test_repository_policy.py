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
ROC_AUTOMATION = "2b0ffc9c556f465759ca81055bb819c7e15624eb"


def comparison_policy_violations(workflows: dict[str, str], plan: str, blueprint: str) -> list[str]:
    """Keep workflow entry points and their default plan free of foreign runs."""
    failures = []
    expected_sections = ["scripts", "package", "examples", "docs", "fuzz", "bench"]
    if re.findall(r'name: "([^"\n]+)"', plan) != expected_sections:
        failures.append("default check sections must contain only native validation")
    if 'Task("check", [Use("package"),' not in blueprint:
        failures.append("default check must use the package environment")
    package_environment = re.search(r'Environment\("package",[^\n]+', blueprint)
    if not package_environment or 'Extend("tooling")' not in package_environment[0] or 'Tools(' in package_environment[0]:
        failures.append("package environment must use tooling without reference toolchains")
    commands = plan.split("\n\t## Reject", 1)[0]
    for command in re.findall(r'\[("[^"\n]*"(?:,\s*"[^"\n]*")*)\]', commands):
        args = re.findall(r'"([^"\n]*)"', command)
        if args[0] == "python3":
            if args[1:4] == ["-m", "unittest", "discover"]:
                continue
            if args[1:2] == ["scripts/bench.py"] and "--langs" in args and args[args.index("--langs") + 1:] == ["roc", "--timeout", "60", "--fail-on-error"] and "--refresh-references" not in args:
                continue
            failures.append("default check runs a comparison or unreviewed Python entry point")
        elif args[0] not in {"roc-stable", "scripts/test_package.roc", "release-package", "scripts/check_doc_examples.roc", "scripts/fuzz.roc", "scripts/test_automation.roc"}:
            failures.append("default check runs a comparison or unreviewed entry point")
    native_tasks = {"check", "test", "fuzz", "fuzz-smoke", "doc-examples", "docs", "api-docs", "release-package", "sbom-composition", "bundle", "examples", "test-examples-archive"}
    comparison_script = re.compile(r'scripts/(?:review_|compare_|readme_|setup_oracles|check_asciidoc|check_roc_syntax|bench)')
    for name, text in workflows.items():
        for line in text.splitlines():
            command = re.match(r'\s*(?:-\s*)?run:\s*(.*)', line)
            if command:
                run = command[1]
                if not re.match(r'(?:blueprint run |gh |echo |test )', run):
                    failures.append(f"{name}: unreviewed workflow command may execute a reference")
                for task in re.findall(r'blueprint\s+run\s+([^\s]+)', run):
                    if task not in native_tasks:
                        failures.append(f"{name}: local comparison task {task} cannot run in CI")
                if comparison_script.search(run):
                    failures.append(f"{name}: comparison scripts are local-only")
            if re.search(r'ROC_PARSER_RUN_ORACLE_TESTS\s*[:=]', line):
                failures.append(f"{name}: CI must not enable live oracle tests")
            if re.search(r'cache-workload:\s*[\'\"]?oracles\b|blueprint env oracle|oracle-cache|reference-cache', line):
                failures.append(f"{name}: reference runtimes and caches are local-only")
    return failures


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
    def test_cross_language_comparisons_remain_local(self) -> None:
        workflows = {path.name: path.read_text() for path in WORKFLOW_DIR.glob("*.y*ml")}
        plan = (ROOT / "scripts/src/CheckPlan.roc").read_text()
        blueprint = (ROOT / "Blueprint.roc").read_text()
        self.assertEqual(comparison_policy_violations(workflows, plan, blueprint), [])

    def test_local_comparison_guard_catches_direct_and_indirect_execution(self) -> None:
        plan = (ROOT / "scripts/src/CheckPlan.roc").read_text()
        blueprint = (ROOT / "Blueprint.roc").read_text()
        for command in ("blueprint run conformance", "blueprint run bench -- --langs roc rust", "python3 scripts/review_qif.py check", "python3 scripts/compare_markup_performance.py", "go run ./comparator", "ruby scripts/asciidoc/oracle.rb", "ROC_PARSER_RUN_ORACLE_TESTS=1 python3 -m unittest"):
            with self.subTest(command=command):
                self.assertTrue(comparison_policy_violations({"new.yml": "run: " + command}, plan, blueprint))
        self.assertTrue(comparison_policy_violations({}, plan.replace('name: "bench"', 'name: "conformance"'), blueprint))
        self.assertTrue(comparison_policy_violations({}, plan, blueprint.replace('Task("check", [Use("package")', 'Task("check", [Use("oracles")')))
        self.assertTrue(comparison_policy_violations({}, plan.replace('"--langs", "roc"', '"--langs", "roc", "rust"'), blueprint))

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
