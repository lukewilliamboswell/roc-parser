from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path
from typing import Mapping, Sequence


ROOT = Path(__file__).resolve().parents[1]


def resolve_command(command: str) -> str:
    """Resolve path-like commands while leaving PATH lookups unchanged."""
    if "/" in command or "\\" in command:
        return str(Path(command).expanduser().resolve())
    return command


def roc_command() -> str:
    return resolve_command(os.environ.get("ROC", "roc"))


def display_command(command: Sequence[os.PathLike[str] | str]) -> str:
    return " ".join(str(part) for part in command)


def run_command(
    command: Sequence[os.PathLike[str] | str],
    *,
    cwd: Path = ROOT,
    env: Mapping[str, str] | None = None,
    capture_output: bool = False,
) -> subprocess.CompletedProcess[str]:
    normalized = [str(part) for part in command]
    return subprocess.run(
        normalized,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE if capture_output else None,
        stderr=subprocess.STDOUT if capture_output else None,
        check=False,
    )


def report_failure(
    completed: subprocess.CompletedProcess[str],
    command: Sequence[os.PathLike[str] | str],
) -> int:
    if completed.stdout:
        print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
    print(
        f"command failed with exit code {completed.returncode}: {display_command(command)}",
        file=sys.stderr,
    )
    return completed.returncode
