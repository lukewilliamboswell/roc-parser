# Roc nightly updates

This repository checks once daily at 13:16 UTC, about four hours
after the upstream 09:00 UTC build. Late publication can wait until the next day.

`.roc-version` is the compiler pin. `.github/roc-nightly.json` selects this
repository's validation workflows, including their validation-only release paths.
Nightly candidates must pass both current-source bundle tests and committed example
tests against unchanged released dependency URLs. Both lanes run on pull requests
to provide stable required checks; the current-source result is authoritative for
package changes, while the published result is additionally authoritative for
compiler-pin or committed-example changes.
The controller, its tests, and job permissions are maintained in
[roc-automation](https://github.com/lukewilliamboswell/roc-automation).
The caller workflows pin shared code to `e1c09717083a1f8c0e380f9bc33ef1e9c46a78c4`.
Dependabot proposes reviewed updates to Actions/workflow references.

Follow the shared [integration and permissions guide](https://github.com/lukewilliamboswell/roc-automation/blob/e1c09717083a1f8c0e380f9bc33ef1e9c46a78c4/docs/integration.md)
for the PR-creation setting, action allowlists, required checks, and first live
GITHUB_TOKEN run. Keep default token permissions read-only. Successful candidates
are merged automatically after the controller independently rechecks their signed
commit, validation evidence, and branch-protection rules. Failed or abnormal
candidates remain open for investigation. The controller never approves PRs and
receives no protection bypass.

`automation/roc-nightly` is reserved for the bot's pin-only commits. Put manual
compatibility changes on a separate branch. Candidate failures require diagnosis;
do not weaken tests or mechanically replace baselines to accept a compiler.

The PR configuration check validates the local pin and selected workflow files.
The shared repository owns the controller regression suite. Project tests remain
in this repository and run on the exact candidate commit. Scheduled bot-token
acceptance must be verified after merge; file changes alone cannot prove it.

Use the shared [OpenSSF rollout checklist](https://github.com/lukewilliamboswell/roc-automation/blob/e1c09717083a1f8c0e380f9bc33ef1e9c46a78c4/docs/openssf.md)
to record project-specific evidence. This integration does not establish badge
compliance or change repository settings.
