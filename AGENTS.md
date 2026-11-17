# Working on roc-parser

## Documentation

Documentation serves current users and contributors. Every paragraph must help a
current task; delete obsolete or duplicated material. Keep implementation history
in Git, unfinished work in issues, and release changes in release records. Preserve
required public anchors without explaining their history. Follow the
[shared documentation guidance](https://lukewilliamboswell.github.io/roc-automation/documentation.html#write-project-documentation).

## API design

Prefer a unified, ergonomic API over parallel entry points and unnecessary
duplication. Share common contracts and configuration; keep format-specific
behavior in format-specific values and constructors. Do not add aliases or
wrappers that only duplicate an existing operation. Before 1.0, breaking changes
are permitted when a well-supported simpler design improves usability. Migrate
the implementation, callers, tests and documentation together.

## Errors

Represent library failures with descriptive tag unions and structured payloads,
including locations and relevant limits or offending values. Callers must be
able to distinguish failures without matching diagnostic strings. String
payloads should hold source data or user-supplied context, rather than prose
that identifies an error. Render text at presentation boundaries, including
compiler quotation diagnostics, instead of carrying message strings through
the parser. Apply this rule consistently when adding or revising errors.
