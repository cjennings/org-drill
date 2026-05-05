# Contributing to org-drill

Thanks for your interest. This is a small project; the contribution flow is informal.

## Reporting a bug

Open an issue at <https://github.com/cjennings/org-drill/issues> with:

- The behavior you expected vs. what you saw
- A minimal `:drill:`-tagged Org snippet that reproduces it (if applicable)
- Your Emacs version (`M-x emacs-version`) and Org version (`M-x org-version`)
- Anything in `*Messages*` that looks related

If you have a stack trace, paste the full backtrace inside a fenced code block.

## Sending a patch

1. Fork the repo and branch from `main`.
2. Write or update tests for whatever you're changing — see [Testing](#testing) below.
3. Run `make test-unit` and `make compile` and confirm they pass.
4. Open a PR. Conventional-commits-style commit messages are appreciated (`fix:`, `feat:`, `refactor:`, `test:`, `docs:`, `ci:`, `chore:`); see the existing `git log` for examples.

CI will run the test matrix (Emacs 28.2, 29.4, snapshot), `make lint` (informational), `make compile`, and `make coverage` against your branch.

## Development setup

Requires [Cask](https://github.com/cask/cask) and Emacs 25.3+ with Org 9.6+ available.

```sh
# Install dependencies into a project-local .cask/ directory
make setup

# Run the unit-test suite
make test-unit

# Run a single test file
make test-file FILE=test-org-drill-entry-empty-p.el

# Run tests matching a name pattern
make test-name TEST='*scheduler*'

# Generate a coverage report (writes .coverage/simplecov.json)
make coverage

# Byte-compile org-drill.el (informational warnings)
make compile

# Run checkdoc + package-lint + elisp-lint (informational)
make lint

# Quick parens sanity check
make validate-parens

# List every available target
make help
```

## Testing

The project uses [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/) for unit tests. Tests live under `tests/` as `test-<topic>.el` files.

When adding new code:

- **Write a failing test first**, then implement until green. This isn't optional — `make coverage` is currently around 78%, and we want to keep it climbing.
- For each new public function, cover three categories: normal cases (happy path), boundary cases (edge values, empty inputs, max/min), and error cases (invalid inputs, missing required state).
- Mock at external boundaries (`current-time`, `read-key-sequence`, `read-string`, `org-id-get-create`). Don't mock internal helpers.
- Slow tests (multi-second integration scenarios) should be tagged `:tags '(:slow)` so they're excluded from the default `make test-unit` runner.

When fixing a bug:

- Write a regression test that reproduces the bug **before** changing the implementation.
- Verify the test fails on the unfixed code.
- Apply the fix and verify the test passes.
- Mention the upstream issue number in the commit message when applicable.

The `robot/` directory contains end-to-end UI tests that drive a real Emacs frame via window-system key events. They require X11 and aren't part of `make test-unit`. Run them with `make robot` (or `make robot-all` for the full set) only on a graphical session.

## Style notes

- Lisp code should byte-compile cleanly with `make compile`. Existing source has known docstring debt that `make lint` will surface; new code should not add to it.
- Function and variable names are `kebab-case` and prefixed with `org-drill-` (or `org-drill--` for internal).
- Use `defcustom` for user-configurable settings, with a clear `:type` and a `:group` of `org-drill`.
- Prefer `cl-letf`, `let`, and `unwind-protect` over global state mutation. Where global state is unavoidable (session objects, persisted matrices), keep its lifecycle explicit.

## License

By contributing you agree that your contributions are licensed under the same GPL-3.0-or-later as the rest of the project.
