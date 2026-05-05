# Changelog

All notable changes to this fork are documented here, following [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The first tagged release of this fork hasn't shipped yet — the entries below are pending under `[Unreleased]`.

## [Unreleased]

### Added
- GitHub Actions CI workflow with test matrix (Emacs 28.2 / 29.4 / snapshot), lint, and coverage jobs.
- `Makefile` targets `make coverage`, `make compile`, `make lint`, `make validate-parens`, plus a `:slow` tag filter on the ERT runners.
- ~370 new ERT tests across 27 files, raising coverage on `org-drill.el` from 12% to ~78%.
- Regression tests pinning the fixes for upstream issues #13, #33, #38, #44, #45, #52, #58, #59.

### Fixed
- **#59**: `[Y-08-27 Wed 16:%]`-shaped timestamps in `DRILL_LAST_REVIEWED` on Org 9.6+. The legacy `(substring ... 1 -1)` slicing dropped `%` directives because Org 9.6 dropped the angle brackets the slice assumed.
- **#52, #58**: Drill sessions silently nulled out the user's `default-input-method`. `(set-input-method nil)` clears the global as a side effect; replaced with `deactivate-input-method` / `activate-input-method` guarded for the no-op case.
- **#13**: Drill entries whose answer lived inside a child sub-heading were skipped as empty. Search bound now covers the whole subtree.
- **#38**: Cloze face leaked onto unrelated org headings when `org-drill-use-visible-cloze-face-p` was non-nil. Cloze regex now constrained to a single line.
- **#44**: "Window system frame should be used" error in TTY emacsclient (e.g. inside tmux). LaTeX preview helpers now guarded with `display-graphic-p`.
- **#45**: A corrupted persist file broke the entire package's load (`persist-load` raised "End of file during parsing"). Wrapped in `condition-case` with a fresh-state fallback.
- **#33**: `org-drill-final-report` skipped after edit-and-resume. The session's `end-pos` slot is now cleared on resume.
- **#43**: Cards with `DRILL_CARD_TYPE: translate_number` hit a void-function error. The dead alist entry referencing a no-longer-existing function was removed; legacy decks now skip gracefully.
- **#53**: A new (no-ID) entry's "hash-table-p, nil" error stopped the whole collection scan, forcing one-at-a-time invocations. Per-entry processing now wrapped in `condition-case`.
- Stray `[debug] org-drill: at marker position N` message logged on every drilled card.
- Zero-width overlay created by `org-drill-hide-cloze-hints` for every hint-less cloze.
- Arith-error in `org-drill-final-report` warning branch when `dormant + due` was zero.
- Crash in `org-drill-smart-reschedule` when `&optional days-ahead` was passed nil.
- Mode-line / variable-pitch state restored in the wrong buffer if the user switched buffers mid-session.
- `DRILL_LEITNER_BOX` of nil triggered `string-to-number` errors in leitner-rebox.
- `org-drill-hide-drawers` created junk overlays for malformed drawers without `:END:`.
- `org-drill-resume` and `org-drill-again` crashed with cryptic eieio errors on first invocation; they now raise a clear `user-error` if no prior session exists.

### Changed
- Bumped Org dependency from 9.3 to 9.6 in both `Cask` and the package's `Package-Requires` header. The seven unguarded calls to `org-fold-show-entry`/`-subtree` (added in Org 9.6) had the package effectively requiring 9.6 already at runtime — the declaration now matches.
- Renamed `make install` to `make setup`. The target installs Cask deps into a project-local `.cask/`, not the package onto the user's system.
- Replaced `.gitlab-ci.yml` with a GitHub Actions workflow under `.github/workflows/ci.yml`.
- `org-drill` main entry split into named phases (`prepare-fresh-session`, `collect-entries`, `show-end-message`).  The function dropped from 137 lines to 36.
- `org-drill-entries` main loop flattened from 7 levels of nesting to 4 via two extracted helpers (`pick-next-marker`, `route-rating-result`).
- `org-drill-final-report` split into format helpers; queue tags extracted to a single `org-drill--queue-tag` helper.
- `org-drill-merge-buffers` split into named phases (`build-dest-id-table`, `migrate-from-source`, `strip-unmatched-dest-entries`).
- New macro `org-drill-with-card-display` collapses the three-deep `with-hidden-comments / with-hidden-cloze-hints / with-hidden-cloze-text` wrap shared by five card presenters.
- New helper `org-drill--read-rating-key` shared by `org-drill-reschedule` and `org-drill-leitner-rebox`, which previously duplicated 60 lines of key-handling code each.
- Spanish-verb 6-way `cl-case` converted to a 6-element alist dispatch.
- README rewritten as a slim front-page (1049 → 227 lines); the long-form manual stays in `org-drill.org`.

### Removed
- Legacy `Org < 9.6` fallback in `org-drill-time-to-inactive-org-timestamp`.
- `Org < 9.2` advice on `org-get-tags` and the Org 8.x `defalias` shim around `org-latex-preview`.
- 60+ lines of commented-out alternative function bodies (replaced implementations).
- Dead org-version warning block (org < 7.9.3f).

[Unreleased]: https://github.com/cjennings/org-drill/compare/v2.7.0...HEAD
