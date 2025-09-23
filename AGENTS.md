# Repository Guidelines

## Project Structure & Module Organization
Configuration lives at the repo root: `early-init.el` handles startup tuning, while `init.el` wires packages and user workflows. Feature-specific modules belong in `lisp/` (e.g., `init-ai.el`, `init-org.el`); name new files `init-<topic>.el`, keep `lexical-binding` enabled, and finish with the matching `provide`. The bundled `lisp/codex.el/` directory tracks the Codex assistant package and its tests—treat it as a submodule and avoid editing generated assets under `elpa/`, `auto-save-list/`, or `backups/`.

## Build, Test, and Development Commands
Run `emacs --debug-init` after changes to catch startup regressions. For noninteractive validation from the repo root, use `emacs -Q --batch -l init.el --eval '(message "init ok")'`. The Codex package has its own suite: `cd lisp/codex.el && make test` executes `checkdoc`, `package-lint`, and the ERT specs. When adjusting AI credentials, confirm they resolve inside Emacs with `emacs --batch -l init.el --eval '(message "OPENAI_API_KEY set? %s" (if (getenv "OPENAI_API_KEY") "yes" "no"))'` (swap the variable name as needed) before shipping.

## Coding Style & Naming Conventions
Indent Emacs Lisp with two spaces, commit with `indent-tabs-mode` disabled, and prefer `use-package` blocks for declarative setup. Configure hooks and keybindings inside the related form, using `my/<verb>-<noun>` helpers or the package prefix to stay discoverable. Docstrings must be full sentences because `checkdoc` runs in CI; include brief package comments at the top of every module and keep line length under 100 characters for readability.

## Testing Guidelines
Add new ERT cases under `lisp/codex.el/test/`, naming them `codex-<feature>-<behavior>` to group failures sensibly. When touching core init flows, smoke-test common modes (Org, Magit, vterm) interactively and note any regressions in the PR. Record manual validation steps in the pull request body so reviewers can repeat them quickly.

## Commit & Pull Request Guidelines
Write commit subjects in imperative mood with <= 72 characters (examples: `Tighten JSON helpers`, `Add copilot toggle hints`). Reference related issues in the body and explain user-facing impact plus any follow-up tasks. Pull requests should summarize configuration changes, list verification commands, and provide screenshots or short clips if UI or keybinding behavior shifts. Update `README.md` or `CLAUDE.md` whenever contributor-facing workflows change.
