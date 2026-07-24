# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal macOS dotfiles for David. Cloned to `~/.dotfiles`; the shell/git configs are activated by symlinking them into `$HOME` (see `README.md`). There is no build or test step — changes take effect when the relevant app reloads its config.

## Layout

- `.zshrc`, `.zprofile`, `.gitconfig` — symlinked into `$HOME` on a new machine.
- `Brewfile` / `Brewfile.lock.json` — Homebrew manifest. Regenerate with `brew bundle dump`; install with `brew bundle --file ~/.dotfiles/Brewfile`.
- `emacs/` — Emacs config (see below). This directory is used directly as `~/.emacs.d` (or symlinked to it).
- `hammerspoon/` — window-toggle hotkeys and (formerly) a calendar→Slack sync.
- `ghostty/config` — Ghostty terminal config.

Most of `emacs/` is committed **package artifacts and runtime state that are NOT hand-edited**: `elpa/` (installed packages), `.cache/`, `org-persist/`, `*.db`, `transient/`, etc. Newer runtime files are gitignored (`emacs/.gitignore`). When editing config, only touch `emacs/init.el`, `emacs/config/db-*.el`, and `ghostty`/`hammerspoon` sources — never the `elpa/` tree.

## Emacs configuration

- Entry point: `emacs/init.el`. It loads `custom.el` (gitignored, auto-generated — don't hand-edit), then `require`s each module from `emacs/config/` in order.
- Config is split into `db-*.el` modules under `emacs/config/`, each a `(provide 'db-NAME)` feature: `db-ui`, `db-packages`, `db-navigation`, `db-development`, `db-languages`, `db-markdown`, `db-file-management`, `db-org`, `db-functions`, `db-keybindings`, `db-env`, `db-ghostel`, `db-fonts`. To add config, edit the matching module (or add a new `db-*.el` and `require` it in `init.el`).
- **Package management is `package.el` + `use-package`** (MELPA/GNU/org archives), with `use-package-always-ensure t`. It is NOT straight.el — `fix-straight.sh` and `emacs/fix-straight.sh` are leftover repair scripts, not the active setup.
- Languages via `lsp-mode` (deferred): Go, Python, TypeScript, Terraform. Format-on-save is wired per language (Go: goimports/gofmt + LSP organize-imports; Python: `lsp-format-buffer` + black/isort; Terraform: `terraform-format-on-save-mode`).
- Terminal is **ghostel** (`db-ghostel.el`), which replaced vterm. `C-c t` opens a new terminal. Claude Code often runs inside these Emacs terminal buffers.

### Restarting after config changes

Emacs runs as a daemon via a LaunchAgent. Config edits require a daemon restart to take effect (not just re-opening a frame). Restart the daemon rather than assuming a running instance has picked up changes.

### Non-obvious keybindings (`db-keybindings.el`)

`C-w` and `M-w` are **swapped from Emacs defaults**: `C-w` = copy (`kill-ring-save`), `M-w` = cut (`kill-region`). Keep this in mind before suggesting cut/copy bindings.

## Hammerspoon

`hammerspoon/init.lua` binds app-toggle hotkeys (Alt+Space → Ghostty, Cmd+Ctrl+E → Emacs). Secrets (Slack token, ICS URL) live in `hammerspoon/secrets.lua`, which is **gitignored** — never commit it, and never move its values into a tracked file.

## Git / commit conventions

- `.gitconfig` sets `pushRemote = no_push` and GPG-signs all commits.
- Never add Claude as an author or co-author on David's commits.
- Recent history uses Conventional Commits (`feat:`, `fix:`, `refactor:`, optionally scoped like `feat(hammerspoon):`).
