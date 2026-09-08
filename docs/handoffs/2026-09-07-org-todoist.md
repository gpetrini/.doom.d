# Handoff — org-todoist / org-gtd integration

**Date:** 2026-09-07
**Repos:** `~/.doom.d` (branch `desktop`), `~/Dropbox/GTD` (branch `main`)
**Session:** https://claude.ai/code/session_01N4p9RSNs8tTfKc42BucyM5

## Where the design record lives

Read these first — they are the source of truth and predate this session:

- **[gpetrini/.doom.d#1](https://github.com/gpetrini/.doom.d/issues/1)** (`wayfinder:map`) — the map. Destination, notes, decisions, not-yet-specified. Updated at the end of this session with the implemented state and the divergence from the original bucket list.
- **[#3](https://github.com/gpetrini/.doom.d/issues/3)** (`wayfinder:research`) — **still open, blocking.** `ORG_GTD_TIMESTAMP` vs `SCHEDULED`/`DEADLINE`.
- **#2** and **#4** — closed this session with findings in the comments (metadata survival; recurrence → org repeaters).

The repo uses GitHub issues as its design layer. Run `gh issue list --repo gpetrini/.doom.d` before touching this work, and update the issues at the end. The failure mode observed in this session was staleness, not absence: #1 and #3 already contained conclusions that were rediscovered from scratch.

## Current state

Everything is committed, **nothing is pushed**.

| Repo | Commit | Contents |
|---|---|---|
| `~/.doom.d` | `8c02e51` | package, config, per-machine gate |
| `~/.doom.d` | `af1f16f` | unrelated pre-existing ESS/LaTeX fix, split out |
| `~/Dropbox/GTD` | `161d98c` | `TODOIST_TYPE` markers, tag removal, file header |

The commit messages carry the full rationale. Do not re-derive it.

Config lives in `config.org` section `* Org-todoist`, which tangles to `config.el`. **Edit the `.org`, never the `.el`**, then tangle:

```
emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"/home/gpetrini/.doom.d/config.org\"))"
```

## Blocking: no token yet

Nothing has ever run against the live API. Every conclusion in the commits and issues comes from reading `org-todoist.el` and simulating its type inference over the real file with `org-element`. **The first real sync is still untested and is the moment of truth.**

The user said they would supply the login and token. When they do:

1. Make sure `~/.authinfo` is mode 600 before putting a token in it.
2. Add `machine api.todoist.com login <login> password <token>`.
3. Restart Emacs; accept the `org-list-indent-offset` file-local prompt when opening the GTD file — a refusal causes sync errors.
4. `, T s` (localleader prefix `T` in `org-mode-map`).

Note `auth-sources` was `'(password-store "~/.authinfo.gpg")`, but there is no GPG secret key and `pass` is not installed on this machine, so both backends were inert. The config appends `~/.authinfo` under the machine gate.

## Expected on first sync

- **96 headlines gain `TODO`.** Accepted deliberately by the user; see the GTD commit.
- Two new level-1 headings appear in the GTD file: `Collaborators` and `Todoist Metadata`.
- `Habits` (22 tasks) is the fragile bucket — recurrence support is partial upstream. Check it first if anything misbehaves.

## Open decision the user has not answered

At the end of the session I recommended **reducing the sync to one project (`Actions`, 14 tasks)** and dropping `Someday/Maybe` (39) and `Habits` (22).

Rationale: successive verified constraints have narrowed the value proposition to phone capture — issue #1's own *"Next serve de zona de pouso para capturas ambíguas feitas no celular"* is the only part that survives every constraint. 39 of the 75 synced tasks are incubation items that do not need to be on a phone, and `Habits` carries the recurrence risk. The user replied "Commite e gere um handoff" without addressing it, so **it is still open**. Raise it before the first sync, not after.

## Verified constraints worth not rediscovering

These are recorded in the issue comments with line numbers. Summary only:

- org-todoist routes a task to a project **by tree position**, never by todo keyword. `Next`/`Waiting` cannot be separate Todoist projects while WAIT items live under `* Actions`.
- Todo keywords have **no** Todoist representation. NEXT, WAIT and STRT are all just "not completed" in the app.
- `Effort` and `Priority` **are** managed by org-todoist (push and pull), so the `my-gtd-add-effort` / `my-gtd-add-priority` hooks can be overwritten from the phone.
- The ignore property value is `IGNORE`; the upstream README's `IGNORED` is wrong.
- `org-gtd-default-file-name` is a `defconst`. Moving a bucket out of `org-gtd-tasks.org` makes `org-gtd-refile--do` silently recreate it empty.
- Dependencies have no Todoist representation at all. org-gtd v4 already has a full DAG (`ORG_GTD_DEPENDS_ON`/`BLOCKS`, cycle detection, SVG/ASCII graph); selection is `completing-read` over **titles**, not IDs. ID-free alternatives if the user still objects: `:ORDERED: t` with `org-enforce-todo-dependencies`, or org-edna's structural finders (only `ids` needs an identifier).

## Per-machine isolation — do not break this

The GTD file is shared over Syncthing. Two machines syncing it to Todoist would submit duplicate commands. The integration is opt-in via a gitignored `.todoist-enabled` marker in `doom-user-dir`, gated in **both** `packages.el` and the `use-package!` form. The user's other machine must stay dormant, and Doom must boot unchanged there. Any refactor of this config has to preserve both gates.

## Useful scratch

Reusable scripts from this session (session-scoped, may be gone — they are short enough to rewrite):

- `migrate.el` — applies the markers and reports every heading touched.
- `unsync.el` — pulls a bucket back out of the sync.
- `sim.el` — dry run: loads org-todoist, runs its type inference over a copy, reports projects/sections/tasks per bucket and how many headlines would gain `TODO`.

**Always simulate on a copy before touching `~/Dropbox/GTD/org-gtd-tasks.org`.** It is 164 KB and the single source of truth for the user's GTD system. The dry run caught two design errors in this session that reasoning alone had missed.

## Suggested skills

- **`mattpocock-skills:research`** — the natural fit for the remaining issue #3 question (do org-gtd's views fall back to `SCHEDULED`/`DEADLINE` when `ORG_GTD_TIMESTAMP` is absent?). Sources are local: `~/.emacs.d/.local/straight/repos/org-gtd.el/`, especially `org-gtd-view-lang.el`, `org-gtd-engage.el` and `org-gtd-types.el`.
- **`diagnose`** — if the first live sync misbehaves. Reproduce against a copy of the GTD file, never the original.
- **`handoff`** — at the end of your session, write the next one to `docs/handoffs/`, alongside this file.

## User preferences observed

- Portuguese, academic register, brevity. Wrong premises must be corrected immediately.
- Wants to itemise destructive changes **before** applying them. I broke this once by selecting three specific subtrees under a vague approval; the user pushed back and asked for a full revert. Do not widen an approved class of change into specific unlisted targets.
- Explicitly wants to avoid over-complicating the project.

## Where continuity lives

The user works across two machines and wants the record to travel with the repo:

1. **GitHub issues** — design decisions and open questions.
2. **`docs/handoffs/`** — session handoffs, this file included. Write yours here, not to `/tmp`.
3. **Commit messages** — rationale for each change.

There is also machine-local memory at `~/.claude/projects/-home-gpetrini--doom-d/memory/`, but it does **not** cross machines. Anything that matters belongs in one of the three above.

The `episodic-memory` plugin has been disabled at the user's request — do not suggest it, and do not treat its absence as a reason to assume nothing was decided before. Read the issues and this directory instead.
