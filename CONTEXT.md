# Context: Doom Emacs Academic Configuration

## Glossary

**Activity tag** — An org-mode tag prefixed with `@` that describes the kind of mental work a task requires. Used in the GTD context to filter tasks by what you can do right now (e.g., `@writing`, `@reading`). Multiple activity tags can be combined on a single task.

**Location tag** — An org-mode tag prefixed with `@` that describes where a task must happen. Currently: `@home`, `@unicamp`, `@santanna`, `@MADE`, `@server`. Any institution not in the alist can be typed manually.

**Bib note** — A denote file with the `:bib:` filetag, created via `citar-denote` when opening or creating notes for a bibliography entry. Linked to a citar/Zotero key via the `#+reference:` property. The primary container for reading annotations.

**Meta note** — A denote file with the `:meta:` filetag. Collects and synthesizes multiple related bib notes or concept notes on a topic. Future: linked to bib notes via denote backlinks.

**Placeholder note** — A denote file with the `:placeholder:` filetag. A stub for a concept that exists in the knowledge base but has not yet been developed.

**org-noter category** — A heading title used inside bib notes when annotating PDFs with org-noter. These headings form the canonical vocabulary for org-ql queries across the note library. See ADR-0001.

**FISH-5SS** — A structured reading framework (Five-Second Summary). Kept as a fallback section in bib note templates but no longer the primary workflow. Primary workflow is org-noter annotation → org-ql retrieval.

**GTD Area of Focus** — A named domain used by org-gtd to organize actions and projects. Distinct from tags: areas are project-level categories, tags are task-level context descriptors.

**Denote filetag** — A keyword in the `#+filetags:` header of a denote file (e.g., `:bib:`, `:meta:`, `:journal:`). Managed by denote at file creation. Distinct from org heading tags (`:tag:`) and org-tag-alist activity/location tags.

**Daily panoramic view** — The unified GTD engagement command (`my/gtd-daily-view`, bound to `, D e`). Shows today's schedule, ticklers, and delegations at the top, followed by per-area blocks (next-actions, stuck projects, someday) for every GTD Area of Focus that has at least one item. Areas with no items are omitted. Replaces the need to run `org-gtd-engage` and manually browse org files.
