# ADR-0001: Professor-Oriented Tag and org-noter Category Taxonomy

**Date:** 2026-05-28  
**Status:** Accepted

## Context

The Doom Emacs configuration was built during a PhD, accumulating tags, GTD areas of focus, and note categories reflecting a graduate student's workflow: `@dissertation`, `@concurso`, `@PED`, `@ysi` as primary location/activity tags; FISH-5SS as the primary reading framework; org-roam as the note system (later abandoned for denote).

After completing the PhD and starting as a professor, these tags became obsolete or misleading. The primary workflows shifted to: teaching, research paper writing, grant work, student supervision, peer review, editorial service, and institutional administration.

A second architectural shift also occurred: the primary reading workflow moved from filling FISH-5SS templates upfront to annotating PDFs with org-noter and querying annotations on the fly with org-ql. This required a canonical heading vocabulary for org-noter categories that org-ql can reliably match.

## Decision

### Two-layer tag system

Tags are split into two non-overlapping layers, combinable on any task:

**Activity tags** (what kind of work):
`@writing`, `@paper`, `@reading`, `@coding`, `@reviewing`, `@grading`, `@supervising`, `@meetings`, `@email`, `@bureaucracy`, `@service`, `@planning`, `@teaching`, `@lecture-prep`, `@grant`, `@learning`, `@emacs`, `@workflow`, `@conference`, `@travel`, `@free`, `@personal`, `@ysi`, `@presentation`, `@editing`, `@seminar`

**Location tags** (where the work happens):
`@home`, `@unicamp`, `@santanna`, `@MADE`, `@server`

Key distinctions preserved:
- `@teaching` (class delivery) vs `@lecture-prep` (preparation)
- `@reviewing` (peer/editorial review) vs `@grading` (student work)
- `@bureaucracy` (red tape) vs `@service` (institutional citizenship)
- `@writing` (any prose) vs `@paper` (research paper specifically)

### org-noter canonical category vocabulary (21 categories)

`Takeaway`, `Background`, `Arguments`, `Contribution`, `Theoretical`, `Empirical`, `Model`, `Data`, `Methodology`, `Results`, `Observations`, `Limitations`, `Critique`, `Policy`, `Future Work`, `Related Work`, `Literature`, `Debate`, `Questions`, `Definition`, `Example`

These heading titles are the query targets for org-ql paper digest views. They must not be renamed without updating existing bib notes.

### GTD Areas of Focus

Teaching, Lectures, Supervisions, Paper-related, Grants, Service, Editorial, MADE, YSI, Unicamp, SantAnna, Conferences, Events and Trips, Paper reviews, Bureaucracy, Email, Research Groups, Home/Chores, Health, Reading list, Literature update, Planning, Meetings, Computer-related, Emacs-related, Github, Inbox

## Alternatives Considered

**Coarse activity tags** (`@research`, `@teaching`, `@admin`) — rejected. Too broad for context-switching: "I have 30 minutes, what can I do?" requires finer granularity than knowing you have research tasks.

**Single `@reviewing` tag** — rejected. Peer review and student grading are different mental modes and different scheduling contexts; separating them allows better filtering.

**FISH-5SS as primary note structure** — retained as fallback only. The org-noter + org-ql workflow produces the same information incrementally during reading rather than requiring upfront structure.

## Consequences

- Existing bib notes using old FISH-5SS table format remain valid but are not queryable via the new org-noter categories.
- New bib notes use the lightweight template: Metadata → Takeaway → Related Notes → FISH-5SS (fallback).
- org-ql queries targeting org-noter categories will only return results from notes created after this ADR.
- The `#+TAGS:` header in denote templates is replaced with `#+FILETAGS:` for denote compatibility.
