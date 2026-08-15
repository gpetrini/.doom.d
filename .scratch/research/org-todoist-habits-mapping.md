# org-todoist and Todoist Recurrence: Does the Habits Bucket Work "For Free"?

Source: [Lillenne/org-todoist](https://github.com/Lillenne/org-todoist), file `org-todoist.el`
(fetched via GitHub Contents API, commit on `main` as of 2026-08-15; 3623 lines).

## Summary Answer

**Partial credit, not "for free."**

- org-todoist **does** write a real org repeater cookie (e.g. `+1w`, `++1w`) onto the
  `SCHEDULED`/`DEADLINE` timestamp when a synced Todoist task has `due.is_recurring = true`.
  It is not just overwriting a bare date on every sync — it reconstructs the repeater from
  Todoist's `due.string` field on every pull. (Question 1)
- org-todoist **never** sets `STYLE: habit` (or any equivalent) anywhere in the codebase.
  There is no reference to `"STYLE"`, `"habit"`, or `org-habit` anywhere in `org-todoist.el`
  (confirmed by exhaustive case-insensitive grep of the full source). (Question 2)
- Consequently: a recurring Todoist task synced into the "Habits" org-agenda view is a normal
  `TODO` with a proper repeater, so `org-habit`'s consistency-graph rendering (which requires
  `STYLE_PROPERTY: habit` in the `PROPERTIES` drawer) will **not** activate automatically.
  Getting the org-habit graph requires a small post-processing step (a sync hook or manual
  edit) that adds `:STYLE: habit:` to the property drawer of tasks org-todoist already tagged
  with `is_recurring` — the repeater syntax itself does not need to be touched, since
  org-todoist already produces valid `+Nunit`/`++Nunit` style repeaters that org-habit can read.

---

## 1. Does org-todoist write a real repeater, or just overwrite a plain date?

**It writes a real repeater**, reconstructed from Todoist's `due.string` on every sync.

The pull-direction (Todoist → org) chain is:

`org-todoist--update-tasks` → `org-todoist--schedule` → `org-todoist--create-planning` →
`org-todoist--scheduled-date` / `org-todoist--deadline-date` → `org-todoist--get-timestamp` →
`org-todoist--get-ts-from-date` (base date) + `org-todoist--add-repeater` (repeater cookie).

The critical function, `org-todoist--get-timestamp` (`org-todoist.el:2058-2096`):

```elisp
(defun org-todoist--get-timestamp (SYMBOL TASK)
  "Get a timestamp object for scheduled, deadline, or closed SYMBOL under TASK."
  (when-let* ((DATEOBJ (assoc-default SYMBOL TASK))
              (date (assoc-default 'date DATEOBJ))
              (hasdate (org-todoist--has-date date))
              (ts (org-todoist--get-ts-from-date date)))
    (when (eq t (assoc-default 'is_recurring DATEOBJ))
      (org-todoist--add-repeater ts (assoc-default 'string DATEOBJ)))
    ...
    ts))
```

`DATEOBJ` here is Todoist's `due` (or `deadline`) object — exactly the `{date, is_recurring,
string, ...}` shape from the API. If `is_recurring` is `t`, it calls
`org-todoist--add-repeater` on the freshly-built timestamp, passing Todoist's natural-language
recurrence `string` (e.g. `"every 2 weeks"`).

`org-todoist--add-repeater` (`org-todoist.el:1187-1240`) parses that string and mutates the
org timestamp element's repeater properties directly:

```elisp
(defun org-todoist--add-repeater (timestamp string)
  "Add a repeater value to TIMESTAMP from Todoist STRING."
  (unless (org-todoist--unsupported-recurring-date-type string)
    (let* ((normalized-string (s-downcase string))
           (repeater-type (if (string-match-p "!" normalized-string)
                              'restart 'cumulate))
           (interval (org-todoist--parse-interval normalized-string))
           (unit (org-todoist--parse-unit normalized-string))
           ...)
      (org-element-put-property timestamp :repeater-type repeater-type)
      (org-element-put-property timestamp :repeater-unit
                                (org-todoist--get-repeater-symbol normalized-string))
      (org-element-put-property timestamp :repeater-value (or interval 1))
      ...)
    timestamp))
```

- `repeater-type` is `'restart` (org's `++`/reset-from-today semantics, triggered by a literal
  `!` in Todoist's string, e.g. Todoist's "every!" markers) or `'cumulate` (org's plain `+`,
  i.e. shift forward from the last scheduled date) otherwise.
- `:repeater-unit`/`:repeater-value` become org's day/week/month/year/hour count, parsed out of
  Todoist's string via `org-todoist--get-repeater-symbol` (`org-todoist.el:1242-1250`) and
  `org-todoist--parse-interval`/`org-todoist--parse-unit`.
- Special-cased time-of-day words ("morning"/"afternoon"/"evening"/"night") in Todoist's string
  are mapped to fixed hour/minute values with a daily repeater.

Unsupported Todoist recurrence idioms are explicitly rejected before any repeater is attached,
via `org-todoist--unsupported-recurring-date-type` (`org-todoist.el:1179-1185`):

```elisp
(defun org-todoist--unsupported-recurring-date-type (string)
  "Check if STRING is an unsupported recurring date pattern.
Return t if the pattern is unsupported by `org-mode' (e.g. variable intervals)."
  (let ((normalized (s-downcase string)))
    (or (string-match-p "\\bworkday\\b" normalized)
        (string-match-p "\\bweekday\\b" normalized)
        (string-match-p ",[^,]*," normalized)))) ; Multiple days separated by commas
```
("workday", "weekday", and comma-separated multi-day patterns have no direct org repeater
equivalent, so org-todoist falls back to a plain non-repeating timestamp for those.)

The resulting planning line/timestamp element is what actually gets rendered into the org
buffer via `org-todoist--create-planning` and `org-todoist--schedule`
(`org-todoist.el:2119-2142`):

```elisp
(defun org-todoist--create-planning (TASK)
  "Create a planning element for Todoist TASK API response."
  (let ((props nil)
        (sch (org-todoist--scheduled-date TASK))
        (dead (org-todoist--deadline-date TASK))
        (closed (org-todoist--closed-date TASK)))
    (when sch (setq props (plist-put props :scheduled sch)))
    (when dead (setq props (plist-put props :deadline dead)))
    (when closed (setq props (plist-put props :closed closed)))
    (when props (org-element-create 'planning props))))

(defun org-todoist--schedule (HEADLINE TASK)
  "Add planning information to HEADLINE using API response info TASK."
  (let ((planning (org-todoist--create-planning TASK)))
    (if-let ((existing (org-element-map HEADLINE 'planning ...)))
        (if planning
            (org-element-set existing planning)
          (org-element-extract existing))
      (when planning (org-element-insert-before planning (org-todoist--get-property-drawer HEADLINE))))
    (when (eq t (assoc-default 'is_recurring (assoc-default 'due TASK)))
      (org-todoist--add-prop HEADLINE "is_recurring" t))))
```

Since `sch`/`dead` already carry the `:repeater-type`/`:repeater-unit`/`:repeater-value`
properties set by `org-todoist--add-repeater`, the `SCHEDULED`/`DEADLINE` line written into
the buffer serializes with a proper cookie, e.g. `SCHEDULED: <2026-08-22 Sat +1w>`, not a bare
date. So each sync re-derives (and correctly re-serializes) the repeater rather than clobbering
it with a plain date — the answer to Q1 is "yes, a real repeater is generated," and it survives
resync because it is *recomputed from Todoist's `due.string` every time*, not hand-preserved
from local edits.

Note `org-todoist--schedule` also sets a plain org PROPERTY `is_recurring: t` on the headline
(via `org-todoist--add-prop`) — this is a bookkeeping flag org-todoist uses to detect recurring
tasks on the *push* path (see `org-todoist--task-is-recurring`,
`org-todoist.el:1252-1265`), not an org-habit-related property.

### Completion / re-sync behavior for recurring tasks

`org-todoist--item-close-hook` (`org-todoist.el:537-559`) is the hook that fires when a task
is marked DONE:

```elisp
(defun org-todoist--item-close-hook ()
  "Send an item_close request for TODO at point."
  ...
  (when (and (equal org-state org-todoist-done-keyword)
             (or (org-entry-get nil "is_recurring")
                 (org-todoist--task-is-recurring (org-element-resolve-deferred (org-element-at-point)) t)))
    (when-let* ((id (org-entry-get nil org-todoist--id-property))
                (args `(("id" . ,id)))
                (request-data `(("sync_token" . ,(org-todoist--get-sync-token))
                                ("commands" . ((("type" . "item_close") ...))))))
      ...)))
```

For a recurring task, marking it DONE fires org's own native repeater advance (org-mode
reopens the TODO locally and bumps `SCHEDULED`/`DEADLINE` by the cookie, standard org
behavior — org-todoist does not reimplement this), *and* sends Todoist an `item_close` command
(rather than a full completion/delete), so the Todoist-side task also stays alive and advances
its own due date server-side. On the next sync, `org-todoist--get-timestamp` simply
re-derives the timestamp+repeater from the server's fresh `due` object again.

---

## 2. Is `STYLE: habit` (or equivalent) ever applied automatically?

**No.** Exhaustive case-insensitive search of the entire file:

```
$ grep -n -iE "STYLE|habit" org-todoist.el
(no matches)
```

Neither the literal string `"STYLE"` nor `"habit"` nor `org-habit` appears anywhere in
`org-todoist.el`. There is no code path that inspects `org-habit-mode`, writes a
`:STYLE: habit:` property, or otherwise integrates with `org-habit`. The only "recurring"
metadata org-todoist itself writes is the plain `is_recurring` PROPERTY (used purely for its
own push/pull bookkeeping, see `org-todoist--task-is-recurring`,
`org-todoist.el:1252-1265`, and `org-todoist--schedule`, `org-todoist.el:2130-2142`).

Consequence: a recurring Todoist task, once synced, is exactly a normal org `TODO` headline
with a `SCHEDULED`/`DEADLINE` repeater and an `is_recurring` custom property — nothing about it
signals `org-habit` to render a consistency graph or treat it specially in the agenda. To get
`org-habit` behavior you must add `STYLE_PROPERTY: habit` (or the appropriate
`:STYLE: habit:` line) to affected headlines' PROPERTIES drawers yourself, e.g. via a
post-sync hook keyed off the same `is_recurring` property org-todoist already sets.

---

## 3. Where SCHEDULED/DEADLINE strings get constructed — full call graph

### Pull direction (Todoist API → org buffer)

| Function | Location | Role |
|---|---|---|
| `org-todoist--update-tasks` | `org-todoist.el:2144` | Top-level loop over the `items`/tasks array of a sync response; creates/updates task headlines, calls `org-todoist--schedule` for each. |
| `org-todoist--schedule` | `org-todoist.el:2130-2142` | Builds/replaces the `planning` element (SCHEDULED/DEADLINE/CLOSED line) on a headline; sets the `is_recurring` property. |
| `org-todoist--create-planning` | `org-todoist.el:2119-2128` | Assembles the org `planning` element from `:scheduled`/`:deadline`/`:closed` timestamp objects. |
| `org-todoist--scheduled-date` | `org-todoist.el:2040-2042` | Wraps `org-todoist--get-timestamp 'due TASK`. |
| `org-todoist--deadline-date` | `org-todoist.el:2044-2046` | Wraps `org-todoist--get-timestamp 'deadline TASK`. |
| `org-todoist--closed-date` | `org-todoist.el:2035-2038` | Builds an inactive timestamp from `completed_at`. |
| `org-todoist--get-timestamp` | `org-todoist.el:2058-2096` | **Core function.** Reads Todoist's `due`/`deadline` object, builds the base timestamp via `org-todoist--get-ts-from-date`, and if `is_recurring` is true, calls `org-todoist--add-repeater` to attach the repeater cookie. Also folds `duration` into a timestamp range if `org-todoist-duration-as-timestamp` is set. |
| `org-todoist--get-ts-from-date` | `org-todoist.el:2048-2056` | Parses Todoist's `date` string (date-only or RFC3339 datetime, `Z`-suffixed UTC or local) into a plain org timestamp element, no repeater. |
| `org-todoist--add-repeater` | `org-todoist.el:1187-1240` | Parses Todoist's `due.string` natural-language recurrence text and mutates the timestamp's `:repeater-type`/`:repeater-unit`/`:repeater-value` (and optionally time-of-day) properties. |
| `org-todoist--unsupported-recurring-date-type` | `org-todoist.el:1179-1185` | Guards against Todoist recurrence idioms with no org repeater equivalent (workday/weekday/multi-day-of-week lists). |
| `org-todoist--get-repeater-symbol` | `org-todoist.el:1242-1250` | Maps a substring match ("week"/"day"/"month"/"year"/"hour") in the Todoist string to the org repeater-unit symbol. |
| `org-todoist--parse-interval`, `org-todoist--parse-unit`, `org-todoist--parse-weekday`, `org-todoist--parse-time-component`, `org-todoist--parse-date-component` | near `org-todoist.el:1130-1165` | Regex-based sub-parsers feeding `org-todoist--add-repeater`. |

### Push direction (org buffer → Todoist API), for completeness

| Function | Location | Role |
|---|---|---|
| `org-todoist--todoist-date-object-for-kw` | `org-todoist.el:~1113-1118` | Builds the JSON `due`/`deadline` object to send to Todoist from a headline's org timestamp; if `org-todoist--task-is-recurring` is true, pushes `("is_recurring" . t)` and `("string" . (org-todoist--repeater-to-string ...))`. |
| `org-todoist--repeater-to-string` | `org-todoist.el:1167-1177` | Converts an org `:repeater-value`/`:repeater-unit` pair back into a Todoist-style string, e.g. `"every 2 week"` / `"every other week"`. |
| `org-todoist--task-is-recurring` | `org-todoist.el:1252-1265` | Checks the `is_recurring` property or the presence of `:repeater-type` on SCHEDULED/DEADLINE to decide if a task should be treated as recurring when pushing. |
| `org-todoist--item-close-hook` | `org-todoist.el:537-559` | `org-after-todo-state-change-hook`-style hook; on DONE for a recurring task, POSTs an `item_close` command instead of completing/deleting, relying on org's native repeater to reopen/advance the entry locally. |

### Confirmed absence of habit-related code

```
$ grep -n -iE "recurring|repeater|STYLE|habit|is_recurring" org-todoist.el
```
returned only the lines cataloged above (recurring/repeater matches); zero matches for
`STYLE` or `habit` in the entire 3623-line file.

---

## Practical takeaway for org-habit integration

Because `org-todoist--schedule` already stamps every recurring task's headline with a plain
`is_recurring` PROPERTY (`org-todoist.el:2141-2142`), and because the SCHEDULED/DEADLINE
timestamp already carries a valid org repeater cookie by the time it lands in the buffer, a
minimal post-processing hook needs only to:

1. Run after `org-todoist-sync` completes (e.g. advise/hook on `org-todoist--do-sync-callback`
   or `org-todoist-sync`), and
2. For every headline where `(org-entry-get nil "is_recurring")` is non-nil, ensure a
   `:STYLE: habit:` line exists in its PROPERTIES drawer (and, per `org-habit` requirements,
   that the timestamp is on `SCHEDULED`, not `DEADLINE`, since `org-habit` only reads
   `SCHEDULED` repeaters).

No rewriting of the SCHEDULED string itself is needed — org-todoist's repeater generation
(§1 above) already produces syntax org-habit can parse directly.
