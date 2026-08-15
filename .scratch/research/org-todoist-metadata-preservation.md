# org-todoist: Metadata Preservation Across Sync

Source examined: `org-todoist.el` (single-file package, 3623 lines, commit at
HEAD of `main` as of 2026-08-15), `README.org`, `test.el`, and the GitHub
issue tracker at https://github.com/Lillenne/org-todoist.
Repository: https://github.com/Lillenne/org-todoist

## (a) Do custom org properties (ORG_GTD, ORG_GTD_TIMESTAMP, CATEGORY, ORG_GTD_PROJECT_IDS) survive a sync round-trip?

**Answer: Yes, they survive.** org-todoist does an in-place AST merge, not a
full regeneration of headlines/property drawers.

Evidence:

1. The file is parsed once per sync into a full org-element AST
   (`org-todoist--file-ast`, line 2382), which captures every existing
   headline, property, and drawer, including ones org-todoist knows nothing
   about.

2. On pull, existing task nodes are looked up by ID and mutated in place via
   `org-todoist--get-or-create-node` (line 2015), which calls
   `org-todoist--add-all-properties` (line 2549):

   ```elisp
   (defun org-todoist--add-all-properties (NODE PROPERTIES &optional SKIP)
     "Add or update the values of all properties in the alist PROPERTIES.
   Properties are added to NODE unless they are in plist SKIP.
   RETURNS the mutated NODE."
     (dolist (kv PROPERTIES)
       (unless (member (car kv) SKIP)
   ```

   This only *adds or updates* the specific keys present in the Todoist API
   response (id, labels, assignee, etc., minus a skip-list of fields handled
   elsewhere: `org-todoist--task-skip-list`, line 431). It never clears or
   iterates over the full property drawer, so unrelated existing properties
   (ORG_GTD, ORG_GTD_TIMESTAMP, CATEGORY, ORG_GTD_PROJECT_IDS, etc.) are
   never touched, added, or removed.

3. `org-todoist--add-prop` (line 2503) / `org-todoist--set-prop-in-place`
   (line 2477) only replace the value of the named key if it already exists,
   or append a new node-property for that one key — again, no drawer-level
   wipe.

4. The buffer write-back, `org-todoist--update-file` (line 2354), confirms
   the design intent explicitly in its docstring: *"Use diff to only apply
   changes rather than rewriting the entire file, preserving marks and point
   location."* It serializes the (mutated) AST to a string and uses
   `replace-buffer-contents` for an efficient diff-based buffer update — the
   AST itself, and thus any untouched properties, originated from the
   original buffer parse.

5. The literal org `CATEGORY` property is never referenced by name anywhere
   in the source (`grep -n -i "CATEGORY" org-todoist.el` only matches an
   unrelated internal term "category-node" used for metadata/user headline
   bookkeeping, unrelated to the org `:CATEGORY:` property).

No GitHub issue was found reporting loss of unrelated/custom properties on
sync (searched issue titles/bodies for "custom", "property", "overwrite" —
see Confidence note below).

**Confidence: High** for the mechanism (grounded directly in the merge code
and the explicit "diff, don't rewrite" docstring). **Medium** for the
absence-of-bug-reports claim, since GitHub's issue search only covers
issue title/body text, not all historical discussion, and no automated test
in `test.el` exercises this scenario explicitly.

## (b) Do custom TODO keywords (NEXT/WAIT/KILL, e.g. from org-gtd) survive sync?

**Answer: Partially — depends on the item's state in Todoist.**

Core function, `org-todoist--set-todo` (line 2280), called unconditionally
on every pulled task via `org-todoist--update-tasks` (line 2185):

```elisp
(defun org-todoist--set-todo (NODE CHECKED &optional DELETED)
  "Set the TODO state of NODE from the CHECKED and DELETED properties.
CHECKED and DELETED are from the Todoist API response."
  (cond ((eql t DELETED)
         (org-element-put-property NODE :todo-keyword org-todoist-deleted-keyword)
         (org-element-put-property NODE :todo-type 'done))
        ((eql t CHECKED)
         (org-element-put-property NODE :todo-keyword org-todoist-done-keyword)
         (org-element-put-property NODE :todo-type 'done))
        (t
         (org-element-put-property NODE :todo-type 'todo)
         (unless (org-element-property :todo-keyword NODE)
           (org-element-put-property NODE :todo-keyword org-todoist-todo-keyword)))))
```

- If the task is **still active (not checked, not deleted) in Todoist**, the
  keyword text is only defaulted to `org-todoist-todo-keyword` ("TODO") when
  *no keyword is present at all*. If a keyword already exists (e.g. `NEXT`
  or `WAIT`), it is left untouched. So NEXT/WAIT **do survive** as long as
  the item stays open on the Todoist side. (Note: `:todo-type` is still
  force-set to `'todo` in this branch regardless of what it was, which is
  harmless for keywords whose local org-todo-keywords config already types
  them as "todo"-class, but is a latent inconsistency for a keyword like
  `KILL` that a user's local config types as "done"-class — the *text*
  "KILL" survives in the buffer, but the in-memory `:todo-type` gets flipped
  to `'todo` for that sync pass.)

- If Todoist reports the task as **checked (done) or deleted**, the keyword
  is hard-overwritten to `org-todoist-done-keyword` (default "DONE") or
  `org-todoist-deleted-keyword` (default "CANCELED") respectively — any
  custom completed/cancelled keyword such as `KILL` is **lost** and replaced
  with the generic default.

- On the push side (`org-todoist--push`, line 1460), only the binary
  `:todo-type` (todo vs. done) is compared to the previous sync state (line
  1626: `(when (not (equal todo-type old-todo-type)) ...)`), and whether to
  send `item_delete` vs `item_complete` is decided purely by string-matching
  the keyword against `org-todoist-deleted-keyword` (line 1628:
  `(if (string= org-todoist-deleted-keyword todo-kw) ...)`). Todoist only
  has done/not-done semantics, confirming the premise in the question.

- README.org footnote 4 (line 382) states: *"Changing todo-keywords only
  triggers an update if the todo-state changes or the keyword is the
  `org-todoist-deleted-keyword`."* This confirms that switching between two
  "not-done"-type custom keywords (e.g., TODO → NEXT → WAIT) triggers no
  push at all, and is therefore purely a local-only distinction that
  org-todoist does not model or interfere with — until the state flips to
  "done" on either side.

No GitHub issue currently discusses org-gtd-style keyword loss explicitly.

**Confidence: High** for the mechanism as described (directly grounded in
the `org-todoist--set-todo` code and README footnote). The nuance about
"survives only while not-done" is a direct logical consequence of the code,
not speculation.

## (c) Do locally-set Effort and Priority properties survive the next sync?

**Answer: No — both are unconditionally re-derived from Todoist's fields on
every pull**, though in most workflows the *push* step (which happens before
pull in the same sync) propagates local edits to Todoist first, so a genuine
local edit is not silently discarded — it round-trips through Todoist's
lossy 4-level priority scale and minute/day duration granularity.

**Priority** — `org-todoist--set-priority` (line 2258), called
unconditionally for every task on pull (`org-todoist--update-tasks`, line
2183: `(org-todoist--set-priority task (assoc-default 'priority data))`):

```elisp
(defun org-todoist--set-priority (NODE PRIORITY)
  "Set priority of NODE to equivalent of Todoist PRIORITY."
  (org-element-put-property NODE :priority (cond
                                            ((equal PRIORITY 4) org-todoist-p1)
                                            ((equal PRIORITY 3) org-todoist-p2)
                                            ((equal PRIORITY 2) org-todoist-p3)
                                            ((equal PRIORITY 1) org-todoist-p4))))
```

This always overwrites `:priority` from Todoist's 4-level integer, mapped
through the four `org-todoist-p1..p4` chars (default `?A`–`?D`). Any org
priority value outside that 4-level mapping (e.g., using a wider
`org-priority-highest`/`org-priority-lowest` range) would collapse to the
nearest of the four configured levels on the next pull.

**Effort** — `org-todoist--set-effort` (line 2245), also called
unconditionally on pull (line 2182):

```elisp
(defun org-todoist--set-effort (NODE TASK)
  "Set the EFFORT property of NODE using the API response data TASK."
  (when-let* ((duration (assoc-default 'duration TASK))
              (amount (assoc-default 'amount duration))
              (unit (assoc-default 'unit duration))
              (effortval (cond
                          ((string-equal unit "minute") amount)
                          ((string-equal unit "day") (* 1440 amount))
                          (t nil))))
    ;; Only set EFFORT if duration-as-timestamp is disabled
    (unless org-todoist-duration-as-timestamp
      (org-todoist--add-prop NODE "EFFORT" (org-duration-from-minutes effortval)))))
```

Important asymmetry: this only fires `when-let*` succeeds, i.e. **only when
Todoist actually has a `duration` value for the task**. If the task has no
duration set on Todoist, this function is a no-op and a purely local EFFORT
value is left untouched. But if a duration exists on Todoist (which will be
the case any time the local EFFORT was ever pushed, since push sends
`("duration" . ...)` in every `item_update`/`item_add`, lines 1532/1617),
then EFFORT is force-rewritten from Todoist's stored duration on every pull.

Net effect for both properties: because org-todoist performs push-then-pull
within one `org-todoist-sync` call, a genuine local edit is sent to Todoist
first and then read back — so it is not lost in the typical single-client
round trip. But the property is not "preserved" in the sense of being left
alone; it is recomputed from Todoist's canonical value every single sync,
which means (i) any precision/format not representable in Todoist's
priority/duration model is lost, and (ii) if org-todoist's push step is
skipped, disabled, or fails for that field (see push guard condition at
line 1596–1604, which only sends effort/priority when they differ from the
last-synced snapshot), a stale/out-of-band local edit made between syncs by
another tool could be overwritten on the next pull.

**Confidence: High** for the mechanism (both functions are short, explicit,
and unconditionally invoked on pull). **Medium** for the practical
round-trip-survives-in-normal-use conclusion, since it depends on push
firing correctly first in the same sync pass and no GitHub issue was found
either confirming or refuting real-world data loss for these two fields.
