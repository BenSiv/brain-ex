# Comments as content, not a separate concept

**Status: implemented, tested (140/140 `tst/*.bats`).** See "Live
rollout" at the end.

## Goal

`doc/unified-items-design.md` already folded "ongoing comments" into
`notes.content` at the data layer -- a task's comment log is just its
note's content, appended to. What's left is that the CLI still treats
"commenting" as its own task-only verb (`task comment`, `task done
--comment`), separate from the general "append to a note" operation
that already exists for plain notes (`note add --update`). Since a
task is nothing but a note with a `tasks` row attached, there is no
reason "add a log entry" should be a different command depending on
whether the note happens to have that row. This change removes the
task-specific comment verb entirely and makes `note add --update` the
one way to append to any item's content, task or plain note alike.

## What doesn't change

No schema change. `notes`/`tasks` stay exactly as
`doc/unified-items-design.md` left them -- this is a CLI-surface and
call-site change, not a data model change.

## What changes

### One shared "log entry" helper, used at every real append site

Today, `append_content(brain_file, subject, title, content)` in
`note.lua` is a plain, timestamp-free append -- `notes.content ..
"\n" .. content`. Task code (`mark_done`, `comment_task`) builds its
own `os.date("%Y-%m-%d %H:%M:%S") .. "\n" .. comment` string before
calling it, ad hoc, so only task comments ever got a timestamp line.
Plain `note add --update` does not.

New: `note.lua` exports a small helper --

```lua
function timestamped_entry(content)
    return os.date("%Y-%m-%d %H:%M:%S") .. "\n" .. content
end
```

-- and every call site that represents "append a new log entry to an
existing item" builds its string through it before calling
`append_content`/`write_note`, instead of task.lua being the only
place that ever did this. Concretely:

- `take_note`'s `args["update"] == true` branch (the `note add
  --update` path, vault and non-vault both) wraps `content` in
  `timestamped_entry` before calling `append_content` /
  `write_note(..., "a")`.
- `log_note`'s repeat-append branch (hit only if two default `note`
  invocations land in the same wall-clock second, since the title is
  the timestamp itself) gets the same treatment, for the same reason:
  it's still "append a new entry," just a rare case.
- `find_or_create_note` in `task.lua` (the promotion path: `task add`
  with `-c` on a title that already exists as a note) wraps its
  `initial_content` in `timestamped_entry` too -- this is already
  documented as "appended as a comment instead," so it gets the same
  formatting the word "comment" implies everywhere else.

Left untouched, deliberately:
- A *new* note/task's initial content (`insert_note`, `write_note(...,
  "w")`, the non-existing branch of `find_or_create_note`) stays raw,
  no timestamp -- that's the item's body, not a log entry appended to
  it. `notes.time` already records creation time.
- `do_note_connect` -- appends `[[links]]` lines only, not content;
  unrelated to this change.

This makes "append a timestamped entry to an item's content" a
property of *appending*, not a property of *being a task* -- the same
formatting `task comment`/`task done --comment` always produced, now
reachable for any note through the one existing flag that already
means "append": `--update`.

### `task.lua`: comment verb removed

- `comment_task` deleted.
- `mark_done`: drops its `comment` arg entirely -- marks `tasks.done`
  and nothing else. No more building a `"DONE: " .. comment` string
  inline.
- `-m --comment` removed from `task`'s `arg_string` (it had no other
  use). `-m` becomes free for reuse if a future flag needs it.
- `valid_subs` / dispatch / the two "Unknown subcommand" usage lines
  drop `comment`.
- `find_or_create_note` (unchanged call shape, just now routes its
  append through `timestamped_entry` as above).

### Closing out a task, before vs. after

Before:
```
brex task done --id 12345678 --comment "Verified against real data, 166 rows"
```
After:
```
brex task done --id 12345678
brex note add --subject benchling --title "the task's title" --content "Verified against real data, 166 rows" --update
```
Two commands instead of one for the common "done + note" case is the
real ergonomic cost of this change -- accepted deliberately, per the
choice to make `comment` disappear as a concept rather than
generalizing it into a second command (`item comment --id ...`) or
duplicating it onto `note` (`note comment`). `task show --id <id>`
still needs `subject`/`title` looked up by id for the `note add`
call -- `task show --id <id>` (unchanged) prints both.

### `help.lua`

- `brex task comment` entry removed.
- `brex task done` loses its `-m --comment` line and the
  `--comment "..."` example; description drops "and optionally adds a
  final comment."
- Top-level `brex` usage and `brex task` subcommand lists (three
  places: `help.lua`'s top banner, `task.lua`'s two "Available
  subcommands" print statements) drop `comment`.
- `brex task list`'s "use `brex task show --id <id>` for the full
  comment history" and `brex task show`'s "full comment/content log"
  wording stay -- content is still the log, just appended-to through
  `note add` now.

### Agent-facing surface

- `agents/common.lua`'s `tool_instructions`: `task.done` no longer
  documents `comment=...`. `note.add` gains `update=...` in its
  documented args (it already accepted this via `take_note`, it was
  just never surfaced to the agent) -- this is how an agent leaves a
  comment on a task or note going forward.
- `agent_tools/bridge.lua` needs no dispatch change -- `task.mark_done`
  and `note.take_note` are already the functions it calls; it forwards
  `args` straight through either way.

### Tests

- `tst/task.bats` / `tst/task_enhanced.bats`: delete `task comment`
  coverage, drop `--comment` from `task done` cases, add/keep a case
  that `note add -u` on a task's own subject/title appends visibly in
  `task show`'s content.
- `tst/note.bats`: new case asserting `note add --update` produces a
  timestamped entry (was previously untested since nothing exercised
  timestamped appends outside `task.lua`).
- Any `agent.bats` case that exercises `task done` with a comment arg,
  or the tool-instructions text, updated to match.

## A real bug found only by testing: root-level notes mislabeled by subject

`update.lua`'s `update_note_from_file` (the single-file sync behind
`note edit`, `update --file`, and now every `note add --update` call
that goes through a vault) derived a note's subject from its path with
`string.match(note_path, ".*/([^/]+)/[^/]+%.md$")` -- "whatever
directory the file's immediate parent is." That's wrong the moment the
file has *no* subject: for a root-level note at
`<vault>/title.md`, the immediate parent directory is the vault root
itself, and the old pattern still matched, capturing the **vault
folder's own name** as the subject instead of `""`.

This was latent before this change -- nothing in the existing test
suite exercised a vault-backed brain, a subject-less note/task, *and*
a single-file resync in combination. The new `note add --update` path
does exactly that (closing out a subject-less task is a completely
ordinary case), and two new tests
(`note.bats`'s timestamp case, `task.bats`'s "note add --update on a
task appends without marking done") hit it immediately: the append
landed in the vault file correctly, but re-synced into a *second*,
spurious `notes` row under `subject='<vault folder name>'` instead of
updating the original `subject=''` row -- so the original row's
content read back unchanged and the assertion failed.

Fixed by computing the note's path relative to the vault root before
parsing subject/title out of it, anchoring on the vault directory's
own basename appearing as a path segment (handles both the
vault_path-prefixed absolute paths note.lua's own sync builds, and the
cwd-relative paths a user passes to `update --file`) rather than a
literal string-prefix match, which would have broken the existing
`update --file <relative path>` tests. Fixes every caller of
`update_note_from_file`, not just the new `note add --update` path --
`note edit` on a root-level note had the same latent bug.

## Migration for `~/documents/bensiv-notes`

Lighter than the unified-items migration -- **no schema change, no
content rewrite, no vault file changes**. Existing entries keep
whatever shape they already have:
- Legacy-migrated done-comments (`"DONE: " .. comment`, no timestamp
  line -- written once by `migrate_legacy_tasks`) are left exactly as
  they are; this design doesn't touch historical content, only how
  *new* entries get written.
- Entries already written by `task comment`/`task done --comment`
  (timestamp line + text) are indistinguishable in shape from what the
  new `note add --update` path will produce going forward -- nothing
  to reconcile.

What the rollout actually is:
1. Build the new binary (`bld/build.sh`), run the full `bats` suite.
2. Confirm (grepped already, see below) nothing outside this repo
   invokes `task comment` or `task done --comment` -- no cron job, no
   other script, no agent system prompt found referencing either.
   `agents/common.lua`'s own tool_instructions is the only in-repo
   "spec" an agent reads, and that's updated as part of this change.
3. Swap `/usr/local/bin/brex` for the new build, same as the
   unified-items rollout's step 4 -- back up the current binary first
   (cheap, matches existing practice, even though this change carries
   none of that migration's schema risk).
4. No `ensure_schema`/data-migration step runs on next use, since
   `sql_schema.sql_init`/`ensure_notes_id_column`/
   `migrate_legacy_tasks` are all unchanged and already idempotent --
   `bensiv-notes.db` needs nothing done to it.
5. Spot check: run `task show --id <id>` on a task with pre-existing
   comments before and after the swap and diff the output -- confirms
   read-side formatting is untouched, since this change only touches
   the append/write side.

No DB or vault backup is strictly required by this change's own logic
(nothing destructive happens to existing rows or files), but taking
one before the binary swap costs nothing and matches how the last
rollout was done.

### Live rollout: a second real bug, found by the rollout's own smoke test

Backed up `/usr/local/bin/brex` first, swapped in the new build, and
diffed `task show --id 240768930` (a task with a real multi-entry
comment log) before/after -- byte-identical, confirming the read side
is untouched as expected.

Then ran a write-side smoke test: `note add --update` on a real live
task to confirm the new "commenting is just note add -u" path actually
works end to end. Picked `enable database update of created/modified
since last backup` (one of the two tasks recovered earlier this
session) -- its title happens to contain a `/`, which turned out to
matter. The append silently landed in a **second, spurious `notes`
row** (`subject='benchling-to-sql'`, `title='...created-modified...'`)
instead of updating the real one, which was left with empty content.

Root cause, in `update_note_from_file` (the single-file sync behind
`note edit`, `update --file`, and now `note add --update`'s vault
path): it derived a note's title from its **filename**, but
`get_note_paths` sanitizes `/` to `-` when writing that filename (a
fix from the original unified-items rollout, so a slash in a title
doesn't get read as an extra path level). Reading title back out of
that sanitized filename never recovers the original `/` -- so the
lookup-by-`(subject, title)` used to find the existing row misses it
every time, and a new row gets created under the mangled title
instead. `vault_to_sql.lua`'s bulk walker has the identical
filename-derived title logic, so this was already a live, latent risk
for any of the vault's other 29 slash-containing titles -- it just
hadn't been triggered yet, because nothing had exercised a
single-file resync against one of them until this smoke test did.

Fixed by preferring the title already sitting in the file's own
frontmatter (`title: enable database update of created/modified since
last backup` -- task.lua writes it there verbatim, slash intact) over
the filename-derived one, whenever frontmatter is present. Plain notes
(no frontmatter) still derive title from the filename as before --
this fix covers exactly the class of file this feature actually
touches (task-tracked notes), not the harder, more general problem of
a slash-containing title on a *plain* note, which remains unsolved
(pre-existing, out of scope here). Added a regression test
(`tst/task.bats`, "note add --update on a task whose title contains a
slash updates the real row"); full suite re-run at 141/141 before
redeploying.

The spurious row this smoke test created against the real
`bensiv-notes.db`/vault was deleted and the target task's content
(and its vault file) restored to empty, its correct pre-test state, by
hand immediately after -- confirmed via `task show` and a direct
`notes` count before/after.
