# brain-ex
Brain extension note taking and task management app. Minimalist, fast and intuitive.

## Help
```
Usage: brex [brain] <command> [subcommand] [arguments]

brex init
brex brain < list | use >
brex [brain] note < add | edit | connect | last >
brex [brain] task < add | list | done | delay | last >
brex [brain] update < file >
brex [brain] sql
brex [brain] agent < view | ask | note | task >
brex [brain] knowledge < search | sync | browse | process | queue | promote >

defaults:
init -> sqlite database only
brain -> list configured brains
note -> log note add/edit
task -> add new task
update -> rebuild from vault
sql -> sqlite shell
agent -> view log, or ask prompt
knowledge -> search, process, and promote note knowledge

brex <command> -h or --help for more info
```

## Examples
- `brex init --vault my_vault --name work --editor vim --git`
- `brex note add --title "standup" --subject "work" --content "daily sync" --update`
- `brex work task add --content "finish docs" --due_to "2026-02-20"`
- `brex work knowledge search "database indexes" --limit 5`
- `brex work knowledge process`
- `brex work knowledge promote 3 --tier 2`
- `brex brain list`
- `brex brain use --name work`

## Knowledge Layer
`brex knowledge` indexes notes into a SQLite-backed knowledge pool, records retrieval history, reinforces frequently reused notes, detects exact duplicates, and promotes useful notes into vault-backed markdown artifacts.

See [doc/knowledge.md](doc/knowledge.md) for the full command reference, schema notes, retrieval model, promotion workflow, and test coverage.

## Initilazation
It is possible to initilize with Obsidian vault or without it.

## TODOs
- support positional shorthand for switching default brain (`brex brain use <name>`)
- support incremental vault sync in `brex update` (without full database rebuild)
- add `brex brain remove --name <name>` for cleaning obsolete brain entries

## Directory Structure

| Directory | Purpose |
|-----------|---------|
| `bld/` | Build scripts |
| `bin/` | Binary output (gitignored) |
| `pub/` | Publication artifacts (e.g. `.deb`, `.rpm`, release archives) |
| `src/` | Source code |
| `tst/` | Tests |
| `doc/` | Documentation |
