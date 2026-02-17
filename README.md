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

defaults:
init -> sqlite database only
brain -> list configured brains
note -> log note add/edit
task -> add new task
update -> rebuild from vault
sql -> sqlite shell

brex <command> -h or --help for more info
```

## Examples
- `brex init --vault my_vault --name work --editor vim --git`
- `brex note add --title "standup" --subject "work" --content "daily sync" --update`
- `brex work task add --content "finish docs" --due_to "2026-02-20"`
- `brex brain list`
- `brex brain use --name work`

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
| `src/` | Source code |
| `tst/` | Tests |
| `doc/` | Documentation |
