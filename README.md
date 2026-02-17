# brain-ex
Brain extension note taking and task management app. Minimalist, fast and intuitive.

## Help
```
Usage: brex < command > < subcommand > < argument >

brex init
brex note < add | edit | connect | last >
brex task < add | list | done | delay | last >
brex update < file >
brex sql

defaults:
init -> sqlite database only
note -> log note add
task -> add new task
update -> rebuild from vault
sql -> sqlite shell

brex < command > -h or --help for more info
```

## Initilazation
It is possible to initilize with Obsidian vault or without it.

## TODOs
- align README help/examples with current CLI features (`brex [brain] ...`, `init --name`, `init --git`, `note add --update`)
- add end-to-end tests for full `brex update` behavior (without `--file`)
- add CLI support to list configured brains and switch default brain

## Directory Structure

| Directory | Purpose |
|-----------|---------|
| `bld/` | Build scripts |
| `bin/` | Binary output (gitignored) |
| `src/` | Source code |
| `tst/` | Tests |
| `doc/` | Documentation |
