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
- fix update from file after manual edit
- edit with non existent title should create and open it in default editor
- if subject is passed but not title raise an error to the user (instead of writing to the daily note)
- parse NULL as empty string for view
- add update flag to append new content to existing note on add subcommand

## Directory Structure

| Directory | Purpose |
|-----------|---------|
| `bld/` | Build scripts |
| `bin/` | Binary output (gitignored) |
| `src/` | Source code |
| `tst/` | Tests |
| `doc/` | Documentation |
