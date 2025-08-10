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
note -> todays note add/edit
task -> add new task
update -> rebuild from vault
sql -> sqlite shell

brex < command > -h or --help for more info
```

## Initilazation
It is possible to initilize with Obsidian vault or without it.

## TODOs
- should the default note be just timestamped instead of daily?
- fix update from file after manual edit
- edit with non existent title should create and open it in default editor
