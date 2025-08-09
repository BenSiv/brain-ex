# brain-ex
Brain extension note taking and task management app. Minimalist, fast and intuitive.

## Setup

* add lua-utils submodule to the LUA_PATH variable
```
LUA_PATH="<lua-utils>?.lua;;"
```

## Help
```
Usage: brex < command > < argument >

       brex init
       brex note < add | edit | last >
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

### Initilazation
It is possible to initilize with Obsidian vault or without.

### Notes
You can add note using 'add' argument, and also append to daily note without argument.

### Tasks
List tasks, create, mark as done, or delay due date.

### update
Reset database based on vault content.

### SQL
Query sqlite database either by passing a query or interactively in the shell.


### TODOs
- should the default note be just timestamped instead of daily?
- add note connect argument
- fix update from file after manual edit
- edit with non existent title should create and open it in default editor
