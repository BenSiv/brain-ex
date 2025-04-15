# brain-ex
Brain extension note taking and task management app. Minimalist, fast and intuitive.

## Setup

* add lua-utils submodule to the LUA_PATH variable
```
LUA_PATH="<lua-utils>?.lua;;"
```

## Help
```
Usage: brex < command > < arguments >

       brex init < --vault >
       brex note < add | edit | last >
       brex task < list | add | done | delay >
       brex update
       brex sql < --query >
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
