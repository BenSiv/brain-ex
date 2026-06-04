-- Define a module table
help = {}

function get_help_string(command)
    help_strings = {
        ["brex"] = """
Usage: brex [brain] <command> [subcommand] [arguments]

brex init
brex brain < list | use >
brex [brain] note < add | edit | connect | last >
brex [brain] task < add | list | done | delay | prioritize | last >
brex [brain] update < file >
brex [brain] sql
brex [brain] agent < view | ask | note | task | process_tasks >
brex [brain] knowledge < search | sync | browse | show | history | process | queue | promote >

defaults:
init      -> sqlite database only
brain     -> list configured brains
note      -> log note add/edit
task      -> add new task
update    -> rebuild from vault
sql       -> sqlite shell
agent     -> view log, or ask prompt
knowledge -> search, process, and promote note knowledge

brex <command> -h or --help for more info
        """,
        ["brex init"] = """
Description:
Initializes a new brain-ex database in the current directory.
If a vault directory is specified, it will also import notes and tasks from the vault.

Options:
-n --name <name>      Name of the brain database (default: "brain").
-v --vault <vault>    Path to the vault directory to import notes and tasks from.
-e --editor <editor>  Default text editor to use (default: "nano").
-g --git              Initialize the vault as a git repository.

Examples:
brex init
brex init --name "my_brain"
brex init --vault "my_vault"
brex init --name "my_brain" --vault "my_vault" --editor "vim" --git
        """,
        ["brex brain"] = """
Description:
Manage configured brains.

Subcommands:
list                 Show configured brains and current default.
use --name <name>    Set default brain to one of the configured named brains.

Options:
-n --name <name> Brain name to use as default (for `use`).

Examples:
brex brain list
brex brain use --name "work"
        """,
        ["brex brain list"] = """
Description:
Lists all configured brains and identifies the current default brain.

Examples:
brex brain list
        """,
        ["brex brain use"] = """
Description:
Sets the default brain to the specified named brain.

Required:
-n --name <name> Name of the brain to use as default.

Examples:
brex brain use --name "work"
        """,
        ["brex note"] = """
Description:
Create log note with the specified content.
Links can be provided as a comma-separated list.

Optional:
-c --content <content> Note's content.
-l --links <links>     Links to other notes, separated by commas.
-s --subject <subject> Subject of the note, defaults to "log".

Examples:
brex note --content "This is log note content"
brex note --content "This is log note content" --links "link1,link2"
        """,
        ["brex note add"] = """
Description:
Adds a new note with the specified title and content.
Links can be provided as a comma-separated list.

Required:
-t --title <title>     Note's title.
-c --content <content> Note's content.

Optional:
-s --subject <subject> Subject of the note.
-l --links <links>     Links to other notes, separated by commas.
-u --update            Append to the note if it already exists.

Examples:
brex note add --title "My Note" --content "This is the content of my note"
brex note add --title "My Note" --content "This is the content of my note" --subject "My Subject" --links "link1,link2" --update
        """,
        ["brex note edit"] = """
Description:
Opens the specified note in the default editor for editing.
If the note does not exist, it will be created.

Required:
-t --title <title> Title of the note to edit.

Optional:
-s --subject <subject> Subject of the note.

Examples:
brex note edit --title "My Note"
brex note edit --title "My Note" --subject "My Subject"
        """,
        ["brex note last"] = """
Description:
Displays the last notes.
If no subject is provided, defaults to "log". The number of notes displayed can be specified with the --number option, defaulting to 5.

Optional:
-s --subject <subject> Subject of the notes to display.
-n --number <number>   Number of notes to display, default is 5.

Examples:
brex note last
brex note last --subject "log" --number 10
        """,
        ["brex note connect"] = """
Description:
Connect notes by adding links.

Optional:
-t --title <title>     Title of the note to connect, default is log note.
-s --subject <subject> Subject of the note, default is "log".
-l --links <links>     Links to other notes, separated by commas.

Examples:
brex note connect --title "note1" --links "note2,log/note3"
brex note connect --links "todo,review"
        """,
        ["brex task"] = """
Description:
Adds a new task. The due date can be specified in the format yyyy-mm-dd HH:MM:SS, or part of it.

Required:
-c --content <content> Task's content.

Optional:
-s --subject <subject>      Task's subject, defaults to NULL.
-t --due_to <due_to>        Task's due date, defaults to 24 hours from now.
-o --owner <owner>          Task owner, such as "agent".
-p --importance <1-5>       Task importance (1-5), default 1.
-u --urgency <1-5>          Task urgency (1-5), default 1.

Examples:
brex task add --content "This is a new task"
brex task --content "This is a work task" --subject "work" --due_to "2024-12-31" --importance 5
        """,
        ["brex task add"] = """
Description:
Adds a new task.

Required:
-c --content <content> Task's content.

Optional:
-s --subject <subject>      Task's subject, defaults to NULL.
-t --due_to <due_to>        Task's due date, defaults to 24 hours from now.
-o --owner <owner>          Task owner, such as "agent".
-p --importance <1-5>       Task importance (1-5), default 1.
-u --urgency <1-5>          Task urgency (1-5), default 1.

Examples:
brex task add --content "This is a new task"
brex task add --content "This is a work task" --subject "work" --due_to "2024-12-31" --importance 5
        """,
        ["brex task list"] = """
Description:
Lists all tasks that are not done yet.

Optional: 
-s --subject <subject> Filter tasks by subject.
-t --due_to <due_to>   Filter tasks by due date.
-o --owner <owner>     Filter tasks by owner, for example "agent".

Example:
brex task list
brex task list --subject "work"
brex task list --due_to "2024-12-31"
brex task list --owner "agent"
        """,
        ["brex task done"] = """
Description:
Marks a task as done by its ID and optionally adds a comment.

Required:
-i --id <id> ID of the task to mark as done.

Optional:
-m --comment <comment> Comment to add when marking the task as done.

Example:
brex task done --id 12345678
brex task done --id 12345678 --comment "This task is completed"            
        """,
        ["brex task delay"] = """
Description:
Delays a task's due time, pass * for all tasks.

Required:
-i --id <id> ID of the task to delay, or * to delay all tasks.

Optional:
-t --due_to <due_to> New due date. If not provided, defaults to 24 hours from now. Pass "indefinitely" to remove the due date.

Example:
brex task delay --id "85560914" --due_to "2024-12-31"
brex task delay --id "85560914" --due_to "indefinitely"
brex task delay --id "*"
        """,
        ["brex task prioritize"] = """
Description:
Update task importance and urgency.

Required:
-i --id <id> ID of the task to update, or * for all pending tasks.

Optional:
-p --importance <1-5> Task importance (1-5).
-u --urgency <1-5>    Task urgency (1-5).

Examples:
brex task prioritize --id 12345678 --importance 5 --urgency 4
brex task rank --id "*" --importance 3
        """,
        ["brex task last"] = """
Description:
Lists recently completed tasks.

Optional:
-s --subject <subject> Filter by subject.
-n --number <number>   Number of tasks to show (default 5).

Examples:
brex task last
brex task last --subject "work" --number 10
        """,
        ["brex update"] = """
Description:
Update the brain database from the vault.
By default, this command performs an incremental update (only changed files).

Options:
-f --file <note_path>  Update a specific note from the given file path.
-c --force             Force a full rebuild (drops and recreates all tables).

Examples:
brex update
brex update --force
brex update --file "/path/to/vault/subject/note.md"
        """,
        ["brex sql"] = """
Description:
Opens an interactive sqlite3 shell or runs a query on the database.

Optional:
-q --query <query> SQL query to run on the database.

Examples:
brex sql
brex sql --query "SELECT * FROM tasks;"
        """,
        ["brex agent"] = """
Description:
Manage and interact with the Brain-Ex AI agent.
Defaults to 'view' if no arguments are provided.
Defaults to 'ask' if a prompt is provided without a subcommand.

Subcommands:
view             Displays the agent log file in a pager.
ask <prompt>     Ask the general brain assistant.
note <prompt>    Run the note-focused assistant.
task <prompt>    Run the task-focused assistant.
process_tasks    Run the agent task processing loop.

Examples:
brex agent
brex agent "what did I write about retries?"
brex agent note "capture meeting notes for backend"
brex agent task "create follow-ups from yesterday"
brex agent process_tasks
        """,
        ["brex agent view"] = """
Description:
Displays the agent log file in a pager.

Examples:
brex agent view
        """,
        ["brex agent ask"] = """
Description:
Asks the general brain assistant a question or prompt.

Required:
<prompt> The question or instruction for the agent.

Examples:
brex agent ask "how do I use SQLite?"
        """,
        ["brex agent note"] = """
Description:
Runs the note-focused assistant with the specified prompt.

Required:
<prompt> The instruction for the note agent.

Examples:
brex agent note "summarize my recent logs"
        """,
        ["brex agent task"] = """
Description:
Runs the task-focused assistant with the specified prompt.

Required:
<prompt> The instruction for the task agent.

Examples:
brex agent task "what are my most urgent tasks?"
        """,
        ["brex agent process_tasks"] = """
Description:
Gathers background tasks (owner="agent") and executes them.

Examples:
brex agent process_tasks
        """,
        ["brex knowledge"] = """
Description:
Search, review, and promote notes through the knowledge pool.

Subcommands:
search <query> [--limit N]  Retrieve notes with tier and heat ranking.
sync                       Synchronize notes into the knowledge pool.
browse [--limit N]         List indexed knowledge and provenance.
show <id>                  Show details of one knowledge item.
history [id]               Show retrieval history for an item or all.
process                    Review duplicates and promotion readiness.
queue                      Show items ready for promotion or review.
promote <id> [--tier N]    Materialize an item into the vault knowledge tree.

Optional:
-n --limit <number>        Limit results (default 5 or 20).
-t --tier <number>         Target tier for promotion.
-s --status <status>       Artifact status for promotion.

Examples:
brex knowledge search "database optimization" --limit 5
brex knowledge browse --limit 20
brex knowledge show 123
brex knowledge promote 123 --tier 2
brex knowledge process
        """,
        ["brex knowledge search"] = """
Description:
Searches the indexed knowledge pool for the specified query.

Required:
<query> The search terms.

Optional:
-n --limit <number> Limit the number of results (default 5).

Examples:
brex knowledge search "git flow" --limit 3
        """,
        ["brex knowledge sync"] = """
Description:
Synchronizes notes from the database into the knowledge pool tables.

Examples:
brex knowledge sync
        """,
        ["brex knowledge browse"] = """
Description:
Lists indexed knowledge with concise provenance metadata.

Optional:
-n --limit <number> Limit the number of items shown (default 20).

Examples:
brex knowledge browse
        """,
        ["brex knowledge show"] = """
Description:
Shows detailed information and content for a specific knowledge item.

Required:
<id> The ID of the knowledge item to show.

Examples:
brex knowledge show 42
        """,
        ["brex knowledge history"] = """
Description:
Shows retrieval history. If an ID is provided, shows history for that item.

Optional:
[id] The ID of the knowledge item.

Examples:
brex knowledge history
brex knowledge history 42
        """,
        ["brex knowledge process"] = """
Description:
Runs the review loop across indexed notes to detect duplicates and mark promotion readiness.

Examples:
brex knowledge process
        """,
        ["brex knowledge queue"] = """
Description:
Shows items that need review (ready for promotion, duplicate, or stale).

Examples:
brex knowledge queue
        """,
        ["brex knowledge promote"] = """
Description:
Materializes a knowledge item into a markdown artifact in the vault.

Required:
<id> The ID of the knowledge item to promote.

Optional:
-t --tier <number>   Target tier (2 or 3).
-s --status <status> Artifact status (e.g., 'materialized', 'draft').

Examples:
brex knowledge promote 42 --tier 3
        """
    }

    return help_strings[command]
end
help.get_help_string = get_help_string

return help
