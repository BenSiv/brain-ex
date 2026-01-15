-- Define a module table
help = {}

function get_help_string(command)
    help_strings = {
        ["brex"] = """
Usage: brex [brain] <command> [subcommand] [arguments]

brex init
brex [brain] note < add | edit | connect | last >
brex [brain] task < add | list | done | delay | last >
brex [brain] update < file >
brex [brain] sql

defaults:
init -> sqlite database only
note -> log note add/edit
task -> add new task
update -> rebuild from vault
sql -> sqlite shell

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

    Examples:
    brex init
    brex init --name "my_brain"
    brex init --vault "my_vault"
    brex init --name "my_brain" --vault "my_vault" --editor "vim"
    brex init --name "my_brain" --vault "my_vault" --editor "vim" --git
        """,
        ["brex note"] = """
Description:
Create log note with the specified content.
Links can be provided as a comma-separated list.

Required:
-c --content <content> Note's content.

Optional:
-l --links <links> Links to other notes, separated by commas.

Examples:
brex note --content "This is log note content"
brex note --content "This is log note content" --links "link1,link2"
        """,
        ["brex note add"] = """
Description:
Adds a new note with the specified title, and content.
Links can be provided as a comma-separated list.

Required:
-t --title <title> Note's title.
-c --content <content> Note's content.

Optional:
-s --subject <subject> Subject of the note.
-l --links <links> Links to other notes, separated by commas.

Examples:
brex note add --title "My Note" --content "This is the content of my note"
brex note add --title "My Note" --content "This is the content of my note" --subject "My Subject" --links "link1,link2"
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
-n --number <number> Number of notes to display, default is 5.

Examples:
brex note last
brex note last --subject "log" --number 10
        """,
        ["brex note connect"] = """
Description:
Connect notes.

Optional:
-t --title <title>     Title of the note to connect, default is log note
-s --subject <subject> Subject of the note, default is log
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
-s --subject <subject> Task's subject, defaults to NULL.
-t --due_to <due_to> Task's due date in the format yyyy-mm-dd, defaults to 24 hours from now.

Examples:
brex task add --content "This is a new task"
brex task --content "This is a work task" --subject "work" --due_to "2024-12-31"
        """,
        ["brex task add"] = """
Description:
Adds a new task. The due date can be specified in the format yyyy-mm-dd HH:MM:SS, or part of it.

Required:
-c --content <content> Task's content.

Optional:
-s --subject <subject> Task's subject, defaults to NULL.
-t --due_to <due_to> Task's due date in the format yyyy-mm-dd, defaults to 24 hours from now.

Examples:
brex task add --content "This is a new task"
brex task --content "This is a work task" --subject "work" --due_to "2024-12-31"
        """,
        ["brex task list"] = """
Description:
Lists all tasks that are not done yet.

Optional: 
-s --subject <subject> Filter tasks by subject.
-t --due_to <due_to> Filter tasks by due date in the format yyyy-mm-dd.

Example:
brex task list
brex task list --subject "work"
brex task list --due_to "2024-12-31"
        """,
        ["brex task done"] = """
Description:
Marks a task as done by its ID and optionally adds a comment.

Required:
-i --id <id> ID of the task to mark as done.

Optional:
-m --comment <comment> Comment to add when marking the task as done.

Example:
brex task done
brex task done --comment "This task is completed"            
        """,
        ["brex task delay"] = """
Description:
Delays a task's due time, pass * for all tasks.

Required:
-i --id <id> ID of the task to delay, or * to delay all tasks.

Optional:
-t --due_to <due_to> New due date in the format yyyy-mm-dd HH:MM:SS, or part of it. If not provided, defaults to 24 hours from now.

Example:
brex task delay --id "85560914" --due_to "2024-12-31"
brex task delay --id "*"
        """,
        ["brex update"] = """
Description:
Update the brain database from the vault.
By default, this command will reinitialize the database and import all notes and tasks from the vault.

Options:
-f --file <note_path>  Update a specific note from the given file path instead of the entire vault.

Examples:
brex update
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
	    """
    }

    return help_strings[command]
end

help.get_help_string = get_help_string

return help
