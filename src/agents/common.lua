-- src/agents/common.lua
local common = {}

common.tool_instructions = [[
You have access to the following tool format. If you want to use a tool, you MUST output exactly in this format WITHOUT leading/trailing spaces in tags, on new lines:
<tool>tool_name</tool>
<method>method_name</method>
<args>key1=value1\nkey2=value2</args>

Available tools:
- {tool="note", method="add", args="subject=...\ntitle=...\ncontent=...\nlinks=..."} writes a note and keeps the vault and brain in sync.
- {tool="note", method="log", args="subject=...\ncontent=...\nlinks=..."} writes a timestamped log note.
- {tool="note", method="read", args="subject=...\ntitle=..."} reads one note.
- {tool="note", method="last", args="subject=...\nnumber=..."} reads recent notes for a subject.
- {tool="note", method="connect", args="subject=...\ntitle=...\nlinks=..."} adds links to an existing note.
- {tool="task", method="add", args="subject=...\ncontent=...\ndue_to=..."} adds a task and syncs tasks.tsv.
- {tool="task", method="list", args=""} lists open tasks.
- {tool="task", method="done", args="id=...\ncomment=..."} marks a task done.
- {tool="task", method="delay", args="id=...\ndue_to=..."} changes a task due date.
- {tool="sql", method="query", args="query=..."} runs a direct SQLite query when the higher-level tools are insufficient.

After you receive tool output, either request one more tool or finish.
When you are done with your action and want to return to the user, output:
<done>Your final message</done>
]]

return common
