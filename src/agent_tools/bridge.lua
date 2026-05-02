-- src/agent_tools/bridge.lua
bridge = {}

task = require("task")
note = require("note")
sql = require("sql")
database = require("database")
local_query = database.local_query

function normalize_rows(rows, columns)
    lines = {}
    if rows == nil or #rows == 0 then
        return "(empty)"
    end

    for _, row in ipairs(rows) do
        values = {}
        for _, column in ipairs(columns) do
            value = row[column]
            if value == nil then
                value = ""
            end
            table.insert(values, column .. "=" .. tostring(value))
        end
        table.insert(lines, table.concat(values, ", "))
    end

    return table.concat(lines, "\n")
end

function bridge.dispatch(brain_file, tool_name, method, args)
    if tool_name == "task" then
        if method == "add" then
            if args["owner"] == nil or args["owner"] == "" then
                args["owner"] = "agent"
            end
            return task.add_task(brain_file, args)
        elseif method == "done" then
            return task.mark_done(brain_file, args)
        elseif method == "delay" then
            return task.delay_due(brain_file, args)
        elseif method == "list" then
            rows = local_query(brain_file, "SELECT id, subject, content, due_to, overdue FROM tasks WHERE done IS NULL ORDER BY due_to, subject;")
            return normalize_rows(rows, {"id", "subject", "content", "due_to", "overdue"})
        end
    elseif tool_name == "note" then
        if method == "add" then
            return note.take_note(brain_file, args)
        elseif method == "log" then
            return note.log_note(brain_file, args)
        elseif method == "connect" then
            return note.do_note_connect(brain_file, args)
        elseif method == "read" then
            subject = args["subject"] or ""
            title = args["title"] or ""
            query = string.format("SELECT subject, title, content FROM notes WHERE subject='%s' AND title='%s';", subject, title)
            rows = local_query(brain_file, query)
            return normalize_rows(rows, {"subject", "title", "content"})
        elseif method == "last" then
            subject = args["subject"] or "log"
            number = tonumber(args["number"] or "5") or 5
            query = string.format("SELECT subject, title, content FROM notes WHERE subject='%s' ORDER BY title DESC LIMIT %s;", subject, number)
            rows = local_query(brain_file, query)
            return normalize_rows(rows, {"subject", "title", "content"})
        end
    elseif tool_name == "sql" then
        if method == "query" then
            return sql.sqlite_query(brain_file, args["query"])
        end
    end
    return nil, "Tool not found"
end

return bridge
