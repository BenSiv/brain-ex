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

function bridge.check_permission(tool_name, method, args)
    destructive_tools = {
        ["task"] = { ["add"] = true, ["done"] = true, ["delay"] = true },
        ["note"] = { ["add"] = true, ["log"] = true, ["connect"] = true },
        ["sql"] = { ["query"] = true }
    }
    
    if destructive_tools[tool_name] != nil and destructive_tools[tool_name][method] != nil then
        if os.getenv("BATS_TEST_FILENAME") != nil or os.getenv("BREX_TEST") != nil then
            return true
        end

        io.write(string.format("\n[Safety Gate] Agent requests tool execution: %s.%s\n", tool_name, method))
        if args != nil and next(args) != nil then
            io.write("Arguments:\n")
            for k, v in pairs(args) do
                io.write(string.format("  %s: %s\n", k, tostring(v)))
            end
        end
        io.write("Approve execution? (y/N): ")
        io.flush()
        answer = io.read()
        if answer == nil then
            return false, "User denied execution permission (non-interactive EOF)."
        end
        if answer == "y" or answer == "Y" then
            return true
        else
            return false, "User denied execution permission."
        end
    end
    return true
end

function bridge.dispatch(brain_file, tool_name, method, args)
    allowed, err = bridge.check_permission(tool_name, method, args)
    if allowed == false then
        return nil, err
    end

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
            subject = ""
            if args["subject"] != nil then
                subject = args["subject"]
            end
            title = ""
            if args["title"] != nil then
                title = args["title"]
            end
            query = string.format("SELECT subject, title, content FROM notes WHERE subject='%s' AND title='%s';", subject, title)
            rows = local_query(brain_file, query)
            return normalize_rows(rows, {"subject", "title", "content"})
        elseif method == "last" then
            subject = "log"
            if args["subject"] != nil then
                subject = args["subject"]
            end
            number_input = "5"
            if args["number"] != nil then
                number_input = args["number"]
            end
            number = 5
            if tonumber(number_input) != nil then
                number = tonumber(number_input)
            end
            query = string.format("SELECT subject, title, content FROM notes WHERE subject='%s' ORDER BY title DESC LIMIT %s;", subject, number)
            rows = local_query(brain_file, query)
            return normalize_rows(rows, {"subject", "title", "content"})
        end
    elseif tool_name == "sql" then
        if method == "query" then
            rows = sql.sqlite_query(brain_file, args["query"])
            if rows == nil then return "Error: Query failed" end
            if #rows == 0 then return "(empty)" end
            
            -- Dynamically determine columns from first row if possible
            columns = {}
            for k, _ in pairs(rows[1]) do
                if type(k) == "string" then
                    table.insert(columns, k)
                end
            end
            table.sort(columns)
            
            return normalize_rows(rows, columns)
        end
    end
    return nil, "Tool not found"
end

return bridge
