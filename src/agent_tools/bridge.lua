-- src/agent_tools/bridge.lua
bridge = {}

task = require("task")
note = require("note")
sql = require("sql")

function bridge.dispatch(tool_name, method, args)
    if tool_name == "task" then
        if method == "add" then
            return task.add_task(nil, args) -- Needs brain_file
        end
    elseif tool_name == "sql" then
        if method == "query" then
            return sql.sqlite_query(nil, args["query"])
        end
    end
    return nil, "Tool not found"
end

return bridge
