-- src/agent_tools/bridge.lua
bridge = {}

task = require("task")
note = require("note")
sql = require("sql")

function bridge.dispatch(brain_file, tool_name, method, args)
    if tool_name == "task" then
        if method == "add" then
            return task.add_task(brain_file, args)
        end
    elseif tool_name == "sql" then
        if method == "query" then
            return sql.sqlite_query(brain_file, args["query"])
        end
    end
    return nil, "Tool not found"
end

return bridge
