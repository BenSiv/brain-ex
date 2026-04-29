-- src/agent_tools/bridge.lua
bridge = {}

task = require("task")
note = require("note")

function bridge.dispatch(tool_name, method, args)
    if tool_name == "task" then
        if method == "add" then
            return task.add_task(nil, args) -- Needs brain_file
        end
    end
    return nil, "Tool not found"
end

return bridge
