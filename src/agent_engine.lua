-- src/agent_engine.lua
agent_engine = {}

config = require("config")
paths = require("paths")
database = require("database")
bridge = require("agent_tools.bridge")

function agent_engine.get_provider_config()
    return config.get_agent_config()
end

function agent_engine.run_agent(subagent, prompt, brain_file)
    provider_name, model_name = agent_engine.get_provider_config()
    
    status, provider = pcall(require, "agent_providers." .. provider_name)
    if status == false then
        status, provider = pcall(require, provider_name)
    end

    if status == false or provider == nil then
        print("Error: Could not load provider '" .. provider_name .. "': " .. tostring(provider))
        return "error"
    end
    
    system_prompt = ""
    if not (subagent == nil or subagent == "") then
        p_status, loaded_prompt = pcall(require, "agents." .. subagent)
        
        if p_status == false then
            -- If subagent is 'worker', it might be at the root if legacy or compiled weirdly
            -- but generally we expect agents.<name>
            p_status_fallback, loaded_prompt_fallback = pcall(require, subagent)
            if p_status_fallback then
                p_status = true
                loaded_prompt = loaded_prompt_fallback
            end
        end

        if p_status == true then
            system_prompt = loaded_prompt
        else
            -- Check if it's a "module not found" or an internal error in the module
            missing_module = false
            if type(loaded_prompt) == "string" then
                missing_module = string.match(loaded_prompt, "module '.*' not found") != nil
            end
            if missing_module then
                print("Warning: subagent '" .. subagent .. "' not found.")
            else
                print("Error loading subagent '" .. subagent .. "': " .. tostring(loaded_prompt))
            end
            system_prompt = ""
        end
    end

    print("Running " .. subagent .. " via " .. provider_name .. " (" .. model_name .. ")...")

    current_prompt = prompt
    for _ = 1, 2 do
        result, err = provider.generate(model_name, system_prompt, current_prompt)

        if err != nil then
            print("Error: " .. err)
            return "error"
        end

        if result == nil then
            print("Error: Agent returned no result.")
            return "error"
        end

        done_message = string.match(result, "<done>%s*(.-)%s*</done>")
        if done_message != nil then
            print("Agent reply:\n" .. done_message)
            return "success"
        end

        tool_name = string.match(result, "<tool>%s*(.-)%s*</tool>")
        method_name = string.match(result, "<method>%s*(.-)%s*</method>")
        args_str = string.match(result, "<args>%s*(.-)%s*</args>")

        if tool_name == nil or method_name == nil then
            print("Agent reply:\n" .. result)
            return "success"
        end

        print("Agent requested tool: " .. tool_name .. "." .. method_name)
        args = {}
        if args_str != nil then
            for line in string.gmatch(args_str, "[^\r\n]+") do
                k, v = string.match(line, "^(.-)=(.*)$")
                if k != nil and v != nil then
                    args[k] = v
                end
            end
        end

        tool_result, tool_err = bridge.dispatch(brain_file, tool_name, method_name, args)

        tool_summary = tostring(tool_result)
        if tool_err != nil then
            tool_summary = "ERROR: " .. tostring(tool_err)
        elseif tool_result == true then
            tool_summary = "ok"
        end

        current_prompt = prompt .. "\n\nTool call result for " .. tool_name .. "." .. method_name .. ":\n" .. tool_summary
    end

    print("Agent reply:\nUnable to complete tool-assisted run in one follow-up.")

    return "success"
end

function agent_engine.process_tasks(brain_file)
    query = "SELECT id, subject, content FROM tasks WHERE owner = 'agent' AND done IS NULL;"
    result = database.local_query(brain_file, query)
    if not result or #result == 0 then
        print("No pending tasks for agent.")
        return "success"
    end
    for _, task in ipairs(result) do
        task_id = task.id or task[1]
        prompt = "Please handle task. Subject: " .. (task.subject or task[2]) .. "\nContent: " .. (task.content or task[3] or "")
        print("Processing task " .. task_id .. "...")
        agent_engine.run_agent("worker", prompt, brain_file)
        database.local_update(brain_file, "UPDATE tasks SET done = datetime('now') WHERE id = '" .. task_id .. "';")
    end
    return "success"
end

return agent_engine
