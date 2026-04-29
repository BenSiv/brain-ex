-- src/agent.lua
agent = {}

database = require("database")
local_update = database.local_update
local_query = database.local_query
bx_utils = require("bx_utils")
generate_id = bx_utils.generate_id

-- Constants for agent-tagged tasks
AGENT_TAG = "agent"
dataframes = require("dataframes")
view = dataframes.view
agent_engine = require("agent_engine")

-- Utility to check if a column exists in a table
function column_exists(brain_file, table_name, column_name)
    query = "PRAGMA table_info(" .. table_name .. ");"
    columns = local_query(brain_file, query)
    if columns == nil then return false end
    for _, col in pairs(columns) do
        -- Check both 'name' key and integer index 2 (name)
        if col["name"] == column_name or col[2] == column_name then
            return true
        end
    end
    return false
end

-- Ensure 'owner' column exists in tasks table
function ensure_owner_column(brain_file)
    -- Check if table exists first
    check_table = "SELECT name FROM sqlite_master WHERE type='table' AND name='tasks';"
    if local_query(brain_file, check_table) == nil or #local_query(brain_file, check_table) == 0 then
        return
    end

    if not column_exists(brain_file, "tasks", "owner") then
        local_update(brain_file, "ALTER TABLE tasks ADD COLUMN owner TEXT;")
    end
end

function do_agent(brain_file, cmd_args)
    ensure_owner_column(brain_file)
    
    subcommand = cmd_args[1]
    
    -- Default behavior:
    -- If no subcommand, show view
    -- If the first arg looks like a prompt (and isn't a known subcommand), assume 'run'
    if subcommand == nil or subcommand == "" then
        subcommand = "view"
    elseif subcommand != "view" and subcommand != "tasks" and subcommand != "run" and subcommand != "-h" and subcommand != "--help" then
        subcommand = "run"
        table.insert(cmd_args, 1, subcommand) -- Re-insert as the first arg for parsing consistency
    end
    
    if subcommand == "-h" or subcommand == "--help" then
        help = require("help")
        print(help.get_help_string("brex agent"))
        return "success"
    end
    
    if subcommand == "view" then
        -- Implementation: open agent.log in pager
        -- Assuming agent.log is in the same directory as the brain file or root
        log_file = "agent.log" -- Needs correct path resolution
        pager = os.getenv("PAGER") or "less"
        os.execute(pager .. " " .. log_file)
        return "success"
    elseif subcommand == "tasks" then
        -- List tasks where owner = 'agent'
        query = "SELECT id, subject, content, due_to, overdue FROM tasks WHERE owner = '" .. AGENT_TAG .. "' AND done IS NULL;"
        result = local_query(brain_file, query)
        if result  !=  nil then
            -- Use the existing view function if possible
            -- Assuming 'view' is global or accessible
            view(result, {columns={"id", "subject", "content", "due_to", "overdue"}})
        else
            print("No agent tasks.")
        end
        return "success"
    elseif subcommand == "run" then
        prompt = cmd_args[2]
        return agent_engine.handle_prompt(prompt)
    else
        print("Unknown agent command: " .. (subcommand or ""))
        return "error"
    end
end

agent.do_agent = do_agent
return agent
