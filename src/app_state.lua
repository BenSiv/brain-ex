-- Define a module table
local app_state = {}

local config = require("config")
local database = require("database")

-- Create application state table
function app_state.create()
    local brain_file = config.get_brain_file()
    if not brain_file then
        return nil, "No brain file configured. Run 'brex init' first."
    end
    
    return {
        brain_file = brain_file,
        vault_path = config.get_vault_path(),
        default_editor = config.get_default_editor(),
        config = config,
        database = database
    }
end

-- Helper functions that operate on state
function app_state.execute_sql(state, query)
    return state.database.execute(state.brain_file, query)
end

function app_state.query_sql(state, query)
    return state.database.query(state.brain_file, query)
end

function app_state.cleanup(state)
    state.database.close_connection(state.brain_file)
end

-- Export the module
return app_state
