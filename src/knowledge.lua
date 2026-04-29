-- src/knowledge.lua
knowledge = {}

database = require("database")
local_query = database.local_query

-- Basic retrieval using LIKE
function knowledge.retrieve(query_str)
    query = "SELECT id, content FROM notes WHERE content LIKE '%" .. query_str .. "%' LIMIT 5;"
    results = local_query(nil, query) -- Need to pass brain_file
    return results or {}
end

-- Propagate ideas by scanning content for existing keywords/notes
function knowledge.propagate(id, content)
    -- Stub for linking logic: scan content for keywords found in other note subjects
    print("Propagating ideas for note: " .. id)
    return true
end

return knowledge
