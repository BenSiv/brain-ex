-- src/knowledge.lua
knowledge = {}

database = require("database")
local_query = database.local_query
local_update = database.local_update
config = require("config")
agent_engine = require("agent_engine")
knowledge_pool = require("knowledge_pool")

-- Try to load sqlite-vss
function knowledge.load_vss(db)
    -- This depends on lsqlite3 supporting load_extension and the .so being available
    -- In some environments, it might be disabled for security
    success, err = pcall(function()
        db.load_extension(db, "vector0")
        db.load_extension(db, "vss0")
    end)
    return success, err
end

function knowledge.ensure_vss_tables(brain_file)
    -- Create a virtual table for embeddings if it doesn't exist
    -- We need to know the dimension. qwen2.5:0.5b or similar might be 896 or 1024 or 1536
    -- For now, let's assume we use a specific embedding model or get it dynamically
    -- common dimensions: 384 (all-MiniLM-L6-v2), 768 (bert), 1536 (ada-002)

    -- We'll use a helper table to store the dimension and model used
    create_metadata = """
        CREATE TABLE IF NOT EXISTS vss_metadata (
            key TEXT PRIMARY KEY,
            value TEXT
        );
    """
    local_update(brain_file, create_metadata)

    -- Check if we already have a dimension set
    res = local_query(brain_file, "SELECT value FROM vss_metadata WHERE key='dimension';")
    dimension = (res and res[1] and (res[1].value or res[1][1])) or "1536" -- Default to 1536

    -- Create the VSS table
    -- vss_notes will store the embedding for each note
    -- we use the rowid from the notes table or just map title/subject
    -- VSS tables in sqlite-vss 0.1.x look like this:
    create_vss = string.format("CREATE VIRTUAL TABLE IF NOT EXISTS vss_notes USING vss0(embedding(%s));", dimension)

    -- However, we need to load extension before running this.
    -- Since we use local_update which opens and closes the DB,
    -- we might need a way to load extension in local_update.
end

-- Basic retrieval using LIKE
function knowledge.retrieve(brain_file, query_str)
    query = "SELECT title, subject, content FROM notes WHERE content LIKE '%" .. query_str .. "%' LIMIT 5;"
    results = local_query(brain_file, query)
    return results or {}
end

function knowledge.get_embeddings(text)
    provider_name, model_name = config.get_embedding_config()
    command_tmpl = config.get_embedding_command()
    status, provider = pcall(require, "agent_providers." .. provider_name)
    if status == false or provider == nil or provider.embeddings == nil then
        return nil, "Provider does not support embeddings"
    end
    return provider.embeddings(model_name, text, command_tmpl)
end

function knowledge.search(brain_file, query_str)
    -- 1. Get embedding for query_str
    emb, err = knowledge.get_embeddings(query_str)

    -- Record retrieval interaction
    knowledge_pool.record_interaction(brain_file, "search", query_str, "retrieval")

    if emb == nil then
        print("Error getting embeddings: " .. tostring(err))
        return knowledge.retrieve(brain_file, query_str) -- Fallback
    end

    -- 2. Search in VSS table
    -- This requires loading extension first
    -- For now, this is a placeholder as we need to figure out extension loading
    print("Vector search for: " .. query_str)

    -- Fallback to LIKE for now until VSS is fully wired
    return knowledge.retrieve(brain_file, query_str)
end

function knowledge.sync(brain_file)
    -- Sync all notes to VSS table
    notes = local_query(brain_file, "SELECT title, subject, content FROM notes;")
    if notes == nil or #notes == 0 then return "success" end

    for _, note in ipairs(notes) do
        title = note.title or note[1]
        subject = note.subject or note[2]
        content = note.content or note[3]

        -- Get embedding and update vss_notes
        -- (Placeholder)
    end
end

function do_knowledge(brain_file, cmd_args)
    subcommand = cmd_args[1]
    if subcommand == "search" then
        query_str = cmd_args[2]
        if query_str == nil or query_str == "" then
            print("Usage: brex knowledge search <query>")
            return "error"
        end
        results = knowledge.search(brain_file, query_str)
        -- display results
        dataframes = require("dataframes")
        dataframes.view(results)
    elseif subcommand == "sync" then
        knowledge.sync(brain_file)
    else
        print("Usage: brex knowledge <search|sync> [query]")
    end
    return "success"
end

knowledge.retrieve = knowledge.retrieve
knowledge.search = knowledge.search
knowledge.sync = knowledge.sync
knowledge.do_knowledge = do_knowledge

return knowledge
