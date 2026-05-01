-- src/knowledge_pool.lua
knowledge_pool = {}

database = require("database")
local_query = database.local_query
local_update = database.local_update

-- Record interaction in the pool
function knowledge_pool.record_interaction(brain_file, source_type, source_id, interaction_type)
    -- interaction_type: 'retrieval', 'use', 'validation'
    -- Record the interaction with a weight increment
    query = string.format("INSERT INTO knowledge_pool (source_type, source_id, interaction_type, weight) VALUES ('%s', '%s', '%s', 1.0);",
        source_type, source_id, interaction_type)
    local_update(brain_file, query)

    total_weight = 0
    res = local_query(brain_file, string.format("SELECT SUM(weight) FROM knowledge_pool WHERE source_type='%s' AND source_id='%s';", source_type, source_id))
    if res != nil and type(res) == "table" and res[1] != nil then
        total_weight = res[1].SUM or res[1][1] or 0
    end

    new_status = 'cold'
    if total_weight > 10 then
        new_status = 'hot'
    elseif total_weight > 3 then
        new_status = 'warm'
    end

    update_status = string.format("UPDATE knowledge_pool SET weight = %f, status = '%s' WHERE source_type='%s' AND source_id='%s';",
        total_weight, new_status, source_type, source_id)
    local_update(brain_file, update_status)
end

-- Ensure knowledge pool table exists
function knowledge_pool.ensure_table(brain_file)
    create_table = """
        CREATE TABLE IF NOT EXISTS knowledge_pool (
            id INTEGER PRIMARY KEY,
            source_type TEXT,
            source_id TEXT,
            interaction_type TEXT,
            timestamp TIMESTAMP DEFAULT (datetime('now', 'localtime')),
            weight REAL DEFAULT 1.0,
            status TEXT DEFAULT 'cold' -- cold, warm, hot
        );
        CREATE INDEX IF NOT EXISTS idx_pool_source ON knowledge_pool(source_type, source_id);
    """
    local_update(brain_file, create_table)
end

-- Get top retrieved items
function knowledge_pool.get_hot_items(brain_file, limit)
    limit = limit or 10
    query = string.format("""
        SELECT source_type, source_id, COUNT(*) as usage_count
        FROM knowledge_pool
        GROUP BY source_type, source_id
        ORDER BY usage_count DESC
        LIMIT %d;
    """, limit)
    return local_query(brain_file, query)
end

knowledge_pool.ensure_table = knowledge_pool.ensure_table
knowledge_pool.record_interaction = knowledge_pool.record_interaction
knowledge_pool.get_hot_items = knowledge_pool.get_hot_items

return knowledge_pool
