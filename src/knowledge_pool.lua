-- src/knowledge_pool.lua
knowledge_pool = {}

database = require("database")
local_query = database.local_query
local_update = database.local_update
config = require("config")
paths = require("paths")
joinpath = paths.joinpath
lfs = require("lfs")

function knowledge_pool.escape_sql(value)
    if value == nil then
        return ""
    end
    return string.gsub(tostring(value), "'", "''")
end

function knowledge_pool.row_value(row, key, index, default_value)
    value = nil
    if row != nil then
        if row[key] != nil then
            value = row[key]
        elseif row[index] != nil then
            value = row[index]
        end
    end
    if value == nil then
        return default_value
    end
    return value
end

function knowledge_pool.number_value(value, default_value)
    number_value = tonumber(value)
    if number_value == nil then
        return default_value
    end
    return number_value
end

function knowledge_pool.note_source_id(subject, title)
    subject = subject or ""
    title = title or ""
    if subject == "" then
        return title
    end
    return subject .. "/" .. title
end

function knowledge_pool.note_source_ref(subject, title)
    source_id = knowledge_pool.note_source_id(subject, title)
    return source_id .. ".md"
end

function knowledge_pool.content_hash(content)
    content = content or ""
    normalized = string.lower(content)
    hash = 5381
    for i = 1, #normalized do
        hash = (hash * 33 + string.byte(normalized, i)) % 4294967296
    end
    return string.format("%08x", hash)
end

function knowledge_pool.slugify(text)
    text = string.lower(text or "knowledge")
    text = string.gsub(text, "[^%w]+", "-")
    text = string.gsub(text, "^-+", "")
    text = string.gsub(text, "-+$", "")
    if text == "" then
        text = "knowledge"
    end
    return text
end

function knowledge_pool.ensure_dir(path)
    mode = lfs.attributes(path, "mode")
    if mode == nil then
        status, err = lfs.mkdir(path)
        if status == nil and lfs.attributes(path, "mode") == nil then
            return nil, err
        end
    end
    return true
end

function knowledge_pool.ensure_table(brain_file)
    create_tables = """
        CREATE TABLE IF NOT EXISTS knowledge_pool (
            id INTEGER PRIMARY KEY,
            source_type TEXT,
            source_id TEXT,
            interaction_type TEXT,
            timestamp TIMESTAMP DEFAULT (datetime('now', 'localtime')),
            weight REAL DEFAULT 1.0,
            status TEXT DEFAULT 'cold'
        );
        CREATE INDEX IF NOT EXISTS idx_pool_source ON knowledge_pool(source_type, source_id);

        CREATE TABLE IF NOT EXISTS knowledge_items (
            id INTEGER PRIMARY KEY,
            source_type TEXT NOT NULL DEFAULT 'note',
            source_id TEXT NOT NULL,
            source_ref TEXT,
            subject TEXT,
            title TEXT,
            content TEXT,
            content_hash TEXT,
            tier INTEGER DEFAULT 1,
            process_level TEXT DEFAULT 'working',
            heat REAL DEFAULT 1.0,
            retrieval_count INTEGER DEFAULT 0,
            last_retrieved_at TEXT,
            duplicate_of INTEGER,
            merged_into INTEGER,
            artifact_kind TEXT,
            artifact_ref TEXT,
            artifact_path TEXT,
            artifact_status TEXT DEFAULT 'none',
            promotion_status TEXT DEFAULT 'pool',
            created_at TEXT DEFAULT (datetime('now', 'localtime')),
            updated_at TEXT DEFAULT (datetime('now', 'localtime')),
            UNIQUE(source_type, source_id)
        );
        CREATE INDEX IF NOT EXISTS idx_knowledge_items_source ON knowledge_items(source_type, source_id);
        CREATE INDEX IF NOT EXISTS idx_knowledge_items_tier ON knowledge_items(tier, process_level);
        CREATE INDEX IF NOT EXISTS idx_knowledge_items_artifact ON knowledge_items(artifact_status);
        CREATE INDEX IF NOT EXISTS idx_knowledge_items_hash ON knowledge_items(content_hash);

        CREATE TABLE IF NOT EXISTS knowledge_retrievals (
            id INTEGER PRIMARY KEY,
            query_text TEXT,
            created_at TEXT DEFAULT (datetime('now', 'localtime'))
        );

        CREATE TABLE IF NOT EXISTS knowledge_retrieval_results (
            retrieval_id INTEGER,
            knowledge_id INTEGER,
            rank INTEGER,
            score REAL,
            tier_weight REAL,
            reinforcement_delta REAL,
            PRIMARY KEY (retrieval_id, knowledge_id)
        );
        CREATE INDEX IF NOT EXISTS idx_knowledge_retrieval_results_item ON knowledge_retrieval_results(knowledge_id);

        CREATE TABLE IF NOT EXISTS knowledge_links (
            from_id INTEGER,
            to_id INTEGER,
            link_type TEXT,
            weight REAL DEFAULT 1.0,
            updated_at TEXT DEFAULT (datetime('now', 'localtime')),
            PRIMARY KEY (from_id, to_id, link_type)
        );

        CREATE TABLE IF NOT EXISTS knowledge_reviews (
            id INTEGER PRIMARY KEY,
            retrieval_id INTEGER,
            knowledge_id INTEGER,
            atomicity_status TEXT,
            connectivity_status TEXT,
            duplication_status TEXT,
            title_status TEXT,
            promotion_status TEXT,
            action_summary TEXT,
            created_at TEXT DEFAULT (datetime('now', 'localtime'))
        );
        CREATE INDEX IF NOT EXISTS idx_knowledge_reviews_item ON knowledge_reviews(knowledge_id);
    """
    return local_update(brain_file, create_tables)
end

function knowledge_pool.record_interaction(brain_file, source_type, source_id, interaction_type)
    knowledge_pool.ensure_table(brain_file)
    source_type = knowledge_pool.escape_sql(source_type)
    source_id = knowledge_pool.escape_sql(source_id)
    interaction_type = knowledge_pool.escape_sql(interaction_type)

    query = string.format("INSERT INTO knowledge_pool (source_type, source_id, interaction_type, weight) VALUES ('%s', '%s', '%s', 1.0);",
        source_type, source_id, interaction_type)
    local_update(brain_file, query)

    total_weight = 0
    res = local_query(brain_file, string.format("SELECT SUM(weight) AS total_weight FROM knowledge_pool WHERE source_type='%s' AND source_id='%s';", source_type, source_id))
    if res != nil and type(res) == "table" and res[1] != nil then
        total_weight = tonumber(knowledge_pool.row_value(res[1], "total_weight", 1, 0)) or 0
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

function knowledge_pool.upsert_note(brain_file, subject, title, content, note_time)
    subject = subject or ""
    title = title or ""
    content = content or ""
    note_time = note_time or os.date("%Y-%m-%d %H:%M:%S")

    source_id = knowledge_pool.note_source_id(subject, title)
    source_ref = knowledge_pool.note_source_ref(subject, title)
    content_hash = knowledge_pool.content_hash(content)

    esc_source_id = knowledge_pool.escape_sql(source_id)
    existing = local_query(brain_file, string.format("SELECT id, content_hash, artifact_status FROM knowledge_items WHERE source_type='note' AND source_id='%s';", esc_source_id))

    if existing != nil and existing[1] != nil then
        old_hash = knowledge_pool.row_value(existing[1], "content_hash", 2, "")
        artifact_status = knowledge_pool.row_value(existing[1], "artifact_status", 3, "none")
        artifact_update = ""
        if old_hash != content_hash and artifact_status != nil and artifact_status != "" and artifact_status != "none" then
            artifact_update = ", artifact_status='stale', promotion_status='stale'"
        end
        update_sql = string.format("""
            UPDATE knowledge_items
            SET source_ref='%s',
                subject='%s',
                title='%s',
                content='%s',
                content_hash='%s',
                updated_at=datetime('now', 'localtime')
                %s
            WHERE source_type='note' AND source_id='%s';
        """,
            knowledge_pool.escape_sql(source_ref),
            knowledge_pool.escape_sql(subject),
            knowledge_pool.escape_sql(title),
            knowledge_pool.escape_sql(content),
            knowledge_pool.escape_sql(content_hash),
            artifact_update,
            esc_source_id)
        local_update(brain_file, update_sql)
    else
        insert_sql = string.format("""
            INSERT INTO knowledge_items
                (source_type, source_id, source_ref, subject, title, content,
                 content_hash, tier, process_level, heat, retrieval_count,
                 artifact_status, promotion_status, created_at, updated_at)
            VALUES
                ('note', '%s', '%s', '%s', '%s', '%s',
                 '%s', 1, 'working', 1.0, 0,
                 'none', 'pool', '%s', datetime('now', 'localtime'));
        """,
            esc_source_id,
            knowledge_pool.escape_sql(source_ref),
            knowledge_pool.escape_sql(subject),
            knowledge_pool.escape_sql(title),
            knowledge_pool.escape_sql(content),
            knowledge_pool.escape_sql(content_hash),
            knowledge_pool.escape_sql(note_time))
        local_update(brain_file, insert_sql)
    end

    return true
end

function knowledge_pool.mark_duplicates(brain_file)
    local_update(brain_file, "UPDATE knowledge_items SET duplicate_of=NULL WHERE merged_into IS NULL;")

    duplicate_groups = local_query(brain_file, """
        SELECT content_hash AS content_hash, MIN(id) AS canonical_id, COUNT(*) AS duplicate_count
        FROM knowledge_items
        WHERE content_hash != ''
        GROUP BY content_hash
        HAVING COUNT(*) > 1;
    """)

    duplicate_count = 0
    if duplicate_groups == nil then
        return duplicate_count
    end

    for _, group in ipairs(duplicate_groups) do
        hash = knowledge_pool.row_value(group, "content_hash", 1, "")
        canonical_id = knowledge_pool.number_value(knowledge_pool.row_value(group, "canonical_id", 2, 0), 0)
        if hash != "" and canonical_id > 0 then
            local_update(brain_file, string.format(
                "UPDATE knowledge_items SET duplicate_of=%d, process_level='duplicate', promotion_status='duplicate' WHERE content_hash='%s' AND id != %d;",
                canonical_id,
                knowledge_pool.escape_sql(hash),
                canonical_id
            ))
            local_update(brain_file, string.format(
                "UPDATE knowledge_items SET promotion_status='pool' WHERE id=%d AND promotion_status='duplicate';",
                canonical_id
            ))
            duplicate_count = duplicate_count + 1
        end
    end

    return duplicate_count
end

function knowledge_pool.sync_notes(brain_file)
    knowledge_pool.ensure_table(brain_file)
    notes = local_query(brain_file, "SELECT time, subject, title, content FROM notes ORDER BY subject, title;")
    if notes == nil then
        return 0
    end

    count = 0
    for _, note in ipairs(notes) do
        note_time = knowledge_pool.row_value(note, "time", 1, os.date("%Y-%m-%d %H:%M:%S"))
        subject = knowledge_pool.row_value(note, "subject", 2, "")
        title = knowledge_pool.row_value(note, "title", 3, "")
        content = knowledge_pool.row_value(note, "content", 4, "")
        knowledge_pool.upsert_note(brain_file, subject, title, content, note_time)
        count = count + 1
    end

    knowledge_pool.mark_duplicates(brain_file)
    return count
end

function knowledge_pool.count_words(text)
    text = text or ""
    count = 0
    for _ in string.gmatch(text, "%S+") do
        count = count + 1
    end
    return count
end

function knowledge_pool.review_status_for_item(item)
    content = knowledge_pool.row_value(item, "content", 6, "")
    title = knowledge_pool.row_value(item, "title", 5, "")
    tier = knowledge_pool.number_value(knowledge_pool.row_value(item, "tier", 9, 1), 1)
    retrieval_count = knowledge_pool.number_value(knowledge_pool.row_value(item, "retrieval_count", 12, 0), 0)
    duplicate_of = knowledge_pool.row_value(item, "duplicate_of", 14, nil)
    artifact_status = knowledge_pool.row_value(item, "artifact_status", 19, "none")

    atomicity_status = "atomic"
    if knowledge_pool.count_words(content) > 180 then
        atomicity_status = "review-split"
    end

    title_status = "ok"
    if title == nil or title == "" then
        title_status = "missing"
    end

    duplication_status = "unique"
    if duplicate_of != nil and tostring(duplicate_of) != "" then
        duplication_status = "duplicate"
    end

    promotion_status = "pool"
    process_level = "working"
    if duplication_status == "duplicate" then
        promotion_status = "duplicate"
        process_level = "duplicate"
    elseif artifact_status == "stale" then
        promotion_status = "stale"
        process_level = "review"
    elseif tier >= 3 then
        promotion_status = "materialized"
        process_level = "atomic"
    elseif retrieval_count >= 2 and atomicity_status == "atomic" then
        promotion_status = "ready"
        process_level = "draft-ready"
    elseif retrieval_count > 0 then
        promotion_status = "review"
        process_level = "review"
    end

    return atomicity_status, duplication_status, title_status, promotion_status, process_level
end

function knowledge_pool.process_items(brain_file)
    synced = knowledge_pool.sync_notes(brain_file)
    duplicates = knowledge_pool.mark_duplicates(brain_file)

    items = local_query(brain_file, """
        SELECT id, source_type, source_id, source_ref, subject, title, content,
               content_hash, tier, process_level, heat, retrieval_count,
               last_retrieved_at, duplicate_of, merged_into, artifact_kind,
               artifact_ref, artifact_path, artifact_status, promotion_status
        FROM knowledge_items;
    """)

    reviewed = 0
    ready = 0
    if items != nil then
        for _, item in ipairs(items) do
            id = knowledge_pool.number_value(knowledge_pool.row_value(item, "id", 1, 0), 0)
            atomicity_status, duplication_status, title_status, promotion_status, process_level = knowledge_pool.review_status_for_item(item)
            if id > 0 then
                local_update(brain_file, string.format(
                    "UPDATE knowledge_items SET process_level='%s', promotion_status='%s', updated_at=datetime('now', 'localtime') WHERE id=%d;",
                    knowledge_pool.escape_sql(process_level),
                    knowledge_pool.escape_sql(promotion_status),
                    id
                ))
                knowledge_pool.record_review(
                    brain_file,
                    0,
                    id,
                    atomicity_status,
                    duplication_status,
                    title_status,
                    promotion_status,
                    "processing review"
                )
                if promotion_status == "ready" then
                    ready = ready + 1
                end
                reviewed = reviewed + 1
            end
        end
    end

    return {
        {synced = synced, duplicate_groups = duplicates, reviewed = reviewed, ready = ready}
    }
end

function knowledge_pool.escape_pattern(text)
    return string.gsub(text, "([%(%)%.%%%+%-%*%?%[%]%^%$])", "%%%1")
end

function knowledge_pool.count_matches(text, term)
    text = string.lower(text or "")
    term = string.lower(term or "")
    if term == "" then
        return 0
    end
    pattern = knowledge_pool.escape_pattern(term)
    count = 0
    for _ in string.gmatch(text, pattern) do
        count = count + 1
    end
    return count
end

function knowledge_pool.tier_weight(tier)
    tier = knowledge_pool.number_value(tier, 1)
    if tier >= 3 then
        return 1.75
    elseif tier == 2 then
        return 1.35
    elseif tier == 1 then
        return 1.0
    end
    return 0.7
end

function knowledge_pool.search_score(item, query_terms, query_text)
    title = knowledge_pool.row_value(item, "title", 5, "")
    subject = knowledge_pool.row_value(item, "subject", 4, "")
    content = knowledge_pool.row_value(item, "content", 6, "")
    tier = knowledge_pool.number_value(knowledge_pool.row_value(item, "tier", 7, 1), 1)
    heat = knowledge_pool.number_value(knowledge_pool.row_value(item, "heat", 9, 1.0), 1.0)
    retrieval_count = knowledge_pool.number_value(knowledge_pool.row_value(item, "retrieval_count", 10, 0), 0)
    duplicate_of = knowledge_pool.row_value(item, "duplicate_of", 12, nil)
    artifact_status = knowledge_pool.row_value(item, "artifact_status", 16, "none")

    score = 0
    searchable = title .. " " .. subject .. " " .. content
    for _, term in ipairs(query_terms) do
        score = score + (knowledge_pool.count_matches(title, term) * 4)
        score = score + (knowledge_pool.count_matches(subject, term) * 2)
        score = score + knowledge_pool.count_matches(content, term)
    end

    query_text = string.lower(query_text or "")
    if query_text != "" then
        if string.find(string.lower(title), knowledge_pool.escape_pattern(query_text)) != nil then
            score = score + 6
        elseif string.find(string.lower(searchable), knowledge_pool.escape_pattern(query_text)) != nil then
            score = score + 3
        end
    end

    if score <= 0 then
        return 0, knowledge_pool.tier_weight(tier)
    end

    tier_weight = knowledge_pool.tier_weight(tier)
    score = (score * tier_weight) + (heat * 0.2) + (retrieval_count * 0.05)

    if duplicate_of != nil and tostring(duplicate_of) != "" then
        score = score * 0.25
    end
    if artifact_status == "materialized" or artifact_status == "draft" then
        score = score + 0.5
    end

    return score, tier_weight
end

function knowledge_pool.query_terms(query_text)
    terms = {}
    query_text = string.lower(query_text or "")
    for term in string.gmatch(query_text, "%S+") do
        table.insert(terms, term)
    end
    return terms
end

function knowledge_pool.insert_retrieval(brain_file, query_text)
    local_update(brain_file, string.format(
        "INSERT INTO knowledge_retrievals (query_text) VALUES ('%s');",
        knowledge_pool.escape_sql(query_text)
    ))
    rows = local_query(brain_file, "SELECT MAX(id) AS retrieval_id FROM knowledge_retrievals;")
    if rows != nil and rows[1] != nil then
        return knowledge_pool.number_value(knowledge_pool.row_value(rows[1], "retrieval_id", 1, 0), 0)
    end
    return 0
end

function knowledge_pool.record_review(brain_file, retrieval_id, knowledge_id, atomicity_status, duplication_status, title_status, promotion_status, action_summary)
    retrieval_id = retrieval_id or 0
    knowledge_id = knowledge_id or 0
    local_update(brain_file, string.format("""
        INSERT INTO knowledge_reviews
            (retrieval_id, knowledge_id, atomicity_status, connectivity_status,
             duplication_status, title_status, promotion_status, action_summary)
        VALUES
            (%d, %d, '%s', 'linked', '%s', '%s', '%s', '%s');
    """,
        retrieval_id,
        knowledge_id,
        knowledge_pool.escape_sql(atomicity_status),
        knowledge_pool.escape_sql(duplication_status),
        knowledge_pool.escape_sql(title_status),
        knowledge_pool.escape_sql(promotion_status),
        knowledge_pool.escape_sql(action_summary)))
end

function knowledge_pool.reinforce_links(brain_file, ids)
    if ids == nil or #ids < 2 then
        return true
    end

    for i = 1, #ids do
        for j = i + 1, #ids do
            from_id = ids[i]
            to_id = ids[j]
            if from_id > to_id then
                tmp = from_id
                from_id = to_id
                to_id = tmp
            end
            existing = local_query(brain_file, string.format(
                "SELECT weight FROM knowledge_links WHERE from_id=%d AND to_id=%d AND link_type='co-retrieved';",
                from_id,
                to_id
            ))
            if existing != nil and existing[1] != nil then
                local_update(brain_file, string.format(
                    "UPDATE knowledge_links SET weight=weight+1.0, updated_at=datetime('now', 'localtime') WHERE from_id=%d AND to_id=%d AND link_type='co-retrieved';",
                    from_id,
                    to_id
                ))
            else
                local_update(brain_file, string.format(
                    "INSERT INTO knowledge_links (from_id, to_id, link_type, weight) VALUES (%d, %d, 'co-retrieved', 1.0);",
                    from_id,
                    to_id
                ))
            end
        end
    end
    return true
end

function knowledge_pool.record_retrieval(brain_file, query_text, results)
    retrieval_id = knowledge_pool.insert_retrieval(brain_file, query_text)
    ids = {}

    for rank, item in ipairs(results) do
        id = knowledge_pool.number_value(item.id, 0)
        if id > 0 then
            reinforcement_delta = 0.25 + (knowledge_pool.number_value(item.tier, 1) * 0.05)
            local_update(brain_file, string.format("""
                INSERT OR REPLACE INTO knowledge_retrieval_results
                    (retrieval_id, knowledge_id, rank, score, tier_weight, reinforcement_delta)
                VALUES (%d, %d, %d, %f, %f, %f);
            """,
                retrieval_id,
                id,
                rank,
                item.score,
                item.tier_weight,
                reinforcement_delta))

            local_update(brain_file, string.format(
                "UPDATE knowledge_items SET retrieval_count=retrieval_count+1, heat=heat+%f, last_retrieved_at=datetime('now', 'localtime') WHERE id=%d;",
                reinforcement_delta,
                id
            ))

            atomicity_status, duplication_status, title_status, promotion_status, process_level = knowledge_pool.review_status_for_item(item)
            knowledge_pool.record_review(
                brain_file,
                retrieval_id,
                id,
                atomicity_status,
                duplication_status,
                title_status,
                promotion_status,
                "post-retrieval review"
            )
            table.insert(ids, id)
        end
    end

    knowledge_pool.reinforce_links(brain_file, ids)
    return retrieval_id
end

function knowledge_pool.search(brain_file, query_text, limit)
    knowledge_pool.sync_notes(brain_file)
    limit = tonumber(limit) or 5
    query_terms = knowledge_pool.query_terms(query_text)
    if #query_terms == 0 then
        return {}, 0
    end

    rows = local_query(brain_file, """
        SELECT id, source_type, source_id, subject, title, content, tier,
               process_level, heat, retrieval_count, last_retrieved_at,
               duplicate_of, merged_into, artifact_ref, artifact_path,
               artifact_status, promotion_status, source_ref
        FROM knowledge_items
        WHERE merged_into IS NULL
        ORDER BY tier DESC, heat DESC, title;
    """)

    scored = {}
    if rows != nil then
        for _, row in ipairs(rows) do
            score, tier_weight = knowledge_pool.search_score(row, query_terms, query_text)
            if score > 0 then
                item = {
                    id = knowledge_pool.number_value(knowledge_pool.row_value(row, "id", 1, 0), 0),
                    title = knowledge_pool.row_value(row, "title", 5, ""),
                    subject = knowledge_pool.row_value(row, "subject", 4, ""),
                    tier = knowledge_pool.number_value(knowledge_pool.row_value(row, "tier", 7, 1), 1),
                    process_level = knowledge_pool.row_value(row, "process_level", 8, ""),
                    score = score,
                    tier_weight = tier_weight,
                    source_ref = knowledge_pool.row_value(row, "source_ref", 18, ""),
                    artifact_status = knowledge_pool.row_value(row, "artifact_status", 16, "none"),
                    artifact_ref = knowledge_pool.row_value(row, "artifact_ref", 14, ""),
                    content = knowledge_pool.row_value(row, "content", 6, ""),
                    duplicate_of = knowledge_pool.row_value(row, "duplicate_of", 12, nil),
                    retrieval_count = knowledge_pool.number_value(knowledge_pool.row_value(row, "retrieval_count", 10, 0), 0)
                }
                table.insert(scored, item)
            end
        end
    end

    table.sort(scored, function(a, b)
        if a.score == b.score then
            return a.id < b.id
        end
        return a.score > b.score
    end)

    results = {}
    for i, item in ipairs(scored) do
        if i > limit then
            break
        end
        item.score = math.floor(item.score * 100 + 0.5) / 100
        table.insert(results, item)
    end

    retrieval_id = 0
    if #results > 0 then
        retrieval_id = knowledge_pool.record_retrieval(brain_file, query_text, results)
    else
        retrieval_id = knowledge_pool.insert_retrieval(brain_file, query_text)
    end

    return results, retrieval_id
end

function knowledge_pool.browse(brain_file, limit)
    knowledge_pool.sync_notes(brain_file)
    limit = tonumber(limit) or 20
    return local_query(brain_file, string.format("""
        SELECT id, title, subject, tier, process_level, retrieval_count,
               heat, artifact_status, promotion_status, source_ref, artifact_ref
        FROM knowledge_items
        ORDER BY tier DESC, heat DESC, retrieval_count DESC, title
        LIMIT %d;
    """, limit)) or {}
end

function knowledge_pool.get_item(brain_file, id)
    knowledge_pool.sync_notes(brain_file)
    id = tonumber(id) or 0
    rows = local_query(brain_file, string.format("""
        SELECT id, title, subject, tier, process_level, retrieval_count,
               heat, source_type, source_ref, artifact_kind, artifact_ref,
               artifact_path, artifact_status, promotion_status, duplicate_of,
               merged_into, content
        FROM knowledge_items
        WHERE id=%d;
    """, id))
    if rows != nil and rows[1] != nil then
        return rows[1]
    end
    return nil
end

function knowledge_pool.history(brain_file, id)
    knowledge_pool.ensure_table(brain_file)
    if id != nil and tostring(id) != "" then
        return local_query(brain_file, string.format("""
            SELECT kr.id AS retrieval_id, kr.query_text, kr.created_at,
                   krr.rank, krr.score
            FROM knowledge_retrieval_results krr
            JOIN knowledge_retrievals kr ON kr.id = krr.retrieval_id
            WHERE krr.knowledge_id=%d
            ORDER BY kr.id DESC
            LIMIT 20;
        """, tonumber(id) or 0)) or {}
    end

    return local_query(brain_file, """
        SELECT id, query_text, created_at
        FROM knowledge_retrievals
        ORDER BY id DESC
        LIMIT 20;
    """) or {}
end

function knowledge_pool.queue(brain_file)
    knowledge_pool.process_items(brain_file)
    return local_query(brain_file, """
        SELECT id, title, tier, process_level, promotion_status,
               retrieval_count, heat, artifact_status, source_ref, duplicate_of
        FROM knowledge_items
        WHERE promotion_status IN ('ready', 'duplicate', 'stale')
           OR duplicate_of IS NOT NULL
        ORDER BY
            CASE promotion_status
                WHEN 'ready' THEN 0
                WHEN 'stale' THEN 1
                WHEN 'duplicate' THEN 2
                ELSE 3
            END,
            heat DESC,
            title;
    """) or {}
end

function knowledge_pool.promote(brain_file, id, target_tier, artifact_status)
    knowledge_pool.sync_notes(brain_file)
    id = tonumber(id) or 0
    if id <= 0 then
        return nil, "Must provide a knowledge item id"
    end

    item = knowledge_pool.get_item(brain_file, id)
    if item == nil then
        return nil, "Knowledge item not found: " .. tostring(id)
    end

    duplicate_of = knowledge_pool.row_value(item, "duplicate_of", 15, nil)
    if duplicate_of != nil and tostring(duplicate_of) != "" then
        return nil, "Refusing to promote duplicate item " .. tostring(id)
    end

    vault_path = config.get_vault_path()
    if vault_path == nil or vault_path == "" then
        return nil, "No vault configured; promote requires a vault-backed knowledge artifact"
    end

    current_tier = knowledge_pool.number_value(knowledge_pool.row_value(item, "tier", 4, 1), 1)
    target_tier = tonumber(target_tier) or current_tier
    if target_tier < 2 then
        target_tier = 2
    end
    if target_tier > 3 then
        target_tier = 3
    end

    artifact_status = artifact_status or "draft"
    if target_tier >= 3 and artifact_status == "draft" then
        artifact_status = "materialized"
    end

    title = knowledge_pool.row_value(item, "title", 2, "knowledge-" .. tostring(id))
    subject = knowledge_pool.row_value(item, "subject", 3, "")
    content = knowledge_pool.row_value(item, "content", 17, "")
    source_ref = knowledge_pool.row_value(item, "source_ref", 9, "")

    knowledge_dir = joinpath(vault_path, "knowledge")
    status, err = knowledge_pool.ensure_dir(knowledge_dir)
    if status == nil then
        return nil, "Failed to create knowledge directory: " .. tostring(err)
    end

    tier_dir_name = "tier" .. tostring(target_tier)
    tier_dir = joinpath(knowledge_dir, tier_dir_name)
    status, err = knowledge_pool.ensure_dir(tier_dir)
    if status == nil then
        return nil, "Failed to create tier directory: " .. tostring(err)
    end

    filename = knowledge_pool.slugify(title) .. "-" .. tostring(id) .. ".md"
    artifact_rel_path = joinpath("knowledge", tier_dir_name, filename)
    artifact_full_path = joinpath(vault_path, artifact_rel_path)

    file = io.open(artifact_full_path, "w")
    if file == nil then
        return nil, "Failed to write artifact: " .. artifact_full_path
    end

    process_level = "draft"
    if target_tier >= 3 then
        process_level = "atomic"
    end

    io.write(file, "# " .. title .. "\n\n")
    io.write(file, "Tier: " .. tostring(target_tier) .. "\n")
    io.write(file, "Process: " .. process_level .. "\n")
    io.write(file, "Source: " .. source_ref .. "\n")
    if subject != "" then
        io.write(file, "Subject: " .. subject .. "\n")
    end
    io.write(file, "Promoted: " .. os.date("%Y-%m-%d %H:%M:%S") .. "\n\n")
    io.write(file, content .. "\n")
    io.close(file)

    local_update(brain_file, string.format("""
        UPDATE knowledge_items
        SET tier=%d,
            process_level='%s',
            artifact_kind='file',
            artifact_ref='%s',
            artifact_path='%s',
            artifact_status='%s',
            promotion_status='%s',
            updated_at=datetime('now', 'localtime')
        WHERE id=%d;
    """,
        target_tier,
        knowledge_pool.escape_sql(process_level),
        knowledge_pool.escape_sql(artifact_rel_path),
        knowledge_pool.escape_sql(artifact_rel_path),
        knowledge_pool.escape_sql(artifact_status),
        knowledge_pool.escape_sql(artifact_status),
        id))

    knowledge_pool.record_review(
        brain_file,
        0,
        id,
        "atomic",
        "unique",
        "ok",
        artifact_status,
        "promoted to " .. artifact_rel_path
    )

    return artifact_rel_path
end

function knowledge_pool.get_hot_items(brain_file, limit)
    knowledge_pool.ensure_table(brain_file)
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

knowledge_pool.ensure_schema = knowledge_pool.ensure_table

return knowledge_pool
