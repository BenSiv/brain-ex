sql_schema = {}
sql_schema.sql_init = """
PRAGMA foreign_keys = ON;

-- The shared knowledge pool: plain notes and tasks are both rows here.
-- A task is a row that also has a matching tasks row (see below) --
-- there is no kind/type column, since that would just be a second
-- source of truth that could drift from what tasks actually says.
-- `id` is populated via bx_utils.generate_id (same random-id
-- convention task ids have always used), not AUTOINCREMENT -- see
-- doc/unified-items-design.md.
CREATE TABLE IF NOT EXISTS notes (
    id INTEGER PRIMARY KEY,
    time TIMESTAMP DEFAULT (datetime('now', 'localtime')),
    subject TEXT,
    title TEXT,
    content TEXT,
    size INTEGER DEFAULT 0,
    UNIQUE (title, subject)
);

CREATE TABLE IF NOT EXISTS connections (
    source_title TEXT NOT NULL,
    source_subject TEXT,
    target_title TEXT NOT NULL,
    target_subject TEXT,
    PRIMARY KEY (source_title, source_subject, target_title, target_subject)
);

-- Operative/actionable fields only -- no content, no subject, no
-- title. Those live solely on notes; tasks is joined to it by
-- item_id. Promoting an existing note into a task, or demoting a task
-- back into a plain note, is just inserting/deleting a row here.
CREATE TABLE IF NOT EXISTS tasks (
    item_id INTEGER PRIMARY KEY REFERENCES notes(id),
    due_to TIMESTAMP,
    overdue INTEGER,
    done TIMESTAMP DEFAULT NULL,
    owner TEXT DEFAULT NULL,
    importance INTEGER DEFAULT 1,
    urgency INTEGER DEFAULT 1
);

CREATE TABLE IF NOT EXISTS agent_sessions (
    id TEXT PRIMARY KEY,
    name TEXT,
    created_at TEXT DEFAULT (datetime('now', 'localtime')),
    updated_at TEXT DEFAULT (datetime('now', 'localtime'))
);

CREATE TABLE IF NOT EXISTS agent_messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id TEXT NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    metadata TEXT,
    in_context INTEGER DEFAULT 1,
    created_at TEXT DEFAULT (datetime('now', 'localtime')),
    FOREIGN KEY(session_id) REFERENCES agent_sessions(id)
);
"""

return sql_schema
