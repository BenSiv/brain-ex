sql_schema = {}
sql_schema.sql_init = """
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS notes (
    time TIMESTAMP DEFAULT (datetime('now', 'localtime')),
    subject TEXT,
    title TEXT,
    content TEXT,
    size INTEGER DEFAULT 0,
    PRIMARY KEY (title, subject)
);

CREATE TABLE IF NOT EXISTS connections (
    source_title TEXT NOT NULL,
    source_subject TEXT,
    target_title TEXT NOT NULL,
    target_subject TEXT,
    PRIMARY KEY (source_title, source_subject, target_title, target_subject)
);

CREATE TABLE IF NOT EXISTS tasks (
    id INTEGER PRIMARY KEY,
    time TIMESTAMP DEFAULT (datetime('now', 'localtime')),
    content TEXT,
    subject TEXT,
    due_to TIMESTAMP,
    overdue INTEGER,
    done TIMESTAMP DEFAULT NULL,
    comment TEXT DEFAULT NULL,
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
