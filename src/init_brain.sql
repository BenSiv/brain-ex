PRAGMA foreign_keys = ON;

CREATE TABLE notes (
    id INTEGER PRIMARY KEY,
    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    title TEXT,
    content TEXT
);

CREATE TABLE connections (
    id INTEGER PRIMARY KEY,
    source INTEGER,
    target INTEGER,
    FOREIGN KEY (source) REFERENCES notes(id),
    FOREIGN KEY (target) REFERENCES notes(id)
);

CREATE TABLE todos (
    id INTEGER PRIMARY KEY,
    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    task TEXT,
    due_to TIMESTAMP,
    done INTEGER
);