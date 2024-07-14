PRAGMA foreign_keys = ON;

CREATE TABLE notes (
    "time" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "group" TEXT,
    "name" TEXT,
    "content" TEXT,
    PRIMARY KEY ("name", "group")
);

CREATE TABLE connections (
    "source" TEXT,
    "target" TEXT,
    PRIMARY KEY ("source", "target")
);

CREATE TABLE todos (
    "id" INTEGER PRIMARY KEY,
    "time" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "task" TEXT,
    "due_to" TIMESTAMP,
    "overdue" INTEGER,
    "done" INTEGER
);
