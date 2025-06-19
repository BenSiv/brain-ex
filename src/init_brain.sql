PRAGMA foreign_keys = ON;

CREATE TABLE notes (
    "time" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "subject" TEXT,
    "name" TEXT,
    "content" TEXT,
    PRIMARY KEY ("name", "subject")
);

CREATE TABLE connections (
    "source" TEXT,
    "target" TEXT,
    PRIMARY KEY ("source", "target")
);

CREATE TABLE tasks (
    "id" INTEGER PRIMARY KEY,
    "time" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "content" TEXT,
    "subject" TEXT,
    "due_to" TIMESTAMP,
    "overdue" INTEGER,
    "done" TIMESTAMP DEFAULT NULL,
    "comment" TEXT DEFAULT NULL
);
