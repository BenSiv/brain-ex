# brain-ex Demo

## Opening

Hi everyone, today I’m going to show you **brain-ex**, a minimalist, fast, and intuitive note-taking and task management app, designed for people who live in the terminal.  
You can think of it as a brain extension that works as a stand-alone CLI tool, or as an Obsidian vault companion, so you can capture notes and manage tasks without leaving your workflow.

---

## Intro Concept

The philosophy is simple:  
1. Don’t force a UI switch when you’re deep in coding.  
2. Make capturing ideas and tasks as frictionless as possible.  
3. Keep the data in plain text or SQLite, so you own it.

---

## Section 1 — Initialization

```bash
brex init
```

To start, I’ll run `brex init`.  
This creates a local SQLite database for notes and tasks.  
I can also connect it to an **Obsidian vault** so my CLI notes sync directly with my markdown files.

```bash
brex init --vault ~/Documents/my_vault --editor micro
```

Here I’m pointing it to my vault and telling it to open notes in micro by default.

---

## Section 2 — Notes

Let’s add today’s note. This is perfect for journaling your work progress or writing quick reminders.

```bash
brex note --content "Refactored authentication flow"
```

This automatically tags it with today’s date and the default subject `daily`.

```bash
brex note add --title "API rate limit fix" \
--content "Patched endpoint to throttle requests" \
--subject "backend" \
--links "rate-limit,bugfix"
```

Notes can have titles, subjects, and links to other notes — creating a web of related ideas.

---

## Section 3 — Editing and Browsing Notes

```bash
brex note edit --title "API rate limit fix"
```

If I want to expand a note, I just edit it in my favorite editor.

```bash
brex note last --subject backend --number 3
```

Here are the last three backend notes — super fast to retrieve.

---

## Section 4 — Tasks

```bash
brex task add --content "Deploy new rate limiting patch" \
--subject "backend" \
--due_to "2025-08-15"
```

Tasks can have subjects and due dates. I can also list all open tasks…

```bash
brex task list
```

…mark them done…

```bash
brex task done --id 1 --comment "Deployed successfully"
```

…or delay them in bulk if needed.

---

## Section 5 — Updating from Vault

```bash
brex update
```

If I’ve made changes in Obsidian, `brex update` will rebuild the database from the vault.  
I can also update a single note file without touching the rest.

---

## Section 6 — SQL Mode

```bash
brex sql --query "SELECT title, subject FROM notes"
```

Since the database is SQLite, I can query it directly — perfect for quick data analysis.

---

## Closing

That’s **brain-ex**, your notes and tasks, right where you live as a programmer:  
in the terminal, minimal, and always ready.
Whether you use Obsidian or not, brain-ex is built to fit into your flow without stealing your focus.
