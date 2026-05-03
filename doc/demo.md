# Brain-Ex Demo: Feature Coverage

## Opening

Hi everyone, today I’m going to show you **brain-ex**, a minimalist, fast, and intuitive note-taking and task management app.  
It's designed to be your terminal's brain extension, supporting multiple contexts, a deep knowledge pool, and integrated AI assistance.

---

## Section 1 — Multi-Brain Setup

In Brain-Ex, you can manage separate "brains" for different parts of your life.

```bash
# Setup a work brain
brex init --vault work_vault --name work

# Setup a personal brain
brex init --vault personal_vault --name personal
```

You can list all your brains and switch between them easily.

```bash
brex brain list
brex brain use --name personal
```

---

## Section 2 — Enhanced Note Taking

Beyond simple notes, Brain-Ex supports quick logging and relationships.

```bash
# Quick log (automatically timestamped)
brex note --content "Bought groceries for the week"

# Note with subject and links
brex note add --title "Travel Plans" --content "Thinking about Japan in Autumn" --subject "travel" --links "bucket-list,vacation"
```

You can also connect existing notes to build your second brain.

```bash
brex note connect --links "finances"
```

---

## Section 3 — Advanced Task Management

Manage tasks across brains and even delegate them to the AI agent.

```bash
# Add a task to the 'work' brain without switching context
brex work task add --content "Finalize Q2 report" --subject "reporting" --due_to "2026-05-15"

# Delegate a task to the agent
brex task add --content "Organize digital photos" --owner "agent"
```

Listing tasks supports filtering by owner or subject.

```bash
brex task list --owner "agent"
```

And you can review your accomplishments.

```bash
brex task last --number 3
```

---

## Section 4 — Knowledge Pool & RAG

Brain-Ex includes a "Knowledge Pool" system that indexes your notes for semantic search and refinement.

```bash
# Sync your notes into the searchable pool
brex knowledge sync

# Perform a semantic search
brex knowledge search "Japan"

# Browse and process knowledge items
brex knowledge browse
brex knowledge process
```

---

## Section 5 — AI Agent Interaction

The integrated agent can help you query your knowledge and manage your tasks.

```bash
# Ask the general assistant
brex agent "What are my upcoming travel plans?"

# Use a specialized task agent
brex agent task "Remind me to call the bank tomorrow"
```

---

## Closing

That’s **brain-ex**. It keeps your data in plain Markdown and SQLite, giving you both the speed of the terminal and the power of a modern knowledge management system.
Minimal, fast, and built for your flow.
