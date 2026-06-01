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

## Section 3 — Advanced Task Management & Eisenhower Prioritization

Manage tasks across brains, delegate them to the AI agent, and leverage our new **Eisenhower prioritization system**! Task creation now supports optional deadlines, importance ratings (1-5), and manual urgency ratings (1-5). 

Tasks are dynamically grouped into priority quadrants with visual badges and sorted by priority score ($Importance \times Active\_Urgency$), with overdue tasks getting an automatic active urgency boost to 5.

```bash
# Add an urgent, high-importance task with a hard deadline
# (Deadlines within 24 hours or overdue are automatically boosted to Active Urgency 5)
brex task add --content "Urgent security patch" --importance 5 --urgency 4 --due_to "2026-06-02"

# Add a strategic task with high importance but no hard deadline (defaults to NULL/listed last)
brex task add --content "Design architecture plan" --importance 4 --urgency 2

# Add a tactical task with low importance but high manual urgency
brex task add --content "Reply to team emails" --importance 2 --urgency 4

# Delegate a backlog task to the agent
brex task add --content "Organize digital photos" --importance 1 --urgency 1 --owner "agent"
```

Listing tasks dynamically orders them by priority score and highlights their priority quadrants:
* `🔥 Q1 (Critical)`: High Importance ($\ge 4$), High Urgency ($\ge 4$)
* `⭐ Q2 (Strategic)`: High Importance ($\ge 4$), Low Urgency ($< 4$)
* `⚡ Q3 (Tactical)`: Low Importance ($< 4$), High Urgency ($\ge 4$)
* `💤 Q4 (Backlog)`: Low Importance ($< 4$), Low Urgency ($< 4$)

```bash
# List all pending tasks ordered by priority score
brex task list
```

You can review your accomplishments as well.

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
