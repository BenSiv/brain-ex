#!/bin/bash

# Brain-Ex CLI Showcase Demo
# Uses demo-magic: https://github.com/paxtonhare/demo-magic

DEMO_MAGIC_PATH="${DEMO_MAGIC_PATH:-~/demo-magic/demo-magic.sh}"

# Parse arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -m|--demo-magic) DEMO_MAGIC_PATH="$2"; shift ;;
        -h|--help) 
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  -m, --demo-magic PATH  Path to demo-magic.sh (default: $DEMO_MAGIC_PATH)"
            echo "  -h, --help             Show this help"
            exit 0
            ;;
    esac
    shift
done

# Try to source demo-magic, fallback to simple echo if not found
if [ -f "$DEMO_MAGIC_PATH" ]; then
  source "$DEMO_MAGIC_PATH"
else
  echo "Warning: demo-magic.sh not found at $DEMO_MAGIC_PATH. Falling back to simple execution."
  function pe() { echo "> $1"; eval "$1"; }
  function comment() { echo -e "\n$1"; }
  alias p='read -p "..."';
  GREEN=""
  BLUE=""
  COLOR_RESET=""
fi

DEMO_PROMPT="${GREEN}Brain-Ex Demo${COLOR_RESET} | ${BLUE}\w${COLOR_RESET} > "
TYPE_SPEED=20

function comment() {
  cmd=$DEMO_COMMENT_COLOR$1$COLOR_RESET
  echo -en "$cmd"; echo ""
}

clear

echo "Welcome to Brain-Ex — Minimalist, fast, and intuitive note-taking and task management for the terminal."
sleep 1

# Cleanup previous demo runs
rm -rf work_vault personal_vault work.db personal.db brain.db ~/.config/brain-ex/config.yaml

comment "# --- Multi-Brain Initialization ---\n"
comment "# Initialize a 'work' brain"
mkdir -p work_vault/daily
pe 'brex init --vault work_vault --name work --editor nano'

comment "# Initialize a 'personal' brain"
mkdir -p personal_vault/daily
pe 'brex init --vault personal_vault --name personal --editor nano'

comment "# List all configured brains"
pe 'brex brain list'

comment "# Switch to personal brain"
pe 'brex brain use --name personal'

comment "# List all configured brains"
pe 'brex brain list'

pe "clear"

comment "# --- Enhanced Note Taking ---\n"

comment "# Quick log (timestamped title)"
pe 'brex note --content "Bought groceries for the week"'

comment "# Add a note with links"
pe 'brex note add --title "Travel Plans" --content "Thinking about Japan in Autumn" --subject "travel" --links "bucket-list,vacation"'

comment "# Connect the last log note to another note"
pe 'brex note connect --links "finances"'

comment "# --- Advanced Task Management ---\n"
comment "# Add a task for the 'work' brain without switching"
pe 'brex work task add --content "Finalize Q2 report" --subject "reporting" --due_to "2026-05-15"'

comment "# Show last notes in personal brain"
pe 'brex note last --number 3'

pe "clear"

comment "# Add a task to personal brain and delegate to agent"
pe 'brex task add --content "Organize digital photos" --owner "agent" --due_to "2026-06-01"'

comment "# List tasks owned by the agent"
pe 'brex task list --owner "agent"'

comment "# List all pending tasks"
pe 'brex task list'

# Get an ID for marking done
TASK_ID=$(sqlite3 personal.db "SELECT id FROM tasks LIMIT 1;")
comment "# Mark a task as done"
pe "brex task done --id $TASK_ID --comment 'Done!'"

comment "# Show recently completed tasks"
pe 'brex task last --number 3'

pe "clear"

comment "# --- Knowledge Pool & RAG ---\n"
comment "# Sync notes into the knowledge pool"
pe 'brex knowledge sync'

comment "# Search the knowledge pool"
pe 'brex knowledge search "Japan"'

comment "# Browse the knowledge base"
pe 'brex knowledge browse'

comment "# Process knowledge (check for duplicates/readiness)"
pe 'brex knowledge process'

pe "clear"

comment "# --- Agent Interaction ---\n"
comment "# Set mock agent for demo"
echo "agent_provider: mock" >> ~/.config/brain-ex/config.yaml

comment "# Ask the general assistant"
pe 'brex agent "What are my upcoming travel plans?"'

comment "# Use the task-focused agent"
pe 'brex agent task "Remind me to call the bank tomorrow"'

pe "clear"

comment "# That’s Brain-Ex — power and simplicity in your terminal."
comment "# All your data is stored in plain markdown and SQLite."

# Final Cleanup
rm -rf work_vault personal_vault work.db personal.db brain.db
