#!/bin/bash

# Brain-Ex CLI Showcase Demo
# Uses demo-magic: https://github.com/paxtonhare/demo-magic

source demo-magic.sh

DEMO_PROMPT="${GREEN}Brain-Ex Demo${COLOR_RESET} | ${BLUE}\w${COLOR_RESET} > "
TYPE_SPEED=20

function comment() {
  cmd=$DEMO_COMMENT_COLOR$1$COLOR_RESET
  echo -en "$cmd"; echo ""
}

clear

echo "Welcome to Brain-Ex — Minimalist, fast, and intuitive note-taking and task management for the terminal."
sleep 1

comment "# --- Initialization ---\n"
comment "# Default initialization"
pe 'brex init'
comment "# Brain file initilized"
pe 'ls brain.db'
rm brain.db
mkdir my_vault
comment "# Initialize with Obsidian vault and micro editor"
pe 'brex init --vault my_vault --editor micro'
comment "# Brain file initilized"
pe 'ls my_vault.db'

comment "# Show configurations"
pe 'cat ~/.config/brain-ex/config.yaml ; echo'

pe "clear"

comment "# --- Notes ---\n"

comment "# Add today's note"
pe 'brex note --content "Refactored authentication flow"'
comment "# Show today's note"
pe 'brex note last'
pe 'ls my_vault'
pe 'ls my_vault/daily'

comment "# Add a note with title, subject, and links"
pe 'brex note add --title "api-fix" --content "Patched endpoint to throttle requests" --subject "backend" --links "rate-limit,bugfix"'
pe 'ls -1 my_vault'
pe 'ls -1 my_vault/backend'
pe 'cat "my_vault/backend/api-fix.md"'

comment "# Edit a note"
pe 'brex note edit --title "api-fix" --subject "backend"'

pe "clear"

comment "# --- Tasks ---\n"
comment "# Add a new task"
pe 'brex task add --content "Deploy new rate limiting patch" --subject "backend" --due_to "2025-08-15"'

comment "# List all open tasks"
pe 'brex task list'

comment "# Delay all tasks by 24 hours"
pe 'brex task delay --id "*"'

comment "# List all open tasks"
pe 'brex task list'

TASK_ID=$(sqlite3 my_vault.db "select id from tasks limit 1;")
comment "# Mark task as done"
pe 'brex task done --id $TASK_ID --comment "Deployed successfully"'

comment "# List all open tasks"
pe 'brex task list'

pe "clear"

comment "# --- Vault Updates ---\n"
comment "# Update database from vault"
pe 'brex update'

comment "# Update from a specific note file"
pe 'brex update --file "my_vault/backend/api-fix.md"'

pe "clear"

comment "# --- SQL Mode ---\n"
comment "# Query notes directly with SQL"
pe 'brex sql --query "SELECT title, subject FROM notes;"'

pe "clear"

rm -r my_vault
rm my_vault.db

comment "# That’s Brain-Ex — minimal, fast, and built for your terminal workflow."
