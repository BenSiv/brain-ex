#!/usr/bin/env bats

setup() {
  rm -rf tmp_vault
  rm -f tmp_vault.db
  mkdir tmp_vault
  brex init --vault tmp_vault --editor micro
}

teardown() {
  rm -rf tmp_vault
  rm -f tmp_vault.db
}

@test "add a task and list it" {
  run brex task add --content "Deploy new patch" --subject "backend" --due_to "2025-08-15"
  [ "$status" -eq 0 ]

  run brex task list
  [[ "$output" =~ "Deploy new patch" ]]
}

@test "mark task as done and list should be empty" {
  brex task add --content "Deploy new patch" --subject "backend"
  TASK_ID=$(sqlite3 tmp_vault.db "select id from tasks limit 1;")

  run brex task done --id "$TASK_ID" --comment "Done"
  [ "$status" -eq 0 ]

  # Now check the open task list
  run brex task list
  [ "$status" -eq 0 ]
  [[ "$output" =~ "Empty task list" ]]   # ensures list is empty
}
