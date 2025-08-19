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

@test "add a daily note" {
    run brex note --content "Refactored authentication flow"
    [ "$status" -eq 0 ]
    run brex note last
    [[ "$output" =~ "Refactored authentication flow" ]]
}

@test "add a note with subject + links" {
    run brex note add --title "api-fix" --content "Patched endpoint" --subject "backend" --links "bugfix"
    [ "$status" -eq 0 ]
    [ -f "tmp_vault/backend/api-fix.md" ]
    grep -q "Patched endpoint" tmp_vault/backend/api-fix.md
}
