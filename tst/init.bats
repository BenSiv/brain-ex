#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"

setup() {
    rm -rf tmp_vault
    rm -f brain.db tmp_vault.db my_brain.db
    mkdir -p tmp_vault
}

teardown() {
    rm -rf tmp_vault
    rm -f brain.db tmp_vault.db my_brain.db
    rm -f "$CONFIG"
}

@test "default init creates brain.db and config file" {
    run brex init
    [ "$status" -eq 0 ]
    [ -f "brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "brain: .*brain.db" "$CONFIG"
    grep -q "editor: nano" "$CONFIG"
}

@test "init with vault and editor creates vault db and sets config" {
    run brex init --vault tmp_vault --editor micro
    [ "$status" -eq 0 ]
    [ -f "tmp_vault.db" ]
    [ -f "$CONFIG" ]

    grep -q "vault: .*tmp_vault" "$CONFIG"
    grep -q "brain: .*tmp_vault.db" "$CONFIG"
    grep -q "editor: micro" "$CONFIG"
}

@test "init with custom name creates specified db and config" {
    run brex init --name "my_brain"
    [ "$status" -eq 0 ]
    [ -f "my_brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "brain: .*my_brain.db" "$CONFIG"
    grep -q "editor: nano" "$CONFIG"
}

@test "init with vault and custom name updates config correctly" {
    run brex init --vault tmp_vault --name "my_brain" --editor micro
    [ "$status" -eq 0 ]
    [ -f "my_brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "vault: .*tmp_vault" "$CONFIG"
    grep -q "brain: .*my_brain.db" "$CONFIG"
    grep -q "editor: micro" "$CONFIG"
}

@test "init with git and auto-backup enabled" {
    run brex init --vault tmp_vault --git --auto-backup
    [ "$status" -eq 0 ]

    # A git repo should be created
    [ -d "tmp_vault/.git" ]

    # Config should record both git and auto_backup flags
    [ -f "$CONFIG" ]
    grep -q "auto_backup: true" "$CONFIG"
    grep -q "git: true" "$CONFIG"

    # Verify that the initial commit exists
    pushd tmp_vault >/dev/null
    git log --oneline | grep -q "Initial commit"
    popd >/dev/null
}
