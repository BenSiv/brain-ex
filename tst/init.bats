#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"
BREX="brex"

resolve_brex() {
    if [ -x "./bin/brex" ]; then
        BREX="./bin/brex"
    else
        BREX="brex"
    fi
}

setup() {
    resolve_brex
    rm -rf tmp_vault
    rm -f brain.db tmp_vault.db my_brain.db tmp_vault_repro.db
    mkdir -p tmp_vault
}

teardown() {
    rm -rf tmp_vault
    rm -f brain.db tmp_vault.db my_brain.db tmp_vault_repro.db
    rm -f "$CONFIG"
}

@test "default init creates brain.db and config file" {
    run $BREX init
    [ "$status" -eq 0 ]
    [ -f "brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "^brain: .*brain.db$" "$CONFIG"
    grep -q "editor: nano" "$CONFIG"
}

@test "init with vault and editor creates vault db and sets config" {
    run $BREX init --vault tmp_vault --editor micro
    [ "$status" -eq 0 ]
    [ -f "tmp_vault.db" ]
    [ -f "$CONFIG" ]

    grep -q "^vault: .*tmp_vault$" "$CONFIG"
    grep -q "^brain: .*tmp_vault.db$" "$CONFIG"
    grep -q "^  tmp_vault: .*tmp_vault.db$" "$CONFIG"
    grep -q "editor: micro" "$CONFIG"
}

@test "init with custom name creates specified db and config" {
    run $BREX init --name "my_brain"
    [ "$status" -eq 0 ]
    [ -f "my_brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "^  my_brain: .*my_brain.db$" "$CONFIG"
    grep -q "editor: nano" "$CONFIG"
}

@test "init with vault and custom name updates config correctly" {
    run $BREX init --vault tmp_vault --name "my_brain" --editor micro
    [ "$status" -eq 0 ]
    [ -f "my_brain.db" ]
    [ -f "$CONFIG" ]

    grep -q "^vault: .*tmp_vault$" "$CONFIG"
    grep -q "^  my_brain: .*my_brain.db$" "$CONFIG"
    grep -q "editor: micro" "$CONFIG"
}

@test "init with git enabled" {
    run $BREX init --vault tmp_vault --git
    [ "$status" -eq 0 ]

    # A git repo should be created
    [ -d "tmp_vault/.git" ]

    # Config should record both git and auto_backup flags
    [ -f "$CONFIG" ]
    grep -q "git: true" "$CONFIG"
}
@test "init with vault containing root file" {
    echo "Root file content" > tmp_vault/root_file.md
    run $BREX init --vault tmp_vault --name tmp_vault_repro
    [ "$status" -eq 0 ]
}
