#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    mkdir -p "$HOME"
    export PATH="$PWD/bin:$PATH"
    rm -rf tmp_vault_md
    rm -f tmp_vault_md.db
}

teardown() {
    rm -rf tmp_vault_md
    rm -f tmp_vault_md.db
    cleanup_test_env
}

@test "backup markdown export creates individual task markdown files with frontmatter" {
    # setup vault and brain
    run brex init --vault tmp_vault_md
    [ "$status" -eq 0 ]
    
    # add task
    run brex task -d add -s "Test Task" -c "Content"
    [ "$status" -eq 0 ]
    
    # Check that individual task markdown file is created in tmp_vault_md/tasks/Test Task/
    # Let's find the md file
    md_files=(tmp_vault_md/tasks/Test\ Task/*.md)
    [ -f "${md_files[0]}" ]
    
    # Read the markdown file content
    content=$(cat "${md_files[0]}")
    echo "Content: $content"
    
    # Assert contains frontmatter and body, but no subject field (inferred from dir structure)
    [[ "$content" != *"subject:"* ]]
    [[ "$content" == *"importance: 1"* ]]
    [[ "$content" == *"urgency: 1"* ]]
    [[ "$content" == *"Content"* ]]
}
