#!/usr/bin/env bats

@test "agent command exists" {
    ./bin/brex mybrain agent --help
}

@test "agent run works" {
    ./bin/brex mybrain agent run "hello"
}
