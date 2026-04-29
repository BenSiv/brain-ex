#!/usr/bin/env bats

@test "agent command defaults to view" {
    ./bin/brex mybrain agent
}

@test "agent view works" {
    ./bin/brex mybrain agent view
}

@test "agent help flag works" {
    ./bin/brex mybrain agent -h
}

@test "agent run works" {
    ./bin/brex mybrain agent run "hello"
}
