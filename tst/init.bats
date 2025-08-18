#!/usr/bin/env bats

setup() {
  rm -rf tmp_vault
  rm -f brain.db tmp_vault.db
}

teardown() {
  rm -rf tmp_vault
  rm -f brain.db tmp_vault.db
}

@test "default init creates brain.db" {
  run brex init
  [ "$status" -eq 0 ]
  [ -f "brain.db" ]
}

@test "init with vault and editor creates vault db" {
  mkdir tmp_vault
  run brex init --vault tmp_vault --editor micro
  [ "$status" -eq 0 ]
  [ -f "tmp_vault.db" ]
}
