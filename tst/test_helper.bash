setup_test_env() {
    export PROJECT_ROOT="$(cd "$BATS_TEST_DIRNAME/.." && pwd)"
    export HOME="$PROJECT_ROOT/.tmp-home"
    export XDG_CONFIG_HOME="$HOME/.config"
    export CONFIG="$XDG_CONFIG_HOME/brain-ex/config.yaml"
    export PATH="$PROJECT_ROOT/bin:$PATH"
    mkdir -p "$XDG_CONFIG_HOME/brain-ex"
}

cleanup_test_env() {
    rm -rf "$PROJECT_ROOT/.tmp-home"
}
