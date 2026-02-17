#!/usr/bin/env bash
set -euo pipefail

VERBOSE=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -h|--help)
            cat <<'EOF'
Usage: ./bld/build.sh [options]

Options:
  -v, --verbose   Print full build command output
  -h, --help      Show this help
EOF
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Use --help for usage." >&2
            exit 1
            ;;
    esac
done

run_cmd() {
    if [[ "$VERBOSE" -eq 1 ]]; then
        "$@"
    else
        "$@" >>"$BUILD_LOG" 2>&1
    fi
}

on_error() {
    if [[ "$VERBOSE" -eq 0 ]]; then
        echo "Build failed. Re-run with --verbose for full output." >&2
        echo "Last build log lines:" >&2
        tail -n 40 "$BUILD_LOG" >&2 || true
    fi
}

# Create temp dir
TMPDIR=$(mktemp -d)
BUILD_LOG=$(mktemp)
trap on_error ERR
trap 'rm -rf "$TMPDIR" "$BUILD_LOG"' EXIT

# Determine absolute paths
LUAM_DIR=$(cd ../luam && pwd)
LUAM_BIN="$LUAM_DIR/bin/luam"
STATIC_TOOL="$LUAM_DIR/lib/static/init.lua"
LUAM_LIB="$LUAM_DIR/obj/liblua.a"

echo "Preparing build"
# Copy brain-ex files
run_cmd cp src/brex.lua "$TMPDIR"/
run_cmd cp src/*.lua "$TMPDIR"/

# Copy luam standard libraries (replaces bundled lua-utils)
run_cmd cp "$LUAM_DIR/lib/"*.lua "$TMPDIR"/
# Copy relocated modules
run_cmd cp "$LUAM_DIR/lib/socket/init.lua" "$TMPDIR/socket.lua"
run_cmd cp "$LUAM_DIR/lib/mime/init.lua" "$TMPDIR/mime.lua"
run_cmd cp "$LUAM_DIR/lib/ltn12/init.lua" "$TMPDIR/ltn12.lua"
run_cmd cp "$LUAM_DIR/lib/ssl/init.lua" "$TMPDIR/ssl.lua"

# Remove static.lua (tool) to prevent it from being compiled into the binary source list inadvertently
run_cmd rm -f "$TMPDIR"/static.lua

# Copy dkjson alias (renamed from json)
run_cmd cp "$LUAM_DIR/lib/dkjson/init.lua" "$TMPDIR/dkjson.lua"

# Build
pushd "$TMPDIR" >/dev/null

# Construct file list
# brex.lua must be first (main entry point)
# Exclude brex.lua from wildcard to avoid dupe (though shell expansion handles non-overlapping well, here we manually order)
FILES="brex.lua $(ls *.lua | grep -v '^brex.lua$')"

echo "Generating C source"
run_cmd env CC="" "$LUAM_BIN" "$STATIC_TOOL" \
    $FILES \
    "$LUAM_LIB" \
    -I "$LUAM_DIR/src" \
    -lm -ldl -lreadline -lpthread

# Inject lsqlite3, lfs, and yaml preload
run_cmd sed -i '/luaL_openlibs(L);/a \
  int luaopen_sqlite3(lua_State *L); \
  int luaopen_lfs(lua_State *L); \
  int luaopen_yaml(lua_State *L); \
  lua_getglobal(L, "package"); \
  lua_getfield(L, -1, "preload"); \
  lua_pushcfunction(L, luaopen_sqlite3); \
  lua_setfield(L, -2, "sqlite3"); \
  lua_pushcfunction(L, luaopen_lfs); \
  lua_setfield(L, -2, "lfs"); \
  lua_pushcfunction(L, luaopen_yaml); \
  lua_setfield(L, -2, "yaml"); \
  lua_pop(L, 2);' brex.static.c

# Compile lsqlite3
run_cmd cc -c -O2 -I"$LUAM_DIR/src" "$LUAM_DIR/lib/sqlite/lsqlite3.c" -o lsqlite3.o

# Compile lfs
run_cmd cc -c -O2 -I"$LUAM_DIR/src" "$LUAM_DIR/lib/lfs/src/lfs.c" -o lfs.o

# Compile yaml (all source files)
YAML_SRC="$LUAM_DIR/lib/yaml"
run_cmd cc -c -O2 -I"$LUAM_DIR/src" -I"$YAML_SRC" \
    "$YAML_SRC/lyaml.c" "$YAML_SRC/api.c" "$YAML_SRC/b64.c" \
    "$YAML_SRC/dumper.c" "$YAML_SRC/emitter.c" "$YAML_SRC/loader.c" \
    "$YAML_SRC/parser.c" "$YAML_SRC/reader.c" "$YAML_SRC/scanner.c" "$YAML_SRC/writer.c"

# Compile binary
run_cmd cc -Os brex.static.c lsqlite3.o lfs.o lyaml.o api.o b64.o dumper.o emitter.o loader.o parser.o reader.o scanner.o writer.o "$LUAM_LIB" \
    -I "$LUAM_DIR/src" \
    -lm -ldl -lreadline -lpthread -lsqlite3 \
    -Wl,--export-dynamic \
    -o brex

popd >/dev/null

# bin/ holds only final binaries
run_cmd mkdir -p bin
run_cmd mv "$TMPDIR"/brex bin/
echo "Build complete. Binary in bin/brex"
if [[ "$VERBOSE" -eq 1 ]]; then
    ls -lh bin/brex
fi
