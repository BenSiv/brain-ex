#!/usr/bin/env bash
set -euo pipefail

# Determine absolute paths
ROOT=$(pwd)
LUAM_DIR=$(cd ../luam && pwd)
LUAM_BIN="$LUAM_DIR/bld/luam"
STATIC_TOOL="$LUAM_DIR/lib/static/init.lua"
LUAM_LIB="$LUAM_DIR/obj/liblua.a"
LUA_INIT="$LUAM_DIR/src/lualib.lua"

# Create temp dir
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

echo "Preparing source files..."
# Copy brain-ex files
cp brex.lua "$TMPDIR"/
cp src/*.lua "$TMPDIR"/

# Copy luam standard libraries (replaces bundled lua-utils)
cp "$LUAM_DIR/lib/"*.lua "$TMPDIR"/
# Copy relocated modules
cp "$LUAM_DIR/lib/socket/init.lua" "$TMPDIR/socket.lua"
cp "$LUAM_DIR/lib/mime/init.lua" "$TMPDIR/mime.lua"
cp "$LUAM_DIR/lib/ltn12/init.lua" "$TMPDIR/ltn12.lua"
cp "$LUAM_DIR/lib/ssl/init.lua" "$TMPDIR/ssl.lua"

# Remove static.lua (tool) to prevent it from being compiled into the binary source list inadvertently
rm "$TMPDIR"/static.lua 2>/dev/null || true

# Copy dkjson alias (renamed from json)
cp "$LUAM_DIR/lib/dkjson/init.lua" "$TMPDIR/dkjson.lua"

# Build
pushd "$TMPDIR" > /dev/null

# Construct file list
# brex.lua must be first (main entry point)
# Exclude brex.lua from wildcard to avoid dupe (though shell expansion handles non-overlapping well, here we manually order)
FILES="brex.lua $(ls *.lua | grep -v '^brex.lua$')"

echo "Generating C source..."
CC="" "$LUAM_BIN" "$STATIC_TOOL" \
    $FILES \
    "$LUAM_LIB" \
    -I "$LUAM_DIR/src" \
    -lm -ldl -lreadline -lpthread

# Inject lsqlite3, lfs, and yaml preload
sed -i '/luaL_openlibs(L);/a \
  int luaopen_sqlite3(lua_State *L); \
  int luaopen_lfs(lua_State *L); \
  int luaopen_yaml(lua_State *L); \
  lua_getglobal(L, "package"); \
  lua_getfield(L, -1, "preload"); \
  lua_pushcfunction(L, luaopen_sqlite3); \
  lua_setfield(L, -2, "lsqlite3"); \
  lua_pushcfunction(L, luaopen_lfs); \
  lua_setfield(L, -2, "lfs"); \
  lua_pushcfunction(L, luaopen_yaml); \
  lua_setfield(L, -2, "yaml"); \
  lua_pop(L, 2);' brex.static.c

# Compile lsqlite3
cc -c -O2 -I"$LUAM_DIR/src" "$LUAM_DIR/lib/sqlite/lsqlite3.c" -o lsqlite3.o

# Compile lfs
cc -c -O2 -I"$LUAM_DIR/src" "$LUAM_DIR/lib/lfs/src/lfs.c" -o lfs.o

# Compile yaml (all source files)
YAML_SRC="$LUAM_DIR/lib/yaml"
cc -c -O2 -I"$LUAM_DIR/src" -I"$YAML_SRC" \
    "$YAML_SRC/lyaml.c" "$YAML_SRC/api.c" "$YAML_SRC/b64.c" \
    "$YAML_SRC/dumper.c" "$YAML_SRC/emitter.c" "$YAML_SRC/loader.c" \
    "$YAML_SRC/parser.c" "$YAML_SRC/reader.c" "$YAML_SRC/scanner.c" "$YAML_SRC/writer.c"

# Compile binary
cc -Os brex.static.c lsqlite3.o lfs.o lyaml.o api.o b64.o dumper.o emitter.o loader.o parser.o reader.o scanner.o writer.o "$LUAM_LIB" \
    -I "$LUAM_DIR/src" \
    -lm -ldl -lreadline -lpthread -lsqlite3 \
    -Wl,--export-dynamic \
    -o brex

popd > /dev/null

mkdir -p bld
mv "$TMPDIR"/brex bld/
cp -r "$LUAM_DIR/lib/dkjson" bld/
cp "$LUAM_DIR/lib/yaml/yaml.so" bld/
cp "$LUAM_DIR/lib/lfs/lfs.so" bld/
cp "$LUAM_DIR/lib/sqlite/lsqlite3.so" bld/
cp -r "$LUAM_DIR/lib/socket" bld/
cp -r "$LUAM_DIR/lib/mime" bld/
echo "Build complete. Binary in bld/brex"
ls -lh bld/brex
