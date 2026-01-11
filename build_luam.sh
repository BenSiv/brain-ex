#!/usr/bin/env bash
set -euo pipefail

# Determine absolute paths
ROOT=$(pwd)
LUAM_DIR=$(cd ../luam && pwd)
LUAM_BIN="$LUAM_DIR/bld/luam"
STATIC_TOOL="$LUAM_DIR/lib/static.lua"
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
# Remove static.lua (tool) to prevent it from being compiled into the binary source list inadvertently
rm "$TMPDIR"/static.lua 2>/dev/null || true

# Copy dkjson alias (already in lib/*.lua, ensuring it's there)
# cp "$LUAM_DIR/lib/dkjson.lua" "$TMPDIR"/

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

# Inject lsqlite3 preload
sed -i '/luaL_openlibs(L);/a \
  int luaopen_lsqlite3(lua_State *L); \
  lua_getglobal(L, "package"); \
  lua_getfield(L, -1, "preload"); \
  lua_pushcfunction(L, luaopen_lsqlite3); \
  lua_setfield(L, -2, "lsqlite3"); \
  lua_pop(L, 2);' brex.static.c

# Compile lsqlite3
cc -c -O2 -I"$LUAM_DIR/src" "$LUAM_DIR/lib/sqlite/lsqlite3.c" -o lsqlite3.o

# Compile binary
cc -Os brex.static.c lsqlite3.o "$LUAM_LIB" \
    -I "$LUAM_DIR/src" \
    -lm -ldl -lreadline -lpthread -lsqlite3 \
    -o brex

popd > /dev/null

mkdir -p bld
mv "$TMPDIR"/brex bld/
cp -r "$LUAM_DIR/lib/json" bld/
cp "$LUAM_DIR/lib/yaml.so" bld/
cp "$LUAM_DIR/lib/lfs.so" bld/
cp "$LUAM_DIR/lib/lsqlite3.so" bld/
cp -r "$LUAM_DIR/lib/socket" bld/
cp -r "$LUAM_DIR/lib/mime" bld/
echo "Build complete. Binary in bld/brex"
ls -lh bld/brex
