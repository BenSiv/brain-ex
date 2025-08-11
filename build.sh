#!/usr/bin/env bash
set -euo pipefail

# Move to script directory
cd "$(dirname "$0")"

# Create temp dir
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# Copy all lua source files into one flat dir
cp brex.lua "$TMPDIR"/
cp src/*.lua "$TMPDIR"/
cp lua-utils/src/*.lua "$TMPDIR"/

# Build in temp dir
pushd "$TMPDIR" > /dev/null

luastatic brex.lua \
    init.lua \
    note.lua \
    task.lua \
    sql.lua \
    update.lua \
    vault_to_sql.lua \
    bx_utils.lua \
    help.lua \
    argparse.lua \
    database.lua \
    dataframes.lua \
    dates.lua \
    delimited_files.lua \
    paths.lua \
    prettyprint.lua \
    string_utils.lua \
    table_utils.lua \
    utils.lua \
    -I/usr/include/lua5.1 -llua5.1 -lm -ldl -lreadline -o brex

popd > /dev/null

# Ensure output dir exists
mkdir -p bld

# Move build outputs
mv "$TMPDIR"/brex bld/
mv "$TMPDIR"/brex.luastatic.c bld/

echo "Build complete. Binary in bld/brex"
