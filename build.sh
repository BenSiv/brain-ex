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
    bx_utils.lua \
    config.lua \
    init.lua \
    note.lua \
    task.lua \
    update.lua \
    sql.lua \
    vault_to_sql.lua \
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
echo "Suggested next step: cp bld/brex /usr/local/bin/brex"

cat <<'EOF'
 ____            _             _____      
| __ ) _ __ __ _(_)_ __       | ____|_  __
|  _ \| '__/ _` | | '_ \ _____|  _| \ \/ /
| |_) | | | (_| | | | | |_____| |___ >  < 
|____/|_|  \__,_|_|_| |_|     |_____/_/\_\
EOF

# bats tst/
