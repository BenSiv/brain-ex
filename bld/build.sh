#!/usr/bin/env bash
set -euo pipefail

# Thin delegation to luam's own shared build orchestrator
# (lib/static/build.lua) -- this project no longer hand-rolls its own
# temp-dir/file-flattening/C-extension-compiling/preload-injection
# logic; that's all owned in one place, in luam itself, instead of
# copy-pasted and independently re-edited across every downstream
# project that needs it. See lib/static/build.lua's own header comment
# for why it's a Luam script rather than another bash script.

cd "$(dirname "$0")/.."

if [ -z "${LUAM_DIR:-}" ]; then
    LUAM_DIR=$(cd ../luam && pwd)
fi

if [ ! -f "$LUAM_DIR/obj/liblua.a" ]; then
    echo "Error: $LUAM_DIR/obj/liblua.a not found. Set LUAM_DIR to a built luam checkout." >&2
    exit 1
fi

exec "$LUAM_DIR/bin/luam" "$LUAM_DIR/lib/static/build.lua" \
    --entry brex.lua \
    --bin brex \
    --with sqlite3,lfs,yaml \
    --luamdir "$LUAM_DIR" \
    "$@"
