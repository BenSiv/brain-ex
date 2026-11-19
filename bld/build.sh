#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
LUAM_DIR="${LUAM_DIR:-$(cd ../luam 2>/dev/null && pwd || true)}"
exec "$LUAM_DIR/bin/luam" "$LUAM_DIR/lib/static/build.lua" --luamdir "$LUAM_DIR" "$@"
