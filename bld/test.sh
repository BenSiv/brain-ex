#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
LUAM_DIR="${LUAM_DIR:-$(cd ../luam 2>/dev/null && pwd || true)}"

echo "Building bin/brex..."
LUAM_DIR="$LUAM_DIR" ./bld/build.sh

exec bats tst/*.bats
