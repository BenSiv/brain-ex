#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PKG_NAME="${PKG_NAME:-brain-ex}"
BIN_PATH="${BIN_PATH:-$ROOT_DIR/bin/brex}"
OUT_DIR="${OUT_DIR:-$ROOT_DIR/pub}"
ARCH="${ARCH:-$(dpkg --print-architecture)}"
MAINTAINER="${MAINTAINER:-Ben Sivan <bensiv92@gmail.com>}"

sanitize_deb_version() {
    raw="$1"
    # Allowed chars in Debian version: [A-Za-z0-9.+:~\-]
    cleaned="$(sed 's/[^A-Za-z0-9.+:~-]/+/g' <<<"$raw")"
    if [[ ! "$cleaned" =~ ^[0-9] ]]; then
        cleaned="0.0.0+$cleaned"
    fi
    echo "$cleaned"
}

if [[ -z "${VERSION:-}" ]]; then
    if git -C "$ROOT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        derived_version="$(git -C "$ROOT_DIR" describe --tags --always 2>/dev/null | sed 's/^v//' | tr '-' '+')"
        VERSION="$(sanitize_deb_version "$derived_version")"
    else
        VERSION="0.1.0"
    fi
fi

if [[ ! -x "$BIN_PATH" ]]; then
    echo "Missing executable: $BIN_PATH"
    echo "Build first: ./bld/build.sh"
    exit 1
fi

if ! command -v dpkg-deb >/dev/null 2>&1; then
    echo "dpkg-deb is required to build .deb packages"
    exit 1
fi

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

PKG_ROOT="$TMP_DIR/${PKG_NAME}_${VERSION}_${ARCH}"
DEBIAN_DIR="$PKG_ROOT/DEBIAN"
DOC_DIR="$PKG_ROOT/usr/share/doc/$PKG_NAME"
BIN_DST="$PKG_ROOT/usr/local/bin"

mkdir -p "$DEBIAN_DIR" "$DOC_DIR" "$BIN_DST" "$OUT_DIR"
install -m 0755 "$BIN_PATH" "$BIN_DST/brex"
install -m 0644 "$ROOT_DIR/README.md" "$DOC_DIR/README.md"
install -m 0644 "$ROOT_DIR/LICENSE" "$DOC_DIR/copyright"

declare -a detected_deps
while IFS= read -r line; do
    lib_path="$(awk '{print $3}' <<<"$line")"
    if [[ "$lib_path" == /* ]]; then
        search_path="$lib_path"
        if ! dpkg-query -S "$search_path" >/dev/null 2>&1; then
            resolved="$(realpath "$lib_path" 2>/dev/null || true)"
            if [[ -n "$resolved" ]]; then
                search_path="$resolved"
            fi
        fi
        pkg="$(dpkg-query -S "$search_path" 2>/dev/null | head -n1 | cut -d: -f1 || true)"
        if [[ -n "$pkg" ]]; then
            detected_deps+=("$pkg")
        fi
    fi
done < <(ldd "$BIN_PATH")

# Command-level dependency for `brex sql` shell mode.
detected_deps+=("sqlite3")

# De-duplicate and sort dependencies.
mapfile -t unique_deps < <(printf '%s\n' "${detected_deps[@]}" | awk 'NF' | sort -u)
depends="$(IFS=, ; echo "${unique_deps[*]}")"

cat >"$DEBIAN_DIR/control" <<EOF
Package: $PKG_NAME
Version: $VERSION
Section: utils
Priority: optional
Architecture: $ARCH
Maintainer: $MAINTAINER
Depends: $depends
Recommends: git, nano
Description: brain-ex note taking and task management CLI
 Minimalist, fast, CLI-first notes and tasks.
EOF

OUT_DEB="$OUT_DIR/${PKG_NAME}_${VERSION}_${ARCH}.deb"
dpkg-deb --build "$PKG_ROOT" "$OUT_DEB" >/dev/null

echo "Built: $OUT_DEB"
echo "Depends: $depends"
echo "Recommends: git, nano"
