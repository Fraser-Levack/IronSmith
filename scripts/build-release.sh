#!/usr/bin/env bash
# Builds the Haskell forge and the Rust viewer in release mode and
# packages both binaries into a tarball under dist/, ready to attach to a
# GitHub release.
#
# Usage: ./scripts/build-release.sh [version]

set -euo pipefail

VERSION="${1:-0.1.0}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

case "$(uname -s)" in
    Linux*)  PLATFORM="linux" ;;
    Darwin*) PLATFORM="macos" ;;
    *)       PLATFORM="unknown" ;;
esac

echo "==> Building IronSmith-Viewer (Rust, release)..."
(cd "$REPO_ROOT/IronSmith-Viewer" && cargo build --release)

echo "==> Building ironsmith (Haskell, cabal)..."
(cd "$REPO_ROOT" && cabal build)
IRONSMITH_EXE="$(cd "$REPO_ROOT" && cabal list-bin ironsmith)"

PKG_NAME="IronSmith-v${VERSION}-${PLATFORM}"
PKG_DIR="$REPO_ROOT/dist/$PKG_NAME"

rm -rf "$PKG_DIR"
mkdir -p "$PKG_DIR"

echo "==> Copying binaries..."
cp "$IRONSMITH_EXE" "$PKG_DIR/ironsmith"
cp "$REPO_ROOT/IronSmith-Viewer/target/release/ironsmith-viewer" "$PKG_DIR/ironsmith-viewer"
chmod +x "$PKG_DIR/ironsmith" "$PKG_DIR/ironsmith-viewer"

echo "==> Copying extra files..."
cp "$REPO_ROOT/README.md" "$PKG_DIR/"
cp "$REPO_ROOT/LICENSE" "$PKG_DIR/"
cp "$REPO_ROOT/demo.irsm" "$PKG_DIR/"
cp "$REPO_ROOT/scripts/install.sh" "$PKG_DIR/"

TARBALL="$REPO_ROOT/dist/$PKG_NAME.tar.gz"
rm -f "$TARBALL"
tar -czf "$TARBALL" -C "$REPO_ROOT/dist" "$PKG_NAME"

echo "==> Done! Package created at $TARBALL"
