#!/usr/bin/env bash
# Installs IronSmith for the current user.
#
# Copies this folder's contents to ~/.local/share/ironsmith and symlinks
# 'ironsmith' into ~/.local/bin so it can be run from anywhere (assuming
# ~/.local/bin is on your PATH, which it is by default on most distros).
#
# Usage: ./install.sh

set -euo pipefail

SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSTALL_DIR="$HOME/.local/share/ironsmith"
BIN_DIR="$HOME/.local/bin"

echo "Installing IronSmith to $INSTALL_DIR ..."
mkdir -p "$INSTALL_DIR" "$BIN_DIR"
cp -r "$SOURCE_DIR"/* "$INSTALL_DIR/"
rm -f "$INSTALL_DIR/install.sh"
chmod +x "$INSTALL_DIR/ironsmith" "$INSTALL_DIR/ironsmith-viewer"

ln -sf "$INSTALL_DIR/ironsmith" "$BIN_DIR/ironsmith"

case ":$PATH:" in
    *":$BIN_DIR:"*)
        echo "$BIN_DIR is already in your PATH."
        ;;
    *)
        echo "Add $BIN_DIR to your PATH to run 'ironsmith' from anywhere, e.g.:"
        echo "  echo 'export PATH=\"\$PATH:$BIN_DIR\"' >> ~/.bashrc"
        ;;
esac

echo ""
echo "Done! Open a new terminal and run 'ironsmith' to get started."
