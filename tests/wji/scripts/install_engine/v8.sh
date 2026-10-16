#!/bin/bash
# ------------------------------------------------------------------------------
# Usage: tests/wji/scripts/install_engine/v8.sh [--force]
#
# Builds V8 (the engine Chrome/Node use) from source at a pinned commit, so
# WJI's own test output can be checked against a real engine's -- see
# README.md in this directory for the full picture (why this exists, disk/time
# cost, prerequisites).
#
# Refuses to run if $ENGINES_DIR/depot_tools or $ENGINES_DIR/v8 already exists
# -- pass --force to wipe and rebuild both from scratch instead.
#
# Result: $ENGINES_DIR/v8/v8/out/<arch>.release/d8
# ------------------------------------------------------------------------------

cd "$(dirname "${BASH_SOURCE[0]}")"
# shellcheck source=./common.sh
. ./common.sh

# Commit matching Chrome 155.0.8059.84 (Stable channel, checked 2026-10-16
# against https://chromiumdash.appspot.com/releases?channel=Stable) -- i.e.
# "whatever a current real Chrome install actually ships", not just V8's own
# tip-of-tree. Bump this (and the version comment) the next time this script
# needs to track a newer stable release.
V8_COMMIT=712e1808de2d7416f1276d0deb1ee4ac978132b8 # V8 15.5.35.25

# gm.py's target architecture must match the host -- building "x64" on an
# arm64 host would still produce a binary, just one that only runs under
# Rosetta/qemu emulation instead of natively.
case "$(uname -m)" in
  arm64 | aarch64) gm_arch=arm64 ;;
  *) gm_arch=x64 ;;
esac

export TAR_OPTIONS=--no-same-owner

require_absent_or_force "$ENGINES_DIR/depot_tools" -- "$@"
require_absent_or_force "$ENGINES_DIR/v8" -- "$@"

cd "$ENGINES_DIR"
git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
export PATH="$ENGINES_DIR/depot_tools:$PATH"
gclient # first run with no args: lets depot_tools self-bootstrap

mkdir v8
cd v8

fetch v8
cd v8
git checkout "$V8_COMMIT"
gclient sync
tools/dev/gm.py "$gm_arch.release"

echo "V8 (d8) built at: $PWD/out/$gm_arch.release/d8"
