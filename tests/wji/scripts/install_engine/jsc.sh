#!/bin/bash
# ------------------------------------------------------------------------------
# Usage: tests/wji/scripts/install_engine/jsc.sh [--force]
#
# Builds JavaScriptCore (jsc, Safari's engine) from source at a pinned commit
# -- see README.md in this directory for the full picture.
#
# Refuses to run if $ENGINES_DIR/WebKit already exists -- pass --force to wipe
# and rebuild it from scratch instead.
#
# Result: wherever the build actually places the `jsc` binary under
# $ENGINES_DIR/WebKit/WebKitBuild/ -- found automatically at the end (see
# README.md's note on why this isn't a single fixed path).
# ------------------------------------------------------------------------------

cd "$(dirname "${BASH_SOURCE[0]}")"
# shellcheck source=./common.sh
. ./common.sh

# WebKit main HEAD as of 2026-10-16 (checked via
# `git ls-remote https://github.com/WebKit/WebKit.git HEAD`) -- WebKit doesn't
# tag discrete "stable release" commits in-repo the way Firefox does, so this
# just tracks a recent `main` HEAD commit instead (not independently verified
# to build cleanly -- WebKit's own CI is the only real signal for that). Bump
# this the next time this script needs a newer commit.
WEBKIT_COMMIT=6de1d8257b7a575d1633e33a6565ff49730156d8

require_absent_or_force "$ENGINES_DIR/WebKit" -- "$@"

cd "$ENGINES_DIR"
mkdir WebKit
cd WebKit

git init
git remote add origin https://github.com/WebKit/WebKit.git
git fetch --depth=1 --no-tags origin "$WEBKIT_COMMIT"
git reset --hard "$WEBKIT_COMMIT"

./Tools/Scripts/build-webkit --jsc-only --release

jsc_bin=$(find WebKitBuild -name jsc -type f -perm -u+x | head -n 1)
echo "JavaScriptCore (jsc) built at: ${jsc_bin:-<not found -- check the build log above>}"
