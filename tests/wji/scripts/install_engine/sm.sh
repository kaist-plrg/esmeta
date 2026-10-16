#!/bin/bash
# ------------------------------------------------------------------------------
# Usage: tests/wji/scripts/install_engine/sm.sh [--force]
#
# Builds SpiderMonkey (the JS shell, not the full Firefox browser) from source
# at a pinned commit -- see README.md in this directory for the full picture.
#
# Refuses to run if $ENGINES_DIR/firefox already exists -- pass --force to
# wipe and rebuild it from scratch instead.
#
# Result: $ENGINES_DIR/firefox/<objdir>/dist/bin/js
# ------------------------------------------------------------------------------

cd "$(dirname "${BASH_SOURCE[0]}")"
# shellcheck source=./common.sh
. ./common.sh
script_dir="$PWD"

# The FIREFOX_149_0_RELEASE tag commit (latest stable release tag as of
# 2026-10-16, checked via `git ls-remote --tags
# https://github.com/mozilla-firefox/firefox.git`) -- a real shipped release,
# not a possibly-broken mozilla-central tip. Bump this the next time this
# script needs to track a newer release.
FIREFOX_COMMIT=c7fa3c91990bac266ee99a9e31863c202469f369 # FIREFOX_149_0_RELEASE

require_absent_or_force "$ENGINES_DIR/firefox" -- "$@"

cd "$ENGINES_DIR"
mkdir firefox
cd firefox

git init
git remote add origin https://github.com/mozilla-firefox/firefox.git
git fetch --depth=1 --no-tags origin "$FIREFOX_COMMIT"
git reset --hard "$FIREFOX_COMMIT"

export MOZCONFIG="$script_dir/mozconfig"

# `--no-interactive` (a global `mach` flag, so it has to come *before* the
# `bootstrap` subcommand -- `bootstrap` itself rejects it as an unrecognized
# argument) + `--application-choice=js` bootstraps only what a JS-shell-only
# build needs, with no interactive prompts -- replaces an earlier version of
# this script that piped canned answers into the interactive numbered menu,
# which silently breaks the moment Mozilla reorders/adds a menu entry.
./mach --no-interactive bootstrap --application-choice=js

./mach build

js_bin=$(find . -path '*/dist/bin/js' | head -n 1)
echo "SpiderMonkey (js) built at: ${js_bin:-<not found -- check the build log above>}"
