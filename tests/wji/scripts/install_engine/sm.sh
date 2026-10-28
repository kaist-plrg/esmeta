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

# The FIREFOX_158_0_RELEASE tag commit (latest stable release as of
# 2026-10-16 per https://www.firefox.com/en-US/releases/, cross-checked
# against `git ls-remote --tags https://github.com/mozilla-firefox/firefox.git`
# for the matching tag) -- must be *recent*, not just *a* release tag:
# `mach bootstrap` fetches toolchain/SDK artifacts whose URLs expire after
# some retention window, so an old-enough release tag (an earlier version of
# this script pinned FIREFOX_149_0_RELEASE, ~7 months old at the time) makes
# bootstrap itself fail with an HTTP 403 partway through, before a build is
# even attempted. Bump this the next time this script needs to track a newer
# release -- re-check the live releases page above rather than just grepping
# nearby version numbers out of the tag list (easy to undercount how far the
# version has actually climbed).
FIREFOX_COMMIT=3c71aeacdb296402e41fb2084b13e797d1770642 # FIREFOX_158_0_RELEASE

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
