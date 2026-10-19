#!/bin/bash
# ------------------------------------------------------------------------------
# Shared config/helpers sourced by v8.sh/sm.sh/jsc.sh -- see README.md for what
# these scripts are for and how to use them. Not meant to be run directly.
# ------------------------------------------------------------------------------

set -euo pipefail

# Where each engine's full source checkout + build output lives. Override by
# exporting ENGINES_DIR before running a script. Defaults to a dedicated
# directory rather than e.g. ~/v8 or ~/firefox directly -- each engine script
# does a destructive `rm -rf` of its own subdirectory here on every run, and a
# generic name under $HOME is exactly the kind of thing a user might already
# have for something unrelated.
: "${ENGINES_DIR:=$HOME/wji-engines}"
mkdir -p "$ENGINES_DIR"

# Whether "$@" (the calling script's own argv) contains --force.
has_force_flag() {
  local arg
  for arg in "$@"; do
    [ "$arg" = "--force" ] && return 0
  done
  return 1
}

# usage: require_absent_or_force <target-dir> -- "$@" (the calling script's
# own argv, so this can check it for --force)
#
# Refuses to touch an already-existing <target-dir> unless --force was passed,
# in which case it's wiped here so the caller can freely recreate it. Default
# is "stop", not "wipe", specifically because every caller's own next step is
# an unconditional `rm -rf` of exactly this directory -- erring the other way
# would mean a stray extra run (or a typo'd re-run) silently throws away a
# build that took anywhere from tens of minutes to a few hours.
require_absent_or_force() {
  local target="$1"
  shift
  if [ -e "$target" ]; then
    if ! has_force_flag "$@"; then
      echo "error: $target already exists -- pass --force to wipe and reinstall" >&2
      exit 1
    fi
    rm -rf "$target"
  fi
}

# ------------------------------------------------------------------------------
# Locating an already-built engine binary under $ENGINES_DIR. Each echoes the
# found path on success and nothing on failure (return 1) -- never a `set -e`
# -fatal exit, since this file is sourced by both the install scripts (where a
# binary genuinely not existing yet is normal) and diff-engines (which must
# skip an engine that isn't built, not abort the whole run).
# ------------------------------------------------------------------------------

# $ENGINES_DIR/v8/v8/out/<arch>.release/d8 -- <arch> (arm64/x64) varies by
# host, so glob rather than hardcode (mirrors v8.sh's own arch detection).
locate_v8() {
  local f
  for f in "$ENGINES_DIR"/v8/v8/out/*.release/d8; do
    [ -x "$f" ] && echo "$f" && return 0
  done
  return 1
}

# $ENGINES_DIR/firefox/<objdir>/dist/bin/js -- <objdir> is a mach-generated
# name that varies per host/build config, so find rather than hardcode (same
# pattern sm.sh itself uses to report where it ended up).
locate_sm() {
  find "$ENGINES_DIR/firefox" -type f -path '*/dist/bin/js' -perm -u+x \
    2>/dev/null | head -n1 || true
}

# $ENGINES_DIR/WebKit/WebKitBuild/Release/JavaScriptCore.framework/Versions/A/
# Helpers/jsc -- deliberately matched by this specific path shape, not just
# `-name jsc`: a plain `-name jsc` also turns up a second, different binary at
# .../WebKitBuild/Release/jsc (a thin top-level launcher) which crashes
# outright when run directly -- confirmed empirically:
#   dyld[...]: Symbol not found: __ZNK3JSC14SourceProvider22...
#   Referenced from: .../WebKitBuild/Release/jsc
#   Expected in:     /System/Library/Frameworks/JavaScriptCore.framework/...
# it dynamically links against the *system's* built-in JavaScriptCore.framework
# rather than the one just built sitting next to it (no working
# @rpath/DYLD_FRAMEWORK_PATH when invoked this way), so it's missing whatever
# symbols this build added. The `Helpers/jsc` *inside* the framework bundle
# resolves its sibling framework binary via a bundle-relative path instead, so
# it actually runs against what was just built -- this is the one jsc.sh's own
# build already verified works (wasm instantiate + call, confirmed in-session).
# `find`'s result order isn't guaranteed, so a plain `-name jsc | head -n1`
# could silently pick either one -- matching the exact path shape avoids that.
locate_jsc() {
  find "$ENGINES_DIR/WebKit/WebKitBuild" -path '*/Helpers/jsc' -type f \
    -perm -u+x 2>/dev/null | head -n1 || true
}
