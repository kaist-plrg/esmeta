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
