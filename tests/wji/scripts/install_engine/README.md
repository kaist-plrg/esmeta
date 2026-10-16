# install_engine

Builds a real JS engine from source, pinned to a specific commit, so WJI's own
test output (`tests/wji/manual/`, `tests/wji/complementary/`) can be checked
against what a real browser actually does -- the same "confirmed against real
Node X.Y.Z" discipline those test files already use, but for V8/SpiderMonkey/
JavaScriptCore directly instead of only via Node (which only ever gives you
V8's behavior).

Ported from `spectec/spectec/scripts/install_engine/` (the original `west`
research script set, pinned to much older commits and missing a few things --
see "Changes from the original" below) -- only the three real browser engines
(`v8.sh`/`sm.sh`/`jsc.sh`), not `wasmer.sh`/`wasmtime.sh` (standalone Wasm
runtimes, a different kind of oracle, out of scope here).

## Usage

```
tests/wji/scripts/install_engine/v8.sh   # -> $ENGINES_DIR/v8/v8/out/<arch>.release/d8
tests/wji/scripts/install_engine/sm.sh   # -> $ENGINES_DIR/firefox/<objdir>/dist/bin/js
tests/wji/scripts/install_engine/jsc.sh  # -> found automatically, printed at the end
```

Each script is independent -- run whichever engine(s) you actually need.
`$ENGINES_DIR` (default `$HOME/wji-engines`) holds every engine's full source
checkout and build output; override it by exporting the variable before
running a script.

**Each script refuses to run if its own target directory under `$ENGINES_DIR`
already exists** (e.g. `sm.sh` checks `$ENGINES_DIR/firefox`) -- pass
`--force` to wipe and rebuild it from scratch instead. Default is "stop", not
"wipe", on purpose: a stray extra run or a typo'd re-run shouldn't silently
throw away a build that took anywhere from tens of minutes to a few hours.

## Before you run one of these

- **Disk space and time**: each of these is a full from-source build of a
  real browser engine. Expect on the order of tens of GB of disk and anywhere
  from tens of minutes to a few hours per engine, depending on the machine.
  Not something to kick off without knowing that going in.
- **Prerequisites**: V8 needs `depot_tools` (which `v8.sh` clones itself) and
  a working `git`/`python3`. SpiderMonkey and JavaScriptCore need platform
  build tools -- on macOS, a full Xcode install with its license accepted
  (not just the standalone command-line tools -- V8's own build docs call
  this out explicitly: mixing the two breaks the build). `sm.sh`'s `mach
  --no-interactive bootstrap --application-choice=js` step installs most of
  what SpiderMonkey itself needs, but may still prompt for your password to
  install system packages (Homebrew casks, etc.).
- **This isn't a build dependency of WJI/esmeta itself** -- same spirit as
  `tests/wji/scripts/wat2js`'s own disclaimer. These exist purely to let a
  human (or an agent working interactively) check a specific behavior against
  a real engine while authoring a `tests/wji/complementary/` test case.

## Pinned versions

Each script hardcodes one commit, chosen to track a real, currently-shipping
build rather than a possibly-broken tip-of-tree:

- **V8**: the commit matching Chrome's current Stable channel release (looked
  up via `https://chromiumdash.appspot.com/releases?channel=Stable` -- its
  `hashes.v8` field is exactly this commit).
- **SpiderMonkey**: the latest `FIREFOX_*_0_RELEASE` tag on
  `https://github.com/mozilla-firefox/firefox` (a real shipped release, not a
  mozilla-central tip that might not even build).
- **JavaScriptCore**: WebKit doesn't tag discrete "stable release" commits
  in-repo the way Firefox does, so this just tracks a recent `main` HEAD
  commit instead -- not independently verified to build cleanly, unlike the
  other two.

These will drift out of date -- there's no automation keeping them current.
Bump the commit (and its comment) in the relevant script the next time you
need a newer one; each lookup method above is documented right in that
script's own comment, next to the commit it pins.

## Changes from the original `spectec` scripts

- **Non-interactive SpiderMonkey bootstrap**: the original piped three canned
  answers (`5`, `Y`, `Y`) into `mach bootstrap`'s interactive numbered menu,
  which silently breaks the moment Mozilla reorders or adds a menu entry.
  Replaced with `mach --no-interactive bootstrap --application-choice=js`
  (`--no-interactive` is a global `mach` flag, so it must come before the
  subcommand -- `bootstrap` itself rejects it as unrecognized), the actual
  flag-based non-interactive path `mach` itself provides for exactly this
  "SpiderMonkey only, no prompts" case.
- **`set -e` everywhere**: only `v8.sh` had it originally. `sm.sh`/`jsc.sh`
  would silently continue past a failed step (e.g. a failed `git fetch`)
  straight into the next one -- `set -euo pipefail` is now shared by all
  three via `common.sh`.
- **Actually-exported environment variables**: `v8.sh`'s own `PATH=`/
  `TAR_OPTIONS=` and `sm.sh`'s `MOZCONFIG=` were plain (non-`export`ed) shell
  variable assignments in the original -- invisible to the child processes
  (`gclient`/`fetch`/`mach build`) that actually need to read them from their
  environment. Now `export`ed.
- **Host-architecture-aware V8 build**: the original always built
  `x64.release`; on an arm64 host that's an emulated (Rosetta/qemu) binary,
  not a native one. `v8.sh` now picks `arm64.release`/`x64.release` to match
  `uname -m`.
- **No separately-maintained `input` file**: no longer needed once
  `sm.sh` moved to flag-based non-interactive bootstrapping.
- **Dedicated, configurable `$ENGINES_DIR`** instead of hardcoded `~/v8`/
  `~/firefox`/`~/WebKit`: those are exactly the kind of generic names a
  machine might already have something unrelated at, which a script that
  unconditionally `rm -rf`s its target on every run should avoid by default.
- **Versions repinned**: all three were pinned to commits from the original
  `west` research (dated by inline comments like `#11/12`); repinned to
  current releases as of 2026-10-16 (see "Pinned versions" above).
- **Refuses to clobber an existing checkout**: the original unconditionally
  `rm -rf`'d its target directory every run. Each script now stops with an
  error if that directory already exists, requiring `--force` to actually
  wipe it -- see "Usage" above.
