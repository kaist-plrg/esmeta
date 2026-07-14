# Spec Errors

스펙 작성자에게 보고할 오류 목록입니다.

## 1. Empty ordered map literal written as `« »` instead of `«[ ]»`

- **File**: `spectec/document/js-api/index.bs`, line 486
- **Current**: `Let |builtinOrStringImports| be the ordered map « ».`
- **Expected**: `Let |builtinOrStringImports| be the ordered map «[ ]».`
- **Reason**: `« »` is the Infra spec notation for an empty *list*, while `«[ ]»` is the notation for an empty *ordered map*. These are semantically distinct types.

## 2. Variable `keys` written without pipe delimiters in `[[OwnPropertyKeys]]`

- **File**: `spectec/document/js-api/index.bs`, line 1642
- **Current**:
  ```
  1. Let keys be a new empty list.
  1. Return keys.
  ```
- **Expected**:
  ```
  1. Let |keys| be a new empty list.
  1. Return |keys|.
  ```
- **Reason**: Bikeshed/Infra spec variables must be wrapped in `|...|` to be recognized as variable references. Without the pipes, `keys` is treated as an unrecognized identifier rather than a variable binding.

## 3. `a new promise` called without its required `realm` argument

- **File**: `spectec/document/js-api/index.bs`, lines 448, 616, 637
- **Current**: `1. Let |promise| be [=a new promise=].`
- **Expected**: `1. Let |promise| be [=a new promise=] in the [=current Realm=].`
- **Reason**: `a new promise` is defined in `webidl/index.bs` (line 8607-8609) as `To create a new Promise<T> in a [=realm=] |realm|, perform the following steps: ...` — i.e. it takes a required `|realm|` parameter. js-api/index.bs calls it with no argument at all, presumably relying on the convention that algorithms implicitly run "in the current realm" unless stated otherwise. That convention isn't machine-checkable: without an explicit argument there is nothing to bind `|realm|` to, so the call cannot be mechanized as written.

## 4. `|module|.[=imports=]` accesses a decoded module as a record field, but `module` isn't a record

- **File**: `spectec/document/js-api/index.bs`, line 485
- **Current**: `1. If |module|.[=imports=] [=list/is empty|is not empty=], and |importObject| is undefined, throw a {{TypeError}} exception.`
- **Expected**: something like `1. If [=module_imports=](|module|) is not empty, and |importObject| is undefined, throw a {{TypeError}} exception.` (matching how every other use of this same data, e.g. lines 405, 474, 497, 736, goes through the `module_imports` embedding function instead)
- **Reason**: `module_imports` is declared as an Embedding-API function (`appendix/embedding.html#embed-module-imports`, aliased at line 138) precisely because a decoded `module` is not a record with named fields — in the current (post-GC-proposal) Wasm Core Spec / SpecTec representation it's an opaque `CaseV("MODULE", [...])` value with *positional*, not named, fields (see `Construct.al_of_module` in `spectec/spectec/src/backend-interpreter/construct.ml:2002`). Older versions of the Wasm spec modeled `module` as a plain record (`StrV`), for which `|module|.[=imports=]` field access would have been valid; this one leftover use at line 485 was never updated when the representation changed, while every other call site in the same file correctly goes through `module_imports(|module|)`.

## 5. {{Global}}'s `value` getter and setter share a single `<div algorithm>`

- **File**: `spectec/document/js-api/index.bs`, lines 1217-1231
- **Current**: one `<div algorithm>` containing both "The getter of the `<dfn attribute for="Global">value</dfn>` attribute of {{Global}}, when invoked, performs the following steps: ..." and, immediately after its steps, "The setter of the value attribute of {{Global}}, when invoked, performs the following steps: ...".
- **Expected**: two separate `<div algorithm>` blocks, one per algorithm — matching how every other algorithm in the file, including every other getter (e.g. {{Instance}}.exports, {{Memory}}.buffer, {{Table}}.length), gets its own `<div algorithm>`. This is the only setter in the file, so there's no other setter to match against, but there's likewise no precedent anywhere else for two algorithms sharing one block.
- **Reason**: every algorithm-extraction tool in this space (including ours) extracts one algorithm per `<div algorithm>` block. With both crammed into one, a naive extractor concatenates the setter's steps onto the end of the getter's body — text that reads as prose introducing a *second* algorithm, but mechanically is indistinguishable from dead code appended after the getter's own `Return` step. This is the only place in the file where two algorithms share a block (verified by scanning every `<div algorithm>` for more than one top-level "performs/runs the following steps:" trigger, excluding nested ones like `Queue a task`/`Run the following steps in parallel`).

## 6. `[=map/exist|contains=]` uses "list contains" phrasing for a map-membership check

- **File**: `spectec/document/js-api/index.bs`, line 498
- **Current**: `1. If |builtinOrStringImports| [=map/exist|contains=] |moduleName|,`
- **Expected**: `1. If |builtinOrStringImports|[|moduleName|] [=map/exists=],` — matching every other map-membership check in the file (15+ occurrences, e.g. lines 864, 1028, 1164, 1263, all written `|map|[|key|] [=map/exists=]`, with negation as `|map|[|key|] doesn't [=map/exist=]`).
- **Reason**: `[=map/exist|contains=]` links to the `map/exist` dfn but *displays* "contains" (Bikeshed's `|display-text=]` aliasing), and the sentence around it — `X contains Y` — is exactly WebIDL/Infra's "list contains" idiom, not the map-membership idiom used everywhere else in this file (`base[key] exists`). This reads like a leftover from when `builtinOrStringImports` was a list: spec error #1 already shows its empty-literal is still written `« »` (the *list* notation) instead of `«[ ]»` (the *ordered map* notation), i.e. the same variable's declaration has the same kind of leftover. It looks like only the link target was updated from `list/contains` to `map/exist` when the type changed from list to map, without updating the surrounding sentence to the map idiom.

## 7. Dead host-function/module-function branch in `name of the WebAssembly function`

- **File**: `spectec/document/js-api/index.bs`, lines ~513 (`read the imports`) and ~1249-1255 (`name of the WebAssembly function`)
- **Current**:
  ```
  1. [=Create a host function=] from |v| and |functype|, and let |funcaddr| be the result.
  1. Let |index| be the number of external functions in |imports|. This value |index| is known as the <dfn>index of the host function</dfn> |funcaddr|.
  ```
  and
  ```
  1. If |funcinst| is of the form {type <var ignore>functype</var>, hostcode |hostfunc|},
      1. Assert: |hostfunc| is a JavaScript object and [$IsCallable$](|hostfunc|) is true.
      1. Let |index| be the [=index of the host function=] |funcaddr|.
  1. Otherwise,
      1. Let |moduleinst| be |funcinst|.module.
      1. Assert: |funcaddr| is contained in |moduleinst|.funcaddrs.
      1. Let |index| be the index of |moduleinst|.funcaddrs where |funcaddr| is found.
  ```
- **Expected**: drop the "index of the host function" tracking in `read the imports`, and collapse `name of the WebAssembly function` to just the `Otherwise` branch's steps unconditionally.
- **Reason**: this branch exists because a host function's `funcinst` used to be shaped `{type, hostcode hostfunc}` — no `module` field — so it couldn't use the module-defined path's `|funcinst|.module`/`|funcaddrs|` lookup, and `read the imports` had to separately track a host function's position among `|imports|` ("index of the host function") to feed the other branch. The underlying Wasm Core Spec's `funcinst` representation has since changed so that both a host function's and a module-defined function's instance carry a `module` field — the branch (and the index-tracking that only existed to feed it) is now dead code the spec was never updated to drop.

## 8. `[=external value|X=]` pipe-aliasing instead of the registered `for`-scoped `[=external value/X=]` form

- **File**: `spectec/document/js-api/index.bs`, lines 513, 529, 533, 538, 560, 561, 565, 566, 570, 571, 575, 576 (12 occurrences across `read the imports` and `create an exports object`)
- **Current**: `[=external value|func=]`, `[=external value|global=]`, `[=external value|mem=]`, `[=external value|table=]` (Bikeshed pipe-display aliasing: linking text `external value`, display text the variant name).
- **Expected**: `[=external value/func=]`, `[=external value/global=]`, `[=external value/mem=]`, `[=external value/table=]` — matching the file's own already-correct `[=external value/tag=]` (lines 544, 581, 582), which links `for`-scoped to `external value` the proper Bikeshed way.
- **Reason**: the file's own link-defaults block (lines 217-220) registers exactly one `for`-scoped sub-term under `external value`: `tag`. It never registers `func`/`global`/`mem`/`table` the same way — unlike the parallel "external-type" block a few lines below (226-233), which correctly registers all 5 of *its* own variants (`func`/`table`/`mem`/`global`/`tag`) `for: external-type`. Without a registered anchor, `[=external value/func=]` wouldn't validly resolve under Bikeshed, so the spec author fell back to pipe-display aliasing for 4 of the 5 "external value" variants, leaving only `tag` — the one that happened to get registered — using the "correct" `for`-scoped form. The real fix is registering the missing anchors in the link-defaults block; absent that, this project instead normalizes the *prose* to the `for`-scoped form for internal consistency (see `SpecPatch` #15), which doesn't by itself make the links resolve in a real Bikeshed build.
