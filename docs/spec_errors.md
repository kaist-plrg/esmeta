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
- **Reason**: `[=map/exist|contains=]` links to the `map/exist` dfn but *displays* "contains" (Bikeshed's `|display-text=]` aliasing), and the sentence around it — `X contains Y` — is exactly WebIDL/Infra's "list contains" idiom, not the map-membership idiom used everywhere else in this file (`base[key] exists`). Per the Infra Standard, `contains` is only defined for lists — maps have no such operation — so this isn't just stylistically off, it names an operation that doesn't exist for `|builtinOrStringImports|`'s actual (map) type. This reads like a leftover from when `builtinOrStringImports` was a list: spec error #1 already shows its empty-literal is still written `« »` (the *list* notation) instead of `«[ ]»` (the *ordered map* notation), i.e. the same variable's declaration has the same kind of leftover. It looks like only the link target was updated from `list/contains` to `map/exist` when the type changed from list to map, without updating the surrounding sentence to the map idiom.

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

## 8. `is of the form [=external-type/tag=] |attribute| ...` binds a field the runtime representation no longer has

- **File**: `spectec/document/js-api/index.bs`, lines 540-541 and 579-580 (`read the imports` and `create an exports object`)
- **Current**:
  ```
  1. If |externtype| is of the form [=external-type/tag=] |attribute| <var ignore>functype</var>,
      1. Assert: |attribute| is [=tagtype/attribute/exception=].
  ```
- **Expected**: `1. If |externtype| is of the form [=external-type/tag=] <var ignore>functype</var>,` (drop the `|attribute|` binding and the Assert that follows it).
- **Reason**: `al_of_tagtype`/`TagT` (`spectec/spectec/src/backend-interpreter/construct.ml:1198-1199`) wrap a tag's typeuse directly, with no separate attribute-kind field — "exception" is still the only tag attribute this proposal defines, so the runtime representation dropped the field entirely. The very next step only asserts `|attribute|` always equals that one constant, then never uses it again anywhere in either branch — a vestigial binding left over from before the representation change, the same pattern as #7's dead host-function branch.

## 9. `Let [|parameters|] → [|results|] be |functype|.` destructures a `deftype` as if it were already its own comptype

- **File**: `spectec/document/js-api/index.bs`, lines 1269, 1283, 1323 (`a new Exported Function`, `call an Exported Function`, `run a host function`)
- **Current**: `1. Let [|parameters|] → [|results|] be |functype|.` (and the `[|paramTypes|] → [<var ignore>resultTypes</var>]` variant at line 1269).
- **Expected**: `1. Let [|parameters|] → [|results|] be [=expand=](|functype|).`
- **Reason**: `|functype|` here is a `deftype` — either `func_type`'s own return value, or (for `run a host function`) the payload of an imported function's externtype, itself resolved to a `deftype` by `module_imports` (see `spectec/spectec/src/backend-interpreter/embedding.ml`'s `module_imports`, which resolves it via the `$Module_ok` relation). Per the Wasm Core Spec's Embedding API (`embedding.rst`'s `func_type` post-condition: "the returned defined type ... expands to a function type"), a `deftype` is only a `params -> results` comptype *after* the `$Expand` relation (`2.1-validation.types.spectec`) is applied to it — the caller's responsibility, not something `func_type` (or any embedding function returning a `deftype`) does automatically. Every occurrence in this file skips that step and destructures `|functype|` directly, silently projecting into the wrong fields (e.g. a recursive-type wrapper's own inner fields, instead of the function's actual parameter/result lists) whenever `|functype|` isn't already flat. Made explicit via a new `expand` convenience `wjmeta-bridge`/`spectec-server` exposes, wrapping the same `$Expand` relation (`spectec/spectec/src/backend-interpreter/embedding.ml`'s `expand`, backed by `Relation.expand`, already implemented in `relation.ml`).

## 10. `|moduleinst|.funcaddrs` names a field the runtime `moduleinst` record doesn't have

- **File**: `spectec/document/js-api/index.bs`, lines 1254-1255 (`name of the WebAssembly function`)
- **Current**: `1. Assert: |funcaddr| is contained in |moduleinst|.funcaddrs.` and `1. Let |index| be the index of |moduleinst|.funcaddrs where |funcaddr| is found.`
- **Expected**: `1. Assert: |funcaddr| is contained in |moduleinst|.funcs.` and `1. Let |index| be the index of |moduleinst|.funcs where |funcaddr| is found.`
- **Reason**: the Wasm Core Spec's actual runtime `moduleinst` record names its function-address list `FUNCS`, not `funcaddrs` — confirmed directly off a live decoded module instance (its printed record has `TYPES`/`TAGS`/`GLOBALS`/`MEMS`/`TABLES`/`FUNCS`/`DATAS`/`ELEMS`/`EXPORTS` fields, no `FUNCADDRS`). `funcaddrs` reads like a plausible field name (mirroring `funcaddr`, the element type) but doesn't correspond to anything in the actual representation.

## 11. Steps 2 and 4 of `react` end with a colon instead of a period

- **File**: `webidl/index.bs`, lines 8678 and 8686 (`react` to a Promise)
- **Current**:
  ```
  1.  Let |onFulfilled| be [$CreateBuiltinFunction$](|onFulfilledSteps|, 1, "", « »):
  ```
  and
  ```
  1.  Let |onRejected| be [$CreateBuiltinFunction$](|onRejectedSteps|, 1, "", « »):
  ```
- **Expected**: both steps should end with a period (`.`), not a colon:
  ```
  1.  Let |onFulfilled| be [$CreateBuiltinFunction$](|onFulfilledSteps|, 1, "", « »).
  ```
  and
  ```
  1.  Let |onRejected| be [$CreateBuiltinFunction$](|onRejectedSteps|, 1, "", « »).
  ```
- **Reason**: a trailing colon immediately followed by an indented step list reads as a typo for a period here — steps 2 and 4 are plain, self-contained `Let` bindings with no substeps of their own (unlike steps 1 and 3, `Let |onFulfilledSteps|/|onRejectedSteps| be the following steps ...:`, which genuinely do introduce indented substeps) — the next line at each point is a new top-level step (3 and 5 respectively), not a continuation. Ending them with `:` instead of `.` misleadingly suggests each introduces steps that were never written.

## 12. `|options|["builtins"]` / `|options|["importedStringConstants"]` map-indexed without accounting for the WebIDL default empty dictionary

- **File**: `spectec/document/js-api/index.bs`, lines 421-422 (`validate`) and 453-454 (`asynchronously compile a WebAssembly module`, reached from both `compile()` and `WebAssembly.instantiate(bytes, importObject, options)`)
- **Current**:
  ```
  1. Let |builtinSetNames| be |options|["builtins"].
  1. Let |importedStringModule| be |options|["importedStringConstants"].
  ```
- **Expected**: something that accounts for the keys being absent, e.g. `1. Let |builtinSetNames| be |options|["builtins"] if it [=map/exists=], otherwise an empty list.` and `1. Let |importedStringModule| be |options|["importedStringConstants"] if it [=map/exists=], otherwise null.` — matching the `|map|[|key|] [=map/exists=]` idiom used everywhere else map membership is checked in this file (see spec error #6).
- **Reason**: `options`'s IDL type, `WebAssemblyCompileOptions` (lines 364-367), declares `importedStringConstants` and `builtins` as plain optional members with no `=` default value. Per `webidl/index.bs` lines 4654-4657, only members that are `required` or carry an explicit default value are guaranteed a corresponding entry when a dictionary value is converted to an ordered map — other optional members may or may not have an entry, depending on what the caller actually supplied. The IDL also defaults the `options` argument itself to `{}` when omitted (line 371-375, `optional WebAssemblyCompileOptions options = {}`), which is the common case for calls like `WebAssembly.instantiate(bytes, importObject)` or `WebAssembly.compile(bytes)`. Converting that empty `{}` yields a map with *no* entries for either key, yet `|options|["builtins"]`/`|options|["importedStringConstants"]` index into it unconditionally, with no `[=map/exists=]` guard — unlike every other map-key access in this file (spec error #6). The result is that the algorithm is unhandled precisely for its most common call pattern, where no options object (or an options object missing one of these two properties) is supplied at all.

## 13. `[=exception=]` ambiguous between two anchors registered in the same `<pre class=anchors>` block

- **File**: `spectec/document/js-api/index.bs`, line 1297 (`call an Exported Function`'s use of `[=exception=]`); the colliding registrations: lines 165 and 248, in the same `<pre class="anchors">` block (starting line 37)
- **Current**: two separate `text: exception` entries are registered in the same anchors block:
  ```
  text: exception; url: appendix/embedding.html#embed-error          (line 165, unscoped)
  text: exception; for: tagtype/attribute; url: syntax/types.html#syntax-tagtype   (line 248, `for: tagtype/attribute`)
  ```
  Confirmed by actually running `bikeshed spec` on this file, which reports at line 1297:
  ```
  Multiple possible 'exception' dfn refs.
  Arbitrarily chose https://webassembly.github.io/spec/core/appendix/embedding.html#embed-error
  spec:webassembly; type:dfn; for:/; text:exception
  spec:webassembly; type:dfn; for:tagtype/attribute; text:exception
  ```
- **Expected**: disambiguate the bare `[=exception=]` at line 1297 — e.g. `[=exception|exception=]` with an explicit `for`-scope, or add a `<pre class=link-defaults>` entry selecting `for:/` for this text — so the choice isn't left to Bikeshed's arbitrary tie-breaking.
- **Reason**: not a missing/dead link (Bikeshed does resolve it, to `#embed-error`, the section covering both `embedding.rst`'s `exception`/`error` productions) — the actual problem is that two independently-registered anchors share the exact same linking text ("exception") within the same anchors block, one unscoped (line 165, meant for the embedding-API production `[=exception=]` at line 1297 needs) and one `for`-scoped to `tagtype/attribute` (line 248, meant for `[=tagtype/attribute/exception=]`, used at lines 541/580). Bikeshed currently happens to pick the right one, but says so explicitly as an arbitrary, non-deterministic choice — a small unrelated edit elsewhere in the file could flip it to the wrong candidate with no visible change at the call site itself.

## 14. `ToJSValue` is called with the `!` (ReturnIfAbrupt) shorthand even though it never returns a Completion Record

- **File**: `spectec/document/js-api/index.bs`, lines 1096, 1214, 1302, 1308, 1312, 1327, 1762 (`ReadTableElement` (name unconfirmed — the table-read algorithm), `GetGlobalValue`, `call an Exported Function` ×3, `run a host function`, and the exception-payload read at line 1762)
- **Current**: e.g. `1. Otherwise, if |outArity| is 1, return [=!=] [=ToJSValue=](|ret|[0]).` (line 1308) — and likewise at the other 6 sites, each prefixing the `ToJSValue` call with `[=!=]`.
- **Expected**: drop the `[=!=]` prefix at all 7 sites, e.g. `1. Otherwise, if |outArity| is 1, return [=ToJSValue=](|ret|[0]).`
- **Reason**: the `!` shorthand (imported from ECMA-262's own "ReturnIfAbrupt Shorthands" convention) presupposes its target returns a Completion Record whose `[[Value]]` gets unwrapped. `ToJSValue`'s own definition (lines 1376-1402) never does: all 12 of its steps are bare `Return`/`If ... return` statements — no `Throw`, `NormalCompletion`, or `ThrowCompletion` anywhere in its body. Its own single self-recursive call (line 1402, the `ref.extern` case) correctly invokes it without `!`, confirming the omission elsewhere is the mistake, not this one. The other 5 occurrences of `ToJSValue` in the document (lines 1934, 1943, 2048, 2080, 2123) are unaffected: there, `!` targets `$Call$` (which *is* completion-returning), with `ToJSValue(...)` merely passed in as a plain argument.

## 15. `[=ref.null=]` still written with a heap-type argument, a shape the current Wasm Core Spec's `ref` value grammar no longer has

- **File**: `spectec/document/js-api/index.bs`, lines 1394, 1449 (`ToJSValue`'s pattern match, `ToWebAssemblyValue`'s construction)
- **Current**: `1. If |w| is of the form [=ref.null=] <var ignore>t</var>, return null.` and `1. Let |r| be [=ref.null=] |heaptype|.`
- **Expected**: `1. If |w| is of the form [=ref.null=], return null.` and `1. Let |r| be [=ref.null=].` — drop the now-nonexistent argument at both sites.
- **Reason**: `[=ref.null=]` links to `exec/runtime.html#values` (the Core Spec's runtime `ref` value grammar), the same target every sibling `[=ref.X=]` link in this file uses (`ref.func`/`ref.host`/`ref.i31`/`ref.struct`/`ref.array`/`ref.extern`) — and for every one of those siblings, the written argument genuinely is part of the produced value (confirmed against `4.1-execution.values.spectec`, e.g. `s |- REF.FUNC_ADDR a : REF dt`). `ref.null` is the sole exception: its current runtime grammar is a bare nullary `REF.NULL_ADDR`, always typed as the bottom heap type regardless of context (`s |- REF.NULL_ADDR : REF NULL BOT`) — an older Wasm Core Spec revision *did* carry the heap type as part of the null value itself, and this phrasing was never updated when that representation changed, leaving behind an argument the current value grammar has no room for.
