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

## 7. (retracted) Dead host-function/module-function branch in `name of the WebAssembly function`

Retracted — its premise was wrong. This entry claimed the Wasm Core Spec's `funcinst` representation changed so that a host function's instance now carries a real `module` field, making the host-function branch (and `read the imports`'s "index of the host function" tracking that fed it) dead code. It doesn't: `embedding.rst`'s `func_alloc(store, deftype, hostfunc)` (the actual current Embedding API text) still allocates a host function with an *empty* module instance (`$allocfunc(S, dt, code, {})`) — `funcinst` does uniformly carry a `MODULE` field now, but for a host function that field's *value* is still empty, so the module-defined branch's `|funcinst|.module.funcaddrs` lookup this entry collapsed onto doesn't work for host functions after all. Left as a numbered gap (not renumbering the rest of this file) since other entries and `SpecPatch` cite entries here by number. See `docs/spec_inconsistencies.md` #11 for the actual fix.

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

- **File**: `spectec/document/js-api/index.bs`, lines 1269, 1283, 1323 (`a new Exported Function`, `call an Exported Function`, `run a host function`), and the same problem again at lines 1730, 1759 (`getArg`, `is`) via `[=tag_type=]` instead of `func_type`.
- **Current**: `1. Let [|parameters|] → [|results|] be |functype|.` (and the `[|paramTypes|] → [<var ignore>resultTypes</var>]` variant at line 1269); `1. Let [|types|] → [] be [=tag_type=](|store|, ...).` at 1730/1759.
- **Expected**: `1. Let [=comp-type/func=] |parameters| → |results| be [=expand=](|functype|).`; `1. Let [=comp-type/func=] |types| → « » be [=expand=]([=tag_type=](|store|, ...)).` (the `[=comp-type/func=]` prefix and dropped `[...]` decoration are #18's fix, applied here too since both land on the same text).
- **Reason**: `|functype|` here is a `deftype` — either `func_type`'s own return value, or (for `run a host function`) the payload of an imported function's externtype, itself resolved to a `deftype` by `module_imports` (see `spectec/spectec/src/backend-interpreter/embedding.ml`'s `module_imports`, which resolves it via the `$Module_ok` relation). Per the Wasm Core Spec's Embedding API (`embedding.rst`'s `func_type` post-condition: "the returned defined type ... expands to a function type"), a `deftype` is only a `params -> results` comptype *after* the `$Expand` relation (`2.1-validation.types.spectec`) is applied to it — the caller's responsibility, not something `func_type` (or any embedding function returning a `deftype`) does automatically. `tag_type` has the exact same post-condition shape (`embedding.rst`: "Return `S.TAGS[a].TYPE`", the returned `tagtype` is a `deftype` too), so `getArg`/`is` destructuring its result the same unexpanded way is the identical bug at two more call sites — not caught by this entry's own `SpecPatch` fix (#14) since that one matches the literal text `"be |functype|."`, which `"be [=tag_type=](...)."` doesn't contain. Every occurrence skips the expand step and destructures the deftype directly, silently projecting into the wrong fields (e.g. a recursive-type wrapper's own inner fields, instead of the function's actual parameter/result lists) whenever the value isn't already flat. Made explicit via a new `expand` convenience `wjmeta-bridge`/`spectec-server` exposes, wrapping the same `$Expand` relation (`spectec/spectec/src/backend-interpreter/embedding.ml`'s `expand`, backed by `Relation.expand`, already implemented in `relation.ml`).

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

## 16. `!` (ReturnIfAbrupt) applied to ECMA-262 abstract operations that never return a Completion Record (`OrdinaryObjectCreate`, `CreateBuiltinFunction`)

- **File**: `spectec/document/js-api/index.bs`
  - lines 473, 555, 1885 (`OrdinaryObjectCreate`, `read the imports`/instantiation-exports-object construction sites)
  - line 1272 (`CreateBuiltinFunction`, `a new Exported Function`)
- **Current**:
  - `1. Let |exportsObject| be [=!=] [$OrdinaryObjectCreate$](null).` (3 sites, byte-identical)
  - `1. Let |function| be [=!=] [$CreateBuiltinFunction$](|steps|, |arity|, |name|, « [[FunctionAddress]] », |realm|).`
- **Expected**: drop the `[=!=]` prefix at all 4 sites.
- **Reason**: same class of mistake as spec error #14's `ToJSValue`. ECMA-262's own "ReturnIfAbrupt Shorthands" (`sec-returnifabrupt-shorthands`) defines `!` as unconditionally asserting its operand is a normal completion before unwrapping `.[[Value]]` — not a graceful no-op when the operand isn't a Completion Record at all. Both `OrdinaryObjectCreate` (10.1.12, "returns an Object") and `CreateBuiltinFunction` (10.3.3, "returns a built-in function object") declare non-completion return types, and every step in either body is `Let`/`Set`/`Perform`/`Return` only — no `Throw`/`NormalCompletion`/`ThrowCompletion` anywhere — so neither can fail nor ever produces a Completion Record; `!` should never have been written before either. (Every other `CreateBuiltinFunction` call site in this project's corpus — `webidl/index.bs`, ~40 occurrences — correctly omits `!`; this is the sole exception for that operation.)

## 17. `tag_alloc(|store|, X → Y)` passes a bare comptype where the Embedding API expects a `deftype`

- **File**: `spectec/document/js-api/index.bs`, lines 1559 (`Tag(type)` constructor), 1798 (`get the JavaScript exception tag`).
- **Current**: `1. Let (|store|, |tagAddress|) be [=tag_alloc=](|store|, |wasmParameters| → « »).` and `1. Let (|store|, |tagAddress|) be [=tag_alloc=](|store|, « [=externref=] » → « »).`
- **Expected**: `1. Let (|store|, |tagAddress|) be [=tag_alloc=](|store|, [=fold=]([=comp-type/func=] |wasmParameters| → « »)).` and `1. Let (|store|, |tagAddress|) be [=tag_alloc=](|store|, [=fold=]([=comp-type/func=] « [=externref=] » → « »)).` (the `[=comp-type/func=]` prefix is #18's fix, applied here too since both land on the same text).
- **Reason**: the mirror-image mistake of #9, in the opposite direction. `X → Y` (SpecTec's comptype-arrow notation, `al_of_comptype`'s `FuncT (rt1, rt2) -> CaseV ("->", ...)`) builds a bare `comptype`, but `tag_alloc(store, tagtype) : (store, tagaddr)` (`embedding.rst`) declares its second parameter `tagtype`, and `tagtype = typeuse` (`1.2-syntax.types.spectec:130`) is only ever a resolved `deftype` at this level, never a bare comptype — the same "caller must convert first, the embedding function doesn't do it for you" contract as #9's `func_type`/`tag_type`, just on the construction side instead of the destructuring side. Neither call site ever builds a real `deftype` first. Made explicit via a new `fold` convenience (the construction-direction mirror of `expand`, `spectec/spectec/src/backend-interpreter/embedding.ml`) wrapping the bare comptype into a fresh, standalone (non-recursive, final, no supertypes) `deftype` — `_DEF (REC [SUB (some FINAL) [] comptype]) 0`, hand-built rather than routed through the mechanized `$rolldt`/`$rollrt` (`1.2-syntax.types.spectec:458-469`) since their only real work (substituting a rec group's internal self-references into absolute indices) is a no-op for a lone, non-self-referential type — every type either call site here ever builds.

## 18. Comptype arrow notation (`[X] → [Y]` / `X → Y`) omits the `FUNC` discriminator its own current grammar requires

- **File**: `spectec/document/js-api/index.bs`, the same 7 sites as #9 (1269, 1283, 1323) and #17 (1559, 1730, 1759, 1798) — every occurrence of the comptype-arrow notation in this document.
- **Current**: `[|parameters|] → [|results|]` / `X → Y` (no discriminator).
- **Expected**: `[=comp-type/func=] |parameters| → |results|` / `[=comp-type/func=] X → Y`.
- **Reason**: `.spectec`'s current `comptype` grammar (`1.2-syntax.types.spectec:114-117`) is `comptype ::= STRUCT list(fieldtype) | ARRAY fieldtype | FUNC resulttype -> resulttype` — three variants, each with its own leading keyword (`STRUCT`/`ARRAY`/`FUNC`) to tell them apart, since `comptype` covers all three (added by the GC proposal). Every arrow occurrence in this document predates that: `comptype` didn't exist as a category until GC introduced struct/array types alongside func types, so a functype's own arrow notation never needed a discriminator to begin with — nothing else it could have been confused with. The prose was never updated when the grammar it's implicitly quoting grew two more variants, so it now writes a *comptype* using syntax that's only valid for the specific case where every reader already knows (from context) it must be the `FUNC` variant — technically no longer valid `comptype` surface syntax on its own. Fixed at the same 7 sites #9/#17 already patch for the deftype↔comptype conversion gaps, since all 7 land on this same underlying notation; `NormalizeSpecTecCaseShapePass.RenamedTag`'s `"FUNC" -> "->"` entry keeps the *runtime* representation exactly as it already was (`al_of_comptype`'s `FuncT (rt1, rt2) -> CaseV ("->", ...)` — the runtime tag for a func-shaped comptype has always been `"->"` itself, never `"FUNC"`) — this fix is purely about the *surface syntax* being valid against its own current grammar, not a runtime-representation change. `[=comp-type/func=]` (the `for: comp-type` scoped form the real anchors block would need, mirroring how `[=heap-type/func=]`/`[=external-type/func=]` are already scoped for their own sections) is technically still a dangling link — no `for: comp-type` anchor block actually exists in this document — but left as-is rather than also adding one, since this project never runs real Bikeshed rendering and the link text alone is enough for `ExprParser`/`NormalizeSpecTecCaseShapePass` to recognize and normalize it correctly.

## 19. `AddressValueToU64` passes an already-IDL-converted value into `ConvertToInt`/`ToBigInt`, both defined over a JavaScript value

- **File**: `spectec/document/js-api/index.bs`, lines 1484-1497 (`AddressValueToU64`)
- **Current**:
  ```
  The algorithm AddressValueToU64(|v|, |addrtype|) converts a JavaScript value to a WebAssembly u64 ..., by performing the following steps:

  1. If |addrtype| is i32,
      1. Let |n| be ?ConvertToInt(|v|, 32, "unsigned"), where the destination type is associated with [EnforceRange].
      ...
  1. If |addrtype| is i64,
      1. Let |n| be ?ToBigInt(|v|).
      ...
  ```
- **Expected**: insert one step right after the head sentence, before either branch:
  ```
  1. Set |v| to |v|, [=converted to a JavaScript value=].
  1. If |addrtype| is [=i32=],
      1. Let |n| be [=?=] [$ConvertToInt$](|v|, 32, "unsigned"), where the destination type is associated with [=[EnforceRange]=].
      ...
  1. If |addrtype| is [=i64=],
      1. Let |n| be [=?=] [$ToBigInt$](|v|).
      ...
  ```
  (both branches' own lines are otherwise unchanged — they already read `|v|`, which the hoisted step now guarantees is a genuine JavaScript value by the time either branch runs)
- **Reason**: `AddressValueToU64(|v|, |addrtype|)` is called from `Table.prototype.get`/`.set`/`.grow`, `Memory`/`Table` constructors and `.grow`, etc. with `|index|`/`|delta|`/`|descriptor|["initial"|"maximum"]` — each one bound by the *method steps* of some WebIDL operation. Per `webidl/index.bs:12562-12570` ("create an operation function"), method steps always run with `|values|` — the result of the *overload resolution algorithm*, i.e. every argument already [=converted to an IDL value=] — never the raw incoming arguments. Each of these parameters' declared IDL type is `AddressValue`, a `typedef any` (`webidl/index.bs:687`), and converting a JavaScript value to `any` is a real, defined conversion (`webidl/index.bs:7314-7339`) that produces an IDL value (an IDL unrestricted double / bigint / DOMString / object / ... depending on the argument's ECMAScript type) — not the JavaScript value itself. Both `ConvertToInt` (`webidl/index.bs:7604`, used everywhere else in the document exclusively as an internal step of the "convert a JavaScript value to `unsigned long`/`long`/..." family, e.g. line 7523) and `ToBigInt` (ECMA-262) are defined over a genuine JavaScript value — so the i64 branch has the exact same problem as the i32 branch, not just the one this entry originally flagged. `webidl/index.bs`'s own §3.2 ("JavaScript type mapping") treats "JavaScript value" and "IDL value" as distinct types precisely so that each direction of conversion between them needs to be named and applied explicitly; `AddressValueToU64` skips the IDL-value-to-JavaScript-value direction and feeds `|v|` to both `ConvertToInt` and `ToBigInt` as-is. In practice this happens not to change either operation's observable behavior (every branch of the `any` conversion is either a no-op passthrough or a representation-preserving wrap), but the spec text itself no longer honors the type distinction it defines and relies on elsewhere.

## 20. `Tag`/`Exception`'s `Exposed=(Window,Worker,Worklet)` makes the unconditional `Assert: |interface| is [=exposed=] in |realm|` in "internally create a new object implementing the interface" fail for any non-browser host

- **File**: `spectec/document/js-api/index.bs`, lines 1519 (`Tag`) and 1678 (`Exception`)
- **Current**: `[LegacyNamespace=WebAssembly, Exposed=(Window,Worker,Worklet)]`
- **Expected**: `[LegacyNamespace=WebAssembly, Exposed=*]` — the form every other interface in the `WebAssembly` namespace uses, including the namespace itself (`WebAssembly` line 369, `Module` line 701, `Instance` line 777, `Memory` line 809, `Table` line 1003, `Global` line 1139 — all `Exposed=*`).
- **Reason**: this is not just a stylistic outlier — it makes a real assertion unsatisfiable for a real, spec-intended host. `webidl/index.bs`'s "internally create a new object implementing the interface" (line 13827) begins with `1. Assert: |interface| is [=exposed=] in |realm|.`, and this algorithm runs every time a `Tag`/`Exception` JS object is materialized — not only via the `new Tag(...)`/`new Exception(...)` constructors, but via `create a Tag object`/`create an Exception object` (`js-api/index.bs:1539`/`1708`, reached whenever a tag/exception import or export crosses the wasm/JS boundary). `exposed` (`webidl/index.bs:10052`, `dfn-exposed`) returns `false` whenever a construct's exposure set is not `*` and `|realm|.[[GlobalObject]]` does not implement one of the listed interfaces. Since `WebAssembly` and every other interface in its namespace are deliberately `Exposed=*` — precisely because the WebAssembly JS API is designed to run in any ECMAScript host, not only browsers — restricting `Tag`/`Exception` to `(Window,Worker,Worklet)` means the `Assert` above genuinely fails whenever a `Tag`/`Exception` object is created in a realm whose global object isn't one of those three browser globals, e.g. any standalone/embedded JS engine hosting WebAssembly (including WJI itself). There is no comment or rationale anywhere near either interface explaining a narrower exposure than the rest of the namespace; this reads like a `(Window,Worker,Worklet)` boilerplate carried over from a generic browser DOM API template when the exception-handling proposal added `Tag`/`Exception`, never reconciled with the namespace's host-agnostic design. Because exception handling (`try_table`/`throw`, etc.) is core Wasm language functionality rather than a browser-specific feature, this is a real defect in the spec text, not a benign local deviation from an established idiom.

## 21. "shortest argument list" names a field that doesn't exist — the entry field being minimized is the `type list`, not an "argument list"

- **File**: `webidl/index.bs`, three sites: lines 11977-11978 (`create an interface object`), 12028 (`create a legacy factory function`), 12584 (`creating an operation function`).
- **Current**:
  ```
  1.  Set |length| to the length of the
      shortest argument list of the entries in |S|.
  ```
  (lines 11977-11978)
  ```
  1.  Let |length| be the length of the shortest argument list of the entries in |S|.
  ```
  (line 12028)
  ```
  1.  Let |length| be the length of the shortest argument list in the entries in |S|.
  ```
  (line 12584)
- **Expected**: all three with "argument list" replaced by "type list", matching the already-correct sibling step computing the same projection in the other direction: `1.  Let |maxarg| be the length of the longest type list of the entries in |S|.` (line 11529, "Overload resolution algorithm").
- **Reason**: an "effective overload set" entry (an element of `|S|`) is formally defined, once, as the tuple `([=effective overload set tuple/callable=], [=type list=], [=optionality list=])` (line 3157), with `A <dfn>type list</dfn> is a [=list=] of IDL types` (line 3164). Nothing in this document ever defines an "argument list" as a field of that tuple, or as a term at all in this context — these three sites are the only places in the whole document that call this projection an "argument list"; every other reference to it, including the `|maxarg|` step at line 11529 computing the same projection's *longest* value for the same purpose, correctly calls it a "type list". This isn't just inconsistent phrasing to fix for uniformity — "argument list" actively names the wrong concept: "compute the effective overload set" (lines 3179-3256) itself distinguishes the two clearly. It first binds |arguments| to "the [=list=] of arguments |X| is declared to take" (line 3220) — that's the genuine argument list, the operation's own declared parameters — and then, separately, builds |types| ("a [=type list=]", line 3222) by appending "the type of |argument|" for each |argument| in |arguments| (line 3225), before storing |types| (not |arguments|) as the tuple's second element (line 3229). So the value each of the three sites above is minimizing is the length of |types| — the type list — never |arguments| itself; calling it an "argument list" invites confusion with the very value the algorithm just took care to derive it from and name differently. (Line 12028's algorithm, `create a legacy factory function`, isn't currently in `SpecFile.webidlFilter`, so it isn't extracted/compiled by this project today — included here anyway since it's the same defect, worth reporting alongside the other two.)

## 22. `ToWebAssemblyValue`'s host-value-cache hit skips the type check every other branch goes through

- **File**: `spectec/document/js-api/index.bs`, `ToWebAssemblyValue`, lines 1467-1477 (the "ref null heaptype" case's final `Else` branch and its shared tail)
- **Current**:
  ```
  1. Else,
      1. Let |map| be the [=surrounding agent=]'s associated [=host value cache=].
      1. If a [=host address=] |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|,
          1. Return [=ref.host=] |hostaddr|.
      1. Let [=host address=] |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.
      1. [=map/Set=] |map|[|hostaddr|] to |v|.
      1. Let |r| be [=ref.host=] |hostaddr|.
  1. Let |store| be the [=surrounding agent=]'s [=associated store=].
  1. Let |actualtype| be [=ref_type=](|store|, |r|).
  1. If [=match_valtype=](|actualtype|, |type|) is false,
      1. Throw a {{TypeError}}.
  1. Return |r|.
  ```
- **Expected**: the cache-hit branch should bind `|r|` and fall through to the same `ref_type`/`match_valtype` check every other branch (`ref.null`, `ref.extern`, `ref.func`, `ref.i31`, `ref.struct`/`ref.array`, and the cache-*miss* half of this very `Else`) already goes through, not return early:
  ```
  1. If a [=host address=] |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|,
      1. Let |r| be [=ref.host=] |hostaddr|.
  1. Else,
      1. Let |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.
      1. [=map/Set=] |map|[|hostaddr|] to |v|.
      1. Let |r| be [=ref.host=] |hostaddr|.
  ```
- **Reason**: every other way of producing an `r` in this algorithm — including the cache-*miss* half of this exact `Else` branch, three steps below the buggy one — falls through to the shared `ref_type`(`store`, `r`)/`match_valtype`(`actualtype`, `type`) check before ever returning, which is what makes converting a value to, say, `eqref` reject an object that isn't actually eq-castable. The cache-hit branch is the one exception: it `Return`s `ref.host hostaddr` immediately, so a value's *first* successful conversion (under whatever type it was converted to *then*) gets remembered in the cache, and every later conversion of that same value — even to a completely different, narrower type — short-circuits straight past the type check and returns the same cached ref.host, regardless of whether it's actually valid for the new target type. Concretely: converting a BigInt to `anyref` succeeds (nothing else matches it, so it falls to this `Else` and is cached as a fresh `ref.host`; `anyref` accepts anything, so `match_valtype` passes). Converting that *same* BigInt to `eqref` immediately afterward should throw a `TypeError` (a host reference doesn't satisfy `eq`) — and does, if it's the first time. But run the `anyref` conversion first, and the second (`eqref`) conversion finds the cached entry, hits the early `Return`, and never reaches `match_valtype` at all — the invalid `ref.host` value sails through into `func_invoke` (or wherever it's headed) unchecked. There, the Wasm Core Spec's own runtime type check on the actual call boundary (not this algorithm) is what finally catches the mismatch — but by raising `Exception.Fail` internally, in a way `spectec`'s `backend-server`/`backend-interpreter` doesn't cleanly surface as a JS-observable error (an uncaught internal exception rather than the intended `TypeError`). Fixed via `SpecPatch` #46: reword the cache-hit branch's `Return` to `Let |r| be ...`, and wrap the cache-miss branch's three steps in an `Else,` (matching this same algorithm's own "If ... Else if ... Else," idiom a few steps up) so exactly one of the two branches runs and both join the shared tail.
