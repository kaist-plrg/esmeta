# Hardcoded Spec Behavior

이 문서는 명세에 실제로 적혀있는 동작이지만, 지금 당장 명세 텍스트로부터 자동으로
기계화(mechanize)하기엔 시간이 오래 걸려서 대신 손으로 하드코딩해 둔 부분들을
기록합니다. 나중에 시간이 될 때 진짜 자동 기계화로 교체하기 위한 목록입니다 —
`docs/spec_errors.md`(명세 자체의 결함)나 `docs/esmeta_errors.md`와는 별개로,
"명세는 맞는데 우리가 아직 자동으로 못 읽어서 대신 손으로 박아넣은 것"만 다룹니다.

## 1. WebIDL dictionary → JavaScript value conversion (`converted_to_a_javascript_value`)

- **File**: `src/main/resources/manuals/funcs/converted_to_a_javascript_value.ir`
- **Spec source**: `webidl/index.bs`, "convert a dictionary to a JavaScript value" (`id="dictionary-to-js"`, around line 8031)
- **What's hardcoded**: The real WebIDL algorithm creates a new ordinary object, then for each present dictionary member, (recursively) converts its value and does `CreateDataPropertyOrThrow`. This project doesn't parse/mechanize WebIDL's generic, per-IDL-type "converted to a JavaScript value" dispatch table at all — instead, this manual `.ir` stub special-cases "if `argument` is a Map (this project's runtime representation for a dictionary-shaped value, e.g. js-api's `«[ "module" → |module|, "instance" → |instance| ]»` literals), build a real object from its entries the same way `CreateDataPropertyOrThrow` would; otherwise return `argument` unchanged" (covering the primitive-passthrough cases already relied on elsewhere, e.g. `resolve`, `get a copy of the buffer source`).
- **Why not mechanized**: WebIDL's real "converted to a JavaScript value" is a large per-IDL-type dispatch table (any/undefined/boolean/number/dictionary/sequence/...) spread across dozens of `<div algorithm>`/`<p>` blocks, with no single named entry point `AlgorithmExtractor`/`SpecFile.webidlFilter` currently resolves generically. Teaching the pipeline to dispatch on a value's *declared IDL type* (not just its runtime shape) is a bigger design problem than time allows right now — the Map-shape heuristic is a stand-in until that's built.
