# SpecTec Errors

`spectec` 서브모듈에서 발견한, **mainline spectec 자체에 이미 있던**(이 프로젝트가 새로 추가한 게 아닌) 에러 목록입니다 — 스펙 텍스트 자체의 결함(`docs/spec_errors.md` 소관)은 제외하고, 그 나머지 전부: `spectec/spectec/src/*`(`xl/`, `il2al/`, `backend-interpreter/ds.ml`/`numerics.ml` 등 core AL 인터프리터/컴파일러)의 OCaml 구현 버그든, `spectec/test/js-api/` 등 벤더링된 테스트 코퍼스 자체의 버그(오타, 잘못된 헬퍼 함수 등)든 여기 기록합니다.

**범위 밖(중요)**: `spectec/spectec/src/backend-interpreter/embedding.ml`/`relation.ml`/`server.ml` — 이 세 파일은 `official`(업스트림 `WebAssembly/spec`) 브랜치엔 아예 존재하지 않는, 이 프로젝트가 JS-API embedding 브릿지용으로 직접 새로 작성한 코드입니다. 여기서 발견되는 버그는 mainline spectec의 결함이 아니라 **우리 자신의 구현 실수**라서 이 문서 대상이 아닙니다(문서화해봐야 남 탓할 게 없는, 그냥 우리 커밋 이력으로 충분한 내용). 한때 `mem_grow`/`table_grow`/`module_instantiate`(전부 `embedding.ml`)의 버그를 여기 잘못 기록했다가 삭제한 적이 있으니(`9fdab2c9`), 헷갈리면 "이 파일이 `official` 브랜치에도 있는가"부터 확인할 것.

## 1. (moved) `` `Int ``만 받고 `` `Nat ``은 거부하는 지점들이 `xl/num.ml`이 선언한 subtype 관계를 어김

`docs/hardcodes.md` #40으로 옮김. 업스트림에 PR(WebAssembly/spec#2332, #2와 함께)을 냈으나, IL에서 `nat`과 `int`는 하위 타입 관계가 아니고 변환을 명시적으로 넣는 게 의도된 설계라는 답을 받고 닫힘(2026-11-12). 문제는 SpecTec이 아니라 ESMeta 쪽 `toAL`이 IL 타입과 상관없이 `` `Int ``로 태깅하는 데 있고, 포크의 수정은 그걸 덮는 우회책. 번호 유지를 위해 남겨 둠.

## 3. `WasmContext.is_value`의 `REF.`-prefix 체크가 아직 안 풀린 ref 생성 명령어를 이미 값인 것처럼 오판

> **업스트림 버그로 세지 않음 (2026-11-12)**: 업스트림 `WebAssembly/spec` `main`에서 `is_value`는 `push_value`의 안전 검사에만 쓰이고, `step_wasm`은 `CONST`/`VCONST`만 값으로 직접 고른 뒤 나머지(`REF.FUNC` 같은 명령어 포함)를 `create_context`로 보내 정상적으로 reduction함. 그래서 업스트림에서는 관찰되는 잘못된 동작이 없고, 이 프로젝트의 커밋 `3f199fb74`(`fork/embedding`에만 있음)가 `step_wasm`의 값 판정을 `is_value` 기반으로 바꾸면서 처음 드러남. `REF.` 접두사 검사가 목적(값 판정)에 비춰 부정확한 건 맞지만, 값 태그 목록은 `il2al/translate.ml`의 `is_wasm_value`에도 따로 하드코딩돼 있어 근본 해법은 명세의 `val` 정의에서 목록을 뽑는 구조 개선이고, 아래 Expected는 Wasm 2.0 이하의 값 `REF.NULL`을 빠뜨려 업스트림 `spec-test-2` 테스트를 깨뜨림. 그래서 업스트림에 제보하지 않고, 이 프로젝트의 포크 쪽 수정 기록으로만 남김.

- **File**: `spectec/spectec/src/backend-interpreter/ds.ml`, `WasmContext.is_value`
- **Current**:
  ```ocaml
  let is_value = function
    | CaseV ("CONST", _) -> true
    | CaseV ("VCONST", _) -> true
    | CaseV (ref, _)
      when String.starts_with ~prefix:"REF." ref -> true
    | _ -> false
  ```
- **Expected**:
  ```ocaml
  let is_value = function
    | CaseV ("CONST", _)
    | CaseV ("VCONST", _)
    | CaseV ("REF.NULL_ADDR", _)
    | CaseV ("REF.I31_NUM", _)
    | CaseV ("REF.STRUCT_ADDR", _)
    | CaseV ("REF.ARRAY_ADDR", _)
    | CaseV ("REF.FUNC_ADDR", _)
    | CaseV ("REF.EXN_ADDR", _)
    | CaseV ("REF.HOST_ADDR", _)
    | CaseV ("REF.EXTERN", _) -> true
    | _ -> false
  ```
- **Reason**: `construct.ml`의 `al_of_ref`/`al_to_ref`를 보면 ref 관련 `CaseV` 태그는 **명령어(아직 안 풀린 구문)**와 **값(resolve된 런타임 레퍼런스)**이 항상 짝을 이룹니다 — `REF.FUNC`(`ref.func $idx` 명령어, 모듈 상대 함수 인덱스)/`REF.FUNC_ADDR`(resolve된 funcaddr), `REF.STRUCT`/`REF.STRUCT_ADDR`, `REF.ARRAY`/`REF.ARRAY_ADDR`, `REF.I31`/`REF.I31_NUM`, `REF.EXN`/`REF.EXN_ADDR`, `REF.HOST`/`REF.HOST_ADDR`(6쌍 전부 `NormalizeSpecTecCaseShapePass.scala`의 `RenamedTag` 매핑과 정확히 일치). `is_value`의 `String.starts_with ~prefix:"REF."` 체크는 이 둘을 전혀 구분 못 하고 — 명령어 태그도 값 태그도 똑같이 `"REF."`로 시작하니까요 — 아직 reduce가 안 된 `ref.func $idx` 같은 명령어를 이미 값인 것처럼 판단해 `step_wasm`이 `create_context`(실제 reduction 실행)를 안 태우고 그냥 값 스택에 그대로 push해버립니다. 결과적으로 `4.3-execution.instructions.spectec`의 진짜 reduction rule(`z; (REF.FUNC x) ~> (REF.FUNC_ADDR $moduleinst(z).FUNCS[x])`)이 아예 실행되지 않아, active element segment로 채운 테이블 슬롯이 resolve 안 된 `REF.FUNC`(인덱스) 상태로 store에 그대로 남습니다.
  `table-mutation.js`의 `table.get(0)()`가 `typeof`의 최종 `assert (? val: Record[Object])`에서 크래시하는 걸로 발견 — `table_read`가 실제로 리턴한 값을 찍어보니 `CaseV(REF.FUNC, [NumV(Nat(1))])`였고(진짜 `REF.FUNC_ADDR`이 아님), wasm 자신의 `call_indirect`(embedding 레이어를 안 거침)로 접근해도 SpecTec 자신의 `Assert (case(val') == REF.FUNC_ADDR)`가 실패하는 걸로 재확인. `git bisect`(로컬 `official` 브랜치 — 실제 업스트림 `WebAssembly/spec`의 `main` — 기준, spectec 자신의 `--interpreter` 모드로 최소 `.wast` 재현 스크립트를 오라클 삼아 자동 실행)로 정확한 도입 커밋을 특정: `3f199fb74f25b0e3da20a0aad5559a91061b873b`("Give call_ref-host a real thrown-exception outcome")이 `step_wasm`의 `CaseV ("CONST", _) | CaseV ("VCONST", _)`(그리고 버전<=2의 `REF.NULL` 특수 케이스)만 값으로 인정하던 좁은 매칭을, 런타임에 계산된 `$callhostfunc`의 결과 instr*도 커버하려고 `WasmContext.is_value` 기반의 범용 체크로 넓히면서 이 prefix 체크의 부정확함이 처음으로 실제 실행 경로에 노출된 것으로 보임(그 전엔 `il2al/translate.ml`의 컴파일 타임 `is_wasm_value`/`is_wasm_instr` 분리가 항상 먼저 걸러줘서 `step_wasm`의 이 런타임 fallback 자체가 CONST/VCONST 말고는 실행될 일이 없었음).

## 2. (moved) `xl/num.ml`의 `bin`/`cmp`가 타입이 섞인 피연산자(`` `Nat ``/`` `Int ``)를 처리 못 함

#1과 같은 이유로 `docs/hardcodes.md` #40으로 옮김(WebAssembly/spec#2332, 의도된 설계라는 답을 받고 닫힘). 번호 유지를 위해 남겨 둠.

## 4. `test/js-api/limits.any.js`가 정의된 적 없는 `assertEquals`(camelCase)를 호출 — WPT 표준은 `assert_equals`(snake_case)

- **File**: `spectec/test/js-api/limits.any.js`(2곳), `spectec/test/js-api/wasm-module-builder.js`(1곳).
- **Current**: `assertEquals(-1, instance.exports.grow());` / `assertEquals(1, val.length, 'string inputs must have length 1');`
- **Expected**: `assert_equals(-1, instance.exports.grow());` 등 — WPT `testharness.js`의 표준 함수 이름.
- **Reason**: `assertEquals`는 `spectec/test/js-api/` 코퍼스 전체(그리고 이 파일이 끌어오는 유일한 `META: script`인 `wasm-module-builder.js`)를 통틀어 정의된 적이 단 한 번도 없음 — 순수하게 존재하지 않는 전역 식별자를 호출하는 것이라, wasm 엔진의 구현 수준과 무관하게 `ReferenceError`가 나야 정상. 직접 `tests/wji/scripts/wji-node-check`로 실제 Node(V8)에 이 오타를 되살려 돌려봐서 확인: `assertEquals is not defined`로 정확히 같은 방식으로 깨짐 — WJI만의 문제가 아니라 진짜 벤더 코퍼스 자체의 오타. `spectec` 서브모듈 자신의 git 히스토리에 이 파일의 다른 오타(`mininum` → `minimum`)를 고친 커밋(`b55286262`, 2019)이 있는 걸 보면 메인테이너들이 이 파일의 오타를 실제로 고쳐온 이력은 있는데, `assertEquals`는 이 저장소가 vendoring한 최신 커밋(`fb983ce31`, 2025-04-10)까지도 안 고쳐진 채 남아있음 — 아마도 파일 전체(`// META: timeout=long`이 붙은, 기본 CI에서 잘 안 돌리는 무거운 스트레스 테스트)의 200줄 넘는 분량 중 딱 2개 서브테스트만 조용히 깨지는 자리라 눈에 안 띈 것으로 추정.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의 `perFilePatches["limits.any.js"]`에 `["assertEquals(", "assert_equals("]` 텍스트 치환 추가(함수 이름만 바꿈, 인자 순서는 그대로 — `same_value`가 대칭이라 pass/fail 판정엔 영향 없고 실패 메시지 텍스트만 원래 의도와 달라짐, 어차피 SUMMARY N/M만 확인하므로 무해).

## 5. `wasm-module-builder.js`의 `is_shared` 계산이 명시적으로 넘긴 `false`를 `true`로 오판

- **File**: `spectec/test/js-api/wasm-module-builder.js`, memory import 섹션 인코딩(`var is_shared = (typeof imp.shared) != "undefined";`).
- **Current**: `var is_shared = (typeof imp.shared) != "undefined";`
- **Expected**: `var is_shared = imp.shared === true;`
- **Reason**: `addImportedMemory(module, name, initial, maximum, shared)`의 `shared` 인자에 **명시적으로 `false`**를 넘겨도(`typeof false === "boolean"`, `"undefined"`가 아님) `is_shared`가 `true`로 계산됨 — "인자가 생략됐는지"와 "인자 값이 truthy인지"를 혼동한 전형적인 `typeof` 오용. `spectec/test/js-api/` 코퍼스 전체에서 `shared` 인자를 넘기는 호출은 `limits.any.js`의 `addImportedMemory("", "", 1, 1, false)` 단 한 곳뿐이라(그 외는 전부 인자 자체를 생략) 이 버그의 실질적 영향 범위도 그 한 곳으로 국한됨.
- **증상**: `limits.any.js`의 "memories" `testLimit`이 의도한 것(단순히 "non-shared memory import 1개"로 개수 제한만 테스트)과 달리 실제로는 shared memory import를 인코딩함 — 이 저장소의 wasm core 스펙 스냅샷은 threads/shared-memory 프로포절이 없어서(`docs/out_of_scope.md` #5) 레퍼런스 디코더가 `require (flags land 0xfa = 0) ... "malformed limits flags"`로 이 인코딩 자체를 거부, "Validate/Compile/Async compile memories limit"(정확히 경계값 1개) 3개가 (원래 테스트 의도와 무관한 이유로) `CompileError`.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의 `perFilePatches["limits.any.js"]`에 텍스트 치환 추가. 다른 어떤 호출부도 `shared` 인자를 안 넘겨서 영향 없음을 확인. 이 fix로 해당 3개 subtest가 실제로 통과하게 됨(threads 프로포절 없이도, 애초에 이 테스트가 shared memory를 테스트할 의도가 아니었으므로).

## 6. `test/js-api/limits.any.js`의 `kJSEmbeddingMaxMemories` 상수가 스펙 개정 이후 갱신 안 됨(`1` vs 실제 `100`)

- **File**: `spectec/test/js-api/limits.any.js:20`.
- **Current**: `const kJSEmbeddingMaxMemories = 1;`
- **Expected**: `const kJSEmbeddingMaxMemories = 100;` — `spectec/document/js-api/index.bs:2232`("The maximum number of memories, including defined and imported memories, is 100.")과 일치해야 함.
- **Reason**: `git log -S`로 확인한 히스토리 — 이 상수는 2018-12-12 도입 당시(`8f1e01db5`) 스펙 텍스트 자체도 "is 1"이던 시절(multi-memory 프로포절 반영 전, 모듈당 메모리 1개가 진짜 core wasm 하드 리밋이던 시절)에 맞춰 `1`로 설정됐고, 그 이후 스펙 텍스트의 숫자가 `100`으로 올라갔는데도(multi-memory 프로포절 반영) 이 테스트 파일의 상수는 한 번도 안 바뀌었음. 그 결과 "memories over limit" subtest가 실제로는 memory import 2개(진짜 한도 100에 한참 못 미침)로 "invalid해야 한다"고 잘못 기대함 — real Node(V8)가 이 subtest를 실패시키는 것도 "V8이 개수 제한을 안 지켜서"가 아니라 **V8은 진짜 현재 한도(100)를 정확히 지키고 있고, 2개는 100 밑이라 당연히 valid로 판정**하기 때문.
- **처리**: 문서화만 함, 코드 수정 없음 — 이 상수를 고쳐도(1→100) WJI가 통과하는 데는 도움 안 됨. WJI는 이 "Implementation-defined Limits" 섹션 전체(memories 포함)를 애초에 mechanize 안 하기로 결정했기 때문(`docs/out_of_scope.md` #7) — 상수를 몇으로 바꾸든 WJI는 개수 자체를 안 세므로 "over limit" subtest는 여전히 실패함.

## 7. `test/js-api/table/grow-memory64.any.js`가 정의된 적 없는 `nulls` 헬퍼를 호출

- **File**: `spectec/test/js-api/table/grow-memory64.any.js` (3곳: "Basic i64"/"Reached maximum (i64)"/"Exceeded maximum (i64)" 서브테스트).
- **Current**: `assert_equal_to_array(table, nulls(5), "before", "i64");` 등 — `nulls`는 이 파일에도, 이 파일이 끌어오는 `// META: script=assertions.js`에도 정의돼 있지 않음.
- **Expected**: 길이 n짜리 all-null 배열을 만드는 `nulls(n) { return new Array(n).fill(null); }`가 어딘가 정의/import돼 있어야 함.
- **Reason**: `spectec` 서브모듈 git 히스토리(`2929f4497`, "Split memory64 JS API tests into separate files", PR #2026)를 보면 이 3개 테스트는 원래 `table/grow.any.js`(지금도 `nulls`를 자체 정의) 안에 있었는데, memory64 주소 관련 서브테스트만 이 새 파일로 잘라 옮기면서 `nulls` 헬퍼는 안 옮기고 import도 안 함 — 순수 리팩터링 누락. `tests/wji/scripts/wji-node-check`로 실제 Node(V8)에 원본(패치 전) 생성 파일을 그대로 돌려서 확인: 똑같이 `nulls is not defined`로 3개 다 깨지고 `SUMMARY 3/6` — WJI만의 문제가 아니라 진짜 벤더 코퍼스 자체의 버그.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의 `perFilePatches["table/grow-memory64.any.js"]`에 `nulls` 정의를 공유 `assertions.js` 의존성 텍스트 앞에 주입하는 패치 추가 — `perFilePatches`가 relPath로 키를 잡으므로 `assertions.js`의 다른 소비 파일(`table/get-set.any.js` 등)엔 영향 없음. 재생성 후 `SUMMARY 6/6`으로 완전 통과 — `EvalSpec.scala`의 `knownFailing`에서 제거.
