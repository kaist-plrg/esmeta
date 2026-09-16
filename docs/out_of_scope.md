# Out of Scope

`docs/spec_errors.md`/`docs/spec_inconsistencies.md`/`docs/underspecified-behaviors.md`/
`docs/engine_deviations.md`가 전부 "이 gap의 원인이 무엇인가"를 분류하는 문서라면,
여기는 그 원인 분석과는 별개로 **"이 gap을 더 이상 고치지 않기로 명시적으로
결정했다"**는 사실 자체와 그 근거를 기록한다. 단순히 "지금은 미지원"(아직 안
고쳤을 뿐, 나중에 다시 볼 후보)인 것과는 다르다 — 여기 등록된 항목은 재검토
후보 목록에서 의도적으로 제외된, 우선순위 판단의 결과다. 원인 자체가 궁금하면 각
항목이 가리키는 원인 문서를 따라가면 된다.

## 1. `module/customSections.any.js`

- **가리키는 원인 문서**: 근본 원인 자체는 스펙/엔진 어느 한쪽의 결함이 아니라
  WJI에 지금까지 없던 새 컴포넌트(WASM 바이너리 포맷 파서)가 필요한 규모의
  문제라, 별도 분류 문서 없이 이 항목 자체에 원인까지 같이 적는다.
- **막힌 지점**: `Module.prototype.customSections`(`spectec/document/js-api/index.bs`
  lines 745-754)의 `"For each [=custom section=] |customSection| of |bytes|,
  interpreted according to the [=module grammar=], ..."` — 정의된 알고리즘/함수
  호출이 아니라 WASM Core 스펙의 별도 바이너리 그래머 프로덕션을 통째로 참조하는
  서술이라, 애초에 기계화 친화적으로 쓰여있지 않음.
- **제외 이유** (두 겹):
  1. WASM Core 스펙 자신의 바이너리 디코딩 문법이 custom section 정보를 버리도록
     정의돼 있음(`grammar Bcustom : ()* hint(desc "custom data") = | Bname Bbyte*
     => ()`, `spectec/document/core/5.4-binary.modules.spectec:16`) — 파싱은
     하되 결과를 통째로 버림. Embedding API(`spectec/document/core/appendix/
     embedding.rst`)에도 custom section을 읽는 함수가 아예 없어서, SpecTec/
     WasmHost RPC 브릿지로는 이 정보를 물어볼 방법이 원천적으로 없음.
  2. 위 문제와 별개로, 이 알고리즘의 한 단계 안에서만도 서로 다른 파싱 gap이
     4개(`ForEach`가 엉뚱한 걸 순회함 / `<code>name</code>` 필드 접근 미인식 /
     UTF-8 decode 방향 미기계화 / 반환값인 `ArrayBuffer` 슬라이스 구성 자체도
     미파싱) 겹쳐 있음 — 다른 gap들처럼 정규식 하나 확장하거나 lowering pass에
     case 하나 추가하는 규모가 아니라, WASM 바이너리 포맷(섹션 id + LEB128
     varint 길이 파싱)을 처음부터 직접 다루는 새 미니 파서를 작성해야 풀리는
     문제.
- **결정**: 사용자 판단으로 스코프 제외 확정("CustomSection 관련 테스트 파일은
  아예 스코프에서 제외하는 게 낫겠다 ... 완전히 기계화-unfriendly하게
  적혀있네", 2026-09-10) — `SharedArrayBuffer`/`js-string`처럼 "지금은
  미지원, 나중에 다시 볼 후보"가 아니라, 애초에 문제의 성격상 더 파봐야 실익이
  없다는 판단.
- **`knownFailing` 처리**: `EvalSpec.scala`의 `knownFailing`엔 그대로 남겨둠(전체
  js-api corpus를 빠짐없이 생성하는 `tests/wji/js-api/README.md`의 생성-스코프
  정책은 이것과 별개로 그대로 유지 — 이 문서가 다루는 "재검토 우선순위"와
  그 문서가 다루는 "생성 스크립트가 무엇을 만드는가"는 서로 다른 층위의 결정).

## 2. "Setting (sloppy mode)" 서브테스트 — ESMeta mainline이 모든 스크립트를 strict mode로 가정

- **막힌 지점**: `src/main/scala/esmeta/compiler/Compiler.scala`(mainline
  컴파일러, WJI 코드 아님)의 `StrictMode` 조건 컴파일 — `case StrictMode => T
  // XXX assume strict mode`. ECMA-262의 `PutValue`가 `[[Strict]]` 플래그를
  체크해서, 읽기 전용 접근자(accessor)에 값을 대입할 때 strict mode면
  `TypeError`를 던지고 sloppy mode면 조용히 무시(silent no-op)해야 하는데,
  이 플래그가 항상 `true`로 하드코딩돼 있어 sloppy mode의 실제 동작(조용한
  no-op)을 절대 재현 못 함 — 스크립트/노드 단위로 strict mode 여부를 실제로
  판별하는 directive prologue 추적 자체가 mainline에 미구현.
- **증상**: `"Setting (sloppy mode)"`라는 제목의 서브테스트가 이 코퍼스 여러
  파일(`memory/buffer.any.js`, `table/length.any.js`,
  `instance/exports.any.js` 등)에 반복해서 등장 — 읽기 전용 프로퍼티에 값을
  대입했을 때 (sloppy mode 스크립트 기준) 조용히 무시돼야 하는데, WJI가
  mainline strict-mode 하드코딩을 그대로 물려받아 매번 `TypeError`를 던짐.
- **제외 이유**: WJI/wasm 기계화와 전혀 무관한 **ESMeta mainline 자체의
  기존 한계**(WJI가 처음 만들어지기 전부터 있던 `# XXX` 표시가 붙은 임시
  구현) — WJI 쪽에서 아무리 고쳐도 이 gap엔 손이 안 닿고, mainline
  `Compiler`/파서에 진짜 strict-mode 판별 로직을 새로 구현해야 풀리는
  문제라 WJI 작업 범위 밖.
- **처리**: `tests/wji/js-api/skip-known-gaps.js`가 `test()`를 감싸서 이
  제목과 정확히 일치하는 서브테스트는 **아예 실행 자체를 안 시킴**(실패로
  집계하는 게 아니라, 통과할 수 없는 assertion에 인터프리터 시간을 낭비하지
  않도록 처음부터 건너뜀) — `tests/wji/js-api/README.md`의 하네스 설명대로
  `testharness-lite.js` 로드 직후, 실제 테스트 파일 본문이 실행되기 전에
  주입됨. 이 스킵 덕분에 위 3개 파일은 이 gap과 무관하게 `knownFailing`에
  들어있지 않고 정상적으로 완주 중.

## 3. `limits.any.js` — 스펙이 자인한 유일한 느린 테스트, 진짜로 느린 호출만 골라서 무력화

- **막힌 지점**: `limits.any.js`(`spectec/test/js-api/limits.any.js`) — js-api
  공식 코퍼스 전체에서 유일하게 `// META: timeout=long`이 붙은 파일(WPT
  관례, "실제 엔진에서도 원래 오래 걸리니 넉넉한 타임아웃을 달라"는 뜻).
  `testLimit`/`testDynamicLimit`/`testModuleSizeLimit` 호출이 전부 `test()`
  콜백이 등록되기도 전에 **스크립트 최상위에서 동기적으로** 실행됨 — 그중
  일부(`testLimit("types"/"functions"/"imports"/"exports"/"globals"/"data
  segments"/"tables"/"element segments", ...)`)는 콜백 안에서 `for (let i =
  0; i < count; i++) builder.addX(...)` 형태로 `count`(최대 1000만)만큼
  실제로 JS 루프를 돌려서 `WasmModuleBuilder`를 채움 — 실제 엔진에서도
  느리다고 스펙이 인정하는데, AST를 그대로 걷는 인터프리터(ESMeta)에는
  사실상 영원히 안 끝나는 수준. `testModuleSizeLimit`은 한술 더 떠 1GiB짜리
  `Uint8Array`를 직접 할당함.
- **전부 다 그런 건 아님 — 정말 느린 호출만 선별**: 같은 파일 안의 다른
  `testLimit` 호출들(`"function params"`/`"function returns"`, `count`
  상한 1000; `"memories"`, 상한 1)은 루프를 돌아도 반복 횟수 자체가 작아서
  무해하고, `"function locals"`/`"function params+locals"`는 `count`가
  최대 5만이지만 루프를 안 돎 — `builder.addLocals({i32_count: count})`처럼
  큰 `count`를 그냥 인자 하나로 넘길 뿐, `WasmModuleBuilder`의 `addLocals`/
  `getNumLocals`(`spectec/test/js-api/wasm-module-builder.js`) 자신도
  `count`만큼 반복하지 않고(local 개수를 하나의 집계값으로만 저장) O(1)로
  끝남. `testDynamicLimit` 2건과 마지막 `test()`(Table 크기 제한)도 큰
  숫자를 그냥 한계값 인자로 넘기기만 할 뿐 JS 루프가 없어서 무해함 — 이런
  건 굳이 다 같이 지워버릴 이유가 없음(그럴 거면 그냥 파일 전체를
  `knownFailing`에만 넣는 것과 다를 바 없음).
- **왜 `skip-known-gaps.js`(문서 #2) 방식이 안 통하는가**: 그 메커니즘은
  `test()` 콜백 "제목"으로 걸러내는 방식인데, 여기서 느린 부분은 애초에
  어떤 `test()` 콜백 안에도 있지 않고 콜백을 등록하는 시점보다도 먼저,
  스크립트 몸통 자체가 실행되면서 벌어짐 — 타이틀 기준 필터로는 원천적으로
  막을 수 없음.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의
  `perFilePatches["limits.any.js"]`가 진짜로 느린 9개 호출(`testLimit` 7개
  + `testModuleSizeLimit` 2개)의 **첫 줄만** `if (false) `로 접두 — 함수
  호출 하나가 통째로 한 JS statement이므로, 몇 줄에 걸쳐 있든 첫 줄 앞에
  `if (false)`만 붙이면 그 호출 전체가 조건부가 됨(`testPatches`와 같은
  "짧은 `[from, to]` 문자열 치환" 철학 그대로, 여러 줄을 통째로 감쌀 필요
  없음). 재생성해서 확인한 결과 `SUMMARY 35/56`(81초)로, 전엔 `0/0` 아니면
  `TimeoutException`이던 게 이제 실제 서브테스트 결과를 보여줌.
- **`knownFailing`/`EvalSpec.perTestTimeoutSec` 처리**: 35/56이라 여전히
  `knownFailing`에 남아있어야 함(제거 대상 아님 — 남은 실패들은 이 정리와
  무관한 별개의 진짜 WJI gap들, 미조사). 원래 `perTestTimeoutSec = 60`이라는
  값 자체가 "무력화 전 `limits.any.js`가 영원히 안 끝나지 않도록 너무 길지
  않게" 잡혔던 제약이었는데, 이제 이 파일도 81초 안에 끝나니 그 제약이
  사실상 사라짐 — 덕분에 `memory/grow.any.js`(격리 실행 기준 실제 소요
  시간 ~80-90초)가 통과할 수 있도록 `perTestTimeoutSec`을 150으로 올림(같은
  커밋).

## 4. `WebAssembly.Memory`/`Table` 초대형 인스턴스를 만드는 서브테스트 3곳 — 개별 assertion만 제외, 파일 전체는 안 건드림

- **근본 원인**: `spectec`의 `WasmHost`(`spectec/spectec/src/backend-interpreter/host.ml`)가 linear memory를 **바이트 하나당 JSON 객체 하나**로 RPC에 실어보냄. `new WebAssembly.Memory({initial:64, maximum:128})`(64페이지 = 4MB)만 돼도 circe가 이 JSON을 파싱/디코딩하다가 `-Xmx3g`(`.jvmopts`/`build.sbt`에 이미 설정된 값)로도 못 버티고 `OutOfMemoryError`를 던짐. 이 문서 #1(customSections)/#3(limits)과 달리 이건 SpecTec 서브모듈(RPC 브릿지 자체)을 고쳐야 하는 문제라 WJI 쪽에서 당장 근본 해결은 어려움.
- **왜 파일 전체가 아니라 "제외"만 해도 되는지, 그리고 왜 이게 중요한지**: `OutOfMemoryError`는 `TypeError`/`NotSupported` 같은 보통의 예외와 달리 ScalaTest가 "이 suite 전체를 즉시 중단시켜야 하는 신호"로 취급함 — 한 파일에서 이게 터지면 `EvalSpec`의 같은 호출 안에서 그 뒤에 실행될 예정이던 **다른 knownFailing 파일들이 전부 시도조차 안 되고 조용히 스킵**됨(다른 진짜 결과와 구분이 안 가서 위험). `sbt run wji-eval <file>`처럼 파일 하나만 격리해서 돌려도 여전히 OOM은 남(프로세스 격리 문제가 아니라 그 파일 자체가 진짜로 힙을 다 씀 — sbt 서버 JVM 자체는 OOM 이후에도 살아남아서 다음 커맨드엔 영향 없음, 직접 확인함) — 그래서 "그냥 알아서 죽게 두고 knownFailing으로만 막기"보다, 실제로 OOM을 일으키는 딱 그 assertion들만 골라서 원천적으로 실행을 안 시키는 쪽을 선택. 이걸 해두면 (a) 전수 스윕이 이 두 파일 때문에 통째로 끊기는 일이 없어지고, (b) 각 파일이 OOM 자리 이후의 다른(진짜 조사할 가치 있는) gap까지 진행할 수 있음.
- **어디, 그리고 처리 방식이 파일마다 다른 이유**: `tests/wji/scripts/wji-generate-js-api-tests.js`의 `perFilePatches`에 3개 파일 항목 추가, 무거운 할당이 소스에서 **어떻게 도달하는지**에 따라 패치 모양이 다름:
  1. **`instance/constructor.any.js`** (그리고 뒤늦게 합류한 `constructor/instantiate.any.js` — 아래 "결과" 참고) — `instanceTestFactory`(이름+팩토리 함수 배열)의 4개 항목("getter order for imports object"/"imports"/"imports with empty module names"/"imports with empty names")이 각자 자기 `test()`/`promise_test()` 콜백 안에서만 `new WebAssembly.Memory({initial:64,...})`를 만듦(그 서브테스트가 실제로 호출될 때만 실행되는 지연 평가) — 그래서 이 배열을 소비하는 `for (const [name, fn] of instanceTestFactory)` 루프 한 줄만 `.filter(...)`로 바꿔서 이 4개 이름을 걸러내면 됨. `limits.any.js`와 달리 이름이 다른 파일(`module/imports.any.js`— 이미 11/11 통과 중 — 등)과 겹쳐서, 전역 `skip-known-gaps.js`(타이틀 매칭)는 못 쓰고 파일 전용으로만 적용 — 두 파일 다 같은 `spectec/test/js-api/instanceTestFactory.js` META 스크립트를 끌어오고 이 루프 헤더 텍스트가 글자 그대로 같아서, `perFilePatches`에서 `instanceTestFactoryOomPatches` 하나를 공유(`wji-generate-js-api-tests.js`).
  2. **`instance/constructor-bad-imports.any.js` / `constructor/instantiate-bad-imports.any.js`** — 둘 다 `spectec/test/js-api/bad-imports.js`라는 **공유 META 스크립트**(제너레이터가 `depsSrc`로 따로 resolve — `body`가 아님, 그래서 처음엔 패치가 `body`에만 적용되도록 짜서 조용히 안 먹혔던 실수를 발견해서 고침)의 `nonMemories`/`nonTables` 배열 **리터럴**에 있음 — `test_bad_imports`가 파일 최상위에서 즉시 호출되면서 이 배열이 통째로 즉시 평가됨. 배열 원소는 statement가 아니라 expression이라 `if (false)` 접두를 못 씀 — 그냥 그 원소 한 줄(`"WebAssembly.Memory object (too large)"`/`"WebAssembly.Table object (too large)"`)을 통째로 삭제. `constructor/instantiate-bad-imports.any.js`는 지금 이 OOM 지점 전에 이미 다른 gap(IEEE754 라운딩)에 막혀서 실제로 도달은 안 하지만, 같은 공유 스크립트를 쓰므로 미리 같이 패치해둠(그 앞 gap이 풀리는 순간 똑같이 재발할 것이므로).
- **결과**: 세 파일 다 OOM 없이 다른 지점까지 진행 확인 — `instance/constructor.any.js`는 `InvalidConversion: invalid conversion to [math]: f64, wasm<CaseV(POS,...NORM...)>`(wasm→JS f64 변환 미기계화, `global/value-get-set.any.js`의 f32-subnormal 케이스와 같은 큰 gap의 f64판), `instance/constructor-bad-imports.any.js`는 `WasmHost error: ProtocolError(...Fail)`(SpecTec 백엔드 자체 에러, 미조사), `constructor/instantiate-bad-imports.any.js`는 원래대로 IEEE754 라운딩 gap. 셋 다 `knownFailing`에 그대로 남음 — 이 정리로 뭔가 통과하게 된 파일은 없고, 순수하게 "스윕이 안전해지고 각 파일이 더 진행할 수 있게 됨"이 목적.
- **2026-09-14 추가**: `docs/hardcodes.md` #20(`AllowSharedBufferSource` 검증)으로 `constructor/instantiate.any.js`가 더 진행하면서 똑같은 원인(`instanceTestFactory`의 같은 4개 항목)으로 새로 OOM에 부딪힘 — 이 파일은 그동안 이 항목 대상에 없었을 뿐, `instance/constructor.any.js`와 완전히 같은 코드 경로였음. `instanceTestFactoryOomPatches`를 `instance/constructor.any.js`와 공유하도록 리팩터링해서 이 파일에도 적용(`wji-generate-js-api-tests.js`). 적용 후 OOM 없이 145초 만에 `~auto~`(`docs/hardcodes.md` #20의 "결과" 참고, `personal/TODO.md` #44) gap까지 진행 확인 — `instance/constructor.any.js`(25/25) 회귀 없음.
- **2026-09-16 추가 — 같은 근본 원인의 `WebAssembly.Table` 변종, `limits.any.js`에서 새로 발견**: `personal/TODO.md` #62(legacy `assert_throws`/`promise_rejects` 시그니처 이식) 적용 직후 `limits.any.js`가 상시 떠있는 sbt 서버(`-Xmx3g`)를 반복적으로 `OutOfMemoryError`로 죽임 — 이전엔 이 파일의 관련 subtest들이 harness 함수 자체가 없어서 매번 `ReferenceError`로 조용히(캐치돼서) fail했을 뿐, 실제로 큰 `Table`을 만드는 코드까지 도달한 적이 없었음. `host.ml`의 `create_tableinst`/table 관련 RPC도 `create_meminst`와 똑같이 원소 하나당 JSON 값 하나로 표현해서, 스펙이 정의한 `kJSEmbeddingMaxTableSize`(1000만) 근처 크기의 진짜 `Table`을 만들면 이 문서 #4의 Memory 케이스와 동일하게 힙이 터짐 — 세 지점(`new WebAssembly.Table({initial: kJSEmbeddingMaxTableSize + 1, ...})`, 기존 테이블의 `.grow(kJSEmbeddingMaxTableSize)`, 그런 초기 크기를 선언한 모듈의 실제 `instantiate`)을 스크래치 스크립트로 각각 격리해서 셋 다 독립적으로 OOM 재현 확인. `testDynamicLimit("maximum table size", ...)`는 실제 할당이 `initial: 1`뿐이라(`maximum`은 저장만 되고 실제로 그 크기까지 자라는 호출이 없음) 안전함을 직접 실행해 확인 — 이 하나는 그대로 둠. `wji-generate-js-api-tests.js`의 `perFilePatches["limits.any.js"]`에 `testDynamicLimit("initial table size", ...)` 호출 전체와 "Grow WebAssembly.Table object beyond the embedder-defined limit" `test()` 전체에 `if (false) ` 접두 추가. 적용 후 `limits.any.js` OOM 없이 `SUMMARY 35/50` 안정적으로 도달 확인(`personal/DONE.md` 참고).

## 7. JS-API "Implementation-defined Limits" 섹션 — 선언형 제약이라 ESMeta/SpecTec의 알고리즘 실행 모델과 안 맞음, 스코프 제외

- **배경**: `spectec/document/js-api/index.bs:2208`("Implementation-defined
  Limits") — locals 5만/params·returns 각 1000 등 약 20개 숫자 상한을 나열한
  순수 산문 `<ul>` 목록(`personal/TODO.md` #63에 상세 조사 기록). `compile a
  WebAssembly module`(index.bs:395)은 `module_decode`/`module_validate` 두
  단계만 밟을 뿐 이 섹션을 가리키는 `[=...=]` 링크가 스펙 전체에 단 하나도
  없음 — 다른 모든 정의처럼 "이 알고리즘이 이 정의를 호출한다"는 명시적
  연결이 전혀 없는, 완전히 붕 떠 있는 산문. `module_validate`가 위임하는
  core wasm의 `Module_ok`(`Reference_interpreter.Valid.check_module`)에도
  이런 숫자 제약은 없음 — JS-API가 core wasm 위에 순수하게 얹은 정책층이라
  core wasm 검증기가 대신 걸러줄 수도 없음.
- **왜 스코프 제외로 결정했는지**: ESMeta/SpecTec 둘 다 "알고리즘을 실행한다"는
  모델에 특화된 프레임워크이지, 이런 식으로 알고리즘 그래프 밖에 선언형으로
  떠 있는 제약을 처리하는 데 특화된 도구가 아님(사용자 판단). `Memtype_ok`/
  `Tabletype_ok`(`docs`에 별도 기록 없음, `relation.ml` 참고 — IL2AL 번역
  대상이 아니라 손으로 구현한 core wasm 릴레이션)처럼 스펙 자신이 정의한
  릴레이션을 손으로 옮기는 선례와 달리, 이 섹션은 애초에 알고리즘/릴레이션
  형태조차 아니라서 "어느 지점에 끼워넣을지"부터 설계가 필요한 완전히 다른
  성격의 작업 — 가치 대비 비용이 안 맞는다고 판단.
- **처리**: `limits.any.js`가 이 섹션만 검증하는 4개 `testLimit` 호출
  (`"function locals"`/`"function params"`/`"function params+locals"`/
  `"function returns"`)을 `wji-generate-js-api-tests.js`의
  `perFilePatches`에서 `if (false) ` 접두로 제외(이 문서 #3/#4와 같은
  "첫 줄만 조건부로" 패턴) — 호출 하나당 9개 subtest(Validate/Compile/Async
  compile × minimum/limit/over limit)이므로 4개 호출 = 36개 subtest 제외.
  재생성 후 `limits.any.js`가 `SUMMARY 11/14`(OOM/타임아웃 없이 34초)로
  안정적으로 도달, 남은 3개 fail은 전부 `personal/TODO.md` #64(memories
  경계값 오판정, 별개의 미해결 gap)뿐임을 확인.
- **참고**: 이 문서 #3이 예전에 이 4개 호출을 "성능상 무해해서 그대로 둠"이라고
  적어뒀는데, 그건 여전히 사실(느리지 않음) — 이번 제외는 성능과 무관하게
  별개의 이유(선언형 제약 자체를 스코프 밖으로 결정)로 이뤄진 것.

## 6. `js-string/constants.any.js`의 100,000자 문자열 상수를 100자로 축소 — 진짜 gap이 다 풀린 뒤 마지막으로 남은 건 순수 성능 문제였음

- **막힌 지점**: `constants` 배열(`goodGlobalTypes` 루프가 각 원소를 wasm
  import 이름으로 써서 `instantiateImportedGlobal`을 호출)의 한 항목이
  `'0'.repeat(100000)`. `String.prototype.repeat`/`StringPad`(이 문서와
  무관한 별개의 mainline 파싱 gap 4개, `docs/esmeta_changes.md` #6)가
  전부 풀리고 나서도 이 파일만은 여전히 끝나지 않았음.
- **원인 조사**: 사용자가 "무한루프 가능성을 먼저 점검해달라"고 요청해서
  코드 리뷰부터 했고, 그 과정에서 `manuals/rule.json`의 `String.prototype.
  repeat` 구현 자체가 O(n²)였던 게 먼저 발견됨(JVM/Scala `String`
  불변성 때문에 매 반복 전체 문자열을 재복사 — `docs/esmeta_changes.md`
  #6에 상세 기록) — 지수적 doubling으로 재작성해서 `n=100000` 기준
  무한대에서 3초로 단축. 그런데도 `'0'.repeat(100000)`을 실제 wasm
  import 이름으로 쓰는 지점은 여전히 몇 분 넘게 안 끝나서, `-wji-eval:log`
  스텝-로그 추적으로 더 파봄: `__FLAT_LIST__`/`__APPEND_LIST__`(리스트
  이어붙이기 AUX 함수)가 의심됐지만 `Obj.scala`의 `push` 구현이
  `Vector`의 `+:=`/`:+=`(상각 O(1))라 여기엔 알고리즘 버그가 없음을
  직접 확인. 격리 재현(문자 하나짜리 `instantiateImportedGlobal` 단독
  호출)은 33초, 100,000자 버전은 5분 가까이 걸리면서도 StepCnt가 시종
  등속(초당 ~200-220K)으로 계속 증가 — 프로토타입 체인 순회
  (`OrdinaryGet`→`GetPrototypeOf`)/SDO 호출/`CreateDataPropertyOrThrow`
  체인 등 문자 하나당 수백 스텝이 드는 spec-level 처리가 10만 번
  곱해지는, 순수 **ESMeta 트리-워킹 인터프리터의 스텝당 오버헤드 ×
  N** 문제로 확정 — `limits.any.js`(이 문서 #3)와 같은 종류의 한계.
- **왜 그냥 줄여도 되는지**: 이 subtest가 실제로 검증하는 건 "임의
  길이/멀티바이트 문자열이 wasm import 이름으로 정확히 왕복되는가"지,
  길이 자체(100,000)는 아님 — 짧은 문자열로도 같은 코드 경로를 그대로
  탄다. `limits.any.js`처럼 "스펙 자신이 실제 엔진에서도 느리다고
  인정하는 스트레스 테스트"가 아니라, 그냥 테스트 작성자가 고른 임의의
  큰 상수일 뿐이라 값 자체를 줄이는 게 안전함.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의
  `perFilePatches["js-string/constants.any.js"]`에 `["'0'.repeat(100000)",
  "'0'.repeat(100)"]` 한 줄 추가 — `limits.any.js`/`SharedArrayBuffer`류와
  같은 "짧은 `[from, to]` 문자열 치환" 패턴.
- **결과**: 재생성 후 `SUMMARY 40/40`(콜드스타트 포함 47초)로 완전 통과 —
  `EvalSpec.scala`의 `knownFailing`에서 제거, `wjiEvalTest`로 회귀 없음
  재확인(54 succeeded / 12 canceled / 0 failed). `docs/esmeta_changes.md`
  #6(rule.json 두 gap)과 `docs/spec_inconsistencies.md` #21(`` `global
  const (ref extern)` `` 패치) 등 이 파일이 그동안 거쳐온 모든 mainline
  gap이 이걸로 전부 마무리됨 — `personal/DONE.md` 참고.

## 5. `SharedArrayBuffer` 생성 자체를 일반 `ArrayBuffer`로 대체 — mainline이 `%SharedArrayBuffer%`를 통째로 `YetObj`로만 가짐

- **막힌 지점**: `esmeta.es.builtin.package.yets`가 `SharedArrayBuffer`를 `Date`/`RegExp`/`DataView`/`Atomics`/`JSON`과 나란히 통째로 `YetObj` 플레이스홀더로 등록(개별 스텝이 아니라 intrinsic 전체 단위) — `esmeta.ty.ValueTy.contains`가 `YetObj`를 만나면 바로 `NotSupported`를 던져서, `new SharedArrayBuffer(...)`를 실제로 호출해보기도 전에 그냥 `globalThis.SharedArrayBuffer`를 읽는 것만으로 죽는다. `AllocateSharedArrayBuffer` → `CreateSharedByteDataBlock`(ecma262 §25.2)이 "Agent Record"/"Candidate Execution"/"Agent Events Record"(`WriteSharedMemory` 이벤트) 같은 ECMAScript 메모리 모델/멀티 에이전트 개념을 직접 다뤄서, mainline이 아직 기계화 못 한 영역이라 통째로 스킵된 것으로 보임.
- **증상**: `constructor/compile.any.js`/`constructor/instantiate.any.js`/`constructor/validate.any.js`/`module/constructor.any.js` 4개 파일 전부, 파일 맨 위 `setup(() => {...})`에서 무조건 `copyToSharedBuffer(emptyModuleBinary)`를 호출해 `new SharedArrayBuffer(...)`를 실제로 생성함 — `setup()`엔 try/catch가 없어서 여기서 던져진 예외가 파일 전체를 죽이고, `SUMMARY`도 못 찍는 "그룹 B" 크래시가 됨. 정작 그 값을 쓰는 subtest는 파일당 4개(`"[Growable] SharedArrayBuffer-backed view"`/`"Invalid module in [growable] SharedArrayBuffer"`, 전체 subtest 15~16개 중)뿐인데, `setup()`이 최상단에서 죽는 바람에 나머지 11~12개 무관한 subtest까지 전부 실행 자체가 안 됨.
- **대체가 안전한 이유**: 그 4개 subtest가 검증하는 건 `WebAssembly.compile`/`instantiate`/`validate`/`new Module`이 SharedArrayBuffer 기반 뷰(및 growable 변형)를 다른 `BufferSource`와 동등하게 받아들이는지, 유효하지 않은 바이트열이면 똑같이 거부하는지뿐 — detach 불가능성, cross-agent 공유, growable in-place mutation 관찰 같은 shared-ness 자체의 의미론은 전혀 검증하지 않는다. 결정적으로 4개 파일 모두 바로 옆에 **완전히 동일한 목적의 resizable `ArrayBuffer` 변형**(`copyToResizableBuffer` → `"Resizable ArrayBuffer-backed view"`/`"Invalid module in resizable ArrayBuffer"`)이 이미 나란히 존재 — 이 테스트 스위트 자체가 "여러 종류의 `BufferSource`를 다 받아들이는지" 확인하는 파라미터화된 반복이고, `SharedArrayBuffer`는 그중 한 flavor일 뿐이다.
- **처리**: `tests/wji/scripts/wji-generate-js-api-tests.js`의 `perFilePatches`에 4개 파일 공용 `sharedArrayBufferPatches`(`["new SharedArrayBuffer(", "new ArrayBuffer("]`) 추가 — `badImportsPatches`(문서 #4)와 같은 "여러 파일이 공유하는 patch 배열" 패턴. `maxByteLength` 옵션은 `ArrayBuffer`도 동일 지원해서 growable 변형도 그대로 치환됨.
- **결과**: 4개 파일 다 `setup()`은 더 이상 안 죽지만, 대신 **전혀 별개의, 지금까지 한 번도 도달한 적 없던 gap**에 새로 부딪힘 — 처음엔 `get_a_copy_of_the_buffer_source`가 buffer *view*를 못 알아보는 문제로 보였으나(`InvalidRefBase`), 더 파보니 실제 원인은 훨씬 더 근본적이었음: 이 함수 자신의 "Assert: is an {{ArrayBuffer}} or {{SharedArrayBuffer}} object"가 `A || (yet ...)` 꼴로 컴파일되는데, `Interpreter.IAssert`가 assert 평가 중 발생한 예외를 전부 삼켜 스킵 처리해서 **이 assert가 구조적으로 절대 실패할 수 없었음** — 그래서 `AllowSharedBufferSource` 인자가 진짜 검증 없이(`WebIdlConversion`이 identity passthrough) 무엇이든 그대로 통과해 들어와, 파일의 실제 첫 크래시 지점은 SharedArrayBuffer-backed view subtest가 아니라 그보다 훨씬 앞선 "Invalid arguments" 테스트(`undefined`/`{}`/`7`/... 11종을 넣고 전부 `TypeError`가 나야 함)였음. 이 assert-파싱 버그와 `AllowSharedBufferSource` 검증 부재는 `docs/hardcodes.md` #20으로 둘 다 고침 — 그 결과 4개 파일 다 "Invalid arguments"는 통과하고 더 진행했지만, 그 과정에서 또 다른 별개의 새 gap 두 개(resizable/length-tracking 뷰의 `[[ByteLength]]`가 `~auto~`인 경우 미처리; `constructor/instantiate.any.js`의 새 OOM)에 부딪혀 4개 파일 다 여전히 `knownFailing`에 남음(제거 대상 아님, `personal/TODO.md` 참고). 이 문서 항목(#5) 자체의 목적("`SharedArrayBuffer` 자체는 더 이상 병목이 아니게 됨")은 달성됨 — 이후 발견된 gap들은 SharedArrayBuffer와 무관한 별개 문제.
- **2026-09-16 추가 — 같은 뿌리의 또 다른 증상 2건, `personal/TODO.md` #64/#65**: (1) `limits.any.js`의 "memories limit"(정확히 `kJSEmbeddingMaxMemories=1`개) 3개 subtest가 처음엔 이 항목과 같은 뿌리로 보였음 — memory import의 wasm 바이너리 limits flags에 shared 비트가 서면 이 저장소의 core wasm 스펙 스냅샷(threads 프로포절 없음)이 디코드 자체를 거부하기 때문. 근데 실제로는 벤더 코퍼스 자체의 독립적인 버그(`wasm-module-builder.js`의 `is_shared` 계산이 명시적 `shared: false`를 `true`로 오판, `docs/spectec_errors.md` #5)였고, 이 테스트 자신은 애초에 shared memory를 테스트할 의도가 전혀 없었음(순수 "메모리 import 개수 제한"만 테스트) — 그 버그를 고치자 threads 지원 없이도 3개 다 정상 통과(`personal/DONE.md` 참고), `TODO.md` #64는 그걸로 해결. (2) `memory/grow.any.js`의 "Growing shared memory does not detach old buffer" 1개는 반대로 **진짜** 이 항목과 같은 뿌리 — `{shared: true}`가 `MemoryDescriptor`에 선언 안 된 멤버라 조용히 무시되는 것뿐이라 `is_shared`류의 국소적 버그 픽스로는 못 고침, threads 프로포절을 진짜로 mechanize해야 함(`TODO.md` #65). `wji-generate-js-api-tests.js`의 `perFilePatches["memory/grow.any.js"]`에 `if (false) `로 이 subtest 하나만 제외 — 18/18 완전 통과로 `knownFailing`에서 제거.

## 8. `limits.any.js` 파일 전체를 스코프 제외로 확정 — 파일 전체가 사실상 `#7`(Implementation-defined Limits 섹션) 하나로 귀결됨

- **배경**: `#3`/`#4`/`#7`로 이 파일의 개별 조각들(느린 호출, OOM, Implementation-defined Limits `testLimit` 일부)을 하나씩 스코프 제외해왔고, 벤더 코퍼스 자체의 버그도 여럿 고쳤음(harness 이식 누락, `assertEquals`/`is_shared`/`kJSEmbeddingMaxMemories` 오타·미갱신, `docs/spectec_errors.md` #4/#5/#6). 이 모든 정리 끝에 남는 실패("memories over limit" 3개)까지 포함해서 전수 확인한 결과, **이 파일이 테스트하는 항목 전부가 "Implementation-defined Limits" 섹션(정적/동적 두 서브리스트) 하나로 완전히 귀결됨** — 파일 맨 위 상수 선언 자체가 `// Static limits`/`// Dynamic limits`로 나뉘어 있고, 스펙 섹션의 두 `<ul>` 목록과 정확히 대응. 이 섹션 밖의 내용은 파일에 전혀 없음.
- **결정**: `#7`이 이미 "이 섹션 전체는 ESMeta/SpecTec의 알고리즘 실행 모델과 안 맞는 선언형 제약이라 스코프 제외"라고 내린 결정이 사실상 이 파일 전체를 커버함 — 그래서 `limits.any.js`를 `module/customSections.any.js`(`#1`)와 같은 방식으로 **파일 단위로 완전히 스코프 제외**하기로 결정(사용자 판단). `EvalSpec.knownFailing`엔 이미 등록돼 있어서(파일 존재 자체가 이미 실행 안 됨) 코드 변경은 필요 없음 — 이 항목의 의미는 "앞으로 이 파일에 대해 더 파고들거나 부분 통과율을 개선하려 하지 않는다"는 걸 명시적으로 기록해두는 것.
- **우선순위**: 스코프 제외 — 더 이상 후보 목록에 안 올림. `personal/TODO.md`/`test_fails.md`에서도 "다음 후보"가 아니라 "스코프 제외/재검토 안 함"으로 이동.
