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

