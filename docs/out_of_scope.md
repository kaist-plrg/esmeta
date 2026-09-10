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

