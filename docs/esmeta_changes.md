# ESMeta Changes

`docs/esmeta_errors.md`와 달리, 여기는 ESMeta mainline이 **의도적으로** 하고
있던 스코프 제한(버그가 아니라 설계/우선순위상의 선택으로 보이는 것)을 WJI
쪽 필요에 의해 넓힌 변경들을 기록합니다.

## 1. `emu-annex` 밑 알고리즘은 추출 대상에서 제외되어 있었음

- **File**: `src/main/scala/esmeta/extractor/Extractor.scala`, `extractHeads`
- **Before**: `if (parent.tagName != "emu-clause") return Nil` — 알고리즘의
  바로 위 부모가 `emu-clause`가 아니면 무조건 스킵.
- **After**: `emu-clause`뿐 아니라 `emu-annex`도 허용.
- **영향 범위**: ecma262/spec.html에는 `emu-annex`가 81개 있고, 그 중
  "Annex B: Additional ECMAScript Features for Web Browsers" 하위의
  `escape`/`unescape`, `String.prototype.substr`/`anchor`/`big`/`blink`/
  `bold`/`fixed`/`fontcolor`/`fontsize`/`italics`/`link`/`small`/`strike`/
  `sub`/`sup`, `Date.prototype.getYear`/`setYear`/`toGMTString`,
  `RegExp.prototype.compile`, 그리고 `FunctionDeclarationInstantiation` 등의
  web-compat 재정의 알고리즘들이 전부 여기 걸려서 지금까지 아예
  mechanize된 적이 없었다. 이 변경 하나로 49개 알고리즘이 새로 추출됨
  (`algorithms: 2870 → 2919`, `spec-summary` 참고).
- **왜 "버그"가 아니라 "변경"으로 분류했는가**: Annex B는 스펙 자체가
  "normative optional"(구현체가 골라서 지원해도 되는) 내용이라고 명시하는
  섹션이고, `emu-clause`/`emu-annex`를 애초부터 구분해서 다르게 다룬 흔적
  (`isInAnnex`가 grammar production 쪽엔 이미 존재)도 있어서, 단순 누락이
  아니라 "본체 스펙(emu-clause)만 우선 mechanize한다"는 의도적 스코프
  결정이었을 가능성이 높다고 판단함. 다만 그 스코프 결정이 어디에도
  명시적으로 문서화돼 있지 않았고, WJI의 공식 js-api 테스트 corpus
  (`wasm-module-builder.js`)가 `unescape(encodeURIComponent(...))`라는
  흔한 UTF-8 인코딩 관용구를 통해 Annex B의 `unescape`를 실제로 필요로
  해서 이번에 범위를 넓혔다.
- **검증**: 새로 뚫린 49개 알고리즘 각각이 실제로 뭔가를 깨뜨리지는
  않는지 `sbt test`(525개) 전체 통과로 확인 — 유일한 diff는 CFG
  fingerprint 골든(함수 개수가 늘었으니 당연히 바뀜) 재생성뿐이었음.

## 2. `escape`/`unescape`가 실제 global 프로퍼티로 연결되지 않고 있었음

- **File**: `src/main/scala/esmeta/es/builtin/GlobalObject.scala`,
  `GlobalObject.map`
- **Before**: `spec.tables(WELL_KNOWN_INTRINSICS).rows`(메인 72행짜리
  `table-well-known-intrinsic-objects`)만 읽어서 global 프로퍼티 맵을
  구성.
- **After**: `table-additional-well-known-intrinsic-objects`(Annex B
  "Additional Properties of the Global Object" 섹션의 별도 2행짜리
  테이블, `%escape%`/`%unescape%` → `` `escape` ``/`` `unescape` `` 매핑)
  도 합쳐서 구성.
- **왜 "버그"가 아니라 "변경"으로 분류했는가**: 위 1번과 같은 맥락 —
  Annex B의 별도 테이블 자체가 메인 테이블과 분리돼 있는 건 스펙
  구조상 자연스러운 선택이고, `GlobalObject.scala`가 메인 테이블만 읽는
  것도 "Annex B는 일단 범위 밖" 결정의 연장선으로 보임. 스펙 본문은
  "The entries in [additional table] are added to [main table]"이라고
  명시하므로, 위 1번으로 Annex B 알고리즘 자체를 mechanize하기로 한
  이상 이 병합은 그 결정을 실행하는 데 필요한 자연스러운 후속 조치.
