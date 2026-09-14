# Engine Deviations

`docs/spec_errors.md`(명세 자체의 오타/결함), `docs/spec_inconsistencies.md`(같은
문서 안에서 패턴이 어긋나는 것), `docs/underspecified-behaviors.md`(명세가 아예
아무 말도 안 하는 것)와 별개로 관리하는 목록이다. 이 셋은 전부 **스펙 문서 자체**의
문제를 다루는 반면, 여기 항목들은 반대 방향 — **스펙 텍스트는 명확하고 자기
일관적인데, 실제 브라우저 엔진(V8 등)이 그 텍스트대로 동작하지 않는** 경우를
기록한다. 스펙을 고쳐도 소용없고(스펙이 틀린 게 아니므로), WJI가 스펙 문자 그대로
기계화하면 실제 엔진/공식 conformance 테스트(WPT)와 어긋나는 결과가 나온다 — 그래서
WJI 쪽에서 실제 관찰된 동작에 맞춰 의도적으로 스펙 문구를 벗어나는 선택을 할 때마다
그 근거를 여기 남긴다.

## 1. `WebAssembly.Table.prototype.set`/`grow`의 `optional any value` — 명시적 `undefined`와 생략을 실제 엔진이 다르게 취급함

- **관련 스펙 텍스트**: `spectec/document/js-api/index.bs`의 `Table.prototype.set`(line 1100-1113)/`grow`(line 1057-1071) — 둘 다 `"If |value| is missing, Let |ref| be [=DefaultValue=](|elementtype|). ... Otherwise, Let |ref| be [=?=] [=ToWebAssemblyValue=](|value|, |elementtype|)."` 형태. `|value|`가 "missing"이 되는 조건은 `webidl/index.bs`의 overload resolution algorithm(line 11540-11780)이 정의: `optional` 인자에 대해 실제로 넘어온 JS 값 `V`가 (default value가 없다면) 리터럴 `undefined`이면 — **생략됐든 명시적으로 `undefined`를 넘겼든 값 비교만으로 판정하므로 둘을 구분하지 않고** — "missing" sentinel로 수렴한다.
- **실제 동작(Node v24.2.0, V8)**:
  ```js
  const table = new WebAssembly.Table({element: "anyfunc", initial: 1});
  table.grow(1);            // 성공 — missing → DefaultValue(funcref) → null ref
  table.grow(1, undefined); // TypeError — 진짜 값 변환(ToWebAssemblyValue)을 태움
  ```
- **대조군(같은 파일, 같은 관용구, 스펙대로 정확히 동작)**: `WebAssembly.Global`의 생성자(`constructor(GlobalDescriptor descriptor, optional any v)`, line 1141)도 구조적으로 완전히 동일한 "If |v| is missing, DefaultValue; Otherwise, ToWebAssemblyValue" 패턴이다. `i32`처럼 두 경로(`DefaultValue`/실제 변환)가 같은 결과를 내는 타입으로는 구분이 안 되지만, `i64`처럼 두 경로가 갈라지는 타입(`DefaultValue(i64)`는 성공해서 `0n`을 주지만, `ToBigInt64(undefined)`는 진짜로 `TypeError`를 던짐)으로 확인하면:
  ```js
  new WebAssembly.Global({value: "i64"});            // 성공, valueOf() = 0n
  new WebAssembly.Global({value: "i64"}, undefined);  // 성공, valueOf() = 0n (동일!)
  ```
  `Global` 생성자는 명시적 `undefined`를 정확히 스펙 문구대로 "missing"으로 collapse한다 — `DefaultValue(i64)` 경로를 탔다는 뜻(실제 값 변환을 탔다면 `ToBigInt64(undefined)`가 던져서 실패했어야 함).
- **의미**: 텍스트가 완전히 동일한 두 알고리즘(`Global` 생성자 vs `Table.set`/`grow`)이 실제 엔진에서 서로 다르게 동작한다 — `Table` 쪽만 "명시적 `undefined`"를 "missing"으로 collapse하지 않고 실제 타입 변환을 태운다. 스펙 텍스트 어디에도 이 둘을 다르게 취급해야 할 근거(주석, 예외 조항)가 없다 — 순수하게 실제 엔진(과 그걸 검증하는 공식 conformance 테스트)의 동작이 스펙 문구를 벗어난 것으로 보인다.
- **근거**: `table.set(0, undefined)`가 `TypeError`를 기대하는 테스트는 WJI가 만든 게 아니라 `spectec/test/js-api/table/get-set.any.js`(원본 upstream WPT, "Setting non-function" subtest)에서 그대로 가져온 공식 conformance 테스트 — 즉 실제 브라우저들이 이 동작으로 CI 검증받고 있다는 뜻.
- **WJI 쪽 처리**: 미정 — `Compiler.scala`의 `Cond.IsMissing` 컴파일(`value == undefined`, 스펙 문구 그대로)을 전역적으로 바꾸면 `Global` 생성자처럼 스펙대로 정확히 동작 중인 다른 케이스를 깨뜨릴 위험이 있어, `Table.set`/`grow` 한정 처리가 필요해 보임.
