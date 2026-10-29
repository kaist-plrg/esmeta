# Test-Spec Mismatches

공식 conformance 테스트(`spectec/test/js-api/`, upstream WPT와 동일)가 **스펙과 다른 동작을
기대하는** 경우를 기록한다. 이런 테스트는 보통 실제 엔진들의 동작을 그대로 반영하고 있어서
엔진들도 같이 스펙에서 벗어나 있다(그 엔진 쪽 위반은 `docs/engine_bugs.md`에 기록). WJI는 스펙 문구를 그대로 기계화하고 테스트나 엔진에
맞추지 않으므로, 해당 subtest는 WJI에서 알려진 실패가 된다.

항목마다: 어긋나는 테스트와 스펙 텍스트, 엔진 쪽 동작(자세한 건 `docs/engine_bugs.md`), 업스트림 상황.

## 1. `table/get-set.any.js` "Setting non-function" — `table.set(0, undefined)`에 `TypeError`를 기대함

- **테스트**: anyfunc 테이블에 대해 `[undefined, true, false, "test", Symbol(), 7, NaN, {}]` 각각으로
  `table.set(0, v)`가 `TypeError`를 던지기를 기대한다. 세 엔진 통과, WJI는 `undefined`에서 실패
  (던지지 않음).
- **스펙**: `Table.set`은 `undefined set(AddressValue index, optional any value);`이고 알고리즘이
  `"If |value| is missing, Let |ref| be [=DefaultValue=](|elementtype|). ... Otherwise, Let |ref| be
  [=?=] [=ToWebAssemblyValue=](|value|, |elementtype|)."`로 갈린다. "missing"은 `webidl/index.bs`의
  overload resolution algorithm(line 11540-11780)이 정의하며, default 없는 `optional` 인자에 넘어온
  값이 `undefined`이면 **생략이든 명시적 `undefined`든 구분 없이** "missing"이 된다. 따라서
  `table.set(0, undefined)`는 `table.set(0)`과 같은 호출이고, `DefaultValue(funcref)` = null이
  저장되어야 한다. 같은 파일의 "Arguments for anyfunc table set"은 `table.set(0)`이 null을 저장하기를
  기대하므로, 두 subtest가 스펙상 동일한 두 호출에 대해 서로 다른 결과를 기대하는 셈이다.
- **엔진 동작**: 세 엔진 모두 테스트대로 `TypeError`를 던진다 — 명시적 `undefined`를 실제 값
  변환(`ToWebAssemblyValue`)으로 태우는 엔진 쪽 스펙 위반이며, `Table.grow`·생성자들까지 포함한
  엔진별 동작은 `docs/engine_bugs.md` #19에 정리.
- **테스트 스위트 안의 모순**: `global/constructor.any.js` "Explicit value undefined for type i64"는
  `new WebAssembly.Global({value: "i64"}, undefined)`가 `0n`이기를 기대한다. 실제 값 변환이었다면
  `ToBigInt64(undefined)`가 던지므로, 이 테스트는 명시적 `undefined`를 "missing"으로 취급하라고
  요구하는 셈이고, 세 엔진 모두 통과한다. 즉 구조가 같은 두 API에 대해 공식 테스트끼리 서로
  반대되는 규칙을 기대한다. (`table.grow(n, undefined)`, anyfunc `Table`/`Global` 생성자에
  `undefined`를 넘기는 경우는 공식 테스트가 없다.)
- **업스트림**: 규칙 자체는 WebAssembly/spec#1660(2023, `Global` 생성자 중심)에서 질문됐으나 결론
  없이 열려 있음. get-set 테스트를 스펙에 맞추고 `table.grow(2, undefined)` 테스트를 추가하는 수정을
  WebAssembly/spec#2307로 제출, 2026-10-28 머지됨 — 테스트가 틀린 쪽으로 확정. WJI가 고정한 commit의
  코퍼스엔 아직 수정 전 테스트가 있어서, 해당 subtest는 알려진 실패로 그대로 둔다.
