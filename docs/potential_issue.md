# Potential Issues

`docs/spec_errors.md`(명세 자체의 오타/결함), `docs/spec_inconsistencies.md`(같은
문서 안에서 패턴이 어긋나는 것), `docs/underspecified-behaviors.md`(명세가 아예
아무 말도 안 하는 것), `docs/engine_deviations.md`(스펙은 멀쩡한데 실제 엔진이
스펙 문구와 다르게 동작한다고 **확인된** 것)와 별개로 관리하는 목록이다.

여기 항목들은 이 넷 중 어디에도 정확히 들어맞지 않는다 — 명세 텍스트 자체는 (표면적으로는)
뭔가를 말하고 있고, WJI는 그걸 나름의 근거로 특정한 방식으로 해석해서 기계화했지만:

- 그 해석이 유일하게 타당한 해석이라는 확신은 없고,
- 공식 conformance 테스트(WPT)가 그 해석이 갈리는 입력 범위를 실제로 검증하지
  않아서, 우리 해석이 맞는지 틀린지조차 지금은 알 방법이 없고,
- 그래서 실제 브라우저 엔진들이 서로 다르게 구현했을 가능성이 낮지 않다.

즉 `docs/engine_deviations.md`가 "실제로 어긋난다고 확인된 것"을 기록하는 것과 달리,
여기는 아직 확인되지 않은 **위험 신호**를 기록하는 곳이다 — 나중에 관련 테스트가
추가되거나 실제 엔진에서 확인할 기회가 생기면, 결과에 따라
`docs/engine_deviations.md`로 승격되거나(실제로 어긋남이 확인된 경우) 그냥
제거될 수 있다(우리 해석이 맞았던 경우).

## 1. `fromCodePoint`/`fromCharCodeArray`/`intoCharCodeArray`의 i32 파라미터를 unsigned로 해석하기로 함 — 최상위 비트가 켜진 값에 대한 공식 테스트 없음

- **관련 스펙 텍스트**: `spectec/document/js-api/index.bs`의 `fromCodePoint`(line 2046, "If |v| > 0x10ffff, throw a trap"), `fromCharCodeArray`(line 1986, "If |start| > |end| or |end| > |length|"), `intoCharCodeArray`(line 2014, "If |start| + |stringLength| > |arrayLength|") — 셋 다 funcType이 `i32`로 선언한 파라미터를 아무 변환 명시 없이 곧바로 산술/비교에 사용한다 (`docs/spec_errors.md` #33).
- **WJI의 해석**: `SpecPatch` #60/#61/#62에서 이 값들을 **unsigned**로 해석해서 수학값으로 변환한다(`|start| interpreted as a [=mathematical value=]`, 부호 재해석 없이 raw payload를 그대로 사용). 즉 최상위 비트가 켜진 i32 값(예: `0x80000000`)은 매우 큰 양수로 취급되어, `fromCodePoint`라면 곧바로 트랩한다.
- **왜 확신이 없는지**: 이 파라미터들이 반드시 unsigned로 해석되어야 한다고 스펙이 명시하지는 않는다. `ToJSValue`(index.bs:1383-1385)는 i32 값을 JS에 노출할 때 `[=signed_32=]`(부호 있는 32비트)로 해석하는데, 이건 이 코퍼스에 정의된 유일한 i32→수학값 변환 관례다. 만약 signed로 해석했다면, 최상위 비트가 켜진 값은 음수가 되어 `fromCodePoint`의 상한 체크(`> 0x10ffff`)를 통과해버려 트랩을 안 하게 된다 — 즉 signed냐 unsigned냐에 따라 관찰 가능한 동작(트랩 여부)이 실제로 갈린다.
- **왜 테스트로 확인이 안 되는지**: `spectec/test/js-api/js-string/basic.any.js`의 `testCodePoints`/`testCharCodes`(line 192-193)는 `[1, 2, 3, 10, 0x7f, 0xff, 0xfffe, 0xffff, 0x10000, 0x10001]`뿐이다 — 전부 작은 양수이고, 최상위 비트가 켜진 값이나 음수는 아예 없다. `testExternRefValues`(다른 edge case들을 담은 배열)도 `fromCodePoint`/`fromCharCode`엔 안 쓰인다. 즉 이 경계값을 시험하는 공식 테스트가 존재하지 않는다.
- **다음 단계**: 실제 브라우저(V8 등)에서 `new WebAssembly.Instance(...)`로 js-string 빌트인을 직접 가져와 `fromCodePoint(0x80000000)` 같은 호출을 실제로 관찰해볼 수 있으면, 그 결과에 따라 이 항목을 `docs/engine_deviations.md`로 옮기거나(우리 해석이 실제 엔진과 다르면) 제거할 수 있다(우리 해석이 맞았으면).
