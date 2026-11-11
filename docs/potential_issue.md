# Potential Issues

`docs/spec_errors.md`(명세 자체의 오타/결함), `docs/spec_inconsistencies.md`(같은
문서 안에서 패턴이 어긋나는 것), `docs/underspecified-behaviors.md`(명세가 아예
아무 말도 안 하는 것), `docs/engine_bugs.md`(실제 엔진이 스펙과 다르게 동작한다고
**확인된** 것), `docs/test_spec_mismatches.md`(공식 테스트가 스펙과 다른 동작을 기대한다고
**확인된** 것)와 별개로 관리하는 목록이다.

여기 항목들은 이 다섯 중 어디에도 정확히 들어맞지 않는다 — 명세 텍스트 자체는 (표면적으로는)
뭔가를 말하고 있고, WJI는 그걸 나름의 근거로 특정한 방식으로 해석해서 기계화했지만:

- 그 해석이 유일하게 타당한 해석이라는 확신은 없고,
- 공식 conformance 테스트(WPT)가 그 해석이 갈리는 입력 범위를 실제로 검증하지
  않아서, 우리 해석이 맞는지 틀린지조차 지금은 알 방법이 없고,
- 그래서 실제 브라우저 엔진들이 서로 다르게 구현했을 가능성이 낮지 않다.

즉 `docs/engine_bugs.md`/`docs/test_spec_mismatches.md`가 "실제로 어긋난다고 확인된 것"을 기록하는 것과 달리,
여기는 아직 확인되지 않은 **위험 신호**를 기록하는 곳이다 — 나중에 관련 테스트가
추가되거나 실제 엔진에서 확인할 기회가 생기면, 결과에 따라
그 문서들로 승격되거나(실제로 어긋남이 확인된 경우) 그냥
제거될 수 있다(우리 해석이 맞았던 경우).

## 1. (moved) `fromCodePoint`/`fromCharCodeArray`/`intoCharCodeArray`의 i32 파라미터를 unsigned로 해석하기로 함

`docs/spec_errors.md` #32로 승격. 이 항목이 "다음 단계"로 적어둔 엔진 확인을 해보니(2026-11-11) V8, SpiderMonkey, JSC 모두 음수 인덱스에서 trap해서 unsigned 해석이 맞았고, 원 제안서(`js-string-builtins` Overview)도 i32 파라미터마다 `>>>= 0`으로 unsigned 재해석을 명시함. 다만 이 항목을 쓴 뒤 `AddBuiltinFunctionHostfuncPass`가 인자를 전부 `[=ToJSValue=]`(signed)로 변환하게 바뀌어서, 지금 WJI는 이 항목의 설명과 달리 사실상 signed로 해석함. 명세 쪽 결함(값 영역 규약 부재)과 WJI의 현재 동작은 #32에 정리. 번호 유지를 위해 남겨 둠.
