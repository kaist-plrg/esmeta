# ESMeta Errors

ESMeta 자체(스펙이 아니라 mainline 구현) 코드에서 발견한 버그 목록입니다.

## 1. `getLocals`의 부족한 인자 케이스가 예외를 던지지 않음

- **File**: `src/main/scala/esmeta/interpreter/Interpreter.scala`, `getLocals`'s `aux` helper
- **Current**: `else RemainingParams(ps)`
- **Expected**:
  ```scala
  else
    // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
    // (mirrors the symmetric remaining-args case just below)
    callee match
      case _: Cont =>
      case _       => throw RemainingParams(ps)
  ```
- **Reason**: `aux`가 파라미터 개수보다 인자가 적게 들어온(그리고 해당 파라미터가 `optional`이 아닌) 경우, `RemainingParams` 예외 객체를 생성만 하고 버린 채 그대로 통과시켜버렸다 — 바로 아래의 대칭 케이스(`(Nil, args)`, 인자가 남는 경우)는 `throw RemainingArgs(args)`로 제대로 예외를 던지는데, 이쪽만 `throw`가 빠져 있었다. 결과적으로 해당 파라미터는 `map`에 바인딩되지 않은 채 조용히 넘어가고, 이후 그 파라미터를 참조하는 시점에서야 훨씬 더 알아보기 힘든 `unknown variable: <name>` 에러로 나타난다. WJI의 `toHostFunc`(SpecTec가 재진입 호출하는 host function 클로저를 빌드하는 곳)이 인자를 위치별로 스플랫해서 넘기다가(파라미터 1개 vs. 인자 0개인 상황) 이 경로를 밟으면서 발견됨.

  단순히 `throw`만 추가하면 `esmeta.interpreter.EvalTinyTest`의 `cont.ir` 테스트가 깨졌다 — GeneratorStart/GeneratorResume이 실제로 파라미터 수보다 적은 인자로 continuation을 재개하는 정당한 케이스가 있고, 바로 아래 "remaining args" 케이스는 이미 `callee`가 `Cont`일 때 관대하게 봐주고 있었다(`XXX Handle GeneratorStart <-> GeneratorResume arith mismatch` 주석). 대칭을 맞춰 "remaining params" 쪽도 `Cont`일 때만 예외로 두고 나머지(`Clo` 등)는 던지도록 했다.
