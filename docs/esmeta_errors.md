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

## 2. `ValiditySmallTest`의 fingerprint 갱신 코드가 `fail` 뒤에 있어 죽은 코드

- **File**: `src/test/scala/esmeta/cfg/ValiditySmallTest.scala`, `init`
- **Current**:
  ```scala
  if (prev != cur) {
    fail(
      "function/node IDs have changed -- CFG fingerprint mismatch:" +
      s"\n* previous: $prev" +
      s"\n* current : $cur",
    )
    dumpFile(cur, path)
  }
  ```
- **Expected**: `dumpFile(cur, path)`가 `fail(...)`보다 먼저 실행되거나, `SnapshotSpec`류가 쓰는 `-Dupdate=true` 방식처럼 별도 플래그로 갱신 여부를 가르는 형태.
- **Reason**: ScalaTest의 `fail(...)`은 그 자리에서 즉시 `TestFailedException`을 던지므로, 같은 블록 안에서 `fail` 다음에 오는 `dumpFile(cur, path)`는 실행될 일이 없다 — fingerprint가 바뀌어 테스트가 실패해도 골든 파일(`src/main/resources/result/cfg-fingerprint`)이 자동으로는 절대 갱신되지 않는다. WJI lowering pass 순서를 바꾸면서 merged CFG(WJI+mainline)의 함수/노드 ID가 실제로 바뀌어 이 테스트가 실패했을 때 발견했다 — 재실행해도 계속 같은 mismatch로 실패해서 이상하다 싶어 코드를 보니 `dumpFile`이 도달 불가능한 위치에 있었다. 그때는 `fail`/`dumpFile` 순서를 임시로 바꿔 한 번 돌려서 골든을 수동 갱신한 뒤 원상복구하는 식으로 우회했다.
