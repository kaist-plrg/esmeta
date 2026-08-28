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

## 3. 조건문 문법에 "X is TYPE that has a [[SLOT]] internal slot" 관계절 형태가 없음

- **File**: `src/main/scala/esmeta/lang/util/Parser.scala`, `baseCond`/`typeCheckCond`
- **Current**: `typeCheckCond = expr ~ isEither(singleLangType)` — "EXPR is/is not TYPE"만 소비하고 끝남. `hasFieldCond`가 "EXPR has a [[SLOT]] internal slot"은 따로 지원하지만, 이 둘을 "TYPE **that** has a [[SLOT]] ..."처럼 관계절로 묶는 규칙은 아예 없었다.
- **Expected**: `typeCheckWithFieldCond`라는 새 alternative를 `baseCond`(전부 `|||`, longest-match)에 추가 — `ref ~ isEither(singleLangType) ~ ("that" ~> hasNeg) ~ field ~ form ~ fieldType`를 파싱해 `CompoundCondition(TypeCheckCondition(...), And, HasFieldCondition(...))`로 구성한다. 컴파일러 쪽(`esmeta.compiler.Compiler`)은 `CompoundCondition`을 이미 범용으로 처리하므로 추가 작업이 필요 없었다.
- **Reason**: 예: `%TypedArray%.prototype.set`의 "If `_source_` is an Object that has a [[TypedArrayName]] internal slot, then ..."(ecma262/spec.html:42183). `typeCheckCond`가 "is an Object"까지만 삼키고 남은 "that has a [[TypedArrayName]] internal slot"을 못 삼켜서 조건 전체 파싱이 실패하고, 결과적으로 `Cond.Unknown`(원문 그대로 보존) → 컴파일 시 `EYet(원문)` → 실행 시점에 `[NotSupported] metalanguage/...`로 떨어졌다. ecma262 전체에서 이 관용구는 딱 4곳뿐([[TypedArrayName]] 1곳, [[SyncIteratorRecord]] 3곳)이라 범위는 좁지만, `%TypedArray%.prototype.set`이 WJI의 `wasm-module-builder.js`(공식 js-api 테스트 corpus 대부분이 wasm 모듈 바이트 조립에 씀)가 내부적으로 쓰는 함수라 실질적 영향은 컸다(52개 js-api 테스트 중 24개가 이 gap에 막혀있었음).

  발견 경위: `[[SyncIteratorRecord]]` 3곳은 `src/main/resources/manuals/rule.json`에 손으로 넣어둔 "manual rule"(정규식으로 못 잡는 조건을 문자열 그대로 매칭해 대체 IR을 주입하는 우회)로 이미 우회돼 있었다 — 고치고 나니 `compilerTest`(`esmeta.compiler.ValiditySmallTest`)가 "there are unused manual rules"로 정확히 이 사실을 잡아내서, 이제 안 쓰는 그 rule.json 항목을 지웠다. `extractorValidityTest`가 자동 갱신한 `src/main/resources/result/spec-summary` 골든에 개선폭이 그대로 남는다: `algorithms complete 2503 → 2507`, `algorithm steps complete 21358 → 21362`(`equals` 지표는 2662 → 2658로 소폭 하락하는데, 이건 `CompoundCondition`으로 재구성된 조건의 pretty-print가 원문 "that" 어투를 그대로 복원하지 않아서 생기는 예상된 결과 — `TypeCheckCondition(...) and HasFieldCondition(...)`로 렌더링되므로 원문과 글자 그대로는 달라지지만 의미상 동등하고, 관련 함수는 여전히 "complete"로 잡힘).

## 4. `EContains`가 List만 상정하고 String을 못 받음

- **File**: `src/main/scala/esmeta/interpreter/Interpreter.scala`, `eval(Expr)`의 `EContains` 케이스
- **Before**:
  ```scala
  case EContains(list, elem) =>
    val l = eval(list).asList(st)
    val e = eval(elem)
    Bool(l.values.contains(e))
  ```
- **After**: `eval(list)`가 `Str`이면 `elem`을 `CodeUnit`으로 보고 그 문자열이 해당 code unit을 포함하는지 직접 검사하고, 그 외에는 기존처럼 `asList(st)`로 List를 기대하도록 분기.
- **Reason**: ecma262/spec.html:1257은 "contains"를 "List 안에서 값을 찾는 용도"로 소개하지만, 실제로는 String도 "code unit들의 순서 있는 시퀀스"(String 타입 자체의 정의)라서 스펙 곳곳에서 String에도 그대로 재사용된다 — `Encode`의 `_alwaysUnescaped_`/`_unescapedSet_`("the string-concatenation of ..."로 정의되어 명백히 String), Annex B `escape`의 `_unescapedSet_`, `decodeURI`/`decodeURIComponent`가 쓰는 `Decode`의 `_preserveEscapeSet_`(파라미터 헤더에 `: a String`으로 명시), `parseInt`의 `_S_`(`TrimString`의 결과라 String) 등. `ContainsCondition`은 파싱/컴파일 단계에서 대상이 List인지 String인지 구분하지 않고 그대로 `EContains`로 컴파일되므로, 이 gap은 파서/컴파일러가 아니라 순수하게 인터프리터의 런타임 타입 처리에만 있었다.

  다만 이 gap은 지금까지 한 번도 관측된 적이 없었다 — `parseInt`는 이 "contains" 스텝보다 앞선 다른 스텝들(코드 유닛 리터럴 비교 등)이 이미 미기계화 상태라 실행이 그 앞에서 먼저 멈췄고, `escape`/`Encode`는 애초에 이번 세션 전까지 `escape`/`unescape` 자체가 추출조차 안 됐다(`docs/esmeta_changes.md` #1/#2/#3 참고) — 세 겹의 이전 gap을 차례로 걷어내고 나서야 `gc/casts.tentative.any.js`(WJI js-api 테스트)를 통해 처음으로 이 코드 경로에 도달해 드러났다.
