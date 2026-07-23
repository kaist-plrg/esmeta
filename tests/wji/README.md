# `tests/wji/` fixtures

WJI 상호작용 테스트 fixture들이 사는 디렉터리입니다. `tests/es/`(순수 ECMAScript
fixture들)와 같은 컨벤션을 미러링하되, `WebAssembly.*` 관련 fixture를 위해
`.wat` sidecar가 하나 더 있습니다.

```
<name>.js      필수 -- 실제로 실행/리뷰되는, standalone self-checking fixture.
               wasm 바이트가 이미 `Uint8Array` 리터럴로 inline돼 있습니다.
<name>.wat     선택 -- 그 바이트를 만든 WAT 소스 (provenance/재생성용).
```

## fixture 작성 컨벤션: `.js` 하나로 self-checking

`.ir` sidecar 같은 별도 assert 파일 없이, `.js` 파일 하나가 그 자체로
standalone 테스트입니다. 규칙은 하나: **모든 체크(동기든 비동기든)가
통과했을 때만, fixture 스스로 맨 마지막에 `globalThis.__wjiOk = true;`를
세팅**합니다.

- 동기적으로 검증 가능한 부분은 그냥 평범하게 `throw`로 assert하세요 (읽기
  좋고, 실제 브라우저/Node에 그대로 갖다 놔도 동일하게 동작합니다).
- **`WebAssembly.instantiate(...).then(cb)`처럼 아무도 반환값을 안 받는
  `.then()` 콜백 안에서는 `throw`만으로는 안 됩니다** — ECMA-262 스펙상
  `NewPromiseReactionJob`이 그 `throw`를 "아무도 안 보는 파생 promise를
  reject시키는 것"으로 조용히 흡수해버려서, `RunJobs`의 최종 결과
  (`GLOBAL_RESULT`)엔 전혀 안 잡힙니다 (직접 `Promise.resolve().then(() =>
  { throw ... })`로 확인함 — `@RESULT`가 그래도 `undefined`로 남음). 그러니
  async 콜백 안에서도 가독성을 위해 `throw`는 그대로 쓰되, 콜백 맨 끝에
  성공 시에만 `globalThis.__wjiOk = true;`를 반드시 추가하세요.
- harness(`EvalSpec`, `src/test/scala/esmeta/wji/EvalSpec.scala`)는 실행 후
  `GLOBAL_RESULT === Undef`(동기 throw 잡힘) **그리고** `__wjiOk === true`
  (async 실패나, promise가 애초에 안 불린 경우까지 잡힘) 둘 다 확인합니다.

## `.wat`에서 `.js`용 바이트 만들기

저장소 자체엔 WAT 컴파일러가 없습니다. 외부 `wat2wasm`(WABT 툴체인)이 PATH에
있다면:

```
scripts/wat2js tests/wji/<name>.wat
```

가 `new Uint8Array([...])` 스니펫을 stdout에 출력합니다 — 그걸 `.js` fixture에
직접 붙여넣으세요. 이건 저작 시점 편의 도구일 뿐 빌드 의존성이 아닙니다:
`wat2wasm`이 없어도 `sbt compile`/`sbt test`엔 전혀 영향 없습니다.

`.wat`을 수정했다면 `.js`에 inline된 바이트 배열도 손으로 다시 붙여넣어야
합니다 — 이 두 파일이 서로 일치하는지 자동으로 검증하는 장치는 없습니다.

## 실행

이 디렉터리를 순회하며 실제로 실행/검증하는 `EvalSpec`
(`src/test/scala/esmeta/wji/EvalSpec.scala`)은 기본 `sbt test` 티어에는 없는
opt-in task입니다 (fixture마다 외부 SpecTec 프로세스를 새로 띄우는 비용 때문):

```
sbt --client wjiEvalTest
```

## 아직 기계화가 안 된 부분에 막힌 fixture

새 fixture가 진짜 버그가 아니라 WJI가 아직 못 다루는 스펙 구문/동작에 막히는
경우가 있습니다. 이런 경우엔 fixture를 지우지 말고, `EvalSpec.scala`의
`knownFailing` map에 파일명과 이유(및 `personal/TODO.md` 항목 참조)를
추가하세요 — 실행 대신 `cancel(reason)`으로 처리돼서 `wjiEvalTest`는 계속
초록불을 유지하면서 gap은 계속 추적됩니다. gap이 실제로 해결되면
`knownFailing`에서 그 항목을 지우세요.
