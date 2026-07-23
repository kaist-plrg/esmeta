# `tests/wji/` fixtures

WJI 상호작용 테스트 fixture들이 사는 디렉터리입니다. `tests/es/`(순수 ECMAScript
fixture들)와 같은 컨벤션을 미러링하되, `WebAssembly.*` 관련 fixture를 위해
`.wat` sidecar가 하나 더 있습니다.

```
<name>.js      필수 -- 실제로 실행/리뷰되는 fixture. wasm 바이트가 이미
               `Uint8Array` 리터럴로 inline돼 있습니다.
<name>.wat     선택 -- 그 바이트를 만든 WAT 소스 (provenance/재생성용).
<name>.ir      선택 -- 실행 후 검증할 NormalInst assert들 (`tests/es/*.ir`와
               동일한 형식). 없으면 "크래시 없이 끝까지 도는지"만 확인하는
               스모크 테스트로 취급됩니다.
```

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
