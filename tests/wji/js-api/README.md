# `tests/wji/js-api/` — 공식 wasm js-api 테스트

`spectec/test/js-api/`(W3C/WHATWG가 관리하는 WebAssembly JS API의 공식
web-platform-tests 스위트, `testharness.js` 기반)를 WJI로 돌리기 위한
인프라와, 거기서 생성한 실제 테스트 케이스가 사는 디렉터리입니다.

```
shell-shim.js         필수 -- testharness.js/testharness-lite.js가 가정하는
                       브라우저/워커 전역(`self`)을 채워줌.
testharness-lite.js    필수 -- 진짜 testharness.js 대신 쓰는 경량 재구현.
report-shim.js         필수 -- subtest별 PASS/FAIL을 print로 출력하고,
                       전부 통과했을 때만 globalThis.__wjiOk = true 세팅
                       (tests/wji/manual/*.js의 컨벤션과 동일).
generated/             `tests/wji/scripts/wji-generate-js-api-tests.js`가
                       spectec/test/js-api에서 만들어낸 self-contained
                       테스트 케이스들. 손으로 고치지 말 것 -- 다시 생성됨.
```

## 왜 진짜 `testharness.js`를 못 쓰는가

`spectec/test/harness/testharness.js`(공식 W3C 테스트 하네스, 5000줄 넘음)를
그대로 WJI에 태우면 로드 단계에서부터 `[NotSupported] feature/RegExp`로
죽습니다 — ESMeta의 ECMA-262 기계화가 정규식 리터럴 평가를 아예 구현 안 했고
(`Interpreter.scala`의 `RegularExpressionLiteral` 케이스가 무조건
`NotSupported`를 던짐), `testharness.js`는 자기 자신의 스크립트 경로를
알아내는 등 파일 로드 시점에 무조건 실행되는 초기화 코드 안에서부터 정규식을
씁니다.

`spectec/test/js-api`의 실제 테스트 파일 52개 + 카테고리별 `assertions.js` +
`wasm-module-builder.js`를 전수 조사한 결과, **정규식은 `testharness.js`
안에만 있고 그 외 어디에도 없었습니다.** 또한 `testharness.js`가 제공하는
~30개의 `assert_*` 중 실제로 corpus에서 쓰이는 건 12개뿐이었습니다
(`test`/`promise_test`/`setup`/`format_value`와 함께). 그래서 "패치해서
정규식만 없애기"가 아니라, 실제로 쓰이는 부분만 알고리즘 단위로 그대로
옮겨 새로 짜는 쪽을 택했습니다 — `testharness-lite.js`가 그 결과물입니다
(자세한 이식 범위/의도적으로 뺀 부분은 그 파일 자신의 doc comment 참고).

## `generated/`는 어떻게 만들어지나

`tests/wji/scripts/wji-generate-js-api-tests.js`가 `spectec/test/js-api` 밑
모든 `*.any.js` 파일을 재귀로 순회하며, 각 파일의 `// META: script=` 의존성을
읽어 `shell-shim.js` + `testharness-lite.js` + 그 의존 스크립트들 + 테스트
본문 + `report-shim.js`를 하나로 이어붙인 self-contained `.any.js` 파일을
`generated/`에 씁니다(spectec 쪽 디렉터리 구조 그대로 미러링 — `toString.any.js`
처럼 카테고리마다 이름이 겹치는 파일이 있어서 필요).

`scripts/wat2js`와 같은 철학입니다: 저작/동기화 시점 편의 스크립트일 뿐 빌드
의존성이 아니고, `generated/`가 지금 `spectec/test/js-api`와 실제로
일치하는지 자동으로 확인하는 장치는 없습니다. `spectec` 서브모듈을 bump해서
js-api corpus가 바뀌었으면 손으로 다시 돌리세요:

```
node tests/wji/scripts/wji-generate-js-api-tests.js
```

**스코프**: 제외 없이 `spectec/test/js-api` 전체(`gc/`/`exception/`/`tag/`/
`js-string/` 포함, 52개 전부). 이전엔 이 넷을 "최신 wasm proposal이라 WJI가
전혀 기계화 안 했을 것"이라는 카테고리 이름만 보고 뺐었는데, 실제로는
`tests/wji/manual/wasm-throw-propagation.js`가 이미 `WebAssembly.Tag`/
`Exception`을 end-to-end로 통과시키고 있어서 근거가 틀렸던 걸로 드러났습니다
— 검증 없이 배제하지 말고 전부 생성해서 `knownFailing`으로 개별 gap을
드러내는 쪽으로 정정.

## 실행과 판정

`tests/wji/manual/`과 완전히 같은 파이프라인을 씁니다 — `EvalSpec`이
`WJI_JS_API_TEST_DIR`(`tests/wji/js-api/generated`)도 같이 순회하고,
`__wjiOk`를 똑같이 확인합니다. 다만 `report-shim.js`가 그 값을 "이 파일
안의 모든 subtest가 다 통과했는가"로 집계해서 세팅한다는 점이 다릅니다 —
그래서 **subtest 하나만 실패해도 파일 전체가 FAIL**로 잡힙니다. 어느
subtest가 왜 깨졌는지는 `sbt run wji-eval tests/wji/js-api/generated/<path> -silent`로
직접 돌려서 print 출력을 보면 됩니다.

```
sbt --client wjiEvalTest
```

파일 단위 테스트 이름은 `"js-api/<spectec 기준 상대경로>"`(예:
`"js-api/memory/toString.any.js"`)입니다. 아직 WJI가 못 다루는 gap에
막힌 파일은 `tests/wji/manual/`과 똑같이 지우지 말고 `EvalSpec.scala`의
`knownFailing`에 등록(`cancel()`)하고, 고쳐지면 빼세요.

## `testharness-lite.js` 자체를 검증하기

`tests/wji/scripts/wji-harness-check`가 `spectec/test/js-api`의 각 테스트
파일을 real `testharness.js`와 `testharness-lite.js` 양쪽에 그대로 붙여
Node에서 돌리고, subtest별 PASS/FAIL이 일치하는지 자동 비교합니다 — WJI가
아니라 우리 하네스 구현 자체가 맞는지만 보는 도구입니다:

```
tests/wji/scripts/wji-harness-check                              # 전체 corpus
node tests/wji/scripts/wji-harness-check.js <file.any.js>        # 파일 하나만
```

작성 시점 기준 52개 중 51개 MATCH, 1개(`js-string/basic.any.js`)는 SKIP —
이 파일은 host의 js-string builtins 미지원으로 real 쪽 `promise_test` 체인
자체가 끝나지 않아 ground truth를 세울 수 없는 케이스입니다(우리 하네스
문제 아님). `testharness-lite.js`를 고칠 때마다 이 스크립트로 재검증하세요.
