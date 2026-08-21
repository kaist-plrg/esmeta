# `tests/wji/js-api/` — 공식 wasm js-api 테스트 이식 준비

`spectec/test/js-api/`(W3C/WHATWG가 관리하는 WebAssembly JS API의 공식
web-platform-tests 스위트, `testharness.js` 기반)를 WJI로 돌리기 위한 인프라가
사는 디렉터리입니다. 아직 실제 테스트 파일을 이식/실행하는 단계는 아니고,
그 전 단계인 "테스트 하네스 자체가 WJI(정확히는 정규식 없는 순수 JS 셸)에서
돌아가는가"를 검증하는 조각들만 있습니다.

```
shell-shim.js       필수 -- testharness.js/testharness-lite.js가 가정하는
                     브라우저/워커 전역(`self`)을 채워줌.
testharness-lite.js  필수 -- 진짜 testharness.js 대신 쓰는 경량 재구현.
report-shim.js       필수 -- 완료 후 subtest별 PASS/FAIL을 print로 출력.
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

## 로드 순서

`shell-shim.js` → `testharness-lite.js` → (테스트 파일이 `// META: script=`로
선언하는 의존 스크립트들) → 테스트 파일 본문 → `report-shim.js`(반드시 맨
마지막 — `add_completion_callback`이 등록되는 시점까지 큐에 쌓인
`promise_test`만 잡기 때문).

## `testharness-lite.js` 검증하기

`tests/wji/scripts/wji-harness-check`가 `spectec/test/js-api`의 각 테스트
파일을 real `testharness.js`와 `testharness-lite.js` 양쪽에 그대로 붙여
Node에서 돌리고, subtest별 PASS/FAIL이 일치하는지 자동 비교합니다:

```
tests/wji/scripts/wji-harness-check                              # 전체 corpus
node tests/wji/scripts/wji-harness-check.js <file.any.js>        # 파일 하나만
```

작성 시점 기준 52개 중 51개 MATCH, 1개(`js-string/basic.any.js`)는 SKIP —
이 파일은 host의 js-string builtins 미지원으로 real 쪽 `promise_test` 체인
자체가 끝나지 않아 ground truth를 세울 수 없는 케이스입니다(우리 하네스
문제 아님). `testharness-lite.js`를 고칠 때마다 이 스크립트로 재검증하세요.

이 스크립트는 어디까지나 "우리 하네스가 real testharness.js와 같은 판정을
내리는가"만 Node 대 Node로 비교합니다 — WJI 자체가 특정 테스트를
통과하는지는 별개 질문입니다(`sbt run wji-eval <파일> -silent`로 직접
확인).

## TODO: 실제 테스트를 어떻게 이식/실행할지는 아직 미정

여기까지는 "하네스가 동작한다"는 것만 확인된 상태고, 다음 질문들은 아직
답이 없습니다:

- `spectec/test/js-api`의 실제 `.any.js` 파일을 이 디렉터리에 그대로
  복사해 넣을지, 아니면 빌드/실행 시점에 원본 + 이 세 shim을 동적으로
  이어붙일지.
- subtest 여러 개 중 일부만 실패하는 걸 `EvalSpec`/`wjiEvalTest`가 어떻게
  판정할지 — 지금 `tests/wji/manual/`의 "파일 하나 = pass/fail 이진 판정"
  컨벤션(`__wjiOk`)과는 다른 보고 체계가 필요해 보임(subtest 단위로 알려진
  gap은 known-failing 처리하고 싶을 수 있음).
- gc/exception/tag/js-string처럼 WJI가 아직 안 다루는 표면을 건드리는
  파일들을 어느 시점까지 스코프 밖으로 미룰지.
