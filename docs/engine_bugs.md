# Engine Bugs

`docs/engine_deviations.md`와는 성격이 다른 목록이다 — 그쪽은 "스펙은 멀쩡한데 실제 엔진이
스펙과 다르게 동작해서, WJI가 의도적으로 스펙 문구를 벗어나 실제 엔진에 맞추기로 한 선택"을
기록하는 곳이고, 여기는 `tests/wji/scripts/diff-engines`(V8/SpiderMonkey/JavaScriptCore
세 엔진에 같은 테스트를 돌려 교차검증하는 도구 — 기본 대상은
`tests/wji/manual/`+`tests/wji/complementary/`지만 파일 인자로 `tests/wji/js-api/generated/`
(공식 wpt 코퍼스)도 돌려볼 수 있음)로 발견한, **세 엔진 중 하나(또는 둘)만 다른 둘과 다르게
동작하는 경우**를 기록한다.
WJI가 뭘 어떻게 할지 결정할 필요는 없다(WJI는 스펙대로 기계화하면 그만) — 이건 순수하게
"실제 엔진 버그일 가능성이 있다, 업스트림에 제보할 가치가 있어 보인다"는 관찰 기록.

항목마다: 어떤 엔진이 다른지, 최소 재현, 기대 동작(스펙 + 다른 두 엔진의 합의) vs 실제 동작,
확인한 엔진 버전/커밋, 확신도(재현 확인 여부, 업스트림 제보 여부).

## 1. SpiderMonkey: `new.target.prototype` getter가 던진 예외를 WebAssembly 생성자 7개 전부에서 "out of memory"로 뭉갬

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/constructor-prototype-getter-throws.js`
  — V8/JSC는 PASS, SpiderMonkey만 FAIL.
- **최소 재현**:
  ```js
  function makeFakeTarget(thrown) {
    return new Proxy(function () {}, {
      get(target, prop, receiver) {
        if (prop === "prototype") throw thrown;
        return Reflect.get(target, prop, receiver);
      },
    });
  }
  const thrown = new Error("boom");
  try {
    Reflect.construct(WebAssembly.Table, [{ element: "anyfunc", initial: 1 }], makeFakeTarget(thrown));
  } catch (e) {
    print(e);                 // SpiderMonkey: "out of memory" / V8, JSC: "Error: boom"
    print(e === thrown);      // SpiderMonkey: false            / V8, JSC: true
  }
  ```
- **기대 동작**: WebIDL의 "internally create a new object implementing the interface" 전제
  (`new.target.prototype`을 `GetPrototypeFromConstructor`로 읽는 과정이 reentrant — 임의의
  JS를 실행할 수 있음, `new.target`이 Proxy면 그 `get` trap이 그 JS) — 이 trap이 예외를
  던지면 생성자 호출 전체가 그 예외로 그대로 abrupt completion 해야 함(`?` 전파). V8/JSC
  둘 다 원본 `Error` 객체를 참조 동일성까지 그대로 보존해서 전파함 — 스펙이 요구하는 동작과
  일치.
- **실제 동작(SpiderMonkey)**: 원본 예외 대신 매번 새로운 `InternalError: out of memory`를
  던짐 — 원본 메시지/타입/참조가 전부 사라짐. 실제 메모리 부족과는 무관해 보임(동일 인자로
  평범한 `new WebAssembly.Table(...)`은 정상 동작 — 아래 "확인한 것" 참고). `GetPrototypeFromConstructor`
  를 호출하는 **7개 생성자 전부**(`Table`/`Global`/`Memory`/`Module`/`Instance`/`Tag`/`Exception`)
  에서 동일하게 재현 — 생성자 하나의 우연이 아니라 그 공통 내부 구현(아마 SpiderMonkey
  자신의 WebAssembly 생성자들이 공유하는 `GetPrototypeFromConstructor`에 대응하는 C++
  헬퍼)의 체계적인 버그로 보임.
- **확인한 것**:
  - 평범한(Proxy 아닌) `new WebAssembly.Table({element: "anyfunc", initial: 1})`은 SpiderMonkey에서
    정상 동작 — 이 버그는 "Table 생성 자체가 메모리를 많이 씀" 같은 리소스 문제가 아니라,
    `new.target`이 Proxy이고 그 `prototype` getter가 던질 때만 트리거됨.
  - 7개 생성자 전부에서 독립적으로 재현(스크립트로 전수 확인, 매번 `caught=out of memory,
    sameRef=false`).
- **확인한 엔진 버전**: SpiderMonkey, `FIREFOX_158_0_RELEASE`
  (`3c71aeacdb296402e41fb2084b13e797d1770642`, `tests/wji/scripts/install_engine/sm.sh`로
  빌드) vs V8 15.5.35.25(`v8.sh`)/WebKit `main` HEAD 2026-10-16(`jsc.sh`) — 둘 다 정상.
- **상태**: 후보(candidate) — 로컬 재현만 확인, 업스트림(Bugzilla) 제보는 아직 안 함, 최신
  Firefox 안정 버전/Nightly에서도 재현되는지는 미확인.

## 2. SpiderMonkey/JSC: `WebAssembly.compile`/`.instantiate`의 `options` 변환 실패가 Promise reject 대신 동기 throw로 escape

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/compile-options-conversion-throws.js`
  — V8만 PASS.
- **배경**: WebIDL의 Promise 반환 오퍼레이션 관례상, 인자(여기선 `options`) 변환 중 발생한
  예외도 동기적으로 던지면 안 되고 **반환된 Promise를 그 에러로 reject**해야 함(`?`/`!`가
  아니라 "a new promise ... 변환이 abrupt면 그 Promise를 reject"라는 별도 단계로 명시).
  `options`의 `builtins` getter가 던지는 경우로 테스트.
- **최소 재현**:
  ```js
  // compile()
  try {
    var p = WebAssembly.compile(emptyModule, { get builtins() { throw new TypeError("boom"); } });
    p.then(ok, (e) => print("rejected: " + e));  // 기대: 여기 도달, reject됨
  } catch (e) { print("SYNC THROW: " + e); }      // 실제: 일부 엔진은 여기로 옴

  // instantiate()도 동일 패턴
  ```
- **실제 동작** (직접 isolate해서 확인, 둘을 따로 테스트해야 정확히 구분됨 — 같은 파일에
  둘 다 들어있어서 처음엔 뭉뚱그려 보임):
  | | `compile()` | `instantiate()` |
  |---|---|---|
  | V8 | reject (정상) | reject (정상) |
  | SpiderMonkey | reject (정상) | **동기 throw** |
  | JSC | **동기 throw** | **동기 throw** |
- **의미**: JSC는 둘 다, SpiderMonkey는 `instantiate()`만 — 겹치지만 완전히 같은 범위는
  아닌 두 개의 별개 버그로 보임(JSC가 더 넓은 범위). 둘 다 "Promise 반환 오퍼레이션은
  인자 변환 실패도 reject로"라는 같은 WebIDL 관례를 어김.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 3. JSC: 활성화된 builtin set에 없는 컴포넌트 이름을 import하면 `importObject`로 폴백하지 않고 바로 실패

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/import-builtin-fallback-to-import-object.js`
  — V8/SpiderMonkey PASS, JSC만 FAIL.
- **배경**: `read the imports`의 폴백 규칙(index.bs:498-500): "`o`가 Object가 아니거나
  `HasProperty(o, componentName)`가 false면, `o`를 `?Get(importObject, moduleName)`로
  교체." — 즉 `{ builtins: ["js-string"] }`로 활성화했어도, 실제 js-string builtin set이
  제공 안 하는 멤버 이름을 import하면 **진짜 `importObject`로 폴백**해야 함.
- **실제 동작(JSC)**: `new WebAssembly.Instance(mod, { "wasm:js-string": { nonExistentFn: fn } })`가
  폴백 없이 즉시 `TypeError: import wasm:js-string:nonExistentFn is not a valid builtin
  reference`를 던짐 — `importObject`가 그 이름을 실제로 제공하는지 아예 확인 안 함.
- **의미**: js-string-builtins는 비교적 최근/계속 진화 중인 제안이라, "폴백" 규칙 자체가
  JSC가 구현한 초안 버전에선 아직 없었을 가능성도 있음(버그라기보단 아직 안 따라잡은
  스펙 revision일 수도) — 확신도를 낮춰서 기록.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보, 확신도 낮음(스펙 revision 차이일 가능성 있음) — 업스트림 제보 안 함.

## 4. SpiderMonkey: v128 타입 GLOBAL import에 일반 값을 주면 LinkError 대신 TypeError

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/import-type-mismatch-v128-and-tag.js`
  — V8/JSC PASS, SpiderMonkey만 FAIL.
- **배경**: `read the imports`의 v128 전용 규칙: "If valtype is v128, throw a LinkError
  exception" — **무조건**(공급된 JS 값이 뭐든 상관없이, `WebAssembly.Global`을 안 거치고
  바로 일반 값을 줘도) LinkError여야 함.
- **실제 동작(SpiderMonkey)**: `(module (import "m" "g" (global v128)))`에 일반 `Number`를
  import object로 주면 `LinkError`가 아니라 `TypeError: cannot pass value to or from
  JS: expected true, got false`를 던짐 — 에러 **타입 자체**가 스펙과 다름(이 "early,
  무조건 LinkError" 규칙을 안 타고, 더 뒤쪽의 범용 타입 변환 체크로 떨어지는 것으로 보임).
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 5. JSC: `Memory.prototype.toResizableBuffer()`가 `maximum` 미선언 시 TypeError를 안 던짐

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/memory-resizable-buffer.js`
  — V8/SpiderMonkey PASS, JSC만 FAIL.
- **배경**: `toResizableBuffer()`는 `maximum`이 선언 안 된 Memory에 대해서는 TypeError를
  던져야 함(resizable buffer의 상한을 알 수 없으므로). 이 메서드 자체는 이 테스트
  작성 시점(이번 세션 이전)엔 **Node 24.2.0을 포함해 어느 엔진에도 구현 안 돼 있었음**
  (테스트 파일 자신의 doc에 이미 기록) — 그새 V8/SpiderMonkey는 구현을 따라잡았고 이
  특정 케이스까지 맞게 구현했는데, JSC는 메서드 자체는 있지만(`typeof === "function"`
  확인) 이 TypeError 케이스를 안 챙김(그냥 성공해서 resizable `ArrayBuffer`를 반환).
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보, 확신도 중간(아직 계속 구현 중인 최신 기능이라 "버그"보다 "미완성"에
  가까울 수 있음) — 업스트림 제보 안 함.

## 6. JSC: v128 global `.value` 읽기가 TypeError 대신 RuntimeError를 던짐

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/v128-global-and-function-rejected.js`
  — V8/SpiderMonkey PASS, JSC만 FAIL.
- **배경**: `GetGlobalValue`가 v128 타입 global의 값을 JS로 가져오려 하면 TypeError를
  던져야 함(JS엔 v128을 표현할 값이 없으므로).
- **실제 동작(JSC)**: `exports.g.value` 읽기가 `RuntimeError: Cannot get value of v128
  global`을 던짐 — 4번 항목과 같은 패턴의 "에러 타입 자체가 다름"(wasm 레벨 trap처럼
  보이는 `RuntimeError`를 쓰는데, 이건 JS-API 레벨의 타입 변환 실패지 wasm 트랩이 아님).
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 7. V8/SpiderMonkey: `options.builtins` 리스트의 중복 이름 검증 미구현(JSC는 구현됨)

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/validate-duplicate-builtins.js`
  — JSC만 PASS, V8/SpiderMonkey 둘 다 FAIL.
- **배경**: 이미 테스트 작성 시점(이번 세션 이전 스윕)에 "Node 24.2.0은 이 옵션 검증
  자체를 전혀 구현 안 함"이 테스트 파일 자신의 doc에 기록돼 있던, **사실상 이미 알려진
  gap** — 새 발견이라기보단 세 엔진 교차비교로 범위가 더 분명해진 경우.
- **실제 동작**: `WebAssembly.validate(module, { builtins: ["js-string", "js-string"] })`가
  스펙대로면 중복 때문에 `false`를 반환해야 하는데 — V8은 그냥 `true`를 반환(검증
  자체를 안 함), SpiderMonkey는 `false`를 반환하는 대신 **`TypeError: duplicate
  builtin`을 던짐**(검증은 하지만 반환값이 아니라 예외로 처리 — 이것도 스펙과 다른
  모양). JSC만 스펙대로 `false`를 반환.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 이미 알려진 gap(V8 쪽)의 재확인 + SpiderMonkey 쪽 추가 관찰 — 업스트림
  제보 안 함.

## 8. SpiderMonkey: `Table.prototype.set`의 `.length`가 1이 아니라 2

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/js-api/generated/interface.any.js`
  (공식 wpt 코퍼스를 파일 인자로 직접 돌려본 경우) — V8/JSC PASS, SpiderMonkey만 FAIL
  (`interface.any.js`의 "WebAssembly.Table.set: length" 서브테스트).
- **배경**: WebIDL 선언(`webidl/index.bs`, `interface Table { ... undefined
  set(AddressValue index, optional any value); ... }`)에서 `value`가
  `optional`이므로, WebIDL의 표준 `.length` 계산 규칙상 `Table.prototype.set.length`는
  **1**이어야 함(뒤에 required 인자가 없는 한, optional 인자는 `.length`에 안 잡힘).
- **실제 동작(SpiderMonkey)**: `WebAssembly.Table.prototype.set.length === 2` —
  `value`를 required처럼 `.length`에 포함시킴. V8/JSC는 둘 다 정확히 `1`.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인(공식 wpt subtest 자체가 바로 이 사실을 테스트함,
  `assert_equals: expected 1 but got 2`), 업스트림 제보 안 함.

## 9. SpiderMonkey: `Exception` 생성자의 `.length`가 2가 아니라 1

- **발견 경로**: `tests/wji/scripts/diff-engines`를 `tests/wji/js-api/generated/`
  전체(52개, 서브디렉터리 포함)에 돌려본 경우 —
  `js-api/generated/exception/constructor.tentative.any.js`에서 V8/JSC PASS,
  SpiderMonkey만 FAIL ("length" 서브테스트).
- **배경**: WebIDL 선언(`webidl/index.bs`, `interface Exception { constructor(Tag
  exceptionTag, sequence<any> payload, optional ExceptionOptions options = {});
  ... }`)에서 앞의 두 인자(`exceptionTag`/`payload`)가 required, 세 번째(`options`)만
  `optional`이므로 `.length`는 **2**여야 함 — 8번 항목(`Table.set`)과 정반대 방향의
  같은 종류의 버그(이번엔 과다가 아니라 **과소** 계산).
- **실제 동작(SpiderMonkey)**: `WebAssembly.Exception.length === 1`. V8/JSC는 둘 다
  정확히 `2`.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인(`assert_equals: expected 2 but got 1`), 업스트림
  제보 안 함.

## 10. JSC: `Exception.prototype.getArg`의 범위 밖 index가 TypeError 대신 RangeError

- **발견 경로**: 위와 같은 전체 실행 — `js-api/generated/exception/getArg.tentative.any.js`
  에서 V8/SpiderMonkey PASS, JSC만 FAIL ("Getting out-of-range argument" 서브테스트).
- **배경**: WebIDL 선언(`webidl/index.bs`, `any getArg(Tag exceptionTag, [EnforceRange]
  unsigned long index);`)의 `index`에 `[EnforceRange]`가 붙어있음 — WebIDL 스펙 자체가
  `[EnforceRange]` 정수 변환이 범위를 벗어나면 **TypeError**를 던지도록 명시(다른 어떤
  예외 타입도 아님).
- **실제 동작(JSC)**: 범위 밖 index(테스트는 매우 큰 수를 줌)에 대해 `RangeError: Expect
  an integer argument in the range: [0, 2^32 - 1]`을 던짐 — 메시지 내용 자체는 맞는
  설명이지만 **예외 타입이 스펙과 다름**(`[EnforceRange]`는 항상 TypeError). V8/SpiderMonkey
  둘 다 TypeError.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 11. SpiderMonkey: `WebAssembly.Global`의 value setter를 인자 없이 호출하면 실제로는 안 던져야 하는데 던짐

- **발견 경로**: 위와 같은 전체 실행 — `js-api/generated/global/value-get-set.any.js`
  에서 V8/JSC PASS, SpiderMonkey만 FAIL ("Calling setter without argument" 서브테스트).
- **실제 동작(SpiderMonkey)**: setter를 인자 0개로 호출(`setter.call(global)`류)하면
  `TypeError: At least 1 argument required, but only 0 passed`를 던짐 — V8/JSC는 둘 다
  안 던지고 정상 처리(인자가 없으면 `undefined`가 암묵적으로 주어진 것처럼 동작해야
  함 — WebIDL의 접근자 프로퍼티 호출 관례).
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 12. SpiderMonkey: U+FFFD(유니코드 대체 문자)가 포함된 커스텀 섹션 이름 매칭이 스펙과 다름

- **발견 경로**: 위와 같은 전체 실행 — `js-api/generated/module/customSections.any.js`
  에서 V8/JSC PASS, SpiderMonkey만 FAIL ("Custom sections with U+FFFD" 서브테스트,
  `assert_equals: expected 0 but got 1`).
- **의미**: 잘못된 UTF-8 바이트 시퀀스를 포함한 커스텀 섹션 이름을 디코딩/매칭하는
  과정에서 SpiderMonkey가 V8/JSC와 다른 결과를 냄(정확한 바이트 재현/스펙 조항은
  아직 안 따라감 — 바로 이어서 할 작업으로 남겨둠).
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보, 확신도 낮음(세부 바이트/스펙 조항 미확인) — 업스트림 제보 안 함.

## 13. JSC: `TableDescriptor`의 멤버 읽기 순서가 스펙과 다름

- **발견 경로**: 위와 같은 전체 실행 — `js-api/generated/table/constructor.any.js`
  에서 V8/SpiderMonkey PASS, JSC만 FAIL ("Order of evaluation for descriptor"
  서브테스트, `assert_array_equals: property 0 expected "element" but got
  "address"`).
- **의미**: WebIDL dictionary 변환은 멤버를 선언 순서대로 `Get`해야 하는데(각 멤버
  읽기 자체가 getter를 통해 관찰 가능한 side effect를 가질 수 있어 순서가 명세돼
  있음), JSC가 `TableDescriptor`의 `address`를 `element`보다 먼저 읽음 — V8/SpiderMonkey는
  둘 다 `element`를 먼저 읽어 스펙과 일치.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보 — 로컬 재현만 확인, 업스트림 제보 안 함.

## 14. JSC: nontrivial rec group 안에 있는 타입으로 선언된 js-string-builtins import가 validate를 통과해버림

- **발견 경로**: 위와 같은 전체 실행 — `js-api/generated/js-string/basic.any.js`에서
  V8/SpiderMonkey PASS, JSC만 FAIL ("Incorrect types" 서브테스트).
- **배경**(`spectec/test/js-api/js-string/basic.any.js`의 "Incorrect types" 서브테스트
  직접 확인): `wasm:js-string` import를 "nontrivial rec group"(재귀 타입 그룹) 안에 있는
  함수 타입으로 선언하면, js-string-builtins 제안 자체가 요구하는 "trivial(비재귀) 타입만
  허용" 규칙 때문에 `WebAssembly.validate`는 `false`를, `new WebAssembly.Module(...)`는
  `CompileError`를 내야 함.
- **실제 동작(JSC)**: `WebAssembly.validate(...)`가 `true`를 반환 — 이 특정 "rec group
  안의 타입"이라는 무효 조건을 검증 안 함.
- **의미**: 3번 항목(builtin 미지원 멤버 폴백)과 마찬가지로 js-string-builtins가
  비교적 최근/계속 진화 중인 제안이라, JSC의 구현이 이 특정 타입-검증 규칙을 아직
  안 따라잡았을 가능성 있음 — 버그라기보단 미완성에 가까울 수 있어 확신도를 낮춰서
  기록.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 후보, 확신도 낮음 — 업스트림 제보 안 함.

## 15. V8: `WebAssembly.validate`가 `options` 인자를 아예 읽지 않음

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/validate-options.js`
  — SpiderMonkey/JSC PASS, V8만 FAIL.
- **기대 동작**: `validate(bytes, optional WebAssemblyCompileOptions options = {})`의 `options`는
  다른 WebIDL dictionary와 똑같이 변환되므로, 멤버 getter가 던지면 그 예외가 전파돼야 함.
  SpiderMonkey/JSC는 그대로 전파.
- **실제 동작(V8)**: `WebAssembly.validate(emptyModule, { get builtins() { throw err; } })`가
  getter를 호출조차 하지 않고 `true`를 반환. Node 26.12.1도 같음.
- **의미**: #7(V8이 `builtins` 중복 검증을 안 함)의 더 근본적인 원인으로 보임 — 중복 검사 이전에
  `validate`가 `options` 자체를 무시함. `compile`/`Module` 쪽은 V8도 `options`를 읽음.
- **확인한 엔진 버전**: 위 1번과 동일(V8은 Node 26.12.1로도 확인).
- **상태**: 재현 확인, 업스트림 제보 안 함.

## 16. SpiderMonkey: funcref global import에 함수 아닌 값을 주면 LinkError 대신 TypeError

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/global-import-ref-conversion-fails.js`
  — V8/JSC PASS, SpiderMonkey만 FAIL.
- **기대 동작**: `read the imports`의 global import 처리에서 `ToWebAssemblyValue(v, valtype)`이
  TypeError를 던지면 "catch it, and throw a LinkError exception"(js-api index.bs). `(import "env" "g"
  (global funcref))`에 `{ env: { g: 5 } }`를 주면 LinkError. V8/JSC는 LinkError.
- **실제 동작(SpiderMonkey)**: `TypeError: can only pass WebAssembly exported functions to funcref`가
  그대로 나옴 — 변환 실패를 LinkError로 바꾸는 단계가 없음.
- **의미**: #4(v128 global import에서 같은 문제)와 같은 원인으로 보임 — SpiderMonkey가 global import
  값 변환 실패를 LinkError로 감싸지 않음.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 재현 확인, 업스트림 제보 안 함.

## 17. V8: `sequence<T>` 인자를 iterable이 아니라 array-like로 읽음

- **발견 경로**: `tests/wji/scripts/diff-engines tests/wji/complementary/sequence-conversion.js`
  — SpiderMonkey PASS, V8 FAIL(JSC는 #18 때문에 FAIL).
- **기대 동작**: WebIDL의 ES→IDL `sequence<T>` 변환("create a sequence from an iterable")은
  `@@iterator`로 순회함. 그래서 `Set` 같은 iterable도 받고, `@@iterator`가 던지면 그 예외가 전파됨.
  SpiderMonkey/JSC는 그대로 동작.
- **실제 동작(V8)**: `length`와 인덱스로 읽음 — `new WebAssembly.Exception(tag, new Set([7]))`이
  `TypeError: Exception values argument has no length`, `new WebAssembly.Tag({ parameters: new
  Set(["i32"]) })`도 `contains parameters without 'length'`로 실패. `@@iterator`가 던지는 객체도
  `@@iterator`를 부르지 않고 같은 TypeError. Node 26.12.1도 같음.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 재현 확인, 업스트림 제보 안 함.

## 18. JSC: `Exception`의 payload로 문자열을 받아들임

- **발견 경로**: 위와 같은 테스트 — JSC FAIL.
- **기대 동작**: `sequence<T>` 변환의 첫 단계가 "V가 Object가 아니면 TypeError"라서, 문자열은
  iterable이어도 TypeError. V8/SpiderMonkey는 TypeError.
- **실제 동작(JSC)**: `new WebAssembly.Exception(tag, "a")`가 성공함(문자열을 순회하는 것으로 보임).
  숫자(`5`)나 `{}`는 TypeError로 정상.
- **확인한 엔진 버전**: 위 1번과 동일.
- **상태**: 재현 확인, 업스트림 제보 안 함.
