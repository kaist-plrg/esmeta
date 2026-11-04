# Underspecified Behaviors

`docs/spec_errors.md`(명세 자체의 오타/결함), `docs/spec_inconsistencies.md`(같은
문서 안에서 패턴이 어긋나는 것)와 별개로 관리하는 목록이다. 여기 항목들은 스펙이
"틀렸다"거나 "일관성이 없다"기보다, **애초에 그 동작이 무엇을 뜻하는지 아무 말도
하지 않고 있어** 우리가 추론해서 채워 넣은 경우를 기록한다 — 스펙 저자에게 "이
부분을 명시해달라"고 보고할 가치가 있는 목록이라 별도로 관리한다.

## 1. Platform object가 아닌 값에 대해 "a new {{X}} with the internal slots ..."가 무엇을 호출해야 하는지 적혀있지 않음

- **File**: `spectec/document/js-api/index.bs`, lines 830, 842 (`create a fixed length memory buffer` / `create a resizable memory buffer`)
- **현재 텍스트**:
  ```
  1. Let |buffer| be a new {{ArrayBuffer}} with the internal slots
     [[ArrayBufferData]], [[ArrayBufferByteLength]], and [[ArrayBufferDetachKey]].
  ```
- **문제**: WebIDL은 "a [=/new=] {{X}}"(platform object 생성) 형태에 대해서는
  "internally create a new object implementing the interface"
  (`webidl/index.bs:13827`)라는 실제 알고리즘을 갖추고 있다. 그러나 이는
  js-api/WebIDL이 직접 정의한 interface(`Memory`, `Instance`, `Global` 등)에만
  적용된다 — `interface prototype object`, `inclusive inherited interfaces`처럼
  IDL interface 정의 자체(상속 체인, extended attribute)에 깊이 의존하는
  절차이기 때문이다. `ArrayBuffer`는 js-api가 정의한 interface가 아니라
  ECMA-262가 이미 정의해둔 것을 그대로 가져다 쓰는 것이므로, 애초에 이 절차의
  대상이 아니다.

  그런데 정작 위 인용문처럼 "internal slots를 나열하며 새 {{ArrayBuffer}}를
  만들라"는 phrasing이 실제로 무엇을 호출해야 하는지는 명세 어디에도 적혀 있지
  않다 — 링크도, 각주도, ECMA-262 쪽으로의 참조도 없다. 비슷한 용도의 다른
  알고리즘(`webidl/index.bs:9269`, "create an ArrayBuffer from a byte
  sequence")도 있지만, 이는 이미 있는 byte sequence를 새로 할당한 버퍼에
  복사해 넣는 용도(`[=ArrayBuffer/Write=] bytes into arrayBuffer`)라 별개의
  절차이며, `[[ArrayBufferDetachKey]]` 같은 커스텀 슬롯도 다루지 않아 위
  인용문이 가리키는 대상과는 다르다. 위 알고리즘 원문 자체도 이 절차를 호출하지
  않고, 슬롯 레벨로 직접 풀어썼다.
- **우리가 추론한 것**: ECMA-262가 내부 슬롯을 가진 exotic object를 새로 만들
  때 실제로 사용하는 절차가 `AllocateArrayBuffer(constructor, byteLength)`라는
  점을 근거로, `[$AllocateArrayBuffer$](%ArrayBuffer%, 0)`을 호출한 뒤 그
  결과의 슬롯들을 스펙이 명시한 대로 다시 덮어쓰는 식으로 처리했다
  (`esmeta.wji.compiler.lowering.ExpandNewArrayBufferPass`). 이것이 "명세가
  실제로 의도한 바"라고 확신할 근거는 없으며, ECMA-262 관례상 가장 자연스러운
  해석일 뿐이다.
- **왜 좋은 명세가 아니라고 보는지**: WebIDL은 platform object 생성에 대해서는
  명확한 알고리즘을 하나 갖추고 있으면서도, ECMA-262 내장 타입을 "내부 슬롯을
  나열하며 새로 만드는" 이 흔한 패턴에는 대응하는 절차를 전혀 두지 않았다.
  매번 mechanize하는 쪽이 "당연히 이런 뜻이겠거니" 추론해야 하는 상황이므로,
  명세로서 완결성이 떨어진다고 본다.

## 2. Interface object / interface prototype object가 realm별로 어디에, 언제 캐싱되는지 적혀있지 않음 (생성 순환 참조)

- **File**: `webidl/index.bs`, lines 11935-11992 (`create an interface object`),
  12046-12099 (`create an interface prototype object`)
- **현재 텍스트**:
  ```
  (create an interface object)
  1.  Let |F| be CreateBuiltinFunction(|steps|, |length|, |id|, « [[Unforgeables]] », |realm|, |constructorProto|).
  1.  Let |proto| be the result of creating an interface prototype object of interface |I| in |realm|.

  (create an interface prototype object)
  1.  If the [LegacyNoInterfaceObject] extended attribute was not specified on |interface|, then:
      1.  Let |constructor| be the [=interface object=] of |interface| in |realm|.
      ...
      1.  Perform ! DefinePropertyOrThrow(|interfaceProtoObj|, "constructor", |desc|).
  ```
- **문제**: 명세는 interface object와 interface prototype object를 realm마다
  interface당 하나씩 존재하는 값으로 다룬다("There will exist an interface
  prototype object for every interface defined", line 12040). `create ...`
  알고리즘은 그 값을 *어떻게 만드는지*만 정의하고, "the [=interface object=]
  of |I| in |realm|" 같은 참조는 이미 존재하는 그 값을 가리킨다. 그런데 이 값이
  realm의 어디에 저장되는지, 언제 생성되는지(eager/lazy)는 명세 어디에도 적혀
  있지 않다. 그 결과:
  - `create an interface object`가 `create an interface prototype object`를
    호출하는데, 후자는 line 12094에서 "the [=interface object=] of
    |interface| in |realm|"를 참조한다. 이 값은 지금 만들고 있는 |F| 자신이지만
    아직 어디에도 등록되지 않았으므로, 문자 그대로 따르면 순환 참조가 된다.
  - 부모 interface의 interface object(line 11963)와 interface prototype
    object(line 12055)도 같은 참조를 쓴다. `define the global property
    references`는 부모가 먼저 오도록 정렬하지만(line 13883), `create a
    namespace object`(line 14837, `[LegacyNamespace]` 경로 — js-api의
    `WebAssembly.*`가 이 경로)는 정렬하지 않는다.
  - `internally create a new object implementing the interface`(line
    13832/13839)와 legacy factory function(line 12030)도 "the [=interface
    prototype object=] for |I| in |realm|"를 참조한다. 이 참조를 `create an
    interface prototype object` 호출로 풀어 해석하면 인스턴스마다 새 prototype이
    생겨 `Object.getPrototypeOf(new WebAssembly.Module(b)) ===
    WebAssembly.Module.prototype`이 거짓이 된다.
  - `[LegacyNoInterfaceObject]` interface는 interface object 없이 interface
    prototype object만 가지므로, prototype 쪽 캐시가 interface object 생성의
    부수효과로만 채워질 수는 없다.
- **우리가 추론한 것**: 실제 엔진들(Blink의 per-context data, Gecko의
  ProtoAndIfaceCache)처럼 realm별 캐시를 둔다.
  `realm.HOST_DEFINED.interfaceObjects[id]`와
  `realm.HOST_DEFINED.interfacePrototypeObjects[id]`에 저장하고,
  `create an interface object`는 `CreateBuiltinFunction` 직후(prototype 생성
  *전*)에 |F|를 캐시에 등록한다. 그러면 line 12094의 조회가 |F|를 돌려주므로,
  명세의 `constructor` 정의 단계를 그대로 쓸 수 있다. "the [=interface
  object=] / [=interface prototype object=] of |I| in |realm|" 참조는 이 캐시
  조회로 해석한다. 이는 `SpecPatch`의 *(suggestion)* 항목으로 명시적인 캐싱
  단계를 추가해 처리했다.
- **왜 좋은 명세가 아니라고 보는지**: 두 객체가 realm별 싱글턴이라는 전제가
  prose 곳곳에 암묵적으로만 깔려 있고, 저장 위치와 생성 시점이 빠져 있어서
  생성 알고리즘이 문자 그대로는 순환 참조를 일으킨다. 구현체는 모두 캐시로 이
  문제를 해결하고 있으므로, 명세도 "realm마다 interface → interface object /
  interface prototype object 맵을 두고, 생성 시 등록하고 참조 시 조회
  (없으면 생성)한다"는 규칙을 명시하는 편이 낫다.

## 3. Interface object가 constructor라는 사실(`[[Construct]]` internal method)이 알고리즘 어디에도 적혀있지 않음

- **File**: `webidl/index.bs`, lines 11979-11984 (`create an interface object`);
  upstream https://webidl.spec.whatwg.org/#create-an-interface-object
- **현재 텍스트**:
  ```
  1.  Let |F| be CreateBuiltinFunction(|steps|, |length|, |id|, « [[Unforgeables]] », |realm|, |constructorProto|).
  1.  Let |proto| be the result of creating an interface prototype object of interface |I| in |realm|.
  1.  Perform ! DefinePropertyOrThrow(|F|, "prototype",
      PropertyDescriptor{[[Value]]: |proto|, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false}).
  ```
- **문제**: ECMA-262는 built-in function object가 `[[Construct]]` internal
  method를 가지는 경우를 "'constructor'라고 기술되어 있거나, 어떤 알고리즘이
  명시적으로 `[[Construct]]`를 설정한 경우"로 한정한다. `CreateBuiltinFunction`
  자체는 `[[Construct]]`를 설치하지 않는다. ECMA-262는 고정된 이름의 intrinsic에는
  전자를, 런타임에 알고리즘이 만드는 constructor에는 후자(`MakeConstructor`,
  `BoundFunctionCreate`, `ProxyCreate`)를 쓴다.

  Interface object는 이 알고리즘이 런타임에 만드는 객체인데, 이 알고리즘(과
  여기서 호출하는 알고리즘) 어디에서도 `[[Construct]]`를 설정하지 않는다. 섹션
  prose는 "the interface object can be called as a constructor"(line 11904)라고
  말하지만, 이를 실현하는 단계가 없다. 문자 그대로 따르면 `IsConstructor(|F|)`가
  거짓이라 `new`가 `EvaluateNew`에서 `|steps|`에 도달하기 전에 `TypeError`를
  던진다. 스펙 본문에도 이 공백이 `Issue(whatwg/webidl#698)`(line 2475)로 남아
  있다 — 즉 "틀린 단계"라기보다 아직 정해지지 않은 부분이다.

  같은 공백이 `legacy factory functions`(line 12029-12032)에도 있다. js-api는
  legacy factory function을 선언하지 않으므로 여기서는 손대지 않았다.
- **우리가 추론한 것**: 모든 interface object를 constructor로 본다. constructor
  operation이 없는 interface도 마찬가지다 — 그 경우 `|steps|`가 처음부터
  `TypeError`를 던지고, WPT도 이를 전제로 모든 interface에 대해
  `IsConstructor`를 검사한다(web-platform-tests/wpt#28089). `"prototype"`을
  정의하는 단계를 `[$MakeConstructor$](|F|, false, |proto|)`로 바꿔
  처리했다(`SpecPatch` #78). `MakeConstructor`의 built-in function object 분기
  (tc39/ecma262#2209)는 `[[Construct]]`를 설정한 뒤 기존
  `DefinePropertyOrThrow`와 똑같은 descriptor로 `"prototype"`을 정의하므로,
  다른 동작은 바뀌지 않는다. 섹션 prose도 "interface object는 built-in
  function object이자 constructor이다"와 "constructor operation이 있을 때만
  생성이 성공한다"를 분리해 적는 편이 자연스럽다.
- **왜 좋은 명세가 아니라고 보는지**: "interface object는 constructor"라는
  전제가 prose와 WPT, 구현체 모두에 깔려 있지만, ECMA-262가 요구하는 방식(명시적
  `[[Construct]]` 설정)으로는 어디에도 적혀 있지 않다. 어떤 방식으로 표시할지
  (`MakeConstructor` 재사용 여부 등)도 아직 열려 있으므로, 명세 저자에게
  명시를 요청할 가치가 있다.
- **관련 이슈**:
  - https://github.com/whatwg/webidl/issues/698 — "Define that interface
    objects are constructors" (open)
  - https://github.com/web-platform-tests/wpt/pull/28089 — "Test
    IsConstructor for all interfaces" (merged)
  - https://github.com/tc39/ecma262/issues/2209 — Web IDL이
    `CreateBuiltinFunction` 결과를 constructor로 표시하는 방법
