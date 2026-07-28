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
