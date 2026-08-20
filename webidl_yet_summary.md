# WebIDL 알고리즘별 yet 정리 (#1~#12)

`create a new object implementing the interface`를 시작점으로 타고 들어가며 실제로 호출되는
WebIDL 알고리즘들을 `esmeta/wji` 파서·컴파일러에 통과시켜보고, 남은 `(yet ...)` 항목들을 정리한
문서입니다.
현재까지 정리한 알고리즘은 다음과 같습니다.

- #1 : `internally_create_a_new_object_implementing_the_interface`
- #2 : `create_an_interface_prototype_object`
- #3 : `create_an_interface_object`
- #4 : `define_the_regular_attributes`
- #5 : `define_the_attributes`
- #6 : `attribute getter`
- #7 : `attribute setter`
- #8 : `define_the_regular_operations`
- #9 : `define_the_operations`
- #10: `creating_an_operation_function`
- #11: `compute_the_effective_overload_set`
- #12: `overload_resolution_algorithm`

카테고리 태그(`I-A`, `II-A` 등)는 `webidl_yet_categorized` 문서의 분류를 가리킵니다.

**⚠️ `#8` ~ `#12`를 위해 `Compiler.scala`에 넣은 임시 우회**: 위 3개를 추가하고 실제로
컴파일해보니, `compute_the_effective_overload_set`의 "range" 표현(카운팅 루프)이
`Compiler.compileExpr`가 "lowering 이후엔 절대 안 남아있어야 함"으로 간주해 온 `Expr.Range`
케이스를 실제로 건드리면서 `impossible()`을 던졌고, `Compiler.compile`이
`algos.flatMap(compileAlgo)`로 전체 알고리즘을 한 번에 처리하다 보니 이 예외 하나가 **전체
컴파일을 중단**시켜서 `ir.actual`이 아예 갱신되지 않는 문제가 있었습니다. 이걸 우회해서 실제
IR을 보기 위해, `Compiler.scala`의 모든 `impossible(...)` 호출을 `EYet("IMPOSSIBLE
(unreachable after lowering): ...")`으로 바꾸는 임시 하드코딩을 넣었습니다(각 함수 정의부에
"TEMPORARY (to be reverted)" 주석 포함). **이건 결과를 보고 이 문서를 정리하기 위한 임시
조치이고, 커밋/푸시하지 않고 곧 되돌릴 예정입니다.** `#8`~`#12` 섹션에서 `IMPOSSIBLE
(unreachable after lowering)`이라고 표시된 yet은 전부 원래는 "yet"이 아니라 컴파일러
크래시였던 자리입니다 — 별도 카테고리(**I-K**)로 분류했습니다.

각 항목은 다음 형식으로 정리합니다:

```
n. 카테고리 - <webidl_yet_categorized의 태그>
   원문 - <webidl/index.bs의 원문>
   IR - <ir.actual에 실제로 컴파일된 최종 IR>
```

**WJI 범위 밖 브랜치 제외 원칙**: 지금 WJI가 다루는 interface(`Module`/`Instance`/`Memory`/
`Table`/`Global`/`Tag`/`Exception`)나 namespace(WebAssembly)가 실제로는 절대 타지 않는다고
판단한 브랜치(예: interface 상속, 특정 extended attribute 조합)에 있는 yet은 이 문서에 별도로
정리하지 않았습니다. 예를 들어 이 7개 interface는 전부 `interface Foo : Bar` 형태의 WebIDL
상속을 쓰지 않으므로(`spectec/document/js-api/index.bs` 확인 결과), "I inherits from some
other interface P" 일 때의 브랜치는 정리하지 않았습니다.

---

## #1 `internally_create_a_new_object_implementing_the_interface`

- **명세 위치**: `webidl/index.bs:13827-13877`
- **하는 일**: `[Global]` 여부, 상속 체인, unforgeable 멤버 등을 반영해서 실제 platform object
  하나를 만드는, `Expr.New(iface)`가 최종적으로 의미하는 범용 알고리즘입니다.

```
1. 카테고리 - III-A
   원문 - Let |prototype| be the [=interface prototype object=] for |interface| in |realm|.
           - Set |prototype| to the [=interface prototype object=] for |interface| in |targetRealm|.
   IR - let prototype = (yet "the [=interface prototype object=] for |interface| in |realm|")
           - prototype = (yet "the [=interface prototype object=] for |interface| in |targetRealm|")

2. 카테고리 - I-O + IV-C
   원문 - [=list/iterate|For every=] [=interface=] |ancestor interface| in |interfaces|: ...
           - [=list|For each=] element |key| of |keys|: ...
   IR - call _ = clo<"list/iterate">((case "INTERFACE" ancestor interface interfaces))
           - (yet "foreach ?(element |key|) in ?(|keys|:)")

3. 카테고리 - III-A + I-A
   원문 - Let |unforgeables| be the value of the \[[Unforgeables]] slot of the [=interface object=] of |ancestor interface| in |realm|.
   IR - let unforgeables = (yet "the value of the \[[Unforgeables]] slot of the [=interface object=] of |ancestor interface| in |realm|")

4. 카테고리 - I-A
   원문 - Let |descriptor| be [=!=] |unforgeables|.\[[GetOwnProperty]](|key|).
   IR - let _comp4 = (yet "|unforgeables|.\[[GetOwnProperty]](|key|)")

5. 카테고리: II-A
   원문 - If |interface| is declared with the [{{Global}}] [=extended attribute=], then: ...
   IR - if (yet "declared with the [{{Global}}] [=extended attribute=]") { ... }

6. 카테고리: II-D
   원문 - Otherwise, if |interfaces| contains an [=interface=] which [=support indexed properties|supports indexed properties=], [=support named properties|named properties=], or both: ...
   IR - else if (yet "|interfaces| contains an [=interface=] which [=support indexed properties|supports indexed properties=]")

7. 카테고리: I-A + I-B
   원문 - Set |instance|.\[[SetPrototypeOf]] as defined in [[#platform-object-setprototypeof]].
        - Set |instance|.\[[GetOwnProperty]] as defined in [[#legacy-platform-object-getownproperty]].
           ...
   IR - (yet "Set |instance|.\[[SetPrototypeOf]] as defined in [[#platform-object-setprototypeof]]")
      - (yet "Set |instance|.\[[GetOwnProperty]] as defined in [[#legacy-platform-object-getownproperty]]")
           ...
```

---

## #2 `create_an_interface_prototype_object`

- **명세 위치**: `webidl/index.bs:12044-12099`
- **하는 일**: interface의 prototype 객체 하나를 만들고, `[Global]`/상속/`DOMException`/
  `is global prototype chain mutable` 등에 따라 그 prototype의 진짜 prototype과 내부 슬롯
  구성을 결정합니다.

```
1. 카테고리 - II-A + II-D
   원문 - If |interface| is declared with the [{{Global}}] [=extended attribute=], and |interface| [=support named properties|supports named properties=], ...
   IR - if (&& (yet "declared with the [{{Global}}] [=extended attribute=]") (yet "|interface| [=support named properties|supports named properties=]")) { ... }

2. 카테고리 - II-C
   원문 - Otherwise, if |interface| is declared to inherit from another interface, ...
   IR - else if (= interface (yet "declared to inherit from another interface")) { ... }

3. 카테고리 - II-J
   원문 - Otherwise, if |interface| is the {{DOMException}} [=interface=], ...
   IR - else if (= interface (yet "the {{DOMException}} [=interface=]")) { ... }

4. 카테고리 - II-J
   원문 - If |realm|'s [=is global prototype chain mutable=] is true, then: ...
   IR - if (= (yet "|realm|'s [=is global prototype chain mutable=]") true) { ... }

5. 카테고리 - II-A + II-C
   원문 - Otherwise, if |interface| is declared with the [{{Global}}] [=extended attribute=], or |interface| is in the set of [=inherited interfaces=] of an interface that is declared with the [{{Global}}] [=extended attribute=], then: ...
   IR - else if (|| (= interface (yet "declared with the [{{Global}}] [=extended attribute=],")) (= interface (yet "in the set of [=inherited interfaces=] of an interface that is declared with the [{{Global}}] [=extended attribute=]")))

6. 카테고리 - II-A
   원문 - If |interface| has any [=member=] declared with the [{{Unscopable}}] [=extended attribute=], then: ...
   IR - if (yet "|interface| has any [=member=] declared with the [{{Unscopable}}] [=extended attribute=]") { ... }

7. 카테고리: II-A
   원문 - If |interface| is not declared with the [{{Global}}] [=extended attribute=], then: ...
   IR - if (! (= interface (yet "declared with the [{{Global}}] [=extended attribute=]"))) { ... }

8. 카테고리 - II-A
   원문 - If the [{{LegacyNoInterfaceObject}}] [=extended attribute=] was not specified on |interface|, then: ...
   IR - if (yet "the [{{LegacyNoInterfaceObject}}] [=extended attribute=] was not specified on |interface|") { ... }

9. 카테고리 - III-A
   원문 - Let |constructor| be the [=interface object=] of |interface| in |realm|.
   IR - let constructor = (yet "the [=interface object=] of |interface| in |realm|")

10. 카테고리 - I-C
    원문 - Let |desc| be the PropertyDescriptor{\[[Writable]]: <emu-val>true</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>true</emu-val>, \[[Value]]: |constructor|}.
    IR - let desc = (yet "the PropertyDescriptor{\[[Writable]]: <emu-val>true</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>true</emu-val>, \[[Value]]: |constructor|}")
```

---

## #3 `create_an_interface_object`

- **명세 위치**: `webidl/index.bs:11933-11990`
- **하는 일**: interface의 생성자 함수 객체(`F`) 자체를 만듭니다. 이 함수 안에서
  `new I(...)`가 호출됐을 때 실제로 실행될 코드(`steps`)와, `F`를 만드는 시점에 한 번만
  실행되는 코드(prototype 연결, static 멤버 정의 등)가 한 알고리즘 안에 같이 들어있습니다.

먼저 `steps`로 묶이는, **생성자가 호출될 때마다 실행되는** 부분:

```
1. 카테고리 - VII-C
   원문 - Let |steps| be |I|'s [=overridden constructor steps=] if they exist, or the following steps otherwise:
   IR - if (yet "they exist") {
          let steps = (yet "|I|'s [=overridden constructor steps=]")
        } else {
          let steps = clo<"create_an_interface_object_closure1", [I, realm]>
        }

2. 카테고리 - II-A
   원문 - If |I| was not declared with a [=constructor operation=], ...
   IR - if (yet "|I| was not declared with a [=constructor operation=]") { ... }

3. 카테고리 - VII-A
   원문 - Let |args| be the passed arguments.
   IR - let args = (yet "the passed arguments")

5. 카테고리 - IV-C
   원문 - [=Compute the effective overload set=] for constructors with [=identifier=] |id| on [=interface=] |I| and with argument count |n|, and let |S| be the result.
   IR - call S = clo<"compute_the_effective_overload_set">((case "IDENTIFIER" id (case "INTERFACE" I n)))

6. 카테고리 - III-B
   원문 - Let &lt;|constructor|, |values|&gt; be the result of passing |S| and |args| to the [=overload resolution algorithm=].
   IR - let _tuple1 = (yet "passing |S| and |args| to the [=overload resolution algorithm=]")
        let constructor = _tuple1[0]
        let values = _tuple1[1]

7. 카테고리 - II-B + III-B
   원문 - Perform the [=constructor steps=] of |constructor| with |object| as [=this=] and |values| as the argument values.
   IR - (yet "Perform the [=constructor steps=] of |constructor| with |object| as [=this=] and |values| as the argument values")

8. 카테고리 - II-C
   원문 - If |I| inherits from some other interface |P|, then set |constructorProto| to the [=interface object=] of |P| in |realm|.
   IR - if (yet "|I| inherits from some other interface |P|") { ... }

9. 카테고리 - II-A
   원문 - If |I| was declared with a [=constructor operation=], then: ...
   IR - if (yet "|I| was declared with a [=constructor operation=]") { ... }

12. 카테고리 - IV-C
    원문 - Let |proto| be the result of [=create an interface prototype object|creating an interface prototype object=] of [=interface=] |I| in |realm|.
    IR - call proto = clo<"create_an_interface_prototype_object">((case "INTERFACE" I realm))

13. 카테고리 - I-C
    원문 - Perform [=!=] <a abstract-op>DefinePropertyOrThrow</a>(|F|, "<code>prototype</code>", PropertyDescriptor{\[[Value]]: |proto|, \[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>false</emu-val>}).
    IR - call _ = clo<"DefinePropertyOrThrow">(F, "<code>prototype</code>", (yet "PropertyDescriptor{\[[Value]]: |proto|, \[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>false</emu-val>}"))

14. 카테고리 - IV-C
    원문 - [=Define the constants=] of [=interface=] |I| on |F| given |realm|.
    IR - call _ = clo<"define_the_constants">((case "INTERFACE" I F realm))

15. 카테고리 - IV-C
    원문 - [=Define the static attributes=] of [=interface=] |I| on |F| given |realm|.
    IR - call _ = clo<"define_the_static_attributes">((case "INTERFACE" I F realm))

16. 카테고리 - IV-C
    원문 - [=Define the static operations=] of [=interface=] |I| on |F| given |realm|.
    IR - call _ = clo<"define_the_static_operations">((case "INTERFACE" I F realm))
```

---

## #4 `define_the_regular_attributes`

- **명세 위치**: `webidl/index.bs:12296-12302`
- **하는 일**: `definition`의 regular attribute 중 unforgeable이 아닌 것만 걸러서
  `define_the_attributes`에 넘깁니다.

```
2. 카테고리 - I-O + II-A + V
   원문 - [=list/Remove=] from |attributes| all the [=attributes=] that are [=unforgeable=].
   IR - call _ = clo<"list/remove">(attributes, ~attributes~, ~unforgeable~)
```

---

## #5 `define_the_attributes`

- **명세 위치**: `webidl/index.bs:12320-12344`
- **하는 일**: 걸러진 attribute 목록의 각 attribute마다 getter/setter를 만들어
  `DefinePropertyOrThrow`로 `target`에 매단다.

```
1. 카테고리 - I-O + IV-C
   원문 - [=list/For each=] [=attribute=] |attr| of |attributes|: ...
   IR - call _ = clo<"list/for_each">((case "ATTRIBUTE" attr attributes))

2. 카테고리 - III-A + IV-B
   원문 - If |attr| is not [=exposed=] in |realm|, then ...
   IR - if (! (= attr (case "EXPOSED" realm))) { ... }

3. 카테고리 - III-B
   원문 - Let |getter| be the result of creating an [=attribute getter=] given |attr|, |definition|, and |realm|.
   IR - let getter = (yet "an [=attribute getter=] given |attr|, |definition|, and |realm|")

4. 카테고리: III-B
   원문 - Let |setter| be the result of creating an [=attribute setter=] given |attr|, |definition|, and |realm|.
   IR - let setter = (yet "an [=attribute setter=] given |attr|, |definition|, and |realm|")

5. 카테고리 - IV-D
   원문 - Let |configurable| be <emu-val>false</emu-val> if |attr| is [=unforgeable=] and <emu-val>true</emu-val> otherwise.
   IR (현재) - if (= attr ~unforgeable~) {
                 let configurable = ~false~
               } else {
                 let configurable = ~true~
               }
   IR (목표) - if (&& (exists attr.extendedAttributes.unforgeable) (= attr.extendedAttributes.unforgeable true)) {
                 let configurable = false
               } else {
                 let configurable = true
               }

6. 카테고리 - I-C
   원문 - Let |desc| be the PropertyDescriptor{\[[Get]]: |getter|, \[[Set]]: |setter|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |configurable|}.
   IR - let desc = (yet "the PropertyDescriptor{\[[Get]]: |getter|, \[[Set]]: |setter|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |configurable|}")
   설명    - record 리터럴 문법을 파서가 못 읽습니다.

9. 카테고리 - II-G
   원문 - If |attr|'s type is an [=observable array type=] with type argument |T|, then: ...
   IR - if (= (yet "|attr|'s type") (yet "an [=observable array type=] with type argument |T|")) { ... }
```

---

## #6 `attribute getter`

- **명세 위치**: `webidl/index.bs:12348-12384`
- **하는 일**: 특정 attribute에 대한 getter 함수 객체를 만들어 반환합니다. 진짜 getter 로직은
  "Try running the following steps: ... And then, if an exception was thrown: ..."라는
  try/catch 형태의 `steps`로 감싸져 있고, 이 `steps`가 `CreateBuiltinFunction`에 그대로
  전달되어 실제 getter 함수의 바디가 됩니다.

```
2. 카테고리 - I-F
   원문 - Try running the following steps: ... And then, if <a lt="an exception was thrown">an exception |E| was thrown</a>:
   IR - (yet "Try running the following steps:")
        ...
        (yet "And then, if <a lt=\"an exception was thrown\">an exception |E| was thrown</a>:")

3. 카테고리 - II-H
   원문 - If |target| is an [=interface=], and |attribute| is a [=regular attribute=]: ...
   IR - if (&& (= target (yet "an [=interface=]")) (= attribute (yet "a [=regular attribute=]"))) { ... }

4. 카테고리 - I-P + VII-C
   원문 - Let |jsValue| be the <emu-val>this</emu-val> value, if it is not <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s [=realm/global object=] otherwise.
   IR - if (|| (! (= (yet "it") null)) (! (= (yet "it") undefined))) {
          let jsValue = (yet "the <emu-val>this</emu-val> value")
        } else {
          let jsValue = (yet "|realm|'s [=realm/global object=]")
        }

5. 카테고리 - VI
   원문 - (This will subsequently cause a {{TypeError}} in a few steps, if the global object does not implement |target| and [{{LegacyLenientThis}}] is not specified.) <!-- https://www.w3.org/Bugs/Public/show_bug.cgi?id=18547#c9 -->
   IR - (yet "(This will subsequently cause a {{TypeError}} in a few steps, ...) <!-- https://www.w3.org/Bugs/... -->")

6. 카테고리 - II-H
   원문 - If |jsValue| [=is a platform object=], then ...
   IR - if (jsValue: Unknown[platform object]) { ... }

7. 카테고리 - II-H
   원문 - If |jsValue| does not [=implement=] |target|, then: ...
   IR - if (yet "|jsValue| does not [=implement=] |target|") { ... }

8. 카테고리 - II-A
   원문 - If |attribute| was specified with the [{{LegacyLenientThis}}] [=extended attribute=], then return <emu-val>undefined</emu-val>.
   IR - if (yet "|attribute| was specified with the [{{LegacyLenientThis}}] [=extended attribute=]") { return undefined }

9. 카테고리 - I-O
   원문 - Otherwise, [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>.
   IR - call _ = clo<"javascript/throw">((record [TypeError] {...}))

10. 카테고리 - II-G
    원문 - If |attribute|'s type is an [=observable array type=], ...
    IR - if (= (yet "|attribute|'s type") (yet "an [=observable array type=]")) { ... }

11. 카테고리 - I-N
    원문 - Set |idlObject| to the IDL [=interface type=] value that represents a reference to |jsValue|.
    IR - idlObject = (yet "the IDL [=interface type=] value that represents a reference to |jsValue|")

12. 카테고리 - II-B + III-B
    원문 - Let |R| be the result of running the [=getter steps=] of |attribute| with |idlObject| as [=this=].
    IR - let R = (yet "running the [=getter steps=] of |attribute| with |idlObject| as [=this=]")

13. 카테고리 - II-G
    원문 - If |attribute|'s type is a [=promise type=], ...
    IR - if (= (yet "|attribute|'s type") (yet "a [=promise type=]")) { ... }

14. 카테고리 - I-F + VII
    원문 - Otherwise, end these steps and allow the exception to propagate.
    IR - (yet "end these steps and allow the exception to propagate")

15. 카테고리 - I-J
    원문 - Let |name| be the string "<code>get </code>" prepended to |attribute|'s [=identifier=].
    IR - let name = @@yet: unresolved ref: Unknown(string "<code>get </code>" prepended to |attribute|).identifier
```

---

## #7 `attribute setter`

- **명세 위치**: `webidl/index.bs:12388-12468`
- **하는 일**: attribute setter 함수 객체를 만듭니다. `readonly`이면서 예외적으로 setter가
  필요한 경우(`LegacyLenientSetter`/`PutForwards`/`Replaceable`)를 걸러내고, 나머지는
  값을 IDL 값으로 변환해서 `setter steps`를 실행합니다.

```
1. 카테고리 - II-H
   원문 - If |target| is a [=namespace=]: ...
   IR - if (= target (yet "a [=namespace=]")) { ... }

2. 카테고리 - II-A
   원문 - If |attribute| is [=read only=] and does not have a [{{LegacyLenientSetter}}], [{{PutForwards}}] or [{{Replaceable}}] [=extended attribute=], return undefined; there is no [=attribute setter=] function.
   IR - if (&& (= attribute ~read only~) (yet "does not have a [{{LegacyLenientSetter}}]")) { ... }

4. 카테고리 - VII-A
   원문 - If any arguments were passed, then ...
   IR - `if (yet "any arguments were passed") { ... }`
   설명    : closure 호출 시 실제 인자가 있었는지 확인하는 관용구입니다.

5. 카테고리 - VII-A
   원문 - set |V| to the value of the first argument passed.
   IR - V = (yet "the value of the first argument passed")

7. 카테고리 - II-H
   원문 - If |attribute| is a [=regular attribute=]: ...
   IR - if (= attribute (yet "a [=regular attribute=]")) { ... }

8. 카테고리 - I-P + VII-C
   원문 - Let |jsValue| be the <emu-val>this</emu-val> value, if it is not <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s [=realm/global object=] otherwise.
   IR - if (|| (! (= (yet "it") null)) (! (= (yet "it") undefined))) {
          let jsValue = (yet "the <emu-val>this</emu-val> value")
        } else {
          let jsValue = (yet "|realm|'s [=realm/global object=]")
        }

9. 카테고리 - VI
   원문 - (This will subsequently cause a {{TypeError}} in a few steps, if the global object does not implement |target| and [{{LegacyLenientThis}}] is not specified.) <!-- https://www.w3.org/Bugs/Public/show_bug.cgi?id=18547#c9 -->
   IR - (yet "(This will subsequently cause a {{TypeError}} in a few steps, ...) <!-- https://www.w3.org/Bugs/... -->")

10. 카테고리 - II-H
    원문 - If |jsValue| [=is a platform object=], then ...
    IR - if (? jsValue: Unknown["platform object"]) { ... }

11. 카테고리 - II-H
    원문 - Let |validThis| be true if |jsValue| [=implements=] |target|, or false otherwise.
    IR - if (yet "|jsValue| [=implements=] |target|") {
           let validThis = true
         } else {
           let validThis = false
         }

12. 카테고리 - II-A
    원문 - If |validThis| is false and |attribute| was not specified with the [{{LegacyLenientThis}}] [=extended attribute=], then ...
    IR - if (&& (= validThis false) (yet "|attribute| was not specified with the [{{LegacyLenientThis}}] [=extended attribute=]")) {  }

13. 카테고리 - I-O
    원문 - [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>.
    IR - call _ = clo<"javascript/throw">((record [TypeError] {...}))

14. 카테고리 - II-A
    원문 - If |attribute| is declared with the [{{Replaceable}}] extended attribute, then: ...
    IR - if (= attribute (yet "declared with the [{{Replaceable}}] extended attribute")) { ... }

16. 카테고리 - II-A
    원문 - If |attribute| is declared with a [{{LegacyLenientSetter}}] extended attribute, then ...
    IR - if (= attribute (yet "declared with a [{{LegacyLenientSetter}}] extended attribute")) { ... }

17. 카테고리 - II-A
    원문 - If |attribute| is declared with a [{{PutForwards}}] extended attribute, then: ...
    IR - if (= attribute (yet "declared with a [{{PutForwards}}] extended attribute")) { ... }

21. 카테고리 - I-N
    원문 - Set |idlObject| to the IDL [=interface type=] value that represents a reference to |jsValue|.
    IR - idlObject = (yet "the IDL [=interface type=] value that represents a reference to |jsValue|")

22. 카테고리 - II-G
    원문 - If |attribute|'s type is an [=observable array type=] with type argument |T|: ...
    IR - if (= (yet "|attribute|'s type") (yet "an [=observable array type=] with type argument |T|")) { ... }

23. 카테고리 - I-M + II-G
    원문 - Let |idlValue| be determined as follows: <dl class="switch"> <dt>|attribute|'s type is an [=enumeration=]</dt> <dd>...</dd> <dt>Otherwise</dt> <dd>...</dd> </dl>
    IR - let idlValue = (yet "determined as follows: <dl class=\"switch\"> <dt>|attribute|'s type is an [=enumeration=]</dt> <dd>")
         (yet "</dd> <dt>Otherwise</dt> <dd> |idlValue| is the result of [=converted to an IDL value|converting=] |V| to an IDL value of |attribute|'s type")
         (yet "</dd> </dl>")

24. 카테고리 - III-C
    원문 - Perform the [=setter steps=] of |attribute|, with |idlObject| as [=this=] and |idlValue| as [=the given value=].
    IR - (yet "Perform the [=setter steps=] of |attribute|, with |idlObject| as [=this=] and |idlValue| as [=the given value=]")

25. 카테고리 - I-J
    원문 - Let |name| be the string "<code>set </code>" prepended to |id|.
    IR - let name = (yet "the string \"<code>set </code>\" prepended to |id|")

```

---

## #8 `define_the_regular_operations`

- **명세 위치**: `webidl/index.bs:12494-12500`
- **하는 일**: `definition`의 regular operation 중 unforgeable이 아닌 것만 걸러서
  `define_the_operations`에 넘깁니다. `define_the_regular_attributes`(#4)의 operation
  버전입니다.

```
2. 카테고리 - V + I-O
   원문 - [=list/Remove=] from |operations| all the [=operations=] that are [=unforgeable=].
   IR - call _ = clo<"list/remove">(operations, ~operations~, ~unforgeable~)
```

---

## #9 `define_the_operations`

- **명세 위치**: `webidl/index.bs:12518-12533`
- **하는 일**: 걸러진 operation 목록의 각 operation마다 함수를 만들어
  `DefinePropertyOrThrow`로 `target`에 매답니다. `define_the_attributes`(#5)의 operation
  버전입니다.

```
1. 카테고리 - I-O + IV-C
   원문 - [=list/For each=] [=operation=] |op| of |operations|: ...
   IR - call _ = clo<"list/for_each">((case "OPERATION" op operations))

2. 카테고리 - I-F + III-A + IV-B
   원문 - If |op| is not [=exposed=] in |realm|, then [=iteration/continue=].
   IR - if (! (= op (case "EXPOSED" realm))) { (yet "continue") }

3. 카테고리 - IV-D
   원문 - Let |modifiable| be <emu-val>false</emu-val> if |op| is [=unforgeable=] and <emu-val>true</emu-val> otherwise.
   IR (현재) - if (= op ~unforgeable~) {
                 let modifiable = ~false~
               } else {
                 let modifiable = ~true~
               }
   IR (목표) - if (&& (exists op.extendedAttributes.unforgeable) (= op.extendedAttributes.unforgeable true)) {
                 let modifiable = false
               } else {
                 let modifiable = true
               }

4. 카테고리 - I-C
   원문 - Let |desc| be the PropertyDescriptor{\[[Value]]: |method|, \[[Writable]]: |modifiable|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |modifiable|}.
   IR - let desc = (yet "the PropertyDescriptor{\[[Value]]: |method|, \[[Writable]]: |modifiable|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |modifiable|}")

```

---

## #10 `creating_an_operation_function`

- **명세 위치**: `webidl/index.bs:12536-12586`
- **하는 일**: 특정 operation에 대한 함수 객체를 만들어 반환합니다. `attribute
  getter`(#6)와 거의 같은 구조 — try/catch로 감싼 `steps`가 `CreateBuiltinFunction`에
  전달됩니다.

```
3. 카테고리 - I-F
   원문 - Try running the following steps: ... And then, if <a lt=\"an exception was thrown\">an exception |E| was thrown</a>:")
   IR - (yet "Try running the following steps:")
        ...
        (yet "And then, if <a lt=\"an exception was thrown\">an exception |E| was thrown</a>:")

4. 카테고리 - II-H
   원문 - If |target| is an [=interface=], and |op| is not a [=static operation=]: ...
   IR - if (&& (= target (yet "an [=interface=]")) (! (= op (yet "a [=static operation=]")))) { ... }

5. 카테고리 - I-P + VII-C
   원문 - Let |jsValue| be the <emu-val>this</emu-val> value, if it is not <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s [=realm/global object=] otherwise.
   IR - if (|| (! (= (yet "it") null)) (! (= (yet "it") undefined))) {
          let jsValue = (yet "the <emu-val>this</emu-val> value")
        } else {
          let jsValue = (yet "|realm|'s [=realm/global object=]")
        }

6. 카테고리 - VI
   원문 - (This will subsequently cause a {{TypeError}} in a few steps, if the global object does not implement |target|.) <!--https://www.w3.org/Bugs/Public/show_bug.cgi?id=18547#c9 -->
   IR - (yet "(This will subsequently cause a {{TypeError}} in a few steps, ...) <!-- https://www.w3.org/Bugs/... -->")

7. 카테고리 - II-H
   원문 - If |jsValue| [=is a platform object=], then ...
   IR - if (? jsValue: Unknown["platform object"])

8. 카테고리 - I-O + II-H
   원문 - If |jsValue| does not [=implement=] the interface |target|, [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>.
   IR - if (yet "|jsValue| does not [=implement=] the interface |target|") { call _ = clo<"javascript/throw">((record [TypeError] {...})) }

9. 카테고리 - I-N
   원문 - Set |idlObject| to the IDL [=interface type=] value that represents a reference to |jsValue|.
   IR - idlObject = (yet "the IDL [=interface type=] value that represents a reference to |jsValue|")

10. 카테고리 - VII-B + IV-C
    원문 - [=Compute the effective overload set=] for [=regular operations=] (if |op| is a regular operation) or for [=static operations=] (if |op| is a static operation) with [=identifier=] |id| on |target| and with argument count |n|, and let |S| be the result.
    IR - call _call1 = clo<"regular_operations">((yet "tuple(Unknown(if |op| is a regular operation) or for [=static operations=] (if |op| is a static operation))"), (case "IDENTIFIER" id target n))
         call S = clo<"compute_the_effective_overload_set">(_call1)

11. 카테고리 - III-B
    원문 - Let &lt;|operation|, |values|&gt; be the result of passing |S| and |args| to the [=overload resolution algorithm=].
    IR - let _tuple1 = (yet "passing |S| and |args| to the [=overload resolution algorithm=]")
         let operation = _tuple1[0]
         let values = _tuple1[1]

12. 카테고리 - II-A
    원문 - If |operation| is declared with a [{{Default}}] [=extended attribute=], then: ...
    IR - if (= operation (yet "declared with a [{{Default}}] [=extended attribute=]")) { ... }

13. 카테고리 - III-B
    원문 - Otherwise, set |R| to the result of running the [=method steps=] of |operation|, with |idlObject| as [=this=] and |values| as the argument values."
    IR - R = (yet "running the [=method steps=] of |operation|, with |idlObject| as [=this=] and |values| as the argument values")

14. 카테고리 - II-G
    원문 - If |op| has a [=return type=] that is a [=promise type=], then return [=!=] <a abstract-op>Call</a>({{%Promise.reject%}}, {{%Promise%}}, «|E|»).
    IR - if (? (yet "|op| has a [=return type=] that"): Unknown["promise type"]) { call _call1 = clo<"Call">(~%Promise.reject%~, ~%Promise%~, (list [E])) ... }

15. 카테고리 - I-F + VII
    원문 - Otherwise, end these steps and allow the exception to propagate.
    IR - (yet "end these steps and allow the exception to propagate")

16. 카테고리 - IV-C + VII-B
    원문 - [=Compute the effective overload set=]  for [=regular operations=] (if |op| is a regular operation) or for [=static operations=] (if |op| is a static operation) with [=identifier=] |id| on |target| and with argument count 0, and let |S| be the result.
    IR - call _call2 = clo<"regular_operations">((yet "tuple(Unknown(if |op| is a regular operation) or for [=static operations=] (if |op| is a static operation))"), (case "IDENTIFIER" id target 0))
         call S = clo<"compute_the_effective_overload_set">(_call2)

19. 카테고리 - II-K
    원문 - If |op| has a [=return type=] that is a [=promise type=]
    IR - if (= (yet "|op| has a [=return type=] that") (yet "a [=promise type=]"))
```

---

## #11 `compute_the_effective_overload_set`

- **명세 위치**: `webidl/index.bs:3179-3256`
- **하는 일**: operation/legacy factory function/constructor의 오버로드 후보들을 모아
  `(callable, type list, optionality list)` 튜플의 집합 `S`로 만듭니다.

```
1. 카테고리 - VII-B
   설명    : compute the effective overload set의 인자 설명이 줄글로 적혀 있습니다.
             여기서 IDL construct의 타입에 따라 다른 인자들을 받고 다른 행동을 보이는데,
             다른 행동을 보이는 부분에 대해서는 두 개의 타입에 대한 알고리즘을
             하드코딩하여 구현하고, 내부 알고리즘만 공유하는 방식으로 구현할 수 있습니다.
             호출 부분에서는 하드코딩된 알고리즘을 호출하도록 변경해야 합니다.

2. 카테고리 - I-K (ordered set 생성)
   원문 - Let |S| be an [=ordered set=].
   IR - let S = (yet "an [=ordered set=]")

3. 카테고리 - I-M + VII-B
   원문 - Let |F| be an [=ordered set=] with [=set/items=] as follows, according to the kind of [=effective overload set=]: <dl class="switch"> : For regular operations :: The elements of |F| are the [=regular operations=] with identifier |A| defined on interface |I|. : For static operations :: ... : For constructors :: ... : For legacy factory functions :: ... </dl>"
   IR - let F = (yet "...regular operations...")
        (yet ":  For static operations :: ...")
        (yet ":  For constructors :: ...")
        (yet ":  For legacy factory functions :: ...")
        (yet "</dl>")

4. 카테고리 - VII-C
   원문 - Let |maxarg| be the maximum number of arguments the operations, legacy factory functions, or callback functions in |F| are declared to take. For [=variadic=] operations and legacy factory functions, the argument on which the ellipsis appears counts as a single argument."
   IR - let maxarg = (yet "the maximum number of arguments the operations, legacy factory functions, or callback functions in |F| are declared to take")
        (yet "For [=variadic=] operations and legacy factory functions, the argument on which the ellipsis appears counts as a single argument")

5. 카테고리 - I-I
   원문 - Let |max| be <a abstract-op>max</a>(|maxarg|, |N|).
   IR - let max = (yet "<a abstract-op>max</a>(|maxarg|, |N|)")
   설명    - (2026-08-18) 파싱 단계(metalang, `Compiler.compile` 이전)에서는 이제
             `Let(|max|, [$max$](|maxarg|, |N|))`로 정상 인식되는 것을 직접 확인함. 다만 이
             알고리즘 전체가 (자신과 무관한, 별도로 알려진 Range-lowering 버그 때문에) 지금
             `Compiler.compile`을 못 통과해서 실제 최종 IR은 확인하지 못했고, 어차피
             `compute_the_effective_overload_set.ir`로 하드코딩되어 있어 이 컴파일 경로 자체를
             안 탐 — 위 "IR" 필드는 옛 값 그대로 둠. 하드코딩을 걷어내기 전까지는 보류.

6. 카테고리 - IV
   원문 - [=set/For each=] operation or extended attribute |X| in |F|: ...
   IR - call _ = clo<"set/for_each">(X, F)

7. 카테고리 - II-K
   원문 - Let |arguments| be the [=list=] of arguments |X| is declared to take.
   IR - let arguments = (yet "the [=list=] of arguments |X| is declared to take")

8. 카테고리 - I-K
   원문 - Let |types| be a [=type list=].
        - Let |optionalityValues| be an [=optionality list=].
   IR - let types = (yet "a [=type list=]")
      - let optionalityValues = (yet "an [=optionality list=]")

9. 카테고리 - I-O
   원문 - [=list/For each=] |argument| in |arguments|: ...
   IR - call _ = clo<"list/for_each">(argument, arguments)

10. 카테고리 - II-K
    원문 - [=list/Append=] the type of |argument| to |types|.
         - [=list/Append=] "variadic" to |optionalityValues| if |argument| is a final, variadic argument, "optional" if |argument| is [=optional argument|optional=], and "required" otherwise.
    IR - push types < (yet "the type of |argument|")
       - push (yet "|optionalityValues| if |argument| is a final, variadic argument, \"optional\" if |argument| is [=optional argument|optional=], and \"required\" otherwise") < "variadic"

11. 카테고리 - I-K + I-O
    원문 - [=set/Append=] the [=tuple=] (|X|, |types|, |optionalityValues|) to |S|.
    IR - call _ = clo<"set/append">(~tuple~, (yet "tuple(Var(X)Var(types)Var(optionalityValues))"), S)

12. 카테고리 - II-K
    근본 원인)
    원문 - If |X| is declared to be [=variadic=], then: ...
    IR - if (= X (yet "declared to be [=variadic=]")) { ... }

13. 카테고리 - I-L
    원문 - Let |i| be |n| − 1.
    IR - let i = (yet "|n| − 1")

14. 카테고리 - I-O
    원문 - [=iteration/While=] |i| ≥ 0: ...
    IR - call _ = clo<"iteration/while">(i, 0)

15. 카테고리 - II-K + IV-C
    원문 - If |arguments|[|i|] is not [=optional argument|optional=] (i.e., it is not marked as "optional" and is not a final, variadic argument), ...
    IR - call _call1 = clo<"optional_argument">((yet "i.e."), (yet "it is not marked as \"optional\" and is not a final"), (yet "variadic argument"))
         if (! (= arguments[i] _call1)) { ... }

16. 카테고리 - I-F + I-O
    원문 - [=iteration/break=]
    IR - call _ = clo<"iteration/break">()

17. 카테고리 - I-K
    원문 - Let |t| be a [=type list=].
         - Let |o| be an [=optionality list=].
    IR - let t = (yet "a [=type list=]")
       - let o = (yet "an [=optionality list=]")

18. 카테고리 - I-O + IV-A
    원문 - [=list/For each=] |j| in [=the range=] 0 to |i| − 1, inclusive: ...
    IR - call _ = clo<"list/for_each">(j, (yet "IMPOSSIBLE (unreachable after lowering): range Num(0) to Unknown(|i| − 1, inclusive:)"))

19. 카테고리 - I-K + I-O
    원문 - [=set/Append=] the [=tuple=] (|X|, |t|, |o|) to |S|.
    IR - call _ = clo<"set/append">(~tuple~, (yet "tuple(Var(X)Var(t)Var(o))"), S)
```

---

## #12 `overload_resolution_algorithm`

- **명세 위치**: `webidl/index.bs:11519-11783`
- **하는 일**: `compute_the_effective_overload_set`이 만든 candidate 집합 `S`와 실제 JS
  인자 목록 `args`를 받아서, 실제로 호출할 `(callable, values)` 쌍을 고릅니다. 몸통 대부분이
  "JS 값 `V`의 런타임 타입에 따라 어떤 IDL 타입으로 갈지"를 고르는 17갈래 dispatch입니다.

요약: IR 218 line 중 129 line이 yet으로 컴파일되고 있습니다.
#12에 있는 yet들은 대부분 #12 specific해서 category가 의미가 없는 것 같아 생략했습니다.

---

## #13 `perform_security_checks`

- **명세 위치**: `webidl/index.bs:11498-11514` + 7.2.1.1 Integration with IDL (HTML Standard)
- **하는 일**: nop in WJI context (assume platform object is not Window or Location)

---

## #14 `define_the_iteration_methods`

-- **명세 위치**: `webidl/index.bs:12792-12871`

1. 카테고리 - II-D
   원문 - If |definition| has an [=indexed property getter=], then: ...
   IR - if (yet "|definition| has an [=indexed property getter=]") { ... }

2. 카테고리 - II-D
   원문 - Otherwise, if |definition| has a [=pair iterator=], then: ...
   IR - else if (yet "|definition| has a [=pair iterator=]") { ... }

## #15 `define_the_asynchronous_iteration_methods`

1. 카테고리 - II-D
   원문 - If |definition| does not have an an [=asynchronously iterable declaration=] (of either sort), then ...
   IR - if (yet "|definition| does not have an an [=asynchronously iterable declaration=] (of either sort)") { ... }
   참고 - an이 두 번 적힌 오류가 있습니다.

## #16 `define the unforgeable regular operations`
## #17 `define the unforgeable regular attributes`
## #18 `define the static attributes`
## #19 `define the static operations`

- #4, #8과 비슷하지만 WJI context에서는 unforgeable regular operation/attribute와 static attribute가 없다고 가정합니다.

## #20 `define_the_constants`

1. 카테고리 - I-O + IV-C
   원문 - [=list/For each=] [=constant=] |const| that is a [=member=] of |definition|:
   IR - call _ = clo<"list/for_each">((case "CONSTANT" const (case "MEMBER" definition)))

2. 카테고리 - III-A + IV-B
   원문 - If |attr| is not [=exposed=] in |realm|, then ...
   IR - if (! (= attr (case "EXPOSED" realm))) { ... }

3. 카테고리 - I-C
   원문 - Let |desc| be the PropertyDescriptor{\[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: <emu-val>false</emu-val> \[[Value]]: |value|}.
   IR - let desc = (yet "the PropertyDescriptor{\\[[Writable]]: <emu-val>false</emu-val>, \\[[Enumerable]]: <emu-val>true</emu-val>, \\[[Configurable]]: <emu-val>false</emu-val>, \\[[Value]]: |value|}")
