# WebIDL 알고리즘별 yet 정리 (#1~#12)

`create a new object implementing the interface`를 시작점으로 타고 들어가며 실제로 호출되는
WebIDL 알고리즘들을 `esmeta/wji` 파서·컴파일러에 통과시켜보고, 남은 `(yet ...)` 항목들을 정리한
문서입니다. 두 그룹으로 나뉩니다:

- **#1~#7 (interface/attribute 생성 경로)**: `internally_create_a_new_object_implementing_the_interface`,
  `create_an_interface_prototype_object`, `create_an_interface_object`,
  `define_the_regular_attributes`/`define_the_attributes`, `attribute getter`, `attribute setter`.
- **#8~#12 (operation 정의 및 overload resolution 경로)**: `define_the_regular_operations`,
  `define_the_operations`, `creating_an_operation_function`,
  `compute_the_effective_overload_set`, `overload_resolution_algorithm`. `#8`/`#9`는 처음부터
  `SpecFile.webidlFilter`에 있었고, `#10`~`#12`는 이 조사를 위해 새로 추가했습니다(아래 참고).

각 섹션은 알고리즘 하나에 대응하고, 스텝 번호(`#N-k`)는 `src/test/resources/golden/wji/ir.actual`의
해당 함수 정의 순서를 그대로 따릅니다. 카테고리 태그(`I-A`, `II-A` 등)는 `webidl_yet_categorized`
문서의 분류를 가리킵니다.

**`#8`~`#12`를 위한 사전 준비**: `src/main/scala/esmeta/wji/lang/SpecFile.scala`의
`webidlFilter`에 `"creating an operation function"`/`"compute the effective overload
set"`/`"overload resolution algorithm"` 3개를 추가해서 추출 대상에 넣었습니다.

**⚠️ `#8`~`#12`를 위해 `Compiler.scala`에 넣은 임시 우회**: 위 3개를 추가하고 실제로
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
n. 카테고리: <webidl_yet_categorized의 태그>
   원문    : <webidl/index.bs의 원문, markup 그대로>
   IR      : <ir.actual에 실제로 컴파일된 최종 IR>
   설명    : <무엇이 문제이고 어떻게 고쳐야 하는지>
```

**WJI 범위 밖 브랜치 제외 원칙**: 지금 WJI가 다루는 interface(`Module`/`Instance`/`Memory`/`Table`/
`Global`/`Tag`/`Exception`)가 실제로는 절대 타지 않는다고 판단한 분기(예: interface 상속, 특정
extended attribute 조합)에 있는 yet은 이 문서에 별도로 기록하지 않았습니다. 예를 들어 이 7개
interface는 전부 `interface Foo : Bar` 형태의 WebIDL 상속을 쓰지 않으므로(`spectec/document/js-api/index.bs`
확인 결과), "I inherits from some other interface P" 관련 분기는 지금 당장은 죽은 코드로 봐도 됩니다.

---

## #1 `internally_create_a_new_object_implementing_the_interface`

- **명세 위치**: `webidl/index.bs:13827-13877`
- **하는 일**: `[Global]` 여부, 상속 체인, unforgeable 멤버 등을 반영해서 실제 platform object
  하나를 만드는, `Expr.New(iface)`가 최종적으로 의미하는 범용 알고리즘입니다.

1. 카테고리: III-A
   원문    : "Let |prototype| be the [=interface prototype object=] for |interface| in |realm|."
             (`newTarget`이 `undefined`인 경우) / "Set |prototype| to the [=interface prototype
             object=] for |interface| in |targetRealm|." (콜러블이지만 `prototype`이 Object가
             아닌 안쪽 분기)
   IR      : `let prototype = (yet "the [=interface prototype object=] for |interface| in
             |realm|")` / `prototype = (yet "the [=interface prototype object=] for |interface|
             in |targetRealm|")` (`ir.actual` 3415번째 줄)
   설명    : interface prototype object를 만드는 하위 알고리즘을 실행해서 그 결과를 받으라는
             문장이 아직 구체적인 호출로 안 바뀌어 있습니다. `call prototype =
             clo<"create_an_interface_prototype_object">(interface, realm)`로 spec patch하면
             됩니다. 두 번째(`targetRealm`) 발생은 지금까지 정리한 노트에 빠져 있었습니다.

2. 카테고리: I-A + I-D
   원문    : "[=list/iterate|For every=] [=interface=] |ancestor interface| in |interfaces|: ..."
             / "[=list|For each=] element |key| of |keys|: ..."
   IR      : `call _ = clo<"list/iterate">((case "INTERFACE" ancestor interface interfaces))`
             (yet 노드조차 아니고 곧바로 case-태그 오인식 결과로 컴파일됨) / `(yet "foreach
             ?(element |key|) in ?(|keys|:)")` (원문과 무관한, 파서 내부의 실패한 매치 표기
             `?(...)`가 그대로 yet 텍스트에 남음)
   설명    : 루프 바디 없는 단일 호출로 뭉개지고 있습니다.
             변수에 대한 설명을 case로 잘못 인식하고 있습니다.


3. 카테고리: III-A + I-B
   원문    : "Let |unforgeables| be the value of the \[[Unforgeables]] slot of the [=interface
             object=] of |ancestor interface| in |realm|."
   IR      : `let unforgeables = (yet "the value of the \[[Unforgeables]] slot of the
             [=interface object=] of |ancestor interface| in |realm|")`
   설명    : `create_an_interface_object(ancestor interface, realm)`를 호출해서 그 결과의
             `.Unforgeables` 필드를 읽으라는 뜻입니다 (III-A: self-referential 알고리즘 호출 +
             I-B: 슬롯 접근 조합).

4. 카테고리: I-B
   원문    : "Let |descriptor| be [=!=] |unforgeables|.\[[GetOwnProperty]](|key|)."
   IR      : `let _comp4 = (yet "|unforgeables|.\[[GetOwnProperty]](|key|)")` (뒤이어
             completion-record 처리 후 `let descriptor = _comp4.Value`)
   설명    : internal slot bracket 표기 `\[[...]]`를 필드 접근으로 못 읽습니다.

5. 카테고리: II-A
   원문    : "If |interface| is declared with the [{{Global}}] [=extended attribute=], then: ..."
   IR      : `if (yet "declared with the [{{Global}}] [=extended attribute=]") { ... }`
   설명    : interface가 `[Global]` extended attribute로 선언됐는지 묻는 술어입니다. interface
             record에 아직 이 플래그를 노출하는 필드가 없어서 못 읽습니다. WJI 범위의 7개
             interface(`Module`/`Instance`/`Memory`/`Table`/`Global`/`Tag`/`Exception`) 중
             `[Global]` extended attribute가 붙은 건 하나도 없으므로(`spectec/document/js-api/index.bs`의
             각 interface 선언부 확인), **지금은 이 조건을 false로 가정합니다.**

6. 카테고리: II-D
   원문    : "Otherwise, if |interfaces| contains an [=interface=] which [=support indexed
             properties|supports indexed properties=], [=support named properties|named
             properties=], or both: ..."
   IR      : `else if (yet "|interfaces| contains an [=interface=] which [=support indexed
             properties|supports indexed properties=]")`
   설명    : yet으로 추출된 텍스트는 "supports indexed properties"까지만 남기고 그 뒤의 "named
             properties, or both"를 빠뜨렸습니다. 지금은 이 분기 전체를 false로 가정하고 있어서
             당장 문제는 없지만, 나중에 이 조건을 실제로 구현할 때는 "named properties" 쪽도
             같이 넣어야 합니다.

7. 카테고리: I-B + I-K
   원문    : "Set |instance|.\[[SetPrototypeOf]] as defined in
             [[#platform-object-setprototypeof]]." / "Set |instance|.\[[GetOwnProperty]] as
             defined in [[#legacy-platform-object-getownproperty]]." (그 외
             `\[[Set]]`/`\[[DefineOwnProperty]]`/`\[[Delete]]`/`\[[PreventExtensions]]`/
             `\[[OwnPropertyKeys]]`도 동일 패턴)
   IR      : `(yet "Set |instance|.\[[SetPrototypeOf]] as defined in
             [[#platform-object-setprototypeof]]")` 등 각 문장마다 동일한 형태의 개별 yet
   설명    : `[Global]` 분기와 legacy-platform-object 분기 양쪽에 있는 "internal method를 다른
             곳에서 정의된 대로 설정하라"는 문장들로, 결국 `instance.GetOwnProperty =
             clo<"...">` 같은 슬롯 대입으로 바뀌어야 합니다.

---

## #2 `create_an_interface_prototype_object`

- **명세 위치**: `webidl/index.bs:12044-12099`
- **하는 일**: interface의 prototype 객체 하나를 만들고, `[Global]`/상속/`DOMException`/
  `is global prototype chain mutable` 등에 따라 그 prototype의 진짜 prototype과 내부 슬롯
  구성을 결정합니다.

1. 카테고리: II-A + II-D
   원문    : "If |interface| is declared with the [{{Global}}] [=extended attribute=], and
             |interface| [=support named properties|supports named properties=], then set
             |proto| to the result of [=create a named properties object|creating a named
             properties object=] for |interface| and |realm|."
   IR      : `if (&& (yet "declared with the [{{Global}}] [=extended attribute=]") (yet
             "|interface| [=support named properties|supports named properties=]")) { call
             _call1 = clo<"create_a_named_properties_object">(interface, realm) ... }`
   설명    : #1-5/#1-6과 근본 원인이 같습니다.

2. 카테고리: II-C
   원문    : "Otherwise, if |interface| is declared to inherit from another interface, then set
             |proto| to the [=interface prototype object=] in |realm| of that [=inherited
             interface=]."
   IR      : `else if (= interface (yet "declared to inherit from another interface")) { proto =
             (yet "the [=interface prototype object=] in |realm| of that [=inherited
             interface=]") }`
   설명    : WJI 범위의 7개 interface는 전부 다른 interface를 상속하지 않으므로, 지금은 이
             분기도 안전하게 false로 가정할 수 있습니다.

3. 카테고리: II-J
   원문    : "Otherwise, if |interface| is the {{DOMException}} [=interface=], then set |proto|
             to |realm|.\[[Intrinsics]].\[[{{%Error.prototype%}}]]."
   IR      : `else if (= interface (yet "the {{DOMException}} [=interface=]")) { proto =
             realm.Intrinsics["%Error.prototype%"] }`
   설명    : 특정 interface와의 동일성 체크입니다.

4. 카테고리: II-J
   원문    : "If |realm|'s [=is global prototype chain mutable=] is true, then: ..."
   IR      : `if (= (yet "|realm|'s [=is global prototype chain mutable=]") true) { call _call2
             = clo<"OrdinaryObjectCreate">(proto) ... }`
   설명    : `webidl/index.bs:10226-10229`를 보면 "All realms have an is global prototype
             chain mutable boolean, which can be set when the realm is created. ... **By
             default it is set to false.**"라고 명시돼 있고, 바로 뒤 노트는 "This allows the
             `ShadowRealm` global to have a mutable prototype"라고 이 값이 `true`가 되는 유일한
             이유를 설명합니다. WJI는 `ShadowRealm`을 모델링하지 않으므로, 이 조건은 `true`가
             아니라 **명세의 기본값 그대로 `false`로 가정.**

5. 카테고리: II-A + II-C
   원문    : "Otherwise, if |interface| is declared with the [{{Global}}] [=extended
             attribute=], or |interface| is in the set of [=inherited interfaces=] of an
             interface that is declared with the [{{Global}}] [=extended attribute=], then: ..."
   IR      : `else if (|| (= interface (yet "declared with the [{{Global}}] [=extended
             attribute=],")) (= interface (yet "in the set of [=inherited interfaces=] of an
             interface that is declared with the [{{Global}}] [=extended attribute=]")))`
   설명    : WJI 범위의 7개 interface는 전부 `[Global]`이 아니고 서로 상속하지도
             않으므로(`#1-5`/`#2-2` 참고), 이 조건도 **지금은 false로 가정합니다.**

6. 카테고리: II-A
   원문    : "If |interface| has any [=member=] declared with the [{{Unscopable}}] [=extended
             attribute=], then: ..."
   IR      : `if (yet "|interface| has any [=member=] declared with the [{{Unscopable}}]
             [=extended attribute=]") { ... }`
   설명    : interface record에 member별 `[Unscopable]` 플래그를 노출하는 필드가 없어서 못
             읽습니다.

7. 카테고리: II-A
   원문    : "If |interface| is not declared with the [{{Global}}] [=extended attribute=], then:
             ..."
   IR      : `if (! (= interface (yet "declared with the [{{Global}}] [=extended
             attribute=]"))) { ... }`
   설명    : 5번과 동일한 `[Global]` 재확인입니다.

8. 카테고리: II-A
   원문    : "If the [{{LegacyNoInterfaceObject}}] [=extended attribute=] was not specified on
             |interface|, then: ..."
   IR      : `if (yet "the [{{LegacyNoInterfaceObject}}] [=extended attribute=] was not
             specified on |interface|") { ... }`
   설명    : WJI 범위의 7개 interface 중 `[LegacyNoInterfaceObject]`가 붙은 게 하나도 없어서,
             이 조건은 (원본 노트 제안대로) 항상 true로 가정할 수 있습니다.

9. 카테고리: III-A
   원문    : "Let |constructor| be the [=interface object=] of |interface| in |realm|."
   IR      : `let constructor = (yet "the [=interface object=] of |interface| in |realm|")`
   설명    : `create_an_interface_object(interface, realm)`를 호출하라는 뜻입니다.

10. 카테고리: I-C
    원문    : "Let |desc| be the PropertyDescriptor{\[[Writable]]: <emu-val>true</emu-val>,
              \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>true</emu-val>,
              \[[Value]]: |constructor|}."
    IR      : `let desc = (yet "the PropertyDescriptor{\[[Writable]]: <emu-val>true</emu-val>,
              \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>true</emu-val>,
              \[[Value]]: |constructor|}")`
    설명    : record 리터럴 문법을 파서가 못 읽습니다.

---

## #3 `create_an_interface_object`

- **명세 위치**: `webidl/index.bs:11933-11990`
- **하는 일**: interface의 생성자 함수 객체(`F`) 자체를 만듭니다. 이 함수 안에서
  `new I(...)`가 호출됐을 때 실제로 실행될 코드(`steps`)와, `F`를 만드는 시점에 한 번만
  실행되는 코드(prototype 연결, static 멤버 정의 등)가 한 알고리즘 안에 같이 들어있습니다.

먼저 `steps`로 묶이는, **생성자가 호출될 때마다 실행되는** 부분:

1. 카테고리: I-H
   원문    : "Let |steps| be |I|'s [=overridden constructor steps=] if they exist, or the
             following steps otherwise:"
   IR      : `let steps = (yet "|I|'s [=overridden constructor steps=] if they exist, or the
             following steps otherwise:")`
   설명    : "the following steps:" 뒤 numbered list를 closure 리터럴로 못 읽습니다.

2. 카테고리: II-A
   원문    : "If |I| was not declared with a [=constructor operation=], then
             [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
   IR      : `if (yet "|I| was not declared with a [=constructor operation=]") { call _ =
             clo<"javascript/throw">() }`
   설명    : "constructor operation 존재 여부" 술어에 필드가 없어서 못 읽습니다. WJI 범위에서는
             false를 false로 가정합니다.

3. 카테고리: IV-A
   원문    : "Let |args| be the passed arguments."
   IR      : `let args = (yet "the passed arguments")`
   설명    : closure가 실제로 호출될 때 넘어온 인자 목록을 이름 있는 변수로 받는 관용구가 아직
             표준화된 IR 표현이 없습니다.

4. 카테고리: II-E
   원문    : "Let |id| be the identifier of interface |I|."
   IR      : `let id = (yet "the identifier of interface |I|")`
   설명    : `interface.id` 필드 접근으로 바뀌면 됩니다.

5. 카테고리: I-D
   원문    : "[=Compute the effective overload set=] for constructors with [=identifier=] |id|
             on [=interface=] |I| and with argument count |n|, and let |S| be the result."
   IR      : `call S = clo<"compute_the_effective_overload_set">((case "IDENTIFIER" id (case
             "INTERFACE" I n)))`
   설명    : 인자 3개(`id`, `I`, `n`)로 나뉘지 않고 중첩된 case-태그 튜플 하나로 뭉쳐 나옵니다.
             파서가 "못 읽는" 게 아니라, "with [=identifier=] |id| on [=interface=] |I| and
             with argument count |n|"라는 원문의 `[=dfn-link=] |var|` 설명 주석을 case 태그
             생성자로 잘못 오인해서 생기는 문제입니다.

6. 카테고리: I-E
   원문    : "Let &lt;|constructor|, |values|&gt; be the result of passing |S| and |args| to
             the [=overload resolution algorithm=]."
   IR      : `let _ = (yet "unsupported Let lhs: Unknown(<|constructor|, |values|>)")`
   설명    : 튜플 destructuring이 파서에 없어서 못 읽습니다.

7. 카테고리: III-C
   원문    : "Perform the [=constructor steps=] of |constructor| with |object| as [=this=] and
             |values| as the argument values."
   IR      : `(yet "Perform the [=constructor steps=] of |constructor| with |object| as
             [=this=] and |values| as the argument values")`
   설명    : "Perform the X steps of Y with Z as this" 관용구를 call로 정규화해야 합니다.

8. 카테고리: II-C
   원문    : "If |I| inherits from some other interface |P|, then set |constructorProto| to the
             [=interface object=] of |P| in |realm|."
   IR      : `if (yet "|I| inherits from some other interface |P|") { constructorProto = (yet
             "the [=interface object=] of |P| in |realm|") }`
   설명    : WJI 범위에서는 항상 false로 봐도 무방합니다(상속하는 interface 없음).

9. 카테고리: II-A
   원문    : "If |I| was declared with a [=constructor operation=], then: ..."
   IR      : `if (yet "|I| was declared with a [=constructor operation=]") { ... }`
   설명    : WJI 범위의 7개 interface 전부 constructor가 있으므로 true로 가정 가능.

10. 카테고리: IV-B
    원문    : "Set |length| to the length of the shortest argument list of the entries in |S|."
    IR      : `length = (sizeof (yet "the shortest argument list of the entries in |S|"))`
    설명    : `compute_the_effective_overload_set`가 만든 overload set의 각 entry가 선언적으로
              몇 개의 인자를 받는지 계산하는 문제로, IV-A(호출 시 실제 인자 접근)와는 근본
              원인이 다릅니다. WJI의 7개 interface는 constructor가 하나씩만 있어서 `S`가 항상
              entry 1개짜리지만, `create an operation function`(별도의, 아직 감사하지 않은
              8번째 알고리즘)이 쓰는 같은 패턴은 `WebAssembly.instantiate`처럼 진짜
              오버로딩(entry 2개 이상)이 있는 경우도 있습니다.

11. 카테고리: I-I
    원문    : "Let |F| be <a abstract-op>CreateBuiltinFunction</a>(|steps|, |length|, |id|, «
              \[[Unforgeables]] », |realm|, |constructorProto|)."
    IR      : `let F = (yet "<a abstract-op>CreateBuiltinFunction</a>(|steps|, |length|, |id|,
              « \[[Unforgeables]] », |realm|, |constructorProto|)")`
    설명    : `<a abstract-op>` 마크업으로 표기된 ECMA-262 abstract operation 호출이 함수
              호출로 인식되지 않습니다.

12. 카테고리: I-D
    원문    : "Let |proto| be the result of [=create an interface prototype object|creating an
              interface prototype object=] of [=interface=] |I| in |realm|."
    IR      : `call proto = clo<"create_an_interface_prototype_object">((case "INTERFACE" I
              realm))`
    설명    : 5번과 동일한 case-튜플 현상입니다(`of [=interface=] |I| in [=realm=] |realm|`
              자체가 dfn-link + var 설명 주석이라서).

13. 카테고리: I-C + I-I
    원문    : "Perform [=!=] <a abstract-op>DefinePropertyOrThrow</a>(|F|,
              "<code>prototype</code>", PropertyDescriptor{\[[Value]]: |proto|,
              \[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>,
              \[[Configurable]]: <emu-val>false</emu-val>})."
    IR      : `call _ =
              clo<"<a_abstract-op>DefinePropertyOrThrow</a>(|F|,_"<code>prototype</code>",_PropertyDescriptor{\[[Value]]:_|proto|,_\[[Writable]]:_<emu-val>false</emu-val>,_\[[Enumerable]]:_<emu-val>false</emu-val>,_\[[Configurable]]:_<emu-val>false</emu-val>})">()`
    설명    : 문장 전체가 closure 이름이 되고 인자는 없는 깨진 호출입니다 — record 리터럴(I-C)과
              abstract-op 호출 인식(I-I)이 겹친 경우입니다.

14. 카테고리: I-D
    원문    : "[=Define the constants=] of [=interface=] |I| on |F| given |realm|."
    IR      : `call _ = clo<"define_the_constants">((case "INTERFACE" I F realm))`
    설명    : 5번과 동일한 case-튜플 현상입니다.

15. 카테고리: I-D
    원문    : "[=Define the static attributes=] of [=interface=] |I| on |F| given |realm|."
    IR      : `call _ = clo<"define_the_static_attributes">((case "INTERFACE" I F realm))`
    설명    : 5번과 동일한 case-튜플 현상입니다.

16. 카테고리: I-D
    원문    : "[=Define the static operations=] of [=interface=] |I| on |F| given |realm|."
    IR      : `call _ = clo<"define_the_static_operations">((case "INTERFACE" I F realm))`
    설명    : 5번과 동일한 case-튜플 현상입니다.

---

## #4 `define_the_regular_attributes`

- **명세 위치**: `webidl/index.bs:12296-12302`
- **하는 일**: `definition`의 regular attribute 중 unforgeable이 아닌 것만 걸러서
  `define_the_attributes`에 넘깁니다.

1. 카테고리: II-F
   원문    : "Let |attributes| be the [=list=] of [=regular attributes=] that are [=members=]
             of |definition|."
   IR      : `let attributes = (yet "the [=list=] of [=regular attributes=] that are
             [=members=] of |definition|")`
   설명    : "definition의 member 중 특정 kind만 골라낸 목록"이라는 필터링된 뷰를 어떻게
             파생시킬지 record 모델 설계가 필요합니다.

2. 카테고리: V (+ I-A류 현상)
   원문    : "[=list/Remove=] from |attributes| all the [=attributes=] that are
             [=unforgeable=]."
   IR      : `call _ = clo<"list/remove">(attributes, ~attributes~, ~unforgeable~)`
   설명    : `list/for_each`/`list/iterate`가 루프 바디 없는 단일 호출로 뭉개지는 것(I-A)과
             같은 방식으로, 이 `list/Remove` 문장도 "definition의 attribute 목록을 복사해 온
             뒤 그중 unforgeable인 것만 제거"라는 원래 의미를 담지 못하고 단일 호출 하나로
             뭉개져 있습니다 — `attributes`가 복사됐는지 원본을 직접 mutate하는지 불분명한
             것은 물론, "unforgeable인지 판정하는 조건" 자체도 `~unforgeable~`이라는 심볼
             하나로만 남아 있어서 실제로 무엇을 제거해야 하는지 계산할 방법이 없습니다. copy
             의미론과 filter 조건 두 가지를 같이 설계해야 합니다.

---

## #5 `define_the_attributes`

- **명세 위치**: `webidl/index.bs:12320-12344`
- **하는 일**: 걸러진 attribute 목록의 각 attribute마다 getter/setter를 만들어
  `DefinePropertyOrThrow`로 `target`에 매단다.

1. 카테고리: I-A + I-D
   원문    : "[=list/For each=] [=attribute=] |attr| of |attributes|: ..."
   IR      : `call _ = clo<"list/for_each">((case "ATTRIBUTE" attr attributes))`
   설명    : #2-1 처럼 루프 바디 없는 단일 호출로 뭉개지고 있습니다.
             변수에 대한 설명을 case로 잘못 인식하고 있습니다.


2. 카테고리: I-F + I-D
   원문    : "If |attr| is not [=exposed=] in |realm|, then [=iteration/continue=]."
   IR      : `if (! (= attr (case "EXPOSED" realm))) { (yet "continue") }`
   설명    : `continue` 자체가 지원 안 되는 것(I-F)뿐 아니라 조건 부분("|attr| is not
             [=exposed=] in |realm|")도 `(case "EXPOSED" realm)`이라는, `#3-5`와 똑같은
             종류의 case-태그 오인식(I-D — `[=exposed=]` dfn-link 뒤에 나오는 `|realm|`이
             case 태그의 인자로 잘못 흡수됨) 결과로 컴파일되어 있습니다.

3. 카테고리: III-B
   원문    : "Let |getter| be the result of creating an [=attribute getter=] given |attr|,
             |definition|, and |realm|."
   IR      : `let getter = (yet "an [=attribute getter=] given |attr|, |definition|, and
             |realm|")`
   설명    : named algorithm을 값처럼 참조하는 관용구를 call로 정규화해야 합니다.

4. 카테고리: III-B
   원문    : "Let |setter| be the result of creating an [=attribute setter=] given |attr|,
             |definition|, and |realm|."
   IR      : `let setter = (yet "an [=attribute setter=] given |attr|, |definition|, and
             |realm|")`
   설명    : 3번과 동일한 관용구입니다.

5. 카테고리: I-G
   원문    : "Let |configurable| be <emu-val>false</emu-val> if |attr| is [=unforgeable=] and
             <emu-val>true</emu-val> otherwise."
   IR      : `let configurable = (yet "<emu-val>false</emu-val> if |attr| is [=unforgeable=]
             and <emu-val>true</emu-val> otherwise")`
   설명    : 조건부 `let` 값 패턴을 확장된 `Let`으로 우선 파싱하고, `if`로의 desugaring은
             별도 pass에서 수행하는 방향입니다.

6. 카테고리: I-C
   원문    : "Let |desc| be the PropertyDescriptor{\[[Get]]: |getter|, \[[Set]]: |setter|,
             \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |configurable|}."
   IR      : `let desc = (yet "the PropertyDescriptor{\[[Get]]: |getter|, \[[Set]]: |setter|,
             \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |configurable|}")`
   설명    : record 리터럴 문법을 파서가 못 읽습니다.

7. 카테고리: II-E
   원문    : "Let |id| be |attr|'s [=identifier=]."
   IR      : `let id = (yet "|attr|'s [=identifier=]")`
   설명    : `attr.id` 필드 접근으로 바뀌면 됩니다.

8. 카테고리: I-I
   원문    : "Perform [=!=] <a abstract-op>DefinePropertyOrThrow</a>(|target|, |id|, |desc|)."
   IR      : `call _ = clo<"<a_abstract-op>DefinePropertyOrThrow</a>(|target|,_|id|,_|desc|)">()`
   설명    : abstract-op 호출 마크업이 인식되지 않아 문장 전체가 closure 이름이 된 깨진
             호출입니다.

9. 카테고리: II-G
   원문    : "If |attr|'s type is an [=observable array type=] with type argument |T|, then:
             ..."
   IR      : `if (= (yet "|attr|'s type") (yet "an [=observable array type=] with type
             argument |T|")) { ... }`
   설명    : WJI 범위의 6개 attribute 중 observable array 타입은 없으므로 false로 가정.

---

## #6 `attribute getter`

- **명세 위치**: `webidl/index.bs:12348-12384`
- **하는 일**: 특정 attribute에 대한 getter 함수 객체를 만들어 반환합니다. 진짜 getter 로직은
  "Try running the following steps: ... And then, if an exception was thrown: ..."라는
  try/catch 형태의 `steps`로 감싸져 있고, 이 `steps`가 `CreateBuiltinFunction`에 그대로
  전달되어 실제 getter 함수의 바디가 됩니다.

1. 카테고리: I-H
   원문    : "Let |steps| be the following series of steps:"
   IR      : `let steps = (yet "the following series of steps:")`
   설명    : numbered list를 closure/thunk 리터럴로 못 읽습니다.

2. 카테고리: I-F
   원문    : "Try running the following steps: ... And then, if an exception |E| was thrown:
             ..."
   IR      : `(yet "Try running the following steps:")` ... `(yet "And then, if <a
             lt=\"an exception was thrown\">an exception |E| was thrown</a>:")`
   설명    : try/catch 구조가 IR/파서에 없습니다.

3. 카테고리: II-H
   원문    : "If |target| is an [=interface=], and |attribute| is a [=regular attribute=]: ..."
   IR      : `if (&& (= target (yet "an [=interface=]")) (= attribute (yet "a [=regular
             attribute=]"))) { ... }`
   설명    : taget과 attribute의 종류를 확인합니다 (II-H).

4. 카테고리: I-G
   원문    : "Let |jsValue| be the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise."
   IR      : `let jsValue = (yet "the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise")`
   설명    : 조건부 `let` 값 패턴(I-G)입니다.

5. 카테고리: VI
   원문    : "(This will subsequently cause a {{TypeError}} in a few steps, if the global
             object does not implement |target| and [{{LegacyLenientThis}}] is not
             specified.) <!-- https://www.w3.org/Bugs/Public/show_bug.cgi?id=18547#c9 -->"
   IR      : `(yet "(This will subsequently cause a {{TypeError}} in a few steps, ...)
             <!-- https://www.w3.org/Bugs/... -->")`
   설명    : 비규범적 에디토리얼 주석입니다. `esmeta.wji.lang.Instr.Note` +
             `DropNotesPass`가 이미 있으니, `InstrParser`가 이 문장 모양도 Note로 인식하도록
             확장하면 됩니다.

6. 카테고리: II-E
   원문    : "If |jsValue| [=is a platform object=], then [=perform a security check=], passing
             |jsValue|, |attribute|'s [=identifier=], and "getter"."
   IR      : `call _ = clo<"perform_a_security_check">(jsValue, attribute, ~identifier~,
             "getter")`
   설명    : attribute의 identifier를 제대로 인식 못하고 있습니다.

7. 카테고리: II-I
   원문    : "If |jsValue| does not [=implement=] |target|, then: ..."
   IR      : `if (yet "|jsValue| does not [=implement=] |target|") { ... }`
   설명    : platform-object의 implements 관계를 나타내는 필드가 필요합니다.

8. 카테고리: II-A
   원문    : "If |attribute| was specified with the [{{LegacyLenientThis}}] [=extended
             attribute=], then return <emu-val>undefined</emu-val>."
   IR      : `if (yet "|attribute| was specified with the [{{LegacyLenientThis}}] [=extended
             attribute=]") { return undefined }`
   설명    : extended attribute 존재 여부 필드가 없어서 못 읽습니다.

9. 카테고리: I-F + I-I
   원문    : "Otherwise, [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
   IR      : `call _ = clo<"javascript/throw">()`
   설명    : `[=JavaScript/throw=]`를 전용 `Throw` instruction으로 파싱할 수
             있도록 파서 확장.
             `<l spec=ecmascript>{{TypeError}}</l>`도 `<a abstract-op>`와는
             다른 `<l spec=X>...</l>` cross-spec 참조 마크업이라 별도의 중첩된
             I-I 문제입니다.

10. 카테고리: II-G
    원문    : "If |attribute|'s type is an [=observable array type=], then return |jsValue|'s
              [=backing observable array exotic object=] for |attribute|."
    IR      : `if (= (yet "|attribute|'s type") (yet "an [=observable array type=]")) { return
              (yet "|jsValue|'s [=backing observable array exotic object=] for
              |attribute|") }`
    설명    : WJI 범위의 6개 attribute 중 observable array 타입은 없으므로 false로 가정.

11. 카테고리: II-I
    원문    : "Set |idlObject| to the IDL [=interface type=] value that represents a reference
              to |jsValue|."
    IR      : `idlObject = (yet "the IDL [=interface type=] value that represents a reference
              to |jsValue|")`
    설명    : platform-object의 internal slot에 interface type을 저장한다고 가정하면 internal
              slot을 읽어오는 방식으로 구현할 수 있을 것 같습니다.

12. 카테고리: III-C
    원문    : "Let |R| be the result of running the [=getter steps=] of |attribute| with
              |idlObject| as [=this=]."
    IR      : `let R = (yet "running the [=getter steps=] of |attribute| with |idlObject| as
              [=this=]")`
    설명    : calling-convention 관용구를 call로 정규화해야 합니다.

13. 카테고리: I-I + II-G
    원문    : "If |attribute|'s type is a [=promise type=], then return [=!=] <a
              abstract-op>Call</a>({{%Promise.reject%}}, {{%Promise%}}, «|E|»)."
    IR      : `if (= (yet "|attribute|'s type") (yet "a [=promise type=]")) { let _comp1 =
              (yet "<a abstract-op>Call</a>({{%Promise.reject%}}, {{%Promise%}}, «|E|»)")
              ... }`
    설명    : WJI 범위의 6개 attribute 중 promise 타입은 없으므로 false로 가정. `Call`
              abstract-op 호출 자체는 I-I에도 해당합니다.

14. 카테고리: I-F
    원문    : "Otherwise, end these steps and allow the exception to propagate."
    IR      : `(yet "end these steps and allow the exception to propagate")`
    설명    : try-block과 짝을 이루는 abrupt-completion 전파 제어 흐름입니다.

15. 카테고리: I-I + I-J
    원문    : "Let |name| be the string "<code>get </code>" prepended to |attribute|'s
              [=identifier=]."
    IR      : `let name = @@yet: unresolved ref: Unknown(string "<code>get </code>" prepended
              to |attribute|).identifier`
    설명    : 문자열 연결 표현식이 IR에 없어서(I-J), `.identifier` 필드 접근까지 얽힌 채로
              `@@yet: unresolved ref`라는, 일반 `(yet ...)`보다 더 심각한 내부 해석 실패
              마커로 남습니다(I-I).

16. 카테고리: I-I
    원문    : "Let |F| be <a abstract-op>CreateBuiltinFunction</a>(|steps|, 0, |name|, « »,
              |realm|)."
    IR      : `let F = (yet "<a abstract-op>CreateBuiltinFunction</a>(|steps|, 0, |name|, «
              », |realm|)")`
    설명    : abstract-op 호출 마크업이 인식되지 않습니다.

---

## #7 `attribute setter`

- **명세 위치**: `webidl/index.bs:12388-12468`
- **하는 일**: attribute setter 함수 객체를 만듭니다. `readonly`이면서 예외적으로 setter가
  필요한 경우(`LegacyLenientSetter`/`PutForwards`/`Replaceable`)를 걸러내고, 나머지는
  값을 IDL 값으로 변환해서 `setter steps`를 실행합니다.

1. 카테고리: II-H
   원문    : "If |target| is a [=namespace=]: Assert: |attribute| is [=read only=]. Return
             <emu-val>undefined</emu-val>."
   IR      : `if (= target (yet "a [=namespace=]")) { assert (= attribute ~read only~) return
             undefined }`
   설명    : `target`이 namespace인지 구분하는 record-kind 술어입니다.

2. 카테고리: II-A
   원문    : "If |attribute| is [=read only=] and does not have a [{{LegacyLenientSetter}}],
             [{{PutForwards}}] or [{{Replaceable}}] [=extended attribute=], return undefined;
             there is no [=attribute setter=] function."
   IR      : `if (&& (= attribute ~read only~) (yet "does not have a
             [{{LegacyLenientSetter}}]")) { (yet "[{{PutForwards}}] or [{{Replaceable}}]
             [=extended attribute=], return <emu-val>undefined</emu-val>; there is no
             [=attribute setter=] function") }`
   설명    : extended attribute 필드가 없는 문제(II-A)와 별개로, 콤마로 이어진 하나의 3-way
             OR 조건 + 그 결과인 return 문 하나가 콤마 지점에서 잘못 두 조각 나서, `if`
             조건에는 "does not have a LegacyLenientSetter"만 남고 `PutForwards`/
             `Replaceable` 부분과 뒤따르는 "return undefined"는 if 바디 안의 문장 하나짜리
             yet으로 잘못 들어갔습니다. parser를 확장해서 콤마로 이어진 3-way
             OR 조건을 파싱해야 합니다.

3. 카테고리: I-H
   원문    : "Let |steps| be the following series of steps:"
   IR      : `let steps = (yet "the following series of steps:")`
   설명    : numbered list를 closure/thunk 리터럴로 못 읽습니다.

4. 카테고리: IV-A
   원문    : "If any arguments were passed, then set |V| to the value of the first argument
             passed." (조건 부분)
   IR      : `if (yet "any arguments were passed") { ... }`
   설명    : closure 호출 시 실제 인자가 있었는지 확인하는 관용구입니다.

5. 카테고리: IV-A
   원문    : "... then set |V| to the value of the first argument passed." (값 부분)
   IR      : `V = (yet "the value of the first argument passed")`
   설명    : 4번과 같은 문장의 나머지 절반으로, `argumentList[0]` 접근에 해당합니다.

6. 카테고리: II-E
   원문    : "Let |id| be |attribute|'s [=identifier=]."
   IR      : `let id = (yet "|attribute|'s [=identifier=]")`
   설명    : `attribute.id` 필드 접근으로 바뀌면 됩니다.

7. 카테고리: II-H
   원문    : "If |attribute| is a [=regular attribute=]: ..."
   IR      : `if (= attribute (yet "a [=regular attribute=]")) { ... }`
   설명    : record-kind 술어입니다.

8. 카테고리: I-G
   원문    : "Let |jsValue| be the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise."
   IR      : `let jsValue = (yet "the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise")`
   설명    : #6-4와 동일한 조건부 `let` 값 패턴입니다.

9. 카테고리: VI
   원문    : (#6-5와 동일한 editorial 주석)
   IR      : `(yet "(This will subsequently cause a {{TypeError}} in a few steps, ...)
             <!-- https://www.w3.org/Bugs/... -->")`
   설명    : `Instr.Note`/`DropNotesPass`로 처리하면 됩니다.

10. 카테고리: II-H
    원문    : "If |jsValue| [=is a platform object=], then [=perform a security check=],
              passing |jsValue|, |id|, and "setter"."
    IR      : `if (? jsValue: Unknown["platform object"]) { ... }`
    설명    : `jsValue`가 platform object인지 구분하는 record-kind 술어입니다.

11. 카테고리: II-I
    원문    : "Let |validThis| be true if |jsValue| [=implements=] |target|, or false
              otherwise."
    IR      : `let validThis = (yet "true if |jsValue| [=implements=] |target|, or false
              otherwise")`
    설명    : implements 관계의 불리언 버전입니다(#6-7과 동일 술어).

12. 카테고리: II-A
    원문    : "If |validThis| is false and |attribute| was not specified with the
              [{{LegacyLenientThis}}] [=extended attribute=], then [=JavaScript/throw=] a <l
              spec=ecmascript>{{TypeError}}</l>."
    IR      : `if (&& (= validThis false) (yet "|attribute| was not specified with the
              [{{LegacyLenientThis}}] [=extended attribute=]")) { call _ =
              clo<"javascript/throw">() }`
    설명    : extended attribute 필드가 없어서 못 읽습니다.

13. 카테고리: II-A
    원문    : "If |attribute| is declared with the [{{Replaceable}}] extended attribute, then:
              ..."
    IR      : `if (= attribute (yet "declared with the [{{Replaceable}}] extended
              attribute")) { ... }`
    설명    : extended attribute 필드가 없어서 못 읽습니다.

14. 카테고리: I-F (+ 중첩 I-I)
    원문    : "then [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
    IR      : `call _ = clo<"javascript/throw">()`
    설명    : #6-9와 동일한 이유입니다 — throw 자체는 전용 `Throw` instruction으로; 인자인
              `<l spec=ecmascript>{{TypeError}}</l>`은 또 다른 중첩된 I-I 문제 —
              `<a abstract-op>`와는 다른 `<l spec=X>...</l>` cross-spec 참조 마크업도 별도로
              인식해야 합니다.

15. 카테고리: I-I
    원문    : "Perform [=?=] <a abstract-op>CreateDataPropertyOrThrow</a>(|jsValue|, |id|,
              |V|)."
    IR      : `call _ =
              clo<"<a_abstract-op>CreateDataPropertyOrThrow</a>(|jsValue|,_|id|,_|V|)">()`
    설명    : abstract-op 호출 마크업이 인식되지 않아 깨진 호출이 됩니다.

16. 카테고리: II-A
    원문    : "If |attribute| is declared with a [{{LegacyLenientSetter}}] extended
              attribute, then return <emu-val>undefined</emu-val>."
    IR      : `if (= attribute (yet "declared with a [{{LegacyLenientSetter}}] extended
              attribute")) { return undefined }`
    설명    : extended attribute 필드가 없어서 못 읽습니다.

17. 카테고리: II-A
    원문    : "If |attribute| is declared with a [{{PutForwards}}] extended attribute, then:
              ..." / "Let |Q| be [=?=] <a abstract-op>Get</a>(|jsValue|, |id|)."
    IR      : `if (= attribute (yet "declared with a [{{PutForwards}}] extended
              attribute")) { let _comp1 = (yet "<a abstract-op>Get</a>(|jsValue|, |id|)")
              ... }`
    설명    : extended attribute 필드가 없는 문제와 별개로, 이 분기 안의 `Get(jsValue, id)`
              호출도 원래 정리에 빠져 있던 새 항목입니다(I-I).

18. 카테고리: I-F (+ 중첩 I-I)
    원문    : "If |Q| [=is not an Object=], then [=JavaScript/throw=] a <l
              spec=ecmascript>{{TypeError}}</l>."
    IR      : `if (! (? Q: Record[Object])) { call _ = clo<"javascript/throw">() }`
    설명    : 14번과 동일한 이유입니다.

19. 카테고리: II-B
    원문    : "Let |forwardId| be the identifier argument of the [{{PutForwards}}] extended
              attribute."
    IR      : `let forwardId = (yet "the identifier argument of the [{{PutForwards}}]
              extended attribute")`
    설명    : `[PutForwards]`가 존재 여부(boolean)가 아니라 값(identifier)을 실어나르는
              경우입니다 — extended attribute record가 payload를 가질 수 있어야 합니다.

20. 카테고리: I-I
    원문    : "Perform [=?=] <a abstract-op>Set</a>(|Q|, |forwardId|, |V|,
              <emu-val>false</emu-val>)."
    IR      : `call _ = clo<"<a_abstract-op>Set</a>(|Q|,_|forwardId|,_|V|,_<emu-val>false</emu-val>)">()`
    설명    : abstract-op 호출 마크업이 인식되지 않아 깨진 호출이 됩니다.

21. 카테고리: II-I
    원문    : "Set |idlObject| to the IDL [=interface type=] value that represents a
              reference to |jsValue|."
    IR      : `idlObject = (yet "the IDL [=interface type=] value that represents a
              reference to |jsValue|")`
    설명    : #6-11과 동일한 필드 접근입니다.

22. 카테고리: II-G
    원문    : "If |attribute|'s type is an [=observable array type=] with type argument |T|:
              ..."
    IR      : `if (= (yet "|attribute|'s type") (yet "an [=observable array type=] with type
              argument |T|")) { ... }`
    설명    : WJI 범위에서는 false로 가정 — 이 분기 안의 `@@yet: unresolved ref`도 같은
              이유로 지금은 죽은 코드입니다.

23. 카테고리: III-D + II-G
    원문    : "Let |idlValue| be determined as follows: <dl class="switch">
              <dt>|attribute|'s type is an [=enumeration=]</dt> <dd>...</dd>
              <dt>Otherwise</dt> <dd>...</dd> </dl>"
    IR      : `let idlValue = (yet "determined as follows: <dl class=\"switch\">
              <dt>|attribute|'s type is an [=enumeration=]</dt> <dd>")` ... `(yet "</dd>
              <dt>Otherwise</dt> <dd> |idlValue| is the result of [=converted to an IDL
              value|converting=] |V| to an IDL value of |attribute|'s type")` ... `(yet
              "</dd> </dl>")`
    설명    : `<dl class="switch">` dispatch 구조를 if/else로 spec patch해야 하고(III-D),
              enum 타입 태그 체크(II-G)도 같이 필요합니다. WJI 범위에서는 enum 타입 attribute가
              없어서 false로 가정.

24. 카테고리: III-C
    원문    : "Perform the [=setter steps=] of |attribute|, with |idlObject| as [=this=] and
              |idlValue| as [=the given value=]."
    IR      : `(yet "Perform the [=setter steps=] of |attribute|, with |idlObject| as
              [=this=] and |idlValue| as [=the given value=]")`
    설명    : calling-convention 관용구를 call로 정규화해야 합니다. "the given value"라는
              용어 자체를 아직 어떻게 다뤄야 할지도 미결입니다.

25. 카테고리: I-J
    원문    : "Let |name| be the string "<code>set </code>" prepended to |id|."
    IR      : `let name = (yet "the string \"<code>set </code>\" prepended to |id|")`
    설명    : 문자열 연결 표현식이 IR에 없습니다.

26. 카테고리: I-I
    원문    : "Let |F| be <a abstract-op>CreateBuiltinFunction</a>(|steps|, 1, |name|, « »,
              |realm|)."
    IR      : `let F = (yet "<a abstract-op>CreateBuiltinFunction</a>(|steps|, 1, |name|, «
              », |realm|)")`
    설명    : abstract-op 호출 마크업이 인식되지 않습니다.

---

## #8 `define_the_regular_operations`

- **명세 위치**: `webidl/index.bs:12494-12500`
- **하는 일**: `definition`의 regular operation 중 unforgeable이 아닌 것만 걸러서
  `define_the_operations`에 넘깁니다. `define_the_regular_attributes`(#4)의 operation
  버전입니다.

1. 카테고리: II-F
   원문    : "Let |operations| be the [=list=] of [=regular operations=] that are
             [=members=] of |definition|."
   IR      : `let operations = (yet "the [=list=] of [=regular operations=] that are
             [=members=] of |definition|")`
   설명    : #4-1과 동일한 패턴(변수 이름만 `attributes`→`operations`).

2. 카테고리: V (+ I-A류 단일 호출 뭉개짐)
   원문    : "[=list/Remove=] from |operations| all the [=operations=] that are
             [=unforgeable=]."
   IR      : `call _ = clo<"list/remove">(operations, ~operations~, ~unforgeable~)`
   설명    : #4-2와 완전히 동일한 패턴 — copy 여부 불분명 + predicate가 `~unforgeable~`
             심볼 하나로만 남음.

---

## #9 `define_the_operations`

- **명세 위치**: `webidl/index.bs:12518-12533`
- **하는 일**: 걸러진 operation 목록의 각 operation마다 함수를 만들어
  `DefinePropertyOrThrow`로 `target`에 매답니다. `define_the_attributes`(#5)의 operation
  버전입니다.

1. 카테고리: I-A + I-D
   원문    : "[=list/For each=] [=operation=] |op| of |operations|: ..."
   IR      : `call _ = clo<"list/for_each">((case "OPERATION" op operations))`
   설명    : #5-1과 동일한 패턴 — 루프 바디 없는 단일 호출로 뭉개짐.
             변수에 대한 설명을 case로 잘못 인식하고 있습니다.

2. 카테고리: I-F + I-D
   원문    : "If |op| is not [=exposed=] in |realm|, then [=iteration/continue=]."
   IR      : `if (! (= op (case "EXPOSED" realm))) { (yet "continue") }`
   설명    : #5-2와 동일 패턴.

3. 카테고리: I-G
   원문    : "Let |modifiable| be <emu-val>false</emu-val> if |op| is [=unforgeable=] and
             <emu-val>true</emu-val> otherwise."
   IR      : `let modifiable = (yet "<emu-val>false</emu-val> if |op| is [=unforgeable=]
             and <emu-val>true</emu-val> otherwise")`
   설명    : #5-5와 동일한 조건부 `let` 값 패턴.

4. 카테고리: I-C
   원문    : "Let |desc| be the PropertyDescriptor{\[[Value]]: |method|, \[[Writable]]:
             |modifiable|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]:
             |modifiable|}."
   IR      : `let desc = (yet "the PropertyDescriptor{\[[Value]]: |method|,
             \[[Writable]]: |modifiable|, \[[Enumerable]]: <emu-val>true</emu-val>,
             \[[Configurable]]: |modifiable|}")`
   설명    : record 리터럴 문법.

5. 카테고리: II-E
   원문    : "Let |id| be |op|'s [=identifier=]."
   IR      : `let id = (yet "|op|'s [=identifier=]")`
   설명    : #5-7과 동일 패턴.

6. 카테고리: I-I
   원문    : "Perform [=!=] <a abstract-op>DefinePropertyOrThrow</a>(|target|, |id|,
             |desc|)."
   IR      : `call _ = clo<"<a_abstract-op>DefinePropertyOrThrow</a>(|target|,_|id|,_|desc|)">()`
   설명    : 깨진 abstract-op 호출 — #5-8과 동일.

---

## #10 `creating_an_operation_function`

- **명세 위치**: `webidl/index.bs:12536-12586`
- **하는 일**: 특정 operation에 대한 함수 객체를 만들어 반환합니다. `attribute
  getter`(#6)와 거의 같은 구조 — try/catch로 감싼 `steps`가 `CreateBuiltinFunction`에
  전달됩니다.

1. 카테고리: II-E
   원문    : "Let |id| be |op|'s [=identifier=]."
   IR      : `let id = (yet "|op|'s [=identifier=]")`

2. 카테고리: I-H
   원문    : "Let |steps| be the following series of steps, given function argument
             values |args|: ..."
   IR      : `let steps = (yet "the following series of steps, given function argument
             values |args|:")`
   설명    : `attribute getter`(#6-1)와 같은 steps-block 패턴인데, 이번엔 "given function
             argument values |args|"라는 파라미터 선언까지 같이 붙어 있습니다 — closure가
             실제 호출 인자(`args`)를 받는다는 것까지 명시된 변형입니다.

3. 카테고리: I-F
   원문    : "Try running the following steps: ..."
   IR      : `(yet "Try running the following steps:")`
   설명    : #6-2와 동일한 패턴입니다.

4. 카테고리: II-H
   원문    : "If |target| is an [=interface=], and |op| is not a [=static operation=]:
             ..."
   IR      : `if (&& (= target (yet "an [=interface=]")) (! (= op (yet "a [=static
             operation=]")))) { ... }`
   설명    : #6-3과 동일한 record-kind 술어 + static operation 여부 판별이 추가.

5. 카테고리: I-G
   원문    : "Let |jsValue| be the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise."
   IR      : `let jsValue = (yet "the <emu-val>this</emu-val> value, if it is not
             <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s
             [=realm/global object=] otherwise")`

6. 카테고리: VI
   원문    : "(This will subsequently cause a {{TypeError}} in a few steps, if the
             global object does not implement |target|.) <!--
             https://www.w3.org/Bugs/Public/show_bug.cgi?id=18547#c9 -->"
   IR      : `(yet "(This will subsequently cause a {{TypeError}} in a few steps, ...)
             <!-- https://www.w3.org/Bugs/... -->")`

7. 카테고리: II-H
   원문    : "If |jsValue| [=is a platform object=], then [=perform a security check=],
             passing |jsValue|, |id|, and "method"."
   IR      : `if (? jsValue: Unknown["platform object"])`
   설명    : `jsValue`가 platform object인지 구분하는 record-kind 술어입니다.

8. 카테고리: I-F (+ 중첩 I-I)
   원문    : "If |jsValue| does not [=implement=] the interface |target|,
             [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
   IR      : `if (yet "|jsValue| does not [=implement=] the interface |target|") { call _
             = clo<"javascript/throw">() }`
   설명    : #6-9와 같은 패턴입니다.

9. 카테고리: II-I
   원문    : "Set |idlObject| to the IDL [=interface type=] value that represents a
             reference to |jsValue|."
   IR      : `idlObject = (yet "the IDL [=interface type=] value that represents a
             reference to |jsValue|")`
   설명    : #6-11과 동일한 패턴.

10. 카테고리: I-D + I-M
    원문    : "[=Compute the effective overload set=] for [=regular operations=] (if
              |op| is a regular operation) or for [=static operations=] (if |op| is a
              static operation) with [=identifier=] |id| on |target| and with argument
              count |n|, and let |S| be the result."
    IR      : `call _call1 = clo<"regular_operations">((yet "tuple(Unknown(if |op| is a
              regular operation) or for [=static operations=] (if |op| is a static
              operation))"), (case "IDENTIFIER" id target n))` 다음 `call S =
              clo<"compute_the_effective_overload_set">(_call1)`
    설명    : Compute the effective overload set이 어떤 타입의 IDL construct인지에 따라
              행동이 조금 달라집니다. WJI 상황에서는 constructor와 regular operation인
              경우로 제한할 수 있습니다. 이 때 다른 행동을 보이는 부분에 대해서는 두 개의
              타입에 대한 알고리즘을 하드코딩하여 구현하고, 내부 알고리즘만 공유하는
              방식으로 구현할 수 있습니다. 그래서 이 경우에 해당 부분에서는 하드코딩된
              알고리즘을 호출하도록 변경해야 합니다. (I-M) 그리고 identifier에 대한 설명이
              case로 오인되어 있습니다. (I-D)

11. 카테고리: I-E
    원문    : "Let &lt;|operation|, |values|&gt; be the result of passing |S| and
              |args| to the [=overload resolution algorithm=]."
    IR      : `let _ = (yet "unsupported Let lhs: Unknown(<|operation|, |values|>)")`
    설명    : #3-6과 동일한 튜플 destructuring 문제.

12. 카테고리: II-A
    원문    : "If |operation| is declared with a [{{Default}}] [=extended attribute=],
              then: ..."
    IR      : `if (= operation (yet "declared with a [{{Default}}] [=extended
              attribute=]")) { ... }`

13. 카테고리: III-C
    원문    : "Otherwise, set |R| to the result of running the [=method steps=] of
              |operation|, with |idlObject| as [=this=] and |values| as the argument
              values."
    IR      : `R = (yet "running the [=method steps=] of |operation|, with |idlObject|
              as [=this=] and |values| as the argument values")`

14. 카테고리: II-K + II-G
    원문    : "If |op| has a [=return type=] that is a [=promise type=], then return
              [=!=] <a abstract-op>Call</a>({{%Promise.reject%}}, {{%Promise%}},
              «|E|»)."
    IR      : `if (= (yet "|op| has a [=return type=] that") (yet "a [=promise
              type=]")) { let _comp1 = (yet "<a abstract-op>Call</a>({{%Promise.reject%}},
              {{%Promise%}}, «|E|»)") ... }`

15. 카테고리: I-F
    원문    : "Otherwise, end these steps and allow the exception to propagate."

16. 카테고리: I-D + I-M (10번과 동일 패턴, 두 번째 발생 — argument count 0)
    IR      : `call _call2 = clo<"regular_operations">(...)` 다음 `call S =
              clo<"compute_the_effective_overload_set">(_call2)`

17. 카테고리: IV-B
    원문    : "Let |length| be the length of the shortest argument list in the
              entries in |S|."
    IR      : `let length = (sizeof (yet "the shortest argument list in the entries
              in |S|"))`
    설명    : `#3-10`과 동일한 개념(overload set entry의 선언적 arity 계산). 여기서는
              `WebAssembly.instantiate`처럼 진짜 오버로딩된 operation이 있을 수 있어서,
              `S`가 entry 1개짜리로 자명하지 않을 수 있습니다.

18. 카테고리: I-I
    원문    : "Let |F| be <a abstract-op>CreateBuiltinFunction</a>(|steps|, |length|,
              |id|, « », |realm|)."
    IR      : `let F = (yet "<a abstract-op>CreateBuiltinFunction</a>(|steps|,
              |length|, |id|, « », |realm|)")`

**검증 결과 (구조적 버그, #3/#6/#7과 동일)**: `converted_to_a_javascript_value(R)` 다음의
`return _result1`이 조건 없이 실행되어서, 그 뒤의 promise-type 예외 처리(15~16번)와 실제
함수 객체를 만드는 마지막 두 줄(18~19번, `length`/`F` 구성)이 전부 죽은 코드입니다.
`create_an_interface_object`/`attribute getter`/`attribute setter`에 이어 **네 번째로 같은
"`steps` closure 승격 실패" 패턴이 재현**됩니다.

---

## #11 `compute_the_effective_overload_set`

- **명세 위치**: `webidl/index.bs:3179-3256`
- **하는 일**: operation/legacy factory function/constructor의 오버로드 후보들을 모아
  `(callable, type list, optionality list)` 튜플의 집합 `S`로 만듭니다.

1. 카테고리: **I-L (신규)** — "input variables" prose 목록이 파라미터로 인식 안 됨
   원문    : "The following input variables are used, if they are required: * the
             identifier of the operation or legacy factory function is |A| * the
             argument count is |N| * the interface is |I|. Whenever an argument of an
             extended attribute is mentioned, it is referring to an argument of the
             extended attribute's [=takes a named argument list|named argument
             list=]."
   IR      : 함수 시그니처 자체가 `def compute_the_effective_overload_set(): Unknown`으로
             **파라미터 0개**이고, 위 문장 전체가 body 맨 앞의 평범한 yet 3개(`(yet "the
             identifier ... |A|")`, `(yet "the argument count is |N|")`, `(yet "the
             interface is |I| Whenever an argument ...")`)로 남아 있습니다.
   설명    : 다른 알고리즘들은 전부 "given |A|, |B|, |C|:" 형태로 파라미터를 선언하는데, 이
             알고리즘은 "The following input variables are used, if they are
             required:"라는 드문 bullet-목록 스타일을 씁니다. 지금 추출기는 이 패턴을
             파라미터 선언으로 인식하지 못해서 `A`/`N`/`I`가 진짜 함수 파라미터가 되지
             못했습니다 — 지금까지의 7개+5개 알고리즘 전부 "given ...:" 스타일만 썼어서
             처음 발견된 새로운 카테고리입니다.

2. 카테고리: II-F (ordered set 리터럴)
   원문    : "Let |S| be an [=ordered set=]."
   IR      : `let S = (yet "an [=ordered set=]")`
   설명    : "ordered set"이라는 WebIDL 전용 자료구조의 빈 리터럴 생성 표현이 인식 안 됨.

3. 카테고리: III-D (switch/dl-dt-dd dispatch, 4-way 버전)
   원문    : "Let |F| be an [=ordered set=] with [=set/items=] as follows, according to
             the kind of [=effective overload set=]: <dl class="switch"> : For regular
             operations :: The elements of |F| are the [=regular operations=] with
             identifier |A| defined on interface |I|. : For static operations :: ...
             : For constructors :: ... : For legacy factory functions :: ... </dl>"
   IR      : 4개의 개별 yet으로 쪼개져 있음 — `let F = (yet "...regular operations...")`,
             `(yet ":  For static operations :: ...")`, `(yet ":  For constructors ::
             ...")`, `(yet ":  For legacy factory functions :: ...")`, `(yet
             "</dl>")`
   설명    : `#7-23`(enum switch, 2-way)과 같은 `<dl class="switch">` dispatch
             구조인데, 이번엔 4-way라 더 잘게 쪼개져 있습니다.

4. 카테고리: (III-C류, note 포함)
   원문    : "Let |maxarg| be the maximum number of arguments the operations, legacy
             factory functions, or callback functions in |F| are declared to take.
             For [=variadic=] operations and legacy factory functions, the argument on
             which the ellipsis appears counts as a single argument."
   IR      : `let maxarg = (yet "the maximum number of arguments the operations,
             legacy factory functions, or callback functions in |F| are declared to
             take")` 다음 `(yet "For [=variadic=] operations and legacy factory
             functions, the argument on which the ellipsis appears counts as a single
             argument")`

5. 카테고리: I-I
   원문    : "Let |max| be <a abstract-op>max</a>(|maxarg|, |N|)."
   IR      : `let max = (yet "<a abstract-op>max</a>(|maxarg|, |N|)")`

6. 카테고리: I-A (`set/for_each` 변형)
   원문    : "[=set/For each=] operation or extended attribute |X| in |F|: ..."
   IR      : `call _ = clo<"set/for_each">(X, F)`
   설명    : I-A의 `list/for_each`와 근본 원인이 같습니다 — "set" 버전이지만 여전히 루프
             바디 없는 단일 호출입니다. 이번엔 case-태그 오인식(I-D) 없이 깔끔한 2-인자
             호출로 컴파일된 게 다릅니다.

7. 카테고리: II-F
   원문    : "Let |arguments| be the [=list=] of arguments |X| is declared to take."
   IR      : `let arguments = (yet "the [=list=] of arguments |X| is declared to
             take")`

8. 카테고리: (신규, "type list"/"optionality list" 리터럴)
   원문    : "Let |types| be a [=type list=]." / "Let |optionalityValues| be an
             [=optionality list=]."
   IR      : `let types = (yet "a [=type list=]")` / `let optionalityValues = (yet "an
             [=optionality list=]")`
   설명    : WebIDL 전용 리스트류 자료구조의 빈 리터럴 생성 표현들이 전부 이런 식으로
             인식되지 않습니다(2번의 "ordered set"과 같은 계열).

9. 카테고리: I-A
   원문    : "[=list/For each=] |argument| in |arguments|: ..."
   IR      : `call _ = clo<"list/for_each">(argument, arguments)`

10. 카테고리: (append 표현, ⚠️ 순서가 뒤바뀐 것으로 보임)
    원문    : "[=list/Append=] the type of |argument| to |types|." / "[=list/Append=]
              "variadic" to |optionalityValues| if |argument| is a final, variadic
              argument, "optional" if |argument| is [=optional argument|optional=], and
              "required" otherwise."
    IR      : `push types < (yet "the type of |argument|")` 다음 `push (yet
              "<emu-val>false</emu-val> ...") < "variadic"` — 정확히는 두 번째 push의
              좌변이 조건부 표현이고 우변이 "variadic" 리터럴입니다.
    설명    : ⚠️ 첫 번째 `push`는 "X를 Y에 append"가 `push Y < X`로 뒤집혀서 컴파일된
              걸로 보이는데(append 대상이 왼쪽), 두 번째 `push`는 조건부 표현(원래
              append할 *값*)이 왼쪽에, `"variadic"`(원래 조건 중 하나의 *리터럴*)이
              오른쪽에 와서 더 헷갈리게 뒤섞여 있습니다. `push X < Y`의 좌/우가 무엇을
              의미하는지부터 다시 확인해야 할 것 같습니다 — 진짜 버그일 수 있습니다.

11. 카테고리: I-E류 (튜플 리터럴 생성 실패)
    원문    : "[=set/Append=] the [=tuple=] (|X|, |types|, |optionalityValues|) to
              |S|."
    IR      : `call _ = clo<"set/append">(~tuple~, (yet "tuple(Var(X)Var(types)Var(optionalityValues))"),
              S)`
    설명    : WebIDL의 `(a, b, c)` 튜플 리터럴이 `tuple(...)`이라는 yet 텍스트로 깨져서
              들어갑니다 — I-E(튜플 *destructuring* 실패)의 반대쪽, "튜플 *생성*" 실패입니다.

12. 카테고리: **I-K (신규)** — range가 call-syntax 위치에서 lowering 안 됨(컴파일러 크래시의
    근본 원인)
    원문    : "If |X| is declared to be [=variadic=], then: 1. [=list/For each=] |i| in
              [=the range=] |n| to |max| − 1, inclusive: ..."
    IR      : `call _ = clo<"list/for_each">(i, (yet "IMPOSSIBLE (unreachable after
              lowering): range Var(n) to Unknown(|max| − 1, inclusive:)"))`
    설명    : ⚠️ **이번 조사에서 발견한 컴파일러 크래시의 근본 원인입니다.** "[=the
              range=] X to Y, inclusive"는 `Expr.Range`로 파싱되는데, 이게 자연어
              "For |i| in the range X to Y:" 형태의 `Instr.For(elem, Range(...),
              body)`가 아니라 `list/For each`의 **call-syntax 인자 자리**에 등장하면,
              `Range`를 없애는 유일한 pass인 `ExpandForPass`가 이 모양을 못 잡습니다.
              원래 `Compiler.compileExpr`는 "lowering 이후엔 절대 안 남아있어야 함"으로
              간주해서 이 케이스를 만나면 즉시 예외를 던지는데, `Compiler.compile`이
              `algos.flatMap(compileAlgo)`로 알고리즘을 전부 한 번에 처리하다 보니 이
              예외 하나가 **그 알고리즘 하나가 아니라 이후 모든 알고리즘의 컴파일까지 통째로
              날려버립니다**(§VII보다 파급 범위가 더 큼). 지금은 이 문서를 정리하기 위해
              `Compiler.impossibleExpr`로 임시 우회해서 `(yet "IMPOSSIBLE ...")`로 표시되고
              있습니다(문서 맨 위 참고 — 이 우회는 곧 되돌릴 예정입니다). 근본 수정 방향은
              `ExpandForPass`가 `Instr.Perform`/`Instr.PerformClosure` 같은 call-syntax
              인자 안에 중첩된 `Expr.Range`도 찾아서 lowering하도록 확장하는 것입니다.
              같은 패턴이 이 알고리즘 안에서만 4번 더 나옵니다(13~15번, 그리고 while 루프
              관련 별도 항목).

13. 카테고리: I-K (12번과 동일 패턴)
    IR      : `call _ = clo<"list/for_each">(j, (yet "IMPOSSIBLE (unreachable after
              lowering): range Num(0) to Unknown(|n| − 1, inclusive:)"))`

14. 카테고리: I-K (12번과 동일 패턴)
    IR      : `call _ = clo<"list/for_each">(j, (yet "IMPOSSIBLE (unreachable after
              lowering): range Var(n) to Unknown(|i|, inclusive:)"))`

15. 카테고리: I-K (12번과 동일 패턴, while 루프 안)
    IR      : `call _ = clo<"list/for_each">(j, (yet "IMPOSSIBLE (unreachable after
              lowering): range Num(0) to Unknown(|i| − 1, inclusive:)"))`

16. 카테고리: (⚠️ while 루프 자체도 잘못 컴파일된 것으로 보임)
    원문    : "Let |i| be |n| − 1." / "[=iteration/While=] |i| ≥ 0: ..."
    IR      : `let i = (yet "|n| − 1")`(산술 표현이 파싱 안 돼서 yet) 다음 `call _ =
              clo<"iteration/while">(i, 0)`
    설명    : "While |i| ≥ 0:"이 `Instr.While`로 컴파일되지 않고 `iteration/while`이라는
              이름의 함수를 호출하는 것처럼 컴파일돼 있고, 역시 바디가 안 붙어 있습니다.
              원문의 "|i| ≥ 0"이 `≥`(Ge) 비교라서 `Cond.Compare` 인식 경로를 못 탄
              것으로 보이는데, 정확한 원인은 추가 확인이 필요합니다.

17. 카테고리: (신규, dfn-link 부연설명 오인식의 또 다른 변형)
    원문    : "If |arguments|[|i|] is not [=optional argument|optional=] (i.e., it is
              not marked as "optional" and is not a final, variadic argument), then
              [=iteration/break=]."
    IR      : `call _call1 = clo<"optional_argument">((yet "i.e."), (yet "it is not
              marked as \"optional\" and is not a final"), (yet "variadic
              argument"))` 다음 `if (! (= arguments[i] _call1)) { call _ =
              clo<"iteration/break">() }`
    설명    : dfn-link `[=optional argument|optional=]` 뒤에 괄호로 붙은 부연설명
              "(i.e., ...)"이, `optional_argument`라는 가짜 함수의 인자 3개로 쪼개져
              들어갔습니다 — I-D 계열과 비슷하지만 트리거가 dfn-link 자체가 아니라 그 뒤에
              붙는 괄호 부연설명이라는 점이 다릅니다. `iteration/break` 자체는 깔끔하게
              컴파일됩니다(yet 아님) — break 문 인식은 이미 잘 되는 것으로 보입니다.

18. 카테고리: I-A + I-K
    원문    : "[=list/For each=] |j| in [=the range=] 0 to |i| − 1, inclusive: ..."
    IR      : 15번과 동일(위에서 이미 다룸). 여기서는 while 루프 바디 안에 있다는 것만
              다릅니다.

19. 카테고리: (append, 재확인)
    원문    : "[=list/Append=] |types|[|j|] to |t|." / "[=list/Append=]
              |optionalityValues|[|j|] to |o|."
    IR      : `push t < types[j]` / `push o < optionalityValues[j]` — **이건 깔끔하게
              컴파일됩니다(yet 아님)**. 10번과 비교하면, "X\[j\]를 Y에 append"처럼 append할
              값이 인덱싱 표현일 때는 정상 컴파일되고, 조건부 표현(if/otherwise)일 때만
              깨진다는 뜻으로 보입니다 — 10번 버그의 트리거 조건을 좁히는 데 도움이 되는
              대조 사례입니다.

---

## #12 `overload_resolution_algorithm`

- **명세 위치**: `webidl/index.bs:11519-11783`
- **하는 일**: `compute_the_effective_overload_set`이 만든 candidate 집합 `S`와 실제 JS
  인자 목록 `args`를 받아서, 실제로 호출할 `(callable, values)` 쌍을 고릅니다. 몸통 대부분이
  "JS 값 `V`의 런타임 타입에 따라 어떤 IDL 타입으로 갈지"를 고르는 17갈래 dispatch입니다.

**앞부분(설정)**:

1. 카테고리: IV-B류
   원문    : "Let |maxarg| be the length of the longest type list of the entries in
             |S|."
   IR      : `let maxarg = (sizeof (yet "the longest type list of the entries in
             |S|"))`

2. 카테고리: IV-A
   원문    : "Let |n| be the [=list/size=] of |args|."
   IR      : `let n = (sizeof args)`(→ 정상 컴파일, yet 아님)

3. 카테고리: (신규, min/제거 연산)
   원문    : "Initialize |argcount| to be min(|maxarg|, |n|)." / "Remove from |S| all
             entries whose type list is not of length |argcount|."
   IR      : `(yet "Initialize |argcount| to be min(|maxarg|, |n|)")` / `(yet "Remove
             from |S| all entries whose type list is not of length |argcount|")`
   설명    : "Initialize X to be ..." 서술형 대입문과, 조건부 predicate로 필터링하는
             `Remove` 문 둘 다 인식이 안 됩니다. V(list 연산 의미론)와 인접한 문제입니다.

4. 카테고리: I-F (+ 중첩 I-I)
   원문    : "If |S| is empty, then [=JavaScript/throw=] a <l
             spec=ecmascript>{{TypeError}}</l>."
   IR      : `if (= S ~empty~) { call _ = clo<"javascript/throw">() }`
   설명    : throw는 깔끔히 컴파일됨 — #6-9/#7-14/#7-18과 동일한 논의.

5. 카테고리: (신규, Initialize 서술형 대입)
   원문    : "Initialize |d| to −1." / "Initialize |method| to
             <emu-val>undefined</emu-val>." / "Initialize |values| to be an empty
             list, where each entry will be either an IDL value or the special value
             "missing"." / "Initialize |i| to 0."
   IR      : 전부 `(yet "Initialize ... to ...")` 형태의 개별 yet.
   설명    : 3번과 같은 "Initialize X to Y" 패턴 — 사실상 `let X = Y`와 동치인데
             인식되지 않습니다. `Let X be Y.` 패턴은 이미 잘 되므로, "Initialize"도
             동의어로 처리하면 한 번에 여러 개 풀릴 것 같습니다.

6. 카테고리: (신규, "distinguishing argument index")
   원문    : "If there is more than one entry in |S|, then set |d| to be the
             [=distinguishing argument index=] for the entries of |S|."
   IR      : `if (= (yet "there") (yet "more than one entry in |S|")) { d = (yet "be
             the [=distinguishing argument index=] for the entries of |S|") }`
   설명    : ⚠️ 조건 자체("there is more than one entry in |S|")가 `(= (yet "there")
             (yet "more than one entry in |S|"))`라는, 원문과 무관한 이상한 동등 비교로
             쪼개져 있습니다 — "there is X"라는 존재 표현이 통째로 오인식된 것으로 보입니다.

**중간(메인 루프, `i < d`까지)**:

7. 카테고리: (신규, while-loop + 조건부 append)
   원문    : "While |i| &lt; |d|: 1. Let |V| be |args|[|i|]. 1. Let |type| be the type
             at index |i| in the type list of any entry in |S|. ... 1. Let
             |optionality| be the value at index |i| in the list of [=optionality
             values=] of any entry in |S|. 1. If |optionality| is "optional" and |V|
             is <emu-val>undefined</emu-val>, then: [기본값 또는 missing 추가] 1.
             Otherwise, append to |values| the result of [=converted to an IDL
             value|converting=] |V| to IDL type |type|. 1. Set |i| to |i| + 1."
   IR      : `while (< i d) { let V = args[i]; let type = (yet "the type at index |i|
             in the type list of any entry in |S|"); let optionality = (yet "the
             value at index |i| in the list of [=optionality values=] of any entry
             in |S|"); if (&& (= optionality (yet "“optional”")) (= V undefined)) {
             ... } else { (yet "append to |values| the result of [=converted to an
             IDL value|converting=] |V| to IDL type |type|") }; i = (+ i 1) }`
   설명    : while 루프 자체(`i < d`)는 정상 컴파일됩니다 — `#11`의 `iteration/While |i|
             ≥ 0`이 깨졌던 것과 대조적으로, 여기 `<` 비교는 잘 처리됩니다(6번의 대조
             사례처럼 `≥`/`Ge` 쪽만 문제인 듯). 그 안의 "append to |values| ..." 서술형
             문장들은 인식이 안 됩니다(list append 관용구가 `[=list/Append=] X to Y`
             형태가 아니라 "append to Y the result of X"처럼 도치되면 못 읽는 것으로
             보임).

**핵심(17갈래 타입 dispatch, `i = d`일 때)**: 아래는 전부 `if |i| = |d|` 안에서, JS 값
`V`의 런타임 타입(platform object, ArrayBuffer, DataView, typed array, callable, async
iterable, iterable, plain object, Boolean, Number, BigInt, string, ...)에 따라 "S의 어느
entry들을 살릴지"를 고르는 17개의 거의 동형인 분기입니다. 각각 원문은 "Otherwise: if |V|
[=is-a-X=], and there is an entry in |S| that has one of the following types at
position |i| of its type list, «...5~6개 타입 나열...», then remove from |S| all other
entries."라는 같은 틀을 공유합니다.

8. 카테고리: **II-G (대규모)** — WebIDL 타입 dispatch 테이블 전체
   원문    : 17개 분기, 예시(`platform object` 분기): "Otherwise: if |V| [=is a platform
             object=], and there is an entry in |S| that has one of the following
             types at position |i| of its type list, * an [=interface type=] that
             |V| [=implements=] * {{object}} * a [=nullable type|nullable=] version of
             any of the above types * an [=annotated type=] whose [=annotated
             types/inner type=] is one of the above types * a [=union type=],
             [=nullable type|nullable=] union type, or [=annotated type|annotated=]
             union type that has one of the above types in its [=flattened member
             types=], then remove from |S| all other entries."
   IR      : 각 분기가 5~10개의 개별 yet으로 쪼개진 `{ (yet "a [=nullable type=]") (yet
             "a [=dictionary type=]") ... }` 블록들로 컴파일되어 있고, 바깥 조건(`if |V|
             is-a-X`)도 대부분 인식되지 않습니다. 예외적으로 `IsCallable`/`GetMethod`
             abstract-op 호출이 들어가는 분기(callback function, async
             sequence/iterable, sequence)는 그 abstract-op 호출 자체는
             완료-레코드 처리까지 포함해서 정상적으로 컴파일됩니다 — 예:
             `let _comp1 = (yet "<a abstract-op>GetMethod</a>(|V|, {{%Symbol.asyncIterator%}})")`
             다음 completion-record 분기 처리.
   설명    : 이건 `docs/hardcodes.md` #1~#3, `webidl_yet_categorized`의 II-G("IDL 타입
             태그 조회")와 **완전히 같은 근본 원인**이 이번엔 개별 "이 attribute의 타입이
             X인가" 체크 하나가 아니라, **"JS 런타임 값이 IDL 타입 목록 중 어느 것과
             매치되는가"를 판정하는 진짜 dispatch 테이블 전체**로 나타난 것입니다. WJI
             파이프라인에 "선언된 IDL 타입"이라는 개념이 흐르지 않는다는 것과, "런타임 JS
             값 → 매치되는 IDL 타입" 판정 규칙 자체가 없다는 것, 두 gap이 여기서 정면으로
             만납니다. 17개 분기를 개별적으로 다 고치기보다는, "IDL 타입 목록에서 어떤
             타입이 이 JS 값과 매치하는가"라는 하나의 범용 판정 함수를 설계하고 그걸
             17번 재사용하는 게 맞는 방향으로 보입니다 — 이 문서에서 개별 분기를 17개 다
             풀어서 적지 않고 한 항목으로 묶은 이유이기도 합니다.
   - 17개 분기 목록(전부 "then remove from |S| all other entries"로 끝남): (1) `V`가
     `undefined`이고 optional인 entry가 있으면, (2) `V`가 `null`/`undefined`이고
     nullable/dictionary/annotated/union 타입 entry가 있으면, (3) `V`가 platform
     object이고 interface-type/object entry가 있으면, (4) `V`가 `[[ArrayBufferData]]`
     슬롯이 있는 Object이고 ArrayBuffer/SharedArrayBuffer/object entry가 있으면, (5)
     `[[DataView]]` 슬롯 + DataView/object entry, (6) `[[TypedArrayName]]` 슬롯 + typed
     array/object entry, (7) `IsCallable(V)`이고 callback function/object entry가
     있으면, (8) `V`가 Object이고 async sequence entry가 있고(+ `[[StringData]]` 슬롯이
     없고 + `GetMethod(V, @@asyncIterator/@@iterator)`가 undefined가 아니면), (9)
     Object + sequence entry + `GetMethod(V, @@iterator)`가 undefined가 아니면, (10)
     Object + callback interface/dictionary/record/object entry, (11) `V`가
     Boolean이고 boolean entry, (12) `V`가 Number이고 numeric entry, (13) `V`가
     BigInt이고 bigint entry, (14) (무조건) string entry가 있으면, (15) (무조건) numeric
     entry, (16) (무조건) boolean entry, (17) (무조건) bigint entry, 마지막으로 `any`
     entry가 있으면 그걸 쓰고, 그것도 없으면 TypeError.

**뒷부분(정리)**:

9. 카테고리: II-E류
   원문    : "Let |callable| be the [=operation=] or [=extended attribute=] of the
             single entry in |S|."
   IR      : `let callable = (yet "the [=operation=] or [=extended attribute=] of the
             single entry in |S|")`

10. 카테고리: (신규, sequence-from-iterable 변환)
    원문    : "If |i| = |d| and |method| is not <emu-val>undefined</emu-val>, then: 1.
              Let |V| be |args|[|i|]. 1. Let |T| be the type at index |i| in the type
              list of the remaining entry in |S|. 1. Assert: |T| is a [=sequence
              type=]. 1. Append to |values| the result of [=creating a sequence from
              an iterable|creating a sequence=] of type |T| from |V| and |method|. 1.
              Set |i| to |i| + 1."
    IR      : `if (&& (yet "|i| = |d|") (! (= method undefined))) { let V = args[i];
              let T = (yet "the type at index |i| in the type list of the remaining
              entry in |S|"); assert (= T (yet "a [=sequence type=]")); (yet "Append
              to |values| the result of [=creating a sequence from an
              iterable|creating a sequence=] of type |T| from |V| and |method|"); i =
              (+ i 1) }`
    설명    : `|i| = |d|`라는 단순 동등 비교조차 `(yet "|i| = |d|")`로 안 읽힙니다(6번과
              같은 "there is" 문제와 별개로, 등호 자체가 `=` 기호(수학 기호)라서 파서가
              놓치는 것으로 보임 — 확인 필요).

11. 카테고리: (7번과 동일 while + append 패턴, `i < argcount` 구간)
    원문    : "While |i| &lt; |argcount|: ... (7번과 거의 동일)"
    IR      : 7번과 구조적으로 동일 (변수만 `d`→`argcount`).

12. 카테고리: (신규, "while X is less than Y" 자연어 비교)
    원문    : "While |i| is less than the number of arguments |callable| is declared
              to take: 1. If |callable|'s argument at index |i| is declared with a
              [=optional argument/default value=], then append to |values| that
              default value. 1. Otherwise, if |callable|'s argument at index |i| is
              not variadic, then append to |values| the special value "missing". 1.
              Set |i| to |i| + 1."
    IR      : `while (= i (yet "less than the number of arguments |callable| is
              declared to take")) { if (= (yet "|callable|'s argument at index |i|")
              (yet "declared with a [=optional argument/default value=]")) { (yet
              "append to |values| that default value") } else if (! (= (yet
              "|callable|'s argument at index |i|") (yet "variadic"))) { (yet "append
              to |values| the special value "missing"") } else nop; i = (+ i 1) }`
    설명    : ⚠️ 심각한 버그입니다 — "|i| is less than N"이라는 **부등호 비교**가
              `(= i (yet "..."))`라는 **동등 비교**로 잘못 컴파일되어 있습니다. `<`
              기호가 아니라 "is less than"이라는 자연어 표현이라서 비교 연산자
              추출기가 이걸 아예 못 알아보고, `while (= i ...)`라는 (원문과 반대 의미의)
              엉뚱한 조건으로 떨어진 것으로 보입니다. `#11`의 `≥`(Ge) 문제, 10번의
              `=`(Eq) 문제와 함께, **비교 연산자 인식이 `<`/`>`가 아닌 자연어·기호
              변형에서 전반적으로 취약**하다는 걸 보여주는 세 번째 사례입니다.

13. 카테고리: III-C
    원문    : "Return the pair &lt;|callable|, |values|&gt;."
    IR      : `return (yet "the pair &lt;|callable|, |values|&gt;")`
    설명    : 튜플 리터럴 반환 — `#11-11`(set/append의 튜플)과 같은 "튜플 생성" 문제의
              또 다른 사례.

---

## 요약: `#8`~`#12` 조사에서 새로 확인된 것

- **컴파일러 크래시의 근본 원인 (I-K, 가장 중요)**: `[=the range=] X to Y, inclusive`가
  자연어 카운팅 루프(`Instr.For`)가 아니라 `list/For each`류 call-syntax의 인자 자리에
  등장하면 `ExpandForPass`가 못 잡고, `Compiler.compileExpr`의 "unreachable after
  lowering" 가드에 걸려 **`Compiler.compile` 전체가 죽습니다**(하나의 알고리즘이 아니라
  `flatMap` 순서상 그 뒤에 오는 모든 알고리즘까지). `compute_the_effective_overload_set`
  안에서만 4번 나옵니다. `ExpandForPass`가 `Instr.Perform`/`Instr.PerformClosure` 인자
  안에 중첩된 `Expr.Range`도 찾아 lowering하도록 확장하는 게 근본 수정 방향입니다.
- **비교 연산자 인식이 `<`/`>` 기호가 아닌 형태에서 전반적으로 약함**: `#11`의 "While |i| ≥
  0"(`iteration/while` 함수 호출로 오인), `#12`-10번의 "|i| = |d|"(yet으로 안 풀림),
  `#12`-12번의 "while |i| is less than N"(**`<` 대신 `=`로 잘못 컴파일**, 의미가 반대로
  뒤집힌 실제 버그)까지, 세 가지 서로 다른 실패 모드가 확인됩니다.
- **새로운 파라미터 선언 스타일 (I-L)**: `compute_the_effective_overload_set`은 "given
  X, Y, Z:" 대신 "The following input variables are used, if they are required:"라는
  bullet 목록으로 파라미터를 선언하는데, 이게 전혀 인식되지 않아서 함수가 파라미터 0개로
  컴파일됩니다 — 지금까지 조사한 12개 알고리즘 중 이 스타일은 여기가 유일합니다.
- **III-B가 저절로 풀리는 사례 확인**: `attribute getter`/`attribute setter`(#5-3/#5-4)가
  yet으로 남은 진짜 이유는 관용구 자체를 못 읽어서가 아니라, 참조 대상 알고리즘이
  `webidlFilter`에 없어서였을 가능성이 높습니다 — `creating an operation function`을
  `webidlFilter`에 추가하니 완전히 같은 "Let X be the result of ALGO given ..." 관용구가
  파서 수정 없이 그냥 `call`이 됐습니다.
- **list append 관용구가 도치되면 못 읽음**: "append to |values| the result of X"처럼
  "append to TARGET VALUE" 어순(목적어가 뒤에 오는 도치문)은 인식이 안 되는 반면, 앞서
  나온 `[=list/Append=] X to Y` 어순은 (`#11`-19번처럼 X가 단순 인덱싱일 때) 정상
  컴파일됩니다.
- **"Initialize X to Y"가 "Let X be Y"의 동의어로 처리되지 않음**: `overload_resolution_algorithm`
  앞부분에 `Initialize` 동사로 시작하는 대입문이 여러 개 있는데 전부 개별 yet으로 빠졌습니다.
- **II-G(IDL 타입 dispatch)가 최대 규모로 재현됨**: 17갈래 타입 매칭 테이블 전체가 이
  gap의 가장 크고 복잡한 사례입니다 — 개별 분기를 하나씩 고치기보다 "JS 런타임 값이 IDL
  타입 후보 목록 중 무엇과 매치하는가"라는 범용 판정 함수 하나를 설계하는 게 맞는 방향으로
  보입니다.
