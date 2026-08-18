# WebIDL yet 카테고리 정리

- **I. 파서(Parser) 문법 gap** —
- **II. Record/IR 모델 gap** — interface/attribute/member/realm 등 레코드에 아직 없는 *필드*가
  필요함. 지금 구상 중인 "interface/attribute를 IR record로 표현" 설계와 직접 관련된 부분.
- **III. 알고리즘-호출 관용구(idiom) 정규화** — 파서 문제도 레코드 문제도 아니고, WebIDL 특유의
  반복되는 완곡한 표현("the X object for Y", "an X given Y", "Perform the X steps of Y with Z as
  this")을 명시적 `call`로 바꿔주는 spec patch가 필요함.
- **IV/V/VI. 기타** — argument-list 모델링, list 연산 의미론, 에디토리얼 노트.

---

## I. 파서 문법 gap


### I-A. 내부 슬롯 / bracket 표기 접근 (`\[[Foo]]`)

- `#1-3` - Let |unforgeables| be the value of the \[[Unforgeables]] slot of the [=interface object=] of |ancestor interface| in |realm|.
- `#1-4` - Let |descriptor| be [=!=] |unforgeables|.\[[GetOwnProperty]](|key|).
- `#1-7` - Set |instance|.\[[…]] as defined in [[…]]

### I-B. Closure expression
**공통 원인**: `as defined in [[#platform-object-setprototypeof]]` 로 이어지는 문장을 파싱하지 못함.
**대응**: closures로 인식.
- `#1-7`   -- Set |instance|.\[[SetPrototypeOf]] as defined in [[#platform-object-setprototypeof]].

### I-C. record/struct 리터럴 (`PropertyDescriptor{...}`)

- `#2-10` - the PropertyDescriptor{\[[Writable]]: <emu-val>true</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>true</emu-val>, \[[Value]]: |constructor|}
- `#3-13` - PropertyDescriptor{\[[Value]]: |proto|, \[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>false</emu-val>, \[[Configurable]]: <emu-val>false</emu-val>} 
- `#5-6` - the PropertyDescriptor{\[[Get]]: |getter|, \[[Set]]: |setter|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |configurable|}
- `#9-4` - the PropertyDescriptor{\[[Value]]: |method|, \[[Writable]]: |modifiable|, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: |modifiable|}
- `#20-3` - the PropertyDescriptor{\[[Writable]]: <emu-val>false</emu-val>, \[[Enumerable]]: <emu-val>true</emu-val>, \[[Configurable]]: <emu-val>false</emu-val> \[[Value]]: |value|}

### I-F. 제어 흐름 관련

- `#6-2`, `#10-3` — Try running the following steps: ... And then, if <a lt="an exception was thrown">an exception |E| was thrown</a>:
- `#6-14`, `#10-15` — end these steps and allow the exception to propagate
- `#9-2` - [=iteration/continue=]
- `#11-16` - [=iteration/break=]

### I-J. 문자열 연결(concat) 표현식: IR에 string concat expression 추가.

- `#6-15` — `let name = "get " prepended to |attribute|.identifier`
- `#7-25` — `let name = "set " prepended to |id|`

### I-K. set, list, tuple 관련
- `#11-2` - Let |S| be an [=ordered set=].
- `#11-8` - Let |types| be a [=type list=].
          - Let |optionalityValues| be an [=optionality list=].
- `#11-11` - [=set/Append=] the [=tuple=] (|X|, |types|, |optionalityValues|) to |S|.
- `#11-17` - Let |t| be a [=type list=].
              Let |o| be an [=optionality list=].
- `#11-19` - [=set/Append=] the [=tuple=] (|X|, |t|, |o|) to |S|.
- `#12-4` - |S| is empty
    - assume false

### I-L. numeric operation
- `#11-13` - |n| − 1
- `#12-10` - |i| = |d|

### I-M. switch/dl-dt-dd 형태 dispatch
- `#7-23` — `<dl class="switch"> <dt>attribute's type is an enumeration</dt> <dd>...</dd> <dt>Otherwise</dt> <dd>...</dd> </dl>`
    - assume no enumuration attribute
- `#11-3`

### I-N. reference to IDL interface type value
- #6-11, #7-21, #10-9
    - Set |idlObject| to the IDL [=interface type=] value that represents a reference to |jsValue|.
    -> idlObject = jsValue

### I-P. `<emu-val>this</emu-val>` 값 / `X's [=realm/global object=]` 필드 미인식
**공통 원인**: 예전 category I-G(`X if COND, Y otherwise`)에 묶여있던 두 branch 값 —
conditional 구조 자체는 이제 파싱되지만, 각 branch의 값이 따로 막혀 있음.
- `<emu-val>this</emu-val>` 값: `ThisOnly`가 인식하는 `**this**`(굵게-별표) 형태가 아니라
  `<emu-val>...</emu-val>` 마크업으로 감싸진 형태라 인식 못 함.
- `|realm|'s [=realm/global object=]`: `AssociatedRealm`(`X's [=associated Realm=]`)과 같은
  possessive-link 꼴이지만 다른 필드 이름이라 별도 매핑 필요.

- `#6-4`, `#7-8`, `#10-5` - Let |jsValue| be the <emu-val>this</emu-val> value, if it is not <emu-val>null</emu-val> or <emu-val>undefined</emu-val>, or |realm|'s [=realm/global object=] otherwise.

### I-O. instruction 파싱 관련
- `#12-2` - Initialize |argcount| to be min(|maxarg|, |n|).
- `#12-3` - Remove from |S| all entries whose type list is not of length |argcount|.
- `#12-5` - Initialize |d| to −1.
          - Initialize |method| to <emu-val>undefined</emu-val>
          - Initialize |values| to be an empty list, where each entry will be either an IDL value or the special value "missing".
          - Initialize |i| to 0."
- `#12-9` - append to |values| that default value
          - append to |values| the special value “missing”
          - append to |values| the result of [=converted to an IDL value|converting=] |V| to IDL type |type|

- `#1-2` - [=list/iterate|For every=] [=interface=] |ancestor interface| in |interfaces|: ...
         - [=list|For each=] element |key| of |keys|: ...
- `#4-2` - [=list/Remove=] from |attributes| all the [=attributes=] that are [=unforgeable=].
- `#5-1` - [=list/For each=] [=attribute=] |attr| of |attributes|: ...
- `#6-9`, `#7-13`, `#10-8` - [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>.
- `#9-1` - [=list/For each=] [=operation=] |op| of |operations|: ...
- `#11-6` - [=set/For each=] operation or extended attribute |X| in |F|: ...
- `#11-9` - [=list/For each=] |argument| in |arguments|: ...
- `#11-11` - [=set/Append=] the [=tuple=] (|X|, |types|, |optionalityValues|) to |S|.
- `#11-14` - [=iteration/While=] |i| ≥ 0: ...
- `#11-16` - [=iteration/break=]
- `#11-18` - [=list/For each=] |j| in [=the range=] 0 to |i| − 1, inclusive: ...
- `#11-19` - [=set/Append=] the [=tuple=] (|X|, |t|, |o|) to |S|.
- `#20-1` - [=list/For each=] [=constant=] |const| that is a [=member=] of |definition|

---

## II. Record/IR 모델 gap (interface/attribute/member record 설계와 직결)

이 그룹이 사용자가 원래 예로 든 `(= interface (yet "declared with the {{Global}} extended
attribute="))` 같은 케이스들이 속하는 곳입니다. 전부 "interface/attribute/member/realm 레코드에
아직 없는 필드를 읽어야 한다"는 동일한 근본 원인을 공유합니다 — record 모양을 어떻게 설계하느냐에
따라 한 번에 여러 yet이 같이 해소됩니다.

### II-A. Extended attribute / member

- `#1-5`, `#2-1`, `#2-5`, `#2-7` - |interface| is declared with the [{{Global}}] [=extended attribute=]
    - assume false
- `#2-6` - |interface| has any [=member=] declared with the [{{Unscopable}}] [=extended attribute=]`[Unscopable]
    - assume false
- `#2-8` - the [{{LegacyNoInterfaceObject}}] [=extended attribute=] was not specified on |interface|
    - assume false
- `#3-2` - |I| was not declared with a [=constructor operation=]
    - assume false
- `#3-9` - |I| was declared with a [=constructor operation=]
    - assume false
- `#4-2` - [=list/Remove=] from |attributes| all the [=attributes=] that are [=unforgeable=].
    - assume no unforgeable attributes
- `#6-8` - |attribute| was specified with the [{{LegacyLenientThis}}] [=extended attribute=]
    - assume false
- `#7-2` - |attribute| is [=read only=] and does not have a [{{LegacyLenientSetter}}], [{{PutForwards}}] or [{{Replaceable}}] [=extended attribute=]
- `#7-12` - |attribute| was not specified with the [{{LegacyLenientThis}}] [=extended attribute=]
    - assume true
- `#7-14` - |attribute| is declared with the [{{Replaceable}}] extended attribute
    - assume false
- `#7-16` - |attribute| is declared with a [{{LegacyLenientSetter}}] extended attribute
    - assume false
- `#7-17` - |attribute| is declared with a [{{PutForwards}}] extended attribute
    - assume false
- `#10-12` - |operation| is declared with a [{{Default}}] [=extended attribute=]
    - assume false

### II-B. Steps
- `#3-7` - the [=constructor steps=] of |constructor|
- `#6-12` - the [=getter steps=] of |attribute|

### II-C. 상속 구조

- `#2-2` - |interface| is declared to inherit from another interface 
    - assume false
- `#2-5` - |interface| is in the set of [=inherited interfaces=] of an interface that is declared with the [{{Global}}] [=extended attribute=]
    - assume false
- `#3-8` - |I| inherits from some other interface |P|
    - assume false

### II-D. 인터페이스 capability 술어
**공통 원인**: interface가 특정 WebIDL 메커니즘(named/indexed properties)을 지원하는지.
**대응**: interface record에 지원 여부를 나타내는 필드 (혹은 해당 mechanism이 정의됐는지 파생 계산).

- `#1-6` - |interfaces| contains an [=interface=] which [=support indexed properties|supports indexed properties=], [=support named properties|named properties=], or both"|interfaces| contains an [=interface=] which supports [=indexed properties=]
    - assume false
- `#2-1` - |interface| [=support named properties|supports named properties=]
    - assume false
- `#14-1` - |definition| has an [=indexed property getter=]
    - assume false
- `#14-2` - |definition| has a [=pair iterator=]
    - assume false
- `#15-1` - |definition| does not have an an [=asynchronously iterable declaration=] (of either sort)
    - assume true

### II-G. IDL 타입 태그 조회

- `#5-9` - |attr|'s type is an [=observable array type=] with type argument |T|
    - assume false
- `#6-10` - |attribute|'s type is an [=observable array type=]
    - assume false
- `#6-13` - |attribute|'s type is a [=promise type=]
    - assume false
- `#7-22` - |attribute|'s type is an [=observable array type=] with type argument |T|
    - assume false
- `#7-23` - |attribute|'s type is an [=enumeration=]
    - assume false
- `#10-14` - "|op| has a [=return type=] that is a [=promise type=]"

### II-H. record-kind 술어

- `#6-3` - |target| is an [=interface=], and |attribute| is a [=regular attribute=]
- `#6-6`, `#7-10`, `#10-7` - |jsValue| [=is a platform object=]
- `#6-7` - |jsValue| does not [=implement=] |target|
- `#7-1` - |target| is a [=namespace=]
- `#7-7` - |attribute| is a [=regular attribute=]
- `#7-11` - |jsValue| [=implements=] |target|
- `#10-4` - |target| is an [=interface=], and |op| is not a [=static operation=]
- `#10-8` - |jsValue| does not [=implement=] the interface |target|

### II-J. 기타 identity/realm 필드
**공통 원인**: 위 카테고리들에 딱 들어맞지 않는, 각각 한 번씩만 등장하는 필드/식별 술어.

- `#2-3` - |interface| is the {{DOMException}} [=interface=]
    - assume false
- `#2-4` - |realm|'s [=is global prototype chain mutable=] is true
    - assume false
    - "All realms have an is global prototype chain mutable boolean, which can be set when the realm is created. ... **By default it is set to false.**" (`webidl/index.bs:10226-10229`)

### II-K. operation의 모델링 관련
- `#10-14`, `#10-19` - If |op| has a [=return type=] that is a [=promise type=]
- `#11-7` - the [=list=] of arguments |X| is declared to take.
- `#11-10` - [=list/Append=] the type of |argument| to |types|.
           - [=list/Append=] "variadic" to |optionalityValues| if |argument| is a final, variadic argument, "optional" if |argument| is [=optional argument|optional=], and "required" otherwise.
- `#11-12` - "If |X| is declared to be [=variadic=], then: ...
    - assume false
- `#11-15` - |arguments|[|i|] is not [=optional argument|optional=]

---

## III. 알고리즘-호출 관용구(idiom) 정규화

파서 문법도, 레코드 필드도 아니라 "이 자연어 문장은 사실 다른 알고리즘 호출을 완곡하게 표현한
것"이라는 관용구 인식 문제. 공통 대응: spec patch로 원문을 명시적 `call clo<...>(...)` 형태로
재작성.

### III-A. 객체를 만들거나 해당 정의를 확인하는 하위 알고리즘 참조: spec patch 로 직접 연결 (hardcode)
- `#1-1` - Let |prototype| be the [=interface prototype object=] for |interface| in |realm|.
           => Let |prototype| be the [=create an interface prototype object=] of |interface| in |realm|.
- `#1-3` - the [=interface object=] of |ancestor interface| in |realm|
- `#2-9` - the [=interface object=] of |interface| in |realm|
- `#5-2` - |attr| is not [=exposed=] in |realm|
          => assume false
- `#9-2`, `#20-2` - |op| is not [=exposed=] in |realm|
          => assume false

### III-B. 알고리즘/클로저 호출
- `#3-7` - Perform the constructor steps of |constructor| with |object| as this and |values| as the argument values
- `#5-3` - the result of creating an [=attribute getter=] given |attr|, |definition|, and |realm|
- `#5-4` - the result of creating an [=attribute setter=] given |attr|, |definition|, and |realm|
- `#6-12` - the result of running the [=getter steps=] of |attribute| with |idlObject| as [=this=]
- `#7-24` - Perform the [=setter steps=] of |attribute|, with |idlObject| as [=this=] and |idlValue| as [=the given value=].
- `#10-13` - the result of running the [=method steps=] of |operation|, with |idlObject| as [=this=] and |values| as the argument values
- `#3-6`, `#10-11` - the result of passing |S| and |args| to the [=overload resolution algorithm=]

### III-C. Passing the given value

- #7-24 - Perform the [=setter steps=] of |attribute|, with |idlObject| as [=this=] and |idlValue| as [=the given value=].
    - the given value modeling

---

## IV. 컴파일 패스 버그

### IV-A. Range expression 관련
- #11-18 - [=list/For each=] |j| in [=the range=] 0 to |i| − 1, inclusive: ...

### IV-B. 알고리즘 호출이 case로 인식되는 경우
- `#5-2`, `#9-2`, `#20-2` - [=exposed=] in |realm|
          => (case "EXPOSED" realm)

### IV-C. `[=dfn-link=] |var|` 설명 주석이 case 태그/알고리즘 호출로 오인됨
- `#3-5` - [=identifier=] |id| on [=interface=] |I| and with argument count |n|
          => (case "IDENTIFIER" id (case "INTERFACE" I n))
- `#3-12` - [=interface=] |I| in |realm|
           => (case "INTERFACE" I realm)
- `#3-14`, `#3-15`, `#3-16` - [=interface=] |I| on |F| given |realm|
                            => (case "INTERFACE" I F realm)
- `#5-1` - [=attribute=] |attr| of |attributes|
          => (case "ATTRIBUTE" attr attributes)
- `#9-1` - [=operation=] |op| of |operations|
          => (case "OPERATION" op operations)
- `#10-10` - [=identifier=] |id| on |target| and with argument count |n|
            => (case "IDENTIFIER" id target n)
- `#10-16` - [=identifier=] |id| on |target| and with argument count |0|
            => (case "IDENTIFIER" id target 0)
- `#11-15` - [=optional argument|optional=] (i.e., it is not marked as "optional" and is not a final, variadic argument)
            => call _call1 = clo<"optional_argument">((yet "i.e."), (yet "it is not marked as \"optional\" and is not a final"), (yet "variadic argument"))
- `#20-1` - [=constant=] |const| that is a [=member=] of |definition|
            => (case "CONSTANT" const (case "MEMBER" definition))

### IV-D. extended attribute 존재 여부 predicate가 bare equality로 오인됨
`X is [=unforgeable=]`처럼 "X가 [ExtendedAttribute] 확장 속성을 갖는지" 묻는 predicate가
`IsTypePos`/`IsTypeNeg`(`[=is a/an NOUN=]` 꼴)에 안 걸리고 일반 "X is Y" fallback으로 빠져서,
`unforgeable`이 bare `SpecTerm`으로 파싱되고 `(= attr ~unforgeable~)`라는, 레코드 값과 enum
태그를 직접 비교하는 (항상 false인) 조건으로 컴파일됨 — yet이 아니라서 눈에 안 띄는 조용한
오컴파일.
- `#5-5` - Let |configurable| be <emu-val>false</emu-val> if |attr| is [=unforgeable=] and <emu-val>true</emu-val> otherwise.
           => (&& (exists attr.extendedAttributes.unforgeable) (= attr.extendedAttributes.unforgeable true))

---

## V. List 연산 semantics gap

설명: `list/remove`로 "unforgeable을 제외한 attribute 목록"을 만들 때, 원본
`definition`의 리스트를 먼저 **복사**한 뒤 걸러내야 하는지가 원문에 암묵적으로만 들어있고 지금
표현엔 그 copy가 빠져 있는 것으로 추정

- `#4-2` - [=list/Remove=] from |attributes| all the [=attributes=] that are [=unforgeable=]

---

## VI. 에디토리얼 — 그냥 drop 가능

- `#6-5`, `#7-9`, `#10-6` - "(This will subsequently cause a TypeError in a few steps, ...) <!-- https://www.w3.org/Bugs/... -->"

---

## VII. 줄글 설명

### VII-A. `argumentList` 관련

- `#3-3` - the passed arguments
- `#7-4` — any arguments were passed
- `#7-5` — "the value of the first argument passed"

### VII-B. overloading 관련
`compute the effective overload set`은 어떤 kind의 IDL construct(regular
operation/static operation/constructor/legacy factory function)를 대상으로 호출됐는지에 따라
행동이 조금씩 달라지는데, 호출부는 이걸 "for [=regular operations=] (if |op| is a regular
operation) or for [=static operations=] (if |op| is a static operation)"처럼 각 대안마다 괄호
조건을 단 완곡한 표현으로 서술합니다.
또한 `compute the effective overload set`의 kind에 따라 다른 행동을 보이는 부분이 줄글 형태의 switch 문으로 작성되어 있습니다.
또한 overload set/item 관련된 동작이 줄글로 작성되어 있고, 모델링이 필요합니다.

- `#3-10`, `#10-17` - the shortest argument list of the entries in |S|
- `#11-1`: compute the effective overload set의 인자 설명
- `#11-3`: compute the effective overload set의 kind에 따라 다른 값 할당
- `#12-1` - the length of the longest type list of the entries in |S|
- `#12-6` - there is more than one entry in |S|
          - the [=distinguishing argument index=] for the entries of |S|
- `#12-7` - Let |type| be the type at index |i| in the type list of any entry in |S|.
          - Let |optionality| be the value at index |i| in the list of [=optionality values=] of any entry in |S|.
- `#12-8` - |optionality| is “optional”
- `#12-9` - If the argument at index |i| is declared with a [=optional argument/default value=], then append to |values| that default value.
          - Otherwise, append to |values| the special value “missing”.
          - Otherwise, append to |values| the result of [=converted to an IDL value|converting=] |V| to IDL type |type|.
- `#12-12` - Let |callable| be the [=operation=] or [=extended attribute=] of the single entry in |S|. 

### VII-C 그 외
- `#3-1` - if they exist
    - assume false
- `#6-4`, `#7-8`, `#10-5` - it is not <emu-val>null</emu-val> or <emu-val>undefined</emu-val>
- `#6-14` - end these steps and allow the exception to propagate
- `#11-4` - Let |maxarg| be the maximum number of arguments the operations, legacy factory functions, or callback functions in |F| are declared to take. For [=variadic=] operations and legacy factory functions, the argument on which the ellipsis appears counts as a single argument.
