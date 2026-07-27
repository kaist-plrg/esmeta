# WebIDL yet 카테고리 정리

`webidl_yet_summary`에 정리된 #1~#7 알고리즘 (`internally_create_a_new_object_implementing_the_interface`,
`create_an_interface_prototype_object`, `create_an_interface_object`, `define_regular_attribute` x2,
`attribute getter`, `attribute setter`) 안의 `(yet ...)` 항목들을 근본 원인(root cause) 기준으로
재분류한 것입니다. 각 항목은 원본 문서의 `#알고리즘-스텝` 표기를 그대로 사용해 역참조합니다
(예: `#1-6` = 원본 "## #1" 섹션의 6번 스텝).

> **2026-07-23 검증 갱신**: 실제 `webidl/index.bs`와 `src/test/resources/golden/wji/ir.actual`을
> 대조해서 확인한 결과를 반영했습니다. 새로 발견된 항목과 정정 사항은 각 섹션에 "⚠️ 검증"으로
> 표시했고, 맨 뒤에 **VII. `steps` closure 승격 실패 (구조적 버그)** 섹션을 새로 추가했습니다 —
> 이번 검증에서 가장 중요한 발견입니다. `webidl_yet_summary`도 이 검증 내용을 반영해 다시
> 작성했습니다.

큰 갈래는 네 가지입니다.

- **I. 파서(Parser) 문법 gap** — WebIDL prose의 특정 문장 구조 자체를 아직 IR로 못 읽음.
  구문(syntax) 문제이지 레코드 모양과는 무관.
- **II. Record/IR 모델 gap** — interface/attribute/member/realm 등 레코드에 아직 없는 *필드*가
  필요함. 지금 구상 중인 "interface/attribute를 IR record로 표현" 설계와 직접 관련된 부분.
- **III. 알고리즘-호출 관용구(idiom) 정규화** — 파서 문제도 레코드 문제도 아니고, WebIDL 특유의
  반복되는 완곡한 표현("the X object for Y", "an X given Y", "Perform the X steps of Y with Z as
  this")을 명시적 `call`로 바꿔주는 spec patch가 필요함.
- **IV/V/VI. 기타** — argument-list 모델링, list 연산 의미론, 에디토리얼 노트.

---

## I. 파서 문법 gap

### I-A. 반복 구문 (for/foreach)
**공통 원인**: `for X in Y`, `foreach element X in Y` 형태의 자연어 반복문이 파서에 없음.
**대응**: 파서에 for-each 루프 파싱 규칙 추가 (list/iterate, list/for_each 호출로 연결).

- `#1-2` — "For every interface |ancestor interface| in |interfaces|: ..." 파싱 안 됨 (parser
  negative 예시)
- `#1-4` — "For each element |key| of |keys|: ..."
- `#5-1` — "For each attribute |attr| of |attributes|: ..." — 위와 동일 패턴

⚠️ **검증 결과**: 이건 단순히 "아직 안 읽힘"이 아니라 실행 시 실제로 잘못 동작합니다.
`ir.actual`에서 이 세 곳 모두 루프 바디가 붙지 않은 `call _ = clo<"list/iterate">(...)` /
`clo<"list/for_each">(...)` 하나로만 컴파일되어 있고, 그 뒤에 이어지는 문장들(unforgeable
프로퍼티 복사, attribute별 getter/setter 정의 등)은 원래 그 루프의 *바디*였는데 지금은
바깥으로 풀려나와 있습니다. 즉 "조상 interface마다 × 그 unforgeable key마다"(#1)나 "attribute
목록의 각 attribute마다"(#5) 반복돼야 할 로직이 지금은 사실상 한 번만(그것도 어떤 원소에
바인딩되는지 불분명한 채로) 실행됩니다.

### I-B. 내부 슬롯 / bracket 표기 접근 (`\[[Foo]]`)
**공통 원인**: `|x|.\[[Slot]]` 형태의 internal-slot 표기(읽기/쓰기 모두)를 필드 접근으로 파싱 못 함.
**대응**: `\[[...]]` 표기를 record field access/assignment로 인식하도록 파서 확장.

- `#1-5` — `|unforgeables|.\[GetOwnProperty]` (읽기)
- `#1-7` — `Set |instance|.\[[…]] as defined in [[…]]` (쓰기)

### I-C. record/struct 리터럴 (`PropertyDescriptor{...}`)
**공통 원인**: `{ [[Key]]: Value, ... }` 형태의 record 리터럴을 파서가 못 읽음.
**대응**: `{}` record 리터럴 문법 지원 — call 인자 안에 인라인으로 등장하는 경우(`#3-13`)도 포함.

- `#2-10` — `PropertyDescriptor{[[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true, [[Value]]: |constructor|}`
- `#5-7` — `PropertyDescriptor{[[Get]]: |getter|, [[Set]]: |setter|, [[Enumerable]]: true, [[Configurable]]: |configurable|}`
- `#3-13` — `DefinePropertyOrThrow` 호출 인자 안의 record 리터럴 (I-I과 중복 발생)

### I-D. `[=dfn-link=] |var|` 설명 주석이 case 태그로 오인됨
**원래 서술 (부정확)**: "`clo<...>((case "IDENTIFIER" id (case "INTERFACE" I n)))` 같은 태그된
튜플 인자를 파서가 못 읽음 → 파서가 case-tagged tuple 문법을 지원하도록 확장해야 함."

⚠️ **검증 결과 (정정)**: 실제 `ir.actual`을 보면 이 호출들은 전부 `(yet ...)` 없이 이미
컴파일되어 있어서, "파서가 못 읽는다"는 진단 자체가 사실과 다릅니다. 진짜 원인은 반대입니다 —
"with [=identifier=] |id| on [=interface=] |I| and with argument count |n|"처럼, WebIDL
명세는 인자 하나하나를 소개할 때 `[=dfn-link=] |var|`(설명적 dfn-link 뒤에 바로 그 값을 가리키는
변수가 오는) 형태를 아주 흔하게 씁니다. 이건 그냥 "|id|가 무엇을 의미하는지"를 설명하는
순수 문서화 주석일 뿐인데, 지금 파서는 이 `[=dfn-link=] |var|` 패턴 자체를 마치 `(case "TAG"
var)`라는 실제 태그 생성자 문법인 것처럼 오인해서, 평범한 flat 인자였어야 할 `id`를
불필요하게 `(case "IDENTIFIER" id ...)`로 감싸버립니다. 즉 case 문법을 "더 잘 지원"해야 하는
게 아니라, 애초에 `[=dfn=] |var|`가 인자 설명으로 등장하는 이 흔한 관용구를 case-태그
생성자로 잘못 승격시키지 않도록(= 그 dfn-link는 무시하고 `var`를 있는 그대로 평범한 flat
인자로 추출하도록) 막는 게 맞는 방향입니다.
**대응**: 파서 문법 확장이 아니라 오인식 자체를 제거 — `[=dfn-link=] |var|` 패턴은 case 태그가
아니라 인자 설명 주석으로 처리하고, `var`를 그대로 flat 인자로 뽑아내도록 고칩니다. 이 관용구가
WebIDL 명세 전반에 매우 흔해서, 이 하나만 고쳐도 `#3` 안에서만 최소 4곳
(`#3-5`, `#3-12`, `#3-14`, `#3-15`, `#3-16`)이 동시에 해결될 것으로 보입니다.

- `#3-5`, `#3-12`, `#3-14`, `#3-15`, `#3-16` — 전부 동일 패턴 (`compute_the_effective_overload_set`,
  `create_an_interface_prototype_object`, `define_the_static_operations` 등 호출부)
- `#1-2`(`list/iterate`), `#5-1`(`list/for_each`)도 같은 case-튜플 오인식 현상이지만, 이쪽은
  I-A(반복 구문) 문제와 겹쳐 있어서 I-A 쪽에 분류했습니다 — 루프의 "매 원소마다" 의미가
  통째로 날아가고 단일 호출 하나로 뭉개진다는 점이 다릅니다.

⚠️ **새로 발견 — `[=exposed=] ... |realm|` 패턴도 동일 오인식을 일으킴**: 지금까지의 예시는 전부
`[=dfn-link=] |var|`처럼 dfn-link 바로 뒤에 var가 붙는 모양이었는데, "|attr| is not [=exposed=]
in |realm|"처럼 dfn-link와 var 사이에 "in" 같은 전치사가 끼어 있는 경우도 똑같이
`(case "EXPOSED" realm)`으로 오인식됩니다 — 즉 트리거 조건이 "바로 인접"보다 조금 더 넓습니다.

- `#5-2` — "If |attr| is not [=exposed=] in |realm|, then [=iteration/continue=]." → 컴파일
  결과가 `if (! (= attr (case "EXPOSED" realm))) { ... }`인데, 조건 부분이 이 오인식 때문에
  `attr`과 `realm`을 비교하는 이상한 식이 되어 있습니다(원래는 "attr가 realm에서 exposed인지"를
  물어야 함). continue 자체의 gap(**I-F**)과는 별개의 문제입니다.
- `#2` (`create_an_interface_prototype_object`, Unscopable 분기) — "For each exposed member
  |member| of |interface| that is declared with the [{{Unscopable}}] extended attribute:" →
  `clo<"list/for_each">((case "EXPOSED" (case "MEMBER" member interface ~extended attribute~)))`
  로, I-A(루프 flattening)와 이 오인식이 겹쳐 이중으로 망가진 경우입니다.
- `#1`(`internally_create_a_new_object_implementing_the_interface`의 "Assert: |interface| is
  [=exposed=] in |realm|.") → `assert (= interface (case "EXPOSED" realm))`도 같은 패턴이지만,
  assert 계열이라 이 문서에서 개별 항목으로 추적하지는 않습니다.

### I-E. `let`의 tuple destructuring
**공통 원인**: `let (constructor, values) = ...` 처럼 다중값을 한 번에 destructure하는 `let`을
파서가 `unsupported Let lhs: Unknown(<...>)`로 떨어뜨림.
**대응**: 파서가 `let`의 lhs로 튜플 패턴을 인식하게 함.

- `#3-6` — `let _ = (yet "unsupported Let lhs: Unknown(<|constructor|, |values|>)")`

### I-F. 제어 흐름 키워드 (`continue` / try-block / abrupt completion 전파 / `throw`)
**공통 원인**: `continue`, "Try running the following steps: ... And then, if an exception E was
thrown: ...", `[=JavaScript/throw=]` 같은 제어 흐름 구조 자체가 IR/파서에 없음.
**대응**: `continue` 문 파싱 + 이를 제거/치환하는 lowering pass. try/exception 쪽은 completion
record 기반 제어 흐름(`Cond.Throws`류)으로 별도 처리 필요.

- `#5-3` — `(yet "continue")`
- `#6-2` — `"Try running the following steps:" ... "And then, if an exception |E| was thrown:"`
- `#6-14` — `"end these steps and allow the exception to propagate"` (위 try-block과 짝)

⚠️ **`[=JavaScript/throw=]`도 여기 속합니다 (I-I에서 이동)**: `#6-9` / `#7-14` / `#7-18`의
"[=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."는 abstract-op 호출이 아니라
제어 흐름 statement이므로, 일반 함수 호출(`I-I`)로 처리하는 대신 전용 `Throw` instruction으로
파싱하고 싶다는 방향입니다. 마침 이미 있는 인프라와 정확히 맞아떨어집니다 —
`esmeta.wji.lang.Instr.Throw(target, body)`가 이미 존재하고, `CompletionWrapping.expandInstr`
+ `WrapCompletionReturnsPass`/`ExpandThrowsPass`가 이걸 `__NEW_ERROR_OBJ__` +
`ThrowCompletion` + `Return`으로 내려주는 lowering도 이미 있습니다(I-G/VI와 같은 설계
철학: 파서는 surface syntax를 그대로 인식하고, 실제 완료-레코드 변환은 분리된 lowering
pass가 담당). 다만 `ir.actual`에는 `Instr.Throw` 대신 `call _ = clo<"javascript/throw">()`라는
정체불명의 closure 호출로 나와 있고, 현재 `src/main/scala` 전체에서 `"javascript/throw"`라는
문자열이 전혀 검색되지 않습니다 — 즉 `[=JavaScript/throw=] a {{TypeError}}`라는 이 특정
표현(WebIDL 쪽 문구, ECMA-262 자체 알고리즘의 "Throw a TypeError exception." 같은 문구와는
살짝 다름)이 지금 `Instr.Throw`로 인식되는 패턴에 안 걸리고 있는 것으로 보입니다. 정확한
원인(파서 패턴 누락인지, `ir.actual`이 최신 소스보다 오래된 골든 파일인지)은 코드를 더 봐야
확실해지지만, 방향 자체는 "`[=JavaScript/throw=] a X.`를 `Instr.Throw`로 인식하게 만들면
나머지 lowering은 이미 있는 걸 그대로 쓴다"로 정리하면 될 것 같습니다.

- `#6-9` — "Otherwise, [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
- `#7-14` — "then [=JavaScript/throw=] a <l spec=ecmascript>{{TypeError}}</l>."
- `#7-18` — "If |Q| [=is not an Object=], then [=JavaScript/throw=] a <l
  spec=ecmascript>{{TypeError}}</l>."

⚠️ **중첩된 I-I 문제 — throw 대상 자체의 마크업**: `[=JavaScript/throw=]`를 `Instr.Throw`로
잘 인식하게 만들어도, 그 인자인 `a <l spec=ecmascript>{{TypeError}}</l>`가 남습니다. `<l
spec=ecmascript>...</l>`는 지금까지 이 문서에서 다룬 `[=dfn-link=]`(WebIDL 내부 정의 참조),
`{{Foo}}`(interface/인터페이스 멤버 참조), `<a abstract-op>Name</a>`(ECMA-262 abstract
operation 호출)과는 또 다른 네 번째 마크업 형태입니다 — "이 용어는 ECMAScript 명세
쪽으로 링크된다"는 cross-spec 참조 표기로, 여기서는 `TypeError`가 WebIDL이 아니라
ECMA-262가 정의한 내장 에러 생성자라는 뜻입니다. `<a abstract-op>` 인식(**I-I**)이 별도로
필요했던 것과 똑같이, `<l spec=X>...</l>` 형태도 별도로 인식해서 "ECMA-262의 `%TypeError%`
같은 intrinsic을 참조/생성하라"는 뜻으로 풀어줘야 합니다 — 이 문서에서 `<l spec=ecmascript>`가
등장하는 자리는 전부 이 세 `throw` 인스턴스뿐이라, `Instr.Throw` 인식과 `<l spec=X>` 인식을
같이 처리해야 이 세 항목이 완전히 풀립니다.

### I-G. 조건부 `let` 값 패턴 (`X if COND, otherwise Y`)
**공통 원인**: `let V = A if COND, or B otherwise` 형태의 자연어 삼항 표현을 못 읽음.
**대응**: 파싱 단계에서는 최대한 surface syntax를 그대로 따라가는 게 원칙이므로, 이 패턴을 곧바로
`if COND then A else B`로 재구성하지 않고 `Let`을 확장한 형태(조건부 값을 그대로 담는 `Let`
노드)로 우선 파싱합니다. `if COND then A else B`로의 desugaring은 그 뒤에 별도 pass에서
수행합니다 — 파서는 원문 구조를 보존하고, 구조 변환은 파싱과 분리된 lowering 단계의 책임으로
둡니다.

- `#5-6` — "false if |attr| is unforgeable and true otherwise"
- `#6-4` / `#7-8` — "the `this` value, if it is not null or undefined, or |realm|'s global object otherwise"

### I-H. steps-block 도입 패턴
**공통 원인**: `let steps = the following (series of) steps: ...` 로 이어지는 numbered list를
closure/thunk 리터럴로 못 읽음. `#3-1`은 여기에 "X's steps if they exist, or 다음 following steps
otherwise"라는 조건까지 추가로 얹혀 있음 (I-G와 결합된 변형).
**대응**: "the following steps:" 뒤 numbered list를 closure 리터럴로 파싱.

- `#3-1` — "|I|'s overridden constructor steps if they exist, or the following steps otherwise:"
- `#6-1` / `#7-3` — "the following series of steps:"

### I-I. ECMA-262 abstract-op 호출 마크업 인식
**공통 원인**: `<a abstract-op>Name</a>(...)` 마크업으로 표기된 ECMA-262 abstract operation
호출(`CreateBuiltinFunction`, `DefinePropertyOrThrow`, `CreateDataPropertyOrThrow`, `Set`,
`Get`)이 함수 호출로 파싱되지 않음. III(알고리즘-호출 idiom)과 목적은 비슷하지만, 여기는
*ECMA-262가 정의한, 이미 이름이 정해진* abstract-op 마크업 인식 문제라 원인이 다름(III은
WebIDL 저자가 쓴 완곡한 자연어 표현 문제). `[=JavaScript/throw=]`는 예전엔 여기 같이 묶여
있었지만, 이건 abstract-op 호출이 아니라 제어 흐름 statement라서 **I-F**로 옮겼습니다.
**대응**: `<a abstract-op>` 태그를 함수 호출로 인식.

- `#3-11` — `CreateBuiltinFunction(steps, length, id, «[[Unforgeables]]», realm, constructorProto)`
- `#3-13` — `DefinePropertyOrThrow(F, "prototype", {...})` (I-C 리터럴과 겹침)
- `#5-9` — `DefinePropertyOrThrow(target, id, desc)`
- `#6-16` / `#7-26` — `CreateBuiltinFunction(steps, 0/1, name, «», realm[, constructorProto])`
- `#7-15` — `CreateDataPropertyOrThrow(jsValue, id, V)`
- `#7-20` — `Set(Q, forwardId, V, false)`
- ⚠️ **새로 발견** `#7-17` 내부 — `Get(jsValue, id)` (`PutForwards` 분기 안). 원본 정리에
  빠져 있었지만 도달 가능한 경로에 있습니다.

⚠️ **검증 노트**: `let`으로 값을 받는 자리(`let F = CreateBuiltinFunction(...)`)는 깔끔한
`(yet "...")` 노드가 되는데, 결과를 버리는 `Perform` 형태(`DefinePropertyOrThrow`,
`CreateDataPropertyOrThrow`, `Set`)는 `ir.actual`에서 `call _ =
clo<"<a_abstract-op>DefinePropertyOrThrow</a>(|F|,...)">()`처럼 **문장 전체가 closure
이름이 되고 인자는 없는 깨진 호출**로 나타납니다 — 겉보기엔 정상적인 `call` 문이라 grep으로
`(yet ...)` 를 찾아도 안 걸립니다. 반대로 `javascript/throw`는 이미 어느 쪽에서든
`call _ = clo<"javascript/throw">()`로 깔끔하게 컴파일됩니다(단, 이 closure가 실제로
TypeError를 던지도록 구현되어 있는지는 별도 확인 필요).

### I-J. 문자열 연결(concat) 표현식
**공통 원인**: `"get " + attribute.identifier` 같은 문자열 접합이 IR 표현식으로 없음.
**대응**: IR에 string concat expression 추가.

- `#6-15` — `let name = "get " prepended to |attribute|.identifier`
- `#7-25` — `let name = "set " prepended to |id|`

### I-K.
**공통 원인**: `as defined in [[#platform-object-setprototypeof]]` 로 이어지는 문장을 파싱하지 못함.
**대응**: closures로 인식.

### I-M. `Compute the effective overload set` 호출부의 kind별 조건 분기
**공통 원인**: `compute_the_effective_overload_set`은 어떤 kind의 IDL construct(regular
operation/static operation/constructor/legacy factory function)를 대상으로 호출됐는지에 따라
행동이 조금씩 달라지는데, 호출부는 이걸 "for [=regular operations=] (if |op| is a regular
operation) or for [=static operations=] (if |op| is a static operation)"처럼 각 대안마다 괄호
조건을 단 완곡한 표현으로 서술합니다.
**대응**: WJI 상황에서는 constructor와 regular operation인 경우로 제한할 수 있습니다. 이때 다른
행동을 보이는 부분에 대해서는 두 개의 타입에 대한 알고리즘을 하드코딩하여 구현하고, 내부
알고리즘만 공유하는 방식으로 구현할 수 있습니다. 호출 부분에서는 하드코딩된 알고리즘을
호출하도록 변경해야 합니다.

- `#10-10` — "Compute the effective overload set for regular operations (if op is a regular
  operation) or for static operations (if op is a static operation) ..." → `clo<"regular_
  operations">(...)`라는 잘못된 호출로 컴파일됨
- `#10-17` — 동일 패턴의 두 번째 발생 (argument count 0)

---

## II. Record/IR 모델 gap (interface/attribute/member record 설계와 직결)

이 그룹이 사용자가 원래 예로 든 `(= interface (yet "declared with the {{Global}} extended
attribute="))` 같은 케이스들이 속하는 곳입니다. 전부 "interface/attribute/member/realm 레코드에
아직 없는 필드를 읽어야 한다"는 동일한 근본 원인을 공유합니다 — record 모양을 어떻게 설계하느냐에
따라 한 번에 여러 yet이 같이 해소됩니다.

### II-A. Extended attribute / member 존재 여부 (boolean-ish)
**공통 원인**: `interface`/`attribute`가 특정 extended attribute로 선언됐는지, 혹은 특정 member(예:
constructor operation)를 갖는지를 묻는 술어. 전부 "record가 해당 flag/멤버를 갖고 있는가"라는 같은
질문의 변형.
**대응**: interface/attribute record에 `extended_attribute: Set[...]` (혹은 개별 boolean 필드)
+ member 존재 여부를 나타내는 옵션 필드 설계.

- `#1-5` / `#2-1` / `#2-5` / `#2-7` — `[Global]` (interface)
- `#2-6` — `[Unscopable]` (member)
- `#2-8` — `[LegacyNoInterfaceObject]` (interface)
- `#3-2` — "I was not declared with a constructor operation" (member 존재)
- `#3-9` — "I was declared with a constructor operation" (위와 대칭)
- `#6-8` / `#7-12` — `[LegacyLenientThis]` (attribute)
- `#7-2` — `[LegacyLenientSetter]` / `[PutForwards]` / `[Replaceable]` 복합 조건 (attribute).
  ⚠️ **검증 결과**: 이 필드 gap과는 별개로, 문장 자체가 잘못 쪼개져 있습니다. 명세 원문(콤마로
  이어진 하나의 3-way OR + 그 결과인 return 문 하나)이 `ir.actual`에서는 콤마 지점에서 두 조각
  나서, `if` 조건에는 "does not have a LegacyLenientSetter"만 남고 `[PutForwards]`/
  `[Replaceable]` 부분과 뒤따르는 "return undefined"는 if 바디 안의 문장 하나짜리 yet으로
  잘못 들어갔습니다. 필드를 다 추가해도 이 조건/바디 경계를 spec patch로 다시 맞추지 않으면
  여전히 틀립니다.
- `#7-13` — `[Replaceable]` (attribute)
- `#7-16` — `[LegacyLenientSetter]` (attribute, 단독)
- `#7-17` — `[PutForwards]` 존재 여부 (attribute)

### II-B. 값을 실어나르는 extended attribute
**공통 원인**: II-A와 달리 "존재하는가"가 아니라 "그 extended attribute가 갖고 있는 파라미터 값이
무엇인가"를 물음 (`[PutForwards=identifier]`의 `identifier` 인자).
**대응**: extended attribute record 자체가 payload를 가질 수 있어야 함 (단순 flag set이 아니라
`Map[name, args]` 형태 필요) — II-A 설계 시 같이 고려해야 함.

- `#7-19` — "the identifier argument of the `[PutForwards]` extended attribute"

### II-C. 상속 구조
**공통 원인**: interface 상속 체인 관련 술어.
**대응**: interface record에 `inherits: Option[Interface]` (또는 ancestor 목록) 필드.

- `#2-2` / `#3-8` — "declared to inherit from another interface" / "I inherits from some other interface P" (동일 술어의 두 표현)
- `#2-5`(뒷부분) — "in the set of inherited interfaces of an interface declared with `[Global]`"

### II-D. 인터페이스 capability 술어
**공통 원인**: interface가 특정 WebIDL 메커니즘(named/indexed properties)을 지원하는지.
**대응**: interface record에 지원 여부를 나타내는 필드 (혹은 해당 mechanism이 정의됐는지 파생 계산).

- `#1-6` — "|interfaces| contains an interface which supports indexed properties"
- `#2-1` — "|interface| supports named properties"

⚠️ **검증 결과**: `#1-7`의 실제 명세 원문(`webidl/index.bs:13862-13864`)은 "supports indexed
properties, **named properties, or both**"까지 포함한 3-way 조건인데, 추출된 yet 텍스트는
"indexed properties"만 남기고 나머지를 빠뜨렸습니다. 지금은 이 분기 전체를 false로 가정해서
당장 문제는 없지만(WJI 범위 밖 브랜치), 나중에 이 조건을 실제로 구현할 때는 원문을 다시 확인해서
"named properties" 쪽도 같이 넣어야 합니다.

### II-E. identifier 필드
**공통 원인**: interface/attribute의 "identifier"를 읽는 매우 단순한 필드 접근인데 아직 없음.
**대응**: `interface.id`, `attribute.id` 필드 — 사실상 파싱만 하면 되는 가장 쉬운 부류.

- `#3-4` — "the identifier of interface I" → `I.id`
- `#5-8` / `#7-6` — "|attr|'s identifier" → `attr.id`

### II-F. member-list projection
**공통 원인**: "definition의 member 중 특정 kind(regular attribute 등)만 골라낸 목록"이라는, 원본
member 리스트의 필터링된 뷰. 단순 필드가 아니라 "정의역 전체에서 어떻게 파생시키는가"를 설계해야
함 — `docs/hardcodes.md`에 이미 기록된 "copy semantics" 이슈(V 참고)와도 연결됨.

- `#4-1` — "the list of regular attributes that are members of |definition|" → `definition.members.regular_attributes`

### II-G. IDL 타입 태그 조회
**공통 원인**: `attribute`나 `operation`의 리턴 타입으로 선언된 IDL 타입이 특정 타입
(Promise, Enum, Observable array)인지 확인.
`docs/hardcodes.md` 1/2/3번 항목에 이미 기록된 "선언된 IDL 타입이 파이프라인에 흐르지 않는다"는
근본 gap과 **동일한 원인**임 — attribute.type 자체를 태그 있는 값으로 노출해야 함.
**대응**: attribute/member record에 IDL 타입을 태그 union(enum/promise/observable-array/...)으로
노출.

- `#5-10` / `#6-10` / `#7-22` — "an observable array type with type argument T"
- `#6-13` — "a promise type"
- `#7-23` — "attribute's type is an enumeration" (switch/dl-dt-dd 형태, III-D와도 겹침)

### II-H. record-kind 술어
**공통 원인**: 값의 "종류"(target이 interface냐 namespace냐, member가 regular attribute냐)를 묻는
타입 판별. II-G와 비슷하지만 대상이 attribute의 IDL 타입이 아니라 WebIDL 구조 자체의 kind.
**대응**: interface/namespace/member를 구분하는 태그 있는 sum type으로 모델링.

- `#6-3` — "target is an interface, attribute is a regular attribute"
- `#7-1` — "target is a namespace"
- `#7-7` — "attribute is a regular attribute"

### II-I. platform-object / implements / interface-type 조회
**공통 원인**: 런타임 JS 값(`jsValue`)이 어떤 WebIDL interface를 구현하는지, 그 값을 IDL interface
type 참조로 어떻게 얻는지. getter/setter 양쪽에 거의 동일하게 등장.
**대응**: platform object 표현에 "이 값이 구현하는 primary interface"를 노출하는 필드
(`jsValue.PrimaryInterface`)와, 그로부터 IDL 참조 값을 만드는 변환.

- `#6-6` / `#7-10` — "jsValue: Unknown[platform object]" (platform object 여부)
- `#6-7` — "jsValue does not implement target" → `jsValue.PrimaryInterface != target`
- `#7-11` — "true if jsValue implements target, or false otherwise" (위와 동일 술어의 불리언 버전)
- `#6-11` / `#7-21` — "the IDL interface type value that represents a reference to jsValue" → `jsValue.interface_type`

### II-J. 기타 identity/realm 필드
**공통 원인**: 위 카테고리들에 딱 들어맞지 않는, 각각 한 번씩만 등장하는 필드/식별 술어.

- `#2-3` — "the {{DOMException}} interface" (특정 interface와의 동일성 체크)
- `#2-4` — "|realm|'s is global prototype chain mutable" → realm record 필드

⚠️ **검증 결과 (정정)**: 원래 노트는 이 조건을 `true`로 가정한다고 적었지만 근거가 불확실했습니다.
`webidl/index.bs:10226-10229`에 "All realms have an is global prototype chain mutable boolean
... **By default it is set to false**"라고 명시돼 있고, 바로 뒤 노트에서 이게 `true`가 되는
유일한 이유로 `ShadowRealm`(mutable global prototype)을 듭니다. WJI는 `ShadowRealm`을 다루지
않으므로 **명세 기본값인 `false`로 가정하는 게 맞습니다.** `#2-5`(바로 다음 `else if`, `[Global]`
관련 조건)도 이미 false로 확인돼 있어서(`#1-6` 참고), 결국 두 조건 다 false로 세 번째 `else`
(`OrdinaryObjectCreate(proto)`)로 떨어지는 결과는 원래의 (근거 없는) `true` 가정과 우연히
동일하지만, "왜 그 분기로 가는지"에 대한 설명 자체는 정정이 필요합니다.

### II-K. operation의 리턴 타입 조회
#** 공통 원인**


---

## III. 알고리즘-호출 관용구(idiom) 정규화

파서 문법도, 레코드 필드도 아니라 "이 자연어 문장은 사실 다른 알고리즘 호출을 완곡하게 표현한
것"이라는 관용구 인식 문제. 공통 대응: spec patch로 원문을 명시적 `call clo<...>(...)` 형태로
재작성.

### III-A. `"the ALGO object/value for X (in realm)"` — 자기 자신을 만드는 하위 알고리즘 참조
- `#1-1` — "the interface prototype object for |interface| in |realm|" → `call clo<create_an_interface_prototype_object>`
- `#1-3` — "the value of [[Unforgeables]] slot of the interface object of |ancestor interface| in |realm|" → `call clo<create_an_interface_object>` 후 `.Unforgeables` 필드 접근 (I-B와 결합)
- `#2-9` — "the interface object of |interface| in |realm|" → `call clo<create_an_interface_object>`
- ⚠️ **새로 발견** `#1-1`의 쌍둥이 — `internally_create_a_new_object_implementing_the_interface`의
  `newTarget`이 콜러블이지만 `prototype`이 Object가 아닌 분기에서 같은 문장이 `realm` 대신
  `targetRealm`을 인자로 한 번 더 등장합니다 (`prototype = (yet "the interface prototype
  object for |interface| in |targetRealm|")`). 원본 정리에는 이 두 번째 발생이 빠져 있었습니다.

### III-B. `"an ALGO given X, Y, Z"` — named algorithm을 값처럼 참조
- `#5-4` — "an attribute getter given |attr|, |definition|, and |realm|" → `call clo<attribute getter>(attr, definition, realm)`
- `#5-5` — "an attribute setter given |attr|, |definition|, and |realm|" → `call clo<attribute setter>(...)`

### III-C. `"Perform/running the ALGO steps of X with Y as this and Z as the argument(s)"`
- `#3-7` — "Perform the constructor steps of |constructor| with |object| as this and |values| as the argument values"
- `#6-12` — "running the getter steps of |attribute| with |idlObject| as this"
- `#7-24` — "Perform the setter steps of |attribute|, with |idlObject| as this and |idlValue| as the given value" (+ "the given value"라는 용어 자체를 아직 어떻게 다뤄야 할지 미결)

### III-D. switch/dl-dt-dd 형태 dispatch → if/else 재작성
- `#7-23` — `<dl class="switch"> <dt>attribute's type is an enumeration</dt> <dd>...</dd> <dt>Otherwise</dt> <dd>...</dd> </dl>` → if/else spec patch (II-G의 enum 타입 태그 체크와 함께 처리)

---

## IV. Argument-list 관련 gap

⚠️ **검증 결과 (분리)**: 원래 이 그룹을 하나로 묶었는데, `#3-10`은 나머지 셋과 근본 원인이
다릅니다. `#3-3`/`#7-4`/`#7-5`는 전부 "closure가 실제로 호출될 때 넘어온 인자 목록
(`argumentList`)을 그 closure 본문 안에서 이름 있는 변수로 어떻게 받는가"의 문제인 반면,
`#3-10`은 호출 시점의 실제 인자와 무관하게 **`compute_the_effective_overload_set`이 만들어낸
`S`(overload set)의 각 entry가 선언적으로 몇 개의 인자를 받는지**를 계산하는 문제입니다. 그래서
IV-A/IV-B로 나눕니다.

### IV-A. Closure의 실제 호출 인자(`argumentList`) 접근
**공통 원인**: ECMA-262 스타일로 "the following steps, given arguments ..."라고 줄글로 서술된
built-in function의 behavior(abstract closure)가, 실제로 호출될 때 넘어온 인자 목록
(`argumentList`)을 자기 본문 안에서 이름 있는 변수로 참조하는 관용구("the passed arguments",
"any arguments were passed", "the value of the first argument passed" 등)가 아직 표준화된
IR 표현/필드가 없음.

- `#3-3` — "the passed arguments" (생성자 closure가 호출될 때 넘어온 인자 전체)
- `#7-4` — "any arguments were passed" → `0 < size(argumentList)`
- `#7-5` — "the value of the first argument passed" → `argumentList[0]`

### IV-B. Overload set entry의 선언적 arity 계산
**공통 원인**: `#3-10`("the shortest argument list of the entries in |S|")은 실제 호출 인자와
무관하게, `compute_the_effective_overload_set`이 만들어낸 overload set `S`의 각 entry(각
overload 시그니처)가 *선언적으로* 몇 개의 인자를 받는지를 보고 그중 최솟값을 구하는 문제입니다
— 이 값이 생성된 함수 객체의 `.length` 프로퍼티가 됩니다. 이건 ECMA-262의 개념이 아니라
**WebIDL의 정책**입니다 — ECMA-262 함수 객체엔 오버로딩이라는 게 없어서 `.length`가 숫자
하나여야 하는데, WebIDL은 같은 이름의 `operation`/`constructor`를 시그니처만 다르게 여러 번
선언(오버로딩)할 수 있게 해주기 때문에, 그 여러 선언을 대표할 숫자 하나를 "그중 필수 인자가
가장 적은 entry의 개수"로 정한 것입니다(`webidl/index.bs:11978`(constructor),
`:12028`(legacy factory function), `:12584`(regular/static operation) 세 군데 모두 동일한
패턴). `compute_the_effective_overload_set`/`overload resolution algorithm` 자체가 아직 전혀
mechanize되지 않은 훨씬 큰 별도 알고리즘이라서, 이 entry가 IR에서 어떤 record 모양이 될지(그리고
그 record가 arity 정보를 어떻게 노출할지)부터 설계해야 풀리는, IV-A보다 훨씬 무거운 문제입니다.

- `#3-10` — "the shortest argument list of the entries in |S|"

⚠️ **실용적 참고**: WJI 범위의 7개 interface는 전부 `constructor(...)`가 하나씩만 선언돼 있어서
(`spectec/document/js-api/index.bs` 확인), `#3-10`이 속한 `create_an_interface_object` 안에서는
`S`가 항상 entry 1개짜리입니다 — 그래서 일반적인 overload resolution 전체를 구현할 필요 없이,
그 interface의 유일한 constructor 선언이 갖는 파라미터 개수(optional/variadic 고려)만 계산하면
됩니다.
- **다만 이게 WJI 전체에 대해 일반적으로 참은 아닙니다.** `create an operation function`
  (`webidl/index.bs:12536-12585` — `define_the_regular_operations`/`define_the_static_operations`가
  호출하는, 우리가 지금까지 감사한 #1~#7과는 별개인 8번째 알고리즘)도 정확히 같은
  "`Let |length| be the length of the shortest argument list in the entries in |S|`" 스텝을
  갖고 있고(`:12584`), `WebAssembly` namespace의 `instantiate`는 실제로
  `instantiate(bytes, importObject, options)`와 `instantiate(moduleObject, importObject)`
  두 시그니처로 **진짜 오버로딩**되어 있습니다(`webidl/index.bs:1140-1147`). 그러니
  `instantiate`에 대해 `S`를 계산하면 entry가 2개 이상 나오고, 이 경우엔 overload resolution을
  진짜로 구현해야 합니다 — `.length` 계산이 "언제나 자명하다"고 일반화하면 안 되고,
  constructor에 한해서만 지금 WJI 범위에서 우연히 자명한 것으로 봐야 합니다.

## V. List 연산 semantics gap

**공통 원인**: `list/remove`로 "unforgeable을 제외한 attribute 목록"을 만들 때, 원본
`definition`의 리스트를 먼저 **복사**한 뒤 걸러내야 하는지가 원문에 암묵적으로만 들어있고 지금
표현엔 그 copy가 빠져 있음 — II-F(member-list projection)와 같은 자리에서 같이 설계하는 게 좋아
보임.

- `#4-2` — "Remove from |attributes| all the attributes that are unforgeable." (컴파일 결과:
  `list/remove(attributes, ~attributes~, ~unforgeable~)` — copy 여부 불명확)

⚠️ **검증 결과**: 문제가 하나 더 있습니다. `list/remove`의 2·3번째 인자가 실제 "이 attribute가
unforgeable인가"를 계산하는 predicate/람다가 아니라 `~unforgeable~`이라는 **심볼 하나**로만
남아있습니다. 즉 copy 여부뿐 아니라, 애초에 "무엇을 기준으로 제거할지"도 지금은 계산 가능한
형태로 표현되어 있지 않습니다. copy 의미론과 filter 조건(predicate) 두 가지를 같이 설계해야
합니다.

## VI. 에디토리얼 — 그냥 drop 가능

**공통 원인**: 알고리즘 동작이 아니라 스펙 저자가 남긴 비규범적 주석(HTML 버그 링크 포함).

**대응**: 새로 만들 필요 없이 이미 있는 메커니즘을 그대로 씁니다 —
`esmeta.wji.lang.Instr.Note(text, body)`가 이미 `InstrParser`에서 파싱되고,
`esmeta.wji.compiler.lowering.DropNotesPass`가 lowering pass 파이프라인 맨 앞에서 이걸 지우는
역할을 이미 하고 있습니다(I-G의 "Let 확장 파싱 → 별도 desugaring pass"와 같은 설계 철학:
파서는 surface syntax를 있는 그대로 인식하고, 의미 판단(drop해도 되는지)은 분리된 lowering
pass의 책임). 즉 진짜 gap은 "Note를 drop하는 메커니즘이 없다"가 아니라, **이 두 항목이 지금
`InstrParser`의 Note 인식 휴리스틱에 안 걸려서 `Instr.Note`로 안 잡히고 있다**는 것입니다 —
`InstrParser`가 이 특정 문장 모양(`(This will subsequently cause ...) <!-- HTML 주석 -->`)도
Note로 인식하도록 확장하면, 나머지는 이미 있는 `DropNotesPass`가 처리합니다.

- `#6-5` / `#7-9` — "(This will subsequently cause a TypeError in a few steps, ...) <!-- https://www.w3.org/Bugs/... -->"

---

## VII. `steps` closure 승격 실패 (구조적 버그) ⚠️ 신규 — 가장 중요한 검증 결과

**공통 원인**: `create_an_interface_object`(#3), `attribute getter`(#6), `attribute
setter`(#7) 세 알고리즘은 전부 `"let |steps| be the following (series of) steps: <nested
list>"` 패턴으로 시작합니다 (I-H 참고). 이 `steps`는 원래 나중에 `CreateBuiltinFunction(steps,
...)`에 넘겨질 **별도의 closure 값**이어야 하는데, 실제 `ir.actual`에서는 이 세 곳 모두 그
중첩 목록이 closure로 분리되지 않고 바깥 함수의 최상위 문장으로 그대로 풀려나옵니다. 그리고
그 중첩 목록의 마지막은 항상 `Return`으로 끝나기 때문에(`Perform 시피스... Return |O|`,
`Return the result of converting R...`, `Return undefined`), 이 `return`이 바깥 함수의
무조건 반환이 되어버려서, 원래 그 아래에 이어져야 할 "진짜" 알고리즘 나머지 절반 —
`create_an_interface_object`라면 `constructorProto`/`unforgeables`/`length` 계산과 실제
`CreateBuiltinFunction` 호출·`F` 반환, `attribute getter`/`setter`라면 예외 처리 분기와 실제
`name`/`F` 구성·반환 — 이 전부 죽은 코드가 됩니다.

즉 지금 컴파일된 세 함수는 전부 "생성자/getter/setter *함수 객체*를 만들어서 반환"하는 대신
"그 함수가 호출됐을 때 할 일을 한 번 즉시 실행하고 그 결과를 반환"하는 것처럼 동작합니다 —
`create_an_interface_object`는 인터페이스 생성자를 안 만들고 인스턴스 하나를 즉석에서
반환하고, `attribute_getter`/`attribute_setter`는 getter/setter 함수를 안 만들고 값 변환
결과 한 번을 반환합니다.

이건 이미 `docs/hardcodes.md` #7에 기록된 근본 원인(“이름 붙은 스텝 자신의 nested bullet list가
`Perform`의 `body`로 다시 읽혀 들어가지 못하고 조용히 버려짐” — `react`의 fulfilled/rejected
분기 케이스)과 **동일한 파서/추출기 결함**이 세 곳에서 독립적으로 재현된 것입니다. `react`
쪽은 소스 텍스트를 재작성하는 spec patch로 우회했지만, 여기 세 알고리즘은 아직 그런 우회도
없이 그대로 방치돼 있습니다.

**대응**: `InstrParser`/`AlgorithmExtractor`가 `"let X be the following (series of) steps:"`
뒤에 오는 중첩 목록을 항상 (바깥 함수 body에 inline하는 게 아니라) 별도 closure 값으로
hoist하도록 일반화하는 게 근본 해법입니다. 그전까지는 `react`처럼 이 세 곳도 개별 spec patch로
`steps`의 본문을 명시적 이름 있는 closure로 뽑아내는 임시 우회가 필요합니다.

- `#3` 전체(스텝 1~7이 사실상 closure 본문) — `create_an_interface_object`
- `#6` 전체(스텝 1~14가 사실상 closure 본문) — `attribute getter`
- `#7` 전체(스텝 3~23이 사실상 closure 본문) — `attribute setter`

---

## 요약: 카테고리별 등장 횟수 (중복 포함, 총 ~70개 yet + 구조적 버그 1건)

| 그룹 | 서브카테고리 수 | 인스턴스 수(대략) |
|---|---|---|
| I. 파서 문법 gap | 10 | ~26 |
| II. Record/IR 모델 gap | 10 | ~29 |
| III. 알고리즘-호출 idiom | 5 | ~10 |
| IV. Argument-list 모델링 | - | 4 |
| V. List 연산 semantics | - | 1 |
| VI. 에디토리얼 | - | 2 |
| VII. `steps` closure 승격 실패 | - | 3개 알고리즘 전체 (구조적) |

가장 인스턴스가 많은 두 카테고리는 **II-A (extended attribute/member 존재 여부, 10건)**와
**I-I (ECMA-262 abstract-op 호출 인식, 7건)** 입니다. 즉 interface/attribute record에
"extended attribute 집합 + payload"를 어떻게 얹을지 하나만 잘 설계해도 II-A와 II-B가 한 번에
풀리고, `<a abstract-op>` 마크업을 함수 호출로 인식하는 파서 규칙 하나로 I-I 전체(CreateBuiltinFunction/
DefinePropertyOrThrow/CreateDataPropertyOrThrow/Set/throw)가 풀립니다.

다만 **우선순위로 보면 VII이 가장 급합니다** — 개수로는 1건(정확히는 3개 알고리즘 공통 패턴)
뿐이지만, II-A/I-I를 전부 해결해도 `create_an_interface_object`/`attribute getter`/
`attribute setter`는 여전히 실제 함수 객체를 만들지 못하고 즉시 반환해버리는 채로 남습니다.
개별 yet을 하나씩 지워나가는 것과 별개로, VII을 먼저 고쳐야 이 세 알고리즘이 "끝까지 실행은
되지만 결과가 이상한" 상태에서 "원래 의도한 대로 함수 객체를 만들어 반환하는" 상태로 넘어갑니다.

---

## 부록: 범위 밖이지만 인접한 gap — WebAssembly Error 클래스(CompileError/LinkError/RuntimeError) 생성

이번 문서의 #1~#7은 전부 `"create a new object implementing the interface"`에서 시작해
"실제로 호출되는 WebIDL 알고리즘"만 다루는데, `CompileError`/`LinkError`/`RuntimeError`는
애초에 이 경로를 타지 않는다는 게 검증 중에 드러났습니다. 그래서 위 I~VII 카테고리 어디에도
속하지 않지만, `compile`/`instantiate`/`validate`/트랩 등 도처에서 이 세 예외를 던지고 있어서
곧 다뤄야 할 gap이라 별도로 기록해둡니다.

- **명세 위치**: `spectec/document/js-api/index.bs:1806-1823` ("Error Objects" 절, `create the
  WebAssembly namespace object` 알고리즘)
- **무엇이 다른가**: `CompileError`/`LinkError`/`RuntimeError`는 `interface X { constructor(...);
  ... }` 같은 일반 WebIDL interface 선언이 전혀 없습니다. 대신 "WebAssembly namespace object가
  만들어질 때, `«"CompileError", "LinkError", "RuntimeError"»` 각각에 대해 ECMA-262의
  *NativeError Object Structure*(= `TypeError`/`RangeError`/`SyntaxError` 등 내장 에러 클래스를
  만드는 것과 동일한 메커니즘)로 새 생성자 객체를 만들고, `DefineMethodProperty`로 namespace
  object에 매단다"는 별도 알고리즘 하나로 정의됩니다. 명세 본문도 "Note: It is not currently
  possible to define this behavior using Web IDL."(`index.bs:1823`)이라고 스스로 명시합니다.
  즉 지금까지 정리한 `create_an_interface_object`/`create_an_interface_prototype_object` 등
  interface object 생성 경로가 이 세 에러 클래스에는 아예 적용되지 않고, ECMA-262 쪽의 NativeError
  생성 알고리즘(있다면 `%NativeError%` 관련 abstract operation들)을 따로 mechanize해야 합니다.
- **현재 상태**: `esmeta/wji` 파서·컴파일러로 이 부분을 아직 시도해보지 않았습니다 — 그래서 아직
  `(yet ...)` 목록도, `docs/hardcodes.md`에 기록된 임시방편도 없습니다. `compile`/`instantiate`/
  `validate`와 여러 트랩 알고리즘이 `{{CompileError}}`/`{{LinkError}}`/`{{RuntimeError}}`
  exception을 던지는 걸 전제로 하고 있어서(`index.bs:452,507,520-542,606-607,762,765,1296,1924` 등),
  이 세 클래스 생성 자체가 안 되면 그 알고리즘들도 끝까지 실행이 안 될 가능성이 높습니다.
- **다음 단계 제안**: 이 문서의 #1~#7과 별개로, "NativeError Object Structure" 알고리즘을 대상으로
  한 번 더 파서·컴파일러를 통과시켜서 yet 목록을 뽑아보는 게 자연스러운 다음 작업으로 보입니다.

## 부록 2: 범위 밖이지만 인접한 gap — `create a namespace object`, 그리고 §VII 우선순위 재평가

**나머지 배제/가정들을 interface뿐 아니라 namespace object(`WebAssembly`)까지 고려해서 다시
검토한 결과**, `#1`~`#3`(interface object/prototype object 생성 경로)의 배제 판단 자체는 전부
그대로 유효합니다 — `[Global]`은 애초에 "appears on an interface"로만 정의돼 있어서
(`webidl/index.bs:10190`) namespace엔 문법적으로 붙을 수조차 없고, 상속·supports indexed/named
properties도 namespace엔 개념 자체가 없습니다. `#4`~`#7`(`define_the_regular_attributes`부터
`attribute setter`까지)은 원래도 "interface **or namespace**"를 대상으로 하는 알고리즘이라
`WebAssembly` namespace의 유일한 attribute인 `JSTag`(readonly, 타입 `Tag`)로 다시 검산해봐도
enum/promise/observable-array/legacy setter 계열 배제는 전부 그대로 false입니다(`JSTag`도 그 어떤
legacy setter 계열 extended attribute도 갖고 있지 않음).

**다만 이 재검토 과정에서 이 문서 전체의 우선순위를 다시 봐야 할 만한 사실을 하나 발견했습니다.**
WebIDL의 일반 `create a namespace object` 알고리즘(`webidl/index.bs:14827-14843`)은:

1. `Define the regular attributes`/`Define the regular operations`/`Define the constants`를
   그 namespace(`WebAssembly`) 자신에 대해 실행하고 (→ `JSTag`가 바로 이 경로로 `#4`~`#6`을 탑니다),
2. **`[{{LegacyNamespace}}]` extended attribute로 그 namespace를 가리키는 모든 exposed
   interface에 대해 `create_an_interface_object`를 호출**해서, 그 결과를
   `DefineMethodProperty(namespaceObject, id, interfaceObject, false)`로 namespace object에
   매답니다.

그런데 WJI의 7개 interface(`Module`/`Instance`/`Memory`/`Table`/`Global`/`Tag`/`Exception`)는
**전부** `[LegacyNamespace=WebAssembly, ...]`로 선언돼 있습니다
(`spectec/document/js-api/index.bs`). 즉 이 알고리즘이 실행되면 `WebAssembly.Module`,
`WebAssembly.Memory` 등 7개 프로퍼티 전부를 `create_an_interface_object` 결과로 채웁니다 —
**사용자 JS가 `new WebAssembly.Memory(...)`를 실제로 호출하는지와 무관하게, `WebAssembly`
namespace 자체를 구성하는 데 `create_an_interface_object`(그리고 그것이 부르는
`create_an_interface_prototype_object`)가 무조건 필요**하다는 뜻입니다. 지금까지 `#3`의
§VII 구조적 버그(생성자 steps가 closure로 승격되지 않아 `create_an_interface_object`가 실제로는
함수 객체를 만들지 못하는 문제)를 "언젠가 `new WebAssembly.X(...)`를 지원할 때"의 문제로 다뤘는데,
사실은 `WebAssembly.instantiate`만 쓰는 지금의 `demo.js` 같은 프로그램도 `WebAssembly.*`
namespace 자체가 제대로 만들어지려면 이 버그를 거쳐야 합니다.

- **현재 상태**: `ir.actual`의 `create the webassembly namespace object`(spec-patch로 재작성된
  js-api 쪽 override)를 보면 `let namespaceObject = ~namespace object~`로, namespace object
  자체를 만드는 이 일반 `create a namespace object` 알고리즘이 아직 전혀 시도되지 않고 그냥 심볼
  placeholder로 남아 있습니다 — CompileError/LinkError/RuntimeError를 거기에 얹는 override
  스텝만 컴파일돼 있고, 그 밑에 깔린 일반 알고리즘은 부록 1의 NativeError 건과 마찬가지로 아직
  손대지 않은 상태입니다.
- **제안**: `create a namespace object`도 부록 1과 함께 다음 감사 대상으로 넣어두면, `#2`/`#3`의
  실제 우선순위(§VII를 포함해서)를 "선택적 기능"이 아니라 "`WebAssembly.*`를 쓰는 모든 프로그램의
  전제조건"으로 재평가할 수 있을 것 같습니다.
