# 하드코딩된 명세 동작 (Hardcoded Spec Behavior)

이 문서는 명세에 실제로 적혀있는 동작이지만, 지금 당장 명세 텍스트로부터 자동으로
기계화(mechanize)하기엔 시간이 오래 걸려서 대신 손으로 하드코딩해 둔 부분들을
기록합니다. 나중에 시간이 될 때 진짜 자동 기계화로 교체하기 위한 목록입니다 —
`docs/spec_errors.md`(명세 자체의 결함)나 `docs/esmeta_errors.md`와는 별개로,
"명세는 맞는데 우리가 아직 자동으로 못 읽어서 대신 손으로 박아넣은 것"만 다룹니다.

## 1. WebIDL dictionary → JavaScript value 변환 (`converted_to_a_javascript_value`)

- **File**: `src/main/resources/manuals/funcs/converted_to_a_javascript_value.ir`
- **Spec source**: `webidl/index.bs`의 "convert a dictionary to a JavaScript value" (`id="dictionary-to-js"`, 8031번째 줄 근방)
- **What's hardcoded**: 실제 WebIDL 알고리즘은 새 ordinary object를 만든 뒤, 존재하는 각 dictionary 멤버마다 값을 (재귀적으로) 변환해서 `CreateDataPropertyOrThrow`를 수행합니다. 이 프로젝트는 WebIDL의 범용적인, IDL 타입별 "converted to a JavaScript value" 디스패치 테이블을 전혀 파싱/기계화하지 않습니다 — 대신 이 manual `.ir` 스텁은 "만약 `argument`가 Map이면(이 프로젝트가 dictionary 모양 값을 런타임에 표현하는 방식, 예를 들어 js-api의 `«[ "module" → |module|, "instance" → |instance| ]»` 리터럴), `CreateDataPropertyOrThrow`가 하는 것과 같은 방식으로 그 entry들로부터 실제 object를 만들고; 아니면 `argument`를 그대로 반환한다"고 특수 케이스 처리만 합니다 (다른 곳에서 이미 의존하고 있는 primitive-passthrough 케이스들, 예를 들어 `resolve`, `get a copy of the buffer source`를 커버).
- **Why not mechanized**: WebIDL의 실제 "converted to a JavaScript value"는 수십 개의 `<div algorithm>`/`<p>` 블록에 흩어진, IDL 타입별(any/undefined/boolean/number/dictionary/sequence/...) 거대한 디스패치 테이블이고, `AlgorithmExtractor`/`SpecFile.webidlFilter`가 지금 범용적으로 처리하는 단일 진입점이 없습니다. 파이프라인이 값의 (런타임 모양이 아니라) *선언된 IDL 타입*을 기준으로 디스패치하도록 만드는 건 지금 당장 시간을 들이기엔 더 큰 설계 문제라서, 이 Map-모양 휴리스틱이 그게 만들어지기 전까지의 임시방편입니다.

## 2. WebIDL JavaScript value → IDL value 변환 (`converted_to_an_idl_value`)

- **File**: `src/main/resources/manuals/funcs/converted_to_an_idl_value.ir`
- **Spec source**: `webidl/index.bs`의 "converted to an IDL value" (`id="dfn-convert-ecmascript-to-idl-value"`, 7299번째 줄 근방)
- **What's hardcoded**: 1번 항목의 반대 방향 버전인 거울상 갭입니다. 실제 알고리즘은 JS 값 `V`를 *선언된* IDL 타입에 대해 검증하고 변환합니다 — 타입이 안 맞으면 `TypeError`를 던지고, 숫자형/문자열/불리언은 각자의 IDL-타입별 규칙대로 변환하고, dictionary 멤버는 재귀적으로 처리하는 식입니다. 이 manual 스텁은 그냥 identity passthrough (`return argument`)입니다 — 검증도, 변환도, 타입에 따른 분기도 전혀 없습니다.
- **Why not mechanized**: 1번과 근본 원인이 같습니다 — WJI 파이프라인엔 값의 선언된 IDL 타입이라는 개념이 흘러다니지 않아서, 타입별 변환 규칙을 범용적으로 고를 방법이 없습니다. 그게 만들어지기 전까지는 모든 호출자가 자기 JS 인자를 그대로 돌려받고, 이미 IDL 모양일 거라고 그냥 믿는 수밖에 없습니다.

## 3. WebIDL buffer-source 인자 변환 (`ToWebIDLArrayBuffer`)

- **File**: `src/main/resources/manuals/funcs/ToWebIDLArrayBuffer.ir`
- **Spec source**: `webidl/index.bs`의 "get a copy of the bytes held by the buffer source" (`id="dfn-get-buffer-source-copy"`, 9312번째 줄 근방); `WebAssembly.instantiate`/`compile`/`validate`의 `bytes: [AllowResizable] AllowSharedBufferSource` 파라미터(`spectec/document/js-api/index.bs:371-375`)가 필요로 하는 변환입니다.
- **What's hardcoded**: 실제 알고리즘은 JS 인자를 {{ArrayBuffer}}/{{SharedArrayBuffer}}/typed-array view로 읽어서 그 바이트를 복사해 내고(필요하면 detached거나 out-of-bounds인 버퍼를 감지해서 예외를 던지고), 그 복사본을 반환합니다. (`INTRINSICS.WebAssembly.instantiate.ir`에서 실제 `instantiate` 알고리즘이 실행되기 전에 호출되는) `ToWebIDLArrayBuffer.ir`는 그냥 identity passthrough (`return argument`)입니다 — unwrap도, 바이트 복사도, detached-buffer 검사도 없습니다.
- **Why not mechanized**: 1번/2번과 근본 원인이 같습니다 — `AllowSharedBufferSource` 자신의 변환 규칙도 그 빠져있는 선언된-IDL-타입 디스패치 테이블의 한 갈래일 뿐이라서, 일단 같은 방식으로 스텁 처리해 두었습니다.

## 4. WebAssembly interface object 생성 (`ExpandNewInterfaceObjectPass`)

- **File**: `src/main/scala/esmeta/wji/compiler/lowering/ExpandNewInterfaceObjectPass.scala`
- **Spec source**: `webidl/index.bs`의 "create a new object implementing the interface" (13818번째 줄) / "internally create a new object implementing the interface" (13827번째 줄) — `Expr.New(iface)`("a new X")가 실제로 의미하는 범용 WebIDL 메커니즘입니다.
- **What's hardcoded**: 실제 범용 알고리즘(주어진 `interface`를 그 interface prototype object로 resolve하고, 그 interface의 operation/attribute들이 다 연결된 object를 만드는)을 거치는 대신, 이 pass는 `Let(x, New(iface), body)`를 곧바로 고정된 이름 `create_new_object_implementing_the_interface` 호출로 재작성하고, `iface`를 `%WebAssembly.X%` intrinsic-slot 이름에 문자 그대로 이어붙여 만든 문자열인 `@currentRealm.Intrinsics.["%WebAssembly.$iface%"]`를 receiver로 넘깁니다. pass 자체의 TODO 주석도 이걸 그대로 인정합니다: "It calls a hardcoded algorithm. It should be replaced to actual WebIDL algorithm call."
- **Why not mechanized**: 실제 알고리즘은 interface 자체의 정의(그 operation들, `[Global]`/named-properties-object 동작, 상속 체인)에 범용적으로 접근해야 하는데, WJI 파이프라인은 지금 이런 걸 전혀 추출하거나 표현하지 않습니다. intrinsic 이름을 이어붙이는 이 편법은 지금까지 등장한 모든 interface(`Module`/`Instance`/`Memory`/`Table`/`Global`/`Tag`/`Exception` 등)가 마침 다른 곳에 이미 연결되어 있는 `%WebAssembly.X%` realm intrinsic slot을 갖고 있기 때문에 우연히 작동할 뿐입니다.

## 5. "queue a task"에서 빠진 HTML task source (`ExpandQueueATaskPass`)

- **File**: `src/main/scala/esmeta/wji/compiler/lowering/ExpandQueueATaskPass.scala`
- **Spec source**: WHATWG HTML Standard의 "queue a task" 알고리즘, `[=Queue a task=]`를 통해 참조됩니다 (예: `webidl/index.bs:8913,8915,8951,8953`) — 어느 task queue/순서로 job이 실제로 실행될지를 결정하는 `taskSource`(및 event loop/document)를 인자로 받습니다.
- **What's hardcoded**: 이 pass는 모든 `"[=Queue a task=] on |taskSource|, if provided, to perform the following steps: ..."` 스텝을 ECMA-262 자체의 `HostEnqueuePromiseJob` 훅에 대한 단순 호출로 재작성하면서 `|taskSource|` 인자를 통째로 버립니다 — `HostEnqueuePromiseJob`은 HTML task source라는 개념 자체가 없어서, 큐잉되는 모든 task가 실제 명세가 지정한 source가 무엇이든 상관없이 전부 같은 하나의 job queue로 합쳐집니다.
- **Why not mechanized**: WJI는 ECMA-262의 Job Queue(`@JOB_QUEUE`)는 모델링하지만 HTML의 별도 task-source/event-loop 메커니즘은 전혀 모델링하지 않습니다 — 그걸 제대로 만드는 건 이 근사보다 훨씬 큰 작업이고, 이 근사는 (pass 자신의 doc에 적힌 대로) "job이 실제로 돌아가게 만들 정도로는 충분"하지만 "HTML event-loop task-queueing semantics를 충실히 기계화한 것"은 아닙니다.

## 6. `react`의 중첩된 fulfilled/rejected 분기를 closure로 재작성 (`SpecPatch` #12)

- **File**: `src/main/scala/esmeta/wji/lang/SpecPatch.scala` (patch #12의 `(hardcoding)` 부분)
- **Spec source**: `webidl/index.bs`의 `react` 알고리즘과 `spectec/document/js-api/index.bs`에 있는 그 2개 호출부(`asynchronously compile a WebAssembly module` / `asynchronously instantiate a WebAssembly module`) — `[=React=] to |X|:` 스텝 바로 아래에 `* If |X| was fulfilled...:` / `* If |X| was rejected...:` 형태의 bullet 하위 분기가 붙어 있는 모양입니다.
- **What's hardcoded**: 어떤 스텝의 "body"가 그 스텝 자신의 중첩 bullet 목록으로 표현되는 이 모양은 `AlgorithmExtractor`/`InstrParser`가 지금 스텝의 연속으로 인식하는 형태가 아니라서, `Perform`의 `body`로 다시 읽혀 들어가지 못하고 fulfilled/rejected 분기 전체가 조용히 버려지고 있었습니다. 이 패치는 소스 텍스트 자체를 재작성해서 이 문제를 피해갑니다 — 두 분기를 명시적인 이름 있는 closure 두 개(`Let |onFulfilledSteps| be the following steps given argument |V|: ...`)로 뽑아내서 `[=React=]`에 일반 인자로 넘기는 식인데, 이건 `ExpandFollowingStepsPass`가 이미 hoist할 줄 아는 모양입니다.
- **Why not mechanized**: `InstrParser`가 bullet 하위 목록을 (이미 이름 붙은 스텝 자신의 `body`로서만이 아니라) *바로 앞* 한 줄짜리 스텝의 연속으로도 인식하게 만드는 건 파서의 구조 자체를 바꾸는 일인데, 지금까지는 이 호출부 한 쌍에만 필요해서 그것 하나만을 위해 일반화할 만한 가치는 아직 없습니다.

## 7. `Expr.New(iface)`가 만드는 인터페이스 객체에 기본 오디너리 오브젝트 필드 하드코딩

- **File**: `src/main/scala/esmeta/wji/compiler/Compiler.scala` (`ordinaryObjectFields`, `interfacesWithPrototypeIntrinsic`, `Expr.New` 컴파일 케이스)
- **Spec source**: `webidl/index.bs`의 "internally create a new object implementing the interface" — 4번 항목(`ExpandNewInterfaceObjectPass`)이 다루는 것과 같은 근본 gap의 다른 발현입니다.
- **What's hardcoded**: `Expr.New(iface)`("a new X")는 원래 `ERecord(iface, Nil)`(인터페이스가 선언한 슬롯만 있는 맨몸 레코드)로 컴파일되고 있었는데, `TyModel.registerDynamicSubtype`으로 `Type(v)`가 이런 레코드를 이제 제대로 `"Object"`로 분류하게 되면서, mainline 코드가 `.Get`/`.Set` 등을 직접 호출하려다 그 필드가 없어서 크래시하는 게 드러났습니다. 지금은 `manuals/funcs/__NEW_OBJ__.ir`을 그대로 본떠, 표준 오디너리 오브젝트 내부 메서드 11개(전부 `Record[OrdinaryObject]`의 진짜 구현을 가리키는 클로저)와 `Extensible`(항상 `true`), `PrivateElements`, `__MAP__`을 모든 `New(iface)` 호출에 하드코딩으로 붙여줍니다. `Prototype`은 `interfacesWithPrototypeIntrinsic`에 하드코딩으로 나열된 인터페이스(지금은 `Instance` 하나뿐 — `manuals/intrinsics`에 실제로 `.prototype` intrinsic이 선언된 게 그것뿐이라서)에 대해서만 진짜 `%WebAssembly.<iface>.prototype%`로 연결되고, 나머지는 여전히 `null`입니다.
  - 이 항목과 별개로, Getter/Setter/Constructor 3종류(WebIDL이 "member"라 부르는 것 중 Method를 뺀 나머지)는 이제 진짜로 컴파일됩니다: `AddInterfaceMemberBuiltinBehaviourPass`(lowering)가 원래 선언된 파라미터를 `ArgumentsList`에서 위치 기반으로 풀어내고(`**the given value**`도 setter에 한해 `ArgumentsList[0]`로), 모든 종료 지점을 Completion으로 래핑(`CompletionWrapping.expandAlgorithm` 재사용, 이미 `returnsCompletion`으로 래핑된 경우는 중복 래핑 안 함)하고, `**this**`는 이제 그냥 평범한 지역 변수(`Name("this")`)라 별도 바인딩도 필요 없습니다. `Compiler.compileAlgo`가 그 결과를 `manuals/intrinsics`가 기대하는 kind별 정확한 이름(`INTRINSICS.get:WebAssembly.<iface>.prototype.<attr>`, `set:...`, constructor는 `INTRINSICS.WebAssembly.<iface>`)의 `<BUILTIN>:` 함수로 등록합니다 — 더 이상 손으로 쓴 `manuals/funcs/get:...` stub이 아닙니다.
  - **`Method`는 여전히 TODO로 남겨둠**: 이 3종류와 달리, WebIDL method 이름은 인터페이스 안에서 유일하지 않을 수 있습니다(`WebAssembly`가 파라미터 타입만 다른 `instantiate` 오버로드 2개를 선언 — `AlgorithmExtractor`가 이 둘을 구분 못 해서 `SpecPatch` #3이 그중 하나를 이미 rename해둔 상태). 게다가 그중 하나(`INTRINSICS.WebAssembly.instantiate`)는 이미 손으로 쓴 `manuals/funcs/INTRINSICS.WebAssembly.instantiate.ir` glue 파일이 그 intrinsic 이름을 쓰고 있어서(WebIDL의 "선언된 반환 타입이 `Promise<T>`일 때 내부 `PromiseCapabilityRecord`에서 `.Promise`를 꺼내는" 변환 — 이 파이프라인이 아직 범용으로 안 함), Method까지 여기서 같이 처리하면 그 파일과 이름이 충돌합니다. 그래서 `AddInterfaceMemberBuiltinBehaviourPass`/`Compiler.compileAlgo` 둘 다 `Method`는 건드리지 않고, 이 pass가 생기기 전과 똑같이(평범한 `AbsOp`, 소문자화된 이름) 컴파일합니다 — 오버로드 충돌과 WebIDL 반환 타입 변환 gap을 같이 정리한 뒤에 처리할 예정.
- **Why not mechanized**: 진짜 WebIDL 알고리즘은 모든 인터페이스에 대해 WebIDL의 "새 플랫폼 객체 생성" 전체 preamble(이 문서 4번 항목이 이미 다루는 그 gap)을 따라가야 하고, getter뿐 아니라 method/setter/constructor도 똑같이 실제 intrinsic 이름으로 배선돼야 하는데, 이건 아직 다 안 되어 있습니다. 지금은 그냥 "테스트를 더 진행시킬 수 있을 정도"만 급하게 만들어 둔 것 — **4번 항목(`ExpandNewInterfaceObjectPass`)과 함께 별도로 제대로 구현될 예정. 그때 이 항목과 코드의 `*(hardcoding)*` 주석도 같이 지울 것.**

## 8. "create a new Exported Function"의 `steps`를 closure 관용구로 재작성 (`SpecPatch` #22)

- **File**: `src/main/scala/esmeta/wji/lang/SpecPatch.scala` (patch #22)
- **Spec source**: `spectec/document/js-api/index.bs:1265` — `create a new Exported Function` 알고리즘이 `[$CreateBuiltinFunction$]`에 넘기는 `|steps|`를, 이미 따로 정의된 `call an Exported Function` 알고리즘을 가리키는 따옴표 문장(`"[=call an Exported Function=] |funcaddr| with arguments."`)으로 정의하는 모양입니다.
- **What's hardcoded**: 이 모양은 `AlgorithmExtractor`/`ExprParser`가 지금 인식하는 closure 관용구들(`"the following steps[, given argument(s) ...]: ..."` / `"the following steps given the list of arguments V: ..."` / `"a [=term=] which performs the following steps when called with arguments ...: ..."`) 어디에도 안 맞아서, 그냥 통짜 문자열(`Expr.Str`)로 파싱돼버리고, 나중에 `CreateBuiltinFunction`이 그 문자열을 실제 코드인 것처럼 저장했다가 호출 시점에 `NoCallable`로 죽습니다. 이 패치는 소스 텍스트를 `"the following steps given the list of arguments |argValues|: ..."` 형태로 재작성해서 이 문제를 피해갑니다 — `argValues`가 "a list of JavaScript arguments"(index.bs:1279, `call an Exported Function`의 실제 선언된 파라미터)라서, WebAssembly 함수의 arity가 동적인 이상 이름 붙인 개별 값(`"given argument V"`)이 아니라 리스트 전체를 통째로 바인딩하는 두 번째 관용구(`variadicLast`, `ExprParser.VariadicStepsClosurePrefix`)가 필요했습니다. `ExpandFollowingStepsPass`/`AddBuiltinBehaviourPass`가 그대로 hoist/3-arg builtin 변환(마지막 파라미터가 variadic이면 `argumentsList`를 통째로 별칭)을 해줍니다.
- **Why not mechanized**: 이 phrasing(따옴표로 감싼 문장이 다른 곳에 정의된 알고리즘을 가리키고, 그 문장에서 언급된 변수가 캡처된 파라미터가 되는 것) 자체는 스펙 오류가 아닙니다 — ECMA-262의 `CreateBuiltinFunction` 정의(`ecma262/spec.html`) 자체가 `behaviour` 파라미터를 "an Abstract Closure, a set of algorithm steps, or *some other definition of a function's behaviour provided in this specification*"라고 명시해서 이런 형태를 명시적으로 허용하고 있습니다. 코퍼스 전체에서 이 한 곳에만 등장해서, 파서 구조를 바꿔 일반화할 만한 가치는 아직 없습니다.

## 9. "call an Exported Function"의 애매한 예외 타입/문장 분리 (`SpecPatch` #23)

- **File**: `src/main/scala/esmeta/wji/lang/SpecPatch.scala` (patch #23)
- **Spec source**: `spectec/document/js-api/index.bs:1296` — `func_invoke`가 실패했을 때 "throw an exception. This exception should be a WebAssembly {{RuntimeError}} exception, unless otherwise indicated by the WebAssembly error mapping." 두 문장으로 서술돼 있습니다.
- **What's hardcoded**: 뒤 문장("This exception should be...")이 `AlgorithmExtractor`에 의해 앞의 `If |ret| is [=error=], throw an exception.`과 같은 `If` 안에 안 들어가고 **형제 스텝으로 잘못 분리**돼서, `|ret|`가 에러든 아니든 항상 무조건 실행되며 컴파일러가 처리 못 해 `NotSupported`로 죽습니다(`demo.js`가 실제로 이걸로 막혀있었습니다). 게다가 `Throw("an exception")` 자체도 `{{Type}}`이 안 붙어 있어서 `CompletionWrapping.ThrowTarget`(`"a {{X}} exception"` 형태만 인식)에 안 걸려, 실제로 `|ret|`가 에러인 경우엔 이것도 따로 크래시났을 것입니다. 이 패치는 두 문장을 `throw a {{RuntimeError}} exception.` 하나로 합쳐서 두 문제를 한 번에 없앱니다 — "unless otherwise indicated by the WebAssembly error mapping"라는 예외 케이스는 버립니다.
- **Why not mechanized**: "WebAssembly 실행 실패가 항상 RuntimeError로 나타나는 게 아니라 경우에 따라 다른 에러 매핑을 따를 수 있다"는 걸 제대로 반영하려면 `#errors`가 가리키는 WebAssembly 에러 매핑 테이블 전체를 별도로 기계화해야 하는데, 지금 어떤 스펙 텍스트도 RuntimeError가 아닌 다른 결과를 요구하지 않아서 그 구분을 지금 당장 만들 가치가 없습니다.
