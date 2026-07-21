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

## 6. `initialize an instance object`의 catch 절 삭제 (`SpecPatch` #0)

- **File**: `src/main/scala/esmeta/wji/lang/SpecPatch.scala` (patch #0)
- **Spec source**: `spectec/document/js-api/index.bs:594` ("initialize an instance object"), `index.bs:628`에서 호출되며 바로 뒤에 "If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps" 절이 따라옵니다.
- **What's hardcoded**: 이 패치는 해당 호출부에 있는 "If this throws an exception, catch it, ..." 절 전체를 삭제해서, `initialize an instance object`가 절대 예외를 던지지 않는 것처럼 취급합니다.
- **Why not mechanized**: 파싱이 안 돼서가 아닙니다 — `Cond.Throws`/`ExpandThrowsPass`는 바로 같은 파일의 다른 곳(patch #10의 이웃 호출부 참고)에서 정확히 이 "catch it, reject, terminate" 관용구를 이미 처리하고 있습니다. 이건 그냥 근거 문서화 없이 남아있는 동작상의 가정일 뿐이고, 이걸 지워도 정말 안전한지에 대한 설명은 아직 없습니다.

## 7. `react`의 중첩된 fulfilled/rejected 분기를 closure로 재작성 (`SpecPatch` #12)

- **File**: `src/main/scala/esmeta/wji/lang/SpecPatch.scala` (patch #12의 `(hardcoding)` 부분)
- **Spec source**: `webidl/index.bs`의 `react` 알고리즘과 `spectec/document/js-api/index.bs`에 있는 그 2개 호출부(`asynchronously compile a WebAssembly module` / `asynchronously instantiate a WebAssembly module`) — `[=React=] to |X|:` 스텝 바로 아래에 `* If |X| was fulfilled...:` / `* If |X| was rejected...:` 형태의 bullet 하위 분기가 붙어 있는 모양입니다.
- **What's hardcoded**: 어떤 스텝의 "body"가 그 스텝 자신의 중첩 bullet 목록으로 표현되는 이 모양은 `AlgorithmExtractor`/`InstrParser`가 지금 스텝의 연속으로 인식하는 형태가 아니라서, `Perform`의 `body`로 다시 읽혀 들어가지 못하고 fulfilled/rejected 분기 전체가 조용히 버려지고 있었습니다. 이 패치는 소스 텍스트 자체를 재작성해서 이 문제를 피해갑니다 — 두 분기를 명시적인 이름 있는 closure 두 개(`Let |onFulfilledSteps| be the following steps given argument |V|: ...`)로 뽑아내서 `[=React=]`에 일반 인자로 넘기는 식인데, 이건 `ExpandFollowingStepsPass`가 이미 hoist할 줄 아는 모양입니다.
- **Why not mechanized**: `InstrParser`가 bullet 하위 목록을 (이미 이름 붙은 스텝 자신의 `body`로서만이 아니라) *바로 앞* 한 줄짜리 스텝의 연속으로도 인식하게 만드는 건 파서의 구조 자체를 바꾸는 일인데, 지금까지는 이 호출부 한 쌍에만 필요해서 그것 하나만을 위해 일반화할 만한 가치는 아직 없습니다.
