# Spec Inconsistencies

`docs/spec_errors.md`와 별개로 관리하는 목록입니다. 여기 항목들은 그 자체로 "틀린"
것은 아닙니다 — 문법/마크업만 놓고 보면 유효합니다. 다만 **같은 문서가 구조적으로
동일한 다른 자리에서는 이미 확립해둔 패턴이 있는데, 그 자리에서만 벗어나 있는
경우**를 기록합니다 (예: 똑같은 방식으로 만들어진 값 N개 중 하나만 다른 관용구를
씀). `docs/spec_errors.md`처럼 스펙 작성자에게 보고할 가치가 있는 목록이라 별도로
관리합니다.

## 1. `ToJSValue`의 `ref.i31` 케이스만 `interpreted as a mathematical value`가 빠짐

- **File**: `spectec/document/js-api/index.bs`, line 1397 (`ToJSValue`)
- **Current**: `1. Return [=𝔽=](|i31|).`
- **Expected**: `1. Return [=𝔽=](|i31| interpreted as a [=mathematical value=]).`
  — `ToJSValue`의 형제 케이스 4개(i64/i32/f32/f64, line 1382-1393)와 동일한 관용구.
- **Reason**: `|i31|`은 바로 앞 스텝에서 `signed_31(|u31|)`의 결과로 바인딩되는데,
  이건 `|i64|`가 `signed_64(|u64|)`의 결과로 바인딩되는 것과 완전히 같은 모양입니다
  — 그런데 `i64`/`i32`/`f32`/`f64` 케이스는 전부 `𝔽`/`ℤ`를 부르기 전에
  `interpreted as a [=mathematical value=]`를 붙이고, `i31` 케이스만 안 붙어있습니다.
  `𝔽`/`ℤ` 호출 7군데를 전부 확인한 결과 이 자리가 유일한 예외입니다. `interpreted
  as a mathematical value`가 실제로 하는 일(wasm spec에서 넘어온 raw 값을 수학적
  값으로 변환)이 이 자리에도 똑같이 필요한데 생략된 것으로 보입니다.
- **WJI 쪽 처리**: `SpecPatch` #25로 우회.

## 2. `[=external value|X=]` pipe-aliasing 대신 등록된 `for`-scoped `[=external value/X=]` 형태를 안 씀

- **File**: `spectec/document/js-api/index.bs`, lines 513, 529, 533, 538, 560, 561, 565, 566, 570, 571, 575, 576 (`read the imports`와 `create an exports object` 전체에서 12군데)
- **Current**: `[=external value|func=]`, `[=external value|global=]`, `[=external value|mem=]`, `[=external value|table=]` (Bikeshed pipe-display aliasing: 링크 텍스트는 `external value`, 표시 텍스트가 각 variant 이름).
- **Expected**: `[=external value/func=]`, `[=external value/global=]`, `[=external value/mem=]`, `[=external value/table=]` — 같은 파일에서 이미 올바르게 쓰인 `[=external value/tag=]`(lines 544, 581, 582)와 동일한, `for`-scoped 형태로 `external value`에 링크하는 정석적인 Bikeshed 방식.
- **Reason**: 파일 자체의 link-defaults 블록(lines 217-220)이 `external value` 아래에 `for`-scoped sub-term으로 딱 하나, `tag`만 등록해뒀습니다 — 나란히 있는 "external-type" 블록(226-233)이 자기 5개 variant(`func`/`table`/`mem`/`global`/`tag`) 전부를 `for: external-type`으로 등록한 것과 다릅니다. `func`/`global`/`mem`/`table`은 등록이 안 돼있어서 `[=external value/func=]`처럼 쓰면 Bikeshed에서 유효하게 resolve가 안 되고, 그래서 스펙 저자가 5개 variant 중 `tag`를 뺀 4개에 대해 pipe-display aliasing으로 우회한 것으로 보입니다. 이 파일 자체가 이미 확립해둔 두 곳(`external value/tag`, `external-type`의 5개 variant 전부)과 비교하면 이 4개만 패턴에서 벗어나 있습니다.
- **WJI 쪽 처리**: `SpecPatch` #15(prose를 `for`-scoped 형태로 정규화)와 #16(누락된 4개 anchor를 link-defaults 블록에 등록)로 우회.

## 3. `the memory address |frame|...`만 다른 6군데와 다르게 `[=memory address=]` dfn-link가 빠짐

- **File**: `spectec/document/js-api/index.bs`, line 929 (`memory.grow`)
- **Current**: `1. Let |memaddr| be the memory address |frame|.[=frame/module=].[=moduleinst/memaddrs=][|x|].`
- **Expected**: `1. Let |memaddr| be the [=memory address=] |frame|.[=frame/module=].[=moduleinst/memaddrs=][|x|].`
- **Reason**: `memory address`는 진짜 dfn이고, 이 파일의 다른 6군데 등장(예: lines 346, 827, 838, 851, 861, 902)에서는 전부 `[= =]`로 링크돼있습니다. 이 자리 하나만 링크 대괄호 없이 맨 텍스트로 쓰여있어서, 이 파일 자체가 이 용어에 대해 이미 확립해둔 관례와 어긋납니다.
- **WJI 쪽 처리**: `SpecPatch` #20으로 우회 (`ExprParser.TypeAnnotatedPrefix`가 이제 `|var|`로 이어지는 EXPR도 받아들이도록 확장해둬서, "the [=TERM=] EXPR" 형태가 TERM을 순수 타입 annotation으로 버리고 EXPR만 남기는 식으로 파싱됨 — 이 파일의 다른 `TypeAnnotatedPrefix` 사용과 동일한 관용구).

## 4. `asynchronously compile a WebAssembly module`에서 `reject`가 링크 안 된 채로 쓰임

- **File**: `spectec/document/js-api/index.bs`, lines 452, 455 (`asynchronously compile a WebAssembly module`)
- **Current**: `1. If |module| is [=error=], reject |promise| with a {{CompileError}} exception and return.` 및 `1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, reject |promise| with a {{CompileError}} exception.`
- **Expected**: `reject` → `[=reject=]`로 링크 (webidl/index.bs의 "To reject a Promise<T> ... with reason r" 알고리즘, `[=reject=]` dfn 주변 8658번 줄).
- **Reason**: 같은 파일에서 이 알고리즘을 부르는 다른 모든 자리는 대소문자 상관없이 링크돼있고(lines 621, 626, 629, 646, 648의 `[=reject=]`/`[=Reject=]`), 짝인 `[=Resolve=]`(line 458, 두 번째 unlinked reject 바로 세 줄 아래)도 올바르게 링크돼있습니다. `asynchronously compile a WebAssembly module` 안의 이 두 곳만 `[= =]` 링크 없는 맨 prose로 쓰여있어서, `[= =]` 마크업에 의존하는 기계화된 추출이 이 두 자리에서만 깨집니다.
- **WJI 쪽 처리**: `SpecPatch` #11로 우회.

## 5. `is [=exception=] |exnaddr|`만 다른 destructuring match와 다르게 `of the form`이 빠짐

- **File**: `spectec/document/js-api/index.bs`, line 1297 (`call an Exported Function`)
- **Current**: `1. If |ret| is [=exception=] |exnaddr|, then`
- **Expected**: `1. If |ret| is of the form [=exception=] |exnaddr|, then` — 이 파일에서 payload를 갖는 case를 destructure하는 다른 모든 자리(예: `is of the form [=external-type/func=] |functype|`, lines 506+)와 동일한 관용구.
- **Reason**: `[=exception=]`은 `embedding.rst`의 `exception ::= EXCEPTION exnaddr`(line 49)에 링크되는데, payload(`exnaddr`)를 갖는 case입니다 — 이 파일 자체의 관례상 payload를 바인딩하려면 "is of the form"으로 도입해야 합니다. 이게 없으면 그냥 평범한 동등 비교로 읽혀서, RHS가 한 번도 선언된 적 없는 변수 `|exnaddr|`로 `EXCEPTION` 값을 구성하는 것처럼 보입니다. `[=error=]`(payload 없는 0-인자 case, `error ::= ERROR`)는 이 문제가 없습니다 — payload를 갖는 케이스를 "of the form" 없이 비교하는 건 이 코퍼스에서 이 자리 하나뿐입니다.
- **WJI 쪽 처리**: `SpecPatch` #24로 우회.
