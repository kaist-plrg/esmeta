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

## 6. `mem_size`만 "the [=mem_size=](...)"로 호출되고, 다른 임베딩 함수 콜은 전부 "[=name=](...)"

- **File**: `spectec/document/js-api/index.bs`, line 905 (`grow the memory buffer`)
- **Current**: `1. Let |ret| be the [=mem_size=](|store|, |memaddr|).`
- **Expected**: `1. Let |ret| be [=mem_size=](|store|, |memaddr|).` (앞의 "the " 제거) — 바로 다음 줄 `1. Let |store| be [=mem_grow=](|store|, |memaddr|, |delta|).`을 포함해, 이 문서 전체에서 임베딩 함수를 호출하는 모든 자리(`func_invoke`, `module_instantiate` 등)와 동일한 관용구.
- **Reason**: `the [=X=](` 형태의 함수 콜은 이 파일(js-api/index.bs) 전체를 통틀어 이 자리 하나뿐입니다(대소문자 무시하고 확인, `webidl/index.bs`도 확인했지만 없음). 함수의 *이름*을 명사구로 가리키는 표현("the mem_size of X")이 실수로 호출 표현에 섞여 들어간 것으로 보입니다. 익스트랙터의 호출 구문 패턴은 링크가 표현식의 맨 앞에서 시작해야 인식하는데, 앞에 붙은 "the "가 이걸 못 미치게 만들어서 콜 자체가 조용히 인식 못 되는 표현으로 빠집니다.
- **WJI 쪽 처리**: `SpecPatch` #27로 우회.

## 7. Host function 호출 지점만 store를 explicit하게 넘기지 않음 — 문서 전체가 지키는 explicit-threading 패턴의 유일한 예외

- **File**: `spectec/document/core/exec/instructions.rst`, lines 385-437
  ("Host Functions" 섹션); `spectec/document/js-api/index.bs`, lines
  1207-1230 (`GetGlobalValue`/`Global.prototype.value`의 setter),
  1344-1372 (`create a host function`)
- **Current**: 이 문서 대부분은 store를 **명시적으로** 주고받는다 — 예를 들어
  `GetGlobalValue`는
  ```
  1. Let |store| be the [surrounding agent]'s [[associated store]].
  1. ...
  1. Let |value| be [=global_read=](|store|, |globaladdr|).
  ```
  로 store를 받아서 그대로 넘기고, `value` attribute의 setter는
  ```
  1. Let |store| be [=global_write=](|store|, |globaladdr|, |value|).
  1. ...
  1. Set the [surrounding agent]'s [[associated store]] to |store|.
  ```
  로 결과 store를 다시 명시적으로 채워넣는다 — Wasm Core 명세 자신의
  `func_invoke(S, ...) : (S', ...)`류 embedding 함수와 정확히 같은
  explicit-threading 스타일이다. 그런데 `create a host function`이 정의하는
  `hostfunc`(Core의 `hf` — `(S'; result) ∈ hf(S; val^n)`, "A host function
  may also modify the store."라고 Core 자신이 명시한 바로 그 함수)만은 이
  패턴을 따르지 않는다(발췌):
  ```
  1. Let |hostfunc| be a host function which performs the following steps
     when called with arguments |arguments|:
      1. ...
      1. Let |result| be the result of running a host function from |func|,
         |functype|, and |arguments|.
      1. ...
      1. Let |store| be the [surrounding agent]'s [[associated store]].
      1. If |result|.[[Type]] is throw, then: ... (여기서만 |store|를 쓰고,
         exn_alloc 등으로 갱신하며 [[associated store]]에 다시 씀)
      1. Otherwise, return |result|.[[Value]].
  ```
  `store`를 인자로 받지도, 결과로 짝지어 반환하지도 않는다 — 성공 경로에서는
  앞서 읽어온 `|store|`조차 쓰이지 않는다.
- **Expected**: `hostfunc`도 나머지 알고리즘들과 같은 explicit-threading
  관용구를 따라, `state`(store)를 인자로 받고 `(state, result)`를 짝지어
  반환해야 한다 — 즉 Core 자신의 `(S'; result) ∈ hf(S; val^n)` 계약을
  js-api 쪽에서도 그대로 명시적으로 구현하는 형태.
- **Reason**: `[=associated store=]`가 등장하는 이 문서의 자리를 전부(약 40곳)
  확인했다 — store를 바꾸는 알고리즘(`Table.grow`/`Table.set`/`grow the
  memory buffer`/`initialize an Exception object`/`new Exception`
  constructor/`get the JavaScript exception tag` 등)은 예외 없이 `Let
  |store| be ...; Let (|store|, |x|) be [=mutating_fn=](|store|, ...); Set
  the [surrounding agent]'s [[associated store]] to |store|` 패턴을 지키고,
  순수하게 읽기만 하는 알고리즘(`Table.get`/`Table.length` 등)은 애초에
  write-back이 필요 없어 `Let |store| be ...`만으로 끝난다 — 둘 다 정상이다.
  즉 이 문서는 store를 다루는 명확하고 일관된 explicit-threading 관례를
  갖고 있고, `create a host function`의 `hostfunc`가 그 관례를 깨는 **유일한
  예외**다.
- **시사점** (나중에 참고용으로 남겨둠): 이걸 단순한 "실수"로 보기는
  어렵다고 본다. 오히려 **스타일이 서로 다른 두 명세(Core의 formal
  store-passing calculus, js-api의 Web-platform 산문체)를 잇는 연결 다리
  명세를 쓸 때 구조적으로 나타나기 쉬운 결과**로 보는 게 더 정확할 것
  같다. js-api의 다른 알고리즘들(`Table.set`, exported function 호출 등)은
  전부 "JS가 wasm 쪽으로 뭔가를 요청하는" 방향이라, Core의 embedding API
  (`func_invoke`, `table_write` 등 이미 `(store, ...) -> (store, ...)`
  형태로 formal하게 정의돼 있는 함수)를 그대로 감싸기만 하면 되고, 그
  과정에서 자연스럽게 explicit-threading을 물려받는다. 반대로 `create a
  host function`은 "wasm이 JS 쪽으로 뭔가를 요청하는" 유일한 방향이고,
  Core의 formal calculus에는 애초에 "JS 콜백을 실행한다"는 개념 자체가
  없다(`hf`는 그냥 미지의 "implementation-defined execution"으로만
  선언돼 있다) — 그래서 이 지점을 쓴 저자는 감쌀 만한 기존 explicit
  embedding 함수가 없는 상태에서, 대신 이 명세 생태계(HTML, ECMA-262 등)가
  "현재 agent의 상태"를 다룰 때 흔히 쓰는 ambient-state 관용구를 가져다
  썼을 가능성이 높다. 즉 이 불일치는 "두 명세의 서로 다른 멘탈 모델이
  맞닿는 이음매(seam)"에서 정확히 발생했다 — 각 저자가 자기 쪽 명세
  전통에서 가장 자연스러운 관용구를 그대로 가져오면서, 그 경계를 넘는
  지점 자체는 누구의 스타일도 아닌 채로 붕 떠버린 셈. 이런 종류의
  "연결 지점에서 발생하는 스타일 불일치"가 이 문서에서 재발하는지는
  앞으로도 눈여겨볼 만하다.

## 8. `ToWebAssemblyValue`의 `[=match_valtype=](...)` 조건 2곳만 "is true"가 안 붙음

- **File**: `spectec/document/js-api/index.bs`, line 1450, 1453 (`ToWebAssemblyValue`, `ref null heaptype` 분기)
- **Current**:
  ```
  1. Else if [=match_valtype=](|type|, [=ref=] |null| [=heap-type/extern=]),
  ...
  1. Else if |v| is an [=Exported Function=] and [=match_valtype=](|type|, [=ref=] |null| [=heap-type/func=]),
  ```
- **Expected**: 두 곳 다 `[=match_valtype=](...)` 뒤에 `is true`를 붙여야 함 — `[=match_valtype=](|type|, [=ref=] |null| [=heap-type/extern=]) is true,` / `... [=match_valtype=](|type|, [=ref=] |null| [=heap-type/func=]) is true,`.
- **Reason**: 이 문서 전체가 boolean을 리턴하는 함수 호출을 조건으로 쓸 때 예외 없이 "is true"/"is false"를 명시적으로 붙이는 확립된 컨벤션입니다 — 함수 이름을 가리지 않고 전부 이 형태입니다: `[$IsCallable$](...) is true`(1250), `[$IsCallable$](...) is false`(507), `[$HasProperty$](...) is false`(500), `[=match_externtype=](...) is false`(409), `[=match_valtype=](...) is false`(1476), `[=IsFixedLengthArrayBuffer=](...) is true`(891, 936) / `is false`(952), `[=SameValue=](...) is true`(971), `[=IsStrictlyEqual=](...) is true`(2141, 2157), `[=IsLessThan=](...) is true`(2159) — 총 8개 서로 다른 함수, 13곳. 이 문서에서 함수 호출 하나가 "is true/false" 없이 그 자체로 boolean 조건인 것처럼 맨몸으로 쓰이는 곳은 이번에 발견한 두 곳(1450, 1453)이 유일합니다. `AlgorithmExtractor`/`CondParser`는 "함수 호출 하나가 통째로 조건으로 오는" 맨몸 형태를 인식하는 규칙이 없어서(다른 모든 조건은 `X is Y`/`X matches Y` 같은 명시적 비교꼴), 이 두 조건이 조용히 파싱 실패로 빠집니다(`tests/wji/js-throw-through-wasm.js` 작성 중 실제 재현).
- **WJI 쪽 처리**: `SpecPatch` #30으로 우회 — 두 곳 다 "is true"를 추가해서, 이미 있는 "X is Y" 비교(`Cond.Eq`) 파싱 경로를 그대로 타게 만듦. 새 파서/`Cond` 변형 없이 해결됨.

## 9. `webidl/index.bs`의 "is not given" 기본값 대입 관용구에서 한 곳만 `let it be`로 대명사를 씀

- **File**: `webidl/index.bs`, line 8648
- **Current**: `1.  If |x| is not given, then let it be the {{undefined}} value.`
- **Expected**: `1.  If |x| is not given, then let |x| be the {{undefined}} value.` — 같은 파일의 동일한 "is not given" 관용구(line 9461: `If |targetRealm| is not given, let |targetRealm| be the [=current realm=].`)가 쓰는, pipe-var를 그대로 반복하는 정석적인 형태.
- **Reason**: "it"이 가리키는 대상이 사람에게는 명백히 `|x|`지만, `ExprParser`/`CondParser`는 pipe로 감싼 변수 이름만 바인딩 대상으로 인식하기 때문에 대명사를 변수로 resolve하지 못합니다. 이 파일 안에서 같은 "기본값 대입" 관용구가 쓰이는 다른 자리(line 9461)는 전부 pipe-var를 반복해서 쓰므로, 이 자리만 그 관례에서 벗어나 있습니다.
- **WJI 쪽 처리**: `SpecPatch` #18로 우회.

## 10. `Let [=host address=] |hostaddr| be ...`만 스칼라 값을 타입 링크로 annotate — 다른 모든 `Let |var| be ...`는 안 그럼

- **File**: `spectec/document/js-api/index.bs`, line 1471 (`ToWebAssemblyValue`, host value cache 할당)
- **Current**: `1. Let [=host address=] |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.`
- **Expected**: `1. Let |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.` — 스칼라 값을 선언하는 다른 모든 `Let |var| be ...`와 동일하게, 타입 링크 없이.
- **Reason**: 이 파일에서 `Let [=TYPE=] |var| be ...` 형태로 쓰이는 곳은 총 6곳뿐인데, 그중 5곳(`external value|func`/`global`/`mem`/`table`/`tag`, line 561/566/571/576/582)은 진짜 SpecTec 태그드 유니온 값을 destructure하는 표기(런타임에서 실제로 Case 태그를 갖는 값)이고, 이 한 곳만 순수 스칼라(정수) 값에 타입 annotation을 붙인 유일한 경우입니다. 다른 스칼라 `Let`은 전부 그냥 `Let |var| be ...`만 씁니다. `ExprParser`는 "[=link=] |var|"(공백만 있고 괄호 없음) 모양을 늘 "링크를 |var| 인자로 호출"로 해석하므로(`LinkProse`), 이 자리는 Let의 LHS가 `AlgoCall`로 잘못 파싱됩니다.
- **WJI 쪽 처리**: `SpecPatch` #31로 우회 — "[=host address=] " 부분을 그냥 삭제.
