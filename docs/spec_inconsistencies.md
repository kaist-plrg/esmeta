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
- **WJI 쪽 처리**: `SpecPatch` #28(`|state|` 파라미터 추가)/#29(성공 경로를
  `Return (|store|, |result|.\[[Value]])`로)/#34(throw 경로를 `Return
  (|store|, «[=ref.exn=] |address|, [=throw_ref=]»)`로)로 세 지점 다
  우회 — hostfunc 클로저의 모든 exit path가 이제 `(state, result)` 명시적
  threading을 따름. spectec 서브모듈 쪽도 짝을 맞춰 `$callhostfunc`가
  `(state, val*)` 대신 `(state, result)`를 리턴하도록 고쳐졌다(`[spec]
  845143d10`).

## 8. `ToWebAssemblyValue`의 `[=match_valtype=](...)` 조건 2곳만 "is true"가 안 붙음

- **File**: `spectec/document/js-api/index.bs`, line 1450, 1453 (`ToWebAssemblyValue`, `ref null heaptype` 분기)
- **Current**:
  ```
  1. Else if [=match_valtype=](|type|, [=ref=] |null| [=heap-type/extern=]),
  ...
  1. Else if |v| is an [=Exported Function=] and [=match_valtype=](|type|, [=ref=] |null| [=heap-type/func=]),
  ```
- **Expected**: 두 곳 다 `[=match_valtype=](...)` 뒤에 `is true`를 붙여야 함 — `[=match_valtype=](|type|, [=ref=] |null| [=heap-type/extern=]) is true,` / `... [=match_valtype=](|type|, [=ref=] |null| [=heap-type/func=]) is true,`.
- **Reason**: 이 문서 전체가 boolean을 리턴하는 함수 호출을 조건으로 쓸 때 예외 없이 "is true"/"is false"를 명시적으로 붙이는 확립된 컨벤션입니다 — 함수 이름을 가리지 않고 전부 이 형태입니다: `[$IsCallable$](...) is true`(1250), `[$IsCallable$](...) is false`(507), `[$HasProperty$](...) is false`(500), `[=match_externtype=](...) is false`(409), `[=match_valtype=](...) is false`(1476), `[=IsFixedLengthArrayBuffer=](...) is true`(891, 936) / `is false`(952), `[=SameValue=](...) is true`(971), `[=IsStrictlyEqual=](...) is true`(2141, 2157), `[=IsLessThan=](...) is true`(2159) — 총 8개 서로 다른 함수, 13곳. 이 문서에서 함수 호출 하나가 "is true/false" 없이 그 자체로 boolean 조건인 것처럼 맨몸으로 쓰이는 곳은 이번에 발견한 두 곳(1450, 1453)이 유일합니다. `AlgorithmExtractor`/`CondParser`는 "함수 호출 하나가 통째로 조건으로 오는" 맨몸 형태를 인식하는 규칙이 없어서(다른 모든 조건은 `X is Y`/`X matches Y` 같은 명시적 비교꼴), 이 두 조건이 조용히 파싱 실패로 빠집니다(`tests/wji/js-throw-propagation.js` 작성 중 실제 재현).
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

## 11. `index of the host function`이 이 문서의 다른 곳과 다르게 "surrounding agent's associated map" 관용구 대신 mid-algorithm dfn으로 표현됨

- **File**: `spectec/document/js-api/index.bs`, 원래 line ~512 (`read the imports`)와 ~1249-1255 (`name of the WebAssembly function`)
- **Current**: `1. Let |index| be the number of external functions in |imports|. This value |index| is known as the <dfn>index of the host function</dfn> |funcaddr|.` — 알고리즘 실행 중 계산한 값에 그 자리에서 dfn을 붙여 이름 짓고, 나중에 다른 알고리즘(`name of the WebAssembly function`)이 그 dfn 링크(`[=index of the host function=] |funcaddr|`)로 역참조하는 형태.
- **Expected**: 바로 옆 `a new Exported Function`(line 1260-)이 정확히 같은 문제(funcaddr로 나중에 값을 다시 찾아써야 함)를 이미 풀어놓은 방식 그대로 — `[=surrounding agent=]`의 `associated` map/list에 명시적으로 적어넣고 나중에 조회.
- **Reason**: "지금 계산한 값을 funcaddr 같은 키로 나중에 다른 알고리즘이 다시 찾아쓴다"는 요구사항 자체는 이 문서에 이미 8번 반복되는 패턴(`Exported Function cache`/`Memory object cache`/`host value cache`/... 전부 `surrounding agent`가 소유한 map)인데, 이 한 자리만 그 관용구를 안 쓰고 "Let |X| be N. This value X is known as Y |key|." 라는 즉석 dfn 도입 형태로 써서, 이 문서 자체가 이미 세운 패턴과 어긋납니다.
- **WJI 쪽 처리**: `SpecPatch` #13으로 우회 — `read the imports`는 `[=surrounding agent=]`의 새 `[=Function Import List=]`(캐시들과 달리 JS 객체가 아니라 순서 정보만 필요하므로 map이 아니라 plain list)에 `funcaddr`을 append하고, `name of the WebAssembly function`은 옆 module-defined 분기가 이미 쓰던 "the index of X where Y is found" 패턴을 그대로 재사용해 조회합니다. 덧붙여 host/module-defined 분기를 가르던 조건(`If |funcinst| is of the form {type functype, hostcode |hostfunc|}`)도 지금은 존재하지 않는 funcinst shape을 매칭하고 있길래, `|funcinst|.code`가 `[=hostfunc=]`/`[=func=]` 중 어느 태그인지로 판별하도록 같이 고쳤습니다 (배경은 `docs/spec_errors.md` #7의 retraction 참고).

## 12. `AddressValueToU64`/`U64ToAddressValue`만 `|addrtype|`을 `[=i32=]`/`[=i64=]` 링크 대신 raw quoted string `"i32"`/`"i64"`와 비교

- **File**: `spectec/document/js-api/index.bs`, `AddressValueToU64`(line 1486, 1491)와 `U64ToAddressValue`(line 1504-1505)
- **Current**: `1. If |addrtype| is "i32", ...` / `1. If |addrtype| is "i64", ...` (그리고 `U64ToAddressValue`의 `Else if` 변형).
- **Expected**: `1. If |addrtype| is [=i32=], ...` / `1. If |addrtype| is [=i64=], ...` — 같은 문서에서 numtype/valtype을 비교하는 다른 모든 자리(예: line 519 `If |valtype| is [=i64=]`, line 1427 `If |type| is [=i32=]`)가 쓰는 링크 형태.
- **Reason**: `addrtype`은 `table_type`/`mem_type`이 돌려주는 진짜 Wasm 값이고, `al_of_addrtype`이 `al_of_numtype`과 완전히 동일한 관용구(`string_of_X at |> nullary`, 즉 소문자 스펙 문자열을 대문자 Case 태그로 감싸는 것)로 만들어져서, 런타임 표현이 numtype/valtype과 구조적으로 완전히 같습니다(`CaseV("I32", [])`). 그런데 이 두 알고리즘만 quoted string literal("i32")과 직접 비교하도록 적혀 있어서, `CondParser`가 이걸 순수 ECMAScript 문자열 리터럴로 파싱해버립니다 — 컴파일된 IR에서 `(= addrtype "i32")`가 Wasm Case 값과 plain string을 비교하게 돼서 항상 `false`이고, 두 분기 다 스킵돼 맨 끝의 "Assert: This step is not reached"에 걸립니다.
- **WJI 쪽 처리**: `SpecPatch` #37로 우회 — 4곳의 `"i32"`/`"i64"`를 각각 `[=i32=]`/`[=i64=]`로 교체.

## 13. `Memory.grow`/`Table.length`만 `mem_type`/`table_type`의 반환값에서 필드 하나를 "the X in Y(...)" 축약형으로 project — 다른 자리는 전부 튜플 destructuring

- **File**: `spectec/document/js-api/index.bs`, `Memory.prototype.grow`(line 917)와 `Table.length` getter(line 1081)
- **Current**: `1. Let |addrtype| be the [=address type=] in [=mem_type=](|store|, |memaddr|).` / `... in [=table_type=](|store|, |tableaddr|).`
- **Expected**: 같은 `table_type(...)` 호출 결과를 이 문서 자신이 이미 세 곳(`table.get`/`table.set`/`Table` 생성자, line 1061/1090/1103)에서 쓰는 튜플 destructuring 형태 — `1. Let (|addrtype|, <var ignore>limits</var>, |elementtype|) be [=table_type=](|store|, |tableaddr|).`
- **Reason**: `table_type`의 런타임 표현(`al_of_tabletype`, `construct.ml`)은 `CaseV("", [addrtype; limits; reftype])`로, 이미 세 자리에서 튜플 destructuring으로 정확히 소비되고 있는 값과 완전히 같은 값입니다. 이 한 곳(`Table.length`)만 그 관용구 대신 필드 하나만 꺼내는 별도 표현을 씁니다. `mem_type`도 같은 패턴이지만, 런타임 표현(`al_of_memorytype`)이 `CaseV("PAGE", [addrtype; limits])`라 실제 위치 필드는 2개뿐입니다 — formal grammar(`memtype = addrtype limits PAGE`)의 `PAGE`는 세 번째 필드가 아니라 이 레코드 자체의 태그이기 때문입니다(`table_type`의 태그가 빈 문자열인 것과 대비). 이 문서 안에 `mem_type`을 튜플로 destructure하는 선례는 없지만, 구조적으로 `table_type`과 동일한 문제이자 동일한 해법이 통하므로 함께 묶어 기록합니다.
- **WJI 쪽 처리**: `SpecPatch` #40으로 우회 — 두 자리 모두 튜플 destructuring `Let (...)  be ...`로 재작성. `mem_type`은 2-tuple(`(|addrtype|, <var ignore>limits page</var>)`), `table_type`은 기존 세 자리와 동일한 3-tuple. `TupleProj`(`ExpandDestructuringLetPass`)는 태그를 보지 않고 순수 위치 기반으로 project하므로(`State.apply`의 `case Wasm(ALValue.CaseV(_, vs)) => apply(vs, field)`), `mem_type`의 태그가 `table_type`과 다르다는 사실은 정확성에 영향이 없습니다.

## 14. `IsFixedLengthArrayBuffer`만 외부 AO를 부르는데 `[=...=]`(값 링크) 문법을 씀 — 다른 자리는 전부 `[$...$]`(AO 호출)

- **File**: `spectec/document/js-api/index.bs`, `refresh the Memory buffer`(line 891), `toFixedLengthBuffer`(line 936), `toResizableBuffer`(line 952)
- **Current**: `1. If [=IsFixedLengthArrayBuffer=](|buffer|) is true, ...`
- **Expected**: `1. If [$IsFixedLengthArrayBuffer$](|buffer|) is true, ...` — 이 문서가 진짜 ECMA-262 AO를 호출할 때 이미 일관되게 쓰는 형태(`[$Get$]`, `[$HasProperty$]`, `[$IsCallable$]`, `[$OrdinaryObjectCreate$]`, `[$CreateDataProperty$]`, `[$SetIntegrityLevel$]` 등 수십 곳).
- **Reason**: `IsFixedLengthArrayBuffer`는 이 문서 안에 정의된 dfn이 아니라, anchor 테이블(`text: IsFixedLengthArrayBuffer; url: sec-isfixedarraybuffer`, index.bs:260)을 통해 ResizableArrayBuffer 제안의 진짜 외부 AO를 가리키는 cross-reference입니다. Bikeshed 문법상 `[=...=]`(값/dfn 링크)와 `[$...$]`(다른 스펙의 abstract-op 호출)는 서로 다른 용도인데, 이 문서는 외부 AO를 부를 땐 항상 `[$...$]`를 쓰고 `[=...=]`는 로컬 dfn/값 링크 전용으로 일관되게 구분해왔습니다. 이 세 곳만 그 구분에서 벗어나 `[=...=]`를 씁니다. `Compiler.nameFromLink`는 `[=...=]` 링크를 전부 이 문서에 로컬로 정의된(그래서 WJI가 소문자로 등록한) 알고리즘으로 가정해 무조건 소문자화하는데, `IsFixedLengthArrayBuffer`는 mainline `cfg.fnameMap`에 원래 대소문자(`IsFixedLengthArrayBuffer`)로 등록돼 있어서 `isfixedlengtharraybuffer`로는 조회가 안 되고 `UnknownFunc`로 죽습니다.
- **WJI 쪽 처리**: `SpecPatch` #41로 우회 — 세 자리 모두 `[=IsFixedLengthArrayBuffer=](|buffer|)`를 `[$IsFixedLengthArrayBuffer$](|buffer|)`로 교체(인자가 동일해 패치 하나로 세 곳 다 커버). `[$...$]` 경로는 이미 `ResolveLinksPass.resolveFuncName`이 원래 대소문자를 보존하므로 별도 코드 변경 불필요.

## 15. `create an operation function`의 두 곳에서만 `regular operation`/`static operation`이 링크 안 된 채로 쓰임

- **File**: `webidl/index.bs`, lines 12558-9, 12581-2 (`create an operation function`)
- **Current**: `"...(if |op| is a regular operation) or for [=static operations=] (if |op| is a static operation)..."` (두 자리, 줄바꿈 위치만 다름 — 12558-9는 `|n|`-인자 호출, 12581-2는 `0`-인자 호출).
- **Expected**: `"...(if |op| is a [=regular operation=]) or for [=static operations=] (if |op| is a [=static operation=])..."`
- **Reason**: 같은 알고리즘 안, 겨우 14줄 위(line 12544)에서 이미 동일한 술어를 링크된 형태로 씁니다 — `"|target| is an [=interface=], and |op| is not a [=static operation=]"`. `regular operation`/`static operation`은 이 파일에서 export된 진짜 dfn이고(`dfn-regular-operation` line 1883, `dfn-static-operation` line 3002), 이 파일 전체에서 15곳 넘게(lines 1067, 1229, 1899, 1911, 2321, 2324, 2344, 2383, 3071-2, 9787, 9793, 10725, 10742, 12592, 12611) 링크된 형태로 쓰이는데, 이 두 자리만 예외적으로 plain text입니다 — `[= =]` 마크업에 의존하는 기계화된 추출이 이 두 자리에서만 깨집니다.
- **WJI 쪽 처리**: `SpecPatch` #42로 우회 — 두 자리 모두 링크. 링크되면 `CondParser.ArticleLink`가 `"|op| is a [=regular operation=]"`을 `Cond.IsType(op, "regular operation")`로 파싱하고, `ExpandWjiIsTypePass`가 `esmeta.wji.Initialize`가 이미 `operation` 레코드에 심어둔 `kind` 필드(`Enum("RegularOperation"|"StaticOperation")`)와 비교하는 `Cond.Eq`로 낮춥니다.
