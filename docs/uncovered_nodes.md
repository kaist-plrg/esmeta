# 커버리지에서 남은 미방문 노드 (Uncovered Nodes)

공식 js-api 테스트와 `tests/wji/complementary/`를 모두 돌려도 한 번도 실행되지 않는
WJI IR 노드와, 각각이 남은 이유를 기록한다. 여기 있는 노드는 전부 테스트를 더 써서는
덮을 수 없다. 범위 밖이거나, 실행 경로상 도달할 수 없다.

- **측정**: `sbt "Test/runMain esmeta.wji.WjiCoverage --full"` (공식 js-api +
  complementary, `tests/wji/manual/`은 제외), 결과는 `logs/wji-coverage-full/`. WebIDL
  알고리즘(`webidl/index.bs`에서 온 것)과 `manuals/funcs`의 손으로 쓴 IR은 측정 대상이
  아니다.
- **결과 (2026-11-04)**: 함수 128/130, 노드 1686/1726 (97.7%), 분기 591/610 (96.9%).
  미방문 노드 40개, 11개 함수. 공식 코퍼스만으로는 노드 1495/1726 (86.6%), 분기 492/610 (80.7%).
  특정 인자로만 실패할 수 없는 호출(WebIDL `new`, `a new promise`, `react`)은 `!`로 하드코딩해
  (`docs/hardcodes.md` #28) 그 호출 쪽 전파 가드가 남지 않는다.
- **노드 번호**: 아래 번호는 그 측정의 IR 기준이다. IR이 바뀌면 번호도 바뀌므로,
  `logs/wji-coverage-full/annotated-ir/<함수>.ir`에서 `[0]`으로 표시된 줄을 내용으로
  다시 찾는다.

| 분류 | 노드 |
|---|---|
| 1. 범위 밖 | 23 |
| 2. 불변 조건이 보장함 | 17 |
| 2.1 호출이 그 입력에서는 실패하지 않음 | 7 |
| 2.2 값이 도메인 밖이라 조건이 성립하지 않음 | 10 |
| 2.2.1 조건이 도메인 바깥을 가리킴 | 4 |
| 2.2.2 경우를 다 나눈 뒤 남는 자리 | 6 |

분류는 "왜 실행될 수 없는가"를 기준으로 한다. 범위 밖이 아닌 노드는 모두, 그 지점에서 노드에
이르는 조건이 늘 거짓이라는 불변 조건 때문에 실행되지 않는다. 그 아래는 불변 조건이 **무엇에
대한 사실인지**로 나눈다.

- **2.1**: 노드가 호출의 실패를 검사한다. 호출 대상은 일반적으로는 실패할 수 있지만, 이 지점에
  넘어오는 입력은 그 실패 조건을 피한다. 노드는 모두 호출 결과를 검사한다. 명세나 `SpecPatch`가
  붙인 `?` 가드, Infra의 암묵적 재throw 규칙으로 `PropagateUnguardedCallsPass`가 넣은 가드(암묵
  전파), 또는 명세가 직접 적은 실패 분기("If |store| is error") 중 하나다.
- **2.2**: 노드가 값의 모양을 검사한다. 그 값이 가질 수 있는 범위(도메인)가 정해져 있어서 그
  조건에 걸리는 값이 없다. 노드는 모두 명세에 적힌 분기이거나, 명세의 if/else-if 뒤나 알고리즘
  끝에 생기는 암묵 else/fallthrough다.

불변 조건이 어디서 오는지(앞선 검사, 값을 만드는 곳, 타입 정의)는 분류 기준이 아니라 각 항목의
근거로 적는다. 한 항목의 근거가 여러 곳에서 올 수도 있다.

## 1. 범위 밖 (23)

- **`Module.prototype.customSections` (11, 함수 전체)**: `docs/out_of_scope.md` #1.
  `customSections.any.js`가 실패하는 것도 이 때문이다.
- **`create the WebAssembly namespace object` (6, 함수 전체)**: 네임스페이스 객체가
  만들어질 때 부르는 훅(`index.bs:1810`)인데, WJI는 이 훅을 호출하지 않는다. 대신
  `CompileError`/`LinkError`/`RuntimeError` 생성자는 정적 `_NativeError_` intrinsic으로
  미리 만들고(`docs/hardcodes.md` #10), 그것을 네임스페이스 객체에 붙이는 일은 손으로 쓴
  namespace 생성 IR이 한다(#29).
- **할당 실패 (6)**: `WebAssembly.Table` 생성자의 `table_alloc` ERROR → RangeError
  (39788-39790), `WebAssembly.Memory` 생성자의 `mem_alloc` ERROR → RangeError
  (39636-39638). implementation-defined 한계라 실제 엔진은 이 분기로 들어간다 (Node
  26.12.1: `new WebAssembly.Table({element: "anyfunc", initial: 10000001})`, memory64
  `initial: 300000n` 둘 다 RangeError). 하지만 SpecTec의 할당은 실패하지 않고, 이 한계를
  다루는 공식 테스트도 범위 밖이다 (`docs/out_of_scope.md` #4, #7, #8).

## 2. 불변 조건이 보장함 (17)

### 2.1 호출이 그 입력에서는 실패하지 않음 (7)

| 노드 | 호출 대상이 실패하는 조건 | 이 지점에 오는 입력 |
|---|---|---|
| `global_write` ERROR (3) | global이 const | 늘 var |
| `? ToInt32(v)` (2) | `v`의 `valueOf`가 throw | 늘 Number |
| `? HasProperty(o, …)` (1) | `o`가 Proxy 등 exotic 객체 | 늘 평범한 객체 |
| `Grow the memory buffer` (1) | max를 넘김 | 늘 max 이내 |

- **`set:WebAssembly.Global.prototype.value`의 `global_write` ERROR → RangeError
  (39984-39986)**: 노드는 명세에 적힌 "If |store| is error" 분기지만, 검사하는 건 `global_write`
  호출의 실패다. `global_write`가 ERROR를 내는 경우는 global이 mut이 아닐 때뿐이고
  (`spectec/document/core/appendix/embedding.rst`), 앞에서 mut이 아닌 global을 이미 TypeError로
  거른다(`index.bs:1226`). mut은 globaltype의 일부라 바뀌지 않으므로 사용자 `valueOf`가
  끼어들어도 그대로다.
- **`towebassemblyvalue`의 `? ToInt32(v)` 가드 2개 (40302, 40311, 명세의 `?`)**: 둘 다 `v`가
  Number임을 확인한 뒤의 호출이라 `ToNumber`가 항등이고 실패하지 않는다.
- **`read_the_imports`의 `? HasProperty(o, …)` 가드 (39309, `SpecPatch`가 붙인 `?`)**:
  `index.bs:500`의 "if [$HasProperty$](|o|, |componentName|) is false"에는 원문에 표시가 없고,
  `?`는 `SpecPatch` #57이 붙인다(`docs/spec_errors.md` #29, 업스트림에도 WebAssembly/spec#2256로
  반영됨). spec_errors #29의 근거는 "일반적으로 HasProperty는 Proxy면 throw할 수 있다"이지만, 이
  자리의 `o`는 Proxy일 수 없다. `o`는 늘 `instantiate a builtin set`/`instantiate imported
  strings`가 `OrdinaryObjectCreate(null)`로 만들고 데이터 프로퍼티만 채운 객체라, `HasProperty`가
  사용자 코드를 부르지 않는다. `?` 대신 `!`가 더 정확하지만, 업스트림에 막 머지된 표기라 그대로
  둔다.
- **`HostResizeArrayBuffer`의 `Grow the memory buffer` 가드 (39731, 암묵 전파)**: grow는 새
  크기가 memory의 max를 넘을 때만 실패한다. 입력이 max 이내인 이유는 세 가지가 함께 성립해서다.
  ECMA-262의 `resize`가 `maxByteLength`보다 큰 크기를 HostResizeArrayBuffer 전에 RangeError로
  거르고, `maxByteLength`를 정하는 곳은 `toResizableBuffer` 하나뿐이며 그 값이 max × 65536이고,
  delta를 버퍼 길이로 계산하는데 버퍼 길이는 메모리 크기와 늘 같다(refresh와 `memory.grow` 훅이
  맞춤). 같은 `Grow the memory buffer`가 `Memory.prototype.grow`에서는 실제로 실패할 수 있어서,
  호출 지점과 무관한 규칙으로는 지울 수 없다.

이 노드들을 지우려면 호출 대상이 이 인자로는 실패하지 않는다는 걸 알아야 한다. 인자를 보는
분석이 필요하고, 그게 없으면 호출을 `!`로 하드코딩하는 수밖에 없다(`docs/hardcodes.md` #28과 같은
방식).

### 2.2 값이 도메인 밖이라 조건이 성립하지 않음 (10)

#### 2.2.1 조건이 도메인 바깥을 가리킴 (4)

명세의 조건이 검사하는 값의 도메인 밖을 가리켜서 늘 거짓이다.

- **`WebAssembly.Table` 생성자의 "elementtype이 REF가 아님" → TypeError (39752-39754, 명세
  분기)**: `TableDescriptor.element`는 IDL enum `TableKind`(`"externref"`/`"anyfunc"`)라서 변환
  단계에서 이 두 값만 통과하고, `ToValueType`(`index.bs:1172`)은 이 둘을 funcref/externref로
  바꾼다. 그래서 elementtype의 도메인은 {funcref, externref}이고 늘 REF다.
- **`read_the_imports`의 "|o|가 Object가 아님" (39306, 명세 분기)**: `index.bs:500`의 "If |o| is
  not an Object or …"에서 앞쪽 조건. `o`는 늘 `OrdinaryObjectCreate(null)`로 만든 객체라(2.1의
  `HasProperty` 항목 참고) Object다.

#### 2.2.2 경우를 다 나눈 뒤 남는 자리 (6)

명세가 경우를 하나씩 나열했는데, 그 나열이 이미 값의 도메인 전체를 덮어서 남는 값이 없다.

- **`string_value_of_the_extern_type`의 끝 fallthrough (39521-39522, 알고리즘 끝 fallthrough)**:
  extern type은 SpecTec 정의상 function/table/memory/global/tag 다섯 가지뿐이라
  (`syntax externtype`), 다섯 경우를 모두 확인한 뒤에 남는 값이 없다.
- **`getglobalvalue`의 globaltype 모양 검사가 거짓인 경우 (39956, "is of the form" 뒤 암묵
  else)**: globaltype은 SpecTec 정의상 생성자가 하나뿐인 튜플(`syntax globaltype = mut? valtype`)
  이라 이 검사는 늘 참이다. 같은 사실을 `set:WebAssembly.Global.prototype.value`는 "Let mut
  valuetype be …"로 받아 assert가 되므로, 명세 문장 형태에 따라 노드가 생기기도 하고 안 생기기도
  한다.
- **`towebassemblyvalue`의 objectkind가 array/struct 둘 다 아닌 경우 (40321, 명세의 "If array /
  Else if struct" 뒤 암묵 else)**: 이 분기는 "|v| is an Exported GC Object"(`index.bs:1460`)일
  때만 들어오고, WJI는 이를 `[[ObjectAddress]]` 내부 슬롯이 있는지로 검사한다
  (`ExpandWjiIsTypePass`). 이 슬롯과 `[[ObjectKind]]`를 가진 객체를 만드는 곳은 "a new Exported
  GC Object"(1653) 하나뿐이고, 그 호출은 `ToJSValue`의 두 곳(1398-1399)뿐이며 둘 다 "struct"나
  "array"를 넘긴다. `[[ObjectKind]]`는 생성 때(1655) 말고는 바뀌지 않고, JS 코드는 객체에 내부
  슬롯을 붙일 수 없다(같은 이름의 프로퍼티를 만들어도 슬롯이 되지 않는다). 그래서 objectkind의
  도메인은 {"array", "struct"}다.
- **`tojsvalue`의 끝 fallthrough (40234-40235, 알고리즘 끝 fallthrough)**: SpecTec 정의상 `val`의
  모양 중 명세 분기가 다루지 않는 건 v128과 exnref뿐이다(이 둘을 막는 명세의 assert는 기계화되지
  않아 `assert (yet …)`로 남음). 이 둘이 실제로 오지 않는 이유는 호출하는 곳마다 다르다.
  - `Table.prototype.get`, `GetGlobalValue`, exported function 호출(인자와 결과), host function
    실행, `Exception.prototype.getArg`는 먼저 TypeError로 거른다(`index.bs` 1091, 1212, 1284,
    1324, 1760).
  - JS 예외 태그의 payload(`index.bs:1302`)는 그 태그 타입이 « externref »로 고정돼 있다
    (`index.bs:1788`, 1798).
  - `REF.EXTERN`을 풀어 다시 부르는 재귀 호출은 Wasm 타입 규칙상 extern이 감싼 값이 exnref일 수
    없다.

이 노드들을 지우려면 lowering이 검사 대상 값의 도메인을 알아야 한다. `string_value_of_the_extern_type`과
`getglobalvalue`는 SpecTec syntax 정의만 보면 되지만, 나머지는 값을 만드는 곳이나 호출하는 쪽까지
따라가야 한다. 노드 수에 비해 비용이 커서 하지 않았다.
