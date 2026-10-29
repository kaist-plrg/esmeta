# 커버리지에서 남은 미방문 노드 (Uncovered Nodes)

공식 js-api 테스트와 `tests/wji/complementary/`를 모두 돌려도 한 번도 실행되지 않는
WJI IR 노드와, 각각이 남은 이유를 기록한다. 여기 있는 노드는 전부 테스트를 더 써서는
덮을 수 없다. 범위 밖이거나, 실행 경로상 도달할 수 없다.

- **측정**: `sbt --client "Test/runMain esmeta.wji.WjiCoverage --full"` (공식 js-api +
  complementary, `tests/wji/manual/`은 제외), 결과는 `logs/wji-coverage-full/`. WebIDL
  알고리즘(`webidl/index.bs`에서 온 것)과 `manuals/funcs`의 손으로 쓴 IR은 측정 대상이
  아니다.
- **결과 (2026-10-29)**: 함수 127/130, 노드 1756/1837 (95.6%), 분기 616/666 (92.5%).
  미방문 노드 81개, 31개 함수. 측정 뒤 `WebAssembly.instantiate` 디스패처의 늘 참인 인자 개수
  검사를 지워, 그 거짓 분기 1개(42290)가 없어졌다. 아래는 남은 80개.
- **노드 번호**: 아래 번호는 그 측정의 IR 기준이다. IR이 바뀌면 번호도 바뀌므로,
  `logs/wji-coverage-full/annotated-ir/<함수>.ir`에서 `[0]`으로 표시된 줄을 내용으로
  다시 찾는다.

| 분류 | 노드 |
|---|---|
| 1. 범위 밖 | 36 |
| 2. WebIDL `new` 호출에 붙은 가드 | 20 |
| 3. 앞단 검사가 이미 걸러낸 분기 | 10 |
| 4. `a new promise`/`react`의 `?` 가드 | 7 |
| 5. 생성자를 전부 다룬 뒤의 fallthrough | 5 |
| 6. 실패할 수 없는 호출에 붙은 암묵 전파 가드 | 2 |

2, 4, 6은 대부분 "호출이 abrupt면 전파"하는 가드다 (2의 41721-41724만 "If this throws"
catch 처리). 어떤 알고리즘이 Completion Record를 반환하는지는 호출 그래프 위의 고정점으로
정해지는데 (`CompletionAlgorithms`), 이 분석은 인자 값을 보지 않는다. 그래서 특정 인자로
부를 때만 실패할 수 없는 호출에도 가드가 붙는다.

## 1. 범위 밖 (36)

- **`Module.prototype.customSections` (11, 함수 전체)**: `docs/out_of_scope.md` #1.
  `customSections.any.js`가 실패하는 것도 이 때문이다.
- **`memory.grow` (13, 함수 전체)**: wasm `memory.grow` 명령 직후에 부르는 훅
  (`spectec/document/js-api/index.bs:923`)인데, WJI는 이걸 호출하지 않고 JS/wasm
  경계에서 길이 불일치로 감지해 버퍼를 갱신한다 (`docs/hardcodes.md` #27).
- **`create the WebAssembly namespace object` (6, 함수 전체)**: 네임스페이스 객체가
  만들어질 때 부르는 훅(`index.bs:1810`)인데, WJI는 이걸 호출하지 않고 그 결과인
  `CompileError`/`LinkError`/`RuntimeError`를 정적으로 만든다 (`docs/hardcodes.md` #10).
- **할당 실패 (6)**: `WebAssembly.Table` 생성자의 `table_alloc` ERROR → RangeError
  (39845-39847), `WebAssembly.Memory` 생성자의 `mem_alloc` ERROR → RangeError
  (39680-39682). implementation-defined 한계라 실제 엔진은 이 분기로 들어간다 (Node
  26.12.1: `new WebAssembly.Table({element: "anyfunc", initial: 10000001})`, memory64
  `initial: 300000n` 둘 다 RangeError). 하지만 SpecTec의 할당은 실패하지 않고, 이 한계를
  다루는 공식 테스트도 범위 밖이다 (`docs/out_of_scope.md` #4, #7, #8).

## 2. WebIDL `new` 호출에 붙은 가드 (20)

`[=/new=]`는 WebIDL의 `new` 알고리즘 호출로 컴파일되고, `new`는
`internally create a new object implementing the interface`를 newTarget = undefined로
부른다 (`webidl/index.bs` 13817-13840).

| 함수 | 노드 | 가드 대상 |
|---|---|---|
| `create_a_global_object`, `create_a_memory_object`, `create_a_table_object`, `create_a_tag_object`, `create_an_exception_object` | 39952, 39649, 39801, 40438, 40497 | `new` |
| `construct_a_webassembly_module_object` | 39253 | `new` |
| `asynchronously_instantiate_a_webassembly_module_closure1` | 41717 | `new` |
| 〃 | 41721-41724 | `initialize an instance object`의 "If this throws" catch |
| `create_an_exports_object` | 39447, 39455, 39463, 39471 | `create a {global,memory,Table,Tag} object` |
| `initialize_an_instance_object` | 39483 | `create an exports object` |
| `INTRINSICS.WebAssembly.Instance` | 39626 | `initialize an instance object` |
| `INTRINSICS.get:WebAssembly.JSTag` | 39539 | `create a Tag object` |
| `call_an_exported_function` | 40131 | `create an Exception object` |
| `asynchronously_compile_a_webassembly_module_closure1` | 41703 | `construct a WebAssembly module object` |

- **실패할 수 없는 이유**: `internally create`가 실패할 수 있는 곳은 세 군데다.
  `? Get(newTarget, "prototype")`과 `? GetFunctionRealm(newTarget)`은 newTarget이 있는
  분기에만 있고, `Define the iteration methods`는 `[Global]` 인터페이스에서만 실행된다.
  `new`는 newTarget을 늘 undefined로 넘기고, WebAssembly 인터페이스에는 `[Global]`이
  없다. 그 밖의 단계는 실패하지 않는다 (unforgeable 복사는 명세가 `!`로 단언한다).
  반면 JS 생성자 경로는 newTarget을 넘기므로 `internally create` 자체는 실제로 실패할 수
  있다 (`tests/wji/complementary/constructor-prototype-getter-throws.js`).
- **지우려면**: `internally create`를 인자에 따라 다르게 판정해야 한다. 예를 들어 상수
  인자로 부르는 호출 지점마다 그 상수로 특수화한 알고리즘을 쓰는 lowering이 필요하다.
  명세에서 `new`의 단계를 `[=!=]`로 바꾸는 것도 방법이지만, 명세 텍스트를 고치는 일이라
  하지 않았다.

## 3. 앞단 검사가 이미 걸러낸 분기 (10)

명세에 적힌 분기지만, 그 조건이 앞선 단계나 값을 만든 쪽에서 이미 정해져 있어 거짓이 될
수 없다. `ToInt32` 두 개만 호출 결과 가드다.

- **`read_the_imports`의 "|o|가 Object가 아님" (39307)**: `index.bs:500`의 "If |o| is not an
  Object or …"에서 앞쪽 조건이 참인 경우. `o`는 늘 `instantiate a builtin set`/`instantiate
  imported strings`가 `OrdinaryObjectCreate(null)`로 만든 객체라 Object다.

- **`set:WebAssembly.Global.prototype.value`의 `global_write` ERROR → RangeError
  (40046-40048)**: 앞에서 mut이 아닌 global을 이미 TypeError로 거르고
  (`index.bs:1226`), `ToWebAssemblyValue`가 값의 타입을 맞춘다. global의 타입은 바뀌지
  않으므로 사용자 `valueOf`가 끼어들어도 쓰기는 실패하지 않는다.
- **`WebAssembly.Table` 생성자의 "elementtype이 REF가 아님" → TypeError
  (39809-39811)**: `TableDescriptor.element`가 IDL enum (`"externref"`/`"anyfunc"`)으로
  먼저 검증되어 `ToValueType`이 늘 REF를 돌려준다.
- **`towebassemblyvalue`의 `? ToInt32(v)` 가드 2개 (40367, 40376)**: 둘 다 `v`가
  Number임을 확인한 뒤의 호출이라 `ToNumber`가 항등이고 실패하지 않는다.
- **`towebassemblyvalue`의 objectkind가 array/struct 둘 다 아닌 경우 (40386)**: Exported
  GC Object는 이 두 kind로만 만들어진다.

## 4. `a new promise`/`react`의 `?` 가드 (7)

WebIDL의 `a new promise`와 `react`는 `[=?=] NewPromiseCapability(|constructor|)`로
쓰여 있어서 (`webidl/index.bs` 8612, 8688) 이 둘과 이들을 부르는 함수가 Completion
Record를 반환하는 알고리즘이 된다.

| 함수 | 노드 | 가드 대상 |
|---|---|---|
| `instantiate_a_promise_of_a_module` | 39516, 39521 | `a new promise`, `react` |
| `instantiate_a_promise_of_a_module_closure1` | 41736, 41741 | `asynchronously instantiate`, `react` |
| `asynchronously_compile_a_webassembly_module` | 39260 | `a new promise` |
| `asynchronously_instantiate_a_webassembly_module` | 39501 | `a new promise` |
| `instantiate_bytes` | 39529 | `Asynchronously compile` |

- **실패할 수 없는 이유**: `constructor`가 언제나 진짜 `%Promise%` intrinsic이다.
  `IsConstructor`는 참이고, `Construct` 안의 `Get(%Promise%, "prototype")`은 쓰기·재설정
  불가 데이터 프로퍼티라 throw하지 않으며, executor의 `resolve`/`reject`는 늘 callable이다.
  ECMA-262 자신도 같은 호출을 `! NewPromiseCapability(%Promise%)`로 쓴다 (14곳 모두 `!`).
- **지우려면**: 두 `?`를 `!`로 바꾸는 *(inconsistency)* SpecPatch면 연쇄 전체가 사라진다.
  명세 텍스트를 고치는 일이라 하지 않았다. `NewPromiseCapability`는 다른 생성자로는 실제로
  실패하는 연산이라, 인자를 보지 않는 lowering 규칙으로는 지울 수 없다.

## 5. 생성자를 전부 다룬 뒤의 fallthrough (5)

- **`tojsvalue` (40299-40300)**: 명세 분기가 다루지 않는 값은 v128과 exnref뿐인데, 이 둘을
  막는 명세의 assert는 기계화되지 않아(`assert (yet …)`) 실제로는 검사하지 않는다. 대신
  호출하는 쪽(`Table.prototype.get`, `GetGlobalValue`, `Exception.prototype.getArg`, exported
  function 호출과 host function 실행)이 모두 v128/exnref를 먼저 TypeError로 거르므로
  (`index.bs` 1090, 1209, 1324, 1759) 이 끝에 닿는 값이 없다.
- **`string_value_of_the_extern_type` (39557-39558)**: function/table/memory/global/tag
  다섯 가지 extern type을 모두 확인한 뒤의 끝.
- **`getglobalvalue` (40018)**: globaltype의 모양 검사가 거짓인 경우. globaltype은
  생성자가 하나뿐인 튜플이라 늘 참이다.
- **지우려면**: lowering이 SpecTec syntax 정의를 참조해 생성자가 전부 다뤄졌는지 알아야
  한다. 노드 수에 비해 비용이 커서 하지 않았다.

## 6. 실패할 수 없는 호출에 붙은 암묵 전파 가드 (2)

명세 텍스트에는 `?`가 없지만, Infra의 "호출이 throw하면 다시 throw한다" 규칙에 따라
`PropagateUnguardedCallsPass`가 넣은 가드다.

- **`read_the_imports`의 `HasProperty(o, …)` 가드 (39310)**: `o`는 3번의 39307에서 본
  null-prototype 객체로, 데이터 프로퍼티만 있다. 그래서 `HasProperty`는 사용자 코드를
  부르지 않는다.
- **`HostResizeArrayBuffer`의 `Grow the memory buffer` 가드 (39783)**: resizable buffer의
  `maxByteLength`는 memory의 max에서 오고, ECMA-262의 `resize`가 그보다 큰 크기를
  HostResizeArrayBuffer 전에 RangeError로 거른다. 그래서 grow는 max 안에서만 일어나 실패하지
  않는다. 같은 `Grow the memory buffer`가 `Memory.prototype.grow`에서는 실제로 실패할 수
  있어서, 호출 지점과 무관한 규칙으로는 지울 수 없다.
