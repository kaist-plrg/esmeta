# 커버리지에서 남은 미방문 노드 (Uncovered Nodes)

공식 js-api 테스트와 `tests/wji/complementary/`를 모두 돌려도 한 번도 실행되지 않는
WJI IR 노드와, 각각이 남은 이유를 기록한다. 여기 있는 노드는 전부 테스트를 더 써서는
덮을 수 없다. 범위 밖이거나, 실행 경로상 도달할 수 없다.

- **측정**: `sbt --client "Test/runMain esmeta.wji.WjiCoverage --full"` (공식 js-api +
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
| 2. 앞단 검사가 이미 걸러냄 | 11 |
| 3. 값의 불변 조건이 보장함 | 6 |

분류는 "왜 실행될 수 없는가"를 기준으로 한다. 노드가 명세에 적힌 분기인지, 호출 결과의
`?` 가드인지(명세나 `SpecPatch`가 붙인 것), Infra의 암묵적 재throw 규칙으로
`PropagateUnguardedCallsPass`가 넣은 가드인지는 각 항목에 적는다.

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

## 2. 앞단 검사가 이미 걸러냄 (11)

앞선 단계의 검사나 변환이 이 노드에 이르는 경우를 이미 막는다.

- **`set:WebAssembly.Global.prototype.value`의 `global_write` ERROR → RangeError
  (39984-39986, 명세 분기)**: 앞에서 mut이 아닌 global을 이미 TypeError로 거르고
  (`index.bs:1226`), `ToWebAssemblyValue`가 값의 타입을 맞춘다. global의 타입은 바뀌지
  않으므로 사용자 `valueOf`가 끼어들어도 쓰기는 실패하지 않는다.
- **`WebAssembly.Table` 생성자의 "elementtype이 REF가 아님" → TypeError
  (39752-39754, 명세 분기)**: `TableDescriptor.element`가 IDL enum
  (`"externref"`/`"anyfunc"`)으로 먼저 검증되어 `ToValueType`이 늘 REF를 돌려준다.
- **`towebassemblyvalue`의 `? ToInt32(v)` 가드 2개 (40302, 40311, 명세의 `?`)**: 둘 다
  `v`가 Number임을 확인한 뒤의 호출이라 `ToNumber`가 항등이고 실패하지 않는다.
- **`HostResizeArrayBuffer`의 `Grow the memory buffer` 가드 (39731, 암묵 전파)**: resizable
  buffer의 `maxByteLength`는 memory의 max에서 오고, ECMA-262의 `resize`가 그보다 큰 크기를
  HostResizeArrayBuffer 전에 RangeError로 거른다. 그래서 grow는 max 안에서만 일어나 실패하지
  않는다. 같은 `Grow the memory buffer`가 `Memory.prototype.grow`에서는 실제로 실패할 수
  있어서, 호출 지점과 무관한 규칙으로는 지울 수 없다.
- **`tojsvalue`의 끝 fallthrough (40234-40235)**: 명세 분기가 다루지 않는 값은 v128과
  exnref뿐인데, 이 둘을 막는 명세의 assert는 기계화되지 않아(`assert (yet …)`) 실제로는
  검사하지 않는다. 대신 호출하는 쪽(`Table.prototype.get`, `GetGlobalValue`,
  `Exception.prototype.getArg`, exported function 호출과 host function 실행)이 모두
  v128/exnref를 먼저 TypeError로 거르므로 (`index.bs` 1090, 1209, 1324, 1759) 이 끝에 닿는
  값이 없다.

## 3. 값의 불변 조건이 보장함 (6)

검사 대상 값이 늘 만족하는 성질(타입 정의나 그 값을 만드는 곳에서 오는)이 이 노드에 이르는
경우를 없앤다.

- **`read_the_imports`의 "|o|가 Object가 아님" (39306, 명세 분기)과 `? HasProperty(o, …)`
  가드 (39309)**: `index.bs:500`의 "If |o| is not an Object or if [=?=] [$HasProperty$](|o|, …)
  is false". `?`는 `SpecPatch` #57로 붙이며(`docs/spec_errors.md` #29), 업스트림에도
  WebAssembly/spec#2256로 반영됐다. `o`는 늘 `instantiate a builtin set`/`instantiate
  imported strings`가 `OrdinaryObjectCreate(null)`로 만들고 데이터 프로퍼티만 채운 객체라,
  Object이고 Proxy일 수 없어 `HasProperty`가 사용자 코드를 부르지 않는다. `?` 대신 `!`가 더
  정확하지만, 업스트림에 막 머지된 표기라 그대로 둔다.
- **`towebassemblyvalue`의 objectkind가 array/struct 둘 다 아닌 경우 (40321, 명세 분기의
  else)**: 이 분기는 "|v| is an Exported GC Object"(`index.bs:1460`)일 때만 들어오고, WJI는
  이를 `[[ObjectAddress]]` 내부 슬롯이 있는지로 검사한다(`ExpandWjiIsTypePass`). 이 슬롯과
  `[[ObjectKind]]`를 가진 객체를 만드는 곳은 "a new Exported GC Object"(1653) 하나뿐이고, 그
  호출은 `ToJSValue`의 두 곳(1398-1399)뿐이며 둘 다 "struct"나 "array"를 넘긴다.
  `[[ObjectKind]]`는 생성 때(1655) 말고는 바뀌지 않고, JS 코드는 객체에 내부 슬롯을 붙일 수
  없다(같은 이름의 프로퍼티를 만들어도 슬롯이 되지 않는다). 그래서 이 분기에 이르는 값의
  objectkind는 늘 둘 중 하나다.
- **`string_value_of_the_extern_type`의 끝 fallthrough (39521-39522)**: extern type은 SpecTec
  정의상 function/table/memory/global/tag 다섯 가지뿐이라, 다섯 경우를 모두 확인한 뒤에 남는
  값이 없다.
- **`getglobalvalue`의 globaltype 모양 검사가 거짓인 경우 (39956)**: globaltype은 SpecTec
  정의상 생성자가 하나뿐인 튜플이라 이 검사는 늘 참이다.

  위 두 개를 지우려면 lowering이 SpecTec syntax 정의를 보고 생성자가 전부 다뤄졌는지 알아야
  한다. 노드 수에 비해 비용이 커서 하지 않았다.
