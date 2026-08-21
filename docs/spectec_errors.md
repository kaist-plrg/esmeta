# SpecTec Errors

`spectec` 서브모듈(스펙 텍스트가 아니라 `spectec/spectec/src/*`의 backend-interpreter/AL 등 OCaml 구현) 코드에서 발견한 버그 목록입니다. 스펙 텍스트 자체의 결함은 `docs/spec_errors.md`를 참고하세요.

## 1. `` `Int ``만 받고 `` `Nat ``은 거부하는 지점들이 `xl/num.ml`이 선언한 subtype 관계를 어김

- **File**:
  - `spectec/spectec/src/al/al_util.ml`, `unwrap_intv`
  - `spectec/spectec/src/backend-interpreter/numerics.ml`, `inv_signed`/`sat`
- **Current**:
  ```ocaml
  (* al_util.ml *)
  let unwrap_intv v =
    match unwrap_numv v with
    | `Int i -> i
    | n -> fail_value "unwrap_natv" (NumV n)  (* 에러 메시지도 복붙 실수로 "unwrap_natv" *)

  (* numerics.ml — inv_signed *)
  | [ NumV (`Nat z); NumV (`Int n) ] -> ...

  (* numerics.ml — sat *)
  | [ NumV (`Nat z); CaseV ("U", []); NumV (`Int i) ] -> ...
  | [ NumV (`Nat z); CaseV ("S", []); NumV (`Int i) ] -> ...
  ```
- **Expected**:
  ```ocaml
  (* al_util.ml *)
  let unwrap_intv v =
    match unwrap_numv v with
    | `Int i | `Nat i -> i
    | n -> fail_value "unwrap_intv" (NumV n)

  (* numerics.ml — inv_signed *)
  | [ NumV (`Nat z); NumV (`Nat n | `Int n) ] -> ...

  (* numerics.ml — sat *)
  | [ NumV (`Nat z); CaseV ("U", []); NumV (`Nat i | `Int i) ] -> ...
  | [ NumV (`Nat z); CaseV ("S", []); NumV (`Nat i | `Int i) ] -> ...
  ```
- **Reason**: `xl/num.ml`의 `sub` 함수(AL 자신의 숫자 타입 subtype 관계)는 `` `NatT, _ -> true ``로, `Nat`이 `Int`(뿐 아니라 `Rat`/`Real`도) 전부의 subtype이라고 명시적으로 선언합니다 — 같은 파일의 `cvt`도 `` `Nat n, `IntT -> Some (`Int n) ``로 이 승격을 이미 지원합니다. 하지만 위 지점들은 `NumV (\`Int n)` 형태의 정확한 태그 일치만 허용하는 naive pattern match라서, 이 subtype 관계를 무시하고 실제로는 항상 유효한 `` `Nat ``-태깅된 인자를 거부합니다(`nat`/`int` 둘 다 OCaml 표현이 `Z.t`로 동일해서, `n`을 non-negative로 강제하지 않는 이상 값 손실 없이 그대로 받아들일 수 있습니다).

  `table-mutation.js` 디버깅 중 실제로 크래시를 만난 건 이것과 **반대 방향**인 `table_read`(`` `Nat ``만 받고 `` `Int ``는 거부 — `docs/hardcodes.md`/커밋 이력 참고)였습니다. 그 크래시를 분석하면서 "esmeta 쪽 `toAL`(`Math(n) => ALValue.NumV(ALNum.Int(n.toBigInt))`, `state/util/ALValueConversion.scala`)이 새로 계산된 수학값을 항상 `` `Int ``로 태깅하니, 반대로 `` `Int ``만 받고 `` `Nat ``은 거부하는 지점도 있을 것"이라는 가설로 코드베이스를 훑다가 `unwrap_intv`/`inv_signed`/`sat`를 발견한 것으로, **이 지점들 자체가 실제로 크래시하는 걸 실행 중에 관찰한 적은 없습니다** — `AddressValueToU64`가 계산한 값은 `inv_signed`로 흘러가지 않고 `table_read`로 흘러가므로 서로 다른 호출 경로입니다. 정적 코드 검사로 발견한, subtype 관계를 어기는 별개의 잠재적 버그로 예방적으로 고친 것입니다.

  같은 파일(`xl/num.ml`)의 범용 산술/비교 연산자(`bin`/`cmp`/`un`)도 구조적으로 똑같은 문제를 갖고 있었습니다 — `` `Nat 3 `` + `` `Int 5 `` 같은 타입이 섞인 연산은 그냥 `None`(실패)으로 떨어졌습니다. 처음엔 이 부분을 "embedding 경계뿐 아니라 Wasm 인터프리터의 일반 산술 전체에 영향을 미치는, 훨씬 큰 범위"라는 이유로 별도 작업으로 미뤘는데, 바로 다음(#2)에서 실제로 고쳤습니다 — 미뤘던 이유("Wasm 실행 내부는 esmeta의 `toAL`을 안 거치니 안전할 것")가 실은 틀렸다는 게 곧바로 실증되었기 때문입니다.

## 3. `WasmContext.is_value`의 `REF.`-prefix 체크가 아직 안 풀린 ref 생성 명령어를 이미 값인 것처럼 오판

- **File**: `spectec/spectec/src/backend-interpreter/ds.ml`, `WasmContext.is_value`
- **Current**:
  ```ocaml
  let is_value = function
    | CaseV ("CONST", _) -> true
    | CaseV ("VCONST", _) -> true
    | CaseV (ref, _)
      when String.starts_with ~prefix:"REF." ref -> true
    | _ -> false
  ```
- **Expected**:
  ```ocaml
  let is_value = function
    | CaseV ("CONST", _)
    | CaseV ("VCONST", _)
    | CaseV ("REF.NULL_ADDR", _)
    | CaseV ("REF.I31_NUM", _)
    | CaseV ("REF.STRUCT_ADDR", _)
    | CaseV ("REF.ARRAY_ADDR", _)
    | CaseV ("REF.FUNC_ADDR", _)
    | CaseV ("REF.EXN_ADDR", _)
    | CaseV ("REF.HOST_ADDR", _)
    | CaseV ("REF.EXTERN", _) -> true
    | _ -> false
  ```
- **Reason**: `construct.ml`의 `al_of_ref`/`al_to_ref`를 보면 ref 관련 `CaseV` 태그는 **명령어(아직 안 풀린 구문)**와 **값(resolve된 런타임 레퍼런스)**이 항상 짝을 이룹니다 — `REF.FUNC`(`ref.func $idx` 명령어, 모듈 상대 함수 인덱스)/`REF.FUNC_ADDR`(resolve된 funcaddr), `REF.STRUCT`/`REF.STRUCT_ADDR`, `REF.ARRAY`/`REF.ARRAY_ADDR`, `REF.I31`/`REF.I31_NUM`, `REF.EXN`/`REF.EXN_ADDR`, `REF.HOST`/`REF.HOST_ADDR`(6쌍 전부 `NormalizeSpecTecCaseShapePass.scala`의 `RenamedTag` 매핑과 정확히 일치). `is_value`의 `String.starts_with ~prefix:"REF."` 체크는 이 둘을 전혀 구분 못 하고 — 명령어 태그도 값 태그도 똑같이 `"REF."`로 시작하니까요 — 아직 reduce가 안 된 `ref.func $idx` 같은 명령어를 이미 값인 것처럼 판단해 `step_wasm`이 `create_context`(실제 reduction 실행)를 안 태우고 그냥 값 스택에 그대로 push해버립니다. 결과적으로 `4.3-execution.instructions.spectec`의 진짜 reduction rule(`z; (REF.FUNC x) ~> (REF.FUNC_ADDR $moduleinst(z).FUNCS[x])`)이 아예 실행되지 않아, active element segment로 채운 테이블 슬롯이 resolve 안 된 `REF.FUNC`(인덱스) 상태로 store에 그대로 남습니다.
  `table-mutation.js`의 `table.get(0)()`가 `typeof`의 최종 `assert (? val: Record[Object])`에서 크래시하는 걸로 발견 — `table_read`가 실제로 리턴한 값을 찍어보니 `CaseV(REF.FUNC, [NumV(Nat(1))])`였고(진짜 `REF.FUNC_ADDR`이 아님), wasm 자신의 `call_indirect`(embedding 레이어를 안 거침)로 접근해도 SpecTec 자신의 `Assert (case(val') == REF.FUNC_ADDR)`가 실패하는 걸로 재확인. `git bisect`(로컬 `official` 브랜치 — 실제 업스트림 `WebAssembly/spec`의 `main` — 기준, spectec 자신의 `--interpreter` 모드로 최소 `.wast` 재현 스크립트를 오라클 삼아 자동 실행)로 정확한 도입 커밋을 특정: `3f199fb74f25b0e3da20a0aad5559a91061b873b`("Give call_ref-host a real thrown-exception outcome")이 `step_wasm`의 `CaseV ("CONST", _) | CaseV ("VCONST", _)`(그리고 버전<=2의 `REF.NULL` 특수 케이스)만 값으로 인정하던 좁은 매칭을, 런타임에 계산된 `$callhostfunc`의 결과 instr*도 커버하려고 `WasmContext.is_value` 기반의 범용 체크로 넓히면서 이 prefix 체크의 부정확함이 처음으로 실제 실행 경로에 노출된 것으로 보임(그 전엔 `il2al/translate.ml`의 컴파일 타임 `is_wasm_value`/`is_wasm_instr` 분리가 항상 먼저 걸러줘서 `step_wasm`의 이 런타임 fallback 자체가 CONST/VCONST 말고는 실행될 일이 없었음).

## 2. `xl/num.ml`의 `bin`/`cmp`가 타입이 섞인 피연산자(`` `Nat ``/`` `Int ``)를 처리 못 함 — 이미 있던 `widen`이 죽은 코드였음

- **File**: `spectec/spectec/src/xl/num.ml`, `bin`/`cmp`
- **Current**:
  ```ocaml
  let rec bin (op : binop) num1 num2 : num option =
    ...
    match op, num1, num2 with
    | `AddOp, `Nat n1, `Nat n2 -> Some (`Nat Z.(n1 + n2))
    | `AddOp, `Int i1, `Int i2 -> Some (`Int Z.(i1 + i2))
    ...
    | _, _, _ -> None

  let cmp (op : cmpop) num1 num2 : bool option =
    ...
    match op, num1, num2 with
    | `LtOp, `Nat n1, `Nat n2 -> Some (n1 < n2)
    | `LtOp, `Int i1, `Int i2 -> Some (i1 < i2)
    ...
    | _, _, _ -> None
  ```
- **Expected**: 타입이 다르면 `widen`(같은 파일에 이미 정의돼 있던, `sub`의 subtype 순서 — Nat < Int < Rat < Real — 대로 작은 쪽을 큰 쪽에 맞춰 승격하는 함수)으로 한 번 맞춘 뒤 재시도:
  ```ocaml
  let rec bin (op : binop) num1 num2 : num option =
    ...
    | _, _, _ when to_typ num1 <> to_typ num2 ->
      let num1', num2' = widen num1 num2 in
      bin op num1' num2'
    | _, _, _ -> None

  let rec cmp (op : cmpop) num1 num2 : bool option =
    ...
    | _, _, _ when to_typ num1 <> to_typ num2 ->
      let num1', num2' = widen num1 num2 in
      cmp op num1' num2'
    | _, _, _ -> None
  ```
  (`widen`은 타입이 이미 같으면 그대로 반환하므로, `to_typ num1 <> to_typ num2`로 진짜 타입 불일치일 때만 타도록 guard해야 무한 재귀를 피할 수 있습니다 — 타입이 같은데 그 연산 조합 자체가 정의 안 된 경우는 이 guard에 안 걸리고 바로 `None`으로 떨어집니다.)
- **Reason**: #1과 같은 근본 원인 — `sub`가 선언한 subtype 관계를 `bin`/`cmp`가 안 지킴 — 인데, 이번엔 esmeta 쪽 `toAL`(WJI가 값을 embedding 경계로 넘길 때 쓰는 변환 함수)이 non-negative `Math` 값을 `` `Nat ``으로 태깅하도록 고쳐보다가 직접 실증됨. `tests/wji`의 5개 테스트가 `$inv_signed_: ... comparison operation <= not defined for +0, 123`류의 에러로 깨졌습니다. 원인을 추적해보니 `signed_31`/`inv_signed_31` 등(`server.ml`의 `call_signed`/`call_inv_signed`)은 `numerics.ml`의 OCaml shortcut(`unwrap_intv`/`inv_signed`/`sat`, #1에서 고친 바로 그 함수들)을 안 거치고 있었습니다 — `call_inv_signed`가 `Interpreter.call_func "inv_signed_"`(끝에 `_`)로 찾는데 `numerics.ml`엔 `"inv_signed"`(언더스코어 없음)로 등록돼 있어 이름이 안 맞았기 때문입니다. 그래서 매번 공식 `.spectec` 정의를 일반 AL 인터프리터로 해석해왔고, 그 정의 안의 `$int$(0) <= i`(리터럴 `int` 상수)가 이제 `` `Nat ``으로 넘어온 인자 `i`와 비교되면서 `cmp`의 same-type-only 제약에 걸린 것입니다. 즉 "Wasm 실행 내부는 esmeta의 `toAL`을 거치지 않는다"는 #1의 가정이 이 경로(공식 spec 정의의 제너릭 해석)에는 안 맞았던 것으로 드러났습니다.

## 4. `mem_grow`가 `Ds.Store`를 안 거쳐서 growth가 `mem_read_bytes`엔 안 보임 + `growmem`의 partial-match 실패를 `Exception.Fail`로 놓침

- **File**: `spectec/spectec/src/backend-interpreter/embedding.ml`, `mem_grow`
- **Current**:
  ```ocaml
  let mem_grow (store : value) (memaddr : value) (n : value) : value =
    match memaddr with
    | NumV (`Nat i) ->
      let mems = strv_access "MEMS" store in
      let mi = listv_nth mems (Z.to_int i) in
      (match Interpreter.call_func "growmem" [ mi; n ] with
       | Some mi' ->
         (match mems with
          | ListV arr_ref -> Array.set !arr_ref (Z.to_int i) mi'; store
          | _ -> failwith "mem_grow: unexpected MEMS shape")
       | None -> embedding_error)
    | _ -> failwith "mem_grow: expected nat memaddr"
  ```
- **Expected**:
  ```ocaml
  let mem_grow (store : value) (memaddr : value) (n : value) : value =
    Ds.Store.set store; (* install the caller's store as the global store *)
    match memaddr with
    | NumV (`Nat i) ->
      let mems = strv_access "MEMS" (Ds.Store.get ()) in
      let mi = listv_nth mems (Z.to_int i) in
      let result =
        try Interpreter.call_func "growmem" [ mi; n ]
        with Exception.Fail -> None
      in
      (match result with
       | Some mi' ->
         (match mems with
          | ListV arr_ref -> Array.set !arr_ref (Z.to_int i) mi'; Ds.Store.get ()
          | _ -> failwith "mem_grow: unexpected MEMS shape")
       | None -> embedding_error)
    | _ -> failwith "mem_grow: expected nat memaddr"
  ```
- **Reason**: 이 파일엔 "호출자가 넘긴 `store` 값을 전역 `Ds.Store`에 설치하고(`Ds.Store.set`), 작업한 뒤, `Ds.Store.get()`으로 최종 상태를 다시 읽는다"는 일관된 관례가 있습니다(`func_alloc`/`table_alloc`/`func_invoke`/`module_instantiate`/`tag_alloc`/`exn_alloc` 전부). `mem_grow`만 이 관례를 안 따르고, 인자로 받은 `store` 값의 `MEMS` 배열을 in-place로 고쳐서 그 값 자체를 그대로 리턴했습니다 — RPC 응답 자체(ESMeta가 자기 `@AGENT_RECORD["associated store"]`로 계속 추적하는 값)로는 정확했지만, `mem_read_bytes`(`memaddr`만 받는 "implicit-store" 함수 — 항상 전역 `Ds.Store`를 읽음)가 읽는 별도의 전역 상태는 전혀 안 건드려서, 실제로 얼마나 grow했든 상관없이 `Memory` 객체의 buffer가 항상 할당 시점 크기로 얼어붙어 있었습니다.

  두 번째로, `growmem`은 `hint(partial)` 함수(4.0-execution.configurations.spectec)인데, il2al의 `Partial` 정의 코드젠(`il2al/translate.ml`, `append_fail_block`)이 "매칭되는 equation 없음"을 `FailI` 명령어로 표현하고, 인터프리터는 이걸 `Exception.Fail`이라는 **진짜 예외**로 던집니다 — `Interpreter.call_func`가 조용히 `None`을 리턴하는 게 아닙니다. `mem_grow`의 `| None -> embedding_error` 처리(그리고 완전히 동일한 패턴인 `table_grow`/`growtable`도)는 이 경우를 놓쳐서, 선언된 max를 초과해 growing하는(스펙상 legit한 실패 → `RangeError`로 이어져야 하는) 시나리오에서 서버 프로세스 자체가 `ProtocolError`로 죽었습니다.
- **발견 경위**: `tests/wji/manual/memory-grow.js`(새로 작성한 WJI 테스트) 디버깅 중 발견 — `Memory.prototype.grow`가 크래시 없이 실행은 되는데 `memory.buffer.byteLength`가 growth 후에도 그대로였음. `Ds.Store.set`/`get()` bracketing 문제로 첫 번째 원인을 고친 뒤, 정상적으로 max 초과 growth를 테스트하는 부분에서 두 번째 문제(`Exception.Fail`)가 새로 드러남.
- **`table_grow`도 동일**: `growtable`(`4.0-execution.configurations.spectec:318`)도 나란히 선언된 `hint(partial)` 함수라 `table_grow`가 완전히 같은 `Exception.Fail` 미포착 버그를 갖고 있었음 — `tests/wji/manual/table-grow.js`로 실제 재현/확인 후 같은 방식(`try ... with Exception.Fail -> None`)으로 고침. 단 `Ds.Store` bracketing은 `table_grow`엔 적용 안 함 — `mem_read_bytes`처럼 `Ds.Store`만 읽는 implicit-store table reader가 없어서(모든 table 읽기가 `store`를 명시적으로 받음), 인자로 받은 `store` 값만 in-place로 고쳐 리턴하는 기존 방식 그대로도 이미 정확함.
