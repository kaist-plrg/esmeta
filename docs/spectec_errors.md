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

  같은 파일(`xl/num.ml`)의 범용 산술/비교 연산자(`bin`/`cmp`/`un`)도 구조적으로 똑같은 문제를 갖고 있습니다 — `` `Nat 3 `` + `` `Int 5 `` 같은 타입이 섞인 연산은 그냥 `None`(실패)으로 떨어집니다. 이걸 위해 만들어진 것으로 보이는 `widen` 함수가 파일 안에 이미 있지만, 코드베이스 전체에서 실제로 호출하는 곳이 단 한 군데도 없어 사실상 죽은 코드입니다 — `bin`/`cmp`가 타입 불일치 시 `widen`을 먼저 태우도록 고치는 건 이번 수정보다 훨씬 큰 범위(embedding 경계뿐 아니라 Wasm 인터프리터의 일반 산술 전체에 영향)라 별도 작업으로 미뤘습니다.
