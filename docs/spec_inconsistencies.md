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
