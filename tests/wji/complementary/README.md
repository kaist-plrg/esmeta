# `tests/wji/complementary/` 테스트 케이스

`tests/wji/js-api/generated`(공식 wasm js-api conformance 코퍼스)가 전혀
건드리지 않는 WJI 스펙 경로를 손으로 메우는 디렉터리입니다. 무엇이 안 건드려지는지는
`esmeta.wji.WjiCoverage`(`sbt "Test/runMain esmeta.wji.WjiCoverage"`)로 측정 —
`logs/wji-coverage/zero-coverage-funcs`(0% 커버 함수 목록)와
`logs/wji-coverage/annotated-ir/<함수명>.ir`(함수별 CFG를 노드/분기 히트 카운트와
함께 보여주는 덤프)를 보고 타겟을 고릅니다.

`tests/wji/manual/`과 완전히 같은 컨벤션입니다 — `.js` 하나가 standalone
self-checking 테스트(`globalThis.__wjiOk = true`로 통과 표시, 자세한 건
`tests/wji/manual/README.md` 참고). 별도 디렉터리로 분리한 이유는 "WJI 자체
개발용 수작업 테스트"(manual)와 "공식 코퍼스의 커버리지 구멍을 메우는 수작업
테스트"(complementary)를 서로 다른 동기로 구분해서 관리하기 위함 — `EvalSpec`의
`roots`에 둘 다 등록돼 있어서 `wjiEvalTest`는 기존처럼 전체를 한 번에 돈다는 점은
동일합니다.

## 개발 루프

- 테스트 케이스 하나 작성/디버깅 중엔: `sbt run wji-eval
  tests/wji/complementary/<name>.js -silent` (`tests/wji/manual/README.md`가
  설명하는 것과 같은 단일 파일 루프).
- `complementary/` 전체만 돌려보고 싶을 때(`manual`/`js-api` 전부를 매번 다시
  돌리는 건 오래 걸림): `sbt "testOnly esmeta.wji.EvalSpec -- -Dverbose=true -z
  complementary"` — `EvalSpec`의 테스트 이름이 저장소 루트 기준 상대경로라서
  `-z complementary`로 부분 문자열 매칭하면 이 디렉터리 파일들만 걸러집니다.
- 전체 스위트(`sbt --client wjiEvalTest`)는 변경 사항을 다 모은 뒤 마지막에 한
  번만 돌리면 됩니다.

## `WjiCoverage`와의 관계

`WjiCoverage`는 의도적으로 `tests/wji/js-api/generated`만 측정 대상으로
삼습니다("공식 테스트의 스펙 커버리지"가 원래 목적) — 여기 추가하는 테스트는
그 측정 범위에 안 들어가므로, 여기서 어떤 gap을 메웠다고 해서 `WjiCoverage`의
퍼센트가 올라가지는 않습니다. 특정 gap이 실제로 메워졌는지 확인하려면 그
함수/분기를 겨냥한 `wji-eval -silent` 실행 결과(또는 필요하면 annotated-ir를
수동으로 다시 들여다보는 것)로 직접 확인하세요.
