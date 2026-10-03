# ESMeta Changes

`docs/esmeta_errors.md`와 달리, 여기는 ESMeta mainline이 **의도적으로** 하고
있던 스코프 제한(버그가 아니라 설계/우선순위상의 선택으로 보이는 것)을 WJI
쪽 필요에 의해 넓힌 변경들을 기록합니다.

## 1. `emu-annex` 밑 알고리즘은 추출 대상에서 제외되어 있었음

- **File**: `src/main/scala/esmeta/extractor/Extractor.scala`, `extractHeads`
- **Before**: `if (parent.tagName != "emu-clause") return Nil` — 알고리즘의
  바로 위 부모가 `emu-clause`가 아니면 무조건 스킵.
- **After**: `emu-clause`뿐 아니라 `emu-annex`도 허용.
- **영향 범위**: ecma262/spec.html에는 `emu-annex`가 81개 있고, 그 중
  "Annex B: Additional ECMAScript Features for Web Browsers" 하위의
  `escape`/`unescape`, `String.prototype.substr`/`anchor`/`big`/`blink`/
  `bold`/`fixed`/`fontcolor`/`fontsize`/`italics`/`link`/`small`/`strike`/
  `sub`/`sup`, `Date.prototype.getYear`/`setYear`/`toGMTString`,
  `RegExp.prototype.compile`, 그리고 `FunctionDeclarationInstantiation` 등의
  web-compat 재정의 알고리즘들이 전부 여기 걸려서 지금까지 아예
  mechanize된 적이 없었다. 이 변경 하나로 49개 알고리즘이 새로 추출됨
  (`algorithms: 2870 → 2919`, `spec-summary` 참고).
- **왜 "버그"가 아니라 "변경"으로 분류했는가**: Annex B는 스펙 자체가
  "normative optional"(구현체가 골라서 지원해도 되는) 내용이라고 명시하는
  섹션이고, `emu-clause`/`emu-annex`를 애초부터 구분해서 다르게 다룬 흔적
  (`isInAnnex`가 grammar production 쪽엔 이미 존재)도 있어서, 단순 누락이
  아니라 "본체 스펙(emu-clause)만 우선 mechanize한다"는 의도적 스코프
  결정이었을 가능성이 높다고 판단함. 다만 그 스코프 결정이 어디에도
  명시적으로 문서화돼 있지 않았고, WJI의 공식 js-api 테스트 corpus
  (`wasm-module-builder.js`)가 `unescape(encodeURIComponent(...))`라는
  흔한 UTF-8 인코딩 관용구를 통해 Annex B의 `unescape`를 실제로 필요로
  해서 이번에 범위를 넓혔다.
- **검증**: 새로 뚫린 49개 알고리즘 각각이 실제로 뭔가를 깨뜨리지는
  않는지 `sbt test`(525개) 전체 통과로 확인 — 유일한 diff는 CFG
  fingerprint 골든(함수 개수가 늘었으니 당연히 바뀜) 재생성뿐이었음.

## 2. `escape`/`unescape`가 실제 global 프로퍼티로 연결되지 않고 있었음

- **File**: `src/main/scala/esmeta/es/builtin/GlobalObject.scala`,
  `GlobalObject.map`
- **Before**: `spec.tables(WELL_KNOWN_INTRINSICS).rows`(메인 72행짜리
  `table-well-known-intrinsic-objects`)만 읽어서 global 프로퍼티 맵을
  구성.
- **After**: `table-additional-well-known-intrinsic-objects`(Annex B
  "Additional Properties of the Global Object" 섹션의 별도 2행짜리
  테이블, `%escape%`/`%unescape%` → `` `escape` ``/`` `unescape` `` 매핑)
  도 합쳐서 구성.
- **왜 "버그"가 아니라 "변경"으로 분류했는가**: 위 1번과 같은 맥락 —
  Annex B의 별도 테이블 자체가 메인 테이블과 분리돼 있는 건 스펙
  구조상 자연스러운 선택이고, `GlobalObject.scala`가 메인 테이블만 읽는
  것도 "Annex B는 일단 범위 밖" 결정의 연장선으로 보임. 스펙 본문은
  "The entries in [additional table] are added to [main table]"이라고
  명시하므로, 위 1번으로 Annex B 알고리즘 자체를 mechanize하기로 한
  이상 이 병합은 그 결정을 실행하는 데 필요한 자연스러운 후속 조치.

## 3. `"the ASCII word characters"` dfn 고정 문구가 리터럴로 파싱되지 않았음

- **File**: `src/main/scala/esmeta/lang/{Expression,util/Parser,util/Stringifier,util/CaseCollector}.scala`
- **Before**: `StringLiteralForm`에 `SyntaxLiteral`/`EmptyString`/
  `EmptyUnicode`/`Code`만 있고, ecma262/spec.html:1310의
  `<dfn id="ASCII-word-characters">the ASCII word characters</dfn>`
  (Basic Latin 블록의 모든 글자/숫자 + `_`로 정의된 63자 고정 문자열)를
  가리키는 형태가 없었다. `literal` 파서가 이 문구를 못 삼켜서, 이 문구를
  쓰는 3곳(`Encode`의 `_alwaysUnescaped_`, Annex B `escape`의
  `_unescapedSet_`, Annex B `WordCharacters`의 `_basicWordChars_`)이 전부
  해당 스텝에서 `[NotSupported] metalanguage/...`로 막혀 있었다.
- **After**: `StringLiteralForm.AsciiWordChars` 케이스를 추가하고,
  `"the empty String"` 처리 바로 옆에 `"the ASCII word characters" ^^!
  StringLiteral("ABC...xyz0123456789_", AsciiWordChars)`를 추가 —
  "the empty String" 등 기존 dfn 고정 문구 리터럴들과 완전히 같은 패턴.
  `Stringifier`/`CaseCollector`의 대응 `match`에도 케이스 추가(둘 다
  `AsciiWordChars`가 없으면 exhaustivity 경고/에러가 나므로 필수).
  컴파일러(`Compiler.scala:839`, `case StringLiteral(s, _) => EStr(s)`)는
  `form`을 안 보고 `s`만 쓰므로 추가 작업 불필요.
- **왜 "esmeta_changes"에 적었는가**: 엄밀히는 `docs/esmeta_errors.md`의
  CondParser 항목(#3)과 같은 성격의 "단순 누락된 문법 규칙" 쪽에 더
  가깝다. 다만 이 gap이 실질적으로 의미를 가지려면 1/2번으로 Annex B
  자체를 mechanize하기로 한 결정이 먼저 있어야 했으므로(순수 mainline
  본체(emu-clause)만 보면 `Encode`/`escape` 경로가 애초에 도달 불가능한
  코드였음), 같은 Annex B 작업 묶음으로 여기 함께 기록.
- **검증**: `sbt test`(525개) 전체 통과. `spec-summary`의 algorithm
  steps complete가 21684 → 21686으로 반영(해당 phrase를 쓰는 3곳 중
  `Encode`/`escape`의 해당 스텝만 완전해짐 — `WordCharacters`의 사용처는
  "the CharSet containing every character in X"라는 또 다른 별개
  wrapping 표현에 걸려 있어서 여전히 미완성으로 남음).

## 4. "code point"가 값 타입으로 아예 모델링돼 있지 않았음

- **File**: `state/Value.scala`(`CodePoint` 케이스 클래스), `lang/{Expression,util/Parser,util/Stringifier,util/CaseCollector}.scala`(`ConversionExpressionOperator.ToCodePoint` + `PredicateConditionOperator.{LeadingSurrogate,TrailingSurrogate}`), `ir/Op.scala` + `ir/util/{Parser,Stringifier}.scala`(`COp.ToCodePoint`), `compiler/Compiler.scala`, `interpreter/Interpreter.scala`, `state/util/{Stringifier,UnitWalker}.scala`, `state/State.scala`, `ty/{ValueTy,package}.scala` + `ty/util/{Walker,UnitWalker,Stringifier,Parser}.scala`(타입 lattice에 `codePoint: Boolean` 필드 — `codeUnit`과 동일한 자리마다 총 18곳).
- **Before**: `CodeUnit(c: Char)`는 있는데 그 형제 개념인 code point는
  `esmeta.state.Value`에도, `esmeta.ty.ValueTy`의 타입 lattice에도 아예
  없었음. `"the code point whose numeric value is X"`(`ecma262/
  spec.html`에 5곳: `CodePointAt`, `Hex4Digits`/`CodePoint`
  static semantics, `RegExpUnicodeEscapeSequence`, 그리고 line 38070)와
  `"is a leading/trailing surrogate"`(`CodePointAt`에 3곳,
  `UTF16SurrogatePairToCodePoint`의 Assert 1곳)가 전부 `[NotSupported]
  metalanguage/...`로 막혀 있었음 — `String.prototype.codePointAt`처럼
  UTF-16 surrogate pair를 다루는 경로 전체가 도달 불가능했음.
- **After**: `CodeUnit`을 그대로 미러링해서 `CodePoint(cp: Int)`(surrogate
  pair 디코딩 결과가 0x10FFFF까지 가서 `Char`가 아니라 `Int`)를 추가하고,
  `ConversionExpressionOperator.ToCodePoint`(`"code point whose numeric
  value is X"` → `EConvert(COp.ToCodePoint, ...)`)와
  `PredicateConditionOperator.{LeadingSurrogate,TrailingSurrogate}`(범위
  체크 `Interpreter`가 아니라 `Compiler`가 `0xD800-0xDBFF`/
  `0xDC00-0xDFFF` inclusive-interval 비교로 직접 컴파일)를 추가. 이 둘로
  `CodePointAt`의 대부분 스텝이 풀렸지만, 나머지 두 gap은 이 패턴을
  일반화하는 대신 기존 하드코딩 메커니즘으로 우회:
  - `"_first_ is neither a leading surrogate nor a trailing surrogate"` —
    `PredicateCondition`이 단일 `op` 하나만 갖는 구조라 "neither/nor"(복수
    predicate 결합)를 표현 못 함. `TypeCheckCondition`처럼 `List[Op]`로
    구조를 바꾸는 대신, `manuals/rule.json`의 "expr" 맵에 한 줄
    추가(`"_base_ is finite and is neither +0 nor -0"`과 완전히 같은
    선례) — `PredicateCondition`/`Walker`/`Stringifier`/`CaseCollector`/
    테스트 등 6개 파일을 건드리는 구조 변경 없이 해결.
  - `UTF16SurrogatePairToCodePoint`의 `"(_lead_ - 0xD800) × 0x400 + ..."`
    — 코드유닛을 "the numeric value of" 같은 명시적 변환 없이 바로
    산술식에 씀(ECMA-262 관행). `Interpreter.eval(bop, ...)`에
    `(CodeUnit, Math)` 조합을 일반적으로 추가하는 대신, 이 조합을 쓰는
    알고리즘이 스펙 전체에 이거 하나뿐이라(code-unit 타입 파라미터를
    가진 알고리즘 3개 중 나머지 둘은 산술을 안 하거나 이미 명시
    변환함) `manuals/funcs/UTF16SurrogatePairToCodePoint.ir`로 알고리즘
    전체를 손으로 대체 — `StringToCodePoints.ir`와 같은 선례.
- **왜 "esmeta_changes"로 분류했는가**: "code point"는 ECMA-262가 String을
  UTF-16으로, 그 String을 다시 Unicode code point 시퀀스로 해석할 때 쓰는
  자기 자신의 핵심 개념(`sec-ecmascript-language-types-string-type`)인데,
  mainline이 이걸 별도 값 타입으로 모델링 안 하기로 한 건 버그라기보다
  "code point 단위 문자열 처리가 필요한 빌트인은 `trimString`처럼 그때그때
  네이티브 Scala로 우회한다"는 기존 전략(1/2/3번과 같은 맥락)의 연장으로
  보임 — 다만 `codePointAt()`처럼 스펙 알고리즘을 그대로 실행해야 하는
  경로는 지금까지 아무도 손 안 댄 채 남아있었음.
- **검증**: `sbt test`(528개, CFG fingerprint 골든 재생성 필요) 전체 통과,
  `wjiEvalTest`(30 succeeded) 회귀 없음, `"😀".codePointAt(0) === 128512`
  직접 확인. `spec-summary`의 algorithm steps complete가 21686 → 21695로
  반영, `complete-funcs`에 `CodePointAt`/`UTF16SurrogatePairToCodePoint`
  외에 `RegExpIdentifierPart`/`RegExpIdentifierStart`/
  `UnicodeEscapeSequence`의 `IdentifierCodePoint` 관련 SDO 4개도 부수적으로
  완전해짐(같은 phrase를 공유). `tycheck-ignore.json`에
  `RegExpUnicodeEscapeSequence[0,0].CharacterValue` 한 항목 추가(새로
  도달 가능해진 기존 타입 느슨함, 이번 변경이 만든 버그 아님).

## 5. "UTF-8 변환 적용"과 "16진수/10진수로 포맷된 문자열 표현" 둘 다 파싱 규칙이 없었음

- **File**: `manuals/funcs/__UTF8_ENCODE__.ir`(신규, 스펙에 없는 이름의
  헬퍼), `manuals/rule.json`("inst" 맵), `lang/Expression.scala`
  (`NumberToStringExpression`), `lang/util/{Parser,Stringifier,
  CaseCollector,Walker,UnitWalker,JsonProtocol}.scala`,
  `compiler/Compiler.scala`, `ir/Op.scala`(`COp.ToStr`에 `upper: Boolean`
  필드 추가) + `ir/util/{Parser,Stringifier,Walker,UnitWalker}.scala`,
  `interpreter/Interpreter.scala`, `analyzer/tychecker/{AbsTransfer,
  AbsValue}.scala`(`COp.ToStr` 필드 추가로 인한 패턴 매치 arity 수정).
- **Before**:
  - `Encode`(`encodeURIComponent`/`encodeURI`가 씀)의 `"Let _Octets_ be the
    List of octets resulting by applying the UTF-8 transformation to
    _cp_.[[CodePoint]]."` 스텝 — 문장 전체가 파싱 실패(`YetStep`)라
    `codePointAt()`류와 달리 `rule.json`의 "expr"가 아니라 "inst" 맵이
    필요한 케이스.
  - `"the String representation of X, formatted as a[n] [lowercase/
    uppercase] decimal/hexadecimal number"` — `ecma262/spec.html`에 5곳
    (`Encode`의 octet→hex 3곳, `UnicodeEscape`의 lowercase hex 1곳 — 4번
    항목에서 "여전히 미완성으로 남음"이라고 적어뒀던 바로 그 자리, 그 외
    decimal 1곳)에서 반복되는데 파싱 규칙 자체가 없었음. `COp.ToStr`
    (`esmeta.ir.Op`)는 이미 진수 변환을 하지만 대소문자 개념이 없었고,
    `String.prototype.toUpperCase` 자체도 mainline에 `yet` 스텁이라 기댈
    기존 빌딩 블록이 없었음.
- **After**:
  - `__UTF8_ENCODE__`(code point → UTF-8 octet List, 표준 1~4바이트
    인코딩을 IR 산술/비트 연산으로 직접 구현)를 새로 만들고,
    `rule.json`의 "inst" 맵에 위 스텝 전체 텍스트를 이 헬퍼 호출로
    매핑 — `WJI`의 `__NEW_ERROR_OBJ__`처럼 스펙에 없는 이름의 헬퍼를
    새로 만들어 쓰는 것과 같은 선례.
  - `"String representation of X, formatted as ..."`는 5곳 반복이라
    `NumberToStringExpression`(`radix: Int`, `upper: Boolean`)이라는
    진짜 `Expression` AST 노드를 새로 추가 — `ConversionExpression`
    옆에 나란히, `CalcExpression`을 상속. `COp.ToStr`에도 `upper: Boolean`
    필드를 추가(default `false`라 기존 두 manual `.ir`
    `Number::toString.ir`/`BigInt::toString.ir`의 `[str]`/`[str radix]`
    호출은 그대로 유효)하고, `Interpreter`의 `Number`/`BigInt`/`Math` 세
    타입 각각의 `ToStr` case에서 `upper`면 `.toUpperCase` — `Math` 타입은
    기존에 `ToStr` 자체가 없어서(옥텟 값이 `Math`로 흘러들어옴) 이 김에
    같이 추가.
- **왜 "esmeta_changes"로 분류했는가**: 4번 항목과 같은 맥락 — UTF-8
  인코딩과 16진수 포맷팅은 ECMA-262 자기 자신의 `Encode`/`Decode`,
  `UnicodeEscape`, `Quote` 같은 여러 알고리즘이 공통으로 쓰는 관용구인데
  mainline이 이걸 문법 규칙으로도, 값 타입으로도 모델링 안 하기로 한 건
  "이 정도 문자열 포맷팅은 필요할 때마다 네이티브로 우회한다"는 기존
  전략(1~4번과 동일 맥락)의 연장으로 보임.
- **검증**: `sbt test`(528개, CFG fingerprint 골든 재생성, 연속 2회 안정
  확인) 전체 통과, `wjiEvalTest`(30 succeeded) 회귀 없음.
  `encodeURIComponent("😀")` === `"%F0%9F%98%80"`,
  `encodeURIComponent("hello world!")` === `"hello%20world!"`,
  `encodeURIComponent("é")` === `"%C3%A9"` 전부 실제 JS와 동일한 결과
  직접 확인. `complete-funcs`에 `Encode` 외에도 `INTRINSICS.escape`(1/2번
  항목에서 필요하다고 적어뒀던 Annex B 함수), `UnicodeEscape`(4번에서
  미완성으로 남겨뒀던 것), `ToZeroPaddedDecimalString`까지 부수적으로
  완전해짐 — `spec-summary`의 algorithm steps complete가 21695 → 21700,
  algorithms complete가 2541 → 2544로 반영.

## 6. `String.prototype.repeat`/`StringPad`의 "N번 반복해서 이어붙이기" 단계도 파싱 규칙이 없었음

- **File**: `manuals/rule.json`("inst" 맵) 두 줄 추가. 새 헬퍼/`Expression`
  노드/컴파일러 변경은 전혀 없음.
- **Before**:
  - `String.prototype.repeat`의 마지막 스텝 — `"Return the String value
    that is made from _n_ copies of _S_ appended together."`(`ecma262/
    spec.html:35841`) — 앞의 4개 스텝(`RequireObjectCoercible`/`ToString`/
    `ToIntegerOrInfinity`/range 체크)은 정상 컴파일되는데 이 한 문장만
    파싱 실패. "made from N copies of ... appended together"(정적
    피연산자 목록이 아니라 *동적 반복 횟수*)를 아는 규칙이 `esmeta.lang`에
    없었고, 가장 비슷한 기존 노드 `StringConcatExpression`("the
    string-concatenation of A, B, and C")은 정적으로 나열된 피연산자만
    다뤄서 이 문장엔 안 맞음. `ecma262/spec.html` 전체에서 이 정확한
    관용구는 이 한 곳뿐(재발하지 않는 singleton phrasing).
  - `StringPad`(`padStart`/`padEnd`가 쓰는 공용 헬퍼, `Encode`의 16진수
    자릿수 패딩에도 재사용됨)의 5번째 스텝 — `"Let _truncatedStringFiller_
    be the String value consisting of repeated concatenations of
    _fillString_ truncated to length _fillLen_."`(`ecma262/
    spec.html:35804`) — 위와 같은 "N번 반복" 관용구에 "정확히 안 맞아도
    잘라낸다"(truncate)는 조건까지 붙어서 더 복잡한 변형. 이것도 파싱
    규칙이 없어서 `js-string/constants.any.js`(WJI js-api 테스트)가
    `encodeURIComponent("'")`를 평가하다 처음 도달.
- **After**: 둘 다 `esmeta.lang` 문법을 확장하는 대신(재발 가능성이 낮은
  singleton/거의-singleton 문구라 5번 항목의 `NumberToStringExpression`급
  진짜 AST 노드를 새로 만들 정도는 아니라고 판단), `manuals/rule.json`의
  "inst" 맵에 스펙 문장 원문을 키로, 손으로 짠 IR을 값으로 추가:
  ```
  "Return the String value that is made from _n_ copies of _S_ appended together.":
    "{ let result = \"\" let base = S let count = n while (< 0 count) { if (= (% count 2) 1) { result = (concat result base) } base = (concat base base) count = (floor (/ count 2)) } return result }"
  "Let _truncatedStringFiller_ be the String value consisting of repeated concatenations of _fillString_ truncated to length _fillLen_.":
    "{ let truncatedStringFiller = \"\" while (< (sizeof truncatedStringFiller) fillLen) { truncatedStringFiller = (concat truncatedStringFiller fillString) } truncatedStringFiller = (substring truncatedStringFiller 0 fillLen) }"
  ```
  `concat`은 `StringConcatExpression`이 컴파일되는 것과 같은
  `EVariadic(VOp.Concat, ...)` 연산자의 IR 텍스트 표기(정적 리스트가
  아니라 매 반복마다 누적하는 루프로 "동적 횟수"를 우회) — `sizeof`는
  `Str`에 대해서도 이미 길이를 반환하고(`Interpreter.scala`의 `ESizeOf`),
  `substring`도 이미 있는 IR 연산이라(`to`가 실제 길이를 넘으면 자동으로
  clamp — `Interpreter.scala`의 `ESubstring`) 둘 다 새 IR primitive 없이
  기존 것만으로 조립됨.
  `String.prototype.repeat`의 IR은 처음엔 `result`를 한 글자씩(정확히는
  `S` 한 조각씩) 이어붙이는 단순 루프였는데, JVM/Scala `String`의
  불변성 때문에 매 반복마다 누적된 전체 문자열을 다시 복사해 O(n²)
  총 문자 복사가 됨 — `js-string/constants.any.js` 자신의 `'0'.
  repeat(100000)` 상수를 실제로 태워보니(무한루프 여부를 먼저 점검해
  달라는 요청으로 코드 리뷰 중 발견) 수 분 넘게 안 끝남. 위 스니펫은
  그 대신 `base`를 매 반복 두 배로 불리고 `count`의 이진수 자리마다
  조건부로 `result`에 붙이는 지수적 doubling(반복 횟수 O(log n), 총
  문자 복사량 O(n))으로 재작성한 최종본 — `n=100000` 기준 무한대에서
  3초로 단축 확인.
- **왜 "esmeta_changes"로 분류했는가**: `rule.json`은 WJI가 새로 만든
  메커니즘이 아니라 **mainline ESMeta 자신이 이미 갖고 있던** "특정 스펙
  문장 하나가 아직 자동 파싱이 안 될 때, 그 문장 원문을 손으로 IR에
  매핑해두는" escape hatch(5번 항목의 `Encode`/`UnicodeEscape` 케이스가
  이미 이 메커니즘의 선례) — 이번 두 건도 그 기존 메커니즘을 그대로
  이어서 쓴 것뿐, WJI 전용 하드코딩(`docs/hardcodes.md`)이 아님. 다만
  두 gap 다 WJI js-api corpus(`js-string/constants.any.js`)를 통해
  발견됨 — mainline 자체 테스트(test262 등)로는 지금까지 한 번도 안
  걸렸던 것으로 보임(각 항목 자체가 "이 관용구는 spec.html 전체에서
  이 한두 곳뿐"이라 커버리지 우연에 좌우됨).
- **검증**: 스크래치 스크립트로 `'ab'.repeat(3)` === `"ababab"`,
  `''.repeat(5)`/`'x'.repeat(0)` === `""`, `'5'.padStart(3,'0')` ===
  `"005"`, `'abc'.padEnd(7,'xy')` === `"abcxyxy"`,
  `encodeURIComponent("'")` === `"'"` 전부 `sbt run eval`로 직접 확인.
  doubling 재작성 후엔 `n=100000`(`'0'.repeat(100000)`)도 3초 만에
  정확한 결과를 냄을 별도로 재확인. `sbt test`는 세 수정(naive
  `repeat`/`StringPad`/doubling 재작성) 각각 `cfg.ValiditySmallTest`의
  CFG fingerprint 골든(`src/main/resources/result/cfg-fingerprint`)이
  legitimate하게 바뀌어서 매번 갱신 — 전체 529개 그린. `js-string/
  constants.any.js`(WJI)를 다시 돌려서 두 gap 다 완전히 사라진 것 확인
  — 그 파일은 이후 또 다른 별개 gap들에 부딪혀 여전히 `knownFailing`
  이었다가, 남은 gap들도 모두 풀리면서 최종적으로 40/40 완전 통과함
  (`docs/out_of_scope.md` #6, `personal/DONE.md` 참고).

## 7. `ir.ETup`이 원소 전부를 무조건 `ALValue`로 변환하도록 스코프가 좁혀져 있었음

- **File**: `src/main/scala/esmeta/state/Value.scala`(신규 `Tup` 값),
  `src/main/scala/esmeta/interpreter/Interpreter.scala`(`ETup`의 `eval`,
  `toHostFunc`), `src/main/scala/esmeta/state/util/ALValueConversion.scala`
  (`toAL`), `src/main/scala/esmeta/state/State.scala`(`applyTup`),
  `src/main/scala/esmeta/state/util/{Stringifier,UnitWalker}.scala`,
  `src/main/scala/esmeta/ty/ValueTy.scala`(exhaust성 케이스).
- **Before**: `esmeta.ir.Expr`의 `SpecTecExpr` 계열(`ECase`/`EOpt`/`ETup`)은
  각각 `ALValue.CaseV`/`OptV`/`TupV`와 1:1 대응하도록 설계돼 있고, `ETup`의
  `eval`은 `Wasm(ALValue.TupV(elems.map(e => toAL(st, eval(e)))))` — **구성되는
  즉시** 원소 전부를 `ALValue`로 변환. `func_alloc` 결과 `(store, funcaddr)`처럼
  진짜 SpecTec 경계를 넘나드는 값을 짓는 용도로는 맞는 설계였지만, WJI의
  `Compiler.compileExpr`는 스펙 산문의 "(A, B)" ad-hoc pairing을 **전부**(wasm과
  무관한 것까지) 이 `Expr.Tuple → ETup` 경로로 컴파일하고 있었음 — 지금까지는
  그 "(A, B)"가 등장하는 자리가 우연히 다 wasm 값들이라 안 걸렸을 뿐.
  `js-string` 빌트인 테이블에 클로저(`steps`)를 담아야 하면서 처음으로 깨짐
  (`find_a_builtin`의 순수 WJI 내부용 `(|builtinSetName|, |builtin|)`가
  클로저를 실어나르게 됨) — `NoWasmValue: cannot pass clo<...> across the
  WasmHost boundary`로 크래시.
- **After**: `esmeta.state.Value`에 `Tup(values: Vector[Value])`를 새로
  추가 — `Str`/`Bool`/`Number`처럼 순수 WJI/ESMeta 쪽 값으로 남고,
  `ALValue`로의 변환은 **실제로 WasmHost 경계를 넘는 시점**(`toAL`)에만
  일어나도록 늦춤. `ETup`의 `eval`은 이제 `Tup(elems.map(eval).toVector)`만
  만들고, `toAL`엔 `case Tup(vs) => ALValue.TupV(vs.map(toAL(st, _)).toList)`
  케이스 추가(변환이 실제로 필요한 지점에서만 재귀적으로 시도). `State.apply`엔
  `Wasm(ALValue.TupV(vs)) => apply(vs, field)`와 나란히 `Tup(vs) =>
  applyTup(vs, field)`를 추가해서 `base[i]` 인덱싱이 두 표현 모두에서
  동일하게 동작. `toHostFunc`(hostfunc 클로저의 반환값을 해석하는 자리)도
  `case Wasm(ALValue.TupV(List(newStateAL, returnAL)))` 대신 `case
  Tup(Vector(newState, returnVal))`로 바꾸고, 그 안에서 `toAL`을 그 자리에서
  호출하도록 조정.
- **왜 "esmeta_changes"로 분류했는가**: `ETup`이 원소를 즉시 `ALValue`로
  변환하는 것 자체는 버그가 아니라 "SpecTec 경계를 넘나드는 값을 짓는
  용도"라는 좁은 의도로 처음부터 설계된 동작(`SpecTecExpr` 그룹 전체가
  `ALValue`의 각 case를 그대로 미러링하도록 만들어져 있음, 코드 주석으로
  확인) — WJI가 그 범위를 넘어 "그냥 WJI 내부용 ad-hoc 그룹핑"까지 같은
  노드로 우겨넣어 쓴 것이 원인. `ALValue` 변환 시점을 "구성 시" → "실제 경계를
  넘을 때"로 늦춰서, 원래 mainline이 이미 다른 모든 `Value`(`Str`/`Bool`/
  `Number`/...)에 적용하고 있던 것과 같은 원칙("WasmHost 경계를 실제로 넘을
  때만 `toAL`")을 `Tup`에도 동일하게 적용한 것 — mainline 자신의 기존
  설계 원칙을 일반화한 확장이라 WJI 전용 하드코딩(`docs/hardcodes.md`)이
  아니라 이쪽으로 분류.
- **부수 효과 (WJI 쪽 정리)**: `find_a_builtin`의 `Return (|builtinSetName|,
  |builtin|)`이 클로저를 실어나르기 위해 임시로 만들었던
  `FixFindABuiltinReturnPass`(`Expr.Tuple`을 `Expr.List_`로 바꿔치기하던 패치)가
  이 수정으로 완전히 불필요해져서 삭제 — `find_a_builtin`도, `js-string`
  빌트인 테이블도 이제 스펙 산문 그대로 `Expr.Tuple`(→ `(tup ...)`)로 컴파일됨.
- **검증**: `sbt test` 529/529(`Stringifier`/`UnitWalker`/`ValueTy`의
  exhaustivity 경고까지 전부 해소). `js-string/basic.any.js`를 리팩터링
  전/후로 각각 `-wji-eval:log`로 돌려서 `match_externtype` 호출까지 정확히
  같은 지점(같은 StepCnt)에 도달하는 것 확인 — 동작 회귀 없음(이 파일 자체는
  `match_externtype` 불일치라는 별개 미해결 gap 때문에 여전히 `knownFailing`,
  `personal/TODO.md` #54 참고).
