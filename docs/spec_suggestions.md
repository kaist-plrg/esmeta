# Spec Suggestions

`docs/spec_errors.md`(명세 자체의 오타/결함), `docs/spec_inconsistencies.md`(같은
문서 안에서 패턴이 어긋나는 것), `docs/underspecified-behaviors.md`(명세가 아예
아무 말도 안 하는 것)와 별개로 관리하는 목록이다. 여기 항목들은 명세가 틀린 것도,
동작을 빠뜨린 것도 아니다 — 동작 자체는 규범적으로(normatively) 정해져 있지만
**알고리즘 단계 밖(선언적 문장 등)에 적혀 있어서**, 알고리즘만 그대로 따라가면 그
동작이 나오지 않는 경우를 기록한다. 그 요구사항을 명시적인 알고리즘 단계로 옮기자는
개선 제안으로, 스펙 저자에게 보고할 가치가 있는 목록이라 별도로 관리한다.
(`SpecPatch`의 *(suggestion)* 태그와 대응된다.)

## 1. Creation algorithms for objects with a class string never define `%Symbol.toStringTag%`

- **File**: `webidl/index.bs`, lines 12044-12100 (`create an interface prototype object`) and 14828-14843 (`create a namespace object`); same text upstream at https://webidl.spec.whatwg.org/#create-an-interface-prototype-object and https://webidl.spec.whatwg.org/#create-a-namespace-object. The same applies to every other object given a class string: the named properties object (line 12174), iterator prototype objects (line 12971), and asynchronous iterator prototype objects (line 13276).
- **Current**: neither algorithm has a step for `%Symbol.toStringTag%`. The property comes only from a declarative rule (line 7182):
  ```
  If an object has a [=class string=] |classString|, then the object must,
  at the time it is created, have a property whose name is the {{%Symbol.toStringTag%}} symbol
  with PropertyDescriptor{\[[Writable]]: false, \[[Enumerable]]: false, \[[Configurable]]: true,
  \[[Value]]: |classString|}.
  ```
  and from a sentence after each algorithm naming the class string ("The [=class string=] of an [=interface prototype object=] is the [=interface=]'s [=qualified name=].", line 12140; "The [=class string=] of a [=namespace object=] is the [=namespace=]'s [=identifier=].", line 14845).
- **Suggested** (`create an interface prototype object`, just before `Return |interfaceProtoObj|.`):
  ```
  1.  Let |classString| be the [=qualified name=] of |interface|.
  1.  Perform [=!=] <a abstract-op>DefinePropertyOrThrow</a>(|interfaceProtoObj|, {{%Symbol.toStringTag%}},
      PropertyDescriptor{\[[Writable]]: false, \[[Enumerable]]: false, \[[Configurable]]: true, \[[Value]]: |classString|}).
  ```
  and the same step in `create a namespace object` with `|namespace|'s [=identifier=]` as the value (likewise for the other objects above). The rule at line 7182 then becomes redundant and can be removed or turned into a note.
- **Reason**: the behavior is normatively specified, but only outside the steps: following either algorithm's steps alone produces an object with no `%Symbol.toStringTag%` property, so `Object.prototype.toString.call(WebAssembly)` returns `"[object Object]"` rather than `"[object WebAssembly]"` (and `"[object Object]"` rather than `"[object WebAssembly.Module]"` for `WebAssembly.Module.prototype`). Some other own property of these objects are defined by an explicit step, including `%Symbol.unscopables%`, which has exactly the same descriptor shape. The "must, at the time it is created, have a property" rule effectively inserts an unnumbered step into each creation algorithm with no stated position. whatwg/webidl#494 made `create an interface prototype object` imperative, converted the equivalent `@@unscopables` rule ("there must be a property on the interface prototype object whose name is the @@unscopables symbol ...") into steps, and flagged the properties it left declarative with `Issue: Define those properties imperatively instead.` (still present at line 12108). `%Symbol.toStringTag%` is not on that list. Whether that was deliberate (e.g. one shared rule covering every kind of object with a class string) is not recorded; worth asking rather than asserting. whatwg/webidl#357 later changed only the class string's *value* (dropping the `"Prototype"` suffix), not how the property is defined.
- **Open question — where the step goes**: `Reflect.ownKeys` lists string keys before symbol keys, so the step's position is observable only relative to the *other symbol-keyed* own properties of the same object: `%Symbol.unscopables%` (any `[Unscopable]` member), `%Symbol.iterator%` (`iterable`/`maplike`/`setlike`/indexed getter, via "define the iteration methods"), and `%Symbol.asyncIterator%` (`async_iterable`). Placing it just before `Return` (as suggested above, and as `SpecPatch` does) gives e.g. `[Symbol.unscopables, Symbol.toStringTag]` for `Element.prototype`; placing it right after the object is allocated gives the reverse. A scan of WPT's `interfaces/*.idl` (wpt@3440698, 2026-10-23; regex-level, so treat counts as approximate) finds 79 non-`[Global]` interfaces whose prototype has exactly two symbol keys, and none with three:
  - `unscopables` + `toStringTag` (5): `CharacterData`, `Document`, `DocumentFragment`, `DocumentType`, `Element` (all via DOM's `ChildNode`/`ParentNode` mixins)
  - `asyncIterator` + `toStringTag` (2): `ReadableStream`, `FileSystemDirectoryHandle`
  - `iterator` + `toStringTag` (72): e.g. `NodeList`, `HTMLCollection`, `DOMTokenList`, `Headers`, `URLSearchParams`, `FormData`, `FontFaceSet`, `CSSStyleDeclaration`

  For the namespace object and the WebAssembly JS-API interfaces, `%Symbol.toStringTag%` is the only symbol key, so the order is not observable there. No WPT test checks this order: the only exact `Reflect.ownKeys` assertion involving `%Symbol.toStringTag%` is `webidl/ecmascript-binding/window-named-properties-object.html` (an object with that single key), and the order tests in `webidl/ecmascript-binding/builtin-function-properties.any.js` / `legacy-factory-function-builtin-properties.window.js` cover only `length`/`name`/`prototype` on functions. Engine behavior not yet checked; e.g. `Object.getOwnPropertySymbols(Element.prototype)`, `Object.getOwnPropertySymbols(NodeList.prototype)` and `Object.getOwnPropertySymbols(ReadableStream.prototype)` in each browser would show whether engines agree, which would decide the step's position and justify a new WPT test.
- **Known issue** (to cite in an upstream PR):
  - https://github.com/whatwg/webidl/pull/494 — "Make interface prototype object creation imperative"; merged 2018-04-10. The precedent: the same conversion, already done for `@@unscopables`.
  - https://github.com/whatwg/webidl/pull/357 — "Only install @@toStringTag on the prototype"; merged 2020-04-23. Last change to the class string rules.
- Applied via `SpecPatch` #81 (`create an interface prototype object`, with `qualified name` added to `SpecFile.webidlFilter`) and directly in `manuals/funcs/create_a_namespace_object.ir` (`create a namespace object` is hand-written, not extracted).
