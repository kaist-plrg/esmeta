//tail -n +1 */* > preview.js

==> arrow-functions/input1.js <==
//assumption: #body# has no this, arguments, new.target, super.prop, super()
(#params#) => {#body#}

==> arrow-functions/output.js <==
(function (#params#) {
  #body#
});

==> async-to-generator/input1.js <==
async function f() {
  await #a#;
  return #b#;
}

==> async-to-generator/output1.js <==
function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
  try {
    var info = gen[key](arg);
    var value = info.value;
  } catch (error) {
    reject(error);
    return;
  } if (info.done) {
    resolve(value);
  } else { 
    Promise.resolve(value).then(_next, _throw);
  }
}

function _asyncToGenerator(fn) {
  return function () {
    var self = this, args = arguments;
    return new Promise(function (resolve, reject) {
      var gen = fn.apply(self, args);
      function _next(value) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
      }
      function _throw(err) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
      }
      _next(undefined);
    });
  };
}

function f() {
  return _f.apply(this, arguments);
}

function _f() {
  _f = _asyncToGenerator(function* () {
    yield #a#;
    return #b#;
  });
  return _f.apply(this, arguments);
}


==> computed-properties-wrong/input1.js <==
o = {
  [k]: v
};

==> computed-properties-wrong/output1.js <==
var _o;

o = ((_o = {}), (_o[k] = v), _o);

==> computed-properties/input1.js <==
o = {
  [k]: v
};

==> computed-properties/output1.js <==
function _defineProperty(obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }
  return obj;
}

o = _defineProperty({}, k, v);

==> exponentiation-operator/input1.js <==
//assumption: no bigint
#a# ** #b#;

==> exponentiation-operator/output1.js <==
Math.pow(#a#, #b#);

==> for-of/input1.js <==
for(a of b) {}

==> for-of/input2.js <==
//assumption: arr is array
for (a of #arr#) {}

==> for-of/output1.js <==
function _createForOfIteratorHelper(o, allowArrayLike) {
  var it =
    (typeof Symbol !== "undefined" && o[Symbol.iterator]) || o["@@iterator"];
  if (!it) {
    if (
      Array.isArray(o) ||
      (it = _unsupportedIterableToArray(o)) ||
      (allowArrayLike && o && typeof o.length === "number")
    ) {
      if (it) o = it;
      var i = 0;
      var F = function F() {};
      return {
        s: F,
        n: function n() {
          if (i >= o.length) return { done: true };
          return { done: false, value: o[i++] };
        },
        e: function e(_e) {
          throw _e;
        },
        f: F
      };
    }
    throw new TypeError(
      "Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."
    );
  }
  var normalCompletion = true,
    didErr = false,
    err;
  return {
    s: function s() {
      it = it.call(o);
    },
    n: function n() {
      var step = it.next();
      normalCompletion = step.done;
      return step;
    },
    e: function e(_e2) {
      didErr = true;
      err = _e2;
    },
    f: function f() {
      try {
        if (!normalCompletion && it["return"] != null) it["return"]();
      } finally {
        if (didErr) throw err;
      }
    }
  };
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
    return _arrayLikeToArray(o, minLen);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) {
    arr2[i] = arr[i];
  }
  return arr2;
}

var _iterator = _createForOfIteratorHelper(b),
  _step;

try {
  for (_iterator.s(); !(_step = _iterator.n()).done; ) {
    a = _step.value;
  }
} catch (err) {
  _iterator.e(err);
} finally {
  _iterator.f();
}


==> for-of/output2.js <==
for (var _i = 0, _arr = #arr#; _i < _arr.length; _i++) {
  a = _arr[_i];
}


==> instanceof/input1.js <==
#a# instanceof #b#;

==> instanceof/output1.js <==
function _instanceof(left, right) {
  if (
    right != null &&
    typeof Symbol !== "undefined" &&
    right[Symbol.hasInstance]
  ) {
    return right[Symbol.hasInstance](left);
  } else {
    return left instanceof right;
  }
}

_instanceof(#a#, #b#);

==> logical-assignment-operator-wrong/input1.js <==
#a# ||= #b#;

==> logical-assignment-operator-wrong/input2.js <==
#a# &&= #b#;

==> logical-assignment-operator-wrong/output1.js <==
#a# = #a# || #b#;

==> logical-assignment-operator-wrong/output2.js <==
#a# = #a# && #b#;

==> logical-assignment-operator/input1.js <==
//cond: #a# is not a property access
#a# ||= #b#;

==> logical-assignment-operator/input2.js <==
//cond: #a# is not a property access
#a# &&= #b#;

==> logical-assignment-operator/input3.js <==
#a#.p ||= #b#;

==> logical-assignment-operator/input4.js <==
#a#.p &&= #b#;

==> logical-assignment-operator/output1.js <==
#a# || (#a# = #b#);

==> logical-assignment-operator/output2.js <==
#a# && (#a# = #b#);

==> logical-assignment-operator/output3.js <==
(_a = #a#).p || (_a.p = #b#);

==> logical-assignment-operator/output4.js <==
(_a = #a#).p && (_a.p = #b#);

==> new-target-wrong/input1.js <==
var f = function() {
  new.target;
}

f();
new f();

==> new-target-wrong/output1.js <==
var f = function _target() {
  this instanceof _target ? this.constructor : void 0;
}

f();
new f();

==> new-target/input1.js <==
function f() {
  new.target;
}

f();
new f();

==> new-target/input2.js <==
var f = function() {
  new.target;
}

f();
new f();

==> new-target/output1.js <==
function f() {
  this instanceof f ? this.constructor : void 0;
}

f();
new f();

==> new-target/output2.js <==
var f = function f() {
  this instanceof f ? this.constructor : void 0;
}

f();
new f();

==> nullish-coalescing-operator-wrong/input1.js <==
#a# ?? #b#;

==> nullish-coalescing-operator-wrong/output1.js <==
#a# != null ? #a# : #b#;

==> nullish-coalescing-operator/input1.js <==
#a# ?? #b#;

==> nullish-coalescing-operator/output1.js <==
(_a = #a#) != null ? _a : #b#;

==> object-rest-spread-wrong/input1.js <==
let #o1# = {...#o2#};

==> object-rest-spread-wrong/output1.js <==
let #o1# = Object.assign({}, #o2#);

==> object-rest-spread/input1.js <==
let #o1# = {...#o2#};

==> object-rest-spread/input2.js <==
let {...#o1#} = #o2#;

==> object-rest-spread/input3.js <==
let {x, ...#o1#} = #o2#;

==> object-rest-spread/output1.js <==
function ownKeys(object) {
  var keys = Object.keys(object);
  if (Object.getOwnPropertySymbols) {
    var symbols = Object.getOwnPropertySymbols(object).filter(function (sym) {
        return Object.getOwnPropertyDescriptor(object, sym).enumerable;
    });
    keys.push.apply(keys, symbols);
  }
  return keys;
}

function _objectSpread(target, source) {
  ownKeys(Object(source)).forEach(function (key) {
    _defineProperty(target, key, source[key]);
  })
  return target;
}

function _defineProperty(obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }
  return obj;
}

#o1# = _objectSpread({}, #o2#);

==> object-rest-spread/output2.js <==
var _o = #o2#,
  #o1# = Object.assign({}, _o);


==> object-rest-spread/output3.js <==
function _objectWithoutProperties(source, excluded) {
  if (source == null) return {};
  var target = _objectWithoutPropertiesLoose(source, excluded);
  var key, i;
  if (Object.getOwnPropertySymbols) {
    var sourceSymbolKeys = Object.getOwnPropertySymbols(source);
    for (i = 0; i < sourceSymbolKeys.length; i++) {
      key = sourceSymbolKeys[i];
      if (excluded.indexOf(key) >= 0) continue;
      if (!Object.prototype.propertyIsEnumerable.call(source, key)) continue;
      target[key] = source[key];
    }
  }
  return target;
}

function _objectWithoutPropertiesLoose(source, excluded) {
  if (source == null) return {};
  var target = {};
  var sourceKeys = Object.keys(source);
  var key, i;
  for (i = 0; i < sourceKeys.length; i++) {
    key = sourceKeys[i];
    if (excluded.indexOf(key) >= 0) continue;
    target[key] = source[key];
  }
  return target;
}


let _o = #o2#, {x} = _o, #o1# = _objectWithoutProperties(_o, ["x"]);


==> optional-catch-binding/input1.js <==
try {
  #stmtlist1#
} catch {
  #stmtlist2#
}

==> optional-catch-binding/output1.js <==
try {
  #stmtlist1#
} catch(_unused) {
  #stmtlist2#
}

==> optional-chaining-wrong/input1.js <==
#obj#?.p;

==> optional-chaining-wrong/input2.js <==
#obj#?.p.q;

==> optional-chaining-wrong/output1.js <==
#obj# == null ? void 0 : #obj#.p;

==> optional-chaining-wrong/output2.js <==
var _obj;

((_obj = #obj#) == null ? void 0 : _obj.p).q;

==> optional-chaining/input1.js <==
#obj#?.p;

#obj#?.p.q;
#obj#.p?.q;
#obj#?.p?.q;

==> optional-chaining/input2.js <==
#f#?.();

#obj#?.f();
#obj#.f?.();
#obj#?.f?.();

==> optional-chaining/output1.js <==
var _obj, _obj2, _obj$p, _obj3, _obj3$p;

(_obj = #obj#) == null ? void 0 : _obj.p;
(_obj2 = #obj#) == null ? void 0 : _obj2.p.q;
(_obj$p = #obj#.p) == null ? void 0 : _obj$p.q;
(_obj3 = #obj#) == null
  ? void 0
  : (_obj3$p = _obj3.p) == null
  ? void 0
  : _obj3$p.q;


==> optional-chaining/output2.js <==
var _f, _obj, _obj$f, _obj2, _obj3, _obj3$f;

(_f = #f#) == null ? void 0 : _f();
(_obj = #obj#) == null ? void 0 : _obj.f();
(_obj$f = (_obj2 = #obj#).f) == null ? void 0 : _obj$f.call(_obj2);
(_obj3 = #obj#) == null
  ? void 0
  : (_obj3$f = _obj3.f) == null
  ? void 0
  : _obj3$f.call(_obj3);


==> parameter-wrong/input1.js <==
function f(p = 42) {
}

==> parameter-wrong/output1.js <==
function f(p) {
  if (p === void 0) {
    p = 42;
  }
}

==> parameter/input1.js <==
function f({a,b}) {
}

==> parameter/input2.js <==
function f(p = 42) {
}

==> parameter/input3.js <==
function f(...p) {
}

==> parameter/output1.js <==
function f(_ref) {
  var a = _ref.a,
    b = _ref.b;
}

==> parameter/output2.js <==
function f() {
  var p =
    arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 42;
}

==> parameter/output3.js <==
function f() {
  for (
    var _len = arguments.length, p = new Array(_len), _key = 0;
    _key < _len;
    _key++
  ) {
    p[_key] = arguments[_key];
  }
}

==> shorthand-properties/input1.js <==
let a;
let b;
let c;
let o = {a, b, c};

==> shorthand-properties/input2.js <==
var o = {
  f() {#body1#},
  g(#a#) {#body2#}
}

==> shorthand-properties/output1.js <==
let a;
let b;
let c;
let o = {a:a, b:b, c:c};

==> shorthand-properties/output2.js <==
var o = {
  f: function f() {#body1#},
  g: function g(#a#) {#body2#}
};

==> spread-wrong/input1.js <==
([#a#, ...#b#, #c#]);

==> spread-wrong/input2.js <==
f(#a#, ...#b#, #c#);

==> spread-wrong/output1.js <==
[#a#].concat(#b#, [#c#]);

==> spread-wrong/output2.js <==
f.apply(void 0, [#a#].concat(#b#, [#c#]));

==> spread/input1.js <==
([#a#, ...#b#, #c#]);

==> spread/input2.js <==
f(#a#, ...#b#, #c#);

==> spread/input3.js <==
new f(#a#, ...#b#, #c#);

==> spread/output1.js <==
function _toConsumableArray(arr) {
  return (
    _arrayWithoutHoles(arr) ||
    _iterableToArray(arr) ||
    _unsupportedIterableToArray(arr) ||
    _nonIterableSpread()
  );
}

function _nonIterableSpread() {
  throw new TypeError(
    "Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."
  );
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
    return _arrayLikeToArray(o, minLen);
}

function _iterableToArray(iter) {
  if (
    (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null) ||
    iter["@@iterator"] != null
  )
    return Array.from(iter);
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) {
    arr2[i] = arr[i];
  }
  return arr2;
}

[#a#].concat(_toConsumableArray(#b#), [#c#]);


==> spread/output2.js <==
function _toConsumableArray(arr) {
  return (
    _arrayWithoutHoles(arr) ||
    _iterableToArray(arr) ||
    _unsupportedIterableToArray(arr) ||
    _nonIterableSpread()
  );
}

function _nonIterableSpread() {
  throw new TypeError(
    "Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."
  );
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
    return _arrayLikeToArray(o, minLen);
}

function _iterableToArray(iter) {
  if (
    (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null) ||
    iter["@@iterator"] != null
  )
    return Array.from(iter);
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) {
    arr2[i] = arr[i];
  }
  return arr2;
}

f.apply(void 0, [#a#].concat(_toConsumableArray(#b#), [#c#]));

==> spread/output3.js <==
function _construct(Parent, args, Class) {
  if (_isNativeReflectConstruct()) {
    _construct = Reflect.construct.bind();
  } else {
    _construct = function _construct(Parent, args, Class) {
      var a = [null];
      a.push.apply(a, args);
      var Constructor = Function.bind.apply(Parent, a);
      var instance = new Constructor();
      if (Class) _setPrototypeOf(instance, Class.prototype);
      return instance;
    };
  }
  return _construct.apply(null, arguments);
}

function _isNativeReflectConstruct() {
  if (typeof Reflect === "undefined" || !Reflect.construct) return false;
  if (Reflect.construct.sham) return false;
  if (typeof Proxy === "function") return true;
  try {
    Boolean.prototype.valueOf.call(
      Reflect.construct(Boolean, [], function () {})
    );
    return true;
  } catch (e) {
    return false;
  }
}

function _setPrototypeOf(o, p) {
  _setPrototypeOf = Object.setPrototypeOf
    ? Object.setPrototypeOf.bind()
    : function _setPrototypeOf(o, p) {
        o.__proto__ = p;
        return o;
      };
  return _setPrototypeOf(o, p);
}

function _toConsumableArray(arr) {
  return (
    _arrayWithoutHoles(arr) ||
    _iterableToArray(arr) ||
    _unsupportedIterableToArray(arr) ||
    _nonIterableSpread()
  );
}

function _nonIterableSpread() {
  throw new TypeError(
    "Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."
  );
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
    return _arrayLikeToArray(o, minLen);
}

function _iterableToArray(iter) {
  if (
    (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null) ||
    iter["@@iterator"] != null
  )
    return Array.from(iter);
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) {
    arr2[i] = arr[i];
  }
  return arr2;
}

_construct(f, [#a#].concat(_toConsumableArray(#b#), [#c#]));


==> transform-destructuring-wrong/input1.js <==
let {#a#, #b#} = #o#;

==> transform-destructuring-wrong/input2.js <==
let [#a#, #b#] = #arr#;

==> transform-destructuring-wrong/output1.js <==
var #a# = #o#.#a#,
  #b# = #o#.#b#;

==> transform-destructuring-wrong/output2.js <==
var _arr = #arr#,
  #a# = _arr[0],
  #b# = _arr[1];

==> transform-destructuring/input1.js <==
let {#a#, #b#} = #o#;

==> transform-destructuring/output1.js <==
var _o = #o#,
  #a# = _o.#a#,
  #b# = _o.#b#;

==> typeof-symbol/input1.js <==
typeof #x#;

==> typeof-symbol/output1.js <==
function _typeof(obj) {
  "@babel/helpers - typeof";
  return (
    (_typeof =
      "function" == typeof Symbol && "symbol" == typeof Symbol.iterator
        ? function (obj) {
            return typeof obj;
          }
        : function (obj) {
            return obj &&
              "function" == typeof Symbol &&
              obj.constructor === Symbol &&
              obj !== Symbol.prototype
              ? "symbol"
              : typeof obj;
          }),
    _typeof(obj)
  );
}

_typeof(#x#);
