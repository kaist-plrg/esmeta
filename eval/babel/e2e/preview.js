//tail -n+1 * > preview.js

==> comp-prop-input.js <==
//assumption: No setters
{
  [k]: v
}

==> comp-prop-output.js <==
_ref = {}, _ref[k] = v, _ref

==> destructuring-input1.js <==
{a, b} = o

==> destructuring-input2.js <==
//assumption: o is array
[a, b] = o

==> destructuring-output1.js <==
_o = o,
a = _o.a,
b = _o.b,
_o

==> destructuring-output2.js <==
_o = o,
a = _o[0],
b = _o[1],
_o

==> exp-input.js <==
//assumption: no bigint
a ** b

==> exp-output.js <==
Math.pow(a, b)

==> logical-assign-input1.js <==
//cond: a is not a property access
a ||= b

==> logical-assign-input2.js <==
//assumption: a is not a property access
a &&= b

==> logical-assign-input3.js <==
a.p ||= b

==> logical-assign-input4.js <==
a.p &&= b

==> logical-assign-output1.js <==
a || (a = b)

==> logical-assign-output2.js <==
a && (a = b)

==> logical-assign-output3.js <==
(_a = a).p || (_a.p = b)

==> logical-assign-output4.js <==
(_a = a).p && (_a.p = b)

==> nullish-coalescing-input.js <==
a ?? b

==> nullish-coalescing-output.js <==
(_a = a) != null ? _a : b

==> obj-spread-input.js <==
{
  p,
  ...o,
  q
}

==> obj-spread-output.js <==
Object.assign(
  {p},
  o,
  {q}
)

==> opt-chain-input1.js <==
o?.p

==> opt-chain-input2.js <==
o?.p.q

==> opt-chain-input3.js <==
o?.p?.q

==> opt-chain-input4.js <==
f?.()

==> opt-chain-input5.js <==
o?.f()

==> opt-chain-input6.js <==
o?.f?.()

==> opt-chain-output1.js <==
(_o = o) == null ? void 0 : _o.p

==> opt-chain-output2.js <==
(_o = o) == null ? void 0 : _o.p.q

==> opt-chain-output3.js <==
(_o = o) == null ? void 0 : (_op = _o.p) == null ? void 0 : _op.q

==> opt-chain-output4.js <==
(_f = f) == null ? void 0 : f()

==> opt-chain-output5.js <==
(_o = o) == null ? void 0 : _o.f()

==> opt-chain-output6.js <==
(_o = o) == null ? void 0 : (_of = _o.f) == null ? void 0 : _of.call(_o)

==> shorthand-properties-input1.js <==
{p, q}

==> shorthand-properties-input2.js <==
{
  f(a){body}
}

==> shorthand-properties-output1.js <==
{p: p, q: q}

==> shorthand-properties-output2.js <==
{
  f: function f(a) {body}
}

==> spread-input1.js <==
//assumption: b is array
[a, ...b, c]

==> spread-input2.js <==
//assumption: b is array
f(a, ...b, c)

==> spread-output1.js <==
[a].concat(b, [c])

==> spread-output2.js <==
f.apply(void 0, [a].concat(b, [c]))
