// WJI doesn't mechanize DataView at all (ESMeta's own `yets` list, see
// src/main/scala/esmeta/es/builtin/package.scala). The only place in the
// corpus that touches DataView is wasm-module-builder.js's module-level
// `let data_view = new DataView(byte_view.buffer);` -- used only by
// wasmF32Const/wasmF64Const, to encode a JS number as IEEE754 bytes for a
// wasm f32.const/f64.const immediate -- but that line runs unconditionally
// at load time, so it blocks every test that merely loads the builder,
// whether or not it ever calls either function.
//
// This is a minimal DataView polyfill (constructor + setFloat32/setFloat64,
// the only two methods the corpus actually calls) backed by a real Uint8Array
// view over the same buffer, so wasm-module-builder.js's own code runs
// completely unmodified -- byte_view[i] sees exactly what setFloat32/64
// wrote, same as with the real DataView. Load this BEFORE
// wasm-module-builder.js.
//
// `ieee754Bytes` is a little-endian-only port of the well-known `ieee754`
// npm package's write() (MIT, feross), verified against real DataView output
// for the finite/Infinity/-0/subnormal range plus 200k random doubles. The
// one place it can't match bit-for-bit is NaN payloads read back out of raw
// memory (DataView can preserve an arbitrary NaN bit pattern; this always
// emits the canonical quiet NaN) -- irrelevant here since nothing in the
// corpus ever passes NaN to setFloat32/setFloat64.
function ieee754Bytes(value, mLen, nBytes) {
  const bytes = new Array(nBytes);
  let eLen = (nBytes * 8) - mLen - 1;
  const eMax = (1 << eLen) - 1;
  const eBias = eMax >> 1;
  const rt = mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0;
  let i = 0; // little-endian only
  const d = 1;
  const s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0;
  let e, m, c;

  value = Math.abs(value);

  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? Math.pow(2, mLen - 1) : 0; // canonical quiet NaN
    e = eMax;
  } else {
    e = Math.floor(Math.log(value) / Math.LN2);
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--;
      c *= 2;
    }
    if (e + eBias >= 1) {
      value += rt / c;
    } else {
      value += rt * Math.pow(2, 1 - eBias);
    }
    if (value * c >= 2) {
      e++;
      c /= 2;
    }
    if (e + eBias >= eMax) {
      m = 0;
      e = eMax;
    } else if (e + eBias >= 1) {
      m = ((value * c) - 1) * Math.pow(2, mLen);
      e = e + eBias;
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
      e = 0;
    }
  }

  for (; mLen >= 8; bytes[i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

  e = (e << mLen) | m;
  eLen += mLen;
  for (; eLen > 0; bytes[i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

  bytes[i - d] |= s * 128;
  return bytes;
}

function DataView(buffer, byteOffset, byteLength) {
  const view = new Uint8Array(buffer);
  const base = byteOffset || 0;

  function write(offset, bytes, littleEndian) {
    if (!littleEndian) bytes.reverse();
    for (let i = 0; i < bytes.length; i++) view[base + offset + i] = bytes[i];
  }

  this.setFloat32 = (offset, value, littleEndian) => write(offset, ieee754Bytes(value, 23, 4), littleEndian);
  this.setFloat64 = (offset, value, littleEndian) => write(offset, ieee754Bytes(value, 52, 8), littleEndian);
}
