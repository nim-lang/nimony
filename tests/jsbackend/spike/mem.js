// Linear-memory runtime for the Leng JS backend (Typed-Array model).
//
// Araq's M2 direction (PR #2043): Nim `object`/`array`/`seq`/`string` are NOT
// mapped to native JS objects/arrays; they are laid out as bytes in a single
// growable `ArrayBuffer` — a byte-addressable linear memory, the same shape as
// the C memory model and WebAssembly linear memory. A "pointer" is then just an
// integer offset into that buffer, so `cast`, raw byte access, and the packed
// SSO string representation all work uniformly.
//
// This module is that memory: allocation + typed load/store. It is a *spike*
// runtime — the bump allocator here stands in for the real `allocFixed`/mimalloc
// primitives (one of the open questions to Araq); everything above it (layout,
// pointer = offset) is the actual model.

"use strict";

class NimMem {
  constructor(initialBytes = 1 << 16, maxBytes = 1 << 30) {
    // A resizable ArrayBuffer is "the heap". Views are length-tracking, so they
    // stay valid across `resize()` (Node 20+, and this runs on the repo's v25).
    this.buf = new ArrayBuffer(initialBytes, { maxByteLength: maxBytes });
    this.view = new DataView(this.buf);
    this.u8 = new Uint8Array(this.buf);
    // Offset 0 is reserved as the null pointer, so a real allocation is never 0.
    this.brk = 8;
  }

  grow(minBytes) {
    let cap = this.buf.byteLength;
    while (cap < minBytes) cap *= 2;
    this.buf.resize(cap);
  }

  // Bump allocator (spike stand-in for allocFixed). Returns a byte offset.
  alloc(size, align = 8) {
    const p = (this.brk + (align - 1)) & ~(align - 1);
    const end = p + size;
    if (end > this.buf.byteLength) this.grow(end);
    this.brk = end;
    this.u8.fill(0, p, end); // Nim zero-inits fresh memory
    return p;
  }

  // Typed load/store, little-endian (matches x86-64 / wasm, the default target).
  i8(p)        { return this.view.getInt8(p); }
  setI8(p, v)  { this.view.setInt8(p, v); }
  u8At(p)      { return this.view.getUint8(p); }
  setU8(p, v)  { this.view.setUint8(p, v); }
  i16(p)       { return this.view.getInt16(p, true); }
  setI16(p, v) { this.view.setInt16(p, v, true); }
  i32(p)       { return this.view.getInt32(p, true); }
  setI32(p, v) { this.view.setInt32(p, v, true); }
  // 64-bit integers are BigInt in the DataView API. `.n` unwraps small ones back
  // to a JS number where the codegen knows the value fits (indices, lengths).
  i64(p)       { return this.view.getBigInt64(p, true); }
  setI64(p, v) { this.view.setBigInt64(p, BigInt(v), true); }
  i64n(p)      { return Number(this.view.getBigInt64(p, true)); }
  f64(p)       { return this.view.getFloat64(p, true); }
  setF64(p, v) { this.view.setFloat64(p, v, true); }

  // Copy `len` bytes (memcpy / copyMem over linear memory).
  copy(dst, src, len) { this.u8.copyWithin(dst, src, src + len); }

  // A raw byte view [p, p+len) — what `fwrite(readRawData(s), 1, len, f)` needs.
  bytes(p, len) { return this.u8.subarray(p, p + len); }
}

module.exports = { NimMem };
