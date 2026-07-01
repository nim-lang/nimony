// Spike: prove the Typed-Array linear-memory model dissolves the string wall.
//
// The M0/M1 JS backend runs integer `echo` end-to-end, but *string* `echo` was
// blocked: `write` lowers to `readRawData(s)` (a pointer into either the inline
// SSO bytes or the heap buffer) handed to `fwrite` as raw bytes — "byte-address
// into the packed SSO integer", which native JS objects can't do. Under Araq's
// model the string is just bytes in linear memory, so `readRawData` is a byte
// offset and `fwrite` reads from it. This file demonstrates exactly that, by
// hand, so it is the concrete target the codegen must emit.

"use strict";
const { NimMem } = require("./mem.js");
const mem = new NimMem();

// ── struct layout (mirrors tdata's mkpoint) ─────────────────────────────────
// `Point = object x, y: int32` → 8 bytes, x@0, y@4. Construction is an alloc
// plus field stores at their byte offsets; access is a typed load at the offset.
const Point = { size: 8, align: 4, x: 0, y: 4 };
function mkpoint(a, b) {
  const p = mem.alloc(Point.size, Point.align);
  mem.setI32(p + Point.x, a);
  mem.setI32(p + Point.y, b);
  return p;
}
const pointSum = (p) => mem.i32(p + Point.x) + mem.i32(p + Point.y);

// ── SSO string ──────────────────────────────────────────────────────────────
// A `string` value is 16 bytes. Low bit of byte 0 is the SSO discriminant
// (nimony's scheme): set = short/inline, clear = long/heap.
//   short: byte0 = (len << 1) | 1, then up to 15 inline bytes at offset 1.
//   long:  i64 @0 = (len << 1)      (low bit 0), ptr @8 = heap data offset.
const STR_SIZE = 16, SSO_MAX = 15;

function mkstr(s) {
  const bytes = Buffer.from(s, "utf8");
  const sp = mem.alloc(STR_SIZE, 8);
  if (bytes.length <= SSO_MAX) {
    mem.setU8(sp, (bytes.length << 1) | 1);       // inline discriminant + len
    for (let i = 0; i < bytes.length; i++) mem.setU8(sp + 1 + i, bytes[i]);
  } else {
    const data = mem.alloc(bytes.length, 1);      // heap buffer
    for (let i = 0; i < bytes.length; i++) mem.setU8(data + i, bytes[i]);
    mem.setI64(sp, bytes.length << 1);            // len, low bit clear
    mem.setI64(sp + 8, data);                     // data pointer (offset)
  }
  return sp;
}

// The two operations `write` needs — identical call site for inline vs heap.
function readRawData(sp) {
  const b0 = mem.u8At(sp);
  return (b0 & 1) ? sp + 1 : mem.i64n(sp + 8);
}
function strLen(sp) {
  const b0 = mem.u8At(sp);
  return (b0 & 1) ? (b0 >> 1) : (mem.i64n(sp) >> 1);
}

// ── stdio over linear memory (fwrite reads raw bytes from the buffer) ─────────
const cap = { chunks: [], write(u8) { this.chunks.push(Buffer.from(u8)); } };
const captured = () => Buffer.concat(cap.chunks).toString("utf8");
function fwrite(ptr, size, nmemb, f) { f.write(mem.bytes(ptr, size * nmemb)); }
function echo(sp) {                                  // write(stdout, s); write("\n")
  fwrite(readRawData(sp), 1, strLen(sp), cap);
  const nl = mem.alloc(1, 1); mem.setU8(nl, 10);
  fwrite(nl, 1, 1, cap);
}

// ── run ──────────────────────────────────────────────────────────────────────
const short = "hi spike";                            // 8 bytes  → inline SSO
const long = "a longer, heap-allocated string!";     // 32 bytes → heap
echo(mkstr(short));
echo(mkstr(long));
const wantOut = short + "\n" + long + "\n";

const p = mkpoint(3, 4);
const gotSum = pointSum(p);

let ok = true;
if (captured() !== wantOut) { ok = false; console.log("FAIL string: got " + JSON.stringify(captured())); }
if (gotSum !== 7) { ok = false; console.log("FAIL point: got " + gotSum); }
if (ok) {
  process.stdout.write(captured());                  // show the real echo output
  console.log("spike: PASS (struct layout + SSO inline/heap string echo over linear memory)");
} else {
  process.exit(1);
}
