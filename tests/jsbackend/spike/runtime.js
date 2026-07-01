// The consolidated runtime the generated module JS links against.
//
// Every spike so far proved one slice (layout, SSO strings, interop, emission)
// with its own throwaway shims. This gathers them into the single runtime a real
// bundle needs — so the runtime *surface* is defined in one place, and the one
// genuinely-open decision (the allocator ABI) is isolated and labelled rather
// than scattered across per-test stubs.
//
// It is still a spike in exactly one respect: the allocator is the bump
// placeholder. Everything else is the real shape. Exported as a module for the
// spike driver; in a shipped bundle these become the top-level names the
// generated code calls (`allocFixed`, `fwrite`, `stdout`, …), which is what
// `jslink` prepends ahead of the module artifacts.

"use strict";
const { NimMem } = require("./mem.js");
const mem = new NimMem();

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║ ALLOCATOR — THE ONE OPEN ABI DECISION (Araq's Q1 on PR #2043)             ║
// ╠═══════════════════════════════════════════════════════════════════════════╣
// ║ The generated code calls a small, fixed set of primitives:                ║
// ║     allocFixed(size) -> ptr        (zeroed; used by new/seq/closures)      ║
// ║     dealloc(ptr)                    (paired with =destroy / GC)            ║
// ║ The INTERFACE is settled — it is what codegen emits. Only the             ║
// ║ IMPLEMENTATION POLICY is open, and it is a two-way choice:                 ║
// ║   (a) run the existing Nim allocator (mimalloc) compiled to operate over   ║
// ║       THIS ArrayBuffer, WASM-`Memory` style (one heap, byte-addressed);    ║
// ║   (b) a minimal JS shim providing just these primitives over the buffer.   ║
// ║ Ownership (who calls dealloc — manual, =destroy, or a FinalizationRegistry ║
// ║ sweep) rides along with this same decision. This spike uses (b)'s simplest ║
// ║ form, a bump allocator, so the surface runs; swapping the body below for   ║
// ║ the real policy is the whole of the remaining allocator work.              ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
function allocFixed(size) { return mem.alloc(size, 8); }   // zeroed by mem.alloc
function dealloc(_p) { /* bump: no reclaim; the real ABI frees here */ }

// ── strings (SSO over linear memory) — proven in model.spike.js ──────────────
// 16-byte value; low bit of byte 0 is the SSO discriminant (set = inline ≤15,
// clear = heap pointer). `readRawData`/`strLen` are the two ops `write` needs and
// have an identical call site for both branches.
const STR_SIZE = 16, SSO_MAX = 15;
function mkstr(s) {
  const bytes = Buffer.from(s, "utf8");
  const sp = allocFixed(STR_SIZE);
  if (bytes.length <= SSO_MAX) {
    mem.setU8(sp, (bytes.length << 1) | 1);
    for (let i = 0; i < bytes.length; i++) mem.setU8(sp + 1 + i, bytes[i]);
  } else {
    const data = allocFixed(bytes.length);
    for (let i = 0; i < bytes.length; i++) mem.setU8(data + i, bytes[i]);
    mem.setI64(sp, bytes.length << 1);
    mem.setI64(sp + 8, data);
  }
  return sp;
}
function readRawData(sp) {
  const b0 = mem.u8At(sp);
  return (b0 & 1) ? sp + 1 : mem.i64n(sp + 8);
}
function strLen(sp) {
  const b0 = mem.u8At(sp);
  return (b0 & 1) ? (b0 >> 1) : (mem.i64n(sp) >> 1);
}

// ── stdio (fwrite reads raw bytes straight from the buffer) ───────────────────
const stdout = { write(u8) { process.stdout.write(Buffer.from(u8)); } };
function fwrite(ptr, size, nmemb, f) { f.write(mem.bytes(ptr, size * nmemb)); return nmemb; }
function fputc(c, f) { f.write(Buffer.from([c & 0xff])); return c; }
function nimFlushStdStreams() {}
function writeString(f, sp) { fwrite(readRawData(sp), 1, strLen(sp), f); }  // write(f, s)

// ── JS interop registry (jsstring / importjs) — proven in interop.spike.js ───
// JS values with no fixed byte shape are held here; a Nim field stores the int
// handle. Also the GC root set for JS values Nim holds (release = drop root).
class Registry {
  constructor() { this.slots = [null]; this.free = []; }   // 0 = null handle
  add(v) { const h = this.free.length ? this.free.pop() : this.slots.length; this.slots[h] = v; return h; }
  get(h) { return this.slots[h]; }
  release(h) { this.slots[h] = null; this.free.push(h); }
}

// ── pointers (fat `[base,key]` equality) — matches jscodegen's nimPtrEq ──────
function nimPtrEq(a, b) { return a[0] === b[0] && a[1] === b[1]; }

module.exports = {
  mem, allocFixed, dealloc,
  mkstr, readRawData, strLen,
  stdout, fwrite, fputc, nimFlushStdStreams, writeString,
  Registry, nimPtrEq,
};
