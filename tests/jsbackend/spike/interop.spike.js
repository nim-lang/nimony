// Spike: the JS-interop boundary — registry, allocator-backed factory, and the
// ES-class export wrapper. Backs the interop answers on PR #2043 with running
// code, the same way model.spike.js backs the linear-memory answers.
//
// Three questions, three demonstrations:
//   Q1  Where do JS primitives live? number/boolean → unboxed in linear memory;
//       string/object → a handle in the registry (no fixed byte shape).
//   Q2  How does JS allocate a Nim object (Option-A getters/setters)? Via a
//       generated factory that calls the Nim allocator to carve buffer bytes.
//   Q3  ES6? The export interface is an ES class wrapping the buffer offset.

"use strict";
const { NimMem } = require("./mem.js");
const mem = new NimMem();

// ── JS handle registry ───────────────────────────────────────────────────────
// JS values with no linear-memory representation (objects, strings, functions)
// are held here; a Nim field stores the integer handle. This table is also the
// GC root set for JS values Nim is holding (release = drop the root).
class Registry {
  constructor() { this.slots = [null]; this.free = []; } // 0 is the null handle
  add(v) { const h = this.free.length ? this.free.pop() : this.slots.length; this.slots[h] = v; return h; }
  get(h) { return this.slots[h]; }
  release(h) { this.slots[h] = null; this.free.push(h); }
}
const reg = new Registry();

// ── a Nim object in linear memory ────────────────────────────────────────────
// Person { age: int32 @0; name: jsstring @4 }.
//   age  — a JS number, which has a machine rep, so it is stored UNBOXED in the
//          buffer (Q1: primitives-with-a-rep live in linear memory).
//   name — a JS string, which has no fixed byte shape, so the field stores a
//          registry HANDLE (Q1: strings go through the registry).
const PERSON = { size: 8, align: 4, age: 0, name: 4 };

// Generated, allocator-backed factory (Q2). JS cannot carve Nim bytes itself, so
// it calls the compiled Nim allocator + initializer and gets back an offset.
function _nim_Person_new(age, jsName) {
  const p = mem.alloc(PERSON.size, PERSON.align);   // the Nim allocator
  mem.setI32(p + PERSON.age, age);                  // number → unboxed in buffer
  mem.setI32(p + PERSON.name, reg.add(jsName));     // string → registry handle
  return p;
}
// Generated accessors (Option-A get/set at the computed byte offsets).
const _nim_Person_get_age  = (p)    => mem.i32(p + PERSON.age);
const _nim_Person_set_age  = (p, v) => mem.setI32(p + PERSON.age, v);
const _nim_Person_get_name = (p)    => reg.get(mem.i32(p + PERSON.name));
const _nim_Person_free     = (p)    => { reg.release(mem.i32(p + PERSON.name)); /* + nim dealloc(p) */ };

// ── the ergonomic ES6 wrapper JS actually uses (Q3) ──────────────────────────
class Person {
  constructor(age, name) { this._p = _nim_Person_new(age, name); }
  get age()  { return _nim_Person_get_age(this._p); }
  set age(v) { _nim_Person_set_age(this._p, v); }
  get name() { return _nim_Person_get_name(this._p); }
  dispose()  { _nim_Person_free(this._p); this._p = 0; }
}

// ── run ──────────────────────────────────────────────────────────────────────
let ok = true;
const eq = (label, got, want) => {
  if (got !== want) { ok = false; console.log(`FAIL ${label}: got ${JSON.stringify(got)} want ${JSON.stringify(want)}`); }
};

const a = new Person(30, "Ada Lovelace");
eq("age initial (number from buffer)", a.age, 30);
a.age = a.age + 1;                       // mutate the unboxed int in linear memory
eq("age mutated (round-trips buffer)", a.age, 31);
eq("name (string via registry)", a.name, "Ada Lovelace");

// A second instance gets a distinct offset and a distinct handle — no aliasing.
const b = new Person(7, "Grace Hopper");
eq("b.age independent", b.age, 7);
eq("a.age unaffected", a.age, 31);
eq("b.name via registry", b.name, "Grace Hopper");

a.dispose();                            // releases the registry root (and would free buffer bytes)
eq("registry root released on dispose", reg.get(1), null);

if (ok) {
  console.log("interop spike: PASS (registry + allocator factory + ES-class export)");
} else {
  process.exit(1);
}
