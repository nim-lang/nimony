// Spike: buffer EMISSION over the layout pass — the bridge from `jslayout`
// (byte offsets) to codegen. Proves the mechanical translation of a real lowered
// Leng pattern into linear-memory operations, and the one semantic the JS backend
// must own that C gets for free: aggregate VALUE COPY.
//
// Source (compiled through nimony, `structcopy.nim`):
//   type Point = object
//     x, y: int
//   proc mk(a, b: int): Point = result = Point(x: a, y: b)
//   proc bump(p: Point): Point =
//     var q = p          # whole-object value copy
//     q.x = q.x + 1
//     result = q
//   let a = mk(1, 2); let b = bump(a)
//
// Layout is NOT hand-guessed — it is exactly what `bin/jslayout_dump` prints for
// the real module:  Point size=16 align=8,  x @0 akI64,  y @8 akI64.
//
// Emission rules exercised (each 1:1 with a lowered Leng node):
//   var r: Point            (aggregate local)   -> r = alloc(16, 8)
//   Point(x: a, y: b)       (oconstr)           -> setI64(r+0,a); setI64(r+8,b)
//   q.x                     (dot q x)           -> i64(q + 0)
//   q.x = v                 (asgn (dot q x) v)  -> setI64(q + 0, v)
//   var q = p               (aggregate var-init)-> q = alloc; copyMem(q, p, 16)
//   return r                (aggregate ret)     -> return the offset; caller copies

"use strict";
const { NimMem } = require("./mem.js");
const mem = new NimMem();

// From the layout pass (bin/jslayout_dump on the real structcopy.c.nif):
const POINT = { size: 16, align: 8, x: 0, y: 8 };

// proc mk(a, b): Point = result = Point(x: a, y: b)
//   `result` is an aggregate local -> its storage is an allocation the oconstr
//   fills in place; `ret result` hands back the offset.
function _nim_mk(a, b) {
  const result = mem.alloc(POINT.size, POINT.align);
  mem.setI64(result + POINT.x, a);
  mem.setI64(result + POINT.y, b);
  return result;
}

// proc bump(p): Point =
//   var q = p; q.x = q.x + 1; result = q
//   The `var q = p` is the load-bearing case: a *fresh* allocation + copyMem, so
//   mutating q cannot alias p. (In C this is implicit value semantics; the JS
//   backend has to emit the copy itself — confirmed by inspecting the lowered IR,
//   which leaves it as a bare whole-object assignment, not a pre-lowered copyMem.)
function _nim_bump(p) {
  const q = mem.alloc(POINT.size, POINT.align);
  mem.copy(q, p, POINT.size);                       // var q = p  (VALUE copy)
  mem.setI64(q + POINT.x, mem.i64n(q + POINT.x) + 1); // q.x = q.x + 1
  return q;
}

// toplevel:  let a = mk(1, 2);  let b = bump(a)
//   An aggregate `let` copies the returned value into its own storage.
const a = mem.alloc(POINT.size, POINT.align);
mem.copy(a, _nim_mk(1, 2), POINT.size);
const b = mem.alloc(POINT.size, POINT.align);
mem.copy(b, _nim_bump(a), POINT.size);

let ok = true;
const eq = (label, got, want) => {
  if (got !== want) { ok = false; console.log(`FAIL ${label}: got ${got} want ${want}`); }
};

// a = mk(1,2)
eq("a.x", mem.i64n(a + POINT.x), 1);
eq("a.y", mem.i64n(a + POINT.y), 2);
// b = bump(a): b.x is a.x + 1, b.y unchanged
eq("b.x (a.x + 1)", mem.i64n(b + POINT.x), 2);
eq("b.y", mem.i64n(b + POINT.y), 2);
// THE VALUE-SEMANTICS ASSERTION: bump copied, so `a` is untouched by q.x mutation.
eq("a.x unaffected by bump (value copy, not alias)", mem.i64n(a + POINT.x), 1);

if (ok) {
  console.log("structcopy spike: PASS (oconstr+dot+aggregate copyMem over layout-pass offsets)");
} else {
  process.exit(1);
}
