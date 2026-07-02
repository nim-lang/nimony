// Spike: exercise the consolidated runtime surface as one module — so the
// runtime the generated bundle links against is proven coherent, not just its
// pieces in isolation. Covers each section of runtime.js: the allocator ABI
// surface, SSO strings + stdio, the interop registry, and fat-pointer equality.

"use strict";
const {
  mem, mmap, allocFixed, mkstr, strLen, writeString, Registry, nimPtrEq,
} = require("./runtime.js");

let ok = true;
const eq = (label, got, want) => {
  if (got !== want) { ok = false; console.log(`FAIL ${label}: got ${JSON.stringify(got)} want ${JSON.stringify(want)}`); }
};

// the real JS-provided boundary (Araq): mmap hands the allocator page-aligned pages
const pg = mmap(4096);
eq("mmap page-aligned", pg % 4096, 0);

// allocFixed (compiled-Nim allocator stand-in) sub-allocates those pages
const a = allocFixed(8), b = allocFixed(8);
eq("allocs distinct", a !== b, true);
eq("alloc zeroed", mem.i32(a), 0);

// SSO strings, both branches, echoed through the stdio path into a capture sink
const cap = {
  buf: [],
  write(u8) { this.buf.push(Buffer.from(u8)); },
  text() { return Buffer.concat(this.buf).toString("utf8"); },
};
const short = "hi runtime";                                 // inline SSO
const long  = "a longer, heap-allocated runtime string!";   // heap SSO
const s1 = mkstr(short), s2 = mkstr(long);
eq("short len", strLen(s1), Buffer.byteLength(short));
eq("long len", strLen(s2), Buffer.byteLength(long));
writeString(cap, s1); cap.write(Buffer.from("\n"));
writeString(cap, s2); cap.write(Buffer.from("\n"));
eq("echo both branches", cap.text(), short + "\n" + long + "\n");

// interop registry: handle round-trip + release (the GC-root behaviour)
const reg = new Registry();
const h = reg.add({ name: "Ada" });
eq("handle nonzero", h !== 0, true);
eq("handle resolves", reg.get(h).name, "Ada");
reg.release(h);
eq("handle released", reg.get(h), null);

// fat-pointer equality (component-wise, as jscodegen emits)
eq("ptr eq same", nimPtrEq([a, 0], [a, 0]), true);
eq("ptr eq diff", nimPtrEq([a, 0], [b, 0]), false);

if (ok) {
  console.log("runtime spike: PASS (allocator ABI surface + strings + stdio + registry + ptrs)");
} else {
  process.exit(1);
}
