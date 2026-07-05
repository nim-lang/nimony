// Driver for tmemory.nim: lay a Point{x,y,z: int32} out in the shared linear
// memory and pass its byte offset (a pointer) to the exported functions. This is
// the same ArrayBuffer the JS backend uses as `mem` — here it is the module's
// imported WebAssembly.Memory.
"use strict";
const fs = require("fs");
const bytes = fs.readFileSync(process.argv[2]);
const memory = new WebAssembly.Memory({ initial: 1 });
WebAssembly.instantiate(bytes, { env: { memory } }).then(r => {
  const e = r.instance.exports;
  const H = new Int32Array(memory.buffer);
  const p = 64;                       // a byte offset well clear of null
  H[p / 4] = 3; H[p / 4 + 1] = 4; H[p / 4 + 2] = 12;   // x=3, y=4, z=12
  console.log("dot(p) =", e.dot(p));                    // 9 + 16 + 144 = 169
  console.log("manhattan(p) =", e.manhattan(p));        // 3 + 4 + 12 = 19
  e.setY(p, 7);
  console.log("after setY(p,7): y =", H[p / 4 + 1]);    // 7
  console.log("dot(p) =", e.dot(p));                    // 9 + 49 + 144 = 202
}).catch(err => { console.error("WASM ERROR:", err.message); process.exit(1); });
