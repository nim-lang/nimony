// Driver for tglobals.nim: call bump() repeatedly and observe the module-level
// globals persist across calls in the shared linear memory.
"use strict";
const fs = require("fs");
const bytes = fs.readFileSync(process.argv[2]);
const mod = new WebAssembly.Module(bytes);
const memory = new WebAssembly.Memory({ initial: 1 });
const env = { memory };
for (const imp of WebAssembly.Module.imports(mod)) {
  if (imp.module === "env" && imp.kind === "function" && !(imp.name in env)) {
    env[imp.name] = (a) => a;
  }
}
const e = new WebAssembly.Instance(mod, { env }).exports;
console.log("bump() =", e.bump());
console.log("bump() =", e.bump());
console.log("bump() =", e.bump());
console.log("getTotal() =", e.getTotal());
