// Driver for tconstruct.nim. Besides the shared memory, this module imports the
// runtime/cross-module helpers its code calls — here the array bounds check
// `nimIcheckB(i, bound)`, which returns the validated index. We reflect over the
// module's declared imports and provide each as an identity-on-arg0 stub (the
// bounds check returns its index arg; void helpers ignore the value) — standing
// in for the real runtime until the ported allocator/system module is linked.
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
console.log("makeDist() =", e.makeDist());
console.log("fieldStores() =", e.fieldStores());
console.log("valueCopy() =", e.valueCopy());
console.log("arraySum() =", e.arraySum());
