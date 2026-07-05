// Driver for techo.nim: a whole PROGRAM. Call its C `main` and provide the C
// stdio primitives it imports (fwrite/fputc/fprintf) so its output lands on
// stdout — the same runtime seam runtime.js fills for the JS backend. String
// literals come from data segments; integers are formatted through fprintf.
"use strict";
const fs = require("fs");
const bytes = fs.readFileSync(process.argv[2]);
const mod = new WebAssembly.Module(bytes);
const memory = new WebAssembly.Memory({ initial: 16 });
const U8 = () => new Uint8Array(memory.buffer);
const readCStr = (p) => { let s = ""; const m = U8(); for (let i = p; m[i] !== 0; i++) s += String.fromCharCode(m[i]); return s; };
const env = { memory };
env.fwrite = (ptr, size, n) => { process.stdout.write(Buffer.from(U8().subarray(ptr, ptr + size * n))); return n; };
env.fputc = (ch) => { process.stdout.write(Buffer.from([ch & 0xff])); return ch; };
env.fprintf = (stream, fmtPtr, ...args) => {
  let ai = 0;
  const out = readCStr(fmtPtr).replace(/%[-0-9.]*l*[dioux]/g, () => {
    const v = args[ai++]; return typeof v === "bigint" ? v.toString() : String(v);
  });
  process.stdout.write(out);
  return out.length;
};
for (const imp of WebAssembly.Module.imports(mod)) {
  if (imp.kind === "function" && !(imp.name in env)) env[imp.name] = () => 0;
}
new WebAssembly.Instance(mod, { env }).exports.main(0, 0, 0);
