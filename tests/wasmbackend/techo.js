// Driver for techo.nim: a whole PROGRAM (not exported functions). We call its C
// `main(argc,argv,envp)` entry and provide the C stdio primitives it imports
// (fwrite/fputc) so its output lands on stdout — the same runtime seam the JS
// backend fills with runtime.js. The string is materialised from a data segment,
// so this reads real bytes out of the shared linear memory.
"use strict";
const fs = require("fs");
const bytes = fs.readFileSync(process.argv[2]);
const mod = new WebAssembly.Module(bytes);
const memory = new WebAssembly.Memory({ initial: 16 });
const U8 = () => new Uint8Array(memory.buffer);
const env = { memory };
env.fwrite = (ptr, size, n, stream) => {
  const len = size * n;
  process.stdout.write(Buffer.from(U8().subarray(ptr, ptr + len)));
  return n;
};
env.fputc = (ch, stream) => { process.stdout.write(Buffer.from([ch & 0xff])); return ch; };
for (const imp of WebAssembly.Module.imports(mod)) {
  if (imp.kind === "function" && !(imp.name in env)) env[imp.name] = () => 0;
}
const e = new WebAssembly.Instance(mod, { env }).exports;
e.main(0, 0, 0);
