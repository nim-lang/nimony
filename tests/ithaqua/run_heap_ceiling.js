// B0 ceiling check (wasm-only semantics, so not a wasmdiff fixture): with
// nim_set_heap_ceiling set low, an allocation past the ceiling must raise
// Nim's OutOfMemDefect (surfacing as nim_exit / trap), NOT grow past it.
// Run: node run_heap_ceiling.js <heap_grow.wasm>
const fs = require("fs");
let inst, wrote = "";
const imports = { env: {
  nim_write: (fd, buf, len) => { wrote += Buffer.from(inst.exports.memory.buffer, buf, len).toString(); return len; },
  nim_exit: (c) => { throw { nimExit: c }; },
}};
inst = new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync(process.argv[2])), imports);
const E = inst.exports;

const startPages = E.memory.buffer.byteLength / 65536;
E.nim_set_heap_ceiling(16 * 1024 * 1024 + startPages * 65536); // ~16 MB of headroom
let failed = false;
try {
  E._start();                       // heap_grow wants ~48 MB -> must OOM
} catch (e) {
  failed = true;
  console.log("stopped as expected:", e.nimExit !== undefined ? "nim_exit " + e.nimExit : e.message);
}
const endBytes = E.memory.buffer.byteLength;
console.log("rounds completed before OOM:", (wrote.match(/round /g) || []).length);
console.log("memory MB at stop:", (endBytes / 1048576).toFixed(1));
if (!failed) { console.error("FAIL: ran to completion despite ceiling"); process.exit(1); }
if (endBytes > 40 * 1024 * 1024) { console.error("FAIL: memory grew far past ceiling"); process.exit(1); }
console.log("CEILING OK");
