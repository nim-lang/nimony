// The node host for host.nim. `node host.js out.wasm` runs the program with
// the two `env` imports every module has plus the two bridge functions the
// fixture declares. `node host.js --imports out.wasm` lists the module's
// imports and exports instead (`env.NAME` / `export NAME`), sorted.
const fs = require("fs");
const listing = process.argv[2] === "--imports";
const mod = new WebAssembly.Module(fs.readFileSync(process.argv[listing ? 3 : 2]));
if (listing) {
  const rows = WebAssembly.Module.imports(mod).map(i => i.module + "." + i.name)
    .concat(WebAssembly.Module.exports(mod).map(e => "export " + e.name));
  console.log(rows.sort().join("\n"));
  process.exit(0);
}
let inst;
const env = {
  nim_write: (fd, buf, len) => {
    fs.writeSync(fd === 2 ? 2 : 1, Buffer.from(inst.exports.memory.buffer, buf, len));
    return len;
  },
  nim_exit: (code) => { process.exit(code); },
  host_note: (x) => { console.log("note " + x); },
  host_add: (a, b) => a + b,
};
inst = new WebAssembly.Instance(mod, { env });
inst.exports._start();
