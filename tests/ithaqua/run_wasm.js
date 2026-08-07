const fs = require("fs");
const b = fs.readFileSync(process.argv[2]);
let inst;
const imports = { env: {
  nim_write: (fd, buf, len) => { const m = Buffer.from(inst.exports.memory.buffer, buf, len); process.stdout.write(m); return len; },
  nim_exit: (code) => { process.exit(code); }
}};
inst = new WebAssembly.Instance(new WebAssembly.Module(b), imports);
inst.exports._start();
