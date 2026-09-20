// The node host for a module `jorogumo w` emits: the two `env` imports every
// program has, then `_start`. `nim_write` is synchronous so a `nim_exit` that
// follows cannot cut it short; `nim_exit` is the process exit.
const fs = require("fs");
const b = fs.readFileSync(process.argv[2]);
let inst;
const imports = { env: {
  nim_write: (fd, buf, len) => {
    fs.writeSync(fd === 2 ? 2 : 1, Buffer.from(inst.exports.memory.buffer, buf, len));
    return len;
  },
  nim_exit: (code) => { process.exit(code); }
}};
inst = new WebAssembly.Instance(new WebAssembly.Module(b), imports);
inst.exports._start();
