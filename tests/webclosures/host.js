// `node host.js out.wasm`: runs the program with the two `env` imports every
// module has.
const fs = require("fs");
const mod = new WebAssembly.Module(fs.readFileSync(process.argv[2]));
let inst;
const env = {
  nim_write: (fd, buf, len) => {
    fs.writeSync(fd === 2 ? 2 : 1, Buffer.from(inst.exports.memory.buffer, buf, len));
    return len;
  },
  nim_exit: (code) => { process.exit(code); },
};
inst = new WebAssembly.Instance(mod, { env });
inst.exports._start();
