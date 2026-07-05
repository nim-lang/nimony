// Driver for tcompute.nim: instantiate the compiled .wasm (passed as argv[2])
// and call its exported functions. Node's WebAssembly engine validates the
// bytecode on instantiation, so a malformed module throws here.
"use strict";
const fs = require("fs");
const bytes = fs.readFileSync(process.argv[2]);
const memory = new WebAssembly.Memory({ initial: 1 });
WebAssembly.instantiate(bytes, { env: { memory } }).then(r => {
  const e = r.instance.exports;
  console.log("add2(2,3) =", e.add2(2, 3));
  console.log("add2(-5,8) =", e.add2(-5, 8));
  console.log("fib(10) =", e.fib(10));
  console.log("fib(20) =", e.fib(20));
  console.log("sumTo(100) =", e.sumTo(100));
  console.log("arith(7,3) =", e.arith(7, 3));
  console.log("countdown(5) =", e.countdown(5));
  console.log("mul64 =", e.mul64(1000000n, 1000000n).toString());
  console.log("fcombine(2.5,4.0) =", e.fcombine(2.5, 4.0));
}).catch(err => { console.error("WASM ERROR:", err.message); process.exit(1); });
