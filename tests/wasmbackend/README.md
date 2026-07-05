# WebAssembly backend (`lengwasm`)

A fourth codegen over the **Leng** IR, alongside the C (`codegen.nim`), LLVM
(`llvmcodegen.nim`) and JavaScript (`jscodegen.nim`) backends, structured as the
same standalone `{.build.}`-schedulable plugin the JS backend uses (Araq's
"backends are plugins" direction for PR #2043):

- **`lengwasm <module.c.nif> <out.wasm>`** — the per-module backend: reads one
  module's lowered Leng IR and emits a `.wasm` binary module.

## Why it is additive, not a rewrite

**WASM linear memory *is* the JS backend's linear-memory model.** Under Araq's
Typed-Array direction (#2043) Nim-native data lives as bytes in one
byte-addressable buffer and a pointer is an integer offset — which is exactly
what WASM linear memory is (a single resizable `ArrayBuffer`, `i32.load`/`store`
at byte offsets). So this backend **reuses `jslayout` verbatim** — the C-ABI
`sizeof`/alignment/field-offset/`AccessKind` computer — and only the emitted
*instruction* differs:

| | JavaScript backend | WASM backend |
|---|---|---|
| model | one `ArrayBuffer`, pointer = byte offset | *same* |
| layout | `jslayout` byte offsets | **`jslayout`, unchanged** |
| field load `(dot p f)` | `mem.getI32(p + off)` (JS text) | `i32.load` at `p + off` (bytecode) |
| output | a `jsnif` text tree → `.js` | a `WasmModule` → `.wasm` bytecode |

The split mirrors `llvmcodegen`/`llvmserializer`: `wasmcodegen.nim` translates
Leng → a `WasmModule`, and `wasmenc.nim` (which knows nothing about Leng) encodes
that module to the `.wasm` wire format (LEB128, sections, opcodes). `wasmenc` is
independently unit-testable against a hand-built module.

## What this first slice covers

The **pure-compute + linear-memory scalar slice**:

| Leng | WASM |
|------|------|
| `(proc :f (params (param :a . T)) RET …)` | a function; params are locals `0..n`; exported under its `exportc` name |
| `(add T a b)` etc. | `i32.add`/`i64.add`/`f64.add` — the type operand picks the class. **Integer ops wrap natively**, so the 32-bit-wrapping fix the JS backend needed is *free* here |
| `(div/mod T a b)` | `i32.div_s`/`div_u`/`rem_s`/… by signedness |
| `(lt a b)`, `(eq a b)` | `i32.lt_s`/`lt_u`/`eq`/… (width + signedness from the operand type) |
| `(and a b)`, `(or a b)`, `(not a)` | short-circuit via a value-typed `if`; `i32.eqz` |
| `(conv/cast T x)` | `i32.wrap_i64`, `i64.extend_i32_s/u`, `f64.convert_i32_s/u`, `i32.trunc_f64_s/u`, sign/zero narrowing |
| `(if (elif c …) (else …))` | `if`/`else`/`end` (nested for `elif` chains) |
| `(while c …)` | `block { loop { (eqz c) br_if $block; body; br $loop } }` |
| `(jmp L)` / `(lab L)` | `br` out of a `block` whose `end` sits where the label is — hexer's `jmp`s are forward-only and scoped, so a `break` (lowered to `jmp`) needs no relooper; the `.c.nif` is already structured (no raw `goto`) |
| `(ret e)` | `e; return` |
| `(dot p f)` / `(at a i)` / `(deref p)` | address (`base + off` / `base + i*stride` / `p`) then `iN.load` at the **`jslayout`** offset |
| `(asgn lval e)` on the above | address, value, then `iN.store`; whole-aggregate assignment is a `memory.copy` (value semantics) |
| `(oconstr T (kv f v)…)` / `(aconstr T e…)` | bump-allocate `sizeof T`, then a store per field/element at its `jslayout` offset; the allocation's address is the value |
| `(sizeof T)` | `i32.const` of the layout size |

**Aggregates** (objects/arrays) live in linear memory. Storage comes from a bump
pointer `$hp` — a mutable `i32` global that only grows (never freed), mirroring
the JS backend's `allocFixed` (the pre-existing GC/#1518 gap). A pointer is the
integer byte offset the bump returns; construction, field/element access and
value-semantic copy all use the **`jslayout`** offsets unchanged.

**Cross-module / runtime calls → host imports.** A call to a symbol not defined
in this module (a `system` helper, an `importc`, e.g. the array bounds check
`nimIcheckB(i, bound)`) is emitted as a call to a **host function import** whose
signature is read from the callee's decl. A pre-pass collects these so imports
occupy the low function indices (as WASM requires). This is the same seam the JS
backend's runtime fills, and the foundation for linking the ported allocator and
`std` modules later; the tests provide the handful of stubs their code needs.

### Entry model

Two modes:

- **Library (default):** a WASM module is instantiated by a host that calls its
  exports, so the backend emits a function per `exportc` proc (and its callees)
  and exports it. It does *not* emit the C `main(argc,argv,envp)` shim.
- **Whole-program (`--program`, marked by a `<test>.program` file):** the C
  `main` entry *is* emitted along with its whole cross-module closure, and the
  host calls `main(0,0,0)`. This is how a real program like `echo` runs — its
  work happens in the module-init (`ini`) chain `main` calls, not in an `exportc`
  proc.

Constructs outside the slice emit `unreachable` (stack-polymorphic, so the module
still validates) and are reported on stderr, so the coverage gap is visible.

### Multi-module linking, globals, constants → `echo` runs end-to-end

`echo "hello world"` compiles and runs under Node's WASM engine (`techo`). Three
pieces make a real program work, all on the same `jslayout` core:

- **Multi-module linking.** A call to a symbol not defined in this module is
  resolved through `getDeclOrNil`: a foreign *defined* proc (the `std/syncio`
  `write`, a `system` helper) is pulled in — its body loads via the lazy
  `ForeignModule` loader, sharing the main pool's SymIds — and emitted as a real
  function, transitively. Only genuine `importc` primitives (`fwrite`, `fputc`,
  …) stay host imports (the same seam the JS backend's `runtime.js` fills). A
  call-boundary coercion wraps/extends a scalar whose produced WASM type differs
  from the callee's param (cross-module boundaries occasionally need it).
- **Globals as memory slots.** Every module-level var/const — this module's and
  every foreign one a reachable body touches (`stdout`, allocator/syncio state) —
  gets a fixed byte slot in a globals region below the bump heap. Zero-initialised
  (linear memory starts 0).
- **Constant data segments.** A `const` with a constant initializer (a string
  literal's `LongString` — `fullLen`/`rc`/`capImpl` + the `data` flexarray bytes)
  is laid out into a byte image at its slot's `jslayout` offsets and emitted as a
  WASM **data segment**, so the runtime `write` reads real bytes back.

So `echo` runs: `main` → `ini` (init chain) → `write(stdout, str)` → `fwrite`
(host), with the string materialised from a data segment. `echo` of an *integer*
(int→string) is the next small step (one unhandled node in that path).

## Test

The suite is a hastur **custom runner** (`setup.nim`), like the JS backend's.
Each `t*.nim` is compiled to a real `.wasm`, which **Node's `WebAssembly` engine
validates before executing** (a malformed module throws), and a sibling `t*.js`
driver instantiates it, calls its exports, and prints; stdout is diffed against
the sibling `.output`.

```
hastur tests/wasmbackend              # run the suite
hastur --overwrite tests/wasmbackend  # regenerate the .output goldens
```

Per test: `nimony c --bits:32 --define:nimNativeAlloc` (→ the main module's
`.c.nif`) → `lengwasm` → `node t.js <wasm>` → diff. The trailing 32-bit C link
fails on a 64-bit host and is ignored on purpose (the `.c.nif` is emitted by
hexer *before* the C backend, so a genuine front-end error is the one that leaves
no `.c.nif`). `node` is required; the directory carries `hastur.mode = skip` so
this WIP suite stays out of the default `all` sweep but runs when pointed at
directly (the `dagon`/`jsbackend` pattern).

- **`tcompute`** — arithmetic, `if`/`while`, `break` (via `jmp`→`block`+`br`),
  direct calls and recursion (`fib`), `int64` (`i64.mul`) and `float64` — no
  memory.
- **`tmemory`** — a `Point{x,y,z: int32}` laid out by the driver in the shared
  `WebAssembly.Memory` (the same `ArrayBuffer` model as the JS backend's `mem`);
  `dot`/`manhattan` read fields and `setY` writes one, each an `iN.load`/`store`
  at the **`jslayout`** byte offset — proving the layout core is shared.
- **`tconstruct`** — objects and arrays *constructed by the module itself* in
  bump-allocated linear memory: `oconstr`/`aconstr`, field stores, value-semantic
  copy (`memory.copy`), and array indexing whose bounds check (`nimIcheckB`) is
  **linked in as real WASM** from the system module.
- **`tglobals`** — module-level globals as memory slots: state persists across
  exported calls in the shared buffer.
- **`techo`** — a whole *program* (`echo "hello world"`): `main` + its whole
  cross-module closure run under Node, the C stdio primitives (`fwrite`/`fputc`)
  provided as host imports, the string materialised from a data segment.
