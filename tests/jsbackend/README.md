# JavaScript backend (`lengjs`)

A codegen over the **Leng** IR, alongside the C (`codegen.nim`) and LLVM
(`llvmcodegen.nim`) backends. It is structured as a standalone `{.build.}`-
schedulable plugin rather than a `lengc` subcommand — Araq's "backends are
plugins, not codegens baked into the compiler" direction for PR #2043:

- **`lengjs <module.c.nif> <out.js>`** — the per-module backend: reads one
  module's lowered Leng IR and emits its `.js` artifact (the JS counterpart of
  `arkham` on the native path).
- **`jslink <linkmanifest.nif> <out.js> [runtime.js]`** — the `{.bundle.}`
  linker: assembles the runtime + per-module artifacts + entry call into one
  runnable bundle (the JS counterpart of `niflink`).

The JS target is treated as a **32-bit platform** (`--bits:32`): `int`/`uint`
map to a JS `Number`, and only `int64`/`uint64` map to `BigInt`. This is what
lets ordinary integer code stay fast Numbers while wide arithmetic stays exact.

Nim-native data (`object`/`array`/`string`/`seq`/`ref`) is laid out **as bytes
in one `ArrayBuffer`** — JS Typed Arrays used as byte-addressable linear memory
(Araq's direction on PR #2043) — so `cast`, unions and low-level libraries behave
exactly as on the C target. A pointer is a single integer **byte offset** into
that buffer; a new `jslayout` pass computes the C-ABI `sizeof`/alignment/field
offsets, and construction/field access lower to typed load/store calls
(`mem.setX(p+off, v)` / `mem.getX(p+off)`) on the runtime's `DataView`. Control
flow and scalar ops map directly; only the memory model is new:

| Leng | JavaScript |
|------|------------|
| `(proc :f (params (param :a . T)) RET . (stmts …))` | `function f(a) { … }` |
| `(add T a b)` (typed binop) | `(a + b)` — leading type operand skipped |
| `(div T a b)` | `Math.trunc(a / b)` for `int`; real `a / b` for floats; BigInt `a / b` for `int64`/`uint64` (Nim int `div` truncates toward zero) |
| `(lt a b)`, `(and a b)`, `(not a)` | `(a < b)`, `(a && b)`, `(!a)` |
| `(asgn x e)` | `x = e;` |
| `(store e x)` | `x = e;` (operands reversed vs `asgn`) |
| `(keepovf (op T a b) dst)` / `(ovf)` | `dst = (a op b);` / `false` (no 64-bit overflow trap) |
| `(if (elif c (stmts …)) (else (stmts …)))` | `if (c) { … } else { … }` |
| `(while c (stmts …))` | `while (c) { … }` |
| `(case sel (of (ranges v) …) (else …))` | `switch (sel) { case v: … }` |
| `(jmp L)` / `(lab L)` | `break L;` in a labeled block — try/except lowers (in Hexer) to an error-code ABI + a dead `if (false)` landing pad reached by a forward `jmp`; those map directly to `break` in nested labeled blocks, no relooper (Araq's PR #2043 direction) |
| `(ret e)` | `return e;` |
| `(oconstr T (kv f v) …)` | `allocFixed(sizeof T)` then a `mem.setX(p+off, v)` per field — a fresh buffer object, evaluated by an IIFE returning its offset |
| `(aconstr T e0 e1 …)` | `allocFixed(sizeof T)` then `mem.setX(p+i*stride, ei)` per element |
| `(dot obj f)` | `mem.getX(base + off)` — typed load at the field's byte offset (`off` from `jslayout`) |
| `(at arr i)` / `(pat p i)` | `mem.getX(base + i*stride)` — typed load at the element offset |
| `(addr x)` | an integer **byte offset**: a field is `base + off`, an element `base + i*stride`, an address-taken scalar local its own slot offset |
| `(deref p)` | `mem.getX(p)` — typed load at byte offset `p`; `(addr (deref p))` and `(deref (addr loc))` cancel |
| `(eq a b)` / `(nil)` | `(a === b)` — offsets compare as integers; `nil` is offset `0` |
| `importc "fwrite"` proc/global | referenced as `fwrite`; **not** emitted (runtime provides it) |
| `exportc "main"` proc/global | emitted as `main` (its C name) |

## Scope

This is the **M0/M1 pure-compute slice plus data structures**: procs,
locals/globals, assignment, calls, returns, integer/float/bool/string literals,
the arithmetic / bitwise / comparison / logic operators (incl. overflow-checked
`keepovf`/`ovf`), `if`, `while`, `case`, `break`, **object construction + field
access, array construction + indexing, and address-of/deref**. With these the
lowered object/array shapes — including the object representation of a string
literal — generate cleanly (`echo "hello world"` now produces zero `/*TODO*/`s,
referencing only the still-missing runtime functions).

Because these are all *lowered* Leng shapes, a range of surface Nim compiles to
the covered subset with no new backend work — enums (→ ints), tuples (→ objects),
`for` loops (→ `while` + iterator inlining), `case` with multi-value branches,
and `object … case` variants all run correctly under Node. Since bring-up the
slice has grown to cover **strings** (SSO + heap, `echo` and `$`), **`seq`s**,
**`ref`/heap objects**, **closures**, and **exceptions** (nimony's heap-exception
lowering), all exercised end-to-end by the tests below.

The heap is **Nim's own native allocator** — the ported `system/alloc.nim`
(TLSF page-chunk allocator), compiled to JS through `lengjs` like any other
module by building the stdlib with `--define:nimNativeAlloc` (the libc-free
config the native backend uses). The runtime supplies only `mmap`/`munmap` as
the page primitives it sits on, carved from the same `ArrayBuffer`; real `alloc`/
`dealloc`/`realloc` with free-list reuse run as Nim code, so there is no
JS-side bump shim. (`munmap` is a no-op — whole-page reclamation is deferred —
but the allocator reuses cells within its arenas.) Whole-program `{.bundle.}`
wiring + JS interop remain the open items.

**Addresses.** A pointer is a single integer **byte offset** into the shared
`ArrayBuffer` — the C memory model, one cast-transparent representation with no
fat-pointer/box form for `cast` to fail to see through. `addr o.f` is `p + off`,
`addr a[i]` is `p + i*stride`, `deref p` is a typed load at `p`, `p == q` is
integer `===`, and `nil` is offset `0`. The pairs `(addr (deref p))` and
`(deref (addr loc))` cancel, keeping the common cases compact. A local whose
address is taken has no JS storage to point at, so a pre-pass (`scanForAddr`)
spills it to a buffer slot — the C stack model — and every use goes through
`mem`, giving it a real byte address like any other datum. `addr (pat p i)`
(pointer arithmetic) is just `p + i*stride`, so it needs no special case.

Constructs still outside the supported subset emit a `/*TODO:<tag>*/` marker (a
valid `undefined` expression) and are skipped, so generation always completes and
the coverage gap is visible in the output and reported on stderr.

**Foreign symbols (`importc`/`exportc`).** A symbol's external (C) name is
resolved exactly as the C backend's `mangleSym` does — through the lazily-loaded
foreign-module declarations — so a reference works across modules. An `importc`
proc or global names an external entity: it is referenced by its C name (e.g.
`fwrite`, `stdout`) and **no** definition is emitted, leaving a small, explicit
boundary for a runtime to fill. An `exportc` symbol is emitted under its C name
(so `main` is `main`). Object-field keys are never treated as foreign — they
always use the mangled field name.

With `importc`/`exportc` resolved, the FFI boundary is small and explicit: the
generated code calls bare `fwrite`/`fprintf`/`fputc` on `stdout`, and `echo`
runs end-to-end for **every scalar type and for strings** through the real
std/system + std/syncio modules. `runtime.js` supplies the externals over a
single `ArrayBuffer`: `stdout`/`fprintf`/`fputc`, the typed load/store accessors
the linear-memory model reads and writes through, and a `memcmp`/`memcpy` pair.
Strings resolve their data path (`readRawData` into the SSO/heap bytes) against
that buffer, so both `echo "…"` and `$`-of-int print correctly.

## Test

The suite is a hastur **custom runner** (`setup.nim`, the mechanism from the
hastur rewrite in #2095): each `t*.nim` here is a *real program* compiled and run
end-to-end, its stdout diffed against the sibling `.output` golden.

```
hastur tests/jsbackend              # run the suite
hastur --overwrite tests/jsbackend  # regenerate the .output goldens
```

Per test the runner does: `nimony c --bits:32` (harvesting every module's
lowered `.c.nif`) → `lengjs` each module → concatenate `runtime.js` + the
artifacts + `main(0, [])` → `node` → diff. The trailing 32-bit C link fails on a
64-bit host and is ignored on purpose: the `.c.nif` is emitted by hexer *before*
the C backend, so a genuine front-end error is the one that leaves no `.c.nif`
behind (how the runner tells a real failure from the expected link stub). `node`
is required; the directory carries `hastur.mode = skip` so the WIP suite stays
out of the default `all` sweep but still runs when pointed at directly (the
`dagon` pattern).

The programs cover the backend end-to-end: `techo`/`tstrings` (scalars + string
building), `tarith` (integer ops that stay Numbers), `tfloat` (float arithmetic
+ real division), `tbig64` (`int64`/`uint64` → BigInt, exact 10¹⁸ and
two's-complement wraparound), `tconv` (8/16-bit narrowing, int↔BigInt,
int64↔uint64 signedness), `tcontrol` (if/while/for/case), `tproc` (recursion),
`tobject`/`tvariant` (object + tagged-union layout in linear memory), `tseq`
(sequence growth + iteration), `tptr` (`addr`/store-through-deref), and `texc`
(nimony's heap-exception lowering: raise / try-except / resume).
