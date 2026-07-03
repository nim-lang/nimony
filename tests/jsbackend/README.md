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

JavaScript is dynamically typed and garbage collected, so the pass drops all
type generation and maps Leng constructs directly to JS:

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
| `(oconstr T (kv f v) …)` | `{f: v, …}` — object literal (field key = mangled field sym) |
| `(aconstr T e0 e1 …)` | `[e0, e1, …]` — array literal |
| `(dot obj f)` | `obj.f` — field selection |
| `(at arr i)` / `(pat p i)` | `arr[i]` — array / pointer indexing |
| `(addr x)` | `[x, 0]` — `x` is a boxed local (1-element array) |
| `(addr o.f)` / `(addr a[i])` | `[o, "f"]` / `[a, i]` — fat `[base, key]` pointer |
| `(deref p)` | `p[0][p[1]]` — read/write through the fat pointer |
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
lowering), all exercised end-to-end by the tests below. Allocation is a bump
arena over the shared buffer with **no free/GC yet** — that (and whole-program
`{.bundle.}` wiring + JS interop) is the remaining open design question, not
missing codegen.

**Addresses.** JS cannot reference a variable's storage, so a pointer cannot be
the value itself. Following the classic Nim JS backend (`mapType`'s
`etyBaseIndex` "fat pointers"), a pointer is a `[base, key]` pair such that
`base[key]` is the pointee's storage. A pre-pass boxes every local whose address
is taken into a 1-element array, so `let x = init;` becomes `let x = [init];`,
each use of `x` becomes `x[0]`, and `(addr x)` becomes `[x, 0]`. The address of
a field is `[obj, "field"]` and of an element is `[arr, idx]`; `(deref p)` is
`p[0][p[1]]`. Writes through a pointer therefore mutate the underlying storage.
The pairs `(addr (deref p))` and `(deref (addr loc))` cancel, keeping the common
cases compact. Two fat pointers compare component-wise via a small `nimPtrEq`
helper (emitted only when used), since a fresh `[base,key]` array is never `===`
another; `p == nil` stays a plain comparison (a nil pointer is `null`).

Taking the address of a *pointer-indexed* element (`addr (pat p i)`, i.e.
pointer arithmetic) or other raw-memory forms need more than a single base/key
pair and are out of scope for this slice; they emit a
`/*TODO:addr-of-location*/` marker. Anything else outside the subset (`sizeof`,
seqs growth, the GC runtime) likewise emits a `/*TODO:<tag>*/` marker and is
skipped, so generation always completes and the coverage gap is visible in the
output and reported on stderr.

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
