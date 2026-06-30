# JavaScript backend (lengc `js`)

A third codegen over the **Leng** IR, alongside the C (`codegen.nim`) and LLVM
(`llvmcodegen.nim`) backends. Selected with `lengc js file.c.nif`; output is a
`.js` file in the nimcache dir.

JavaScript is dynamically typed and garbage collected, so the pass drops all
type generation and maps Leng constructs directly to JS:

| Leng | JavaScript |
|------|------------|
| `(proc :f (params (param :a . T)) RET . (stmts …))` | `function f(a) { … }` |
| `(add T a b)` (typed binop) | `(a + b)` — leading type operand skipped |
| `(div T a b)` | `Math.trunc(a / b)` (Nim int `div` truncates toward zero) |
| `(lt a b)`, `(and a b)`, `(not a)` | `(a < b)`, `(a && b)`, `(!a)` |
| `(asgn x e)` / `(store e x)` | `x = e;` |
| `(if (elif c (stmts …)) (else (stmts …)))` | `if (c) { … } else { … }` |
| `(while c (stmts …))` | `while (c) { … }` |
| `(case sel (of (ranges v) …) (else …))` | `switch (sel) { case v: … }` |
| `(raise e)` / `(try …)` | `throw e;` / `try { … }` (native) |
| `(ret e)` | `return e;` |
| `(oconstr T (kv f v) …)` | `{f: v, …}` — object literal (field key = mangled field sym) |
| `(aconstr T e0 e1 …)` | `[e0, e1, …]` — array literal |
| `(dot obj f)` | `obj.f` — field selection |
| `(at arr i)` / `(pat p i)` | `arr[i]` — array / pointer indexing |
| `(addr x)` / `(deref x)` | `x` — JS values are references, so addr/deref are identity |

## Scope

This is the **M0/M1 pure-compute slice plus data structures**: procs,
locals/globals, assignment, calls, returns, integer/float/bool/string literals,
the arithmetic / bitwise / comparison / logic operators, `if`, `while`, `case`,
`break`, **object construction + field access, array construction + indexing,
and address-of/deref (identity under JS reference semantics)**. With these the
lowered object/array shapes — including the object representation of a string
literal — generate cleanly (`echo "hello world"` now produces zero `/*TODO*/`s,
referencing only the still-missing runtime functions). Anything outside the
subset (seqs growth, pointers used as raw memory, the GC runtime) emits a
`/*TODO:<tag>*/` marker and is skipped, so generation always completes and the
coverage gap is visible in the output and reported on stderr.

The remaining **M2+** work is the runtime: the system-module FFI procs
(`write`/`stdout`/`nimFlushStdStreams`) need JS shims, and string values use the
lowered SSO representation (`{bytes, more}`) which a `write` shim must decode.
This still depends on the open design question of whether JS consumes
fully-lowered Leng or a higher-level Leng with the destructor passes gated off
for the GC target — but the codegen for the data shapes themselves is now in
place and design-neutral.

## Test

`bash tests/jsbackend/run.sh` — golden-diffs two hand-authored, self-contained
Leng modules (no system deps) and runs each under Node:
- `tcompute.c.nif` (pure compute) → checks `add(2,3)==5`, `fib(10)==55`,
  `fib(20)==6765`.
- `tdata.c.nif` (data structures) → checks `mkpoint(3,4)==7` (object construction
  + field access) and `arrsum()==60` (array construction + indexing).
