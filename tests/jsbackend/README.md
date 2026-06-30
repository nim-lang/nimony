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
| `(addr x)` | `[x, 0]` — `x` is a boxed local (1-element array) |
| `(addr o.f)` / `(addr a[i])` | `[o, "f"]` / `[a, i]` — fat `[base, key]` pointer |
| `(deref p)` | `p[0][p[1]]` — read/write through the fat pointer |

## Scope

This is the **M0/M1 pure-compute slice plus data structures**: procs,
locals/globals, assignment, calls, returns, integer/float/bool/string literals,
the arithmetic / bitwise / comparison / logic operators, `if`, `while`, `case`,
`break`, **object construction + field access, array construction + indexing,
and address-of/deref**. With these the
lowered object/array shapes — including the object representation of a string
literal — generate cleanly (`echo "hello world"` now produces zero `/*TODO*/`s,
referencing only the still-missing runtime functions).

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
- `taddr.c.nif` (addresses, locals) → checks `through()==42` (write through a
  pointer to a boxed local), `usebump()==15` (mutation via a pointer parameter),
  and `addrparam(7)==99` (a value parameter whose address is taken is boxed at
  entry).
- `taddr2.c.nif` (addresses, aggregates) → checks `fieldaddr()==100` (write
  through the address of an object field), `elemaddr()==99` (through the address
  of an array element), and `samefield()`/`difffield()` (fat-pointer equality:
  same key vs different key).
