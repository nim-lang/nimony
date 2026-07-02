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
the covered subset with no new backend work — a survey of programs using enums
(→ ints), tuples (→ objects), `for` loops (→ `while` + iterator inlining), `case`
with multi-value branches, `set` membership, and `object … case` variants all
generate zero user-module `/*TODO*/`s and run correctly under Node. What is *not*
yet covered bottoms out in allocation/GC: `seq`s, `ref`/heap objects, and closure
environments (their `allocFixed(sizeof …)` still emits a `sizeof` TODO) — i.e. the
open design question, not missing codegen.

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

With `importc`/`exportc` resolved, the FFI boundary for a real `echo` is now
small and explicit: the generated code calls bare `fwrite`/`fprintf`/`fputc` on
`stdout`, and module init touches no allocator. Given a tiny JS runtime for those
externals, **integer `echo` already runs end-to-end** through the real std/system
+ std/syncio modules:

```
$ nimony c --nimcache:nc echoint.nim        # echo fib(10); echo 2+3
$ for f in nc/*/*.c.nif; do lengc js --nimcache:out "$f"; done
$ cat runtime.js out/sysvq0asl.js out/for2ybv4p1.js \
      out/syn1lfpjv.js out/<main>.js <(echo 'main(0,[]);') | node
55
5
```

(`runtime.js` defines `stdout`/`fprintf`/`fputc`; module order is irrelevant
since JS hoists function declarations and nothing runs until `main`.) Because an
unsupported node now emits a *valid* placeholder expression (`undefined/*TODO*/`),
a never-called function that contains one — e.g. the string `copyMem` path — no
longer breaks parsing of the whole bundle.

The one piece still missing for *string* `echo` is the data path: `write` lowers
to `readRawData(s)` (a Nim proc returning a pointer into either the inline SSO
bytes or the heap buffer) handed to `fwrite` as a raw byte pointer — and
byte-addressing into the packed SSO integer is the C-memory-model operation the
open design question is about (consume fully-lowered Leng vs. a higher-level Leng
with strings as JS strings). The integer path (`fprintf "%lld"`) has no such
dependency, which is why it runs today.

## Test

`bash tests/jsbackend/run.sh` — golden-diffs hand-authored, self-contained Leng
modules (no system deps, so the test needs only `bin/lengc` and `node`) and runs
each under Node:
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
- `tarith.c.nif` (checked arithmetic) → `keepovf`/`ovf` reduce to a plain
  assignment with `ovf` always false, and `store` assigns with reversed operands.
  Checks `checkedAdd(20,22)==42` and `storeTest()==42`.
- `timportc.c.nif` (foreign symbols) → an `importc` proc is not emitted and is
  called by its C name (`extTriple`, supplied as the runtime); an `exportc`
  global is emitted as `counter`. Checks `run()==21` and `counter==21`.
- `techo.c.nif` (end-to-end I/O) → mirrors how `echo <int>` lowers: Nim procs
  (`echoInt`/`run`) call `importc` stdio (`stdout`, `putInt`, `putChar`) supplied
  by a tiny runtime that captures output; asserts `run()` prints `"55\n5\n"`.
- `texc.c.nif` (exceptions) → mirrors how `try/except` lowers: a raising call
  returns an `ErrorCode`, `if canRaise.err` forward-`jmp`s to a handler in a dead
  `if (false)` landing pad. The `jmp`/`lab`s become `break` in nested labeled
  blocks. Checks `run(false)==42` (normal path) and `run(true)==-1` (handler).
