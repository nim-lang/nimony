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
wiring is the remaining native-path open item; **JS value interop now has its
foundation** (see below).

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

**JS value interop (`jsffi`).** Native Nim data lives in linear memory as byte
offsets; a genuine JS value (string, object, function, DOM node) cannot. So the
interop layer holds each such value in a runtime-side table and hands Nim an
integer **handle** — the same idea as the proc-pointer `_fns` table, generalised
to arbitrary values (slot `0` = `undefined`/`null`, mirroring nil). This rides
the existing `importc` seam: `jsffi.nim` declares body-less `importc` procs that
lower to calls of the runtime's bridge functions (`_strToJs`, `_jsGetProp`,
`_jsCall*`, …). On top of that seam it exposes an ergonomic surface —
`toJs`/`toStr`/`toInt`/`toFloat`/`toBool` marshalling (strings cross as UTF-8
bytes; a Nim `float` is already a JS Number on this `--bits:32` target so it
reuses the number bridge), `global`/`get`/`set`/`call` for globals, properties
and methods, `newOf` for `new Ctor(...)`, `newJsArray`/`add`/`[]`/`[]=`/`len`
for JS arrays (an `arr[i]` read interns a fresh owning handle; `push` hands the
array its own reference to the value, so releasing the Nim handle afterwards
never disturbs it), `jsTypeof`/`hasProp`/`instanceOf` for introspection, `apply`
for a variadic method call (any argument count, marshalled through a JS array),
and `==` as JS `===`. **Nim→JS callbacks** close the loop: a Nim
proc used as a value already lowers to an `_fns` table index, so `toJs(someProc)`
wraps that index in a JS function that marshals the incoming JS arguments to
handles — an `EventTarget`/`addEventListener` handler written in Nim fires back
into Nim and reads the event's properties (see `tevent`). This is the foundation
a DOM binding sits on. **`JsValue` is GC-integrated:** it is a one-field object
carrying ARC hooks, so `=destroy` releases its table slot and a copy (`=copy`/
`=dup`) allocates a *new* slot to the same JS value — every copy is independently
owned, there is no double free, and transient values (a method result you drop,
a member-name handle) are reclaimed at scope exit with no manual release (see
`tgc`, which loops 1000× with zero table growth). The FFI seam itself stays on
plain `int32` handles so no owning `JsValue` crosses an `importc`. (The 4-byte
object storage rides the same never-freed `allocFixed` bump the backend uses for
all value objects — the pre-existing GC/`#1518` gap — but the unbounded leak, the
value-table slots, is now closed.)

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
(sequence growth + iteration), `tptr` (`addr`/store-through-deref), `texc`
(nimony's heap-exception lowering: raise / try-except / resume), `tffi`
(JS value interop: marshal Nim strings/ints/bools to and from real JS values,
call host `console`/`Math`/`JSON`, read results back), `tevent` (Nim→JS
callbacks: a Nim proc registered as an `EventTarget` handler, fired by
`dispatchEvent`, reading the event back — the DOM event mechanism), `tgc`
(handle GC-integration: transient `JsValue`s reclaimed at scope exit, copies are
independent slots — 1000 iterations with zero table growth), `tarray`
(floats and JS arrays: `toFloat` round-trip, build/index/mutate a JS array from
Nim, hand it to `JSON.stringify` and `Array.prototype.join`), `tintro`
(introspection + variadic call: `jsTypeof`/`instanceOf`/`hasProp` and `apply`
of `Math.max` over a five-element argument array), and `tdom` (a real DOM tree
built from Nim through the `dom.nim` binding slice — `createElement`/`textContent`/
`appendChild`/`getElementById`/`querySelector` — plus a Nim proc wired as a
click handler and fired via `dispatchEvent`, all against a genuine WHATWG DOM),
and `turl` (a binding **generated** from official WebIDL — `URL` from
`@webref/idl` via `gen/idl2nim.js` — constructed, attributes read/written,
`toJSON` called; `URL` is a native Node global so no DOM env is needed), and
`tclasslist` (a generated `DOMTokenList` binding — `element.classList` — driving
the variadic `add`/`remove` and the optional-argument `toggle` overloads against
jsdom).

**DOM environment (`tdom`).** `document`/`window`/`HTMLElement` don't exist in
bare Node (only `EventTarget`/`Event` do — which is why `tevent` needs no shim).
So the DOM test runs against **jsdom**, a spec-compliant DOM implementation, set
up as globals by a per-test `tdom.env.js` preamble. jsdom is a **dev-only**
dependency (declared in `package.json`, `node_modules` git-ignored, not
vendored): run `npm install` in this directory once to enable the DOM test. The
runner prepends any `<test>.env.js` to that test's bundle and puts
`node_modules` on `NODE_PATH`; a test that has an `.env.js` but whose deps
aren't installed is **skipped** (not failed), exactly like the whole suite is
skipped when `node` is absent — so a bare checkout stays green. Testing the
bindings against jsdom's real DOM semantics (rather than a hand shim) is also
what makes this the reference target for the WebIDL-driven generator below.

**Spec-backed bindings (`gen/idl2nim.js`, `weburl.nim`, `turl`).** DOM/web
bindings are too large to hand-write, so `dom.nim` is really the *reference
shape* for a generator. `gen/idl2nim.js` reads the curated WebIDL that
`@webref/idl` ships (the W3C/WHATWG official interface data — 325 specs),
parses it with the standard `webidl2` parser, and emits a `jsffi` binding in
exactly the `dom.nim` style: an interface becomes a `JsValue` alias, attributes
become get/set procs, operations become `call` procs, and WebIDL types map to
Nim through jsffi's marshalling (`DOMString`→`string`, `boolean`→`bool`,
integer types→`int`, `double`→`float`, other interfaces→`JsValue`). **Optional
arguments** become one Nim overload per arity (so `new URL(url)` and
`new URL(url, base)` both exist; `toggle(token)` and `toggle(token, force)`
both exist), and a **variadic** tail becomes an `openArray` parameter spread
through `applyArgs` (so `classList.add("a", "b")` works). Constructs it still
doesn't handle (static ops, `>3`-arg fixed ops, iterables, member-name clashes
with jsffi's own verbs like `get`/`set`) are emitted as `## SKIPPED` comments
so the coverage gap is visible, not silent.

**Inheritance and mixins are flattened.** WebIDL spreads an interface's real
surface across an `inheritance` chain (`Element` → `Node` → `EventTarget`) and
across `interface mixin` blocks glued on by `includes` statements (`ParentNode`,
`ChildNode`, `Slottable`, …). Since jsffi's model is a flat `JsValue`, the
generator merges all of it onto the target — every ancestor's own members plus
the mixins each level `includes` — with most-derived-winning on name clashes
(attributes by name, operations by name+arity, so genuine overloads survive but
re-declarations don't). So a single generated `Element` carries `appendChild`
(from `Node`), `addEventListener`/`dispatchEvent` (from `EventTarget`),
`append`/`querySelector`/`children` (from the `ParentNode` mixin) and `remove`
(from `ChildNode`) directly — 103 members from six sources. Ancestors that live
in a different spec file are noted as unresolved rather than silently dropped.

`weburl.nim` and `element.nim` are checked-in generator output (`node
gen/idl2nim.js url URL weburl.nim`, `node gen/idl2nim.js dom Element
element.nim`; regenerate after `npm install`). `turl` exercises the flat `URL`;
`telement` drives the flattened `Element` against jsdom, hitting members from
its own interface, both ancestors, and three mixins in one test.
`webidl2`/`@webref/idl` are dev-only, like jsdom — the generated `.nim` is
committed, so `turl` needs neither them nor a DOM env. This is the path to
spec-backed coverage; adding `@mdn/browser-compat-data` later gates which
members a target actually supports.
