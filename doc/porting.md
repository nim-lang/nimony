# Porting Nim Code to Nimony

A practical guide for taking an existing Nim 2 module — application code,
a third-party library, or a stdlib module — and getting it to compile and
run under Nimony. Companion to [differences.md](differences.md), which
describes the design intent behind Nim 3; this document focuses on the
mechanical fixes you actually have to apply.

The big-picture rule: **Nimony is stricter than Nim 2 in most places and
more lenient in a few**. Most port effort is spent making implicit
assumptions explicit. If your code already passes `--styleCheck:error`,
`--strictDefs:on`, `--strictFuncs:on`, `--experimental:strictEffects`, and
runs cleanly under ARC/ORC, you have done about three quarters of the
porting work already.

The guide is organized roughly in the order you will hit each problem.


## 0. Conditional gating: `when defined(nimony)`

Most porting work involves either a Nimony-only fix or a Nim-only
fallback. The standard idiom is:

```nim
when defined(nimony):
  {.feature: "lenientnils".}
  # or any Nimony-only declaration
```

Use it for:
- Module-level features (`feature: "lenientnils"`, `feature: "untyped"`).
- `import` lists that differ between the two compilers.
- Pragmas that only one side understands.

Do **not** spread `when defined(nimony)` through the body of a function
unless the algorithm itself differs. For "the same code with a tweaked
type", prefer adjusting the type once at the top.


## 1. The effect system

Nimony rewrites Nim's effect system. The differences bite immediately.

### `func` vs `proc`

- `func`, `iterator`, and `converter` are `noSideEffect` by default.
- `proc` is side-effecting by default.
- **There is no inference.** Marking a `proc` `.noSideEffect` is the
  only way to put it in a `func` context.

In particular, `{.borrow.}` does **not** carry `noSideEffect` over from
the underlying type's operator. If you have

```nim
proc `==`*(a, b: MyDistinct): bool {.borrow.}
```

calling `==` from inside a `func` (or from an auto-generated object `==`
that contains a `MyDistinct` field) errors with
`cannot call a routine with side effects from a .noSideEffect context`.

Fix: write `func` instead of `proc` for any borrowed operator that needs
to be usable from `func` callers. Default to `func` for pure ops on
distincts.

### Suppressing the side-effect check locally

The escape hatch is `{.cast(noSideEffect).}:`:

```nim
func arcInc*(memLoc: var int) {.inline.} =
  {.cast(noSideEffect).}:
    discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)
```

Only `noSideEffect` is recognized inside the `cast` pragma — unknown
names are rejected by the compiler. `{.cast(uncheckedAssign).}` is
parsed but currently has no semantics.

### Iterators that do I/O

Iterators are `noSideEffect` by default. If the body calls something that
performs I/O, annotate the iterator explicitly:

```nim
iterator lines*(f: File): string {.sideEffect, raises.} =
  ...
```

### `tags` and `gcsafe` are ignored

You can leave existing `{.tags: [...].}` and `{.gcsafe.}` in place — the
Nimony compiler ignores them. The same is true for `raises: []` (which is
the new default), but `raises: [SomeException]` is mapped to
`{.raises.}` and propagates as such. See section 2.


## 2. Exception handling

Nim and Nimony model exceptions completely differently:

| | Nim 2 | Nimony |
|---|---|---|
| Throwable | object inheriting `Exception` | enum value of `ErrorCode` |
| Construction | `newException(IOError, "msg")` | `raise IOError` |
| Catch shape | `except IOError as e: e.msg` | `except IOError:` (no `as`, no `.msg`) |
| Default for procs | may raise | non-raising |
| Opt-out / opt-in | `{.raises: [].}` to disable | `{.raises.}` to enable |

The `ErrorCode` enum lives in
[vendor/errorcodes/src/errorcodes.nim](../vendor/errorcodes/src/errorcodes.nim)
and contains a flat list of error values: `Success`, `IOError`,
`OSError`, `ValueError`, `KeyError`, `EndOfStreamError`, `OutOfMemError`,
…. There is no inheritance; raising allocates nothing.

When porting:

- Drop `as e` and `e.msg` from every `except` clause. If you need
  context, use a literal string at the catch site:
  `quit "createIndex failed: " & infile`.
- Convert `newException(IOError, msg); raise` to `raise IOError`.
- A bare `except:` is the catch-all idiom.

### Don't propagate `{.raises.}` virally

`raises` is contagious. Marking a leaf proc raises forces every
transitive caller to either also be raises or catch — which spreads
through entire codebases very fast. Prefer **catching locally** in
whatever proc has a sensible error-handling story:

```nim
proc runEval(...): string =
  result = ""        # explicit default — see "Init checking" below
  try:
    ...
  except:
    result = "error: ..."
```

For procs that should abort the whole program on failure, wrap the
raising call in `try/except: quit "FAILURE: ..."`. The `compat2.nim`
helper file ([src/lib/compat2.nim](../src/lib/compat2.nim)) provides
`onRaiseQuit(call)` for exactly this pattern; it is a pass-through under
host Nim and a try/except/quit wrapper under Nimony.

Reserve `{.raises.}` (or `{.canRaise.}` from compat2) for genuine leaf
utilities whose callers already handle raising.

### Init-checking caveat

When the body of a `try:` is the only place `result` gets assigned and
the body may raise before reaching the assignment, Nimony's init checker
flags `result` as possibly uninitialized. Add an explicit default at the
top of the proc (e.g. `result = ""`).


## 3. Nilability: `nil ref/ptr T`

Nimony makes `ref`, `ptr`, `pointer`, `cstring`, and procedural types
**not-nil by default**. See [lenientnils.md](lenientnils.md) for the
full rules; the porting summary is:

- A field or local of `ref T` cannot be `nil`. To allow `nil`, write
  `nil ref T` (also `nil ptr T`, `nil pointer`, `nil cstring`,
  `nil proc(...)`).
- An object constructor must explicitly initialize every not-nil field.
- Dereferences of `nil`-typed values must be guarded by a nil check.

For modules ported from Nim where this would be invasive, opt out of
strict nil checking at module level:

```nim
when defined(nimony):
  {.feature: "lenientnils".}
```

This is what most ported stdlib modules do — `lib/std/system.nim`,
`syncio.nim`, `streams.nim`, `osproc.nim`, `dirs.nim`, `parseutils.nim`,
`strutils.nim`, `widestrs.nim`, `rawthreads.nim`, `threadpool.nim`. It
is a pragmatic stop-gap, not the goal: new Nimony code should use
`nil T` annotations explicitly.

Two practical traps when porting:

- The field type and the API parameter type must agree on nilability.
  If you assign `nil` (or leave a `ptr` defaulted) into a struct, the
  struct field needs `nil ptr X`, **and** every API that takes the field
  by value must accept `nil ptr X`.
- `[T: nil ref]`, `[T: nil ptr]`, and `[T: nil (proc)]` work as generic
  constraints. `lib/std/system/defaults.nim` provides the matching
  `default*[T: nil (proc)](typedesc[T]): T = T(nil)` template that
  Stream-style records rely on.


## 4. Init checking

Nimony enforces that every variable, every `result`, and every not-nil
field is assigned before use. Common porting fixes:

- Local arrays/tuples that the original code filled in element by
  element after declaration: annotate `{.noinit.}` to opt out, or
  initialize explicitly.

  ```nim
  var f {.noinit.}: WIN32_FIND_DATA  # filled in by FindFirstFileW
  ```

- `result` written only inside a branch: assign a default at the top
  (`result = default(T)` or a concrete value). See section 2 for the
  try/except corollary.


## 5. Strings

Nimony's `string` no longer carries an automatic terminating zero. This
shows up as:

- `s.cstring` requires explicit conversion (`s.toCString` for the
  mutating, null-terminating variant; the `cstring` overload otherwise).
  Code that did `cast[cstring](addr s[0])` is broken.
- For raw access, use `readRawData(s, start = 0)` which returns
  `ptr UncheckedArray[char]`. The mutating side uses
  `beginStore(s, newLen, start)` + `endStore(s)`.
- `rawData` is exported only on `seq`, not on `string`.

When porting code that did fancy string slicing or wrote into the
underlying buffer, look for the `beginStore`/`endStore` API in
[lib/std/system/stringimpl.nim](../lib/std/system/stringimpl.nim) and
the `readRawData` accessor.


## 6. Tables — `[]` raises

Nimony's `Tables.[]` is marked `{.raises.}` because a missing key
raises `KeyError`. That propagates: in any non-raising proc, `t[k]`
errors with "cannot call a routine marked as `.raises` …". `addr t[k]`
is also rejected.

For known-present keys, use `getOrQuit`:

```nim
let valuePtr = addr getOrQuit(table, key)
```

`getOrQuit` returns `var V` and aborts on the unreachable-missing
branch. `compat2.nim` provides shims so the same call works under host
Nim too — see [src/lib/compat2.nim](../src/lib/compat2.nim).

## 7. Templates and pragmas

### Parameterless templates need explicit `()`

```nim
template emitArgs = ...
emitArgs()      # OK
emitArgs        # error: "expression of type proc() must be discarded"
```

Nim treats both forms as a call. Nimony resolves the bare form to a
proc value. Always call parameterless templates with `()`.

### Dirty templates that capture caller-scope idents

A `{.dirty.}` template whose body references identifiers defined in the
caller's lexical scope (a nested template, a local `let`, `result`, …)
must be compiled with `{.feature: "untyped".}` at module level. Without
it, sem tries to resolve those identifiers at template-declaration
time, fails, collapses the body to `auto`, and you get cascading
"undeclared field for type auto" errors at the call sites.

```nim
when defined(nimony):
  {.feature: "untyped".}

template evalFloatUnOp(...) {.dirty.} =
  let a = propagateError eval(c, n)   # `propagateError` is local to
  ...                                 # the *caller's* `proc eval`
```

Module-level features auto-stamp themselves onto generic decls so they
travel through cross-module instantiation. You don't have to do anything
special for that — just remember to write the `{.feature.}` line.

### Custom-pragma templates work

`template myAnnotation() {.pragma.}` declares an annotation-only pragma
exactly as in Nim. Attaching `{.myAnnotation.}` to any later decl is
silently accepted and dropped — same as Nim, where the template body
is never expanded for `{.pragma.}` templates either.


## 8. Identifier casing

Nimony is **case-sensitive** for identifier lookup. Nim's partial
case-insensitivity (everything-lowercase-except-the-first-letter) does
not apply. Refer to symbols by their exact declared casing.

For the auto-generated tag enums (`NimonyExpr`, `NimonyType`, …), the
canonical form is

```
<Upper first letter><tag-name verbatim, lowercase except first><Upper suffix>
```

So the constant for the `setconstr` tag is `SetconstrX`, **not**
`SetConstrX`; for `proctype` it is `ProctypeT`; for `oconstr` it is
`OconstrX`. When porting compiler-internal code, grep
`src/models/*_tags.nim` for the canonical name.

## 9. Compile-time evaluation

Nimony's `expreval` only folds magics and ops with a `.semantics:`
pragma; everything else routes through a full sub-compile of a wrapper
program to produce the constant. Each shell-out costs ~5 seconds even
on a warm cache.

Watch for:

- Trivial wrapper procs called from `const` initializers
  (`createXint(1'u64 shl 8)`, `mySimpleOp(x)`). Convert the proc to a
  `template` so `expreval` sees the constructor directly.
- 256-entry lookup tables built with a const block + for loop. Replace
  with an inline formula if one exists, or accept the cost.


## 10. Stdlib porting cheat sheet

These are the recurring shapes you will hit when porting a stdlib
module from Nim 2 to `lib/std/`:

- File API: `File = ptr CFile` (not a ref). `open(out File, …)` returns
  bool. `readBuffer`/`writeBuffer` exist; the older `nimSlice` style
  does not.
- IO that raises: import `syncio`. `IOError` etc. are `ErrorCode`
  values, not types.
- `tags: [ReadIOEffect]` etc. continue to work (they are erased).
- `noinit` variables for buffers that the OS fills in (`stat`,
  `WIN32_FIND_DATA`, `Pthread_attr`, `CpuSet`).
- `var s: var string` for procs that mutate the buffer in-place; pass a
  literal through a `var cmd = literal` local first.
- Forward decls inside `when (windows|posix):` blocks: don't use them
  for `exportc`'d procs — Nimony's codegen has been observed to drop the
  body and emit only the forward signature. Inline the implementation
  directly into the `when` branch.

For new bindings (importc structs, `posix_spawn`, Win32 `STARTUPINFO`,
…), see `lib/std/posix/posix.nim` and `lib/std/windows/winlean.nim`
for working examples. Notable detail: opaque importc structs are fine
declared as empty `object`; Nimony emits the libc typedef so `sizeof`
works.

For `char**` style C interop, declare a local alias

```nim
type CChar {.importc:"char", nodecl.} = char
type CCharArray = ptr UncheckedArray[ptr CChar]
```

and `cast[CCharArray](myCstringArray)` at the call boundary. Nimony's
`cstring = NC8*` triggers `-Wincompatible-pointer-types` against
libc's `char**` otherwise.


## 11. The `compat2.nim` shim

[src/lib/compat2.nim](../src/lib/compat2.nim) is the bootstrap-time
compat layer that makes the same source compile under both Nim and
Nimony. It is `include`d, not `import`ed, so each user file gets its
own private copy and there is no cross-module template ambiguity. It
provides:

- `{.pragma: canRaise.}` — `raises` under Nimony, no-op under Nim.
- `template onRaiseQuit(call: untyped)` — try/except/quit wrapper under
  Nimony, pass-through under Nim. Lets you call a `.raises` proc from
  a non-raising context with a useful diagnostic on failure.
- `proc getOrQuit*[A,B](t: Table[A,B] | OrderedTable[A,B], k: A)` —
  host-Nim shims of the corresponding Nimony stdlib procs (mutable and
  read-only variants).

Reach for it whenever you find yourself writing the same compat hack in
a third place. Don't add Nimony-only or Nim-only sugar to it that is
not actually needed by the bootstrap.


## 12. Build and test

The build/test driver is `bin/hastur` (built from `src/hastur.nim`).
Common invocations:

```sh
nim c -r src/hastur build nimony       # build the toolchain
bin/hastur build nimony                # rebuild after compiler edits
bin/hastur test tests/nimony/<dir>/    # run a test directory
bin/hastur test <file.nim>             # run a single test
bin/hastur --overwrite test <file>     # accept the new golden output
```

When you change a pass that affects NIF output, expect to re-overwrite
golden files in `tests/nimony/`. The diff is part of the code review.
For codegen / behavioral tests, regenerate `nimcache/` (`hastur debug
<file>`) and inspect the `.s.nif` and other lowered NIF artifacts to
diagnose unexpected changes.

If a test fails after a `lib/std/system/stringimpl.nim` change, blow
away `nimcache/t*/` — the build system regenerates the system module's
C file but does not always cascade to per-test caches.
