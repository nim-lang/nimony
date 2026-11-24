# Porting Nim Code to Nimony

How to take an existing Nim 2 module (application code, a third-party
library, or a stdlib module) and get it to compile and run under Nimony.
[differences.md](differences.md) lists the language differences; this
document lists the fixes.

Nimony is stricter than Nim 2 in most places and more lenient in a few.
Most of the work is making implicit assumptions explicit. Code that
already passes `--styleCheck:error`, `--strictDefs:on`,
`--strictFuncs:on` and `--experimental:strictEffects` and runs under
ARC/ORC needs much less of it.

The sections are ordered roughly by when you hit each problem.


## 0. Conditional gating: `when defined(nimony)`

Nimony-only fixes and Nim-only fallbacks go behind `when defined(nimony)`:

```nim
when defined(nimony):
  {.feature: "lenientnils".}
```

Use it for module-level features, `import` lists that differ between the
two compilers, and pragmas that only one side understands. Avoid it
inside routine bodies unless the algorithm itself differs; if only a type
changes, adjust the type once at the top.


## 1. Compatibility features

Most Nim 2 behaviors that Nimony dropped can be restored per module with
a `{.feature: "name".}` statement at module top level. Features do not
propagate to importers. The full list is in
[language.md](language.md#feature-pragma); the ones porting needs most:

| Feature | Restores |
|---|---|
| `"lenientnils"` | nilable `ref`/`ptr`/`pointer`/`cstring`/proc types |
| `"untyped"` | untyped templates and generics |
| `"ignoreStyle"` | partial case insensitivity |
| `"anonBlockBreaks"` | an unlabeled `break` leaving a `block` |
| `"assumeSync"` | unrestricted access to mutable globals |
| `"lenientAliasing"` | `f(x, x)` where one parameter is `var` |
| `"varToverloads"` | overloading on `var T` |
| `"autoclosures"` | implicit `.closure` for nested routines |
| `"canraise"` | raising without a `.raises` annotation |

`{.feature: "v2".}` turns on all compatibility features except
`canraise`. It is the quickest way to get a module compiling; then remove
features one by one.


## 2. The effect system

Nimony overhauled Nim's effect system.

### `func` vs `proc`

- `func`, `iterator` and `converter` are `noSideEffect` by default.
- `proc` is side-effecting by default.
- There is no inference. Marking a `proc` `.noSideEffect` is the only way
  to call it from a `func`.

`{.borrow.}` does not carry `noSideEffect` over from the base type's
operator either. With

```nim
proc `==`*(a, b: MyDistinct): bool {.borrow.}
```

calling `==` from a `func` (or from a generated object `==` that contains
a `MyDistinct` field) fails with `cannot call a routine with side effects
from a .noSideEffect context`. Declare borrowed operators with `func`.

### Suppressing the check locally

```nim
func arcInc*(memLoc: var int) {.inline.} =
  {.cast(noSideEffect).}:
    discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)
```

`cast` accepts `noSideEffect`, `uncheckedAssign`, `uncheckedAccess` and
`assumeSync`; anything else is an error.

### Iterators that do I/O

Iterators are `noSideEffect` by default, so an iterator that calls
`echo` or reads a file needs an annotation:

```nim
iterator lines*(f: File): string {.sideEffect, raises.} =
  ...
```

### `tags` and `gcsafe` are ignored

`{.tags: [...].}`, `{.gcsafe.}` and `{.raises: [].}` can stay; Nimony
ignores them. `raises` with a non-empty list is an error, see section 3.


## 3. Exception handling

The common case is an `ErrorCode` enum value instead of an exception
object:

| | Nim 2 | Nimony |
|---|---|---|
| Exception | object inheriting `Exception` | value of the `ErrorCode` enum |
| Raise | `raise newException(IOError, "msg")` | `raise IOError` |
| Catch | `except IOError as e: e.msg` | `except ErrorCode as e:` then `e == IOError` |
| Default for procs | may raise | does not raise |
| Annotation | `{.raises: [].}` to forbid | `{.raises.}` to allow |

`ErrorCode` lives in
[lib/std/errorcodes/errorcodes.nim](../lib/std/errorcodes/errorcodes.nim).
It is a flat enum (`Success`, `Failure`, `IOError`, `OSError`,
`ValueError`, `KeyError`, `OverflowError`, ...): there is no inheritance, and
raising allocates nothing.

When porting:

- `except IOError:` is an error, because `IOError` is a value, not a type.
  Write `except ErrorCode as e:` and test `e`, typically with `case e`.
- There is no `e.msg`. If you need context, build the message at the
  catch site: `quit "createIndex failed: " & infile`.
- Replace `raise newException(IOError, msg)` with `raise IOError`.
- A bare `except:` catches everything.
- `{.raises: [IOError, OSError].}` does not compile. Write `{.raises.}`.

Exception objects still exist for code that needs to carry data. A
routine may raise `ErrorCode` or one `ref` type with RTTI, not both, and
there is no `CatchableError`; inherit from `Exception`:

```nim
type ParseError = ref object of Exception

proc parse(s: string): int {.raises: ParseError.} =
  raise newException(ParseError, "unexpected end")

try:
  discard parse("x")
except ParseError as e:
  echo e.msg
```

Nothing converts between `ErrorCode` and `ref` exceptions; catch one and
raise the other explicitly.

### Don't propagate `{.raises.}` virally

Marking a leaf proc `.raises` forces every transitive caller to be
`.raises` too or to catch. Catch locally in whatever proc has a sensible
error-handling story instead:

```nim
proc runEval(...): string =
  result = ""        # see "Init checking" below
  try:
    ...
  except:
    result = "error: ..."
```

For procs that should abort the program on failure, wrap the call in
`try/except: quit "FAILURE: ..."`. [src/lib/compat2.nim](../src/lib/compat2.nim)
provides `onRaiseQuit(call)` for this: a pass-through under Nim, a
try/except/quit wrapper under Nimony. Keep `{.raises.}` (or `{.canRaise.}`
from compat2) for leaf utilities whose callers already handle errors.

### Init checking after `try`

If `result` is assigned only inside a `try:` body that may raise before
the assignment, the init checker reports `result` as possibly
uninitialized. Assign a default at the top of the proc.


## 4. Nilability: `nil ref/ptr T`

`ref`, `ptr`, `pointer`, `cstring` and proc types are not nil by default.
See [lenientnils.md](lenientnils.md) for the rules. In short:

- To allow `nil`, write `nil ref T` (likewise `nil ptr T`, `nil pointer`,
  `nil cstring`, `nil proc(...)`).
- An object constructor must initialize every not-nil field.
- Dereferencing a `nil` type requires a preceding nil check.
- There is no `isNil` for `ref`/`ptr`; write `x == nil`. (`isNil` exists
  only for `cstring`.)

Where this is too invasive, opt out per module with
`{.feature: "lenientnils".}`. Many ported stdlib modules do so
(`system`, `syncio`, `streams`, `osproc`, `dirs`, `parseutils`,
`strutils`, `json`, `widestrs`, ...). It is a stop-gap; new code should use
`nil T` annotations.

Two traps:

- The field type and the parameter types of the APIs that take the field
  must agree. If a object field may be `nil` (or a `ptr` field is left
  defaulted), the field needs `nil ptr X`, and so does every parameter it
  is passed to.
- `[T: nil ref]`, `[T: nil ptr]` and `[T: nil (proc)]` work as generic
  constraints; `lib/std/system/defaults.nim` defines `default` for them.


## 5. Init checking

Every variable, every `result` and every not-nil field must be assigned
before use. Typical fixes:

- A buffer that the OS or a C function fills in: mark it `{.noinit.}`.

  ```nim
  var f {.noinit.}: WIN32_FIND_DATA  # filled in by FindFirstFileW
  ```

- `result` assigned only in some branches: assign a default at the top
  (`result = default(T)` or a concrete value).


## 6. Mutable globals

A routine may not read or write a mutable global directly:

```nim
var counter = 0
proc bump = inc counter
# Error: unsynchronized access to global variable 'counter'
```

Access is allowed when the global is passed to a `var`/`ptr` parameter of
a `.sync` routine (atomics, lock operations) or inside
`{.cast(assumeSync).}:`. For single-threaded code ported from Nim,
`{.feature: "assumeSync".}` turns the check off for the module. Better
fixes are to pass the state as a parameter or to make the global a `let`
or `const`.


## 7. Strings

`string` has no terminating zero, so it does not convert to `cstring`
implicitly:

- `s.cstring` works only for string literals. For a variable use
  `toCString(s)`, which takes `var string` and appends the zero in place.
- `cast[cstring](addr s[0])` is broken.
- For raw read access use `readRawData(s, start = 0)`, which returns
  `ptr UncheckedArray[char]`. For bulk writes use
  `beginStore(s, newLen, start)` followed by `endStore(s)`.
- `rawData` exists for `seq` only.

These live in
[lib/std/system/stringimpl.nim](../lib/std/system/stringimpl.nim).

`strformat` is gone. Replace `fmt"abc {x}"` with `&("abc ", x)`.


## 8. Tables: `[]` raises

`Table.[]` is `.raises` because a missing key raises `KeyError`. In a
non-raising proc `t[k]` fails with "cannot call a routine marked as
`.raises`", and `addr t[k]` is rejected too.

For keys known to be present use `getOrQuit`, which returns `var V` and
aborts if the key is missing:

```nim
let valuePtr = addr getOrQuit(table, key)
```

compat2.nim provides `getOrQuit` for host Nim, so the same call compiles
under both.


## 9. Calls, templates and pragmas

### Parameterless routines need `()`

A bare identifier is the routine itself, not a call:

```nim
proc flush = ...
flush()      # OK
flush        # error: expression of type `proc ()` must be discarded
```

This applies to procs and templates alike. At module top level the error
message is `undeclared identifier` instead.

### Dirty templates that use caller-scope identifiers

A `{.dirty.}` template whose body refers to identifiers from the caller's
scope (a nested template, a local `let`, `result`, ...) needs
`{.feature: "untyped".}`. Without it the identifiers are resolved at the
template's declaration, fail, and the call sites report errors such as
"undeclared field for type auto".

```nim
when defined(nimony):
  {.feature: "untyped".}

template evalFloatUnOp(...) {.dirty.} =
  let a = propagateError eval(c, n)   # `propagateError` is local to
  ...                                 # the caller's `proc eval`
```

Generic declarations remember the module's features, so cross-module
instantiation needs nothing extra.

### Custom pragmas

`template myAnnotation() {.pragma.}` declares an annotation-only pragma
as in Nim; attaching `{.myAnnotation.}` to a declaration is accepted and
has no effect.


## 10. Identifier casing

Identifier lookup is case sensitive. Nim's rule (only the first letter is
case sensitive, underscores are ignored) applies only with
`{.feature: "ignoreStyle".}`. Without it, spell every symbol exactly as
declared.

For compiler-internal code, the generated tag enums (`NimonyExpr`,
`NimonyType`, ...) spell a tag with its first letter upper-case, the rest
of the tag name verbatim, and an upper-case kind suffix: `setconstr` is
`SetconstrX`, not `SetConstrX`; `proctype` is `ProctypeT`. Grep
`src/models/*_tags.nim` for the exact name.


## 11. Compile-time evaluation

The compiler folds magics and routines with a `.semantics:` pragma
itself. Any other call in a `const` initializer is evaluated by compiling
and running a small wrapper program, which costs a few tenths of a second
per call site (more on a cold cache).

- A trivial wrapper proc called from many `const` initializers
  (`createXint(1'u64 shl 8)`) adds up. Make it a `template` so the
  compiler sees the expression directly.
- For a lookup table built by a loop, use an inline formula if one
  exists, or accept the cost.


## 12. Generics

Generic bodies are checked when they are declared. An unconstrained `T`
supports only what every type supports, so a body that calls `$x`,
`x == y` or `hash(x)` must name these operations in the constraint:
`Keyable` (`==` and `hash`), `Ordinal`, `Stringable` and so on, or a
`concept` for user-supplied operations. Constraints compose:
`[A: Ordinal and Stringable]`. `{.feature: "untyped".}` turns the check
off, but a ported generic container in `lib/std/` must not use it.

Implicit generics (`proc p(x: SomeInteger)` without a `[T]` section) are
not supported; write `proc p[T: SomeInteger](x: T)`.


## 13. Alias checking

A call that passes the same location to two parameters, one of them
`var`, is rejected:

```nim
coerceValue(c, lhs, srcType, destType, lhs)   # `lhs` is both in and out
addOverflowDecl(c, c.code, pos)               # `c.code` is a field of `c`
```

with `mutable argument aliases with immutable parameter` (or `... with
mutable parameter`). Both shapes are common in Nim code. Fixes:

- In/out through one variable: copy into a temporary, then assign back.
  If this happens at several call sites, write an `inPlace` helper.
- A container passed alongside its owner: don't pass the container;
  return the value and let the caller store it.

`{.feature: "lenientAliasing".}` turns the check off.


## 14. Missing and changed stdlib API

`lib/std` is a reimplementation, not a copy, so some things are missing.
The common ones:

| Nim | Nimony |
|---|---|
| `echo` without imports | `import std/syncio` |
| `strutils.toOctal`, `toBin` | port it |
| `x.isNil` on a `ref`/`ptr` | `x == nil` |
| `system.insert(s, item, i)` | `sequtils.insert(s, [item], i)` |
| `$aDistinct` | `$Base(x)`; `$` is not borrowed automatically |
| `strformat.fmt` | `&` on tuples |

`system` provides `$` for sequences, arrays, open arrays, sets, tuples
and slices. Objects have no generic `$`; define one per type.
`char`/`cstring` conversions are in `strutils`, `typedesc` helpers in
`typetraits`.

If the missing piece is a small general routine (`strutils.join` was
one) or a whole general-purpose module (`std/packedsets`), port it to
`lib/std/` with a test in `tests/nimony/stdlib/` rather than writing it
out at each call site. Niche helpers stay local.

Recurring shapes when porting a stdlib module:

- `File = object`, not a ref. `open(out File, ...)` returns `bool`.
  `readBuffer`/`writeBuffer` exist.
- Use `noinit` for buffers the OS fills in (`stat`, `WIN32_FIND_DATA`,
  `Pthread_attr`, `CpuSet`).
- For working bindings (importc structs, `posix_spawn`, Win32
  `STARTUPINFO`) see `lib/std/posix/posix.nim` and
  `lib/std/windows/winlean.nim`.


## 15. The `compat2.nim` shim

[src/lib/compat2.nim](../src/lib/compat2.nim) lets the same source compile
under Nim and Nimony. It is `include`d, not imported, so every file gets a
private copy and there is no cross-module ambiguity. It provides:

- `{.pragma: canRaise.}`: `raises` under Nimony, nothing under Nim.
- `onRaiseQuit(call)`: try/except/quit under Nimony, pass-through under
  Nim.
- `getOrQuit` for `Table`/`OrderedTable` under host Nim.
- `path(s)` under host Nim, matching Nimony's `dirs`/`paths` API.
- Empty concepts (`Equatable`, `Keyable`, `HasDefault`, ...) under host
  Nim, so constrained generics compile there.
- `uncheckedBorrow` as a no-op under host Nim.

Add to it when the same compat hack shows up a third time, and only what
the bootstrap needs.


## 16. Build and test

```sh
nim c -r src/hastur/hastur build nimony  # build the toolchain
bin/hastur build nimony                  # rebuild after compiler edits
bin/hastur test tests/nimony/<dir>/      # run a test directory
bin/hastur test <file.nim>               # run a single test
bin/hastur --overwrite test <file>       # accept the new expected output
```

A change to a pass that affects NIF output usually means re-overwriting
expected files in `tests/nimony/`; the diff is part of the review. To
diagnose a behavior change, run `hastur debug <file>` and inspect the
`.s.nif` and other files in `nimcache/`.

If tests fail after a change to `lib/std/system/stringimpl.nim`, delete
`nimcache/t*/`: the system module is rebuilt, but per-test caches are not
always.
