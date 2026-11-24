# The Future: Nim 3 (the good)

*Nimony* is the name of the work-in-progress implementation for **Nim 3**. The differences between Nim 3 and Nim 2 are listed here, briefly. [porting.md](porting.md) explains how to port code.

- Explicit `nil ref/ptr T` annotations. Benefit: no more null pointer crashes at runtime. See [lenientnils.md](lenientnils.md) for details.
- `proc mygeneric[T: <ConceptHere>](...) = ...`. Generic bodies are checked at their declaration. Benefit: increased compile-time checking.
- Sum types: an object's `case` section can omit the discriminator field. The branch names then construct the object (`Value(val: 42)`) and `case n` matches on them (`of Value(val): ...`). Benefit: builtin pattern matching. Nim 2's `case kind: Kind` syntax still works.
- Reworked `async` / `await` that is based on continuations. It mitigates the "what color is your function" problem:
  - Instead of `async` write `passive`.
  - Instead of `await` write nothing at all.
  - Async event loop unified with multi-threading: `spawn` will be part of the event loop.
- Reworked exception handling. A routine raises either values of the `ErrorCode` enum or a single `ref` exception type: `raise IOError`, `{.raises.}`, `except ErrorCode as e`. Benefit: faster code, raising an `ErrorCode` allocates nothing.
- `tags` and `gcsafe` annotations are ignored. Benefit: no more "GC safety" related compiler errors.
- `ref` uses atomic reference counting by default (`--mm:atomicArc`), so the concept of "isolation" is not nearly as important.
- A routine cannot access a mutable global unless it goes through a `.sync` routine (atomics, locks) or `{.cast(assumeSync).}`. Use `{.feature: "assumeSync".}` to get Nim 2's behavior back on a per module basis.
- `string` has a different implementation and it lost its terminating zero. So it does not convert to `cstring` without copies anymore. For the following reasons:
  1. Even in Nim 2 this was not completely bulletproof: People cast between `seq[char]` and `string` which can lose the termination zero.
  2. It allows us to provide faster string slice operations.
  3. It saves memory.
- Compiler plugins replace macros as the way to transform code. Macros still exist for compatibility; each macro is compiled into a separate executable.
- Multi-methods are gone for good, use single dispatch methods.
- Cyclic module dependencies are allowed if an explicit `cyclic` import statement is used: `import module {.cyclic.}`.
- Nimony is case sensitive. Use the `{.feature: "ignoreStyle".}` statement to enable Nim 2's partial case sensitivity on a per module basis.
- A routine name without `()` is the routine itself, not a call: `flush` does not call `flush`, `flush()` does.
- An unlabeled `break` only leaves a loop, never a `block`; a block is left via `break <blockname>`. Use `{.feature: "anonBlockBreaks".}` to get Nim 2's behavior back on a per module basis.
- Signed integer overflow wraps around instead of raising `OverflowDefect`: `high(int) + 1 == low(int)`. Use `{.keepOverflowFlag.}` and `overflowFlag()` to detect an overflow.
- `range` types are checked at compile time by the contracts engine, never at run time. A value bound to `range[lo..hi]` must be provably within `lo..hi`: `var x: range[0..10] = 20` is rejected, `if a in 0..10: (var x: range[0..10] = a)` is accepted.
- A call may not pass the same location to a `var` parameter and to another parameter: `f(x, x)` is rejected when one of them is `var`. Use `{.feature: "lenientAliasing".}` to get Nim 2's behavior back on a per module basis.
- `strformat` is replaced by `&` on tuples, which concatenates the tuple's fields into a string: instead of `fmt"abc {x}\n"` write `&("abc ", x, "\n")`. Each field is appended via `addFormatted(result, field)`: strings and chars are added unquoted, numbers are written directly into the result, and any other type falls back to `$`. Overload `addFormatted` for a type whose `$` is a debug view to control how it appears inside `&`. Benefit: no string-literal mini language, the arguments are ordinary expressions that are type checked like any other.
- The side-effect system works differently:
  - `func`, `iterator` and `converter` are treated as `noSideEffect` by default.
  - `proc` is treated as `sideEffect` by default.
  - There is no `noSideEffect` inference.
  - The defaults can be overridden with explicit `.noSideEffect` and `.sideEffect` annotations.
- Overloading based on `var T` is gone as accessors become polymorphic instead: It is tracked if a write to a location like `a[i].x.z` is allowed. It is allowed if `a` is mutable. The builtin array indexing has always worked this way so this is a most natural extension. This means:

```nim
# Nim 2 code
func `[]`(x: var Container): var Element
func `[]`(x: Container): lent Element
```

is simplified to:

```nim
# Nimony
func `[]`(x: Container): var Element # polymorphic accessor
```

It also means we know in the type system if a container access can resize the container or not as the resize would still require the var-ness for the parameter!

Most of the Nim 2 behaviors listed here can be restored per module with a `{.feature.}` statement; `{.feature: "v2".}` enables all of them at once. See [language.md](language.md#feature-pragma).

Overall, Nim 3 will be more picky than Nim 2 in some ways, more lenient in others.


# The present: Nimony (the bad)

The first pre-release of Nimony misses lots of things but might be useful for your needs if you are the kind of person that can write his own standard library.

Nimony currently lacks these features:

- An exception handling system that is fully compatible with Nim's:
  - `raises: <list here>` is rejected; a routine raises `ErrorCode` (`{.raises.}`) or one `ref` type (`{.raises: ref MyError.}`).
  - `IOError`, `ValueError` etc. are `ErrorCode` values, not types, and carry no message. There is no `CatchableError`/`Defect` hierarchy; `ref` exceptions inherit from `Exception`.
  - `raises: []` is the default and ignored.
- Tag tracking: `tags` annotations are ignored. Tag mismatches should produce warnings in Nim 3 with the option to turn these warnings into errors.
- Implicit generics where routines can leave out the `[T]` section but instead are generic as they use a type class such as `SomeInt` are not supported in Nimony.


# Open questions (the ugly)

I am currently not aware of any "open questions", the design is complete.
