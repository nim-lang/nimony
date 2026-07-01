# The Future: Nim 3 (the good)

*Nimony* is the name of the work-in-progress implementation for **Nim 3**. The differences between Nim 3 and Nim 2 are listed here, briefly:

- Explicit `nil ref/ptr T` annotations, benefit: no more null pointer crashes at runtime. See [lenientnils.md](lenientnils.md) for details.
- `proc mygeneric[T: <ConceptHere>](...) = ...`; benefit: increased compile-time checking.
- `case` inside `object` gets a new syntax. Benefit: Enables builtin pattern matching.
- Reworked `async` / `await` that is based on continuations. It mitigates the "what color is your function" problem:
  - Instead of `async` write `passive`.
  - Instead of `await` write nothing at all.
  - Async event loop unified with multi-threading: `spawn` will be part of the event loop.
- Reworked exception handling annotations. Benefit: Faster code.
- The effect system will be opt-in. Benefit: No more "GC safety" related compiler errors.
- `ref` will default to use atomic operations so the concept of "isolation" will not exist or not nearly be as important.
- `string` has a different implementation and it lost its terminating zero. So it does not convert to `cstring` without copies anymore. For the following reasons:
  1. Even in Nim 2 this was not completely bulletproof: People cast between `seq[char]` and `string` which can lose the termination zero.
  2. It allows us to provide faster string slice operations.
  3. It saves memory.
- Macros are replaced by compiler plugins which will offer a different API.
- Multi-methods are gone for good, use single dispatch methods.
- Cyclic module dependencies are allowed if an explicit `cyclic` import statement is used: `import (path / module) {.cyclic.}`.
- Nimony is case sensitive. Use the new `{.feature: "ignoreStyle".}` statement to enable Nim 2's partial case sensitivity on a per module basis.

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

Overall, Nim 3 will be more picky than Nim 2 in some ways, more lenient in others.


# The present: Nimony (the bad)

The first pre-release of Nimony misses lots of things but might be useful for your needs if you are the kind of person that can write his own standard library.

Nimony currently lacks these features:

- An exception handling system that is compatible with Nim's. Instead Nimony uses its own based on an `ErrorCode` enum.
- Nimony checks `range` subtypes at compile time: a statically-known value that
  does not fit a `range` formal is rejected (e.g. `var x: range[0..10] = 20`), and
  a narrower range is accepted where a wider one is expected (`range[2..5]` is a
  subtype of `range[0..10]`). Runtime bounds enforcement for values only known at
  run time (e.g. `var x: range[0..10] = someInt`) is not yet emitted.
- Nim's effect system:
  - `tags` annotations are ignored. Tag mismatches should produce warnings in Nim 3 with the option to turn these warnings into errors.
  - `raises: []` is the new default and ignored. `raises: <list here>` is mapped to `.raises` (but this needs to be changed to an exception system compatible with Nim's).
  - `gcsafe` is ignored.
- Closure iterators.
- Implicit generics where routines can leave out the `[T]` section but instead are generic as they use a type class such as `SomeInt` are not supported in Nimony.


# Open questions (the ugly)

I am currently not aware of any "open questions", the design is complete.
