## `Self` is what ties a concept requirement to the type being checked. A
## requirement that never names it must not be allowed to infer the concept's
## container parameter from a candidate, or every type satisfies the concept
## (issue #2430).

import std/assertions

type
  SparseColor = enum
    red = 0
    green = 2
    blue = 4

  HasFoo*[T] = concept
    proc foo(_: typedesc[T], ordinal: uint64): T

proc foo*(_: typedesc[uint8], ordinal: uint64): uint8 =
  uint8(ordinal)

# The requirement mentions no `Self` and leaves `T` open, so it states nothing
# about the checked type. Before the fix the free `T` bound to the builtin's
# `uint8` and *every* type satisfied `HasFoo`.
assert not (SparseColor is HasFoo)
assert not (int is HasFoo)
assert not (uint8 is HasFoo)

# and the call site agrees with the concept probe
assert not compiles(foo(SparseColor, 0'u64))
assert compiles(foo(uint8, 0'u64))

type
  # A requirement may omit `Self` once its typevars are bound: the first one
  # here binds `T`, so the second is checked against that binding.
  Findable*[T] = concept
    proc first(x: Self): T
    proc `==`(a, b: T): bool

proc first*(x: seq[int]): int = x[0]

assert seq[int] is Findable
assert not (seq[string] is Findable)
