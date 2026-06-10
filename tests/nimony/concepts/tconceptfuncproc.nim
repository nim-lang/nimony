import std/assertions

## `func` implementations satisfy `proc` equality requirements.
## `proc` implementations with the `noSideEffect` pragma satisfy `func` requirements.
## Procs without `noSideEffect` cannot satisfy `func` requirements.
## Concept requirements that differ only by proc/func and parameter names deduplicate.

type
  ProcEq = concept
    proc `==`(a, b: Self): bool

  FuncEq = concept
    func `==`(x, y: Self): bool

  NeedsProcEq = concept of ProcEq
  NeedsFuncEq = concept of FuncEq
  NeedsBothEq = concept of ProcEq, FuncEq

type
  TestType = distinct int

  Inner = object
    v: int

  ProcNoSideEffectEq = distinct Inner
  SideEffectEq = distinct Inner

func `==`*(a, b: TestType): bool =
  int(a) == int(b)

proc `==`*(x, y: ProcNoSideEffectEq): bool {.noSideEffect.} =
  false

proc `==`*(x, y: SideEffectEq): bool {.sideEffect.} =
  false

proc checkBoth[T: NeedsBothEq](x, y: T): bool =
  x == y

assert TestType is NeedsProcEq
assert TestType is NeedsFuncEq
assert ProcNoSideEffectEq is NeedsFuncEq
assert not (SideEffectEq is NeedsFuncEq)
assert int is NeedsProcEq
assert checkBoth(TestType(1), TestType(1))

## Generic bodies must not see duplicate concept `==` requirements as ambiguous
type
  Foo[T: NeedsBothEq] = object
    r, s: T

func `==`*[T: NeedsBothEq](a, b: Foo[T]): bool =
  a.r == b.r and a.s == b.s
