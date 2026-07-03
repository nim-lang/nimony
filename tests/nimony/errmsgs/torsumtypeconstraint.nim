# An `or` (sum-type) constraint only guarantees the operations common to *all*
# of its alternatives (their intersection). `shared` is required by both
# concepts, so it resolves; `foo` is required only by `HasFoo`, so calling it on
# a value constrained by `HasFoo or HasBar` must be rejected — the type may
# satisfy only `HasBar`. A naive fix that unions the operations would wrongly
# accept `foo`. nim-lang/nimony#2029.

type
  HasFoo = concept
    proc foo(a: Self): int
    proc shared(a: Self): int
  HasBar = concept
    proc bar(a: Self): int
    proc shared(a: Self): int

# OK: `shared` is common to both alternatives.
func useShared[T: HasFoo or HasBar](a: T): int =
  shared(a)

# Error: `foo` is not guaranteed when the type may satisfy only `HasBar`.
func useFoo[T: HasFoo or HasBar](a: T): int =
  foo(a)
