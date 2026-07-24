# A compound `and`/`or` condition on an `if` with a *fall-through* body must
# establish the not-nil facts inside the body — not just in the leaving-guard
# form. The `!=` template expands to `(expr (not (== …)))`; xelim's short-circuit
# passthrough strips that trivial `(expr …)` wrapper so both the finalir
# condition compiler and the nil analysis see the pure `not (== …)` leaf.

proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

type
  TT = ref object
    x: int

proc getIt(): nil TT = TT()
proc use(a, b: TT) = printf("%d\n", a.x + b.x)

proc andCond() =
  let a = getIt()
  let b = getIt()
  if a != nil and b != nil:
    use(a, b)          # both proven non-nil inside the `and`

proc guard() =
  let a = getIt()
  let b = getIt()
  if a == nil or b == nil:
    return
  use(a, b)            # the classic leaving-guard `or` form

andCond()
guard()
