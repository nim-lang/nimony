# A `if a == nil or b == nil: return` guard must establish `a != nil` AND
# `b != nil` on the fall-through path. The `or` lowers to jmp/lab control flow
# rather than nested `if`s, so the not-nil facts have to survive the jump-target
# label join — see the per-label fact merge in contracts_fir's `bindKeyBoth`.

import std/syncio

type
  TT = ref object
    x: int

proc getIt(): nil TT = TT()

proc use(a, b: TT) =
  echo a.x + b.x

proc bug() =
  let a = getIt()
  let b = getIt()
  if a == nil or b == nil:
    return
  use(a, b)   # both a and b are proven non-nil here

bug()
