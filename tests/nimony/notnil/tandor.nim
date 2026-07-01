
# Regression for nil-tracking through `and`/`or` short-circuit guards
# (nim-lang/nimony#1984). A guard built from `and`/`or` of nil-checks lowers
# to a join cfvar; the per-disjunct nil facts used to be intersected away at
# the merge, leaving the guarded variables un-refined afterwards. Both the
# `or`-with-early-return form and the positive `and` form must now refine.

import std / syncio

type
  Node = ref object
    v: int

proc sum(a: Node not nil; b: Node not nil): int = a.v + b.v

proc viaOr(a, b: nil Node): int =
  # after the guard returns, both `a` and `b` are non-nil
  if a == nil or b == nil:
    return -1
  sum(a, b)

proc viaAnd(a, b: nil Node): int =
  # inside the positive guard both `a` and `b` are non-nil
  if a != nil and b != nil:
    return sum(a, b)
  -1

proc main =
  let x: nil Node = Node(v: 2)
  let y: nil Node = Node(v: 3)
  echo viaOr(x, y)
  echo viaOr(x, nil)
  echo viaAnd(x, y)
  echo viaAnd(nil, y)

main()
