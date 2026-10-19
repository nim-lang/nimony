# Test: borrow states join correctly where branches meet. Nothing here may
# be reported; `tborrow_branch_errors.nim` has the rejected forms.

import std/syncio

proc reassignInBoth(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  if c: v = toOpenArray(s)
  else: v = toOpenArray(s)
  t.add 5   # `v` borrows `s` on every path
  echo v.len

reassignInBoth(true)

proc leavingArm(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(s)
  if c:
    v = toOpenArray(t)
    return
  t.add 5   # the arm that borrowed `t` never reaches here
  echo v.len

leavingArm(false)

proc otherSource(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  if c: v = toOpenArray(s)
  echo v.len
  var u = @[7]
  u.add 8   # neither `s` nor `t`
  echo u.len

otherSource(true)

proc viewLocalToLoop(n: int) =
  var s = @[1, 2, 3]
  for i in 0..<n:
    let w = toOpenArray(s)
    echo w[0]
  s.add 4   # `w` died with the loop body
  echo s.len

viewLocalToLoop(2)

proc iterateThenMutate =
  var s = @[1, 2, 3]
  for x in mitems(s):
    x = x * 2
  s.add 4   # the iteration borrow ended with the loop
  echo s.len

iterateThenMutate()

proc breakOut =
  var s = @[1, 2, 3]
  for x in mitems(s):
    if x == 2: break
    x = 0
  s.add 4   # the binder died on the `break` edge too
  echo s.len

breakOut()
