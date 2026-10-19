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
