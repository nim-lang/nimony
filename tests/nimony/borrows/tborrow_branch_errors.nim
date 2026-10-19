# Test: a view reassigned in only some branches may still borrow its old
# source after the branches join.

proc reassignInThen(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  if c: v = toOpenArray(s)
  t.add 5
  discard v.len

reassignInThen(false)

proc reassignInElse(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  if c: discard
  else: v = toOpenArray(s)
  t.add 5
  discard v.len

reassignInElse(true)

proc reassignInCaseArm(k: int) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  case k
  of 0: v = toOpenArray(s)
  else: discard
  t.add 5
  discard v.len

reassignInCaseArm(1)

proc reassignInTry(c: bool) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  try:
    v = toOpenArray(s)
  except:
    discard
  t.add 5
  discard v.len

reassignInTry(false)

proc reassignInLoop(n: int) =
  var s = @[1, 2, 3]
  var t = @[4]
  var v: openArray[int] = toOpenArray(t)
  for i in 0..<n:
    v = toOpenArray(s)
  t.add 5   # the loop may run zero times
  discard v.len

reassignInLoop(0)
