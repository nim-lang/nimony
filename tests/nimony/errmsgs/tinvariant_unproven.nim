# A loop invariant that does not hold on entry, is violated on entry, is not
# re-established by the body (at the end or at a `continue`), or depends on
# the loop variable, which is not bound yet at the head.

proc notOnEntry(x: int): int =
  var k = x
  for i in 0 ..< 3:
    {.invariant: k >= 0 and k < 10.}
    k = 1
  result = k

proc violatedOnEntry(): int =
  var k = 20
  for i in 0 ..< 3:
    {.invariant: k < 10.}
    k = 1
  result = k

proc notPreserved(x: int): int =
  var k = 0
  for i in 0 ..< 3:
    {.invariant: k >= 0 and k < 10.}
    k = x
  result = k

proc continueBreaks(): int =
  var k = 0
  for i in 0 ..< 3:
    {.invariant: k < 10.}
    if i == 1:
      k = 50
      continue
    k = 1
  result = k

proc loopVar(): int =
  result = 0
  for i in 0 ..< 3:
    {.invariant: i >= 0.}
    result = i

discard notOnEntry(1)
discard violatedOnEntry()
discard notPreserved(1)
discard continueBreaks()
discard loopVar()
