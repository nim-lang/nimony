# A candidate loop invariant some back-edge does not restore is dropped: none of
# these bounds may be claimed after its loop. The proven side lives in
# `tests/nimony/contracts/tloopinvariants.nim`.

proc overshoot(s: string; start: int) =
  if start < 0 or start > s.len: return
  var j = start
  while j < s.len + 5: inc j
  {.assert: j <= s.len.}

proc stepTwo(s: string) =
  var j = 0
  while j < s.len:
    j += 2
  {.assert: j <= s.len.}

proc resetInOuter(s: string) =
  var j = 0
  var k = 0
  while k < 3:
    while j < s.len: inc j
    j = s.len + 1
    inc k
  {.assert: j <= s.len.}

proc breakAfterJump(s: string) =
  var j = 0
  while true:
    if j >= s.len: break
    if s[j] == 'x':
      j = s.len + 3
      break
    inc j
  {.assert: j <= s.len.}

proc guardNotKept() =
  var i = 0
  while i < 300:
    inc i
  {.assert: i <= 255.}

proc stepOfUnknownSign(s: string) =
  var i = 0
  var k = 0
  while k < s.len:
    let d = s.len - 5
    inc i, d
    inc k
  {.assert: 0 <= i.}

proc subtractedStep(s: string) =
  var i = 0
  var k = 0
  while k < s.len:
    if k >= 0:
      i = i - k
    inc k
  {.assert: 0 <= i.}

overshoot("ab", 0)
stepTwo("abc")
resetInOuter("ab")
breakAfterJump("abx")
guardNotKept()
stepOfUnknownSign("abc")
subtractedStep("abc")
