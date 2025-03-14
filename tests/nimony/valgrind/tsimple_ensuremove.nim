
proc foo(b: var seq[int]) =
  var a = newSeq[int](1)
  b = ensureMove a

var b: seq[int] = @[]
foo(b)
