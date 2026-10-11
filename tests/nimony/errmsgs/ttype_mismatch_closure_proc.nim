proc dontTakeClosure(p: proc ()) = discard

proc test =
  proc closureProc() {.closure.} = discard
  dontTakeClosure(closureProc)

test()

proc takesOpenArray(a: openArray[int]; b: int) = discard
takesOpenArray([1, 2], 3, 4)
