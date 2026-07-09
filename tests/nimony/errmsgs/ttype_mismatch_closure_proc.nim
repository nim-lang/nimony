proc dontTakeClosure(p: proc ()) = discard

proc test =
  proc closureProc() {.closure.} = discard
  dontTakeClosure(closureProc)

test()
