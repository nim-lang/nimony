# Only a routine converts implicitly to a `.closure.` proc type: it gets a
# static env-taking thunk. A proc value (a variable, a parameter, an array
# element) has no routine to forward to and is rejected; wrap it in a proc
# literal that calls it.

type
  Cb = object
    accept: proc(): bool {.closure.}
  P = proc(): bool
  PC = proc(): bool {.closure.}

proc alwaysTrue(): bool = true
proc takeIt(x: PC): bool = x()
proc wire(c: var Cb, accept: proc(): bool) = c.accept = accept

proc main =
  var p: proc(): bool = alwaysTrue
  var cb: proc(): bool {.closure.} = p
  var q: P = alwaysTrue
  discard takeIt(q)
  var arr: array[2, P] = [alwaysTrue, alwaysTrue]
  var cb3: PC = arr[0]
  # fine: a routine, a proc literal, and the explicit wrapper
  var ok1: PC = alwaysTrue
  var ok2: PC = proc (): bool = false
  var ok3: PC = proc (): bool = p()

main()
