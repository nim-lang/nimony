# The same type moved instead of copied: nothing for `check --hexer` to report.

type
  Pool = object
    id: int

proc `=destroy`(x: Pool) =
  discard
proc `=wasMoved`(x: var Pool) =
  x.id = 0
proc `=copy`(d: var Pool; s: Pool) {.error.}
proc `=dup`(s: Pool): Pool {.error.}

proc main =
  var a = Pool(id: 1)
  let b = ensureMove a
  discard b.id
main()
