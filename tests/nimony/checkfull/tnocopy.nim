# A copy of a type whose `=copy`/`=dup` are `{.error.}` is rejected by hexer,
# not by the frontend. `nimony check --fullCheck` reports it, as `nimony c` does.

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
  let a = Pool(id: 1)
  var b = a
  discard b.id == a.id
main()
