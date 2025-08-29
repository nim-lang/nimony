block:
  proc inc2(x: var int) =
    discard

  proc foo =
    var id: ref int
    new id
    # var m = addr id
    inc2 id[]

  foo()

block:
  proc inc2(x: var int) =
    discard

  proc foo =
    var id: int = 34
    var m = addr id
    inc2 m[]

  foo()

block:
  proc inc3(x: var ptr int) =
    discard

  proc foo2 =
    var id: int = 12
    var m = addr id
    inc3 (addr m)[]

  foo2()