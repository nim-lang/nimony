import std/assertions

type
  GlobFilter = object
    incl: bool
    glob: string

  GlobState = object
    one: int
    two: int

proc aa() =
  let filters = @[GlobFilter(incl: true, glob: "**")]
  assert filters.len == 1

  var wbg: seq[GlobState] = @[]
  wbg.add GlobState()

  var dirc: seq[seq[GlobState]] = @[wbg]
  while true:
    wbg = dirc[dirc.len - 1]
    dirc.add wbg
    break

  assert dirc.len == 2

var handlerLocs: seq[string] = @[]
handlerLocs.add "sammich"
aa()
aa()
assert handlerLocs[0] == "sammich"

block:
  proc aa2() =
    var
      a = @[0]
      b = @[a]
    block:
      a = b[0]
      b.add a
    assert b.len == 2
    assert b[0][0] == 0
    assert b[1][0] == 0

  aa2()
