## A move out of a variable whose type has a user `=destroy` resets it to
## binary zero, the moved-from state the built-in `=wasMoved` produces, so the
## variable's own `=destroy` finds nothing to release. Each descriptor below
## must be closed exactly once, and a field's declared default plays no part.

import std/syncio

type
  Handle = object
    fd: int = -1                   # not the moved-from state: that is zero

var closed: seq[int] = @[]

proc `=destroy`(h: Handle) =
  if h.fd != 0: closed.add h.fd

proc open(fd: int): Handle {.noinline.} = Handle(fd: fd)

proc moved(fd: int): Handle {.noinline.} =
  var h = open(fd)
  result = h                       # the last read of h: a move

proc passOn(h: sink Handle): Handle {.noinline.} =
  result = h                       # a move out of a sink parameter

proc main =
  block:
    let a = moved(4)
    let b = passOn(open(5))
    var s: seq[Handle] = @[]
    var c = open(6)
    s.add c                        # a move into the seq
    echo a.fd, " ", b.fd, " ", s[0].fd
    var d = open(7)
    let e = move(d)
    echo "moved-from fd ", d.fd, ", moved-to fd ", e.fd
  for fd in [4, 5, 6, 7, -1]:
    var n = 0
    for x in closed:
      if x == fd: inc n
    echo fd, " closed ", n, " time(s)"

main()
