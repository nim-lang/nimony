## One frame field of every kind, live across a `yield`.
##
## A local that a closure iterator uses after a `yield` is hoisted into
## the coroutine frame, and the frame's constructor must name it:
## `oconstr` is TOTAL. The C back end forgives less than that, since a
## designated initializer zeroes whatever it does not mention, but the
## native one stores exactly the fields the constructor lists — so a
## field the constructor forgets keeps whatever the frame's storage held,
## and a forgotten `string` field is a `=destroy` reading a wild length.
##
## Each local below is written before the first `yield` and read after
## it, so each one becomes a frame field and each exercises one case of
## `hexer/defaultvalues`. A default built at the wrong width or shape
## shows up as a wrong value or a crash rather than as nothing at all.
##
## `set` is missing on purpose: a set-typed local in a closure iterator
## does not compile yet ("BUG: not eliminated: (setconstr …)"), which is
## a separate defect in the desugar/cps interaction.

import std / syncio

type
  Color = enum
    red, green, blue
  Point = object
    x, y: int
    tag: string
  Kilometres = distinct int
  Shape = object
    case round: bool
    of true: radius: int
    of false: side: int

iterator everything(): int {.closure.} =
  var s = "frame"
  var n = 41
  var f = 1.5'f32
  var c = 'q'
  var flag = false
  var col = green
  var sq: seq[int] = @[]
  var arr: array[3, int] = [0, 0, 0]
  var tup: (int, string) = (0, "")
  var pt = Point(x: 1, y: 2, tag: "pt")
  var km = Kilometres(0)
  var sh = Shape(round: true, radius: 3)

  yield 0

  s.add "-field"
  n = n + 1
  f = f + 0.5'f32
  c = 'z'
  flag = true
  col = blue
  sq = @[7, 8]
  arr = [1, 2, 3]
  tup = (9, "tup")
  pt.tag = pt.tag & "!"
  km = Kilometres(int(km) + 5)
  sh = Shape(round: false, side: 4)

  yield 1

  yield s.len
  yield n
  yield (if f > 1.9'f32: 1 else: 0)
  yield ord(c)
  yield (if flag: 1 else: 0)
  yield ord(col)
  yield sq[0] + sq[1]
  yield arr[0] + arr[1] + arr[2]
  yield tup[0]
  yield tup[1].len
  yield pt.x + pt.y
  yield pt.tag.len
  yield int(km)
  yield sh.side

proc main() =
  for v in everything():
    echo v

main()
