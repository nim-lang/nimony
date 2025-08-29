import deps/musingstmt

type
  Foo = object
    discard

using
  x: Foo
  y: array[2, int]

proc test(x; y) =
  discard

proc test2(x; y: array[2, int]) =
  discard

proc test3(x = Foo(); y) =
  discard

var f = Foo()
test f, [1, 2]
test2 f, [1, 2]
test3 f, [1, 2]

# test imported procs
let c = MUsing()
let y = [0.1, 0.2, 0.3]
mtest1(c, y)
mtest2(c, y)
mtest3(c, y, 0)
