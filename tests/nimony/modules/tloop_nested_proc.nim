# Two modules each lift a routine declared in an inlined `for` body. The
# copies' names must stay global, or both modules define the same C symbol
# (#2681).
import std/syncio
import deps/mloop_nested_proc

proc runB(): int =
  result = 0
  for x in two():
    var k = x
    var m = k
    proc inner(): int = 20
    result += inner() + m - x

echo runA() + runB()
