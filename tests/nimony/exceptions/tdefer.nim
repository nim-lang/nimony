
import std / syncio

proc test(a: string) =
  defer:
    echo a

test("ac")

proc testC(a: string) =
  if a.len > 0:
    defer:
      echo "b"

testC("")

proc testD(a: string) =
  echo "begin", a
  defer:
    echo "end"
  echo "middle"

testD("")

proc testB(a: string) =
  defer:
    echo a
  defer:
    echo "b"

testB("ac")

# bug #1440

proc f(n: int): int =
  var n = n
  defer:
    inc n
    echo n
  return n

echo f(5)
