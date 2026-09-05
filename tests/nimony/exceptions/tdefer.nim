
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

# regression: an exit path BEFORE the defer must not run the defer body
# (the lowering used to wrap the whole scope in the try/finally, so an
# early raise ran the defer and its generated code referenced locals
# that are not declared on that path)

proc g(x: int): int {.raises.} =
  if x < 0:
    raise BadOperation
  var v: seq[int] = @[]
  defer:
    echo "release ", v.len
  v.add x
  result = v.len

try:
  echo g(3)
except:
  echo "caught"

try:
  echo g(-1)
except:
  echo "caught"
