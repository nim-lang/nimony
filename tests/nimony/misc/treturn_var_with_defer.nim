import std/syncio

proc f1(n: int): int =
  var n = n
  defer:
    inc n
    echo "f1 defer: ", n
  return n

proc f2(n: int): int =
  var n = n
  defer:
    inc n
    echo "f2 defer: ", n
  result = n

proc f3(n: int): int =
  var n = n
  defer:
    inc n
    echo "f3 defer: ", n
  result = n
  return result

proc f4(n: int): int =
  var n = n
  defer:
    inc n
    echo "f4 defer: ", n
  n

echo f1(10)
echo f2(20)
echo f3(30)
echo f4(40)
