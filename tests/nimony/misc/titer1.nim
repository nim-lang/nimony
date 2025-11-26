
import std / syncio

type
  A = object
    x: int

proc foo(): (A, A, A, string) =
  result = (A(x: 3), A(x: 4), A(x: 5), "abc")

let (a, b, c, _) = foo()

let other = foo()
echo(`[]`(other, 3))
# other[3]

iterator countup(a, b: int): int =
  var x = a
  while x <= b:
    yield x
    inc x

iterator countupB(a, b: int): (int, int) =
  var x = a
  while x <= b:
    let res = (x, 9)
    yield res
    inc x

proc use(x: int) = echo x

proc mymain =
  for i in countup(1, 4):
    for j in countup(3, 5):
      use j
    discard i
    if not (i <= 5):
      break

  for ik, jk in countupB(8, 9):
    use ik
    use jk

mymain()

for i in countup(3, 5):
  echo i
