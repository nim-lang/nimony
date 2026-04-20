import std/syncio

proc a() {.passive} =
  echo "a"
proc b() {.passive} =
  echo "b"

proc io() {.passive.} =
  if true:
    a()
  else:
    b()

io()

var d = 0
proc aa(x: int) {.passive.} =
    echo "aaaa ", d, " x=", x
    inc d
proc main2() {.passive.} =
  var a = 0
  if true:
    if true:
      aa(0)
      if true:
        aa(1)
        if true:
          aa(2)
          a = 2
    else:
      aa(3)
      discard
      discard
    aa(4)
    discard
    discard
  elif true:
    aa(5)
  if true:
    discard
  else:
    discard

main2()

proc main3() {.passive.}=
  for i in 0..3:
    echo "main3: ", i
main3()