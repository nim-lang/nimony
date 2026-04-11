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

proc main2() {.passive.} =
  echo "main2:"
  if true:
    if true:
      a()
      if true:
        a()
        if true:
          a()
    else:
      a()
      discard
      discard
    c()
    discard
    discard
    echo "test"
  elif true:
    c()
  if true:
    discard
  else:
    discard

main2()