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

