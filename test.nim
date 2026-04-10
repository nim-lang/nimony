# import std/syncio

proc a() {.passive.} =
  discard
proc c() {.passive.} =
  discard
proc b() {.passive.} =
  if true:
    a()
  elif true:
    c()

b()