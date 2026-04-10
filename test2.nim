proc a() {.passive.} =
  discard
proc b() {.passive.} =
  if true:
    a()
    discard
  else:
    a()
b()