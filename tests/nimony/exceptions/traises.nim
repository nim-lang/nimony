import std/assertions

proc foo() {.raises.} =
  if false:
    return

try:
  foo()
except:
  assert false