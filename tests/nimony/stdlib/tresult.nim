import std/[assertions, result]

proc parsePort(s: string): Result[int, string] =
  # bare ok/err — the missing type parameter is inferred from the return type
  if s == "": result = err("empty")
  else: result = ok(8080)

proc main {.raises.} =
  # Ok case
  let a = parsePort("80")
  assert a.isOk
  assert not a.isErr
  assert a.get(0) == 8080
  assert a.error("none") == "none"      # total accessor falls back on the wrong variant
  assert a.unsafeGet == 8080

  # Err case
  let b = parsePort("")
  assert b.isErr
  assert not b.isOk
  assert b.get(-1) == -1
  assert b.error("?") == "empty"

  # unsafeGet raises BadOperation on Err
  var raised = false
  try:
    discard b.unsafeGet
  except:
    raised = true
  assert raised

  # Result[T, T] doubles as an either / this-or-that
  let c = ok[int, int](5)
  let d = err[int, int](7)
  assert c.get(0) == 5
  assert d.error(0) == 7

try:
  main()
except:
  assert false, "unexpected exception"
