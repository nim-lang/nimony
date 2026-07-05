import std/[assertions, opt]

proc find(k: string): Opt[int] =
  if k == "x": result = some(1)
  else: result = none[int]()

proc main {.raises.} =
  # Some case
  let a = find("x")
  assert a.isSome
  assert not a.isNone
  assert a.get(0) == 1
  assert a.unsafeGet == 1

  # None case (the fieldless branch)
  let b = find("y")
  assert b.isNone
  assert not b.isSome
  assert b.get(-1) == -1

  # unsafeGet raises BugError on None
  var raised = false
  try:
    discard b.unsafeGet
  except:
    raised = true
  assert raised

  # string payload (non-trivial destructor) + reassignment across branches
  var s = some("hello")
  assert s.get("") == "hello"
  s = none[string]()
  assert s.isNone

try:
  main()
except:
  assert false, "unexpected exception"
