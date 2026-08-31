# The neighbours of `tundeclaredrettype_defer.nim`: each keeps the undeclared
# return type and varies one of the other two properties. All of them reported
# an ordinary diagnostic before nim-lang/nimony#2400 was fixed and must keep
# doing so, unchanged.

proc emptySeq[T](): seq[T] = @[]

proc noDefer(): NotAType =
  return emptySeq()

proc noReturn(): NotAType =
  defer: discard

proc bareReturn(): NotAType =
  defer: discard
  return

proc typedReturn(): NotAType =
  defer: discard
  return 1
