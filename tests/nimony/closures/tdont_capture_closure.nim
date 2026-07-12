import std/assertions

# issue #2115

proc takesClosure(clsr: proc () {.closure.}) =
  clsr()

proc testClosureWOEnv =
  proc closureWOEnv() {.closure.} = discard

  closureWOEnv()
  takesClosure(closureWOEnv)

testClosureWOEnv()

proc takesClosureInt(clsr: proc (x: int): int {.closure.}) =
  assert clsr(123) == 234

proc testClosureWOEnvInt =
  proc closureWOEnvInt(x: int): int {.closure.} = x + 111

  takesClosureInt(closureWOEnvInt)

testClosureWOEnvInt()

proc testClosureWOEnvInt2 =
  var a = 111
  proc closureProcInt(x: int): int {.closure.} = x + a
  proc closureWOEnvInt(x: int): int {.closure.} = x + 111

  takesClosureInt(closureProcInt)
  takesClosureInt(closureWOEnvInt)

testClosureWOEnvInt2()
